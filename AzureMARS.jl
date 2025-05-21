# This file is a part of AzureMARS, distributed under the MIT license.
# See LICENSE.md in the repository root for full license text.

"""
AzureMARS 1.0 Julia bindings.

Exported types:
- `Operation`: operation enum,
- `Modifier`: modifier enum,
- `AddrMode`: address mode enum,
- `Instruction`: a single instruction,
- `Score`: match outcome containing WLT counts and normalized scores,
- `MARSParams`: simulation parameters,
- `Warrior`: warrior as a vector of instructions and an entry point.

Exported MARS functions:
- `init_mars`: must be called right after defining new MARS parameters,
- `load_warrior`: load a precompiled warrior,
- `create_queues`: create empty process queues,
- `destroy_queues`: free the memory allocated for queues on the Pascal side,
- `play_match_random`: play a match with random warrior positions,
- `play_match_permute`: play a match with all possible permutations of starting 
   positions and move orders.

Typical pipeline consists of:
- MARS initialization: `init_mars` must be called after each change of MARS 
  parameters and before any calls to other MARS functions;
- variable allocation: creating the core, the queues, and the score variable;
- running the simulation: calls to `play_match_*` functions. The core and the
  queues can be reused during subsequent calls as long as MARS parameters stay
  the same;
- cleanup: the queues are allocated on the Pascal side and thus must be 
  destroyed manually via `destroy_queues`. The core is managed by Julia. 

Usage example:
```
# Including the module (compiled library must be located in the same folder)
include("AzureMARS.jl")
using .AzureMARS

# Initialization
mars_params = MARSParams(80, 80, 800, 5, 5)
init_mars(mars_params)

# Create the variables required for simulation
core = Vector{Instruction}(undef, mars_params.coresize)
queues = create_queues(mars_params)
score = Score()

# Run the simulation
warrior1 = load_warrior("test/nano/another.rc", mars_params)
warrior2 = load_warrior("test/nano/Anefam_Vesyes.rc", mars_params)
play_match_random(warrior1, warrior2, core, queues, mars_params, 8000, score)

# Cleanup
destroy_queues(queues, mars_params)
```
"""
module AzureMARS

using Libdl
export Instruction, Operation, Modifier, AddrMode, Score, MARSParams, Warrior
export play_match_random, play_match_permute
export init_mars, load_warrior, create_queues, destroy_queues

const mars = dlopen("./AzureMARS.dll")
const p_LoadWarrior         = dlsym(mars, :LoadWarrior)
const p_FreeWarrior         = dlsym(mars, :FreeWarrior)
const p_FreeString          = dlsym(mars, :FreeString)
const p_ClearCore           = dlsym(mars, :ClearCore)
const p_CreateQueues        = dlsym(mars, :CreateQueues)
const p_DestroyQueues       = dlsym(mars, :DestroyQueues)
const p_LoadIntoCore        = dlsym(mars, :LoadIntoCore)
const p_RandomLoadIntoCore  = dlsym(mars, :RandomLoadIntoCore)
const p_InitMARS            = dlsym(mars, :InitMARS)
const p_RunSimulation       = dlsym(mars, :RunSimulation)
const p_PlayMatchRandom     = dlsym(mars, :PlayMatchRandom)
const p_PlayMatchPermute    = dlsym(mars, :PlayMatchPermute)

#-----------------------<< Instruction-Related Types >>------------------------#

"""
    @enum(
        Operation :: UInt8, 
        opDAT = 0, opMOV, opNOP, 
        opADD, opSUB, opMUL, opDIV, opMOD,
        opJMP, opJMZ, opJMN, opDJN, opSPL,
        opSEQ, opSNE, opSLT
    )
    
Pascal-compatible enum defining redcode operations.
"""
@enum(
    Operation :: UInt8, 
    opDAT = 0, opMOV, opNOP, 
    opADD, opSUB, opMUL, opDIV, opMOD,
    opJMP, opJMZ, opJMN, opDJN, opSPL,
    opSEQ, opSNE, opSLT
)
const operation_names = [
    "DAT", "MOV", "NOP", 
    "ADD", "SUB", "MUL", "DIV", "MOD",
    "JMP", "JMZ", "JMN", "DJN", "SPL",
    "SEQ", "SNE", "SLT"
]

"""
    @enum(
        Modifier :: UInt8,
        modF = 0, modX, modI, modA, modB, modAB, modBA
    )
    
Pascal-compatible enum defining modifiers.
"""
@enum(
    Modifier :: UInt8,
    modF = 0, modX, modI, modA, modB, modAB, modBA
)    
const modifier_names = ["F", "X", "I", "A", "B", "AB", "BA"]

"""
    @enum(
        AddrMode :: UInt8,
        amDirect = 0, amImmediate, amAIndirect, amAPreDec, 
        amAPostInc, amBIndirect, amBPreDec, amBPostInc
    )
    
Pascal-compatible enum defining addressation modes.
"""
@enum(
    AddrMode :: UInt8,
    amDirect = 0, amImmediate, amAIndirect, amAPreDec, 
    amAPostInc, amBIndirect, amBPreDec, amBPostInc
)
const addr_mode_names = ["\$", "#", "*", "{", "}", "@", "<", ">"]

"""
    struct Instruction
        operation   :: Operation
        modifier    :: Modifier
        amode       :: AddrMode
        bmode       :: AddrMode
        afield      :: UInt16
        bfield      :: UInt16
    end
    
Pascal-compatible instruction type. Must be immutable in order for 
`Vector{Instruction}` to retain binary compatibility.
"""
struct Instruction
    operation   :: Operation
    modifier    :: Modifier
    amode       :: AddrMode
    bmode       :: AddrMode
    afield      :: UInt16
    bfield      :: UInt16
end
const PInstruction = Ptr{Instruction}

Instruction() = Instruction(opDAT, modF, amDirect, amDirect, 0, 0)

function Base.show(io :: IO, i :: Instruction)
    op = i.operation in instances(Operation) ? 
        operation_names[1 + Int(i.operation)] : "???"
    mod = i.modifier in instances(Modifier) ? 
        rpad(modifier_names[1 + Int(i.modifier)], 3) : "? "
    modes = [
        getfield(i, mode) in instances(AddrMode) ?
            addr_mode_names[1 + Int(getfield(i, mode))] : "?"
        for mode in (:amode, :bmode)
    ]
    fields = [lpad(getfield(i, field), 5) for field in (:afield, :bfield)]
    print(
        io, "$op.$mod$(modes[1])$(fields[1]), $(modes[2])$(fields[2])"
    )
end

#-----------------------<< Other Types >>--------------------------------------#

"""
    mutable struct Score
        w               :: Int64
        l               :: Int64
        t               :: Int64
        int_taken       :: Int64
        int_given       :: Int64
        taken           :: Float64
        given           :: Float64
        cycles          :: Int64
    end

Pascal-compatible structure holding the outcome of a match of A vs B from A's
perspective:
- `w`: number of wins,
- `l`: number of losses,
- `t`: number of ties,
- `rounds`: `w + l + t`,
- `int_taken`: `3w + t`,
- `int_given`: `3l + t`,
- `taken`: `100 * int_taken / rounds`,
- `given`: `100 * int_given / rounds`,
- `cycles`: total simulation cycles.

Methods:
- `Score()`: empty score constructor,
- `+` operator: integer fields are added together, and the normalized `taken`
  and `given` scores are recalculated.
"""
mutable struct Score
    w               :: Int64
    l               :: Int64
    t               :: Int64
    rounds          :: Int64
    int_taken       :: Int64
    int_given       :: Int64
    taken           :: Float64
    given           :: Float64
    cycles          :: Int64
end
const PScore = Ptr{Score}

Base.:(+)(a :: Score, b :: Score) = Score(
    a.w + b.w,
    a.l + b.l,
    a.t + b.t,
    a.rounds + b.rounds,
    a.int_taken + b.int_taken,
    a.int_given + b.int_given,
    (a.int_taken + b.int_taken) / (a.rounds + b.rounds),
    (a.int_given + b.int_given) / (a.rounds + b.rounds),
    a.cycles + b.cycles
)

Score() = Score(0, 0, 0, 0, 0, 0, 0, 0, 0)

"""
    struct MARSParams
        coresize    :: Clonglong
        maxprocs    :: Clonglong
        maxcycles   :: Clonglong
        mindist     :: Clonglong
        maxlength   :: Clonglong
    end
    
Pascal-compatible structure with simulation parameters.
"""
struct MARSParams
    coresize    :: Clonglong
    maxprocs    :: Clonglong
    maxcycles   :: Clonglong
    mindist     :: Clonglong
    maxlength   :: Clonglong
end
const PMARSParams = Ptr{MARSParams}

const PQueues = Ptr{Cvoid}

const PChar = Ptr{Cchar}

#-----------------------<< Warrior >>------------------------------------------#

"""
    mutable struct Warrior
        code  :: Vector{Instruction}
        org   :: Int
    end

Warrior represented as a vector of instructions and an entry point. Supports 
indexation, iteration, hashing, and equality test. `code` and `org` are both 
1-based.
"""
mutable struct Warrior
    code  :: Vector{Instruction}
    org   :: Int
end

Base.show(io :: IO, w :: Warrior) = print(
    io, "ORG $(w.org - 1)\n" * prod(["$(instr)\n" for instr in w.code])
)

Warrior() = Warrior(Instruction[], 0)

Base.hash(w :: Warrior, h :: UInt) = hash(w.code, hash(w.org, h))
Base.:(==)(a :: Warrior, b :: Warrior) = a.code == b.code && a.org == b.org
Base.isequal(a :: Warrior, b :: Warrior) = 
    isequal(a.code, b.code) && isequal(a.org, b.org)
Base.getindex(w :: Warrior, i :: Int) = w.code[i]
Base.setindex!(w :: Warrior, v, i :: Int) = (w.code[i] = v)
Base.firstindex(w :: Warrior) = 1
Base.lastindex(w :: Warrior) = length(w.code)
Base.length(w :: Warrior) = length(w.code)

"""
    mutable struct MARSWarrior
      len   :: Int64
      org   :: Int64
      code  :: PInstruction
    end

Wrapper type for passing warriors to the Pascal library.
"""
mutable struct MARSWarrior
  len   :: Int64
  org   :: Int64
  code  :: PInstruction
end
const PMARSWarrior = Ptr{MARSWarrior}

"""
    load_warrior(
        path        :: String, 
        mars_params :: MARSParams
        )           :: Warrior
        
Return a precompiled warrior loaded from `path`. Throw an `ErrorException` on
failure. Field values are reduced modulo `mars_params.coresize`.
"""
function load_warrior(
    path        :: String, 
    mars_params :: MARSParams
    )
    ptr_error_message = Ref(PChar())
    mars_warrior = MARSWarrior()
    error_len = ccall(
        p_LoadWarrior, Int32,
        (PMARSWarrior     , PChar, PMARSParams     , Bool, Ptr{PChar}),
         Ref(mars_warrior), path , Ref(mars_params), true, ptr_error_message
    )
    
    # Extract error message
    if error_len > 0
        error_message = unsafe_string(ptr_error_message[], error_len)
        ccall(p_FreeString, Cvoid, (PChar, ), ptr_error_message[])
        throw(ErrorException(error_message))
    end
    
    # Extract warrior code
    warrior = Warrior(
        [unsafe_load(mars_warrior.code, i) for i in 1 : mars_warrior.len],
        1 + mars_warrior.org
    )
    ccall(
        p_FreeWarrior, Cvoid,
        (PMARSWarrior, ), Ref(mars_warrior)
    )
    
    return warrior
end

MARSWarrior() = MARSWarrior(0, 0, C_NULL)

MARSWarrior(w :: Warrior) = MARSWarrior(
    length(w), w.org - 1, pointer(w.code, 1)
)

#-----------------------<< MARS Functions >>-----------------------------------#

"""
    init_mars(mars_params :: MARSParams)
    
Initialize MARS with provided simulation parameters. Must be called once before 
any calls to other MARS functions. Must be called again if MARS parameters 
change.
"""
init_mars(mars_params :: MARSParams) = ccall(
    p_InitMARS, Cvoid, (PMARSParams, ), Ref(mars_params)
)

clear_core(core :: Vector{Instruction}, mars_params :: MARSParams) = ccall(
    p_ClearCore, Cvoid,
    (PInstruction, Clonglong), Ref(core, 1), mars_params.coresize
)

load_into_core(
    core        ::  Vector{Instruction}, 
    queues      ::  PQueues,
    warrior     ::  Warrior,
    position    ::  Integer,
    mars_params ::  MARSParams
    ) = ccall(
        p_LoadIntoCore, Cvoid,
        (PInstruction, Ptr{PQueues}, PMARSWarrior, Int64, PMARSParams), 
        Ref(core, 1), Ref(queues), Ref(MARSWarrior(warrior)), 
        position, Ref(mars_params)
    )

"""
    create_queues(mars_params :: MARSParams)
    
Return a pointer to the queues allocated on the Pascal side.
"""
create_queues(mars_params :: MARSParams) = ccall(
    p_CreateQueues, PQueues, (PMARSParams, ), Ref(mars_params)
)

"""
    destroy_queues(queues :: PQueues, mars_params :: MARSParams)
    
Destroy the queues and free the memory allocated on the Pascal side.
"""
destroy_queues(queues :: PQueues, mars_params :: MARSParams) = ccall(
    p_DestroyQueues, Cvoid,
    (PQueues, PMARSParams),
    p_queues, Ref(mars_params)
)

"""
    play_match_permute(
        warrior1    :: Warrior,
        warrior2    :: Warrior,
        core        :: Vector{Instruction}, 
        queues      :: PQueues, 
        mars_params :: MARSParams,
        score       :: Score
        )
        
Play a match between `warrior1` and `warrior2` with all possible permutations
of starting positions and move orders. `core` must be pre-allocated on Julia
side (`length(core) == mars_params.coresize`). `queues` must be pre-allocated 
with `create_queues`.
"""
function play_match_permute(
    warrior1    :: Warrior,
    warrior2    :: Warrior,
    core        :: Vector{Instruction}, 
    queues      :: PQueues, 
    mars_params :: MARSParams,
    score       :: Score
    )
    mars_warrior1 = MARSWarrior(warrior1)
    mars_warrior2 = MARSWarrior(warrior2)
    ccall(
        p_PlayMatchPermute, Cvoid, (
            PMARSWarrior, PMARSWarrior, PInstruction, PQueues, 
            PMARSParams, PScore
        ),
        Ref(mars_warrior1), Ref(mars_warrior2), 
        Ref(core, 1), queues, Ref(mars_params), Ref(score)
    )
end

"""
    play_match_random(
        warrior1    :: Warrior,
        warrior2    :: Warrior,
        core        :: Vector{Instruction}, 
        queues      :: PQueues, 
        mars_params :: MARSParams,
        rounds      :: Integer,
        score       :: Score,
        )
    
Play a match with a given number of `rounds` between `warrior1` and `warrior2` 
with random starting positions and move orders. `core` must be pre-allocated 
on Julia side (`length(core) == mars_params.coresize`). `queues` must be pre-
allocated with `create_queues`.
"""
function play_match_random(
    warrior1    :: Warrior,
    warrior2    :: Warrior,
    core        :: Vector{Instruction}, 
    queues      :: PQueues, 
    mars_params :: MARSParams,
    rounds      :: Integer,
    score       :: Score,
    )
    mars_warrior1 = MARSWarrior(warrior1)
    mars_warrior2 = MARSWarrior(warrior2)
    ccall(
        p_PlayMatchRandom, Cvoid, (
            PMARSWarrior, PMARSWarrior, PInstruction, PQueues, 
            PMARSParams, Int64, PScore
        ),
        Ref(mars_warrior1), Ref(mars_warrior2), 
        Ref(core, 1), queues, Ref(mars_params), rounds, Ref(score)
    )
end

run_simulation(
    core            :: Vector{Instruction}, 
    queues          :: PQueues, 
    side_to_move    :: Int64,
    mars_params     :: MARSParams,
    score           :: Score,
    ) = ccall(
        p_RunSimulation, Cvoid,
        (PInstruction, PQueues, Int64, PMARSParams, PScore),
        Ref(core, 1), queues, side_to_move, Ref(mars_params), Ref(score), 
    )
    
"""
    play_match_permute(
        warrior     :: Warrior,
        benchmark   :: Vector{Warrior},
        core        :: Vector{Instruction}, 
        queues      :: PQueues, 
        mars_params :: MARSParams
        )           :: Score

Play a series of matches between `warrior` and  every opponent in `benchmark`, 
and return the total score. Starting positions and move orders take all possible
combinations. `core` and `queues` must be preallocated.
"""
function play_match_permute(
    warrior     :: Warrior,
    benchmark   :: Vector{Warrior},
    core        :: Vector{Instruction}, 
    queues      :: PQueues, 
    mars_params :: MARSParams
    )
    sum_score = Score()
    score = Score()
    for opponent in benchmark
        play_match_permute(
            warrior, opponent, core, queues, mars_params, score
        )
        sum_score += score
    end
    return sum_score
end

"""
    play_match_random(
        warrior     :: Warrior,
        benchmark   :: Vector{Warrior},
        core        :: Vector{Instruction}, 
        queues      :: PQueues, 
        mars_params :: MARSParams,
        rounds      :: Integer
        )           :: Score

Play a series of matches with a given number of `rounds` between `warrior` and  
every opponent in `benchmark`, and return the total score. Starting positions 
and move orders are random. `core` and `queues` must be preallocated.
"""
function play_match_random(
    warrior     :: Warrior,
    benchmark   :: Vector{Warrior},
    core        :: Vector{Instruction}, 
    queues      :: PQueues, 
    mars_params :: MARSParams,
    rounds      :: Integer
    )
    sum_score = Score()
    score = Score()
    for opponent in benchmark
        play_match_random(
            warrior, opponent, core, queues, mars_params, rounds, score
        )
        sum_score += score
    end
    return sum_score
end

end
