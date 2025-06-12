// This file is a part of AzureMARS, distributed under the MIT license.
// See LICENSE.md in the repository root for full license text.

// Variables cheat sheet:
// VARIABLE    TYPE           EXPRESSION           DESCRIPTION
// CurrentProc PProc          q.current            Current process pointer
// NextProc    PProc          CurrentProc.Next     Next process pointer 
// q           PQueue         @Queues[Side]        Pointer to the current queue 
// Regs.I      TInstruction   Core[Regs.AddrI]     Register holding the current instruction
// Regs.A      TInstruction   Core[Regs.AddrA]     A-field register
// Regs.B      TInstruction   Core[Regs.AddrB]     B-field register
// Regs.AddrI  Int64          CurrentProc.Location Address of the current process
// Regs.AddrA  Int64                               Effective A-field address of the current instruction
// Regs.AddrB  Int64                               Effective B-field address of the current instruction
// AddrInc     Int64                               Address of the icremented location

{$MODE DELPHI} {$POINTERMATH ON}
{$IFDEF lib}
   library AzureMARS;
   uses SysUtils;
{$ELSE}
   unit AzureMARS;
{$ENDIF} 

{$IFNDEF lib}
interface
{$ENDIF}
const 
   MaxCoreSize    =  8000;
   MaxQueueSize   =  8000;

type 
   TOperation = 
   (  opDAT, opMOV, opNOP,
      opADD, opSUB, opMUL, opDIV, opMOD,
      opJMP, opJMZ, opJMN, opDJN, opSPL,
      opSEQ, opSNE, opSLT
   );

   TModifier = (modF, modX, modI, modA, modB, modAB, modBA);

   TAddrMode = 
   (  amDirect, amImmediate, 
      amAIndirect, amAPreDec, amAPostInc,
      amBIndirect, amBPreDec, amBPostInc
   );

   TInstruction = 
      record
      Operation      :  TOperation;
      Modifier       :  TModifier;
      AMode, BMode   :  TAddrMode;
      AField, BField :  Word;
      end;
   PInstruction = ^TInstruction;

   TMARSWarrior = 
      record
      Len, Org : Int64;
      _        : PInstruction;
      end;

   PProc =  ^TProcess;

   TProcess = 
      record
      location    :  Int64;
      next, prev  :  PProc;
      end;

   TQueue = 
      record
      NProcs      :  Int64;
      Current     :  PProc;
      Next        :  PProc;
      Processes   :  PProc;
      Free        :  ^PProc;
      end;
   PQueue = ^TQueue;
   TQueues = array [0 .. 1] of TQueue;
   PQueues = ^TQueues;

   TScore = 
       record
       W, L, T, Rounds     : Int64;
       IntTaken, IntGiven  : Int64;
       Taken, Given        : Double;
       Cycles              : Int64;
       end;
   
   TRegisters = 
      record
      A, B, I              :  TInstruction;
      AddrA, AddrB, AddrI  :  Int64;
      AddrDec, AddrInc     :  Int64;
      end;

   PCore = ^TInstruction;
   
   TMARSParams = 
      record
      CoreSize  ,
      MaxProcs  ,
      MaxCycles ,
      MinDist   ,
      MaxLength : Int64;
      end;
      
const 
   ZeroScore: TScore = (
      W: 0; L: 0; T: 0; Rounds: 0;
      IntTaken: 0; IntGiven: 0;
         Taken: 0;    Given: 0;
      Cycles: 0;
   ); 

var
   ModTable:  array [-(MaxCoreSize - 1) .. 2 * (MaxCoreSize - 1)] of Int64;

const 
   InitInstruction:  TInstruction = 
   (  Operation   :  opDAT   ; Modifier   : modF;
      AMode       :  amDirect; BMode      : amDirect;
      AField      :  0       ; BField     : 0                 
   );
   InstrName:  array [TOperation] of string = 
   (  'dat', 'mov', 'nop',
      'add', 'sub', 'mul', 'div', 'mod',
      'jmp', 'jmz', 'jmn', 'djn', 'spl',
      'seq', 'sne', 'slt'
   );
   ModifierName:  array [TModifier] of string = 
      ('f', 'x', 'i', 'a', 'b', 'ab', 'ba');
   AddrModeName:  array [TAddrMode] of string = 
      ('$', '#', '*', '{', '}', '@', '<', '>');
      
{$IFNDEF lib}
{ - - - - - - - - - - - << Warriors >> - - - - - - - - - - - - - - - - - - - - }
// Dest := Src
procedure AssignWarrior(var Src, Dest: TMARSWarrior);

// Load `Warrior` from a precompiled file at `Path`. If `Allocate` is `True`, 
// new memory is allocated for the warrior code, otherwise it is assumed to be 
// preallocated. A- and B-fields are reduced modulo `MARSParams.CoreSize`. In 
// case of an error, error message is returned in a newly allocated string.
// The returned value is the length of the error message or 0 if no errors
// were encountered.
function LoadWarrior(
   var   Warrior     : TMARSWarrior; 
         Path        : PChar;
   var   MARSParams  : TMarsParams;
         Allocate    : Boolean;
   var   ErrorMessage: PChar
         )           : Integer;

{ - - - - - - - - - - - << Simulation >> - - - - - - - - - - - - - - - - - - - }

// Initialize MARS with provided simulation parameters. Must be called once
// before any calls to other MARS functions. Must be called again if MARS
// parameters change.
procedure InitMARS(var MARSParams: TMARSParams);

// Play a match with a given number of `Rounds` between `Warrior1` and 
// `Warrior2`  with random starting positions and move orders, save the outcome
// in `Scores`. `Core` and `Queues` must point to existing arrays of appropriate
// sizes.
procedure PlayMatchRandom(
   var   Warrior1    : TMARSWarrior;
   var   Warrior2    : TMARSWarrior;
         Core        : PCore;
         Queues      : PQueues;
   var   MARSParams  : TMARSParams;
         Rounds      : Int64;
   var   Score       : TScore);
         
// Play a match between `Warrior1` and `Warrior2` with all possible permutations
// of starting positions and move orders, save the outcome in `Scores`. `Core` 
// and `Queues` must point to existing arrays of appropriate sizes.
procedure PlayMatchPermute(
   var   Warrior1    : TMARSWarrior;
   var   Warrior2    : TMARSWarrior;
         Core        : PCore;
         Queues      : PQueues;
   var   MARSParams  : TMARSParams;
   var   Score       : TScore);
   
// Run the simulation from the given state of `Core` and `Queues`. Execution
// starts from the queue with 0-based `SideToMove` index. Increment WLT and 
// cycle count of `Score`.
procedure RunSimulation(
         Core        : PCore;
         Queues      : PQueues;
         SideToMove  : Int64;
   var   MARSParams  : TMarsParams;
   var   Score       : TScore);
   
// Load `Warrior` into `Core` at a given `Position`, initialize `Queue` with
// a single proccess. 
procedure LoadIntoCore(
         Core        : PCore; 
   var   Queue       : TQueue;
   var   Warrior     : TMARSWarrior; 
         Position    : Int64;
   var   MARSParams  : TMARSParams);

// Fill the core with dat.f $0, $0
procedure ClearCore(
         Core        : PCore; 
         CoreSize    : Int64);

{ - - - - - - - - - - - << Output >> - - - - - - - - - - - - - - - - - - - - - }

// Return a string form of `Instr`
function InstrToString(var Instr: TInstruction): AnsiString;

// Dump the text representation of `Core` to `Path`.
procedure DumpCore(
   const Path        : AnsiString; 
   const Core        : PCore;
         Queues      : PQueues;
   const MARSParams  : TMARSParams;
   const Score       : TScore);
   
{ - - - - - - - - - - - << Memory Management  >> - - - - - - - - - - - - - - - }
   
// Allocate memory for the `Queue`'s active and free process arrays
procedure AllocQueue(
   var   Queue       : TQueue;
   var   MARSParams  : TMARSParams
         );
   
// Free the memory allocated for the `Queue`'s process arrays
procedure FreeQueue(
   var   Queue       : TQueue;
   var   MARSParams  : TMARSParams
         );
   
// Return a pointer to the newly allocated queues
function CreateQueues(
   var   MARSParams  : TMARSParams
         )           : PQueues;
   
// Free the memory allocated for `Queues`, including nested process arrays
procedure DestroyQueues(
         Queues      : PQueues;
   var   MARSParams  : TMARSParams);

implementation
uses SysUtils;
{$ENDIF}

{-----------------------<< Math >>---------------------------------------------}

// Return x mod m for x in [0, 2m - 1]
function ModInc(x: Int64; m: Int64): Int64; inline;
   begin
   if x >= m then
      Result := x - m
   else
      Result := x;
   end;
   
// Return x mod m for x in [-m, m - 1]
function ModDec(x: Int64; m: Int64): Int64; inline;
   begin
   if x < 0 then
      Result := x + m
   else
      Result := x;
   end;
   
// CoreWar-style mod
function Modulo(n, m: Int64): Int64;
   begin
   if n >= 0 then
      Result := n mod m
   else
      Result := m + n mod m;
   end;

(*
// Return x mod m for x in [0, 2m - 1]
function ModInc(x: Int64; m: Int64): Int64; 
   asm
   mov    r8 , x     // Store x - m
   sub    r8 , m
   cmp    x  , m     // Compare x and m
   mov    rax, x
   cmovge rax, r8    // Conditionally move if x >= m        
   end;  
   
// Return x mod m for x in [-m, m - 1]
function ModDec(x: Int64; m: Int64): Int64;
   asm
   mov    r8 , x     // Store x + m
   add    r8 , m
   cmp    x  , 0     // Compare x with 0
   mov    rax, x
   cmovl  rax, r8    // Conditionally move if x < 0
   end; 
*)

{-----------------------<< Core and Warrior Operations >>----------------------}

// Dest := Src
procedure AssignWarrior(var Src, Dest: TMARSWarrior);
   var 
         i  :  Integer;
   begin
   if Dest._ <> nil then
      FreeMem(Dest._, SizeOf(TInstruction) * Dest.Len);
   GetMem(Dest._, SizeOf(TInstruction) * Src.Len);
   Dest.Len := Src.Len;
   Dest.Org := Src.Org;
   for i := 0 to Dest.Len - 1 do
      Dest._[i] := Src._[i]
   end;
   
// Free the memory allocaded for `Warrior`'s code
procedure FreeWarrior(
   var   Warrior     : TMARSWarrior); 
   begin
   FreeMem(Warrior._, SizeOf(TInstruction) * Warrior.Len);
   end;
   
// Fill the core with dat.f $0, $0
procedure ClearCore(
         Core        : PCore; 
         CoreSize    : Int64
         ); cdecl;
   begin
   FillChar(Core[0], CoreSize * SizeOf(Core[0]), 0);
   end;

// Load `Warrior` into `Core` at a given `Position`, initialize `Queue` with
// a single proccess. 
procedure LoadIntoCore(
         Core        : PCore; 
   var   Queue       : TQueue;
   var   Warrior     : TMARSWarrior; 
         Position    : Int64;
   var   MARSParams  : TMARSParams
         ); cdecl;
   var 
         i, x        : Int64;
   begin
   // Place the warrior
   x := position;
   for i := 0 to Warrior.Len - 1 do
      begin
      Core[x] := Warrior._[i];
      x := ModInc(x + 1, MARSParams.CoreSize);
      end;
      
   // Initialize process queue
   Queue.Nprocs := 1;
   Queue.current := @Queue.Processes[0];
   Queue.Processes[0].Next := @Queue.Processes[0];
   Queue.Processes[0].Prev := @Queue.Processes[0];
   Queue.Processes[0].Location := ModTable[position + Warrior.org];
   for i := 0 to MARSParams.MAXPROCS - 1 do
      Queue.Free[i] := @Queue.Processes[i];
end;

// Load `Warrior1` at address 0, and `Warrior2` at a random address subject to
// minimum distance constraint, initialize the respective `Queues`.
procedure RandomLoadIntoCore(
         Core        : PCore; 
         Queues      : PQueues;
   var   Warrior1    : TMARSWarrior; 
   var   Warrior2    : TMARSWarrior; 
   var   MARSParams  : TMARSParams); cdecl;
   var
         x           : Int64;
   begin
   with MARSParams do
      x := MinDist + Random(CoreSize - 2 * MinDist + 1);
   LoadIntoCore(Core, Queues[0], Warrior1, {Position:} 0, MARSParams);
   LoadIntoCore(Core, Queues[1], Warrior2, {Position:} x, MARSParams);
   end;
   
{-----------------------<< Processes and Queues >>-----------------------------}

// Kill `CurrentProc` and add it to the free process pool
procedure KillProc(q: PQueue; CurrentProc: PProc);
   begin
   q.Free[q.NProcs - 1] := CurrentProc;
   CurrentProc.Prev.Next := CurrentProc.Next;
   CurrentProc.Next.Prev := CurrentProc.Prev;
   Dec(q.NProcs);
   end;

// Increment `CurrentProc`'s location
procedure IncProc(CurrentProc: PProc); inline;
   begin
   CurrentProc.Location := ModTable[CurrentProc.Location + 1];
   end;

// Increment `CurrentProc`'s location by 1 or 2 if `Condition` is `True`.
procedure IncIncProc(CurrentProc: PProc; Condition: Boolean); inline;
   begin
   CurrentProc.Location := ModTable[CurrentProc.Location + 1 + Ord(Condition)];
   end;
   
// Allocate memory for the `Queue`'s active and free process arrays
procedure AllocQueue(
   var   Queue       : TQueue;
   var   MARSParams  : TMARSParams
         );
   begin
   GetMem(Queue.Processes, MARSParams.MaxProcs * SizeOf(TProcess));
   GetMem(Queue.Free     , MARSParams.MaxProcs * SizeOf(PProc));
   end;
   
// Free the memory allocated for the `Queue`'s process arrays
procedure FreeQueue(
   var   Queue       : TQueue;
   var   MARSParams  : TMARSParams
         );
   begin
   FreeMem(Queue.Processes, MARSParams.MaxProcs * SizeOf(TProcess));
   FreeMem(Queue.Free   , MARSParams.MaxProcs * SizeOf(PProc));
   end;
   
// Return a pointer to the newly allocated queues
function CreateQueues(
   var   MARSParams  : TMARSParams
         )           : PQueues;
         cdecl;
   begin
   New(Result);
   AllocQueue(Result^[0], MARSParams);
   AllocQueue(Result^[1], MARSParams);
   end;
   
// Free the memory allocated for `Queues`, including nested process arrays
procedure DestroyQueues(
         Queues      : PQueues;
   var   MARSParams  : TMARSParams
         ); cdecl;
   begin
   FreeQueue(Queues^[0], MARSParams);
   FreeQueue(Queues^[1], MARSParams);
   Dispose(Queues);
   end;
   
{-----------------------<< Eval Procedures >>----------------------------------}
// Procedures for evaluating different combinations of effective addresses and 
// registers

procedure EvalRegAi(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.AMode of 
      amImmediate:
         begin
         Regs.AddrA := Regs.AddrI;
         Regs.A := Core[Regs.AddrA];
         end;

      amDirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.A := Core[Regs.AddrA];
         end;

      amAIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A := Core[Regs.AddrA];
         end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A := Core[Regs.AddrA];
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A := Core[Regs.AddrA];
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A := Core[Regs.AddrA];
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A := Core[Regs.AddrA];
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A := Core[Regs.AddrA];
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegBi(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.BMode of 
      amImmediate:
         begin
         Regs.AddrB := Regs.AddrI;
         Regs.B := Core[Regs.AddrB];
         end;

      amDirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.B := Core[Regs.AddrB];
         end;

      amAIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B := Core[Regs.AddrB];
         end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B := Core[Regs.AddrB];
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B := Core[Regs.AddrB];
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B := Core[Regs.AddrB];
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B := Core[Regs.AddrB];
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B := Core[Regs.AddrB];
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegAa(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.AMode of 
      amImmediate:
         begin
         Regs.AddrA := Regs.AddrI;
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amDirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amAIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegBa(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.BMode of 
      amImmediate:
         begin
         Regs.AddrB := Regs.AddrI;
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amDirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amAIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegAb(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.AMode of 
      amImmediate:
         begin
         Regs.AddrA := Regs.AddrI;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amDirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.BField := Core[Regs.AddrA].BField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;

end;

procedure EvalRegBb(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.BMode of 
      amImmediate:
         begin
         Regs.AddrB := Regs.AddrI;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amDirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.BField := Core[Regs.AddrB].BField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegAf(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.AMode of 
      amImmediate:
         begin
         Regs.AddrA := Regs.AddrI;
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amDirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Regs.A.AField := Core[Regs.AddrA].AField;
         Regs.A.BField := Core[Regs.AddrA].BField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalRegBf(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.BMode of 
      amImmediate:
         begin
         Regs.AddrB := Regs.AddrI;
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amDirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Regs.B.AField := Core[Regs.AddrB].AField;
         Regs.B.BField := Core[Regs.AddrB].BField;
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalAddrA(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.AMode of 
      amImmediate:
         Regs.AddrA := Regs.AddrI;

      amDirect:
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];

      amAIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].AField];
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         AddrInc := Regs.AddrA;
         Regs.AddrA := ModTable[Regs.AddrA + Core[Regs.AddrA].BField];
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalAddrB(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   var
         AddrInc  :  Int64;
   begin
   case Regs.I.BMode of 
      amImmediate:
         Regs.AddrB := Regs.AddrI;

      amDirect:
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];

      amAIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].AField];
         Core[AddrInc].AField := ModTable[Core[AddrInc].AField + 1];
         end;

      amBIndirect:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         AddrInc := Regs.AddrB;
         Regs.AddrB := ModTable[Regs.AddrB + Core[Regs.AddrB].BField];
         Core[AddrInc].BField := ModTable[Core[AddrInc].BField + 1];
         end;
   end;
end;

procedure EvalNullA(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   begin
   case Regs.I.AMode of 
      amImmediate, amDirect, amAIndirect, amBIndirect:
         begin end;

      amAPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField - 1];
         end;

      amAPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         Core[Regs.AddrA].AField := ModTable[Core[Regs.AddrA].AField + 1];
         end;

      amBPreDec:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrADec := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField - 1];
         end;

      amBPostInc:
         begin
         Regs.AddrA := ModTable[Regs.AddrI + Regs.I.AField];
         Regs.AddrAInc := Regs.AddrA;
         Core[Regs.AddrA].BField := ModTable[Core[Regs.AddrA].BField + 1];
         end;
   end;
end;

procedure EvalNullB(
         Core     : PCore;
   var   Regs     : TRegisters); inline;
   begin
   case Regs.I.BMode of 
      amImmediate, amDirect, amAIndirect, amBIndirect:
         begin end;

      amAPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
         end;

      amAPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField + 1];
         end;

      amBPreDec:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBDec := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
         end;

      amBPostInc:
         begin
         Regs.AddrB := ModTable[Regs.AddrI + Regs.I.BField];
         Regs.AddrBInc := Regs.AddrB;
         Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField + 1];
         end;
   end;
end;

{-----------------------<< Operation Procedures >>-----------------------------}
// Procedures for executing different combinations of operations and modifiers

procedure pDAT(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalNullA(Core, Regs);
   EvalNullB(Core, Regs);
   KillProc(q, CurrentProc);
   end;

procedure pMOVa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].AField := Regs.A.AField;
   IncProc(CurrentProc);
   end;

procedure pMOVb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].BField := Regs.A.BField;
   IncProc(CurrentProc);
   end;

procedure pMOVab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].BField := Regs.A.AField;
   IncProc(CurrentProc);
   end;

procedure pMOVba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].AField := Regs.A.BField;
   IncProc(CurrentProc);
   end;

procedure pMOVf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].AField := Regs.A.AField;
   Core[Regs.AddrB].BField := Regs.A.BField;
   IncProc(CurrentProc);
   end;

procedure pMOVx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB].BField := Regs.A.AField;
   Core[Regs.AddrB].AField := Regs.A.BField;
   IncProc(CurrentProc);
   end;

procedure pMOVi(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAi(Core, Regs);
   EvalAddrB(Core, Regs);
   Core[Regs.AddrB] := Regs.A;
   IncProc(CurrentProc);
   end;

procedure pNOP(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalNullA(Core, Regs);
   EvalNullB(Core, Regs);
   IncProc(CurrentProc);
   end;

procedure pADDa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField + Regs.A.AField];
   IncProc(CurrentProc);
   end;

procedure pADDb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField + Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pADDab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField + Regs.A.AField];
   IncProc(CurrentProc);
   end;

procedure pADDba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField + Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pADDf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField + Regs.A.AField];
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField + Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pADDx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField + Regs.A.AField];
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField + Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pSUBa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField - Regs.A.AField];
   IncProc(CurrentProc);
   end;

procedure pSUBb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField - Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pSUBab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField - Regs.A.AField];
   IncProc(CurrentProc);
   end;

procedure pSUBba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField - Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pSUBf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField - Regs.A.AField];
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField - Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pSUBx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Regs.B.BField - Regs.A.AField];
   Core[Regs.AddrB].AField := ModTable[Regs.B.AField - Regs.A.BField];
   IncProc(CurrentProc);
   end;

procedure pMULa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := 
      (Regs.B.AField * Regs.A.AField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pMULb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := 
      (Regs.B.BField * Regs.A.BField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pMULab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := 
      (Regs.B.BField * Regs.A.AField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pMULba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := 
      (Regs.B.AField * Regs.A.BField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pMULf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].AField := 
      (Regs.B.AField * Regs.A.AField) mod MARSParams.CoreSize;
   Core[Regs.AddrB].BField := 
      (Regs.B.BField * Regs.A.BField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pMULx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].BField := 
      (Regs.B.BField * Regs.A.AField) mod MARSParams.CoreSize;
   Core[Regs.AddrB].AField := 
      (Regs.B.AField * Regs.A.BField) mod MARSParams.CoreSize;
   IncProc(CurrentProc);
   end;

procedure pDIVa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.A.AField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.AField;
      IncProc(CurrentProc);
      end;
   end;

procedure pDIVb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.A.BField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.BField;
      IncProc(CurrentProc);
      end;
   end;

procedure pDIVab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.A.AField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.AField;
      IncProc(CurrentProc);
      end;
   end;

procedure pDIVba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.A.BField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.BField;
      IncProc(CurrentProc);
      end;
   end;

procedure pDIVf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.A.AField<>0)and(Regs.A.BField<>0) then
      begin
      Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.AField;
      Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.BField;
      IncProc(CurrentProc);
      end
   else
      begin
      if (Regs.A.AField<>0) then
         Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.AField;
      if (Regs.A.BField<>0) then
         Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.BField;
      KillProc(q, CurrentProc);
      end;
   end;

procedure pDIVx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.A.AField<>0)and(Regs.A.BField<>0) then
      begin
      Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.BField;
      Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.AField;
      IncProc(CurrentProc);
      end
   else
      begin
      if (Regs.A.BField<>0) then
         Core[Regs.AddrB].AField := Regs.B.AField div Regs.A.BField;
      if (Regs.A.AField<>0) then
         Core[Regs.AddrB].BField := Regs.B.BField div Regs.A.AField;
      KillProc(q, CurrentProc);
      end;
   end;

procedure pMODa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.A.AField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.AField;
      IncProc(CurrentProc);
      end;
   end;

procedure pMODb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.A.BField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.BField;
      IncProc(CurrentProc);
      end;
   end;

procedure pMODab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.A.AField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.AField;
      IncProc(CurrentProc);
      end;
   end;

procedure pMODba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.A.BField=0 then
      KillProc(q, CurrentProc)
   else
      begin
      Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.BField;
      IncProc(CurrentProc);
      end;
   end;

procedure pMODf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.A.AField<>0)and(Regs.A.BField<>0) then
      begin
      Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.AField;
      Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.BField;
      IncProc(CurrentProc);
      end
   else
      begin
      if (Regs.A.AField<>0) then
         Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.AField;
      if (Regs.A.BField<>0) then
         Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.BField;
      KillProc(q, CurrentProc);
      end;
end;

procedure pMODx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.A.AField<>0)and(Regs.A.BField<>0) then
      begin
      Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.BField;
      Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.AField;
      IncProc(CurrentProc);
      end
   else
      begin
      if (Regs.A.BField<>0) then
         Core[Regs.AddrB].AField := Regs.B.AField mod Regs.A.BField;
      if (Regs.A.AField<>0) then
         Core[Regs.AddrB].BField := Regs.B.BField mod Regs.A.AField;
      KillProc(q, CurrentProc);
      end;
end;

procedure pJMP(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalNullB(Core, Regs);
   CurrentProc.Location := Regs.AddrA;
   end;

procedure pJMZa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.B.AField = 0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pJMZb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.B.BField = 0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pJMZf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.B.AField = 0) and (Regs.B.BField=  0) then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pJMNa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBa(Core, Regs);
   if Regs.B.AField<>0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pJMNb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBb(Core, Regs);
   if Regs.B.BField <> 0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pJMNf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBf(Core, Regs);
   if (Regs.B.AField <> 0) or (Regs.B.BField <> 0) then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pDJNa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBa(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
   Regs.B.AField := ModTable[      Regs.B.AField - 1];
   if Regs.B.AField <> 0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pDJNb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBb(Core, Regs);
   Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
   Regs.B.BField := ModTable[      Regs.B.BField - 1];
   if Regs.B.BField <> 0 then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pDJNf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalAddrA(Core, Regs);
   EvalRegBf(Core, Regs);
   Core[Regs.AddrB].AField := ModTable[Core[Regs.AddrB].AField - 1];
   Regs.B.AField := ModTable[      Regs.B.AField - 1];
   Core[Regs.AddrB].BField := ModTable[Core[Regs.AddrB].BField - 1];
   Regs.B.BField := ModTable[      Regs.B.BField - 1];
   if (Regs.B.AField <> 0) or (Regs.B.BField <> 0) then
      CurrentProc.Location := Regs.AddrA
   else
      IncProc(CurrentProc);
   end;

procedure pSPL(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   var
         NewProc     : PProc;
   begin
   EvalAddrA(Core, Regs);
   EvalNullB(Core, Regs);
   if q.Nprocs <> MARSParams.maxprocs then
      begin
      NewProc := q.Free[q.Nprocs];
      NewProc.Next := CurrentProc.Next;
      NewProc.Prev := CurrentProc;
      CurrentProc.Next.Prev := NewProc;
      CurrentProc.Next := NewProc;
      NewProc.Location := Regs.AddrA;
      inc(q.Nprocs);
      end;
   IncProc(CurrentProc);
end;

procedure pSEQa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField = Regs.B.AField);
   end;

procedure pSEQb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField = Regs.B.BField);
   end;

procedure pSEQab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField = Regs.B.BField);
   end;

procedure pSEQba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField = Regs.B.AField)
   end;

procedure pSEQf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc, 
      (Regs.A.AField = Regs.B.AField) and (Regs.A.BField = Regs.B.BField)
   );
   end;

procedure pSEQx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc, 
      (Regs.A.AField = Regs.B.BField) and (Regs.A.BField = Regs.B.AField)
   );
   end;

procedure pSEQi(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAi(Core, Regs);
   EvalRegBi(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField    = Regs.B.AField   ) and 
      (Regs.A.BField    = Regs.B.BField   ) and
      (Regs.A.Operation = Regs.B.Operation) and 
      (Regs.A.modifier  = Regs.B.modifier ) and
      (Regs.A.AMode     = Regs.B.AMode    ) and 
      (Regs.A.BMode     = Regs.B.BMode    )
   );
   end;

procedure pSNEa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField <> Regs.B.AField);
   end;

procedure pSNEb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField <> Regs.B.BField);
   end;

procedure pSNEab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField <> Regs.B.BField);
   end;

procedure pSNEba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField <> Regs.B.AField);
   end;

procedure pSNEf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField <> Regs.B.AField) or (Regs.A.BField <> Regs.B.BField)
   );
   end;

procedure pSNEx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField <> Regs.B.BField) or (Regs.A.BField <> Regs.B.AField)
   );
   end;

procedure pSNEi(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAi(Core, Regs);
   EvalRegBi(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField    <> Regs.B.AField   ) or 
      (Regs.A.BField    <> Regs.B.BField   ) or
      (Regs.A.Operation <> Regs.B.Operation) or 
      (Regs.A.Modifier  <> Regs.B.Modifier ) or
      (Regs.A.AMode     <> Regs.B.AMode    ) or 
      (Regs.A.BMode     <> Regs.B.BMode    )
   );
   end;

procedure pSLTa(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField < Regs.B.AField);
   end;

procedure pSLTb(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField < Regs.B.BField);
   end;

procedure pSLTab(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAa(Core, Regs);
   EvalRegBb(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.AField < Regs.B.BField)
   end;

procedure pSLTba(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAb(Core, Regs);
   EvalRegBa(Core, Regs);
   IncIncProc(CurrentProc, Regs.A.BField < Regs.B.AField)
   end;

procedure pSLTf(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField < Regs.B.AField) and (Regs.A.BField < Regs.B.BField)
   );
   end;

procedure pSLTx(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);
   begin
   EvalRegAf(Core, Regs);
   EvalRegBf(Core, Regs);
   IncIncProc(
      CurrentProc,
      (Regs.A.AField < Regs.B.BField) and (Regs.A.BField < Regs.B.AField)
   );
   end;

type ProcInstr = procedure(
         Core        : PCore;
         q           : PQueue;
         CurrentProc : PProc;
   var   Regs        : TRegisters;
   var   MARSParams  : TMARSParams);

const 
   ProcArr: array[TOperation, TModifier] of ProcInstr = 
   (  (pDAT , pDAT , pDAT , pDAT , pDAT , pDAT  , pDAT  ),
      (pMOVf, pMOVx, pMOVi, pMOVa, pMOVb, pMOVab, pMOVba),
      (pNOP , pNOP , pNOP , pNOP , pNOP , pNOP  , pNOP  ),
      (pADDf, pADDx, pADDf, pADDa, pADDb, pADDab, pADDba),
      (pSUBf, pSUBx, pSUBf, pSUBa, pSUBb, pSUBab, pSUBba),
      (pMULf, pMULx, pMULf, pMULa, pMULb, pMULab, pMULba),
      (pDIVf, pDIVx, pDIVf, pDIVa, pDIVb, pDIVab, pDIVba),
      (pMODf, pMODx, pMODf, pMODa, pMODb, pMODab, pMODba),
      (pJMP , pJMP , pJMP , pJMP , pJMP , pJMP  , pJMP  ),
      (pJMZf, pJMZf, pJMZf, pJMZa, pJMZb, pJMZb , pJMZa ),
      (pJMNf, pJMNf, pJMNf, pJMNa, pJMNb, pJMNb , pJMNa ),
      (pDJNf, pDJNf, pDJNf, pDJNa, pDJNb, pDJNb , pDJNa ),
      (pSPL , pSPL , pSPL , pSPL , pSPL , pSPL  , pSPL  ),
      (pSEQf, pSEQx, pSEQi, pSEQa, pSEQb, pSEQab, pSEQba),
      (pSNEf, pSNEx, pSNEi, pSNEa, pSNEb, pSNEab, pSNEba),
      (pSLTf, pSLTx, pSLTf, pSLTa, pSLTb, pSLTab, pSLTba)
   );

{-----------------------<< Simulation >>---------------------------------------}
   
// Initialize MARS with provided simulation parameters. Must be called once
// before any calls to other MARS functions. Must be called again if MARS
// parameters change.
procedure InitMARS(var MARSParams: TMARSParams); cdecl;
   var 
         i     :  Int64;
         csm1  :  Int64;
   begin
   csm1 := MARSParams.CORESIZE - 1;
   for i := -csm1 to 2 * csm1 do
      ModTable[i] := Modulo(i, MARSParams.CORESIZE);
   end;
   
// Run the simulation from the given state of `Core` and `Queues`. Execution
// starts from the queue with 0-based `SideToMove` index. Increment WLT and 
// cycle count of `Score`.
procedure RunSimulation(
         Core              : PCore;
         Queues            : PQueues;
         SideToMove        : Int64;
   var   MARSParams        : TMarsParams;
   var   Score             : TScore
         ); cdecl;
   var
         Cycle, Cycles     : Int64;
         CurrentProc,
         NextProc          : PProc;
         Regs              : TRegisters;
         qSideToMove,
         qOtherSide        : PQueue;
   begin
   Cycles := MARSParams.MaxCycles;
   qSideToMove := @Queues[    SideToMove];
   qOtherSide  := @Queues[1 - SideToMove];
   for Cycle := 1 to MARSParams.MaxCycles do
      begin     
      // 1st warrior
      CurrentProc := qSideToMove.current;
      NextProc := CurrentProc.Next;
      Regs.AddrI := CurrentProc.Location;
      Regs.I := Core[Regs.AddrI];
      ProcArr[Regs.I.Operation, Regs.I.modifier](
         Core, qSideToMove, CurrentProc, Regs, MARSParams
      );
      qSideToMove.current := NextProc;
      if qSideToMove.Nprocs = 0 then
         begin
         Cycles := Cycle;
         break;
         end;
         
      // 2nd warrior
      CurrentProc := qOtherSide.current;
      NextProc := CurrentProc.Next;
      Regs.AddrI := CurrentProc.Location;
      Regs.I := Core[Regs.AddrI];
      ProcArr[Regs.I.Operation, Regs.I.modifier](
         Core, qOtherSide, CurrentProc, Regs, MARSParams
      );
      qOtherSide.current := NextProc;
      if qOtherSide.Nprocs = 0 then
         begin
         Cycles := Cycle;
         break;
         end;
      end;
   Score.W += Ord( Queues[1].NProcs = 0 );
   Score.L += Ord( Queues[0].NProcs = 0 );
   Score.T += Ord((Queues[0].NProcs > 0) and (Queues[1].NProcs > 0));
   Score.Cycles += Cycles;
   end;
   
function SpacetimeDiagram(
         Core              : PCore;
         Queues            : PQueues;
         SideToMove        : Int64;
   var   MARSParams        : TMarsParams;
   var   Score             : TScore
         )                 : AnsiString; 
   var
         Cycle, Cycles     : Int64;
         CurrentProc,
         NextProc          : PProc;
         Regs              : TRegisters;
         qSideToMove,
         qOtherSide        : PQueue;
   begin
   Cycles := MARSParams.MaxCycles;
   ClearCore(Core, MARSParams.CoreSize);
   for Cycle := 1 to MARSParams.MaxCycles do
      begin     
      CurrentProc := q.current;
      NextProc := CurrentProc.Next;
      Regs.AddrI := CurrentProc.Location;
      Regs.I := Core[Regs.AddrI];
      Regs.AddrADec := None;
      Regs.AddrBDec := None;
      Regs.AddrAInc := None;
      Regs.AddrBInc := None;
      ProcArr[Regs.I.Operation, Regs.I.modifier](
         Core, q, CurrentProc, Regs, MARSParams
      );
      
      // #TODO specific inc/dec, {}<>
      if Regs.AddrADec <> None then
         row[1 + Regs.AddrADec] := '-';
      if Regs.AddrBDec <> None then
         row[1 + Regs.AddrBDec] := '-';
      if Regs.AddrAInc <> None then
         row[1 + Regs.AddrAInc] := '+';
      if Regs.AddrBInc <> None then
         row[1 + Regs.AddrBInc] := '+';
      row[Regs.AddrI] := '.';
         
      case Regs.I.Operation of
         opMOV: row[1 + Regs.AddrB] := 'X';
         opADD, opSUB, opMUL, opDIV, opMOD: row[1 + Regs.AddrB] := '*';
         opJMZ, opJMN: row[1 + Regs.AddrB] := '?';
         opDJN: row[1 + Regs.AddrB] := '-';
         opSPL: row[1 + Regs.AddrA] := '!';
         opSEQ, opSNE: 
            begin
            row[1 + Regs.AddrA] := '?';
            row[1 + Regs.AddrB] := '?';
            end;
      end
      if q.Nprocs = 0 then
         break;
      end;
   end;

   
{-----------------------<< Matches >>------------------------------------------}
   
// Calculate the rest of `Score` fields based on WLT counts
procedure CalcScore(var Score: TScore); cdecl;
   begin
   with Score do
      begin
      Rounds := W + L + T;
      IntTaken := 3 * W + T;
      IntGiven := 3 * L + T;
      Taken := 100 * IntTaken / Rounds;
      Given := 100 * IntGiven / Rounds;
      end;
   end;

// Play a match with a given number of `Rounds` between `Warrior1` and 
// `Warrior2` with random starting positions and move orders, save the outcome
// in `Scores`. `Core` and `Queues` must point to existing arrays of appropriate
// sizes.
procedure PlayMatchRandom(
   var   Warrior1    : TMARSWarrior;
   var   Warrior2    : TMARSWarrior;
         Core        : PCore;
         Queues      : PQueues;
   var   MARSParams  : TMARSParams;
         Rounds      : Int64;
   var   Score       : TScore
         ); cdecl;
   var                 
         i           : Int64;
   begin
   Score := ZeroScore;
   for i := 1 to Rounds do
      begin
      ClearCore(Core, MARSParams.CoreSize);
      RandomLoadIntoCore(Core, Queues, Warrior1, Warrior2, MARSParams);
      RunSimulation(
         Core, Queues, {SideToMove:} Random(2), MARSParams, Score
      );
      end;
   CalcScore(Score);
   end;

// Play a match between `Warrior1` and `Warrior2` with all possible permutations
// of starting positions and move orders, save the outcome in `Scores`. `Core` 
// and `Queues` must point to existing arrays of appropriate sizes.
procedure PlayMatchPermute(
   var   Warrior1      : TMARSWarrior;
   var   Warrior2      : TMARSWarrior;
         Core          : PCore;
         Queues        : PQueues;
   var   MARSParams    : TMARSParams;
   var   Score         : TScore
         ); cdecl;
   var 
         i, SideToMove :  Int64;
   begin
   Score := ZeroScore;
   for SideToMove := 0 to 1 do
      begin
      for i := MARSParams.MinDist to MARSParams.CoreSize - MARSParams.MinDist do
         begin
         ClearCore(Core, MARSParams.CoreSize);
         LoadIntoCore(Core, Queues[0], Warrior1, 0, MARSParams);
         LoadIntoCore(Core, Queues[1], Warrior2, i, MARSParams);
         RunSimulation(Core, Queues, SideToMove, MARSParams, Score);
         end;
      end;
   CalcScore(Score);
   end;

{-----------------------<< Output >>-------------------------------------------}

// Return a string form of `Instr`
function InstrToString(var Instr: TInstruction): AnsiString;
   var 
         s, x  :  AnsiString;
   begin
   s := instrName[Instr.Operation] + '.' + ModifierName[Instr.modifier];
   if length(s) = 5 then 
      s := s + ' ';
   s := s + ' ' + AddrModeName[Instr.AMode] + ' ';
   Str(Instr.AField:5, x);
   s := s + x + ' , ' + AddrModeName[Instr.BMode] + ' ';
   Str(Instr.BField:5, x);
   s := s + x;
   Result := s;
   end;

// Dump the text representation of `Core` to `Path`.
procedure DumpCore(
   const Path        : AnsiString; 
   const Core        : PCore;
         Queues      : PQueues;
   const MARSParams  : TMARSParams;
   const Score       : TScore);
   var 
         f           :  Text;
         s           :  AnsiString;
         i, j        :  Integer;
         Proc        :  PProc;
   const
         AddrWidth   =  5;
   begin
   Assign(f, Path);
   Rewrite(f);
   WriteLn(f, IntToStr(Score.Cycles) + ' cycles elapsed');
   for j := 0 to 1 do
      begin
      WriteLn(f, 
         'Warrior ' + IntToStr(1 + j) + ', ' + 
         IntToStr(Queues[j].Nprocs) + ' processes:'
      );
      Proc := Queues[j].Current;
      for i := 1 to Queues[j].NProcs do
         begin
         Str(Proc.Location: AddrWidth, s);
         WriteLn(f, s + ' | ' + InstrToString(Core[Proc.Location]));
         Proc := Proc.Next;
         end;
      end;
   WriteLn(f, 'Core:');
   for i := 0 to MARSParams.CoreSize - 1 do
      begin
      Str(i: AddrWidth, s);
      WriteLn(f, s + ' | ' + InstrToString(Core[i]));
      end;
   Close(f);
   end;

{-----------------------<< Warrior Loading >>----------------------------------}

// Load `Warrior` from a precompiled file at `Path`. If `Allocate` is `True`, 
// new memory is allocated for the warrior code, otherwise it is assumed to be 
// preallocated. A- and B-fields are reduced modulo `MARSParams.CoreSize`. In 
// case of an error, error message is returned in a newly allocated string.
// The returned value is the length of the error message or 0 if no errors
// were encountered.
function LoadWarrior(
   var   Warrior     : TMARSWarrior; 
         Path        : PChar;
   var   MARSParams  : TMarsParams;
         Allocate    : Boolean;
   var   ErrorMessage: PChar
         )           : Integer;
         cdecl;
   var
         F           : Text;
         Line, Token,
         Error       : AnsiString;
         LineNum     : Integer;
         InstrCount  : Integer;
         OrgFound    : Boolean;
         i, Value    : Integer;
         Instr       : TInstruction;
         Code        : array of TInstruction;
  
   // Parse Operation from string
   function ParseOperation(const OpStr: AnsiString): TOperation;
      begin
      if      OpStr = 'DAT' then Result := opDAT
      else if OpStr = 'MOV' then Result := opMOV
      else if OpStr = 'NOP' then Result := opNOP
      else if OpStr = 'ADD' then Result := opADD
      else if OpStr = 'SUB' then Result := opSUB
      else if OpStr = 'MUL' then Result := opMUL
      else if OpStr = 'DIV' then Result := opDIV
      else if OpStr = 'MOD' then Result := opMOD
      else if OpStr = 'JMP' then Result := opJMP
      else if OpStr = 'JMZ' then Result := opJMZ
      else if OpStr = 'JMN' then Result := opJMN
      else if OpStr = 'DJN' then Result := opDJN
      else if OpStr = 'SPL' then Result := opSPL
      else if OpStr = 'SEQ' then Result := opSEQ
      else if OpStr = 'CMP' then Result := opSEQ
      else if OpStr = 'SNE' then Result := opSNE
      else if OpStr = 'SLT' then Result := opSLT
      else 
         begin
         Result := opDAT;
         Error := 'Unknown Operation: ' + OpStr;
         end;
   end;
  
   // Parse modifier from string
   function ParseModifier(const ModStr: AnsiString): TModifier;
      begin
      if      ModStr = 'A'  then Result := modA
      else if ModStr = 'B'  then Result := modB
      else if ModStr = 'AB' then Result := modAB
      else if ModStr = 'BA' then Result := modBA
      else if ModStr = 'F'  then Result := modF
      else if ModStr = 'X'  then Result := modX
      else if ModStr = 'I'  then Result := modI
      else 
         begin
         Result := modF;
         Error := 'Unknown modifier: ' + ModStr;
         end;
      end;
  
   // Parse addressing mode from character
   function ParseAddressingMode(const ModeChar: Char): TAddrMode;
      begin
         case ModeChar of
            '#': Result := amImmediate;
            '$': Result := amDirect;
            '*': Result := amAIndirect;
            '{': Result := amAPreDec;
            '}': Result := amAPostInc;
            '@': Result := amBIndirect;
            '<': Result := amBPreDec;
            '>': Result := amBPostInc;
            else 
               begin
               Result := amDirect;
               Error := 'Unknown addressing mode: ' + ModeChar;
               end;
         end;
      end;
  
   // Trim and extract the next token
   function ExtractToken(var Str: AnsiString): AnsiString;
      var
         i  : Integer;
      begin
      Str := Trim(Str);
      if Str = '' then
         begin
         Result := '';
         exit;
         end;
    
      // Find the first whitespace or special character
      i := 1;
      while (i <= Length(Str)) and not (Str[i] in 
         [' ', #9, '.', ',', '#', '$', '*', '{', '}', '@', '<', '>']) do
         Inc(i);
    
      if i > 1 then
         begin
         Result := Copy(Str, 1, i-1);
         Str := Copy(Str, i, Length(Str));
         end
      else
         begin
         Result := Str[1];
         Str := Copy(Str, 2, Length(Str));
         end;
    
      Str := Trim(Str);
      end;
  
   // Parse an integer field
   function ParseField(var Str: AnsiString): Integer;
      var
         NumStr: AnsiString;
         Code  : Integer;
      begin
      Str := Trim(Str);
    
      // Find the end of the number
      i := 1;
      while (i <= Length(Str)) and (Str[i] in ['-', '0'..'9']) do
         Inc(i);
    
      NumStr := Copy(Str, 1, i - 1);
      Str := Trim(Copy(Str, i, Length(Str)));
    
      Val(NumStr, Result, Code);
      if Code <> 0 then
         Error := 'Invalid number format: ' + NumStr;
      end;
  
   begin
   Error := '';
   InstrCount := 0;
   OrgFound := False;
   LineNum := 0;
  
   // Initialize warrior
   Warrior.Len := 0;
   Warrior.Org := 0;
   Warrior._   := nil;
   SetLength(Code, 0);
 
   // Open the file
   {$I-}
   Assign(F, Path);
   Reset(F);
   if IoResult <> 0 then
      begin
      Error := 'Could not open ' + Path;
      Result := Length(Error);
      ErrorMessage := StrAlloc(Length(Error) + 1);
      StrPCopy(ErrorMessage, Error);
{<}   exit;
      end;
   {$I+}
 
   // First pass: count instructions to allocate memory
   while not Eof(F) do
      begin
      ReadLn(F, Line);
      Inc(LineNum);
      Line := Trim(Line);
      if (Line = '') or (Line[1] = ';') then
         continue; 
      Token := ExtractToken(Line);
      if Token = 'END' then
   {<}   break
      else if Token = 'ORG' then
         continue
      else
         Inc(InstrCount);
      end;
 
   // Allocate memory for instructions
   SetLength(Code, InstrCount);
   Warrior.Len := InstrCount;
 
   // Check if warrior is too long
   if Warrior.Len > MARSParams.MaxLength then
      begin
      Error := 'Warrior exceeds maximum length';
      Result := Length(Error);
      ErrorMessage := StrAlloc(Length(Error) + 1);
      StrPCopy(ErrorMessage, Error);
      CloseFile(F);
{<}   exit;
      end;
 
   // Reset file for second pass
   Reset(F);
   LineNum := 0;
   InstrCount := 0;
 
   // Second pass: parse instructions
   while not Eof(F) do
      begin
      ReadLn(F, Line);
      Inc(LineNum);
      Line := Trim(Line);
      if (Line = '') or (Line[1] = ';') then
         continue;
   
      Token := ExtractToken(Line);
      if Token = 'END' then
   {<}   break
      else if Token = 'ORG' then
         begin
         if OrgFound then
            begin
            Error := 'Multiple ORG statements found';
   {<}      break
            end;
         OrgFound := True;
         Value := ParseField(Line);
         if (Value < 0) or (Value >= Warrior.Len) then
            begin
            Error := 'ORG value ' + IntToStr(Value) + ' out of range';
   {<}      break
            end;
         Warrior.Org := Value;
         end
      else
         begin
         // Parse Operation
         Instr.Operation := ParseOperation(Token);
         if Error <> '' then
   {<}      break;
         
         // Parse modifier
         if Line[1] <> '.' then
            begin
            Error := 'Expected modifier after Operation';
            break;
            end;
         Line := Copy(Line, 2, Length(Line));
         Token := ExtractToken(Line);
         Instr.Modifier := ParseModifier(Token);
         if Error <> '' then
   {<}      break;
         
         // Parse A addressing mode
         if Length(Line) = 0 then
            begin
            Error := 'Expected A addressing mode';
   {<}      break;
            end;
         Instr.AMode := ParseAddressingMode(Line[1]);
         if Error <> '' then
   {<}      break;
         Line := Copy(Line, 2, Length(Line));
         
         // Parse A field
         Instr.AField := Modulo(ParseField(Line), MARSParams.CoreSize);
         
         // Parse comma
         if (Length(Line) = 0) or (Line[1] <> ',') then
            begin
            Error := 'Expected comma after A field';
   {<}      break;
            end;
         Line := Trim(Copy(Line, 2, Length(Line)));
         
         // Parse B addressing mode
         if Length(Line) = 0 then
            begin
            Error := 'Expected B addressing mode';
   {<}      break;
            end;
         Instr.BMode := ParseAddressingMode(Line[1]);
         Line := Copy(Line, 2, Length(Line));
         
         // Parse B field
         Instr.BField := Modulo(ParseField(Line), MARSParams.CoreSize);
         
         // Store instruction
         Code[InstrCount] := Instr;
         Inc(InstrCount);
         end;
      end;
      
   // If no ORG was found, default to 0
   if not OrgFound then
      Warrior.Org := 0;
    
   CloseFile(F);
   if Error <> '' then
      begin
      Error := 'Error at line ' + IntToStr(LineNum) + ': ' + Error;
      Warrior.Len := 0;
      end
   else
      begin
      if Allocate then
         GetMem(Warrior._, SizeOf(TInstruction) * Warrior.Len);
      Move(Code[0], Warrior._^, SizeOf(TInstruction) * Warrior.Len);
      end;
      
   Result := Length(Error);
   if Result > 0 then
      begin
      ErrorMessage := StrAlloc(Length(Error) + 1);
      StrPCopy(ErrorMessage, Error);
      end;
end;

// Free the memory allocated for a string `S` such as an error message
procedure FreeString(S: PChar); 
   begin
   StrDispose(S); 
   end;

{-----------------------<< Exports >>------------------------------------------}

{$IFDEF lib}
exports 
   LoadWarrior, FreeWarrior, FreeString,
   ClearCore, LoadIntoCore, RandomLoadIntoCore, CreateQueues, DestroyQueues,
   InitMARS, RunSimulation, PlayMatchRandom, PlayMatchPermute;
{$ENDIF}

end.
