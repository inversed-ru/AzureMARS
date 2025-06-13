// This file is a part of AzureMARS, distributed under the MIT license.
// See LICENSE.md in the repository root for full license text.

// CHANGELOG
// 1.0 : 2025.05.21 
// - Major refactor removing global variables
// - Library compilation mode
// - Command-line interface
// - Julia bindings
// - Nano 100.9 MIPS, 94nop 112.4 MIPS
// 0.7 : 2025.03.26  
// - Autoformatted
// - Added performance and correctness tests
// - CMP is now recognized as opcode
// - Nano 58.9 MIPS, 94nop 63.9 MIPS
// 0.6 : 2008.04.03  
// - Bugfixes in effective address evaluation and dispatch table
// - Performance improvements
// 0.1 : 2007.07.13 
// - Initial version

program main; {$MODE OBJFPC}

uses
   SysUtils, Math, AzureMARS;
   
type
   TWarriors = 
      record
         N  :  Integer;
         _  :  array of TMARSWarrior;
      end;
      
   TCore = array of TInstruction;
   
   TErrorBuffer = array [0 .. 1023] of Char;
   
const 
   MARSParamsNano : TMARSParams = 
   (  CoreSize    : 80;
      MaxProcs    : 80;
      MaxCycles   : 800;
      MinDist     : 5;
      MaxLength   : 5;
   );
   MARSParamsTiny : TMARSParams = 
   (  CoreSize    : 800;
      MaxProcs    : 800;
      MaxCycles   : 8000;
      MinDist     : 20;
      MaxLength   : 20;
   );
   MARSParams94nop: TMARSParams = 
   (  CoreSize    : 8000;
      MaxProcs    : 8000;
      MaxCycles   : 80000;
      MinDist     : 100;
      MaxLength   : 100;
   );
   MARSParamsNull: TMARSParams = 
   (  CoreSize    : 0;
      MaxProcs    : 0;
      MaxCycles   : 0;
      MinDist     : 0;
      MaxLength   : 0;
   );
   
procedure InitWarriors(
   var   Warriors    :  TWarriors);
   begin
   Warriors.N := 0;
   SetLength(Warriors._, 0);
   end;
   
function AppendWarrior(
   var   Warriors    :  TWarriors;
   const Path        :  AnsiString;
   var   MARSParams  :  TMARSParams
         )           :  Boolean;
   var
         ErrorMessage:  PChar;
         ErrorLen    :  Integer;
   begin
   if Length(Warriors._) <= Warriors.N then
      SetLength(Warriors._, 2 * (Warriors.N + 1));
   ErrorLen := LoadWarrior(
      Warriors._[Warriors.N], PChar(Path), MARSParams, True, ErrorMessage
   );
   if ErrorLen <> 0 then
      begin
      WriteLn('Failed to load ' + Path + ': ' + StrPas(ErrorMessage));
      Result := False;
      end
   else
      begin
      Warriors.N += 1;
      Result := True;
      end;
   StrDispose(ErrorMessage);
   end;
   
function LoadWarriorList(
         PathList    :  AnsiString;
         WarriorDir  :  AnsiString;
   var   MARSParams  :  TMARSParams
   )                 :  TWarriors;
   var
         FileList    :  Text;
         Filename    :  AnsiString;
         Warriors    :  TWarriors;
   begin
   InitWarriors(Warriors);
   Assign(FileList, PathList);
   Reset(FileList);
   repeat
      ReadLn(FileList, Filename);
      AppendWarrior(Warriors, WarriorDir + Filename + '.rc', MARSParams);
   until EoF(FileList);
   Close(FileList);
   SetLength(Warriors._, Warriors.N);
   Result := Warriors;
   end;
   
procedure TestCorrectness;
   var
      FileList    :  Text;
      i, j        :  Integer;
      W, L, T     :  Integer;
      Iteration   :  Integer;
      Warriors    :  TWarriors;
      Core        :  TCore;
      Queues      :  PQueues;
      Score       :  TScore;
   begin
   WriteLn('Testing correctness');
   SetLength(Core, MARSParamsNano.CoreSize);
   Queues := CreateQueues(MARSParamsNano);
   InitMARS(MARSParamsNano);
   Warriors := LoadWarriorList(
      'test/list_nano.txt', 'test/nano/', MARSParamsNano
   );
   Assign(FileList, 'test/pmars_nano.txt');
   Reset(FileList);
   Iteration := 0;
   repeat
      readln(FileList, i, j);
      readln(FileList, W, L, T);
      PlayMatchPermute(
         Warriors._[i - 1], Warriors._[j - 1], 
         @Core[0], Queues, MARSParamsNano, Score
      );
      if (Score.W <> W) or (Score.L <> L) or (Score.T <> T) then
         begin
         WriteLn(
            i, ' vs ', j, ': got ', 
            Score.W, '-', Score.L, '-', Score.T, ', expected ',W, '-', L, '-', T
         );
         break;
         end;
      Iteration += 1;
      if Iteration mod 1000 = 0 then
         WriteLn(Iteration);
   until EoF(FileList);
   Close(FileList);
   DestroyQueues(Queues, MARSParamsNano);
   end;
      
procedure TestPerformance(
   var   MARSParams     :  TMarsParams;
         PathWarriorList:  AnsiString;
         WarriorDir     :  AnsiString;
         MaxN           :  Integer;
         NFights        :  Integer;
         Brief          :  Boolean = false);
   var
         Cycles         :  Int64;
         Warriors       :  TWarriors;
         Core           :  TCore;
         Queues         :  PQueues;
         Score          :  TScore;
         s              :  AnsiString;
         t1, t2, dt     :  Double;
         i, j           :  Integer;
   begin
   if not Brief then
      WriteLn('Testing performance (' + PathWarriorList + ')');
   SetLength(Core, MARSParams.CoreSize);
   Queues := CreateQueues(MARSParams);
   InitMARS(MARSParams);
   Warriors := LoadWarriorList(PathWarriorList, WarriorDir, MARSParams);
   Cycles := 0;
   t1 := time;
   for i := 0 to Min(MaxN, Warriors.N) - 1 do
      for j := 0 to i do
         begin
         if NFights = 0 then
            PlayMatchPermute(
               Warriors._[i], Warriors._[j], @Core[0], Queues, MARSParams, Score
            )
         else
            PlayMatchRandom(
               Warriors._[i], Warriors._[j], @Core[0], 
               Queues, MARSParams, NFights, Score
            );
         Cycles += Score.Cycles;
         end;
   DestroyQueues(Queues, MARSParams);
   t2 := time;
   dt := 24 * 60 * 60 * (t2 - t1);
   if Brief then
      begin
      str(2e-6 * Cycles / dt : 3 : 3, s);
      WriteLn(s);
      end
   else
      begin
      str(dt : 3 : 3, s);
      WriteLn('Time: ' + s + ' s.');
      str(2e-6 * Cycles / dt : 3 : 3, s);
      WriteLn('Speed: ' + s + ' MIPS');
      end;
   end;
   
procedure Debug(
   const WarriorPath1,
         WarriorPath2: AnsiString;
   var   MARSParams  : TMARSParams;
         Position    : Int64;
         SideToMove  : Int64);
   var
         Core        :  TCore;
         Queues      :  PQueues;
         Score       :  TScore;
         Warrior1,   
         Warrior2    :  TMARSWarrior;
         Error1,
         Error2      :  PChar;
   begin
   InitMARS(MARSParams);
   SetLength(Core, MARSParams.CoreSize);
   Queues := CreateQueues(MARSParams);
   LoadWarrior(Warrior1, PChar(WarriorPath1), MARSParams, True, Error1);
   LoadWarrior(Warrior2, PChar(WarriorPath2), MARSParams, True, Error2);
   StrDispose(Error1);
   StrDispose(Error2);
   ClearCore(@Core, MARSParams.CoreSize);
   LoadIntoCore(@Core, Queues^[0], Warrior1, {Position:} 0, MARSParams);
   LoadIntoCore(@Core, Queues^[1], Warrior2,  Position    , MARSParams);
   Score := ZeroScore;
   RunSimulation(@Core[0], Queues, SideToMove, MARSParams, Score);
   WriteLn('Cycles     : ', Score.Cycles);
   WriteLn('Processes 1: ', Queues^[0].NProcs);
   WriteLn('Processes 2: ', Queues^[1].NProcs);
   DestroyQueues(Queues, MARSParams);
   end;
   
procedure CheckInit(
         x     : Int64;
   const Name  : AnsiString);
   begin
   if x <= 0 then
      begin
      WriteLn(Name + ' not specified');
      halt;
      end;
   end;
   
procedure CheckMARSParams(
   const MARSParams  : TMARSParams);
   begin
   CheckInit(MARSParams.CoreSize,   'CoreSize'     );
   CheckInit(MARSParams.MaxProcs,   'MaxProcesses' );
   CheckInit(MARSParams.MaxCycles,  'MaxCycles'    );
   CheckInit(MARSParams.MinDist,    'MinDistance'  );
   CheckInit(MARSParams.MaxLength,  'MaxLength'    );
   end;

var   
   MARSParams        : TMARSParams;
   Warriors          : TWarriors;
   Core              : TCore;
   Queues            : PQueues;
   Score             : TScore;
   NRounds, i,
   ExpectedWarriors  : Integer;
   ErrorCode         : Word;
   ShowDiagram       : Boolean;
   Arg               : AnsiString;
const
   Help              : AnsiString =
      'AzureMARS 1.0 Core War simulator by inversed'        + LineEnding +
      'Usage: AzureMARS [options] file1 [file2]'            + LineEnding +
      'Options:'                                            + LineEnding +
      '  -r  Int   Number of rounds'                        + LineEnding +
      '  -s  Int   Core size'                               + LineEnding +
      '  -c  Int   Cycles until tie'                        + LineEnding +
      '  -p  Int   Max. processes'                          + LineEnding +
      '  -l  Int   Max. warrior length'                     + LineEnding +
      '  -d  Int   Min. warriors distance'                  + LineEnding +
      '  -h  Str   Hill preset, one of nano, tiny, 94nop'   + LineEnding +
      '  -P        Permute starting positions'              + LineEnding +
      '  -D        Display spacetime diagram'               + LineEnding +
      '  -Tp       Test performance'                        + LineEnding +
      '  -Tc       Test correctness';

begin
if ParamCount() = 0 then
   begin
   WriteLn(Help);
   exit;
   end;

i := 1;
InitWarriors(Warriors);
MARSParams := MARSParamsNull;
NRounds := -1;
ShowDiagram := False;
while i <= ParamCount() do
   begin
   ErrorCode := 0;
   Arg := ParamStr(i);
   case Arg of
      '-r': 
         begin
         Inc(i);
         Val(ParamStr(i), NRounds, ErrorCode);
         end;
      '-s': 
         begin
         Inc(i);
         Val(ParamStr(i), MARSParams.CoreSize, ErrorCode);
         end;
      '-c': 
         begin
         Inc(i);
         Val(ParamStr(i), MARSParams.MaxCycles, ErrorCode);
         end;
      '-p': 
         begin
         Inc(i);
         Val(ParamStr(i), MARSParams.MaxProcs, ErrorCode);
         end;
      '-l': 
         begin
         Inc(i);
         Val(ParamStr(i), MARSParams.MaxLength, ErrorCode);
         end;
      '-d': 
         begin
         Inc(i);
         Val(ParamStr(i), MARSParams.MinDist, ErrorCode);
         end;
      '-h': 
         begin
         Inc(i);
         case ParamStr(i) of
            'nano' : MARSParams := MARSParamsNano;
            'tiny' : MARSParams := MARSParamsTiny;
            '94nop': MARSParams := MARSParams94nop;
            else
               WriteLn('Unknown hill preset: ' + ParamStr(i));
               exit;
            end;
         end;
      '-P': 
         NRounds := -1;
      '-D':
         ShowDiagram := True;
      '-Tp': 
         begin
         TestPerformance(
            MARSParamsNano, 'test/list_nano.txt' , 'test/nano/' , 
            {MaxN:} 80, {NFights:}   0, {Brief:} True
         );
         TestPerformance(
            MARSParams94nop, 'test/list_94nop.txt', 'test/94nop/', 
            {MaxN:} 10, {NFights:} 30, {Brief:} True
         );
         exit;
         end;
      '-Tc': 
         begin
         TestCorrectness();
         exit;
         end;
      else if ParamStr(i)[1] = '-' then
         begin
         WriteLn('Unknown argument: ' + ParamStr(i));
         exit;
         end
      else if not FileExists(ParamStr(i)) then
         begin
         WriteLn('Warrior file ' + ParamStr(i) + ' does not exist');
         exit;
         end
      else if Warriors.N = 2 then
         begin
         WriteLn(
            'Trying to load 3rd warrior ' + ParamStr(i) + 
            ', multiwarrior mode not supported'
         );
         exit;
         end
      else
         begin
         CheckMARSParams(MARSParams);
         AppendWarrior(Warriors, ParamStr(i), MARSParams);
         end;
      end;
   if ErrorCode <> 0 then
      begin
      WriteLn('Could not convert ' + ParamStr(i) + ' to integer');
      exit;
      end;
   Inc(i);
   end;
   
if ShowDiagram then
   ExpectedWarriors := 1 else
   ExpectedWarriors := 2;
if Warriors.N <> ExpectedWarriors then
   begin
   WriteLn('Expected ', ExpectedWarriors, ' warrior file(s), got ', Warriors.N);
   exit;
   end;
   
InitMARS(MARSParams);
if ShowDiagram then
   begin
   WriteLn(SpacetimeDiagram(Warriors._[0], MARSParams));
   end
else
   begin
   RandSeed := 1;
   SetLength(Core, MARSParams.CoreSize);
   Queues := CreateQueues(MARSParams);
   if NRounds <= 0 then
      PlayMatchPermute(
         Warriors._[0], Warriors._[1], @Core[0], Queues, MARSParams, Score
      )
   else
      PlayMatchRandom(
         Warriors._[0], Warriors._[1], @Core[0], Queues, MARSParams, NRounds, Score
      );
   DestroyQueues(Queues, MARSParams);
   WriteLn('Score1 Score2     W1     L1     T1');
   WriteLn(
      Score.Taken: 6 : 2, ' ',
      Score.Given: 6 : 2, ' ',
      Score.W : 6,        ' ',
      Score.L : 6,        ' ',
      Score.T : 6
   );
   end
end.
