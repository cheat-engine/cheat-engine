unit disassembler;

{$MODE Delphi}

interface

uses windows, imagehlp,sysutils,LCLIntf,byteinterpreter, symbolhandler,CEFuncProc,NewKernelHandler;

type Tprefix = set of byte;
type TMemory = array [0..23] of byte;
type TIntToHexS=function(address:ptrUInt;chars: integer; signed: boolean=false; signedsize: integer=0):string of object;

const BIT_REX_W=8; //1000
const BIT_REX_R=4;
const BIT_REX_X=2;
const BIT_REX_B=1;

type TDisAssemblerValueType=(dvtNone, dvtAddress, dvtValue);

type TDisassembler=class
  private
    inttohexs: TIntToHexS;
    RexPrefix: byte;
    riprelative: boolean;
    function SIB(memory:TMemory; sibbyte: integer; var last: dword): string;
    function MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword): string; overload;
    function MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer): string; overload;

    function MODRM2(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword): string;

    function getReg(bt: byte): byte;
    function getmod(bt: byte): byte;
    function getrm(bt: byte): byte;

    function opcodeToValueType(opcode: string): integer;
    function opcode2F(opcode: string): integer;
    function opcode3FN(opcode: string): integer;
    function opcode3FS(opcode: string): integer;
    function opcode4FST(opcode: string): integer;

    function Rex_B: boolean; inline;
    function Rex_X: boolean; inline;
    function Rex_R: boolean; inline;
    function Rex_W: boolean; inline;

    function rd(bt:byte):string;
    function rd8(bt:byte): string;
    function rd16(bt:byte): string;
    function r8(bt:byte): string;
    function r16(bt:byte): string;
    function r32(bt:byte): string;
    function xmm(bt:byte): string;
    function mm(bt:byte): string;
    function sreg(bt:byte): string;
    function cr(bt:byte): string;
    function dr(bt:byte): string;
    function GetBitOf(Bt: qword; bit: integer): byte;
    function getsegmentoverride(prefix: TPrefix): string;


    function inttohexs_withoutsymbols(address:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
    function inttohexs_withsymbols(address:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;


  public
    isdefault: boolean;
    showsymbols: boolean;
    showmodules: boolean;
    syntaxhighlighting: boolean;

    LastDisassembleData: record
      address: PtrUint;
      prefix: string;
      opcode: pchar; //replaced string with pchar so it now only contains a pointer. Faster. string; //the result without bytes
      parameters: string;
      description: string;
      Bytes: array of byte;
      SeperatorCount: integer;
      Seperators: Array [0..5] of integer; //an index in the byte array describing the seperators (prefix/instruction/modrm/sib/extra)
      modrmValueType: TDisAssemblerValueType;
      modrmValue: ptrUint;
      parameterValueType: TDisAssemblerValueType;
      parameterValue: ptrUint;

    end;



//    showvalues: boolean;
    function disassemble(var offset: ptrUint; var description: string): string;
    procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);
end;




//function rd(bt: byte): string;
//function rd8(bt:byte): string;
//function rd16(bt:byte): string;

//function r8(bt:byte): string;
//function r16(bt:byte): string;
//function r32(bt:byte): string;
//function mm(bt:byte): string;
//function xmm(bt:byte): string;
//function sreg(bt:byte): string;
//function CR(bt:byte):string;
//function DR(bt:byte):string;



//function GetBitOf(Bt: qword; bit: integer): byte;
//function getsegmentoverride(prefix: TPrefix): string;
//function getmod(bt: byte): byte;
//function getRM(bt: byte): byte;
//function getREG(bt: byte): byte;

function disassemble(var offset: ptrUint): string; overload;
function disassemble(var offset: ptrUint; var description: string): string; overload;

function GetBitOf(Bt: qword; bit: integer): byte;
function previousopcode(address: ptrUint):ptrUint;
function has4ByteHexString(d: string; var hexstring: string): boolean;
function hasAddress(d: string; var address: ptrUint; context: PContext=nil):boolean;

//function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean):string;
//function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string):string;
procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);


var visibleDisassembler: TDisassembler; //this disassembler is used to render the disassembly output. Each update it gets synced with the default




implementation

//dont use it by otherunits
{$ifndef net}
{$ifndef standalonetrainer}
uses Assemblerunit,CEDebugger, debughelper, StrUtils;
{$endif}
{$endif}

var defaultDisassembler: TDisassembler;


function TDisassembler.rd(bt:byte):string;
begin
  if rex_B then bt:=bt or 8;
  case bt of
    0: result:='eax';
    1: result:='ecx';
    2: result:='edx';
    3: result:='ebx';
    4: result:='esp';
    5: result:='ebp';
    6: result:='esi';
    7: result:='edi';
    8: result:='r8';
    9: result:='r9';
   10: result:='r10';
   11: result:='r11';
   12: result:='r12';
   13: result:='r13';
   14: result:='r14';
   15: result:='r15';
  end;


  if not rex_w then
  begin
    //not a rex_w field
    if bt>=8 then //but the bt field is higher than 8 (so 32-bit addressing, increased register)
    begin
      result:=result+'d'; //32-bit variant
    end;
  end
  else
  begin
    result[1]:='r' //replace eax,ebx with rax,rbx...
  end;
end;


function TDisassembler.rd8(bt:byte): string;
begin
  case bt of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';
  4: result:='ah';
  5: result:='ch';
  6: result:='dh';
  7: result:='bh';
  end;
end;


function TDisassembler.rd16(bt:byte):string;
begin
  case bt of
  0: result:='ax';
  1: result:='cx';
  2: result:='dx';
  3: result:='bx';
  4: result:='sp';
  5: result:='bp';
  6: result:='si';
  7: result:='di';
  end;
end;


function TDisassembler.r8(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';
  4: result:='ah';
  5: result:='ch';
  6: result:='dh';
  7: result:='bh';
  end;
end;

function TDisassembler.r16(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='ax';
  1: result:='cx';
  2: result:='dx';
  3: result:='bx';
  4: result:='sp';
  5: result:='bp';
  6: result:='si';
  7: result:='di';
  end;
end;

function TDisassembler.r32(bt:byte): string;
begin

  case getreg(bt) of
    0: if rex_w then result:='rax' else result:='eax';
    1: if rex_w then result:='rcx' else result:='ecx';
    2: if rex_w then result:='rdx' else result:='edx';
    3: if rex_w then result:='rbx' else result:='ebx';
    4: if rex_w then result:='rsp' else result:='esp';
    5: if rex_w then result:='rbp' else result:='ebp';
    6: if rex_w then result:='rsi' else result:='esi';
    7: if rex_w then result:='rdi' else result:='edi';
    8: if rex_w then result:='r8' else result:='r8l';
    9: if rex_w then result:='r9' else result:='r9l';
   10: if rex_w then result:='r10' else result:='r10l';
   11: if rex_w then result:='r11' else result:='r11l';
   12: if rex_w then result:='r12' else result:='r12l';
   13: if rex_w then result:='r13' else result:='r13l';
   14: if rex_w then result:='r14' else result:='r14l';
   15: if rex_w then result:='r15' else result:='r15l';
  end;

end;

function TDisassembler.xmm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='XMM0';
  1: result:='XMM1';
  2: result:='XMM2';
  3: result:='XMM3';
  4: result:='XMM4';
  5: result:='XMM5';
  6: result:='XMM6';
  7: result:='XMM7';
  8: result:='XMM8';
  9: result:='XMM9';
  10: result:='XMM10';
  11: result:='XMM11';
  12: result:='XMM12';
  13: result:='XMM13';
  14: result:='XMM14';
  15: result:='XMM15';
  end;
end;

function TDisassembler.mm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='MM0';
  1: result:='MM1';
  2: result:='MM2';
  3: result:='MM3';
  4: result:='MM4';
  5: result:='MM5';
  6: result:='MM6';
  7: result:='MM7';
  8: result:='MM8';
  9: result:='MM9';
  10: result:='MM10';
  11: result:='MM11';
  12: result:='MM12';
  13: result:='MM13';
  14: result:='MM14';
  15: result:='MM15';

  end;
end;

function TDisassembler.sreg(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='ES';
  1: result:='CS';
  2: result:='SS';
  3: result:='DS';
  4: result:='FS';
  5: result:='GS';
  6: result:='HS';  //as if...
  7: result:='IS';
  8: result:='JS';
  9: result:='KS';
 10: result:='LS';
 11: result:='MS';
 12: result:='NS';
 13: result:='OS';
 14: result:='PS';
 15: result:='QS';
  end;
end;

function TDisassembler.CR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='CR0';
  1: result:='CR1';
  2: result:='CR2';
  3: result:='CR3';
  4: result:='CR4';
  5: result:='CR5';
  6: result:='CR6';
  7: result:='CR7';
  8: result:='CR8';
  9: result:='CR9';
  10: result:='CR10';
  11: result:='CR11';
  12: result:='CR12';
  13: result:='CR13';
  14: result:='CR14';
  15: result:='CR15';
  end;
end;

function TDisassembler.DR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='DR0';
  1: result:='DR1';
  2: result:='DR2';
  3: result:='DR3';
  4: result:='DR4';
  5: result:='DR5';
  6: result:='DR6';
  7: result:='DR7';
  8: result:='DR8';//Do not excist, but let's implement the encoding
  9: result:='DR9';
  10: result:='DR10';
  11: result:='DR11';
  12: result:='DR12';
  13: result:='DR13';
  14: result:='DR14';
  15: result:='DR15';
  end;
end;

function GetBitOf(Bt: qword; bit: integer): byte;
begin
  result:=getbit(bit,bt);
end;

function TDisassembler.GetBitOf(Bt: qword; bit: integer): byte;
begin
  result:=getbit(bit,bt);
end;

function TDisassembler.getsegmentoverride(prefix: TPrefix): string;
begin
  if $2e in prefix then result:='CS:' else
  if $26 in prefix then result:='ES:' else
  if $36 in prefix then result:='SS:' else
  if $3e in prefix then result:='' else
  if $64 in prefix then result:='FS:' else
  if $65 in prefix then result:='GS:';
end;

function TDisassembler.getmod(bt: byte): byte;
begin
  result:=(bt shr 6) and 3;
end;

function TDisassembler.getRM(bt: byte): byte;
begin
  result:=bt and 7;

  if rex_b and (NOT ((((bt shl 6) and 3)<>3) and ((bt and 7)=4)))  then //if this instruction does NOT have a SIB byte, only then apply the rex_B bit
    result:=result or 8; //extend the RM field
end;

function TDisassembler.getREG(bt: byte): byte;
begin
  result:=(bt shr 3) and 7;
  if Rex_r then
    result:=result or 8; //extend the reg field
end;



function TDisassembler.Rex_B: boolean; inline;
begin
  result:=(RexPrefix and BIT_REX_B)=BIT_REX_B;
end;

function TDisassembler.Rex_X: boolean; inline;
begin
  result:=(RexPrefix and BIT_REX_X)=BIT_REX_X;
end;

function TDisassembler.Rex_R: boolean; inline;
begin
  result:=(RexPrefix and BIT_REX_R)=BIT_REX_R;
end;

function TDisassembler.Rex_W: boolean; inline;
begin
  result:=(RexPrefix and BIT_REX_W)=BIT_REX_W;
end;


function TDisassembler.MODRM2(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword): string;
var dwordptr: ^dword;
begin
  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=modrmbyte;
  inc(LastDisassembleData.SeperatorCount);

  dwordptr:=@memory[modrmbyte+1];
  last:=modrmbyte+1;

  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
  inc(LastDisassembleData.SeperatorCount);


  if $67 in prefix then
  begin
    // put some 16-bit stuff in here
    // but since this is a 32-bit debugger only ,forget it...

  end
  else
  begin
    case getmod(memory[modrmbyte]) of
      0:
      begin
        case getrm(memory[modrmbyte]) of
            0:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RAX],'
                else
                  result:=getsegmentoverride(prefix)+'[EAX],';

            1:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RCX],'
                else
                  result:=getsegmentoverride(prefix)+'[ECX],';

            2:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDX],'
                else
                  result:=getsegmentoverride(prefix)+'[EDX],';

            3:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBX],'
                else
                  result:=getsegmentoverride(prefix)+'[EBX],';
            4:
            begin
               result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last)+'],';
            end;

            5:
            begin
              if processhandler.is64Bit then
              begin
                riprelative:=true;
                result:=getsegmentoverride(prefix)+'['+inttohexs_withoutsymbols(dwordptr^,8)+'],';


                LastDisassembleData.modrmValueType:=dvtAddress;
                LastDisassembleData.modrmValue:=dwordptr^;

              end
              else
              begin
                result:=getsegmentoverride(prefix)+'['+inttohexs(dwordptr^,8)+'],';
              end;
              last:=last+4;
            end;

            6:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RSI],'
                else
                  result:=getsegmentoverride(prefix)+'[ESI],';

            7:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDI],'
                else
                  result:=getsegmentoverride(prefix)+'[EDI],';

            8:  result:=getsegmentoverride(prefix)+'[R8],';
            9:  result:=getsegmentoverride(prefix)+'[R9],';
           10:  result:=getsegmentoverride(prefix)+'[R10],';
           11:  result:=getsegmentoverride(prefix)+'[R11],';
           12:  result:=getsegmentoverride(prefix)+'[R12],';
           13:  result:=getsegmentoverride(prefix)+'[R13],';
           14:  result:=getsegmentoverride(prefix)+'[R14],';
           15:  result:=getsegmentoverride(prefix)+'[R15],';
        end;

      end;
      1:  begin
            case getrm(memory[modrmbyte]) of
              0:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RAX+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EAX+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RAX-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EAX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              1:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RCX+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ECX+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RCX-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ECX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              2:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDX+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDX+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDX-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              3:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBX+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBX+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBX-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBX-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              4:  begin
                    result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last);
                    if memory[last]<=$7F then
                      result:=result+'+'+inttohexs(memory[last],2)+'],'
                    else
                      result:=result+'-'+inttohexs($100-memory[last],2)+'],';
                  end;

              5:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBP+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBP+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBP-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBP-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              6:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RSI+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ESI+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RSI-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ESI-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              7:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDI+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDI+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDI-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDI-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              8:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R8+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R8-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              9:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R9+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R9-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             10:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R10+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R10-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             11:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R11+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R11-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             12:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R12+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R12-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             13:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R13+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R13-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             14:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R14+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R14-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             15:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'[R15+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'[R15-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
            end;

            inc(last);
          end;

      2:  begin
            LastDisassembleData.modrmValueType:=dvtAddress;
            LastDisassembleData.modrmValue:=dwordptr^;

            case getrm(memory[modrmbyte]) of
              0:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RAX+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EAX+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RAX-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EAX-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              1:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RCX+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ECX+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RCX-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ECX-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              2:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDX+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDX+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDX-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDX-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              3:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBX+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBX+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBX-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBX-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              4:  begin
                    result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last);

                    dwordptr:=@memory[last];
                    if dwordptr^ <=$7FFFFFFF then
                      result:=result+'+'+inttohexs(dwordptr^,8)+'],' else
                      result:=result+'-'+inttohexs($100000000-dwordptr^,8)+'],';


                    LastDisassembleData.modrmValueType:=dvtAddress;
                    LastDisassembleData.modrmValue:=dwordptr^;
                  end;
              5:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBP+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBP+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RBP-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EBP-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              6:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RSI+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ESI+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RSI-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[ESI-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              7:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDI+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDI+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'[RDI-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'[EDI-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              8:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R8+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R8-'+inttohexs($100000000-dwordptr^,8)+'],';

              9:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R9+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R9-'+inttohexs($100000000-dwordptr^,8)+'],';

             10:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R10+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R10-'+inttohexs($100000000-dwordptr^,8)+'],';

             11:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R11+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R11-'+inttohexs($100000000-dwordptr^,8)+'],';

             12:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R12+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R12-'+inttohexs($100000000-dwordptr^,8)+'],';

             13:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R13+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R13-'+inttohexs($100000000-dwordptr^,8)+'],';

             14:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R14+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R14-'+inttohexs($100000000-dwordptr^,8)+'],';

             15:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'[R15+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'[R15-'+inttohexs($100000000-dwordptr^,8)+'],';

            end;
            inc(last,4);

          end;

      3:  begin
            case getrm(memory[modrmbyte]) of
              0:  case inst of
                    0: if rex_w then result:='RAX,' else result:='EAX,';
                    1: result:='AX,';
                    2: result:='AL,';
                    3: result:='MM0,';
                    4: result:='XMM0,';
                  end;

              1:  case inst of
                    0: if rex_w then result:='RCX,' else result:='ECX,';
                    1: result:='CX,';
                    2: result:='CL,';
                    3: result:='MM1,';
                    4: result:='XMM1,';
                  end;

              2:  case inst of
                    0: if rex_w then result:='RDX,' else result:='EDX,';
                    1: result:='DX,';
                    2: result:='DL,';
                    3: result:='MM2,';
                    4: result:='XMM2,';
                  end;

              3:  case inst of
                    0: if rex_w then result:='RBX,' else result:='EBX,';
                    1: result:='BX,';
                    2: result:='BL,';
                    3: result:='MM3,';
                    4: result:='XMM3,';
                  end;

              4:  case inst of
                    0: if rex_w then result:='RSP,' else result:='ESP,';
                    1: result:='SP,';
                    2: if rexprefix<>0 then result:='SPL,' else result:='AH,';
                    3: result:='MM4,';
                    4: result:='XMM4,';
                  end;

              5:  case inst of
                    0: if rex_w then result:='RBP,' else result:='EBP,';
                    1: result:='BP,';
                    2: if rexprefix<>0 then result:='BPL,' else result:='CH,';
                    3: result:='MM5,';
                    4: result:='XMM5,';
                  end;

              6:  case inst of
                    0: if rex_w then result:='RSI,' else result:='ESI,';
                    1: result:='SI,';
                    2: if rexprefix<>0 then result:='SIL,' else result:='DH,';
                    3: result:='MM6,';
                    4: result:='XMM6,';
                  end;

              7:  case inst of
                    0: if rex_w then result:='RDI,' else result:='EDI,';
                    1: result:='DI,';
                    2: if rexprefix<>0 then result:='DIL,' else result:='BH,';
                    3: result:='MM7,';
                    4: result:='XMM7,';
                  end;

              8: case inst of
                    0: if rex_w then result:='R8,' else result:='R8D,';
                    1: result:='R8W,';
                    2: result:='R8L,';
                    3: result:='MM8,';
                    4: result:='XMM8,';
                 end;

              9: case inst of
                   0: if rex_w then result:='R9,' else result:='R9D,';
                   1: result:='R9W,';
                   2: result:='R9L,';
                   3: result:='MM9,';
                   4: result:='XMM9,';
                 end;

             10: case inst of
                   0: if rex_w then result:='R10,' else result:='R10D,';
                   1: result:='R10W,';
                   2: result:='R10L,';
                   3: result:='MM10,';
                   4: result:='XMM10,';
                 end;

             11: case inst of
                   0: if rex_w then result:='R11,' else result:='R11D,';
                   1: result:='R11W,';
                   2: result:='R11L,';
                   3: result:='MM11,';
                   4: result:='XMM11,';
                 end;

             12: case inst of
                   0: if rex_w then result:='R12,' else result:='R12D,';
                   1: result:='R12W,';
                   2: result:='R12L,';
                   3: result:='MM12,';
                   4: result:='XMM12,';
                 end;

             13: case inst of
                   0: if rex_w then result:='R13,' else result:='R13D,';
                   1: result:='R13W,';
                   2: result:='R13L,';
                   3: result:='MM13,';
                   4: result:='XMM13,';
                 end;

             14: case inst of
                   0: if rex_w then result:='R14,' else result:='R14D,';
                   1: result:='R14W,';
                   2: result:='R14L,';
                   3: result:='MM14,';
                   4: result:='XMM14,';
                 end;

             15: case inst of
                   0: if rex_w then result:='R15,' else result:='R15D,';
                   1: result:='R15W,';
                   2: result:='R15L,';
                   3: result:='MM15,';
                   4: result:='XMM15,';
                 end;
            end;
          end;
    end;

  end;

  if last<>(modrmbyte+1) then //add an extra seperator since some bytes have been added, usually the last one, except when the opcode has a immeadiate value followed, which this seperator will then seperate
  begin
    LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
    inc(LastDisassembleData.SeperatorCount);
  end;

  if last>15 then messagebox(0,pchar(result),nil,0);
end;


function tdisassembler.MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword): string;
begin
  result:=modrm2(memory,prefix,modrmbyte,inst,last);
end;

function TDisassembler.MODRM(memory:TMemory; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer): string;
begin
  result:=modrm2(memory,prefix,modrmbyte,inst,last);
  if (length(result)>0) and (result[1]='[') then
  begin
    case opperandsize of
     8 : result:='byte ptr '+result;
     16: result:='word ptr '+result;
     32: result:='dword ptr '+result;
     64: result:='qword ptr '+result;
     80: result:='tword ptr '+result;
     128: result:='dqword ptr '+result;
    end;
  end;
end;

function TDisassembler.SIB(memory:TMemory; sibbyte: integer; var last: dword): string;
var
  dwordptr: ^dword;
  byteptr: ^byte absolute dwordptr;
  ss,index,base, _mod: integer;
  offset: string;

  indexstring: string;
begin
  dwordptr:=@memory[sibbyte+1];
  inc(last);  //sib byte

  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
  inc(LastDisassembleData.SeperatorCount);


  ss:=(memory[sibbyte] shr 6) and 3;
  index:=(memory[sibbyte] shr 3) and 7;
  if Rex_X then index:=index or 8;

  base:=memory[sibbyte] and 7;
  if Rex_B then base:=base or 8;

  _mod:=getmod(memory[sibbyte-1]);

  offset:='';
  case base of
    0: result:='EAX';
    1: result:='ECX';
    2: result:='EDX';
    3: result:='EBX';
    4: result:='ESP';
    5:
    begin
      if _mod<>0 then result:='EBP';
    end;
    6: result:='ESI';
    7: result:='EDI';
    8: result:='R8';
    9: result:='R9';
   10: result:='R10';
   11: result:='R11';
   12: result:='R12';
   13: result:='R13';
   14: result:='R14';
   15: result:='R15';
  end;

  if processhandler.is64Bit then
    if result<>'' then result[1]:='R'; //quick replace

  case index of
    0: indexstring:='EAX';
    1: indexstring:='ECX';
    2: indexstring:='EDX';
    3: indexstring:='EBX';
    4: indexstring:='';
    5: indexstring:='EBP';
    6: indexstring:='ESI';
    7: indexstring:='EDI';
    8: indexstring:='R8';
    9: indexstring:='R9';
   10: indexstring:='R10';
   11: indexstring:='R11';
   12: indexstring:='R12';
   13: indexstring:='R13';
   14: indexstring:='R14';
   15: indexstring:='R15';
  end;

  if processhandler.is64Bit then
    if indexstring<>'' then indexstring[1]:='R'; //quick replace

  if index<>4 then
  begin
    case ss of
      1: indexstring:=indexstring+'*2';
      2: indexstring:=indexstring+'*4';
      3: indexstring:=indexstring+'*8';
    end;
    if result='' then
      result:=indexstring
    else
      result:=result+'+'+indexstring;
  end else
  begin
    if _mod=0 then //special case
    begin
      //sib has a 32-bit displacement value (starting at 0000000000000000)
      LastDisassembleData.modrmValueType:=dvtAddress;
      LastDisassembleData.modrmValue:=dwordptr^;

      result:=inttohexs(dwordptr^,8);
      last:=last+4;
    end;
  end;
{$ifdef disassemblerdebug}
  result:=result+' ss='+inttostr(ss)+' index='+inttostr(index)+' base='+inttostr(base);
{$endif}


end;

function disassemble(var offset: ptrUint; var description: string): string; overload;
begin
  result:=defaultDisassembler.disassemble(offset,description);
end;


function TDisassembler.disassemble(var offset: ptrUint; var description: string): string;
var memory: TMemory;
    actualread: dword;
    startoffset: dword;
    tempresult: string;
    tempst: string;
    wordptr: ^word;
    dwordptr: ^dword;
    dwordptr2: ^dword;
    singleptr: ^single;
    doubleptr: ^double;
    extenedptr: ^extended;
    int64ptr: ^int64;
    i,j: integer;

    prefix: TPrefix;
    prefix2: TPrefix;
    isprefix: boolean;

    last: dword;
    foundit: boolean;

    tempaddress: ptrUint;
    prefixsize: integer;
begin
  last:=0;
  tempresult:='';
  setlength(LastDisassembleData.bytes,0);
  LastDisassembleData.address:=Offset;
  LastDisassembleData.SeperatorCount:=0;
  LastDisassembleData.prefix:='';
  LastDisassembleData.opcode:='';
  LastDisassembleData.parameters:='';

  if isdefault then
    showsymbols:=symhandler.showsymbols;

  if showsymbols then
    intToHexs:=inttohexs_withsymbols
  else
    intToHexs:=inttohexs_withoutsymbols;

  riprelative:=false;


  result:=inttohex(offset,8)+' - ';

  isprefix:=true;

  prefix:=[$f0,$f2,$f3,$2e,$36,$3e,$26,$64,$65,$66,$67];
  prefix2:=[];

  startoffset:=offset;
  readprocessmemory(processhandle,pointer(offset),@memory,24,actualread);


  if actualread>0 then
  begin
    //I HATE THESE...   (I propably will not add them all, but I'll see how far I get)

    if debuggerthread<>nil then
      for i:=0 to actualread-1 do
        if memory[i]=$cc then
          memory[i]:=debuggerthread.getrealbyte(offset+i);


    while isprefix do
    begin
      inc(offset);
      if memory[0] in prefix then
      begin
        if length(LastDisassembleData.bytes)>10 then
        begin
          //prevent a too long prefix from crashing the disassembler (e.g 12GB filled with one prefix....)
          isprefix:=false;
          break;
        end;
        setlength(LastDisassembleData.bytes,length(LastDisassembleData.bytes)+1);
        LastDisassembleData.bytes[length(LastDisassembleData.bytes)-1]:=memory[0];



        result:=result+inttohexs(memory[0],2)+' ';
        isprefix:=true;
        inc(startoffset);
        prefix2:=prefix2+[memory[0]];
        readprocessmemory(processhandle,pointer(offset),addr(memory),24,actualread);
      end else isprefix:=false;
    end;

    if $F0 in prefix2 then tempresult:='lock ';
    if $F2 in prefix2 then tempresult:=tempresult+'repne ';
    if $f3 in prefix2 then tempresult:=tempresult+'repe ';

    LastDisassembleData.prefix:=tempresult;


    RexPrefix:=0;
    if processhandler.is64Bit then
    begin
      if memory[0] in [$40..$4f] then //does it start with a rex prefix ?
      begin
        setlength(LastDisassembleData.bytes,length(LastDisassembleData.bytes)+1);
        LastDisassembleData.bytes[length(LastDisassembleData.bytes)-1]:=memory[0];


        RexPrefix:=memory[0];
        result:=result+inttohexs(RexPrefix,2)+' ';
        inc(offset);
        inc(startoffset);
        prefix2:=prefix2+[RexPrefix];
        MoveMemory(@memory[0], @memory[1], 23);
      end;
    end;

    prefixsize:=length(LastDisassembleData.bytes);

    case memory[0] of  //opcode
      $00 : begin
              description:='Add';

              LastDisassembleData.opcode:='ADD';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);

              inc(offset,last-1);
            end;

      $01 : begin
              description:='Add';

              LastDisassembleData.opcode:='ADD';
              if $66 in prefix2 then LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);

              inc(offset,last-1);
            end;

      $02 : begin
              description:='Add';

              LastDisassembleData.opcode:='ADD';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $03 : begin
              description:='Add';
              LastDisassembleData.opcode:='ADD';
              if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);


              inc(offset,last-1);
            end;



      $04 : begin
              description:='Add '+inttohex(memory[1],2)+' to AL';
              LastDisassembleData.opcode:='ADD';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              inc(offset);
            end;

      $05 : begin
              LastDisassembleData.opcode:='ADD';
              LastDisassembleData.parameterValueType:=dvtValue;


              wordptr:=@memory[1];
              dwordptr:=@memory[1];
              if $66 in prefix2 then
              begin
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;
                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);

                description:='Add '+inttohex(wordptr^,4)+' to AX';



                inc(offset,2);
              end else
              begin
                if Rex_W then
                begin
                  LastDisassembleData.parameterValueType:=dvtValue;
                  LastDisassembleData.parameterValue:=dwordptr^;
                  LastDisassembleData.parameters:='RAX,'+inttohexs(dwordptr^,8);

                  description:='Add '+inttohex(dwordptr^,8)+' to RAX (sign extended)';
                end
                else
                begin
                  LastDisassembleData.parameterValueType:=dvtValue;
                  LastDisassembleData.parameterValue:=dwordptr^;
                  LastDisassembleData.parameters:='EAX,'+inttohexs(dwordptr^,8);

                  description:='Add '+inttohex(dwordptr^,8)+' to EAX';
                end;
                inc(offset,4);
              end;

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $06 : begin
              LastDisassembleData.opcode:='PUSH';
              LastDisassembleData.parameters:='ES';
              description:='Place ES on the stack';
            end;

      $07 : begin
              LastDisassembleData.opcode:='POP';
              LastDisassembleData.parameters:='ES';
              description:='Remove ES from the stack';
            end;

      $08 : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $09 : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              if $66 in prefix2 then LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $0a : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              inc(offset,last-1);
            end;

      $0b : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $0c : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              inc(offset);
            end;

      $0d : begin
              description:='Logical Inclusive OR';
              LastDisassembleData.opcode:='OR';
              LastDisassembleData.parameterValueType:=dvtValue;

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                LastDisassembleData.parameterValue:=wordptr^;
                LastDisassembleData.parameters:='AX,'+inttohexs(LastDisassembleData.parameterValue,4);

                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                begin
                  LastDisassembleData.parameters:='RAX,'+inttohexs(LastDisassembleData.parameterValue,4);
                  description:=description+' (sign-extended)';
                end
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(LastDisassembleData.parameterValue,4);


                LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
                inc(LastDisassembleData.SeperatorCount);
                inc(offset,4);
              end;
            end;

      $0e : begin
              description:='Place CS on the stack';
              LastDisassembleData.opcode:='PUSH';
              LastDisassembleData.parameters:='CS';
            end;

      $0f : begin  //SIMD extensions
              LastDisassembleData.prefix:=''; //these usually treat the F2/F3 prefix differently

              case memory[1] of
                $00 : begin
                        case getReg(memory[2]) of
                         0:  begin
                               LastDisassembleData.opcode:='SLDT';
                               description:='Store Local Descriptor Table Register';
                               if $66 in prefix2 then LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16) else
                                                      LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                               inc(offset,last-1);
                             end;

                         1:  begin
                               description:='Store Task Register';
                               LastDisassembleData.opcode:='STR';
                               LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         2:  begin
                               description:='Load Local Descriptor Table Register';
                               LastDisassembleData.opcode:='LLDT';
                               LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         3:  begin
                               description:='Load Task Register';
                               LastDisassembleData.opcode:='LTR';
                               LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);   ;
                             end;

                         4:  begin
                               description:='Verify a Segment for Reading';
                               LastDisassembleData.opcode:='VERR';
                               LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         5:  begin
                               description:='Verify a Segment for Writing';
                               LastDisassembleData.opcode:='VERW';
                               LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

//the following 2 were made up by me.
                         else
                         begin
                           description:='Not specified by the intel documentation';
                           tempresult:=tempresult+'DB 0F';
                         end;

                        end;

                      end;

                $01 : begin
                        case getReg(memory[2]) of
                         0:  begin
                                case memory[2] of
                                  $c1:  begin
                                          description:='Call to VM monitor by causing VM exit';
                                          LastDisassembleData.opcode:='VMCALL';
                                          inc(offset,2);
                                        end;

                                  $c2:  begin
                                          description:='Launch virtual machine managed by current VMCS';
                                          LastDisassembleData.opcode:='VMLAUNCH';
                                          inc(offset,2);
                                        end;

                                  $c3:  begin
                                          description:='Resume virtual machine managed by current VMCS';
                                          LastDisassembleData.opcode:='VMRESUME';
                                          inc(offset,2);
                                        end;

                                  $c4:  begin
                                          description:='Leaves VMX operation';
                                          LastDisassembleData.opcode:='VMXOFF';
                                          inc(offset,2);
                                        end;

                                  else  begin
                                          description:='Store Global Descriptor Table Register';
                                          LastDisassembleData.opcode:='SGDT';
                                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                          inc(offset,last-1);
                                        end;

                                end;


                              end;

                         1:  begin
                                description:='Store Interrupt Descriptor Table Register';
                                LastDisassembleData.opcode:='SIDT';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         2:  begin
                                description:='Load Global Descriptor Table Register';
                                LastDisassembleData.opcode:='LGDT';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         3:  begin
                                description:='Load Interupt Descriptor Table Register';
                                LastDisassembleData.opcode:='LIDT';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         4:  begin
                                description:='Store Machine Status Word';
                                LastDisassembleData.opcode:='SMSW';

                                if $66 in prefix2 then LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last)
                                                  else LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         6:  begin
                                description:='Load Machine Status Word';
                                LastDisassembleData.opcode:='LMSW';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,1,last);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='Invalidate TLB Entry';
                                LastDisassembleData.opcode:='INVPLG';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;
                        end;
                      end;

                $02 : begin
                        description:='Load Access Rights Byte';
                        LastDisassembleData.opcode:='LAR';
                        if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $03 : begin
                        description:='Load Segment Limit';
                        LastDisassembleData.opcode:='LSL';
                        if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $05 : begin
                        description:='Fast System Call';
                        LastDisassembleData.opcode:='SYSCALL';
                        inc(offset);
                      end;

                $06 : begin
                        description:='Clear Task-Switched Flag in CR0';
                        LastDisassembleData.opcode:='CLTS';
                        inc(offset);
                      end;

                $07 : begin
                        description:='Return From Fast System Call';
                        LastDisassembleData.opcode:='SYSRET';
                        inc(offset);
                      end;

                $08 : begin
                        description:='Invalidate Internal Caches';
                        LastDisassembleData.opcode:='INCD';
                        inc(offset);
                      end;

                $09 : begin
                        description:='Write Back and Invalidate Cache';
                        LastDisassembleData.opcode:='WBINVD';
                        inc(offset);
                      end;

                $0b : begin
                        description:='Undefined Instruction(Yes, this one really excists..)';
                        LastDisassembleData.opcode:='UD2';
                        inc(offset);
                      end;


                $10 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='Move Scalar Double-FP';
                          LastDisassembleData.opcode:='MOVSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='Move Scalar Single-FP';
                          LastDisassembleData.opcode:='MOVSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Unaligned Packed Double-FP';
                          LastDisassembleData.opcode:='MOVUPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Unaligned Four Packed Single-FP';
                          LastDisassembleData.opcode:='MOVUPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $11 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='Move Scalar Double-FP';
                          LastDisassembleData.opcode:='MOVSD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='Move Scalar Single-FP';
                          LastDisassembleData.opcode:='MOVSS';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Unaligned Packed Double-FP';
                          LastDisassembleData.opcode:='MOVUPD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Unaligned Four Packed Single-FP';
                          LastDisassembleData.opcode:='MOVUPS';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;

                $12 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move low packed Double-Precision Floating-Point Value';
                          LastDisassembleData.opcode:='MOVLPD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          LastDisassembleData.opcode:='MOVLPS';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $13 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Low Packed Double-FP';
                          LastDisassembleData.opcode:='MOVLPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Low Packed Single-FP';
                          LastDisassembleData.opcode:='MOVLPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $14 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Single-FP';
                          LastDisassembleData.opcode:='UNPCKLPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Single-FP';
                          LastDisassembleData.opcode:='UNPCKLPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $15 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack and Interleave High Packed Double-FP';
                          LastDisassembleData.opcode:='UNPCKHPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High packed Single-FP';
                          LastDisassembleData.opcode:='UNPCKHPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $16 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move High Packed Double-Precision Floating-Point Value';
                          LastDisassembleData.opcode:='MOVHPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          LastDisassembleData.opcode:='MOVHPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $17 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move High Packed Double-Precision Floating-Point Value';
                          LastDisassembleData.opcode:='MOVHPD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='High to Low Packed Single-FP';
                          LastDisassembleData.opcode:='MOVHPS';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $18 : begin
                        case getReg(memory[2]) of
                          0:  begin
                                description:='Prefetch';
                                LastDisassembleData.opcode:='PREFETCHNTA';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='Prefetch';
                                LastDisassembleData.opcode:='PREFETCHTO';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='Prefetch';
                                LastDisassembleData.opcode:='PREFETCHT1';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='Prefetch';
                                LastDisassembleData.opcode:='PREFETCHT2';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                        end;
                      end;

                $1f:  begin
                        case getReg(memory[2]) of
                          0:  begin
                                description:='Multibyte NOP';
                                LastDisassembleData.opcode:='NOP';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                tempresult:=copy(tempresult,1,length(tempresult)-1);
                                inc(offset,last-1);
                              end;
                        end;
                      end;



                $20 : begin
                        description:='Move from Control Register';
                        LastDisassembleData.opcode:='MOV';
                        LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+cr(memory[2]);
                        inc(offset,last-1);
                      end;

                $21 : begin
                        description:='Move from Debug Register';
                        LastDisassembleData.opcode:='MOV';
                        LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+dr(memory[2]);
                        inc(offset,last-1);
                      end;

                $22 : begin
                        description:='Move to Control Register';
                        LastDisassembleData.opcode:='MOV';
                        LastDisassembleData.parameters:=cr(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $23 : begin
                        description:='Move to Debug Register';
                        LastDisassembleData.opcode:='MOV';
                        LastDisassembleData.parameters:=dr(memory[2])+','+MODRM(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $28 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Aligned Packed Fouble-FP Values';
                          LastDisassembleData.Opcode:='MOVAPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Aligned Four Packed Single-FP';
                          LastDisassembleData.Opcode:='MOVAPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $29 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Aligned Packed Fouble-FP Values';
                          LastDisassembleData.Opcode:='MOVAPD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Aligned Four Packed Single-FP';
                          LastDisassembleData.Opcode:='MOVAPS';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;


                $2a : begin
                        if $f2 in prefix2 then
                        begin

                          description:='Convert Doubleword integer to Scalar DoublePrecision Floating-point value';
                          LastDisassembleData.Opcode:='CVTSI2SD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='Scalar Signed INT32 to Single-FP Conversion';
                          LastDisassembleData.Opcode:='CVTSI2SS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert Packed DWORD''s to Packed DP-FP''s';
                            LastDisassembleData.Opcode:='CVTPI2PD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Signed INT32 to Packed Single-FP Conversion';
                            LastDisassembleData.Opcode:='CVTPI2PS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2B : begin
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='MOVNTPD';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Packed double-precision floating-point using Non-Temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='MOVNTPS';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Aligned Four Packed Single-FP Non Temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $2c : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert with truncation scalar Double-precision floating point value to Signed doubleword integer';
                          LastDisassembleData.Opcode:='CVTTSD2SI';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Scalar Single-FP to Signed INT32 Conversion (Truncate)';
                          LastDisassembleData.Opcode:='CVTTSS2SI';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Packed DoublePrecision-FP to Packed DWORD Conversion (Truncate)';
                            LastDisassembleData.Opcode:='CVTTPD2PI';
                            LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP to Packed INT32 Conversion (Truncate)';
                            LastDisassembleData.Opcode:='CVTTPS2PI';
                            LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2d : begin
                        if $f2 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-6);
                          description:='Convert Scalar Double-Precision Floating-Point Value to Doubleword Integer';
                          LastDisassembleData.Opcode:='CVTSD2SI';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          tempresult:=copy(tempresult,1,length(tempresult)-5);
                          description:='Scalar Single-FP to Signed INT32 Conversion';
                          LastDisassembleData.Opcode:='CVTSS2SI';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert 2 packed DP-FP''s from param 2 to packed signed dword in param1';
                            LastDisassembleData.Opcode:='CVTPI2PS';
                            LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP to Packed INT32 Conversion';
                            LastDisassembleData.Opcode:='CVTPS2PI';
                            LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2e : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unordered Scalar Double-FP Compare and Set EFLAGS';
                          LastDisassembleData.Opcode:='UCOMISD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unordered Scalar Single-FP Compare and Set EFLAGS';
                          LastDisassembleData.Opcode:='UCOMISS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;


                $2f : begin
                        if $66 in prefix2 then
                        begin
                          description:='Compare scalar ordered double-precision Floating Point Values and set EFLAGS';
                          LastDisassembleData.Opcode:='COMISD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Scalar Ordered Single-FP Compare and Set EFLAGS';
                          LastDisassembleData.Opcode:='COMISS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $30 : begin
                        description:='Write to Model Specific Register';
                        LastDisassembleData.Opcode:='WRMSR';
                        inc(offset);
                      end;

                $31 : begin
                        description:='Read Time-Stamp Counter';
                        LastDisassembleData.Opcode:='RDTSC';
                        inc(offset);
                      end;

                $32 : begin
                        description:='Read from Model Specific Register';
                        LastDisassembleData.Opcode:='RDMSR';
                        inc(offset);
                      end;

                $33: begin
                        description:='Read Performance-Monitoring counters';
                        LastDisassembleData.Opcode:='RDPMC';
                        inc(offset);
                      end;

                $34: begin
                        description:='Fast Transistion to System Call Entry Point';
                        LastDisassembleData.Opcode:='SYSENTER';
                        inc(offset);
                      end;

                $35: begin
                        description:='Fast Transistion from System Call Entry Point';
                        LastDisassembleData.Opcode:='SYSEXIT';
                        inc(offset);
                      end;

                $40 : begin
                        description:='Move if overflow';
                        LastDisassembleData.Opcode:='CMOVO';
                        if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $41 : begin
                        description:='Move if not overflow';
                        LastDisassembleData.Opcode:='CMOVNO';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $42 : begin
                        description:='Move if below/ Move if Carry';
                        LastDisassembleData.Opcode:='CMOVB';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $43 : begin
                        description:='Move if above or equal/ Move if not carry';
                        LastDisassembleData.Opcode:='CMOVAE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $44 : begin
                        description:='Move if equal/Move if Zero';
                        LastDisassembleData.Opcode:='CMOVE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $45 : begin
                        description:='Move if not equal/Move if not zero';
                        LastDisassembleData.Opcode:='CMOVNE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $46 : begin
                        description:='Move if below or equal';
                        LastDisassembleData.Opcode:='CMOVBE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;


                $47 : begin
                        description:='Move if Above';
                        LastDisassembleData.Opcode:='CMOVA';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $48 : begin
                        description:='Move if Sign';
                        LastDisassembleData.Opcode:='CMOVS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $49 : begin
                        description:='Move if not sign';
                        LastDisassembleData.Opcode:='CMOVNS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4A : begin
                        description:='Move if parity Even';
                        LastDisassembleData.Opcode:='CMOVPE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4B : begin
                        description:='Move if not parity/Move if parity odd';
                        LastDisassembleData.Opcode:='CMOVNP';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4C : begin
                        description:='Move if less';
                        LastDisassembleData.Opcode:='CMOVL';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4D : begin
                        description:='Move if greater or equal';
                        LastDisassembleData.Opcode:='CMOVGE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4E : begin
                        description:='Move if less or equal';
                        LastDisassembleData.Opcode:='CMOVLE';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);


                        inc(offset,last-1);
                      end;

                $4F : begin
                        description:='Move if greater';
                        LastDisassembleData.Opcode:='CMOVG';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $50 : begin
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='MOVMSKPD';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='Extract Packed Double-Precision Floating-Point sign Mask';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='MOVMSKPS';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='Move Mask To Integer';
                          inc(offset,last-1);
                        end;
                      end;

                $51 : begin
                        if $f2 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='SQRTSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Scalar Double-FP Square Root';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='SQRTSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Scalar Single-FP Square Root';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='SQRTPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Packed Double-FP Square Root';

                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='SQRTPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);
                          description:='Packed Single-FP Square Root';

                          inc(offset,last-1);
                        end;
                      end;

                $52 : begin
                        if $f3 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='RSQRSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Single-FP Square Root Reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='RSQRTPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Square Root Reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $53 : begin
                        if $f3 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='RCPSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='RCPPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Single-FP Reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $54 : begin
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='ANDPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Bit-wise Logical AND of xmm2/m128 and xmm1';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='ANDPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Bit-wise Logical And For Single FP';
                          inc(offset,last-1);
                        end;
                      end;

                $55 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical AND NOT of Packed Double-precision FP Values';
                          LastDisassembleData.Opcode:='ANDNPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical And Not For Single-FP';
                          LastDisassembleData.Opcode:='ANDNPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $56 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical OR of Double-FP';
                          LastDisassembleData.Opcode:='ORPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical OR For Single-FP';
                          LastDisassembleData.Opcode:='ORPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $57 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bit-wise Logical XOR For Double-FP Data';
                          LastDisassembleData.Opcode:='XORPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bit-wise Logical XOR For Single-FP Data';
                          LastDisassembleData.Opcode:='XORPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $58 : begin
                        if $f2 in prefix2 then
                        begin
                          //delete the repne from the tempresult


                          LastDisassembleData.Opcode:='ADDSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Add the lower SP FP number from XMM2/Mem to XMM1.';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          //delete the repe from the tempresult


                          LastDisassembleData.Opcode:='ADDSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Add the lower SP FP number from XMM2/Mem to XMM1.';
                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            LastDisassembleData.Opcode:='ADDPD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='Add packed double-precision floating-point values from XMM2/Mem to xmm1';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            LastDisassembleData.Opcode:='ADDPS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Add packed SP FP numbers from XMM2/Mem to XMM1';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $59 : begin
                        if $f2 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='MULSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Double-FP Multiply';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='MULSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Multiply';
                          inc(offset,last-1);
                        end else
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='MULPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Double-FP Multiply';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='MULPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Single-FP Multiply';
                          inc(offset,last-1);
                        end;
                      end;

                $5A : begin
                        if $f2 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='CVTSD2SS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Convert Scalar Double-Precision Floating-Point Value to Scalar Single-Precision Floating-Point Value';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='CVTSS2SD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Convert Scalar Single-Precision Floating-Point Value to Scalar Double-Precision Floating-Point Value';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            LastDisassembleData.Opcode:='CVTPD2PS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Convert Packed Double Precision FP Values to Packed Single Precision FP Values';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            LastDisassembleData.Opcode:='CVTPS2PD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Convert Packed Single Precision FP Values to Packed Double Precision FP Values';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5B : begin
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='CVTPS2DQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Convert PS-Precision FPoint Values to Packed DWORD''s ';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='CVTDQ2PS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Convert Packed DWORD''s to PS-Precision FPoint Values';
                          inc(offset,last-1);
                        end;
                      end;

                $5c : begin
                        if $f2 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='SUBSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Double-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='SUBSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='SUBPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Double-FP Subtract';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='SUBPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Packed Single-FP Subtract';
                          inc(offset,last-1);
                        end;
                      end;


                $5d : begin
                        if $f2 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='MINSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Minimum';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='MINSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Minimum';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            LastDisassembleData.Opcode:='MINPD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Packed Double-FP Minimum';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            LastDisassembleData.Opcode:='MINPS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Packed Single-FP Minimum';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5e : begin
                        if $f2 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='DIVSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Double-Precision-FP Divide';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          LastDisassembleData.Opcode:='DIVSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                          description:='Scalar Single-FP Divide';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            LastDisassembleData.Opcode:='DIVPD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Packed Double-Precision FP Divide';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            LastDisassembleData.Opcode:='DIVPS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+MODRM(memory,prefix2,2,4,last);

                            description:='Packed Single-FP Divide';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5f : begin
                        if $f2 in prefix2 then
                        begin

                          description:='Scalar Double-FP Maximum';
                          LastDisassembleData.Opcode:='MAXSD';
                          LastDisassembleData.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          description:='Scalar Single-FP Maximum';
                          LastDisassembleData.Opcode:='MAXSS';
                          LastDisassembleData.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Packed Double-FP Maximum';
                            LastDisassembleData.Opcode:='MAXPD';
                            LastDisassembleData.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='Packed Single-FP Maximum';
                            LastDisassembleData.Opcode:='MAXPS';
                            LastDisassembleData.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $60 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLBW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLBW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $61 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLWD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLWD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $62 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLDQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $63 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with signed Saturation';
                          LastDisassembleData.Opcode:='PACKSSWB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with signed Saturation';
                          LastDisassembleData.Opcode:='PACKSSWB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $64 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $65 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $66 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Greater Than';
                          LastDisassembleData.Opcode:='PCMPGTD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $67 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with Unsigned Saturation';
                          LastDisassembleData.Opcode:='PACKUSWB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with Unsigned Saturation';
                          LastDisassembleData.Opcode:='PACKUSWB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $68 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHBW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHBW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $69 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHWD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHWD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6a : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHDQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6B : begin
                        if $66 in prefix2 then
                        begin
                          description:='Pack with signed Saturation';
                          LastDisassembleData.Opcode:='PACKSSDW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Pack with signed Saturation';
                          LastDisassembleData.Opcode:='PACKSSDW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $6c : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack Low Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKLQDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;

                $6d : begin
                        if $66 in prefix2 then
                        begin
                          description:='Unpack High Packed Data';
                          LastDisassembleData.Opcode:='PUNPCKHQDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;


                $6e : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Doubleword';
                          LastDisassembleData.Opcode:='MOVD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 32 Bits';
                          LastDisassembleData.Opcode:='MOVD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $6f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='Move UnAligned Double Quadword';
                          LastDisassembleData.Opcode:='MOVDQU';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move Aligned Double Quadword';
                          LastDisassembleData.Opcode:='MOVDQA';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 64 Bits';
                          LastDisassembleData.Opcode:='MOVDQA';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end;
                      end;

                $70 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='Shuffle Packed Low Words';
                          LastDisassembleData.Opcode:='PSHUFLW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='Shuffle Packed High Words';
                          LastDisassembleData.Opcode:='PSHUFHW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shuffle DoubleWord';
                          LastDisassembleData.Opcode:='PSHUFD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Packed Shuffle Word';
                          LastDisassembleData.Opcode:='PSHUFW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end;
                      end;

                $71 : begin
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[3];
                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=3;
                        inc(LastDisassembleData.SeperatorCount);


                        case getReg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLW';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLW';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  LastDisassembleData.Opcode:='PSRAW';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  LastDisassembleData.Opcode:='PSRAW';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLW';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLW';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $72 : begin
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[3];
                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=3;
                        inc(LastDisassembleData.SeperatorCount);

                        case getReg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLD';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLD';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  LastDisassembleData.Opcode:='PSRAD';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Arithmetic';
                                  LastDisassembleData.Opcode:='PSRAD';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLD';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLD';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $73 : begin
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[3];
                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=3;
                        inc(LastDisassembleData.SeperatorCount);

                        case getReg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLQ';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Right Logical';
                                  LastDisassembleData.Opcode:='PSRLQ';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          3 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Shift double Quadword right Lopgical';
                                  LastDisassembleData.Opcode:='PSRLDQ';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLQ';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='Packed Shift Left Logical';
                                  LastDisassembleData.Opcode:='PSLLQ';
                                  LastDisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          7 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='Shift Double Quadword Left Logical';
                                  LastDisassembleData.Opcode:='PSLLDQ';
                                  LastDisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;



                $74 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $75 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $76 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Compare for Equal';
                          LastDisassembleData.Opcode:='PCMPEQD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $77 : begin
                        description:='Empty MMX™ State';
                        LastDisassembleData.Opcode:='EMMS';
                        inc(offset);
                      end;

                $78 : begin
                        description:='Reads a specified VMCS field (32 Bits)';
                        LastDisassembleData.Opcode:='VMREAD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $79 : begin
                        description:='Writes a specified VMCS field (32 Bits)';
                        LastDisassembleData.Opcode:='VMWRITE';
                        LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $7e : begin
                        if $f3 in prefix2 then
                        begin

                          description:='Move quadword';
                          LastDisassembleData.Opcode:='MOVQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move 32 Bits';
                          LastDisassembleData.Opcode:='MOVD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 32 Bits';
                          LastDisassembleData.Opcode:='MOVD';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $7f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='Move Unaligned double Quadword';
                          LastDisassembleData.Opcode:='MOVDQU';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move aligned double Quadword';
                          LastDisassembleData.Opcode:='MOVDQA';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move 64 Bits';
                          LastDisassembleData.Opcode:='MOVQ';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $80 : begin
                        description:='Jump near if overflow';
                        LastDisassembleData.Opcode:='JO';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $81 : begin
                        description:='Jump near if not overflow';
                        LastDisassembleData.Opcode:='JNO';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

                      end;

                $82 : begin
                        description:='Jump near if below/carry';

                        LastDisassembleData.Opcode:='JB';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

                      end;

                $83 : begin
                        description:='Jump near if above or equal';
                        LastDisassembleData.Opcode:='JAE';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $84 : begin
                        description:='Jump near if equal';
                        LastDisassembleData.Opcode:='JE';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;


                $85 : begin
                        description:='Jump near if not equal';
                        LastDisassembleData.Opcode:='JNE';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

                      end;

                $86 : begin
                        description:='Jump near if below or equal';
                        LastDisassembleData.Opcode:='JBE';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $87 : begin
                        description:='Jump near if above';
                        LastDisassembleData.Opcode:='JA';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $88 : begin
                        description:='Jump near if sign';
                        LastDisassembleData.Opcode:='JS';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $89 : begin
                        description:='Jump near if less';
                        LastDisassembleData.Opcode:='JL';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8a : begin
                        description:='Jump near if parity';
                        LastDisassembleData.Opcode:='JP';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8b : begin
                        description:='Jump near if not parity';
                        LastDisassembleData.Opcode:='JNP';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8c : begin
                        description:='Jump near if less';
                        LastDisassembleData.Opcode:='JL';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8d : begin
                        description:='Jump near if not less';
                        LastDisassembleData.Opcode:='JNL';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8e : begin
                        description:='Jump near if not greater';
                        LastDisassembleData.Opcode:='JNG';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $8f : begin
                        description:='Jump near if greater';
                        LastDisassembleData.Opcode:='JG';

                        LastDisassembleData.parameterValueType:=dvtAddress;
                        if processhandler.is64Bit then
                          LastDisassembleData.parameterValue:=qword(offset+pint(@memory[2])^)
                        else
                          LastDisassembleData.parameterValue:=dword(offset+pint(@memory[2])^);

                        LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=2;
                        inc(LastDisassembleData.SeperatorCount);


                        inc(offset,1+4);
                        LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
                      end;

                $90 : begin
                        description:='Set byte if overflow';
                        LastDisassembleData.Opcode:='SETO';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $91 : begin
                        description:='Set byte if not overfloww';
                        LastDisassembleData.Opcode:='SETNO';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $92 : begin
                        description:='Set byte if below/carry';
                        LastDisassembleData.Opcode:='SETB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $93 : begin
                        description:='Set byte if above or equal';
                        LastDisassembleData.Opcode:='SETAE';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $94 : begin
                        description:='Set byte if equal';
                        LastDisassembleData.Opcode:='SETE';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $95 : begin
                        description:='Set byte if not carry(not equal)';
                        LastDisassembleData.Opcode:='SETNC';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $96 : begin
                        description:='Set byte if below or equal';
                        LastDisassembleData.Opcode:='SETBE';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $97 : begin
                        description:='Set byte if above';
                        LastDisassembleData.Opcode:='SETA';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $98 : begin
                        description:='Set byte if sign';
                        LastDisassembleData.Opcode:='SETS';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $99 : begin
                        description:='Set byte if not sign';
                        LastDisassembleData.Opcode:='SETNS';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9a : begin
                        description:='Set byte if parity';
                        LastDisassembleData.Opcode:='SETP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9b : begin
                        description:='Set byte if not parity';
                        LastDisassembleData.Opcode:='SETNP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9c : begin
                        description:='Set byte if less';
                        LastDisassembleData.Opcode:='SETL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9d : begin
                        description:='Set byte if greater or equal';
                        LastDisassembleData.Opcode:='SETGE';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);

                      end;

                $9e : begin
                        description:='Set byte if less or equal';
                        LastDisassembleData.Opcode:='SETLE';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);

                      end;

                $9f : begin
                        description:='Set byte if greater';
                        LastDisassembleData.Opcode:='SETG';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);


                      end;

                $a0 : begin
                        description:='Push Word or Doubleword Onto the Stack';
                        LastDisassembleData.Opcode:='PUSH';
                        LastDisassembleData.parameters:='FS';
                        inc(offset);
                      end;

                $a1 : begin
                        description:='Pop a Value from the Stack';
                        LastDisassembleData.Opcode:='POP';
                        LastDisassembleData.parameters:='FS';
                        inc(offset);
                      end;


                $a2 : begin
                        description:='CPU Identification';
                        LastDisassembleData.Opcode:='CPUID';
                        inc(offset);
                      end;

                $a3 : begin
                        description:='Bit Test';
                        LastDisassembleData.Opcode:='BT';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);

                        inc(offset,last-1);
                      end;

                $a4 : begin
                        description:='Double Precision Shift Left';
                        LastDisassembleData.Opcode:='SHLD';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $a5 : begin
                        description:='Double Precision Shift Left';
                        LastDisassembleData.Opcode:='SHLD';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+'CL' else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+'CL';
                        inc(offset,last-1);

                      end;

                $a8 : begin
                        description:='Push Word or Doubleword Onto the Stack';
                        LastDisassembleData.Opcode:='PUSH';
                        LastDisassembleData.parameters:='GS';
                        inc(offset);
                      end;

                $a9 : begin
                        description:='Pop a Value from the Stack';
                        LastDisassembleData.Opcode:='POP';
                        LastDisassembleData.parameters:='GS';
                        inc(offset);
                      end;

                $aa : begin
                        description:='Resume from System Management Mode';
                        LastDisassembleData.Opcode:='RSM';
                        inc(offset);
                      end;

                $ab : begin
                        description:='Bit Test and Set';
                        LastDisassembleData.Opcode:='BTS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $ac : begin
                        description:='Double Precision Shift Right';
                        LastDisassembleData.Opcode:='SHRD';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $ad : begin
                        description:='Double Precision Shift Right';
                        LastDisassembleData.Opcode:='SHRD';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+'CL' else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+'CL';
                        inc(offset,last-1);

                      end;

                $ae : begin
                        case getReg(memory[2]) of
                          0:  begin
                                description:='Store FP and MMX State and Streaming SIMD Extension State';
                                LastDisassembleData.Opcode:='FXSAVE';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='Restore FP and MMX State and Streaming SIMD Extension State';
                                LastDisassembleData.Opcode:='FXRSTOR';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='Load Streaming SIMD Extension Control/Status';
                                LastDisassembleData.Opcode:='LDMXCSR';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='Store Streaming SIMD Extension Control/Status';
                                LastDisassembleData.Opcode:='STMXCSR';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='Store Fence';
                                LastDisassembleData.Opcode:='SFENCE';
                                LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                        END;

                      end;

                $af : begin
                        description:='Signed Multiply';
                        LastDisassembleData.Opcode:='IMUL';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b0 : begin
                        description:='Compare and Exchange';
                        LastDisassembleData.Opcode:='CMPXCHG';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $b1 : begin
                        description:='Compare and Exchange';
                        LastDisassembleData.Opcode:='CMPXCHG';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $b2 : begin
                        description:='Load Far Pointer';
                        LastDisassembleData.Opcode:='LSS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b3 : begin
                        description:='Bit Test and Reset';
                        LastDisassembleData.Opcode:='BTR';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $b4 : begin
                        description:='Load Far Pointer';
                        LastDisassembleData.Opcode:='LFS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b5 : begin
                        description:='Load Far Pointer';
                        LastDisassembleData.Opcode:='LGS';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b6 : begin
                        description:='Load Far Pointer';
                        LastDisassembleData.Opcode:='MOVZX';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,2,last,8) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,2,last,8);


                        inc(offset,last-1);
                      end;

                $b7 : begin
                        description:='Load Far Pointer';
                        LastDisassembleData.Opcode:='MOVZX';
                        LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,1,last,16);


                        inc(offset,last-1);
                      end;


                $ba : begin
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[3];

                        case getReg(memory[2]) of
                          4:  begin
                                //BT
                                description:='Bit Test';
                                LastDisassembleData.Opcode:='BT';
                                if $66 in prefix2 then
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter

                                inc(offset,last-1+1);
                              end;

                          5:  begin
                                //BTS
                                description:='Bit Test and Set';
                                LastDisassembleData.Opcode:='BTS';
                                if $66 in prefix2 then
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          6:  begin
                                //BTR
                                description:='Bit Test and Reset';
                                LastDisassembleData.Opcode:='BTR';
                                if $66 in prefix2 then
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          7:  begin
                                //BTC
                                description:='Bit Test and Complement';
                                LastDisassembleData.Opcode:='BTC';
                                if $66 in prefix2 then
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                        end;

                      end;

                $bb : begin
                        description:='Bit Test and Complement';
                        LastDisassembleData.Opcode:='BTC';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;


                $bc : begin
                        //bsf
                        description:='Bit Scan Forward';
                        LastDisassembleData.Opcode:='BSF';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,0,last);


                        inc(offset,last-1);
                      end;

                $bd : begin
                        //bsf
                        description:='Bit Scan Reverse';
                        LastDisassembleData.Opcode:='BSR';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,1,last) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,1,last);


                        inc(offset,last-1);
                      end;

                $be : begin
                        description:='Move with Sign-Extension';
                        LastDisassembleData.Opcode:='MOVSX';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=r16(memory[2])+','+MODRM(memory,prefix2,2,2,last,8) else
                          LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,2,last,8);



                        inc(offset,last-1);
                      end;

                $bf : begin
                        description:='Move with Sign-Extension';
                        LastDisassembleData.Opcode:='MOVSX';
                        LastDisassembleData.parameters:=r32(memory[2])+','+MODRM(memory,prefix2,2,1,last,16);

                        inc(offset,last-1);
                      end;

                $c0 : begin
                        description:='Exchange and Add';
                        LastDisassembleData.Opcode:='XADD';
                        LastDisassembleData.parameters:=MODRM(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $c1 : begin
                        description:='Exchange and Add';
                        LastDisassembleData.Opcode:='XADD';
                        if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,1,last)+r16(memory[2]) else

                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $c2 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='Compare Scalar Dpuble-Precision Floating-Point Values';
                          LastDisassembleData.Opcode:='CMPSD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);

                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        if $F3 in prefix2 then
                        begin
                          description:='Packed Single-FP Compare';
                          LastDisassembleData.Opcode:='CMPSS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Compare packed double-Precision Floating-Point Values';
                            LastDisassembleData.Opcode:='CMPPD';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                            LastDisassembleData.parameterValueType:=dvtValue;
                            LastDisassembleData.parameterValue:=memory[last];
                            LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                            inc(offset,last);
                          end
                          else
                          begin
                            description:='Packed Single-FP Compare';
                            LastDisassembleData.Opcode:='CMPPS';
                            LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                            LastDisassembleData.parameterValueType:=dvtValue;
                            LastDisassembleData.parameterValue:=memory[last];
                            LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                            inc(offset,last);
                          end;
                        end;
                      end;

                $c3 : begin
                        description:='Store doubleword using Non-temporal Hint';
                        LastDisassembleData.Opcode:='MOVNTI';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last);
                      end;

                $c4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Insert Word';
                          LastDisassembleData.Opcode:='PINSRW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,0,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Insert Word';
                          LastDisassembleData.Opcode:='PINSRW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,0,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end;
                      end;

                $c5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Extract Word';
                          LastDisassembleData.Opcode:='PEXTRW';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,3);
                        end
                        else
                        begin
                          description:='Extract Word';
                          LastDisassembleData.Opcode:='PEXTRW';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,3);
                        end;
                      end;

                $c6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Shuffle Double-FP';
                          LastDisassembleData.Opcode:='SHUFPD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='Shuffle Single-FP';
                          LastDisassembleData.Opcode:='SHUFPS';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(LastDisassembleData.parameterValue,2);
                          inc(offset,last);
                        end;
                      end;

                $c7 : begin
                        case getReg(memory[2]) of
                          1:  begin
                                description:='Compare and Exchange 8 Bytes';
                                LastDisassembleData.Opcode:='CMPXCHG8B';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          6:  begin
                                if $66 in prefix2 then
                                begin
                                  description:='Copy VMCS data to VMCS region in memory';
                                  LastDisassembleData.Opcode:='VMCLEAR';
                                  LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                if $f3 in prefix2 then
                                begin
                                  description:='Enter VMX root operation';
                                  LastDisassembleData.Opcode:='VMXON';
                                  LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='Loads ther current VMCS pointer from memory';
                                  LastDisassembleData.Opcode:='VMPTRLD';
                                  LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end;
                              end;

                          7:  begin
                                description:='Stores the current VMCS pointer into memory';
                                LastDisassembleData.Opcode:='VMPTRST';
                                LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                inc(offset,last-1);
                              end;
                        end;

                      end;

                $c8..$cf : begin
                        //BSWAP
                        description:='Byte Swap';
                        LastDisassembleData.Opcode:='BSWAP';
                        LastDisassembleData.parameters:=rd(memory[1]-$c8);

                        inc(offset);
                      end;

                $d1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Logical';
                          LastDisassembleData.Opcode:='PSRLQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Add Packed Quadwprd Integers';
                          LastDisassembleData.Opcode:='PADDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Add Packed Quadwprd Integers';
                          LastDisassembleData.Opcode:='PADDQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;


                $d5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply Low';
                          LastDisassembleData.Opcode:='PMULLW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply Low';
                          LastDisassembleData.Opcode:='PMULLW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $d6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='Move low quadword from xmm to MMX technology register';
                          LastDisassembleData.Opcode:='MOVDQ2Q';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='Move low quadword from xmm to MMX technology register';
                          LastDisassembleData.Opcode:='MOVQ2DQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='Move low quadword from xmm to MMX technology register';
                          LastDisassembleData.Opcode:='MOVQ';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move quadword from MMX technology to xmm register';
                          LastDisassembleData.Opcode:='MOVQ2Dq';
                          LastDisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;


                $d7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Move Byte Mask To Integer';
                          LastDisassembleData.Opcode:='PMOVMSKB';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Move Byte Mask To Integer';
                          LastDisassembleData.Opcode:='PMOVMSKB';
                          LastDisassembleData.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $d8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PSUBUSB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PSUBUSB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PSUBUSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PSUBUSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $da : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Unsigned Integer Byte Minimum';
                          LastDisassembleData.Opcode:='PMINUB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Unsigned Integer Byte Minimum';
                          LastDisassembleData.Opcode:='PMINUB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $db : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical AND';
                          LastDisassembleData.Opcode:='PAND';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical AND';
                          LastDisassembleData.Opcode:='PAND';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $dc : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PADDUSB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PADDUSB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $dd : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PADDUSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add Unsigned with Saturation';
                          LastDisassembleData.Opcode:='PADDUSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $de : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Unsigned Integer Byte Maximum';
                          LastDisassembleData.Opcode:='PMAXUB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Unsigned Integer Byte Maximum';
                          LastDisassembleData.Opcode:='PMAXUB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $df : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical AND NOT';
                          LastDisassembleData.Opcode:='PANDN';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical AND NOT';
                          LastDisassembleData.Opcode:='PANDN';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e0 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Average';
                          LastDisassembleData.Opcode:='PAVGB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Average';
                          LastDisassembleData.Opcode:='PAVGB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Right Arithmetic';
                          LastDisassembleData.Opcode:='PSRAW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Right Arithmetic';
                          LastDisassembleData.Opcode:='PSRAW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSRAD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSRAD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Average';
                          LastDisassembleData.Opcode:='PAVGW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Average';
                          LastDisassembleData.Opcode:='PAVGW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply High Unsigned';
                          LastDisassembleData.Opcode:='PMULHUW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply High Unsigned';
                          LastDisassembleData.Opcode:='PMULHUW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply High';
                          LastDisassembleData.Opcode:='PMULHW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply High';
                          LastDisassembleData.Opcode:='PMULHW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='Convert two packed signed dwords from param2 to two packed DP-Floating point values in param1';
                          LastDisassembleData.Opcode:='CVTPD2DQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='Convert two packed signed dwords from param2 to two packed DP-Floating point values in param1';
                          LastDisassembleData.Opcode:='CVTDQ2PD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='Convert with truncation Packed Double-precision Floating-Point Values to Packed Doubleword Integers';
                            LastDisassembleData.Opcode:='CVTTPD2DQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $e7 : begin
                        if $66 in prefix2 then
                        begin
                          LastDisassembleData.Opcode:='MOVNTDQ';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='Move Double quadword Using Non-Temporal Hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          LastDisassembleData.Opcode:='MOVNTQ';
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,2,3,last)+mm(memory[2]);
                          description:='Move 64 Bits Non Temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $e8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract with Saturation';
                          LastDisassembleData.Opcode:='PSUBSB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract with Saturation';
                          LastDisassembleData.Opcode:='PSUBSB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract with Saturation';
                          LastDisassembleData.Opcode:='PSUBSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract with Saturation';
                          LastDisassembleData.Opcode:='PSUBSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $ea : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Signed Integer Word Minimum';
                          LastDisassembleData.Opcode:='PMINSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Signed Integer Word Minimum';
                          LastDisassembleData.Opcode:='PMINSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $eb : begin
                        if $66 in prefix2 then
                        begin
                          description:='Bitwise Logical OR';
                          LastDisassembleData.Opcode:='POR';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Bitwise Logical OR';
                          LastDisassembleData.Opcode:='POR';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ec : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add with Saturation';
                          LastDisassembleData.Opcode:='PADDSB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add with Saturation';
                          LastDisassembleData.Opcode:='PADDSB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ed : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add with Saturation';
                          LastDisassembleData.Opcode:='PADDSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add with Saturation';
                          LastDisassembleData.Opcode:='PADDSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ee : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Signed Integer Word Maximum';
                          LastDisassembleData.Opcode:='PMAXSW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Signed Integer Word Maximum';
                          LastDisassembleData.Opcode:='PMAXSW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ef : begin
                        if $66 in prefix2 then
                        begin
                          description:='Logical Exclusive OR';
                          LastDisassembleData.Opcode:='PXOR';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Logical Exclusive OR';
                          LastDisassembleData.Opcode:='PXOR';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $F1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Shift Left Logical';
                          LastDisassembleData.Opcode:='PSLLQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $F4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Multiply Packed Unsigned Doubleword Integers';
                          LastDisassembleData.Opcode:='PMULUDQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Multiply Packed Unsigned Doubleword Integers';
                          LastDisassembleData.Opcode:='PMULUDQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $F5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Multiply and Add';
                          LastDisassembleData.Opcode:='PMADDWD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Multiply and Add';
                          LastDisassembleData.Opcode:='PMADDWD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $F6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Sum of Absolute Differences';
                          LastDisassembleData.Opcode:='PSADBW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Sum of Absolute Differences';
                          LastDisassembleData.Opcode:='PSADBW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $F7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Store Selected Bytes of Double Quadword';
                          LastDisassembleData.Opcode:='MASKMOVDQU';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Byte Mask Write';
                          LastDisassembleData.Opcode:='MASKMOVQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $F8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $F9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $Fa : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $Fb : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBQ';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Subtract';
                          LastDisassembleData.Opcode:='PSUBQ';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fc : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDB';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDB';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fd : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDW';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDW';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fe : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDD';
                          LastDisassembleData.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='Packed Add';
                          LastDisassembleData.Opcode:='PADDD';
                          LastDisassembleData.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                end;



            end;

//

//

      $10 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $11 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              if $66 in prefix2 then
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                          LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $12 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last,8);

              inc(offset,last-1);
            end;

      $13 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $14 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.parameters:='AL,'+inttohexs(LastDisassembleData.parameterValue,2);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              inc(offset);
            end;

      $15 : begin
              description:='Add with carry';
              LastDisassembleData.Opcode:='ADC';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(LastDisassembleData.parameterValue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if Rex_W then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(LastDisassembleData.parameterValue,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(LastDisassembleData.parameterValue,8);
                inc(offset,4);
              end;

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

            end;

      $16 : begin
              description:='Place SS on the stack';
              LastDisassembleData.Opcode:='PUSH';
              LastDisassembleData.parameters:='SS';
            end;

      $17 : begin
              description:='Remove SS from the stack';
              LastDisassembleData.Opcode:='POP';
              LastDisassembleData.parameters:='SS';
            end;

      $18 : begin
              description:='Integer Subtraction with Borrow';
              LastDisassembleData.Opcode:='SBB';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $19 : begin
              description:='Integer Subtraction with Borrow';
              LastDisassembleData.Opcode:='SBB';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $1a : begin
              description:='Integer Subtraction with Borrow';
              LastDisassembleData.Opcode:='SBB';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last,8);

              inc(offset,last-1);
            end;

      $1b : begin
              description:='Integer subtraction with Borrow';
              LastDisassembleData.Opcode:='SBB';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);


              inc(offset,last-1);
            end;

      $1c : begin
              description:='Integer Subtraction with Borrow';
              LastDisassembleData.Opcode:='SBB';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);


              inc(offset);
            end;

      $1d : begin
              LastDisassembleData.Opcode:='SBB';
              description:='Integer Subtraction with Borrow';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(LastDisassembleData.parameterValue,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(LastDisassembleData.parameterValue,8);

                inc(offset,4);
              end;

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

            end;

      $1e : begin
              description:='Place DS on the stack';
              LastDisassembleData.Opcode:='PUSH';
              LastDisassembleData.parameters:='DS';
            end;

      $1f : begin
              description:='Remove DS from the stack';
              LastDisassembleData.Opcode:='POP';
              LastDisassembleData.parameters:='DS';
            end;

      $20 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $21 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $22 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);
              inc(offset,last-1);
            end;

      $23 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;


      $24 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              inc(offset);
            end;

      $25 : begin
              description:='Logical AND';
              LastDisassembleData.Opcode:='AND';
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;
                LastDisassembleData.parameters:='AX,'+inttohexs(LastDisassembleData.parameterValue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(LastDisassembleData.parameterValue,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(LastDisassembleData.parameterValue,8);
                inc(offset,4);
              end;
            end;

      $27 : begin
              description:='Decimal Adjust AL after Addition';
              LastDisassembleData.Opcode:='DAA';
            end;

      $28 : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $29 : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $2a : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $2b : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $2c : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);



              inc(offset);
            end;

      $2d : begin
              description:='Subtract';
              LastDisassembleData.Opcode:='SUB';


              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(dwordptr^,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $2f : begin
              description:='Decimal Adjust AL after Subtraction';
              LastDisassembleData.Opcode:='DAS';
            end;

      $30 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $31 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $32 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $33 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $34 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $35 : begin
              description:='Logical Exclusive OR';
              LastDisassembleData.Opcode:='XOR';


              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(dwordptr^,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $37 : begin  //AAA
              LastDisassembleData.Opcode:='AAA';
              description:='ASCII adjust AL after addition'
            end;

//---------
      $38 : begin//CMP
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $39 : begin
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $3a : begin
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $3b : begin
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

//---------

      $3c : begin
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $3d : begin
              description:='Compare Two Operands';
              LastDisassembleData.Opcode:='CMP';
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_x then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(dwordptr^,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

            //prefix bytes need fixing
      $3F : begin  //AAS
              LastDisassembleData.Opcode:='AAS';
              description:='ASCII Adjust AL After Subtraction';
            end;

      $40..$47 :
            begin
              description:='Increment by 1';
              LastDisassembleData.Opcode:='INC';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=rd16(memory[0]-$40) else
                LastDisassembleData.parameters:=rd(memory[0]-$40);
            end;

      $48..$4f :
            begin
              description:='Decrement by 1';
              LastDisassembleData.Opcode:='DEC';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=rd16(memory[0]-$48) else
                LastDisassembleData.parameters:=rd(memory[0]-$48);
            end;

      $50..$57 :
            begin
              description:='Push Word or Doubleword Onto the Stack';
              if processhandler.is64Bit then RexPrefix:=RexPrefix or BIT_REX_W; //so rd will pick the 64-bit version

              LastDisassembleData.Opcode:='PUSH';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=rd16(memory[0]-$50) else
                LastDisassembleData.parameters:=rd(memory[0]-$50);
            end;

      $58..$5f :
            begin
              description:='Pop a Value from the Stack';
              if processhandler.is64Bit then RexPrefix:=RexPrefix or BIT_REX_W; //so rd will pick the 64-bit version
              LastDisassembleData.Opcode:='POP';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=rd16(memory[0]-$58) else
                LastDisassembleData.parameters:=rd(memory[0]-$58);
            end;

      $60 : begin
              description:='Push All General-Purpose Registers';
              if processhandler.is64Bit then description:=description+' (invalid)';
              if $66 in prefix2 then LastDisassembleData.Opcode:='PUSHA' else
                                     LastDisassembleData.Opcode:='PUSHAD';

              if processhandler.is64Bit then
              begin
                description:=description+' (invalid)';
                LastDisassembleData.Opcode:='PUSHAD (INVALID)';
              end;
            end;

      $61 : begin
              description:='Pop All General-Purpose Registers';
              if $66 in prefix2 then LastDisassembleData.Opcode:='POPA' else
                                     LastDisassembleData.Opcode:='POPAD';

              if processhandler.is64Bit then
              begin
                description:=description+' (invalid)';
                LastDisassembleData.Opcode:='POPAD (INVALID)';
              end;

            end;

      $62 : begin
              //BOUND
              description:='Check Array Index Against Bounds';
              LastDisassembleData.Opcode:='BOUND';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);

            end;

      $63 : begin
              //ARPL
              LastDisassembleData.Opcode:='ARPL';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]);
              inc(offset,last-1);
              description:='Adjust RPL Field of Segment Selector';
            end;

      $68 : begin
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.Opcode:='PUSH';
                LastDisassembleData.parameters:=inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                LastDisassembleData.Opcode:='PUSH';
                LastDisassembleData.parameters:=inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
              description:='Push Word or Doubleword Onto the Stack (sign extended)';
            end;

      $69 : begin
              description:='Signed Multiply';
              if $66 in prefix2 then
              begin
                LastDisassembleData.Opcode:='IMUL';
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last);
                wordptr:=@memory[last];

                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;


                tempresult:=tempresult+inttohexs(wordptr^,4);
                inc(offset,last-1+2);
              end
              else
              begin
                LastDisassembleData.Opcode:='IMUL';
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);
                dwordptr:=@memory[last];

                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                tempresult:=tempresult+inttohexs(dwordptr^,8);
                inc(offset,last-1+4);
              end;
            end;

      $6a : begin
              LastDisassembleData.Opcode:='PUSH';

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];

              LastDisassembleData.parameters:=inttohexs(memory[1],2,true,1);
              inc(offset);
              description:='Push Byte Onto the Stack';
            end;

      $6b : begin

              description:='Signed Multiply';
              LastDisassembleData.Opcode:='IMUL';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last)+inttohexs(memory[last],2) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
              inc(offset,last-1+1);
            end;

      $6c : begin
              //m8, DX
              description:='Input from Port to String';
              LastDisassembleData.Opcode:='INSB';
            end;

      $6D : begin
              //m8, DX
              description:='Input from Port to String';
              if $66 in prefix2 then LastDisassembleData.Opcode:='INSW' else
                                     LastDisassembleData.Opcode:='INSD';
            end;

      $6e : begin
              //m8, DX
              description:='Output String to Port';
              LastDisassembleData.Opcode:='OUTSB';
            end;

      $6f : begin
              //m8, DX
              description:='Output String to Port';
              if $66 in prefix2 then LastDisassembleData.Opcode:='OUTSW' else
                                     LastDisassembleData.Opcode:='OUTSD';
            end;


      $70 : begin
              description:='Jump short if overflow';
              LastDisassembleData.Opcode:='JO';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);



            end;

      $71 : begin
              description:='Jump short if not overflow';
              LastDisassembleData.Opcode:='JNO';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $72 : begin
              description:='Jump short if below/carry';
              LastDisassembleData.Opcode:='JB';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $73 : begin
              description:='Jump short if above or equal';
              LastDisassembleData.Opcode:='JAE';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $74 : begin
              description:='Jump short if equal';
              LastDisassembleData.Opcode:='JE';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $75 : begin
              description:='Jump short if not equal';
              LastDisassembleData.Opcode:='JNE';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $76 : begin
              description:='Jump short if not Above';
              LastDisassembleData.Opcode:='JNA';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $77 : begin
              description:='Jump short if above';
              LastDisassembleData.Opcode:='JA';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $78 : begin
              description:='Jump short if sign';
              LastDisassembleData.Opcode:='JS';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $79 : begin
              description:='Jump short if not sign';
              LastDisassembleData.Opcode:='JNS';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7a : begin
              description:='Jump short if parity';
              LastDisassembleData.Opcode:='JP';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7b : begin
              description:='Jump short if not parity';
              LastDisassembleData.Opcode:='JNP';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7c : begin
              description:='Jump short if not greater or equal';
              LastDisassembleData.Opcode:='JNGE';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7d : begin
              description:='Jump short if not less (greater or equal)';
              LastDisassembleData.Opcode:='JNL';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7e : begin
              description:='Jump short if less or equal';
              LastDisassembleData.Opcode:='JLE';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $7f : begin
              description:='Jump short if greater';
              LastDisassembleData.Opcode:='JG';
              inc(offset);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameterValueType:=dvtValue;

              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+shortint(memory[1]))
              else
                LastDisassembleData.parameterValue:=dword(offset+shortint(memory[1]));

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
            end;

      $80 : begin
              case getReg(memory[1]) of
                0:  begin
                      //ADD
                      LastDisassembleData.Opcode:='ADD';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Add x to y';
                    end;

                1:  begin
                      //ADC
                      LastDisassembleData.Opcode:='OR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical Inclusive Or';
                    end;


                2:  begin
                      //ADC
                      LastDisassembleData.Opcode:='ADC';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Add with Carry';
                    end;

                3:  begin
                      //sbb
                      LastDisassembleData.Opcode:='SBB';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Integer Subtraction with Borrow';
                    end;

                4:  begin
                      //AND
                      LastDisassembleData.Opcode:='AND';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical AND';
                    end;

                5:  begin
                      LastDisassembleData.Opcode:='SUB';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Subtract';
                    end;

                6:  begin
                      LastDisassembleData.Opcode:='XOR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //AND
                      LastDisassembleData.Opcode:='CMP';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='Compare Two Operands';
                    end;

              end;
            end;

      $81 : begin
              case getReg(memory[1]) of
                0:  begin
                      //ADD
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ADD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='ADD';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

//                      offset:=offset+last;
                      description:='Add x to y';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='OR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='OR';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Logical Inclusive OR';
                    end;

                2:  begin
                      //ADC
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ADC';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='ADC';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Add with Carry';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SBB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='SBB';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Integer Subtraction with Borrow';
                    end;


                4:  begin
                      //AND
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='AND';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='AND';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Logical AND';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SUB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='SUB';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='Subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='XOR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1);
                      end else
                      begin
                        LastDisassembleData.Opcode:='XOR';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+2);
                      end;

                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //CMP
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='CMP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='CMP';
                        if rex_w then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=dwordptr^;

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

                      description:='Compare Two Operands';
                    end;


              end;
            end;

      $83 : begin
              case getReg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ADD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='ADD';

                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];

                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      inc(offset,last);
                      description:='Add (Sign Extended)';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='OR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='OR';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='Add (Sign Extended)';
                    end;


                2:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ADC';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='ADC';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      inc(offset,last);
                      description:='Add with Carry (Sign Extended)';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SBB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];

                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='SBB';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='Integer Subtraction with Borrow (Sign Extended)';
                    end;

                4:  begin
                      //AND
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='AND';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='AND';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      offset:=offset+last;
                      description:='Logical AND (Sign Extended)';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SUB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='SUB';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='Subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='XOR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='XOR';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='Logical Exclusive OR';
                    end;

                7:  begin
                      //CMP
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='CMP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        LastDisassembleData.Opcode:='CMP';
                        if rex_w then
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          LastDisassembleData.parameterValueType:=dvtValue;
                          LastDisassembleData.parameterValue:=memory[last];
                          LastDisassembleData.Parameters:=LastDisassembleData.Parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='Compare Two Operands';
                    end;


              end;
            end;

      $84 : begin
              description:='Logical Compare';
              LastDisassembleData.Opcode:='TEST';
              LastDisassembleData.parameters:=MODRM(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $85 : begin
              description:='Logical Compare';
              LastDisassembleData.Opcode:='TEST';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $86 : begin
              description:='Exchage Memory with Register';
              LastDisassembleData.Opcode:='XCHG';
              LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $87 : begin
              description:='Exchage Memory with Register';
              LastDisassembleData.Opcode:='XCHG';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $88 : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $89 : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,1,last)+r16(memory[1]) else
                LastDisassembleData.parameters:=MODRM(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $8A : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $8B : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $8c : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+sreg(memory[1]);
              inc(offset,last-1);
            end;

      $8D : begin
              description:='Load Effective Address';
              LastDisassembleData.Opcode:='LEA';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $8e : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameters:=sreg(memory[1])+','+modrm(memory,prefix2,1,1,last);

              inc(offset,last-1);
            end;

      $8f : begin
              case getReg(memory[1]) of
               0:  begin
                     description:='Pop a Value from the Stack';
                     LastDisassembleData.Opcode:='POP';
                     if $66 in prefix2 then
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16) else
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     inc(offset,last-1);
                   end;

               else
               begin
                 LastDisassembleData.Opcode:='DB';
                 LastDisassembleData.parameters:='8F';
                 description:='Undefined by the intel specification';
               end;
              end;
            end;


      $90 : begin
              description:='No Operation';
              LastDisassembleData.Opcode:='NOP';
            end;

      $91..$97:
            begin
              description:='Exchange Register with Register';
              LastDisassembleData.Opcode:='XCHG';

              if $66 in prefix2 then
                LastDisassembleData.parameters:='AX,'+rd16(memory[0]-$90)
              else
              begin
                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+rd(memory[0]-$90)
                else
                  LastDisassembleData.parameters:='EAX,'+rd(memory[0]-$90);
              end;
            end;


      $98 : begin
              //CBW/CWDE
              if $66 in prefix2 then
              begin
                LastDisassembleData.Opcode:='CBW';
                description:='Convert Byte to Word';
              end else
              begin
                if rex_w then
                begin
                  LastDisassembleData.Opcode:='CDQE';
                  description:='Convert Doubleword to Quadword';
                end
                else
                begin
                  LastDisassembleData.Opcode:='CWDE';
                  description:='Convert Word to Doubleword';
                end;
              end;
            end;

      $99 : begin
              if $66 in prefix2 then
              begin
                description:='Convert Word to Doubleword';
                LastDisassembleData.Opcode:='CWD';
              end
              else
              begin

                if rex_w then
                begin
                  LastDisassembleData.Opcode:='CQO';
                  description:='Convert Quadword to Octword';
                end
                else
                begin
                  LastDisassembleData.Opcode:='CDQ';
                  description:='Convert Doubleword to Quadword';
                end;
              end;
            end;

      $9A : begin
              description:='Call Procedure';
              wordptr:=@memory[5];

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=5;
              inc(LastDisassembleData.SeperatorCount);



              if processhandler.is64bit then
                LastDisassembleData.Opcode:='CALL (invalid)'
              else
                LastDisassembleData.Opcode:='CALL';

              LastDisassembleData.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;

              tempresult:=tempresult+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $9b : begin
              case memory[1] of

               $d9 : begin
                       case getReg(memory[2]) of
                         6:  begin
                                 description:='Store FPU Environment';
                                 LastDisassembleData.Opcode:='WAIT:FSTENV';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;


                         7:  begin
                                 description:='Store Control Word';
                                 LastDisassembleData.Opcode:='WAIT:FSTCW';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='Wait';
                            tempresult:='WAIT';
                         end;

                       end;
                     end;

               $db : begin
                       case memory[2] of
                         $e2 : begin
                                 description:='Clear Exceptions';
                                 LastDisassembleData.Opcode:='WAIT:FCLEX';
                                 inc(offset,2);
                               end;

                         $e3 : begin
                                 description:='Initialize Floaring-Point Unit';
                                 LastDisassembleData.Opcode:='WAIT:FINIT';
                                 inc(offset,2);
                               end;
                         else
                         begin
                            description:='Wait';
                            tempresult:='WAIT';
                         end;
                       end;
                     end;

               $dd : begin
                       case getReg(memory[2]) of
                         6:  begin
                                 description:='Store FPU State';
                                 LastDisassembleData.Opcode:='WAIT:FSAVE';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         7:  begin
                                 description:='Store Status Word';
                                 LastDisassembleData.Opcode:='WAIT:FSTSW';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='Wait';
                            tempresult:='WAIT';
                         end;
                       end;
                     end;

               $df : begin
                       case memory[2] of
                         $e0 : begin
                                 description:='Store Status Word';
                                 LastDisassembleData.Opcode:='WAIT:FSTSW AX';
                                 inc(offset,2);
                               end;

                         else
                         begin
                            description:='Wait';
                            tempresult:='WAIT';
                         end;
                       end;
                     end;

               else  begin
                       description:='Wait';
                       LastDisassembleData.Opcode:='WAIT';
                     end;

              end;

            end;

      $9c : begin
              description:='Push EFLAGS Register onto the Stack';
              if $66 in prefix2 then LastDisassembleData.Opcode:='PUSHF' else
              begin
                if processhandler.is64bit then
                  LastDisassembleData.Opcode:='PUSHFQ'
                else
                  LastDisassembleData.Opcode:='PUSHFD';
              end;
            end;

      $9d : begin
              description:='Pop Stack into EFLAGS Register';
              if $66 in prefix2 then LastDisassembleData.Opcode:='POPF' else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='POPFQ'
                else
                  LastDisassembleData.Opcode:='POPFD';
              end;
            end;

      $9e : begin
              description:='Store AH into Flags';
              LastDisassembleData.Opcode:='SAHF';
            end;

      $9f : begin
              description:='Load Status Flag into AH Register';
              LastDisassembleData.Opcode:='LAHF';
            end;

      $a0 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              LastDisassembleData.parameters:='AX,'+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              inc(offset,4);
            end;

      $a1 : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              dwordptr:=@memory[1];


              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if $66 in prefix2 then
              begin
                LastDisassembleData.parameters:='AX,'+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              end
              else
              begin
                if Rex_W then
                  LastDisassembleData.parameters:='RAX,'
                else
                  LastDisassembleData.parameters:='EAX,';

                LastDisassembleData.parameters:=LastDisassembleData.parameters+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';

              end;
              inc(offset,4);
            end;

      $a2 : begin
              description:='Copy memory';
              dwordptr:=@memory[1];
              LastDisassembleData.Opcode:='MOV';

              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              LastDisassembleData.parameters:='byte ptr '+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],AL';
              inc(offset,4);
            end;

      $a3 : begin
              description:='Copy memory';
              LastDisassembleData.Opcode:='MOV';
              dwordptr:=@memory[1];

              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=LastDisassembleData.parameters+'AX'
              else
              begin
                if rex_w then
                  LastDisassembleData.parameters:=LastDisassembleData.parameters+'RAX'
                else
                  LastDisassembleData.parameters:=LastDisassembleData.parameters+'EAX';
              end;
              inc(offset,4);
            end;

      $a4 : begin
              description:='Move Data from String to String';
              LastDisassembleData.Opcode:='MOVSB';
            end;

      $a5 : begin
              description:='Move Data from String to String';
              if $66 in prefix2 then
                LastDisassembleData.Opcode:='MOVSW'
              else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='MOVSQ'
                else
                  LastDisassembleData.Opcode:='MOVSD';
              end;
            end;

      $a6 : begin
              description:='Compare String Operands';
              LastDisassembleData.Opcode:='CMPSB';
            end;

      $a7 : begin
              description:='Compare String Operands';
              if $66 in prefix2 then
                LastDisassembleData.Opcode:='CMPSW'
              else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='CMPSQ'
                else
                  LastDisassembleData.Opcode:='CMPSD';
              end;
            end;

      $a8 : begin
              description:='Logical Compare';
              LastDisassembleData.Opcode:='TEST';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);
              inc(offset);
            end;

      $a9 : begin
              description:='Logical Compare';
              LastDisassembleData.Opcode:='TEST';

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.parameters:='AX,'+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=dwordptr^;

                if rex_w then
                  LastDisassembleData.parameters:='RAX,'+inttohexs(dwordptr^,8)
                else
                  LastDisassembleData.parameters:='EAX,'+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $aa : begin
              description:='Store String';
              LastDisassembleData.Opcode:='STOSB';
            end;

      $ab : begin
              description:='Store String';
              if $66 in prefix2 then LastDisassembleData.Opcode:='STOSW' else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='STOSQ'
                else
                  LastDisassembleData.Opcode:='STOSD';
              end;
            end;

      $ac : begin
              description:='Load string';
              LastDisassembleData.Opcode:='LODSB';
            end;

      $ad : begin
              description:='Load string';
              if $66 in prefix2 then LastDisassembleData.Opcode:='LODSW' else
              begin
                if rex_w then
                  lastDisassembleData.Opcode:='LODSQ'
                else
                  lastDisassembleData.Opcode:='LODSD';
              end;
            end;

      $ae : begin
              description:='Compare AL with byte at ES:EDI and set status flag';
              LastDisassembleData.Opcode:='SCASB';
            end;

      $af : begin
              description:='Scan String';
              if $66 in prefix2 then LastDisassembleData.Opcode:='SCASW' else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='SCASQ'
                else
                  LastDisassembleData.Opcode:='SCASD';
              end;
            end;

      $b0..$b7:
            begin
              description:='Copy Memory';
              LastDisassembleData.Opcode:='MOV';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:=rd8(memory[0]-$b0)+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $b8..$bf:
            begin
              description:='Copy Memory';

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;
                LastDisassembleData.parameterValue:=wordptr^;

                LastDisassembleData.Opcode:='MOV';
                LastDisassembleData.parameters:=rd16(memory[0]-$b8)+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                LastDisassembleData.parameterValueType:=dvtValue;


                if rex_w then
                begin
                  LastDisassembleData.Opcode:='MOV';
                  LastDisassembleData.parameterValue:=pqword(dwordptr)^;
                  LastDisassembleData.parameters:=rd(memory[0]-$b8)+','+inttohexs(pqword(dwordptr)^,16);
                  inc(offset,8);
                end
                else
                begin
                  LastDisassembleData.Opcode:='MOV';
                  LastDisassembleData.parameterValue:=dwordptr^;

                  LastDisassembleData.parameters:=rd(memory[0]-$b8)+','+inttohexs(dwordptr^,8);
                  inc(offset,4);
                end;
              end;
            end;

      $c0 : begin
              case getReg(memory[1]) of
                0:  begin
                      LastDisassembleData.Opcode:='ROL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];

                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                1:  begin
                      LastDisassembleData.Opcode:='ROR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Rotate eight bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                2:  begin
                      LastDisassembleData.Opcode:='RCL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Rotate nine bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                3:  begin
                      LastDisassembleData.Opcode:='RCR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Rotate nine bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                4:  begin
                      LastDisassembleData.Opcode:='SHL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Multiply by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                5:  begin
                      LastDisassembleData.Opcode:='SHR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Unsigned divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

{Not in intel spec}
                6:  begin
                      LastDisassembleData.Opcode:='ROL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;
{^^^^^^^^^^^^^^^^^^}

                7:  begin
                      LastDisassembleData.Opcode:='SAR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=memory[last];
                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                      description:='Signed divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

              end;
            end;

      $c1 : begin
              case getReg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 16 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];

                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 32 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 16 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 32 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 17 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 33 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 17 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Rotate 33 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,16);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last);
                        LastDisassembleData.parameterValueType:=dvtValue;
                        LastDisassembleData.parameterValue:=memory[last];
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                        description:='Signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

              end;
            end;

      $c2 : begin
              description:='Near return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=wordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.Opcode:='RET';
              LastDisassembleData.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $c3 : begin
              description:='Near return to calling procedure';
              LastDisassembleData.Opcode:='RET';
            end;

      $c4 : begin
              description:='Load Far Pointer';
              LastDisassembleData.Opcode:='LES';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $c5 : begin
              description:='Load Far Pointer';
              LastDisassembleData.Opcode:='LDS';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                LastDisassembleData.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $c6 : begin
              case getReg(memory[1]) of
              0 : begin
                    description:='Copy Memory';
                    LastDisassembleData.Opcode:='MOV';
                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                    LastDisassembleData.parameterValueType:=dvtValue;
                    LastDisassembleData.parameterValue:=memory[last];

                    LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(memory[last],2);
                    inc(offset,last);
                  end;

              else begin
                     description:='Not defined by the intel documentation';
                     LastDisassembleData.Opcode:='DB';
                     LastDisassembleData.parameters:='C6';
                   end;
              end;
            end;

      $c7 : begin
              case getReg(memory[1]) of
              0 : begin
                    description:='Copy Memory';
                    if $66 in prefix2 then
                    begin
                      LastDisassembleData.Opcode:='MOV';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);

                      wordptr:=@memory[last];
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=wordptr^;

                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(wordptr^,4);
                      inc(offset,last+1);
                    end
                    else
                    begin
                      LastDisassembleData.Opcode:='MOV';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      dwordptr:=@memory[last];
                      LastDisassembleData.parameterValueType:=dvtValue;
                      LastDisassembleData.parameterValue:=dwordptr^;

                      LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(dwordptr^,8);
                      inc(offset,last+3);
                    end;
                  end;

             else begin
                    description:='Not defined by the intel documentation';
                    LastDisassembleData.Opcode:='DB';
                    LastDisassembleData.parameters:='C7';
                  end;

              end;
            end;

      $c8 : begin
              description:='Make Stack Frame for Procedure Parameters';
              wordptr:=@memory[1];
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=wordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=3;
              inc(LastDisassembleData.SeperatorCount);


              LastDisassembleData.Opcode:='ENTER';
              LastDisassembleData.parameters:=inttohexs(wordptr^,4)+','+inttohexs(memory[3],2);
              inc(offset,3);
            end;

      $c9 : begin
              description:='High Level Procedure Exit';
              LastDisassembleData.Opcode:='LEAVE';
            end;

      $ca : begin
              description:='Far return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              LastDisassembleData.Opcode:='RET';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=wordptr^;
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $cb : begin
              description:='Far return to calling procedure';
              LastDisassembleData.Opcode:='RET';
            end;

      $cc : begin
              //should not be shown if its being debugged using int 3'
              description:='Call to Interrupt Procedure-3:Trap to debugger';
              LastDisassembleData.Opcode:='INT 3';
            end;

      $cd : begin
              description:='Call to Interrupt Procedure';
              LastDisassembleData.Opcode:='INT';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:=inttohexs(memory[1],2);
              inc(offset);
            end;

      $ce : begin
              description:='Call to Interrupt Procedure-4:If overflow flag=1';
              LastDisassembleData.Opcode:='INTO';
            end;

      $cf : begin
              description:='Interrupt Return';
              if $66 in prefix2 then LastDisassembleData.Opcode:='IRET' else
              begin
                if rex_w then
                  LastDisassembleData.Opcode:='IRETQ'
                else
                  LastDisassembleData.Opcode:='IRETD';
              end;
            end;

      $d0 : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Rotate eight bits left once';
                      LastDisassembleData.Opcode:='ROL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Rotate eight bits right once';
                      LastDisassembleData.Opcode:='ROR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='Rotate nine bits left once';
                      LastDisassembleData.Opcode:='RCL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Rotate nine bits right once';
                      LastDisassembleData.Opcode:='RCR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Multiply by 2, once';
                      LastDisassembleData.Opcode:='SHL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Unsigned devide by 2, once';
                      LastDisassembleData.Opcode:='SHR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Not defined by the intel documentation';
                      LastDisassembleData.Opcode:='DB';
                      LastDisassembleData.parameters:='D0 '+inttohexs(memory[1],2);
                    end;

                7:  begin
                      description:='Signed devide by 2, once';
                      LastDisassembleData.Opcode:='SAR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

              end;
            end;

      $d1 : begin
              case getReg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits left once';
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits left once';
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits right once';
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits right once';
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits left once';
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits left once';
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits right once';
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits right once';
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Multiply by 2, Once';
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Multiply by 2, once';
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Unsigned divide by 2, Once';
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Unsigned divide by 2, once';
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                6:  begin
                      description:='Undefined by the intel documentation';
                      LastDisassembleData.Opcode:='DB';
                      LastDisassembleData.parameters:='D1';
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Signed divide by 2, Once';
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Signed divide by 2, once';
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d2 : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Rotate eight bits left CL times';
                      LastDisassembleData.Opcode:='ROL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Rotate eight bits right CL times';
                      LastDisassembleData.Opcode:='ROR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Rotate nine bits left CL times';
                      LastDisassembleData.Opcode:='RCL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Rotate nine bits right CL times';
                      LastDisassembleData.Opcode:='RCR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Multiply by 2, CL times';
                      LastDisassembleData.Opcode:='SHL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Unsigned devide by 2, CL times';
                      LastDisassembleData.Opcode:='SHR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Multiply by 2, CL times';
                      LastDisassembleData.Opcode:='SHL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed devide by 2, CL times';
                      LastDisassembleData.Opcode:='SAR';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'CL';
                      inc(offset,last-1);
                    end;


              end;
            end;

      $d3 : begin
              case getReg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits left CL times';
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits left CL times';
                        LastDisassembleData.Opcode:='ROL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 16 bits right CL times';
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 32 bits right CL times';
                        LastDisassembleData.Opcode:='ROR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits left CL times';
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits left CL times';
                        LastDisassembleData.Opcode:='RCL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Rotate 17 bits right CL times';
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Rotate 33 bits right CL times';
                        LastDisassembleData.Opcode:='RCR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Multiply by 2, CL times';
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Multiply by 2, CL times';
                        LastDisassembleData.Opcode:='SHL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Unsigned divide by 2, CL times';
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Unsigned divide by 2, CL times';
                        LastDisassembleData.Opcode:='SHR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='Signed divide by 2, CL times';
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'CL';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='Signed divide by 2, CL times';
                        LastDisassembleData.Opcode:='SAR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'CL';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $D4 : begin  // AAM
              inc(offset);
              LastDisassembleData.Opcode:='AAM';
              description:='ASCII Adjust AX After Multiply';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if memory[1]<>$0A then
                LastDisassembleData.parameters:=inttohexs(memory[1],2);
            end;

      $D5 : begin  // AAD
              inc(offset);
              LastDisassembleData.Opcode:='AAD';
              description:='ASCII adjust AX before division';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              if memory[1]<>$0A then LastDisassembleData.parameters:=inttohexs(memory[1],2);
            end;

      $D7 : begin
              description:='Table Look-up Translation';
              LastDisassembleData.Opcode:='XLATB';
            end;

      $d8 : begin
              case getReg(memory[1]) of
                0:  begin
                      //fadd
                      description:='Add';
                      LastDisassembleData.Opcode:='FADD';
                      last:=2;
                      if memory[1]>=$c0 then
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$c0)+')' else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        LastDisassembleData.Opcode:='FMUL';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$c8)+')';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FMUL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='Compare Real';
                      last:=2;
                      if memory[1]>=$d0 then
                      begin
                        LastDisassembleData.Opcode:='FCOM';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$d0)+')'
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FCOM';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Compare Real and pop register stack';
                      last:=2;
                      if memory[1]>=$d8 then
                      begin
                        LastDisassembleData.Opcode:='FCOMP';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$d8)+')'
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FCOMP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Substract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        LastDisassembleData.Opcode:='FSUB';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$e0)+')';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FSUB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Reverse Substract';
                      last:=2;
                      if (memory[1]>=$e8) then
                      begin
                        LastDisassembleData.Opcode:='FSUBR';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$e0)+')';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FSUBR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        LastDisassembleData.Opcode:='FDIV';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$d8)+')';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FDIV';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Reverse Divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        LastDisassembleData.Opcode:='FDIVR';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$d8)+')';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FDIVR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;
              end;

            end;

      $d9 : begin
              case memory[1] of
              $00..$bf : begin
                           case getReg(memory[1]) of
                           0:  begin
                                 description:='Load Floating Point Value';
                                 LastDisassembleData.Opcode:='FLD';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           2:  begin
                                 description:='Store Real';
                                 LastDisassembleData.Opcode:='FST';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           3:  begin
                                 description:='Store Real';
                                 LastDisassembleData.Opcode:='FSTP';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           4:  begin
                                 description:='Load FPU Environment';
                                 LastDisassembleData.Opcode:='FLDENV';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           5:  begin
                                 description:='Load Control Word';
                                 LastDisassembleData.Opcode:='FLDCW';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           6:  begin
                                 description:='Store FPU Environment';
                                 LastDisassembleData.Opcode:='FNSTENV';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           7:  begin
                                 description:='Store Control Word';
                                 LastDisassembleData.Opcode:='FNSTCW';
                                 LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;


                           end;
                         end;

              $c0..$c7 : begin
                           description:='Push ST(i) onto the FPU register stack';
                           LastDisassembleData.Opcode:='FLD';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $c8..$cf : begin
                           description:='Exchange Register Contents';
                           LastDisassembleData.Opcode:='FXCH';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c8)+')';
                           inc(offset);
                         end;


              $d9..$df : begin
                           description:='Exchange Register contents';
                           LastDisassembleData.Opcode:='FXCH';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$d9)+')';
                           inc(offset);
                         end;


              $d0 : begin
                      description:='No Operation';
                      LastDisassembleData.Opcode:='FNOP';
                      inc(offset);
                    end;

              $e0 : begin
                      description:='Change Sign';
                      LastDisassembleData.Opcode:='FCHS';
                      inc(offset);
                    end;

              $e1 : begin
                      description:='Absolute Value';
                      LastDisassembleData.Opcode:='FABS';
                      inc(offset);
                    end;

              $e4 : begin
                      description:='TEST';
                      LastDisassembleData.Opcode:='FTST';
                      inc(offset);
                    end;

              $e5 : begin
                      description:='Examine';
                      LastDisassembleData.Opcode:='FXAM';
                      inc(offset);
                    end;



              $e8 : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLD1';
                      inc(offset);
                    end;

              $e9 : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLDL2T';
                      inc(offset);
                    end;

              $ea : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLD2E';
                      inc(offset);
                    end;

              $eb : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLDPI';
                      inc(offset);
                    end;

              $ec : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLDLG2';
                      inc(offset);
                    end;

              $ed : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLDLN2';
                      inc(offset);
                    end;

              $ee : begin
                      description:='Load constant';
                      LastDisassembleData.Opcode:='FLDZ';
                      inc(offset);
                    end;


              $f0 : begin
                      description:='Compute 2^x–1';
                      LastDisassembleData.Opcode:='F2XM1';
                      inc(offset);
                    end;

              $f1 : begin
                      description:='Compute y*log(2)x';
                      LastDisassembleData.Opcode:='FYL2X';
                      inc(offset);
                    end;

              $f2 : begin
                      description:='Partial Tangent';
                      LastDisassembleData.Opcode:='FPTAN';
                      inc(offset);
                    end;

              $f3 : begin
                      description:='Partial Arctangent';
                      LastDisassembleData.Opcode:='FPATAN';
                      inc(offset);
                    end;

              $f4 : begin
                      description:='Extract Exponent and Significand';
                      LastDisassembleData.Opcode:='FXTRACT';
                      inc(offset);
                    end;

              $f5 : begin
                      description:='Partial Remainder';
                      LastDisassembleData.Opcode:='FPREM1';
                      inc(offset);
                    end;

              $f6 : begin
                      description:='Decrement Stack-Top Pointer';
                      tempresult:='FDECSTP';
                      inc(offset);
                    end;

              $f7 : begin
                      description:='Increment Stack-Top Pointer';
                      tempresult:='FINCSTP';
                      inc(offset);
                    end;

              $f8 : begin
                      description:='Partial Remainder';
                      LastDisassembleData.Opcode:='FPREM';
                      inc(offset);
                    end;

              $f9 : begin
                      description:='Compute y*log(2)(x+1)';
                      LastDisassembleData.Opcode:='FYL2XP1';
                      inc(offset);
                    end;

              $fa : begin
                      description:='Square Root';
                      LastDisassembleData.Opcode:='FSQRT';
                      inc(offset);
                    end;

              $fb : begin
                      description:='Sine and Cosine';
                      LastDisassembleData.Opcode:='FSINCOS';
                      inc(offset);
                    end;


              $fc : begin
                      description:='Round to Integer';
                      LastDisassembleData.Opcode:='FRNDINT';
                      inc(offset);
                    end;

              $fd : begin
                      description:='Scale';
                      LastDisassembleData.Opcode:='FSCALE';
                      inc(offset);
                    end;

              $fe : begin
                      description:='Sine';
                      LastDisassembleData.Opcode:='FSIN';
                      inc(offset);
                    end;

              $ff : begin
                      description:='Cosine';
                      LastDisassembleData.Opcode:='FCOS';
                      inc(offset);
                    end;
              end;
            end;

      $da : begin
              if memory[1]<$BF then
              begin
                case getReg(memory[1]) of
                  0:  begin
                        description:='Add';
                        LastDisassembleData.Opcode:='FIADD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  1:  begin
                        description:='Multiply';
                        LastDisassembleData.Opcode:='FIMUL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  2:  begin
                        description:='Compare Integer';
                        LastDisassembleData.Opcode:='FICOM';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  3:  begin
                        description:='Compare Integer';
                        LastDisassembleData.Opcode:='FICOMP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  4:  begin
                        description:='Subtract';
                        LastDisassembleData.Opcode:='FISUB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  5:  begin
                        description:='Reverse Subtract';
                        LastDisassembleData.Opcode:='FISUBR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;


                  6:  begin
                        description:='Devide';
                        LastDisassembleData.Opcode:='FIDIV';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  7:  begin
                        description:='Reverse Devide';
                        LastDisassembleData.Opcode:='FIDIVR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;
                end;
              end
              else
              begin
                case getReg(memory[1]) of
                  0:  begin
                        description:='Floating-Point: Move if below';
                        tempresult:='FCMOVB ST(0),ST('+IntToStr(memory[1]-$c0)+')';
                        inc(offset);
                      end;

                  1:  begin
                        description:='Floating-Point: Move if equal';
                        tempresult:='FCMOVE ST(0),ST('+IntToStr(memory[1]-$c8)+')';
                        inc(offset);
                      end;

                  2:  begin
                        description:='Floating-Point: Move if below or equal';
                        tempresult:='FCMOVBE ST(0),ST('+IntToStr(memory[1]-$d0)+')';
                        inc(offset);
                      end;

                  3:  begin
                        description:='Floating-Point: Move if unordered';
                        tempresult:='FCMOVU ST(0),ST('+IntToStr(memory[1]-$d8)+')';
                        inc(offset);
                      end;

                  5:  begin
                        case memory[1] of
                        $e9 : begin
                                description:='Unordered Compare Real';
                                LastDisassembleData.Opcode:='FUCOMPP';
                                inc(offset);
                              end;
                        end;
                      end;
                end;
              end;
            end;

      $db : begin
              case memory[1] of
                $0..$bf : begin
                            case getReg(memory[1]) of
                              0:  begin
                                    description:='Load Integer';
                                    LastDisassembleData.Opcode:='FILD';
                                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              2:  begin
                                    description:='Store Integer';
                                    LastDisassembleData.Opcode:='FIST';
                                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              3:  begin
                                    description:='Store Integer';
                                    LastDisassembleData.Opcode:='FISTP';
                                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    description:='Load Floating Point Value';
                                    LastDisassembleData.Opcode:='FLD';
                                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                    inc(offset,last-1);
                                  end;

                              7:  begin
                                    description:='Store Real';
                                    LastDisassembleData.Opcode:='FSTP';
                                    LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                                    inc(offset,last-1);
                                  end;

                            end;
                          end;

                $c0..$c7 : begin
                             description:='Floating-Point: Move if not below';
                             tempresult:='FCMOVNB ST(0),ST('+IntToStr(memory[1]-$c0)+')';
                             inc(offset);
                           end;

                $c8..$cf : begin
                             description:='Floating-Point: Move if not equal';
                             tempresult:='FCMOVNE ST(0),ST('+IntToStr(memory[1]-$c8)+')';
                             inc(offset);
                           end;

                $d0..$d7 : begin
                             description:='Floating-Point: Move if not below or equal';
                             tempresult:='FCMOVNBE ST(0),ST('+IntToStr(memory[1]-$d0)+')';
                             inc(offset);
                           end;

                $d8..$df : begin
                             description:='Floating-Point: Move if not unordered';
                             tempresult:='FCMOVNU ST(0),ST('+IntToStr(memory[1]-$d8)+')';
                             inc(offset);
                           end;

                $e2 : begin
                        description:='Clear Exceptions';
                        LastDisassembleData.Opcode:='FNCLEX';
                        inc(offset);
                      end;

                $e3 : begin
                        description:='Initialize floating-Point Unit';
                        LastDisassembleData.Opcode:='FNINIT';
                        inc(offset);
                      end;

                $e8..$ef : begin
                             description:='Floating-Point: Compare Real and Set EFLAGS';
                             tempresult:='FUCOMI ST(0),ST('+IntToStr(memory[1]-$e8)+')';
                             inc(offset);
                           end;

                $f0..$f7 : begin
                             description:='Floating-Point: Compare Real and Set EFLAGS';
                             tempresult:='FCOMI ST(0),ST('+IntToStr(memory[1]-$f0)+')';
                             inc(offset);
                           end;
              end;


            end;

      $dc : begin
              case getReg(memory[1]) of
                0:  begin
                      //fadd
                      description:='Add';
                      last:=2;
                      if memory[1]>=$c0 then
                      begin
                        LastDisassembleData.Opcode:='FADD';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+'),ST(0)'
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FADD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        LastDisassembleData.Opcode:='FMUL';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c8)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FMUL';
                         LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Compare Real';
                      last:=2;
                      LastDisassembleData.Opcode:='FCOM';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Compare Real';
                      last:=2;
                      LastDisassembleData.Opcode:='FCOMP';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Subtract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        LastDisassembleData.Opcode:='FSUBR';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$e0)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FSUB';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Reverse Subtract';
                      last:=2;
                      if memory[1]>=$e8 then
                      begin
                        LastDisassembleData.Opcode:='FSUB';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$e8)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FSUBR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;


                      inc(offset,last-1);
                    end;


                6:  begin
                      description:='Divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        LastDisassembleData.Opcode:='FDIVR';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$f0)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FDIV';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Reverse Divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        LastDisassembleData.Opcode:='FDIV';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$f8)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FDIVR';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;
              end;
            end;

      $dd : begin
              case memory[1] of
              $0..$bf :  begin
                           case getReg(memory[1]) of
                             0:  begin
                                   description:='Load floating point value';
                                   LastDisassembleData.Opcode:='FLD';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             2:  begin
                                   description:='Store Real';
                                   LastDisassembleData.Opcode:='FST';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             3:  begin
                                   description:='Store Real';
                                   LastDisassembleData.Opcode:='FSTP';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             4:  begin
                                   description:='Restore FPU State';
                                   LastDisassembleData.Opcode:='FRSTOR';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             6:  begin
                                   description:='Store FPU State';
                                   LastDisassembleData.Opcode:='FSAVE';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             7:  begin
                                   description:='Store Status Word';
                                   LastDisassembleData.Opcode:='FNSTSW';
                                   LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                           end;

                         end;

              $c0..$c7 : begin
                           description:='Free Floating-Point Register';
                           LastDisassembleData.Opcode:='FFREE';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $d0..$d7 : begin
                           description:='Store Real';
                           LastDisassembleData.Opcode:='FST';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$d0)+')';
                           inc(offset);
                         end;

              $d8..$df : begin
                           description:='Store Real';
                           LastDisassembleData.Opcode:='FSTP';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$d8)+')';
                           inc(offset);
                         end;

              $e0..$e7 : begin
                           description:='Unordered Compare Real';
                           LastDisassembleData.Opcode:='FUCOM';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$e0)+')';
                           inc(offset);
                         end;

              $e8..$ef : begin
                           description:='Unordered Compare Real';
                           LastDisassembleData.Opcode:='FUCOMP';
                           LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$e0)+')';
                           inc(offset);
                         end;
                else
                begin
                  LastDisassembleData.Opcode:='DB';
                  LastDisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $de : begin
              case getReg(memory[1]) of
                0:  begin
                      //faddp
                      description:='Add and pop';
                      last:=2;
                      if (memory[1]=$c1) then LastDisassembleData.Opcode:='FADDP'
                      else
                      if memory[1]>$c0 then
                      begin
                        LastDisassembleData.Opcode:='FADDP';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+'),ST(0)';
                      end
                      else
                      begin
                        description:='Add';
                        LastDisassembleData.Opcode:='FIADD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      end;
                      inc(offset,last-1);
                    end;

                1: begin
                      description:='Multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        LastDisassembleData.Opcode:='FMULP';
                        LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+'),ST(0)';
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='FIMUL';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      end;

                      inc(offset,last-1);
                   end;

                2: begin
                     description:='Compare Integer';
                     last:=2;
                     LastDisassembleData.Opcode:='FICOM';
                     LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                     inc(offset,last-1);
                   end;


                3: begin
                     if memory[1]<$c0 then
                     begin
                       description:='Compare Integer';
                       LastDisassembleData.Opcode:='FICOMP';
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                       inc(offset,last-1);
                     end;

                     if memory[1]=$D9 then
                     begin
                       description:='Compare Real and pop register stack twice';
                       LastDisassembleData.Opcode:='FCOMPP';
                       inc(offset);
                     end;
                   end;

                4: begin
                     description:='Subtract';
                     last:=2;
                     if memory[1]>=$e0 then
                     begin
                       LastDisassembleData.Opcode:='FSUBRP';
                       LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$c0)+'),ST(0)';
                     end
                     else
                     begin
                       LastDisassembleData.Opcode:='FISUB';
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;


                5: begin
                     description:='Reverse Devide';
                     last:=2;
                     if memory[1]>=$e8 then
                     begin
                       description:='Subtract and pop from stack';
                       LastDisassembleData.Opcode:='FSUBP';
                       LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$e8)+'),ST(0)'
                     end
                     else
                     begin
                       LastDisassembleData.Opcode:='FISUBR';
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;

                     inc(offset,last-1);
                   end;


                6: begin
                     description:='Reverse Devide';
                     last:=2;
                     if memory[1]>=$f0 then
                     begin
                       LastDisassembleData.Opcode:='FDIVRP';
                       LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$f0)+'),ST(0)';
                       inc(offset,last-1);
                     end
                     else
                     begin
                       lastDisassembleData.Opcode:='DB';
                       lastDisassembleData.parameters:='DE'
                     end;
                   end;

                7: begin
                     description:='Devide';
                     last:=2;
                     if memory[1]>=$f8 then
                     begin
                       LastDisassembleData.Opcode:='FDIVP';
                       LastDisassembleData.parameters:='ST('+IntToStr(memory[1]-$f8)+'),ST(0)';
                     end
                     else
                     begin
                       LastDisassembleData.Opcode:='FDIVR';
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;

              end;
            end;

      $df : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Load Integer';
                      tempresult:='FILD';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='Store Integer';
                      tempresult:='FIST';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Store Integer';
                      tempresult:='FISTP';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Load Binary Coded Decimal';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        LastDisassembleData.Opcode:='FNSTSW';
                        LastDisassembleData.parameters:='AX';
                      end
                      else
                      begin
                        tempresult:='FBLD';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                      end;
                      inc(offset,last-1);
                    end;

               5:  begin
                     if memory[1]<$c0 then
                     begin
                       description:='Load Integer';
                       tempresult:='FILD';
                       LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                       inc(offset,last-1);
                     end;

                     if memory[1]>=$e8 then
                     begin
                       description:='Compare Real and Set EFLAGS';
                       tempresult:='FUCOMIP';
                       LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$e8)+')';
                       inc(offset);
                     end;
                   end;

               6:  begin
                      if memory[1]>=$f0 then
                      begin
                        description:='Compare Real and Set EFLAGS';
                        tempresult:='FCOMI';
                        LastDisassembleData.parameters:='ST(0),ST('+IntToStr(memory[1]-$f0)+')';
                      end
                      else
                      begin
                        description:='Store BCD Integer and Pop';
                        tempresult:='FBSTP';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      description:='Store Integer';
                      tempresult:='FISTP';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                else
                begin
                  LastDisassembleData.Opcode:='DB';
                  LastDisassembleData.parameters:=inttohexs(memory[0],2);
                end;
              end;

            end;

      $e0 : begin
              description:='Loop According to ECX counter';

              LastDisassembleData.parameterValueType:=dvtAddress;
              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+pshortint(@memory[1])^)
              else
                LastDisassembleData.parameterValue:=dword(offset+pshortint(@memory[1])^);


              if $66 in prefix2 then
                LastDisassembleData.Opcode:='LOOPNE'
              else
                LastDisassembleData.Opcode:='LOOPNZ';

              inc(offset);
              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $e1 : begin
              description:='Loop According to ECX counter';
              if $66 in prefix2 then
              begin
                LastDisassembleData.Opcode:='LOOPE';
              end
              else
              begin
                LastDisassembleData.Opcode:='LOOPZ';
              end;
              inc(offset);

              LastDisassembleData.parameterValueType:=dvtAddress;
              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+pshortint(@memory[1])^)
              else
                LastDisassembleData.parameterValue:=dword(offset+pshortint(@memory[1])^);

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $e2 : begin
              description:='Loop According to ECX counting';
              LastDisassembleData.Opcode:='LOOP';
              inc(offset);

              LastDisassembleData.parameterValueType:=dvtAddress;
              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+pshortint(@memory[1])^)
              else
                LastDisassembleData.parameterValue:=dword(offset+pshortint(@memory[1])^);

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $e3 : begin
              description:='Jump short if CX=0';
              if $66 in prefix2 then
                LastDisassembleData.Opcode:='JCXZ'
              else
                LastDisassembleData.Opcode:='JECXZ';
              inc(offset);

              LastDisassembleData.parameterValueType:=dvtAddress;



              if processhandler.is64Bit then
                LastDisassembleData.parameterValue:=qword(offset+pshortint(@memory[1])^)
              else
                LastDisassembleData.parameterValue:=dword(offset+pshortint(@memory[1])^);

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $e4 : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='IN';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              LastDisassembleData.parameters:='AL,'+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e5 : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='IN';

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);


              if $66 in prefix2 then LastDisassembleData.parameters:='AX,'+inttohexs(memory[1],2)
                                else LastDisassembleData.parameters:='EAX,'+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e6 : begin
              description:='Output to Port';
              tempresult:='OUT';
              LastDisassembleData.parameters:=inttohexs(memory[1],2)+',AL';
              inc(offset);

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount)
            end;

      $e7 : begin
              description:='Output toPort';
              LastDisassembleData.Opcode:='OUT';
              if $66 in prefix2 then
                LastDisassembleData.parameters:=inttohexs(memory[1],2)+',AX' else
                LastDisassembleData.parameters:=inttohexs(memory[1],2)+',EAX';

              inc(offset);

              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
            end;

      $e8 : begin
              //call
              //this time no $66 prefix because it will only run in win32
              description:='Call Procedure';
              LastDisassembleData.Opcode:='CALL';
              inc(offset,4);
              LastDisassembleData.parameterValueType:=dvtAddress;

              if processhandler.is64bit then
                  LastDisassembleData.parameterValue:=qword(offset+pInteger(@memory[1])^)
                else
                  LastDisassembleData.parameterValue:=dword(offset+pInteger(@memory[1])^);

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

            end;

      $e9 : begin
              description:='Jump near';
              if $66 in prefix2 then
              begin
                LastDisassembleData.Opcode:='JMP';
                inc(offset,2);
                LastDisassembleData.parameterValueType:=dvtAddress;
                LastDisassembleData.parameterValue:=dword(offset+psmallint(@memory[1])^);
                LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);
              end
              else
              begin
                LastDisassembleData.Opcode:='JMP';
                inc(offset,4);
                LastDisassembleData.parameterValueType:=dvtAddress;

                if processhandler.is64bit then
                  LastDisassembleData.parameterValue:=qword(offset+pInteger(@memory[1])^)
                else
                  LastDisassembleData.parameterValue:=dword(offset+pInteger(@memory[1])^);

                LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8)
              end;

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

            end;

      $ea : begin
              description:='Jump far';


              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);
              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=5;
              inc(LastDisassembleData.SeperatorCount);


              wordptr:=@memory[5];
              LastDisassembleData.Opcode:='JMP';
              LastDisassembleData.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              LastDisassembleData.parameterValueType:=dvtAddress;
              LastDisassembleData.parameterValue:=dwordptr^;


              LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $eb : begin
              description:='Jump short';
              LastDisassembleData.Opcode:='JMP';
              inc(offset);

              if processhandler.is64bit then
                LastDisassembleData.parameterValue:=qword(offset+pshortint(@memory[1])^)
              else
                LastDisassembleData.parameterValue:=dword(offset+pshortint(@memory[1])^);

              LastDisassembleData.parameters:=inttohexs(LastDisassembleData.parameterValue,8);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

            end;

      $ec : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='IN AL,DX';
            end;

      $ed : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='IN';
              if $66 in prefix2 then LastDisassembleData.parameters:='AX,DX' else
                                     LastDisassembleData.parameters:='EAX,DX';
            end;

      $ee : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='OUT';
              LastDisassembleData.parameters:='DX,AL';
            end;

      $ef : begin
              description:='Input from Port';
              LastDisassembleData.Opcode:='OUT';
              if $66 in prefix2 then LastDisassembleData.parameters:='DX,AX' else
                                     LastDisassembleData.parameters:='DX,EAX';
            end;

      $f3 : begin

            end;

      $f4 : begin
              description:='Halt';
              LastDisassembleData.Opcode:='HLT';
            end;

      $f5 : begin
              description:='Complement Carry Flag';
              LastDisassembleData.Opcode:='CMC';
            end;

      $f6 : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Logical Compare';
                      LastDisassembleData.Opcode:='TEST';
                      LastDisassembleData.parameterValueType:=dvtAddress;
                      LastDisassembleData.parameterValue:=memory[last];

                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      inc(offset,last);
                    end;

                2:  begin
                      description:='One''s Complement Negation';
                      LastDisassembleData.Opcode:='NOT';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Two''s Complement Negation';
                      LastDisassembleData.Opcode:='NEG';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Unsigned Multiply';
                      LastDisassembleData.Opcode:='MUL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Signed Multiply';
                      LastDisassembleData.Opcode:='IMUL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Unsigned Divide';
                      LastDisassembleData.Opcode:='DIV';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed Divide';
                      LastDisassembleData.Opcode:='IDIV';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;
                else
                begin
                  LastDisassembleData.Opcode:='DB';
                  LastDisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $f7 : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Logical Compare';
                      if $66 in prefix2 then
                      begin
                        LastDisassembleData.Opcode:='TEST';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtAddress;
                        LastDisassembleData.parameterValue:=wordptr^;

                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last+1);
                      end
                      else
                      begin
                        LastDisassembleData.Opcode:='TEST';
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        LastDisassembleData.parameterValueType:=dvtAddress;
                        LastDisassembleData.parameterValue:=dwordptr^;
                        LastDisassembleData.parameters:=LastDisassembleData.parameters+inttohexs(dwordptr^,4);
                        inc(offset,last+3);
                      end;
                    end;

                2:  begin
                      description:='One''s Complement Negation';
                      LastDisassembleData.Opcode:='NOT';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16) else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='Two''s Complement Negation';
                      LastDisassembleData.Opcode:='NEG';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='Unsigned Multiply';
                      LastDisassembleData.Opcode:='MUL';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='Signed Multiply';
                      LastDisassembleData.Opcode:='IMUL';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Unsigned Divide';
                      LastDisassembleData.Opcode:='DIV';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='Signed Divide';
                      LastDisassembleData.Opcode:='IDIV';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                  else
                  begin
                    LastDisassembleData.Opcode:='DB';
                    LastDisassembleData.parameters:=inttohexs(memory[0],2);
                  end;
                end;
            end;

      $f8 : begin
              description:='Clear Carry Flag';
              LastDisassembleData.Opcode:='CLC';
            end;

      $f9 : begin
              description:='Set Carry Flag';
              LastDisassembleData.Opcode:='STC';
            end;

      $fa : begin
              description:='Clear Interrupt Flag';
              LastDisassembleData.Opcode:='CLI';
            end;

      $fb : begin
              description:='Set Interrupt Flag';
              LastDisassembleData.Opcode:='STI';
            end;

      $fc : begin
              description:='Clear Direction Flag';
              LastDisassembleData.Opcode:='CLD';
            end;

      $fd : begin
              description:='Set Direction Flag';
              LastDisassembleData.Opcode:='STD';
            end;

      $fe : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Increment by 1';
                      LastDisassembleData.Opcode:='INC';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Decrement by 1';
                      LastDisassembleData.Opcode:='DEC';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,2,last,7);

                      inc(offset,last-1);
                    end;

                else
                begin
                  LastDisassembleData.Opcode:='DB';
                  LastDisassembleData.parameters:=inttohexs(memory[0],2);
                end;
              end;
            end;

      $ff : begin
              case getReg(memory[1]) of
                0:  begin
                      description:='Increment by 1';
                      LastDisassembleData.Opcode:='INC';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='Decrement by 1';
                      LastDisassembleData.Opcode:='DEC';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                2:  begin
                      //call
                      description:='Call Procedure';
                      LastDisassembleData.Opcode:='CALL';
                      if memory[1]>=$c0 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last) else
                      begin
                        if processhandler.is64Bit then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                      end;

                      inc(offset,last-1);
                    end;

                3:  begin
                      //call
                      description:='Call Procedure';
                      LastDisassembleData.Opcode:='CALL';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                4:  begin
                      //jmp
                      description:='Jump near';
                      LastDisassembleData.Opcode:='JMP';
                      if memory[1]>=$c0 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last) else
                      begin
                        if processhandler.is64bit then
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                      end;

                      inc(offset,last-1);
                    end;

                5:  begin
                      //jmp
                      description:='Jump far';
                      LastDisassembleData.Opcode:='JMP far';
                      LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='Push Word or Doubleword Onto the Stack';
                      LastDisassembleData.Opcode:='PUSH';
                      if $66 in prefix2 then
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,1,last)
                      else
                        LastDisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;
                else
                begin
                  LastDisassembleData.Opcode:='DB';
                  LastDisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;

            end;

      else  begin
              LastDisassembleData.Opcode:='DB';
              LastDisassembleData.parameters:=inttohex(memory[0],2);
            end;
    end;

    //strip off the , if it has one
    if (LastDisassembleData.parameters<>'') and (LastDisassembleData.parameters[length(LastDisassembleData.parameters)]=',') then
      LastDisassembleData.parameters:=copy(LastDisassembleData.parameters,0,length(LastDisassembleData.parameters)-1);

    tempresult:=tempresult+LastDisassembleData.opcode+' '+LastDisassembleData.parameters;



    LastDisassembleData.description:=description;
    j:=length(LastDisassembleData.Bytes);
    setlength(LastDisassembleData.Bytes,j+offset-startoffset);

    for i:=0 to (offset-startoffset)-1 do
    begin
      LastDisassembleData.Bytes[j+i]:=memory[i];
      result:=result+inttohex(memory[i],2)+' ';
    end;

    //adjust for the prefix.
    if j<>0 then
      for i:=1 to LastDisassembleData.SeperatorCount-1 do
        inc(LastDisassembleData.Seperators[i],prefixsize);

    //todo: Next time the disassembler is getting an averhaul, do something about the prefix counting and the unnecesary readprocessmemorys associated with it


    result:=result+'- '+tempresult;


    if riprelative then
    begin
      //add the current offset to the code between []
      inc(LastDisassembleData.modrmValue,offset);

      i:=pos('[',LastDisassembleData.parameters);
      j:=PosEx(']',LastDisassembleData.parameters,i);
      tempresult:=copy(LastDisassembleData.parameters,i+1,j-i-1);

      tempaddress:=offset+integer(strtoint('$'+tempresult));

      tempresult:=copy(LastDisassembleData.parameters,1,i);
      tempresult:=tempresult+inttohexs(tempaddress,8);
      LastDisassembleData.parameters:=tempresult+copy(LastDisassembleData.parameters,j,length(LastDisassembleData.parameters));



    end;

    result:=inttohex(LastDisassembleData.address,8)+' - ';
    for i:=0 to length(LastDisassembleData.Bytes)-1 do
    begin
      result:=result+inttohex(LastDisassembleData.Bytes[i],2);

      if i<prefixsize then
        result:=result+' '
      else
      for j:=0 to LastDisassembleData.SeperatorCount-1 do
        if (LastDisassembleData.Seperators[j]=i+1) then  //followed by a seperator
          result:=result+' ';
    end;

    result:=result+' - ';
    result:=result+LastDisassembleData.prefix+' - '+LastDisassembleData.opcode;
    result:=result+' ';
    result:=result+LastDisassembleData.parameters;



  end
  else
  begin
    result:=result+'??';
    inc(offset);
  end;

  result:=lowercase(result);
end;

function disassemble(var offset: ptrUint): string; overload;
var ignore: string;
begin
  result:=disassemble(offset,ignore);
end;



function previousopcode(address: ptrUint):ptrUint;
var x,y: ptrUint;
    s: string;
    found: boolean;
    i: ptrUint;
begin
  y:=address-40;

  while y<address do
  begin
    x:=y;
    disassemble(y,s);
  end;

  i:=address-20;
  while (i<address) and (y<>address) do
  begin
    y:=i;
    while y<address do
    begin
      x:=y;
      disassemble(y,s);
    end;
    inc(i);
  end;

  if i=address then result:=address-1 else result:=x;

 // if x<>address then result:=address-1 else result:=y;
end;


function has4ByteHexString(d: string; var hexstring: string): boolean;
var
  hexcount: integer;
  i: integer;

  lastmatch: integer;
  lasthexcount: integer;
begin
  result:=false;
  hexcount:=0;
  lasthexcount:=0;

  for i:=length(d) downto 1 do
  begin
    if d[i] in ['a'..'f','A'..'F','0'..'9'] then
    begin
      inc(hexcount);

      if hexcount>lasthexcount then
      begin
        lastmatch:=i;
        lasthexcount:=hexcount;
      end;
    end
    else
      hexcount:=0;
  end;

  if lasthexcount>=8 then
  begin
    //it has at least a 4 byte hexadecimal value, so an address specifier
    hexstring:='$'+copy(d,lastmatch,lasthexcount);
    result:=true;
  end;
end;

function hasAddress(d: string; var address: ptrUint; context: PContext=nil):boolean;
{
returns if the opcode has an accessible address specifier or not, and the address
}
var
  s: string;
  i,j: integer;
  x: dword;

  haserror: boolean;
begin
  result:=false;

  if d='' then exit;
  if pos(' ',d)=0 then exit;

  //if the opcode has a , then get the last part
  i:=pos(',',d);
  if i>0 then
  begin
    d:=copy(d,i+1,length(d));
  end;

  if (context=nil) then
  begin
    if pos('+',d)>0 then exit; //it has an offset, so also a register. without a context, this is impossible

    //check O for a hexadecimal value of 8 bytes and longer.
    if has4ByteHexString(d,s) then
    begin
      address:=strtoint64(s); //s already has the $ in front
      result:=isAddress(address);
    end;
  end else
  begin
    //a slower but more effective address detector
    //strip of everything before the space, and if there's a [ ] get what's inbetween
    //then use the symbolhandler to find out what it is
    i:=pos(' ',d);
    if i>0 then //it has a space , so a instruction is still present, strip it
      d:=copy(d,i+1,length(d));

    i:=pos('[',d);
    if i>0 then
    begin
      d:=copy(d,i+1,pos(']',d)-i-1);

    end;

    address:=symhandler.getAddressFromName(d,false,haserror, context);
    result:=not haserror;
  end;



end;

function TDisassembler.opcode4FST(opcode: string): integer;
begin
  result:=3; //float
  if length(opcode)>=4 then
  begin
    case opcode[4] of
      'c','e','s': result:=2;
    end;
  end;
end;

function TDisassembler.opcode3FN(opcode: string): integer;
begin
  result:=3; //float
  if length(opcode)>=3 then
  begin
    case opcode[3] of
      's' : result:=2; //fnst
    end;
  end;
end;

function TDisassembler.opcode3FS(opcode: string): integer;
begin
  result:=3; //float
  if length(opcode)>=3 then
  begin
    case opcode[3] of
      't' : result:=opcode4FST(opcode); //fstxxx  (fst, fstp, fstcw, fstenv, fstsw)
    end;
  end;
end;

function TDisassembler.opcode2F(opcode: string): integer;
begin
  result:=3;
  if length(opcode)>=2 then
  begin
    case opcode[2] of
      'i' : result:=2; //fixxxxx
      'n' : result:=opcode3FN(opcode);
      's' : result:=opcode3FS(opcode);
    end;
  end;
end;

function tdisassembler.opcodeToValueType(opcode: string): integer;
{
figures out what type of memory access this opcode does
returns 2 if unsure
}
begin
  result:=2;
  if length(opcode)>=1 then
  begin
    case opcode[1] of
      'f' : result:=opcode2F(opcode);
    end;
  end;
end;

procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);
begin
  defaultDisassembler.splitDisassembledString(disassembled, showvalues, address,bytes, opcode, special, context);
end;

procedure tdisassembler.splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);
var offset,value:ptrUint;
    e: integer;
    i,j,j2,k,l: integer;
    ts,ts2,ts3: string;
    actualread: dword;
    valuetype: integer;

//    tokens: ttokens;
    fvalue: single;
    fvalue2: double;
    tempbuf: array [0..127] of byte;

    pc: pchar;
    pwc: pwidechar;

    variableType: TVariableType;
    tempaddress: ptrUint;
    err: boolean;
    isjumper: boolean;
    hexstring: string;
begin
  i:=pos(' - ',disassembled);
  address:=uppercase(copy(disassembled,1,i-1));

  inc(i,3);
  j:=PosEx(' - ',disassembled,i);
  if j=0 then j:=length(disassembled)+1;
  bytes:=copy(disassembled,i,(j-i));

  inc(j,3);
  k:=PosEx(' : ',disassembled,j);
  l:=k;
  if k=0 then
    k:=length(disassembled)+1;

  opcode:=copy(disassembled,j,(k-j));

  if showvalues then
  begin
    ts:='';
    special:='';

    if (hasAddress(opcode, tempaddress, context)) or ((length(opcode)>3) and (opcode[1]='l') and (opcode[2]='e') and (opcode[3]='a')) then
    begin
      if isaddress(tempaddress) then
      begin
        try
          if (opcode[1]='l') and (opcode[2]='e') and (opcode[3]='a') then //lea
          begin
            j:=pos('[',opcode);
            j2:=pos(']',opcode);
            ts2:=copy(opcode,j+1,j2-j-1);


            tempaddress:=symhandler.getAddressFromName(ts2,false,err);
            if err then exit; //error

          end;
        except
          tempaddress:=0;
        end;

        isjumper:=false;
        if opcode[1]='j' then isjumper:=true; //jmp, jx
        if (opcode[1]='l') and (opcode[2]='o') and (opcode[3]='o') then isjumper:=true; //loop
        if (opcode[1]='c') and (opcode[2]='a') then isjumper:=true; //call


        valuetype:=opcodeToValueType(opcode);
        {
        //tip: Replace with a function that checks for each opcode what value type it handles
        if (opcode[1]='f') then //fxxx
        begin
          valuetype:=3;
          if length(opcode)>3 then
          begin
            if opcode[2]='i' then valuetype:=2;  //fixxxx
            if (opcode[2]='n') and (opcode[3]='s') then valuetype:=2;   //fns
          end;
        end else valuetype:=2;
        }

        i:=pos('[',disassembled);
        if i>0 then
        begin
          //it might have an override
          if pos('qword ptr',opcode)>0 then valuetype:=4 else //usually a double
          if pos('dword ptr',opcode)>0 then valuetype:=2 else
          if pos('word ptr',opcode)>0 then valuetype:=1 else
          if pos('byte ptr',opcode)>0 then valuetype:=0 else
          begin
            //check the register used
            j2:=pos(',[',opcode);
            k:=pos('],',opcode);
            if j2>0 then //register in front
            begin
              l:=pos(' ',opcode);
              ts3:=copy(opcode,l+1,j2-l-1);

              case TokenToRegisterbit(uppercase(ts3)) of
                ttRegister8Bit: ValueType:=0;
                ttRegister16Bit: valuetype:=1;
                ttRegister32Bit: valuetype:=2;
                else valuetype:=2;
              end;
            end
            else
            if k>0 then  //register after ],
            begin
              l:=pos('],',opcode);
              ts3:=copy(opcode,l+2,length(opcode)-l-1);

              case TokenToRegisterbit(uppercase(ts3)) of
                ttRegister8Bit: valuetype:=0;
                ttRegister16Bit: valuetype:=1;
                ttRegister32Bit: valuetype:=2;
                else valuetype:=2;
              end;
            end; //else no idea, check var
          end;
        end; //not an address specifier

        if valuetype=2 then
        begin
          if ReadProcessMemory(processhandle, pointer(tempaddress), @tempbuf[0], 16, actualread) then
          begin
            variableType:=FindTypeOfData(tempaddress, @tempbuf[0],16);

            case variableType of
              vtSingle: ValueType:=3;
              vtDouble: ValueType:=4;
              vtString: ValueType:=5;
              vtUnicodeString: ValueType:=6;
            end;

          end;

        end;


          if isjumper then
            valuetype:=2; //handle it as a dword

        value:=0;
        fvalue:=0;
        fvalue2:=0;
        case valuetype of
          0: if readprocessmemory(processhandle,pointer(tempaddress),@value,1,actualread) then ts:=inttohex(value,2);
          1: if readprocessmemory(processhandle,pointer(tempaddress),@value,2,actualread) then ts:=inttohex(value,4);
          2: if readprocessmemory(processhandle,pointer(tempaddress),@value,4,actualread) then
          begin

            if isjumper and ((value and $ffff)=$25ff) then //it's a jmp [xxxxxxxx]    / call [xxxxxx] ...
            begin
              value:=0;
              if readprocessmemory(processhandle,pointer(tempaddress+2),@value,4,actualread) then
              begin
                if processhandler.is64Bit then
                  value:=tempaddress+6+value;

                if readprocessmemory(processhandle,pointer(value),@value,processhandler.Pointersize,actualread) then
                begin
                  ts:='->'+symhandler.getNameFromAddress(value);
                end;
              end;
            end
            else
            begin
              ts:=symhandler.getNameFromAddress(value);
            end;


            if isjumper then
            begin
              //check if ts is a name or a hexadecimal value
              //if hex, don't use it
              val('$'+ts,j, i);
              if i=0 then
                ts:=''; //zero the string, it's a hexadecimal string
            end;

          end;
          3: if readprocessmemory(processhandle,pointer(tempaddress),@fvalue,4,actualread) then ts:=format('(float)%.4f',[fvalue]);
          4: if readprocessmemory(processhandle,pointer(tempaddress),@fvalue2,8,actualread) then ts:=format('(double)%.4f',[fvalue2]);
          5:
          begin
            actualread:=0;
            ReadProcessMemory(processhandle, pointer(tempaddress), @tempbuf[0], 128, actualread);

            tempbuf[127]:=0;
            tempbuf[126]:=ord('.');
            tempbuf[125]:=ord('.');
            tempbuf[124]:=ord('.');

            if actualread>0 then
              tempbuf[actualread-1]:=0;
              
            pc:=@tempbuf[0];
            ts:='"'+ pc+'"';
          end;

          6:
          begin
            actualread:=0;
            ReadProcessMemory(processhandle, pointer(tempaddress), @tempbuf[0], 128, actualread);
                      
            tempbuf[127]:=0;
            tempbuf[126]:=0;

            tempbuf[125]:=0;
            tempbuf[124]:=ord('.');
            tempbuf[123]:=0;
            tempbuf[122]:=ord('.');
            tempbuf[121]:=0;            
            tempbuf[120]:=ord('.');

            if actualread>1 then
            begin
              tempbuf[actualread-1]:=0;
              tempbuf[actualread-2]:=0;
            end;

            pwc:=@tempbuf[0];
            ts:='""'+ pwc+'""';
          end;
        end;

        if ts<>'' then
          ts:='['+ts+']';
      end
      else
      begin
        //tempaddress doesn't seem to be an address
        variableType:=FindTypeOfData(0, @tempaddress,processhandler.pointersize);
        if variableType=vtsingle then
          ts:=format('(float)%.4f',[psingle(@tempaddress)^]);
      end;

    end;

    special:=ts;

  end else special:='';
end;

function TDisAssembler.inttohexs_withoutsymbols(address:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
begin
  result:=sysutils.IntToHex(address,chars);
 // if syntaxhighlighting then result:='[COL1]'+result+'[/COL1]';
end;

function TDisAssembler.inttohexs_withsymbols(address:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
var found: boolean;
begin
  if showsymbols and (chars=8) then
  begin
    result:=symhandler.getNameFromAddress(address,found);
  {  if syntaxhighlighting then
    begin
      if not found then
        result:='[COL1]'+result+'[/COL1]'
      else
        result:='[COL2]'+result+'[/COL2]';


    end;  }

  end
  else
  begin
    result:=sysutils.IntToHex(address,chars);
    //if syntaxhighlighting then result:='[COL1]'+result+'[/COL1]';
  end;


end;

initialization
  defaultDisassembler:=TDisassembler.create;
  defaultDisassembler.isdefault:=true;
  defaultDisassembler.syntaxhighlighting:=false;

  visibleDisassembler:=TDisassembler.Create;
  visibleDisassembler.syntaxhighlighting:=true;

finalization
  if visibleDisassembler<>nil then
    freeandnil(visibleDisassembler);

  if defaultDisassembler<>nil then
    freeandnil(defaultDisassembler);

end.



