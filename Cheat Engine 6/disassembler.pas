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

type
  TDisAssemblerValueType=(dvtNone, dvtAddress, dvtValue);

  TDisassembler=class
  private
    inttohexs: TIntToHexS;
    RexPrefix: byte;
    riprelative: boolean;

    colorhex: string;
    colorreg: string;
    colorsymbol: string;
    endcolor: string;

    fsyntaxhighlighting: boolean;

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

    procedure setSyntaxHighlighting(state: boolean);

  public
    isdefault: boolean;
    showsymbols: boolean;
    showmodules: boolean;
    dataOnly: boolean;


    LastDisassembleData: record
      address: PtrUint;
      prefix: string;
      prefixsize: integer;
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
    //  ValueType: TValueType; //if it's not unknown the value type will say what type of value it is (e.g for the FP types)

      isjump: boolean; //set for anything that can change eip/rip
      iscall: boolean; //set if it's a call
      isret: boolean; //set if it's a ret
      isconditionaljump: boolean; //set if it's only effective when an conditon is met
    end;



//    showvalues: boolean;
    function disassemble(var offset: ptrUint; var description: string): string;
    procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);

    function DecodeLastParametersToString: string; //returns the special line splitDisassemblerstring used to return

    function getLastBytestring: string;

    property syntaxhighlighting: boolean read fsyntaxhighlighting write setSyntaxHighlighting;
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


var defaultDisassembler: TDisassembler;

implementation

//dont use it by otherunits
{$ifndef net}
{$ifndef standalonetrainer}
uses Assemblerunit,CEDebugger, debughelper, StrUtils;
{$endif}
{$endif}




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

  result:=colorreg+result+endcolor;
end;


function TDisassembler.rd8(bt:byte): string;
begin
  case bt of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';

  4: if rexprefix=0 then result:='ah' else result:='spl';
  5: if rexprefix=0 then result:='ch' else result:='bpl';
  6: if rexprefix=0 then result:='dh' else result:='sil';
  7: if rexprefix=0 then result:='bh' else result:='dil';
  8: result:='r8l';
  9: result:='r9l';
  10: result:='r10l';
  11: result:='r11l';
  12: result:='r12l';
  13: result:='r13l';
  14: result:='r14l';
  15: result:='r15l';
  end;

  result:=colorreg+result+endcolor;
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
  8: result:='r8w';
  9: result:='r9w';
  10: result:='r10w';
  11: result:='r11w';
  12: result:='r12w';
  13: result:='r13w';
  14: result:='r14w';
  15: result:='r15w';
  end;
  result:=colorreg+result+endcolor;
end;


function TDisassembler.r8(bt:byte): string;
begin

  case getreg(bt) of
  0: result:='al';
  1: result:='cl';
  2: result:='dl';
  3: result:='bl';

  4: if rexprefix=0 then result:='ah' else result:='spl';
  5: if rexprefix=0 then result:='ch' else result:='bpl';
  6: if rexprefix=0 then result:='dh' else result:='sil';
  7: if rexprefix=0 then result:='bh' else result:='dil';
  8: result:='r8l';
  9: result:='r9l';
  10: result:='r10l';
  11: result:='r11l';
  12: result:='r12l';
  13: result:='r13l';
  14: result:='r14l';
  15: result:='r15l';
  end;
  result:=colorreg+result+endcolor;
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
  8: result:='r8w';
  9: result:='r9w';
  10: result:='r10w';
  11: result:='r11w';
  12: result:='r12w';
  13: result:='r13w';
  14: result:='r14w';
  15: result:='r15w';
  end;
  result:=colorreg+result+endcolor;
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
    8: if rex_w then result:='r8' else result:='r8d';
    9: if rex_w then result:='r9' else result:='r9d';
   10: if rex_w then result:='r10' else result:='r10d';
   11: if rex_w then result:='r11' else result:='r11d';
   12: if rex_w then result:='r12' else result:='r12d';
   13: if rex_w then result:='r13' else result:='r13d';
   14: if rex_w then result:='r14' else result:='r14d';
   15: if rex_w then result:='r15' else result:='r15d';
  end;
  result:=colorreg+result+endcolor;
end;

function TDisassembler.xmm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='xmm0';
  1: result:='xmm1';
  2: result:='xmm2';
  3: result:='xmm3';
  4: result:='xmm4';
  5: result:='xmm5';
  6: result:='xmm6';
  7: result:='xmm7';
  8: result:='xmm8';
  9: result:='xmm9';
  10: result:='xmm10';
  11: result:='xmm11';
  12: result:='xmm12';
  13: result:='xmm13';
  14: result:='xmm14';
  15: result:='xmm15';
  end;
  result:=colorreg+result+endcolor;
end;

function TDisassembler.mm(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='mm0';
  1: result:='mm1';
  2: result:='mm2';
  3: result:='mm3';
  4: result:='mm4';
  5: result:='mm5';
  6: result:='mm6';
  7: result:='mm7';
  8: result:='mm8';
  9: result:='mm9';
  10: result:='mm10';
  11: result:='mm11';
  12: result:='mm12';
  13: result:='mm13';
  14: result:='mm14';
  15: result:='mm15';

  end;
  result:=colorreg+result+endcolor;
end;

function TDisassembler.sreg(bt:byte): string;
begin
  case getreg(bt) of
  0: result:='es';
  1: result:='cs';
  2: result:='ss';
  3: result:='ds';
  4: result:='fs';
  5: result:='gs';
  6: result:='hs';  //as if...
  7: result:='is';
  8: result:='js';
  9: result:='ks';
 10: result:='ls';
 11: result:='ms';
 12: result:='ns';
 13: result:='os';
 14: result:='ps';
 15: result:='qs';
  end;
  result:=colorreg+result+endcolor;
end;

function TDisassembler.CR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='cr0';
  1: result:='cr1';
  2: result:='cr2';
  3: result:='cr3';
  4: result:='cr4';
  5: result:='cr5';
  6: result:='cr6';
  7: result:='cr7';
  8: result:='cr8';
  9: result:='cr9';
  10: result:='cr10';
  11: result:='cr11';
  12: result:='cr12';
  13: result:='cr13';
  14: result:='cr14';
  15: result:='cr15';
  end;
  result:=colorreg+result+endcolor;
end;

function TDisassembler.DR(bt:byte):string;
begin
  case getreg(bt) of
  0: result:='dr0';
  1: result:='dr1';
  2: result:='dr2';
  3: result:='dr3';
  4: result:='dr4';
  5: result:='dr5';
  6: result:='dr6';
  7: result:='dr7';
  8: result:='dr8';//Do not excist, but let's implement the encoding
  9: result:='dr9';
  10: result:='dr10';
  11: result:='dr11';
  12: result:='dr12';
  13: result:='dr13';
  14: result:='dr14';
  15: result:='dr15';
  end;
  result:=colorreg+result+endcolor;
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
  if $2e in prefix then result:='cs:' else
  if $26 in prefix then result:='es:' else
  if $36 in prefix then result:='ss:' else
  if $3e in prefix then result:='' else
  if $64 in prefix then result:='fs:' else
  if $65 in prefix then result:='gs:';
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
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rax'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'eax'+endcolor+'],';

            1:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rcx'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ecx'+endcolor+'],';

            2:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdx'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edx'+endcolor+'],';

            3:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbx'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebx'+endcolor+'],';
            4:
            begin
              result:=sib(memory,modrmbyte+1,last);
              dwordptr:=@memory[last];

              if result='' then
              begin
                LastdisassembleData.modrmValueType:=dvtAddress;
                LastdisassembleData.modrmValue:=dwordptr^;
              end;

              if dwordptr^ <=$7FFFFFFF then
              begin
                if result<>'' then
                  result:=result+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if result<>'' then
                  result:=result+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=inttohexs($100000000-dwordptr^,8)+'],';
              end;

              result:=getsegmentoverride(prefix)+'['+result;

              inc(last,4);
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
                LastDisassembleData.modrmValueType:=dvtAddress;
                LastDisassembleData.modrmValue:=dwordptr^;
              end;
              last:=last+4;
            end;

            6:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rsi'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'esi'+endcolor+'],';

            7:  if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdi'+endcolor+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edi'+endcolor+'],';

            8:  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'],';
            9:  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'],';
           10:  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'],';
           11:  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'],';
           12:  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'],';
           13:  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'],';
           14:  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'],';
           15:  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'],';
        end;

      end;
      1:  begin
            case getrm(memory[modrmbyte]) of
              0:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rax'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'eax'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rax'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'eax'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              1:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rcx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ecx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rcx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ecx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              2:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              3:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebx'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
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
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbp'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebp'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbp'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebp'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              6:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rsi'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'esi'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rsi'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'esi'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              7:
              if memory[modrmbyte+1]<=$7F then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdi'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edi'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],'
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdi'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edi'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              end;

              8:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
              9:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             10:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             11:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             12:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             13:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             14:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
             15:  if memory[modrmbyte+1]<=$7F then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'-'+inttohexs($100-memory[modrmbyte+1],2)+'],';
            end;

            inc(last);
          end;

      2:  begin


            case getrm(memory[modrmbyte]) of
              0:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rax'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'eax'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rax'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'eax'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              1:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rcx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ecx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rcx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ecx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              2:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              3:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebx'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebx'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              4:  begin
                    result:=sib(memory,modrmbyte+1,last);
                    dwordptr:=@memory[last];

                    if result='' then
                    begin
                      LastdisassembleData.modrmValueType:=dvtAddress;
                      LastdisassembleData.modrmValue:=dwordptr^;
                    end;

                    if dwordptr^ <=$7FFFFFFF then
                    begin
                      if result<>'' then
                        result:=result+'+'+inttohexs(dwordptr^,8)+'],'
                      else
                        result:=inttohexs(dwordptr^,8)+'],';
                    end
                    else
                    begin
                      if result<>'' then
                        result:=result+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                      else
                        result:=inttohexs($100000000-dwordptr^,8)+'],';
                    end;

                    result:=getsegmentoverride(prefix)+'['+result;
                  end;
              5:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbp'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebp'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rbp'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'ebp'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              6:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rsi'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'esi'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rsi'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'esi'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              7:
              if dwordptr^ <=$7FFFFFFF then
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdi'+endcolor+'+'+inttohexs(dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edi'+endcolor+'+'+inttohexs(dwordptr^,8)+'],';
              end
              else
              begin
                if processhandler.is64Bit then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'rdi'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],'
                else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'edi'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';
              end;

              8:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

              9:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             10:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             11:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             12:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             13:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             14:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

             15:  if dwordptr^ <=$7FFFFFFF then
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'+'+inttohexs(dwordptr^,8)+'],' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'-'+inttohexs($100000000-dwordptr^,8)+'],';

            end;
            inc(last,4);

          end;

      3:  begin
            case getrm(memory[modrmbyte]) of
              0:  case inst of
                    0: if rex_w then result:='rax' else result:='eax';
                    1: result:='ax';
                    2: result:='al';
                    3: result:='mm0';
                    4: result:='xmm0';
                  end;

              1:  case inst of
                    0: if rex_w then result:='rcx' else result:='ecx';
                    1: result:='cx';
                    2: result:='cl';
                    3: result:='mm1';
                    4: result:='xmm1';
                  end;

              2:  case inst of
                    0: if rex_w then result:='rdx' else result:='edx';
                    1: result:='dx';
                    2: result:='dl';
                    3: result:='mm2';
                    4: result:='xmm2';
                  end;

              3:  case inst of
                    0: if rex_w then result:='rbx' else result:='ebx';
                    1: result:='bx';
                    2: result:='bl';
                    3: result:='mm3';
                    4: result:='xmm3';
                  end;

              4:  case inst of
                    0: if rex_w then result:='rsp' else result:='esp';
                    1: result:='sp';
                    2: if rexprefix<>0 then result:='spl' else result:='ah';
                    3: result:='mm4';
                    4: result:='xmm4';
                  end;

              5:  case inst of
                    0: if rex_w then result:='rbp' else result:='ebp';
                    1: result:='bp';
                    2: if rexprefix<>0 then result:='bpl' else result:='ch';
                    3: result:='mm5';
                    4: result:='xmm5';
                  end;

              6:  case inst of
                    0: if rex_w then result:='rsi' else result:='esi';
                    1: result:='si';
                    2: if rexprefix<>0 then result:='sil' else result:='dh';
                    3: result:='mm6';
                    4: result:='xmm6';
                  end;

              7:  case inst of
                    0: if rex_w then result:='rdi' else result:='edi';
                    1: result:='di';
                    2: if rexprefix<>0 then result:='dil' else result:='bh';
                    3: result:='mm7';
                    4: result:='xmm7';
                  end;

              8: case inst of
                    0: if rex_w then result:='r8' else result:='r8d';
                    1: result:='r8w';
                    2: result:='r8l';
                    3: result:='mm8';
                    4: result:='xmm8';
                 end;

              9: case inst of
                   0: if rex_w then result:='r9' else result:='r9d';
                   1: result:='r9w';
                   2: result:='r9l';
                   3: result:='mm9';
                   4: result:='xmm9';
                 end;

             10: case inst of
                   0: if rex_w then result:='r10' else result:='r10d';
                   1: result:='r10w';
                   2: result:='r10l';
                   3: result:='mm10';
                   4: result:='xmm10';
                 end;

             11: case inst of
                   0: if rex_w then result:='r11' else result:='r11d';
                   1: result:='r11w';
                   2: result:='r11l';
                   3: result:='mm11';
                   4: result:='xmm11';
                 end;

             12: case inst of
                   0: if rex_w then result:='r12' else result:='r12d';
                   1: result:='r12w';
                   2: result:='r12l';
                   3: result:='mm12';
                   4: result:='xmm12';
                 end;

             13: case inst of
                   0: if rex_w then result:='r13' else result:='r13d';
                   1: result:='r13w';
                   2: result:='r13l';
                   3: result:='mm13';
                   4: result:='xmm13';
                 end;

             14: case inst of
                   0: if rex_w then result:='r14' else result:='r14d';
                   1: result:='r14w';
                   2: result:='r14l';
                   3: result:='mm14';
                   4: result:='xmm14';
                 end;

             15: case inst of
                   0: if rex_w then result:='r15' else result:='r15d';
                   1: result:='r15w';
                   2: result:='r15l';
                   3: result:='mm15';
                   4: result:='xmm15';
                 end;
            end;

            result:=colorreg+result+endcolor+',';
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
  result:='';

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
    0: result:='eax';
    1: result:='ecx';
    2: result:='edx';
    3: result:='ebx';
    4: result:='esp';
    5:
    begin
        if _mod<>0 then result:='ebp';
    end;
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
  if processhandler.is64Bit then
  begin
    if result<>'' then result[1]:='r'; //quick replace


  end;

  if result<>'' then
    result:=colorreg+result+endcolor;


  case index of
    0: indexstring:='eax';
    1: indexstring:='ecx';
    2: indexstring:='edx';
    3: indexstring:='ebx';
    4: indexstring:='';
    5: indexstring:='ebp';
    6: indexstring:='esi';
    7: indexstring:='edi';
    8: indexstring:='r8';
    9: indexstring:='r9';
   10: indexstring:='r10';
   11: indexstring:='r11';
   12: indexstring:='r12';
   13: indexstring:='r13';
   14: indexstring:='r14';
   15: indexstring:='r15';
  end;

  if processhandler.is64Bit then
  begin
    if indexstring<>'' then indexstring[1]:='r'; //quick replace

  end;
  if indexstring<>'' then
    indexstring:=colorreg+indexstring+endcolor;

  if index<>4 then
  begin
    case ss of
      1: indexstring:=indexstring+'*'+colorhex+'2'+endcolor;
      2: indexstring:=indexstring+'*'+colorhex+'4'+endcolor;
      3: indexstring:=indexstring+'*'+colorhex+'8'+endcolor;
    end;
    if result='' then
      result:=indexstring
    else
      result:=result+'+'+indexstring;
  end else
  begin
    if processhandler.is64bit then
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
    startoffset: ptrUint;
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
  LastDisassembleData.PrefixSize:=0;
  LastDisassembleData.opcode:='';
  LastDisassembleData.parameters:='';
  lastdisassembledata.isjump:=false;
  lastdisassembledata.iscall:=false;
  lastdisassembledata.isret:=false;
  lastdisassembledata.isconditionaljump:=false;
  lastdisassembledata.modrmValueType:=dvtNone;
  lastdisassembledata.parameterValueType:=dvtNone;


  if isdefault then
    showsymbols:=symhandler.showsymbols;

  if showsymbols then
    intToHexs:=inttohexs_withsymbols
  else
    intToHexs:=inttohexs_withoutsymbols;

  riprelative:=false;

  if dataonly then
    result:=''
  else
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


        if not dataonly then
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
        if not dataonly then
          result:=result+inttohexs(RexPrefix,2)+' ';

        inc(offset);
        inc(startoffset);
        prefix2:=prefix2+[RexPrefix];
        MoveMemory(@memory[0], @memory[1], 23);
      end;
    end;

    prefixsize:=length(LastDisassembleData.bytes);
    LastDisassembleData.prefixsize:=prefixsize;

    case memory[0] of  //opcode
      $00 : begin
              description:='add';

              lastdisassembledata.opcode:='add';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);

              inc(offset,last-1);
            end;

      $01 : begin
              description:='add';

              lastdisassembledata.opcode:='add';
              if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);

              inc(offset,last-1);
            end;

      $02 : begin
              description:='Add';

              LastDisassembleData.opcode:='add';
              LastDisassembleData.parameters:=r8(memory[1])+','+MODRM(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $03 : begin
              description:='Add';
              LastDisassembleData.opcode:='add';
              if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[1])+','+MODRM(memory,prefix2,1,1,last) else
                                     LastDisassembleData.parameters:=r32(memory[1])+','+MODRM(memory,prefix2,1,0,last);


              inc(offset,last-1);
            end;



      $04 : begin
              description:='Add '+inttohex(memory[1],2)+' to AL';
              LastDisassembleData.opcode:='add';
              LastDisassembleData.parameterValueType:=dvtValue;
              LastDisassembleData.parameterValue:=memory[1];
              LastDisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);

              LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=1;
              inc(LastDisassembleData.SeperatorCount);

              inc(offset);
            end;

      $05 : begin
              lastdisassembledata.opcode:='add';
              lastdisassembledata.parametervaluetype:=dvtvalue;


              wordptr:=@memory[1];
              dwordptr:=@memory[1];
              if $66 in prefix2 then
              begin
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;
                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);

                description:='add '+inttohex(wordptr^,4)+' to ax';



                inc(offset,2);
              end else
              begin
                if rex_w then
                begin
                  lastdisassembledata.parametervaluetype:=dvtvalue;
                  lastdisassembledata.parametervalue:=dwordptr^;
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8);

                  description:='add '+inttohex(dwordptr^,8)+' to rax (sign extended)';
                end
                else
                begin
                  lastdisassembledata.parametervaluetype:=dvtvalue;
                  lastdisassembledata.parametervalue:=dwordptr^;
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);

                  description:='add '+inttohex(dwordptr^,8)+' to eax';
                end;
                inc(offset,4);
              end;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $06 : begin
              lastdisassembledata.opcode:='push';
              lastdisassembledata.parameters:=colorreg+'es'+endcolor;
              description:='place es on the stack';
            end;

      $07 : begin
              lastdisassembledata.opcode:='pop';
              lastdisassembledata.parameters:=colorreg+'es'+endcolor;
              description:='remove es from the stack';
            end;

      $08 : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $09 : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $0a : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);
              inc(offset,last-1);
            end;

      $0b : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                                     lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $0c : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              inc(offset);
            end;

      $0d : begin
              description:='logical inclusive or';
              lastdisassembledata.opcode:='or';
              lastdisassembledata.parametervaluetype:=dvtvalue;

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                lastdisassembledata.parametervalue:=wordptr^;
                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,4);

                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                begin
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,4);
                  description:=description+' (sign-extended)';
                end
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,4);


                lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                inc(lastdisassembledata.seperatorcount);
                inc(offset,4);
              end;
            end;

      $0e : begin
              description:='place cs on the stack';
              lastdisassembledata.opcode:='push';
              lastdisassembledata.parameters:=colorreg+'cs'+endcolor;
            end;

      $0f : begin  //simd extensions
              lastdisassembledata.prefix:=''; //these usually treat the f2/f3 prefix differently

              case memory[1] of
                $00 : begin
                        case getreg(memory[2]) of
                         0:  begin
                               lastdisassembledata.opcode:='sldt';
                               description:='store local descriptor table register';
                               if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16) else
                                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                               inc(offset,last-1);
                             end;

                         1:  begin
                               description:='store task register';
                               lastdisassembledata.opcode:='str';
                               lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         2:  begin
                               description:='load local descriptor table register';
                               lastdisassembledata.opcode:='lldt';
                               lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         3:  begin
                               description:='load task register';
                               lastdisassembledata.opcode:='ltr';
                               lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);   ;
                             end;

                         4:  begin
                               description:='verify a segment for reading';
                               lastdisassembledata.opcode:='verr';
                               lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         5:  begin
                               description:='verify a segment for writing';
                               lastdisassembledata.opcode:='verw';
                               lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

//the following 2 were made up by me.
                         else
                         begin
                           lastdisassembledata.opcode:='db';
                           LastDisassembleData.parameters:=inttohex(memory[0],2);
                           description:='not specified by the intel documentation';
                         end;

                        end;

                      end;

                $01 : begin
                        case getreg(memory[2]) of
                         0:  begin
                                case memory[2] of
                                  $c1:  begin
                                          description:='call to vm monitor by causing vm exit';
                                          lastdisassembledata.opcode:='vmcall';
                                          inc(offset,2);
                                        end;

                                  $c2:  begin
                                          description:='launch virtual machine managed by current vmcs';
                                          lastdisassembledata.opcode:='vmlaunch';
                                          inc(offset,2);
                                        end;

                                  $c3:  begin
                                          description:='resume virtual machine managed by current vmcs';
                                          lastdisassembledata.opcode:='vmresume';
                                          inc(offset,2);
                                        end;

                                  $c4:  begin
                                          description:='leaves vmx operation';
                                          lastdisassembledata.opcode:='vmxoff';
                                          inc(offset,2);
                                        end;

                                  else  begin
                                          description:='store global descriptor table register';
                                          lastdisassembledata.opcode:='sgdt';
                                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                          inc(offset,last-1);
                                        end;

                                end;


                              end;

                         1:  begin
                                description:='store interrupt descriptor table register';
                                lastdisassembledata.opcode:='sidt';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         2:  begin
                                description:='load global descriptor table register';
                                lastdisassembledata.opcode:='lgdt';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         3:  begin
                                description:='load interupt descriptor table register';
                                lastdisassembledata.opcode:='lidt';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         4:  begin
                                description:='store machine status word';
                                lastdisassembledata.opcode:='smsw';

                                if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)
                                                  else lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                         6:  begin
                                description:='load machine status word';
                                lastdisassembledata.opcode:='lmsw';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='invalidate tlb entry';
                                lastdisassembledata.opcode:='invplg';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;
                        end;
                      end;

                $02 : begin
                        description:='load access rights byte';
                        lastdisassembledata.opcode:='lar';
                        if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $03 : begin
                        description:='load segment limit';
                        lastdisassembledata.opcode:='lsl';
                        if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $05 : begin
                        description:='fast system call';
                        lastdisassembledata.opcode:='syscall';
                        inc(offset);
                      end;

                $06 : begin
                        description:='clear task-switched flag in cr0';
                        lastdisassembledata.opcode:='clts';
                        inc(offset);
                      end;

                $07 : begin
                        description:='return from fast system call';
                        lastdisassembledata.opcode:='sysret';
                        inc(offset);
                      end;

                $08 : begin
                        description:='invalidate internal caches';
                        lastdisassembledata.opcode:='incd';
                        inc(offset);
                      end;

                $09 : begin
                        description:='write back and invalidate cache';
                        lastdisassembledata.opcode:='wbinvd';
                        inc(offset);
                      end;

                $0b : begin
                        description:='undefined instruction(yes, this one really excists..)';
                        lastdisassembledata.opcode:='ud2';
                        inc(offset);
                      end;

                //$0d : begin

                  //    end;


                $10 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='move scalar double-fp';
                          lastdisassembledata.opcode:='movsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='move scalar single-fp';
                          lastdisassembledata.opcode:='movsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move unaligned packed double-fp';
                          lastdisassembledata.opcode:='movupd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move unaligned four packed single-fp';
                          lastdisassembledata.opcode:='movups';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $11 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='move scalar double-fp';
                          lastdisassembledata.opcode:='movsd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='move scalar single-fp';
                          lastdisassembledata.opcode:='movss';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move unaligned packed double-fp';
                          lastdisassembledata.opcode:='movupd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move unaligned four packed single-fp';
                          lastdisassembledata.opcode:='movups';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;

                $12 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move low packed double-precision floating-point value';
                          lastdisassembledata.opcode:='movlpd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';
                          lastdisassembledata.opcode:='movlps';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $13 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move low packed double-fp';
                          lastdisassembledata.opcode:='movlpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move low packed single-fp';
                          lastdisassembledata.opcode:='movlps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $14 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed single-fp';
                          lastdisassembledata.opcode:='unpcklpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed single-fp';
                          lastdisassembledata.opcode:='unpcklps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $15 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack and interleave high packed double-fp';
                          lastdisassembledata.opcode:='unpckhpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed single-fp';
                          lastdisassembledata.opcode:='unpckhps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $16 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move high packed double-precision floating-point value';
                          lastdisassembledata.opcode:='movhpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';
                          lastdisassembledata.opcode:='movhps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $17 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move high packed double-precision floating-point value';
                          lastdisassembledata.opcode:='movhpd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';
                          lastdisassembledata.opcode:='movhps';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $18 : begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='prefetch';
                                lastdisassembledata.opcode:='prefetchnta';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='prefetch';
                                lastdisassembledata.opcode:='prefetchto';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='prefetch';
                                lastdisassembledata.opcode:='prefetcht1';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='prefetch';
                                lastdisassembledata.opcode:='prefetcht2';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                        end;
                      end;

                $1f:  begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='multibyte nop';
                                lastdisassembledata.opcode:='nop';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;
                        end;
                      end;



                $20 : begin
                        description:='move from control register';
                        lastdisassembledata.opcode:='mov';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+cr(memory[2]);
                        inc(offset,last-1);
                      end;

                $21 : begin
                        description:='move from debug register';
                        lastdisassembledata.opcode:='mov';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+dr(memory[2]);
                        inc(offset,last-1);
                      end;

                $22 : begin
                        description:='move to control register';
                        lastdisassembledata.opcode:='mov';
                        lastdisassembledata.parameters:=cr(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $23 : begin
                        description:='move to debug register';
                        lastdisassembledata.opcode:='mov';
                        lastdisassembledata.parameters:=dr(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $28 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move aligned packed fouble-fp values';
                          lastdisassembledata.opcode:='movapd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move aligned four packed single-fp';
                          lastdisassembledata.opcode:='movaps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $29 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move aligned packed fouble-fp values';
                          lastdisassembledata.opcode:='movapd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move aligned four packed single-fp';
                          lastdisassembledata.opcode:='movaps';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;


                $2a : begin
                        if $f2 in prefix2 then
                        begin

                          description:='convert doubleword integer to scalar doubleprecision floating-point value';
                          lastdisassembledata.opcode:='cvtsi2sd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='scalar signed int32 to single-fp conversion';
                          lastdisassembledata.opcode:='cvtsi2ss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert packed dword''s to packed dp-fp''s';
                            lastdisassembledata.opcode:='cvtpi2pd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed signed int32 to packed single-fp conversion';
                            lastdisassembledata.opcode:='cvtpi2ps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2b : begin
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='movntpd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move packed double-precision floating-point using non-temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='movntps';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move aligned four packed single-fp non temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $2c : begin
                        if $f2 in prefix2 then
                        begin
                          description:='convert with truncation scalar double-precision floating point value to signed doubleword integer';
                          lastdisassembledata.opcode:='cvttsd2si';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='scalar single-fp to signed int32 conversion (truncate)';
                          lastdisassembledata.opcode:='cvttss2si';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='packed doubleprecision-fp to packed dword conversion (truncate)';
                            lastdisassembledata.opcode:='cvttpd2pi';
                            lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp to packed int32 conversion (truncate)';
                            lastdisassembledata.opcode:='cvttps2pi';
                            lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2d : begin
                        if $f2 in prefix2 then
                        begin
                          description:='convert scalar double-precision floating-point value to doubleword integer';
                          lastdisassembledata.opcode:='cvtsd2si';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='scalar single-fp to signed int32 conversion';
                          lastdisassembledata.opcode:='cvtss2si';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert 2 packed dp-fp''s from param 2 to packed signed dword in param1';
                            lastdisassembledata.opcode:='cvtpi2ps';
                            lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp to packed int32 conversion';
                            lastdisassembledata.opcode:='cvtps2pi';
                            lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2e : begin
                        if $66 in prefix2 then
                        begin
                          description:='unordered scalar double-fp compare and set eflags';
                          lastdisassembledata.opcode:='ucomisd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unordered scalar single-fp compare and set eflags';
                          lastdisassembledata.opcode:='ucomiss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;


                $2f : begin
                        if $66 in prefix2 then
                        begin
                          description:='compare scalar ordered double-precision floating point values and set eflags';
                          lastdisassembledata.opcode:='comisd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='scalar ordered single-fp compare and set eflags';
                          lastdisassembledata.opcode:='comiss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end;
                      end;

                $30 : begin
                        description:='write to model specific register';
                        lastdisassembledata.opcode:='wrmsr';
                        inc(offset);
                      end;

                $31 : begin
                        description:='read time-stamp counter';
                        lastdisassembledata.opcode:='rdtsc';
                        inc(offset);
                      end;

                $32 : begin
                        description:='read from model specific register';
                        lastdisassembledata.opcode:='rdmsr';
                        inc(offset);
                      end;

                $33: begin
                        description:='read performance-monitoring counters';
                        lastdisassembledata.opcode:='rdpmc';
                        inc(offset);
                      end;

                $34: begin
                        description:='fast transistion to system call entry point';
                        lastdisassembledata.opcode:='sysenter';
                        inc(offset);
                      end;

                $35: begin
                        description:='fast transistion from system call entry point';
                        lastdisassembledata.opcode:='sysexit';
                        inc(offset);
                      end;

                $40 : begin
                        description:='move if overflow';
                        lastdisassembledata.opcode:='cmovo';
                        if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                                               lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);
                        inc(offset,last-1);
                      end;

                $41 : begin
                        description:='move if not overflow';
                        lastdisassembledata.opcode:='cmovno';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $42 : begin
                        description:='move if below/ move if carry';
                        lastdisassembledata.opcode:='cmovb';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $43 : begin
                        description:='move if above or equal/ move if not carry';
                        lastdisassembledata.opcode:='cmovae';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $44 : begin
                        description:='move if equal/move if zero';
                        lastdisassembledata.opcode:='cmove';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $45 : begin
                        description:='move if not equal/move if not zero';
                        lastdisassembledata.opcode:='cmovne';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $46 : begin
                        description:='move if below or equal';
                        lastdisassembledata.opcode:='cmovbe';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;


                $47 : begin
                        description:='move if above';
                        lastdisassembledata.opcode:='cmova';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $48 : begin
                        description:='move if sign';
                        lastdisassembledata.opcode:='cmovs';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $49 : begin
                        description:='move if not sign';
                        lastdisassembledata.opcode:='cmovns';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4a : begin
                        description:='move if parity even';
                        lastdisassembledata.opcode:='cmovpe';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4b : begin
                        description:='move if not parity/move if parity odd';
                        lastdisassembledata.opcode:='cmovnp';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4c : begin
                        description:='move if less';
                        lastdisassembledata.opcode:='cmovl';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4d : begin
                        description:='move if greater or equal';
                        lastdisassembledata.opcode:='cmovge';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $4e : begin
                        description:='move if less or equal';
                        lastdisassembledata.opcode:='cmovle';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);


                        inc(offset,last-1);
                      end;

                $4f : begin
                        description:='move if greater';
                        lastdisassembledata.opcode:='cmovg';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $50 : begin
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='movmskpd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='extract packed double-precision floating-point sign mask';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='movmskps';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+xmm(memory[2]);
                          description:='move mask to integer';
                          inc(offset,last-1);
                        end;
                      end;

                $51 : begin
                        if $f2 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='sqrtsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          description:='scalar double-fp square root';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='sqrtss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          description:='scalar single-fp square root';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='sqrtpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          description:='packed double-fp square root';

                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='sqrtps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          description:='packed single-fp square root';

                          inc(offset,last-1);
                        end;
                      end;

                $52 : begin
                        if $f3 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='rsqrss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed single-fp square root reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='rsqrtps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp square root reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $53 : begin
                        if $f3 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='rcpss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp reciprocal';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='rcpps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed single-fp reciprocal';
                          inc(offset,last-1);
                        end;
                      end;

                $54 : begin
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='andpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='bit-wise logical and of xmm2/m128 and xmm1';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='andps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='bit-wise logical and for single fp';
                          inc(offset,last-1);
                        end;
                      end;

                $55 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical and not of packed double-precision fp values';
                          lastdisassembledata.opcode:='andnpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical and not for single-fp';
                          lastdisassembledata.opcode:='andnps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $56 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical or of double-fp';
                          lastdisassembledata.opcode:='orpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical or for single-fp';
                          lastdisassembledata.opcode:='orps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $57 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical xor for double-fp data';
                          lastdisassembledata.opcode:='xorpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical xor for single-fp data';
                          lastdisassembledata.opcode:='xorps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);


                          inc(offset,last-1);
                        end;
                      end;

                $58 : begin
                        if $f2 in prefix2 then
                        begin
                          //delete the repne from the tempresult


                          lastdisassembledata.opcode:='addsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='add the lower sp fp number from xmm2/mem to xmm1.';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          //delete the repe from the tempresult


                          lastdisassembledata.opcode:='addss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='add the lower sp fp number from xmm2/mem to xmm1.';
                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            lastdisassembledata.opcode:='addpd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='add packed double-precision floating-point values from xmm2/mem to xmm1';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            lastdisassembledata.opcode:='addps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='add packed sp fp numbers from xmm2/mem to xmm1';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $59 : begin
                        if $f2 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='mulsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar double-fp multiply';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='mulss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp multiply';
                          inc(offset,last-1);
                        end else
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='mulpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed double-fp multiply';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='mulps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed single-fp multiply';
                          inc(offset,last-1);
                        end;
                      end;

                $5a : begin
                        if $f2 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='cvtsd2ss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='convert scalar double-precision floating-point value to scalar single-precision floating-point value';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='cvtss2sd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='convert scalar single-precision floating-point value to scalar double-precision floating-point value';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            lastdisassembledata.opcode:='cvtpd2ps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='convert packed double precision fp values to packed single precision fp values';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            lastdisassembledata.opcode:='cvtps2pd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='convert packed single precision fp values to packed double precision fp values';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5b : begin
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='cvtps2dq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='convert ps-precision fpoint values to packed dword''s ';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='cvtdq2ps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='convert packed dword''s to ps-precision fpoint values';
                          inc(offset,last-1);
                        end;
                      end;

                $5c : begin
                        if $f2 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='subsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar double-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='subss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='subpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed double-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='subps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='packed single-fp subtract';
                          inc(offset,last-1);
                        end;
                      end;


                $5d : begin
                        if $f2 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='minsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp minimum';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='minss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp minimum';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            lastdisassembledata.opcode:='minpd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='packed double-fp minimum';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            lastdisassembledata.opcode:='minps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='packed single-fp minimum';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5e : begin
                        if $f2 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='divsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar double-precision-fp divide';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          lastdisassembledata.opcode:='divss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          description:='scalar single-fp divide';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            lastdisassembledata.opcode:='divpd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='packed double-precision fp divide';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            lastdisassembledata.opcode:='divps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            description:='packed single-fp divide';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5f : begin
                        if $f2 in prefix2 then
                        begin

                          description:='scalar double-fp maximum';
                          lastdisassembledata.opcode:='maxsd';
                          lastdisassembledata.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          description:='scalar single-fp maximum';
                          lastdisassembledata.opcode:='maxss';
                          lastdisassembledata.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='packed double-fp maximum';
                            lastdisassembledata.opcode:='maxpd';
                            lastdisassembledata.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp maximum';
                            lastdisassembledata.opcode:='maxps';
                            lastdisassembledata.parameters:=xmm(memory[1])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $60 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpcklbw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpcklbw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $61 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpcklwd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpcklwd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $62 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpckldq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpckldq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $63 : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with signed saturation';
                          lastdisassembledata.opcode:='packsswb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with signed saturation';
                          lastdisassembledata.opcode:='packsswb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $64 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $65 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $66 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          lastdisassembledata.opcode:='pcmpgtd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $67 : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with unsigned saturation';
                          lastdisassembledata.opcode:='packuswb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with unsigned saturation';
                          lastdisassembledata.opcode:='packuswb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $68 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhbw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhbw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $69 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhwd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhwd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6a : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhdq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhdq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $6b : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with signed saturation';
                          lastdisassembledata.opcode:='packssdw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with signed saturation';
                          lastdisassembledata.opcode:='packssdw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $6c : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          lastdisassembledata.opcode:='punpcklqdq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;

                $6d : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          lastdisassembledata.opcode:='punpckhqdq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                      end;


                $6e : begin
                        if rex_w then
                        begin
                          description:='move quadword';
                          lastdisassembledata.opcode:='movq';
                        end
                        else
                        begin
                          description:='move doubleword';
                          lastdisassembledata.opcode:='movd';
                        end;

                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,0,last)
                        else
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $6f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='move unaligned double quadword';
                          lastdisassembledata.opcode:='movdqu';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move aligned double quadword';
                          lastdisassembledata.opcode:='movdqa';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move 64 bits';
                          lastdisassembledata.opcode:='movq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end;
                      end;

                $70 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='shuffle packed low words';
                          lastdisassembledata.opcode:='pshuflw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='shuffle packed high words';
                          lastdisassembledata.opcode:='pshufhw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='packed shuffle doubleword';
                          lastdisassembledata.opcode:='pshufd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='packed shuffle word';
                          lastdisassembledata.opcode:='pshufw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end;
                      end;

                $71 : begin
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[3];
                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=3;
                        inc(lastdisassembledata.seperatorcount);


                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrlw';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrlw';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right arithmetic';
                                  lastdisassembledata.opcode:='psraw';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right arithmetic';
                                  lastdisassembledata.opcode:='psraw';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='psllw';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='psllw';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $72 : begin
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[3];
                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=3;
                        inc(lastdisassembledata.seperatorcount);

                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrld';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrld';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right arithmetic';
                                  lastdisassembledata.opcode:='psrad';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right arithmetic';
                                  lastdisassembledata.opcode:='psrad';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='pslld';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='pslld';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $73 : begin
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[3];
                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=3;
                        inc(lastdisassembledata.seperatorcount);

                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrlq';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  lastdisassembledata.opcode:='psrlq';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          3 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='shift double quadword right lopgical';
                                  lastdisassembledata.opcode:='psrldq';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='psllq';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  lastdisassembledata.opcode:='psllq';
                                  lastdisassembledata.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          7 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='shift double quadword left logical';
                                  lastdisassembledata.opcode:='pslldq';
                                  lastdisassembledata.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;



                $74 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $75 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $76 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          lastdisassembledata.opcode:='pcmpeqd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $77 : begin
                        description:='empty mmx™ state';
                        lastdisassembledata.opcode:='emms';
                        inc(offset);
                      end;

                $78 : begin
                        description:='reads a specified vmcs field (32 bits)';
                        lastdisassembledata.opcode:='vmread';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $79 : begin
                        description:='writes a specified vmcs field (32 bits)';
                        lastdisassembledata.opcode:='vmwrite';
                        lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $7e : begin
                        if $f3 in prefix2 then
                        begin

                          description:='move quadword';
                          lastdisassembledata.opcode:='movq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move 32 bits';
                          lastdisassembledata.opcode:='movd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move 32 bits';
                          lastdisassembledata.opcode:='movd';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $7f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='move unaligned double quadword';
                          lastdisassembledata.opcode:='movdqu';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move aligned double quadword';
                          lastdisassembledata.opcode:='movdqa';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move 64 bits';
                          lastdisassembledata.opcode:='movq';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $80 : begin
                        description:='jump near if overflow';
                        lastdisassembledata.opcode:='jo';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $81 : begin
                        description:='jump near if not overflow';
                        lastdisassembledata.opcode:='jno';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                      end;

                $82 : begin
                        description:='jump near if below/carry';

                        lastdisassembledata.opcode:='jb';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                      end;

                $83 : begin
                        description:='jump near if above or equal';
                        lastdisassembledata.opcode:='jae';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $84 : begin
                        description:='jump near if equal';
                        lastdisassembledata.opcode:='je';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;


                $85 : begin
                        description:='jump near if not equal';
                        lastdisassembledata.opcode:='jne';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                      end;

                $86 : begin
                        description:='jump near if below or equal';
                        lastdisassembledata.opcode:='jbe';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;


                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $87 : begin
                        description:='jump near if above';
                        lastdisassembledata.opcode:='ja';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $88 : begin
                        description:='jump near if sign';
                        lastdisassembledata.opcode:='js';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $89 : begin
                        description:='jump near if less';
                        lastdisassembledata.opcode:='jl';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8a : begin
                        description:='jump near if parity';
                        lastdisassembledata.opcode:='jp';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8b : begin
                        description:='jump near if not parity';
                        lastdisassembledata.opcode:='jnp';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8c : begin
                        description:='jump near if less';
                        lastdisassembledata.opcode:='jl';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8d : begin
                        description:='jump near if not less';
                        lastdisassembledata.opcode:='jnl';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8e : begin
                        description:='jump near if not greater';
                        lastdisassembledata.opcode:='jng';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $8f : begin
                        description:='jump near if greater';
                        lastdisassembledata.opcode:='jg';
                        lastdisassembledata.isjump:=true;
                        lastdisassembledata.isconditionaljump:=true;

                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        if processhandler.is64bit then
                          lastdisassembledata.parametervalue:=qword(offset+pint(@memory[2])^)
                        else
                          lastdisassembledata.parametervalue:=dword(offset+pint(@memory[2])^);

                        lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                        inc(lastdisassembledata.seperatorcount);


                        inc(offset,1+4);
                        lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                      end;

                $90 : begin
                        description:='set byte if overflow';
                        lastdisassembledata.opcode:='seto';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $91 : begin
                        description:='set byte if not overfloww';
                        lastdisassembledata.opcode:='setno';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $92 : begin
                        description:='set byte if below/carry';
                        lastdisassembledata.opcode:='setb';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $93 : begin
                        description:='set byte if above or equal';
                        lastdisassembledata.opcode:='setae';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $94 : begin
                        description:='set byte if equal';
                        lastdisassembledata.opcode:='sete';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $95 : begin
                        description:='set byte if not carry(not equal)';
                        lastdisassembledata.opcode:='setnc';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $96 : begin
                        description:='set byte if below or equal';
                        lastdisassembledata.opcode:='setbe';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $97 : begin
                        description:='set byte if above';
                        lastdisassembledata.opcode:='seta';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $98 : begin
                        description:='set byte if sign';
                        lastdisassembledata.opcode:='sets';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $99 : begin
                        description:='set byte if not sign';
                        lastdisassembledata.opcode:='setns';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9a : begin
                        description:='set byte if parity';
                        lastdisassembledata.opcode:='setp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9b : begin
                        description:='set byte if not parity';
                        lastdisassembledata.opcode:='setnp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9c : begin
                        description:='set byte if less';
                        lastdisassembledata.opcode:='setl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);

                        inc(offset,last-1);
                      end;

                $9d : begin
                        description:='set byte if greater or equal';
                        lastdisassembledata.opcode:='setge';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);

                      end;

                $9e : begin
                        description:='set byte if less or equal';
                        lastdisassembledata.opcode:='setle';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);

                      end;

                $9f : begin
                        description:='set byte if greater';
                        lastdisassembledata.opcode:='setg';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                        inc(offset,last-1);


                      end;

                $a0 : begin
                        description:='push word or doubleword onto the stack';
                        lastdisassembledata.opcode:='push';
                        lastdisassembledata.parameters:='fs';
                        inc(offset);
                      end;

                $a1 : begin
                        description:='pop a value from the stack';
                        lastdisassembledata.opcode:='pop';
                        lastdisassembledata.parameters:='fs';
                        inc(offset);
                      end;


                $a2 : begin
                        description:='cpu identification';
                        lastdisassembledata.opcode:='cpuid';
                        inc(offset);
                      end;

                $a3 : begin
                        description:='bit test';
                        lastdisassembledata.opcode:='bt';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);

                        inc(offset,last-1);
                      end;

                $a4 : begin
                        description:='double precision shift left';
                        lastdisassembledata.opcode:='shld';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $a5 : begin
                        description:='double precision shift left';
                        lastdisassembledata.opcode:='shld';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+colorreg+'cl'+endcolor else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);

                      end;

                $a8 : begin
                        description:='push word or doubleword onto the stack';
                        lastdisassembledata.opcode:='push';
                        lastdisassembledata.parameters:='gs';
                        inc(offset);
                      end;

                $a9 : begin
                        description:='pop a value from the stack';
                        lastdisassembledata.opcode:='pop';
                        lastdisassembledata.parameters:='gs';
                        inc(offset);
                      end;

                $aa : begin
                        description:='resume from system management mode';
                        lastdisassembledata.opcode:='rsm';
                        inc(offset);
                      end;

                $ab : begin
                        description:='bit test and set';
                        lastdisassembledata.opcode:='bts';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $ac : begin
                        description:='double precision shift right';
                        lastdisassembledata.opcode:='shrd';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohex(memory[last],2);
                        inc(last);
                        inc(offset,last-1);
                      end;

                $ad : begin
                        description:='double precision shift right';
                        lastdisassembledata.opcode:='shrd';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2])+','+colorreg+'cl'+endcolor else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2])+','+colorreg+'cl'+endcolor;
                        inc(offset,last-1);

                      end;

                $ae : begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='store fp and mmx state and streaming simd extension state';
                                lastdisassembledata.opcode:='fxsave';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='restore fp and mmx state and streaming simd extension state';
                                lastdisassembledata.opcode:='fxrstor';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='load streaming simd extension control/status';
                                lastdisassembledata.opcode:='ldmxcsr';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='store streaming simd extension control/status';
                                lastdisassembledata.opcode:='stmxcsr';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          7:  begin
                                description:='store fence';
                                lastdisassembledata.opcode:='sfence';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                        end;

                      end;

                $af : begin
                        description:='signed multiply';
                        lastdisassembledata.opcode:='imul';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b0 : begin
                        description:='compare and exchange';
                        lastdisassembledata.opcode:='cmpxchg';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $b1 : begin
                        description:='compare and exchange';
                        lastdisassembledata.opcode:='cmpxchg';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $b2 : begin
                        description:='load far pointer';
                        lastdisassembledata.opcode:='lss';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b3 : begin
                        description:='bit test and reset';
                        lastdisassembledata.opcode:='btr';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $b4 : begin
                        description:='load far pointer';
                        lastdisassembledata.opcode:='lfs';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b5 : begin
                        description:='load far pointer';
                        lastdisassembledata.opcode:='lgs';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);

                        inc(offset,last-1);
                      end;

                $b6 : begin
                        description:='Move with zero-extend';
                        lastdisassembledata.opcode:='movzx';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,2,last,8) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last,8);


                        inc(offset,last-1);
                      end;

                $b7 : begin
                        description:='Move with zero-extend';
                        lastdisassembledata.opcode:='movzx';
                        lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,1,last,16);


                        inc(offset,last-1);
                      end;


                $ba : begin
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[3];

                        case getreg(memory[2]) of
                          4:  begin
                                //bt
                                description:='bit test';
                                lastdisassembledata.opcode:='bt';
                                if $66 in prefix2 then
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter

                                inc(offset,last-1+1);
                              end;

                          5:  begin
                                //bts
                                description:='bit test and set';
                                lastdisassembledata.opcode:='bts';
                                if $66 in prefix2 then
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          6:  begin
                                //btr
                                description:='bit test and reset';
                                lastdisassembledata.opcode:='btr';
                                if $66 in prefix2 then
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                          7:  begin
                                //btc
                                description:='bit test and complement';
                                lastdisassembledata.opcode:='btc';
                                if $66 in prefix2 then
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+inttohexs(memory[3],2) else
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+inttohexs(memory[3],2);     //notice the difference in the modrm 4th parameter
                                inc(offset,last-1+1);
                              end;

                        end;

                      end;

                $bb : begin
                        description:='bit test and complement';
                        lastdisassembledata.opcode:='btc';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;


                $bc : begin
                        //bsf
                        description:='bit scan forward';
                        lastdisassembledata.opcode:='bsf';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,0,last);


                        inc(offset,last-1);
                      end;

                $bd : begin
                        //bsf
                        description:='bit scan reverse';
                        lastdisassembledata.opcode:='bsr';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,1,last) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,1,last);


                        inc(offset,last-1);
                      end;

                $be : begin
                        description:='move with sign-extension';
                        lastdisassembledata.opcode:='movsx';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=r16(memory[2])+','+modrm(memory,prefix2,2,2,last,8) else
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,2,last,8);



                        inc(offset,last-1);
                      end;

                $bf : begin
                        description:='move with sign-extension';
                        lastdisassembledata.opcode:='movsx';
                        lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,1,last,16);

                        inc(offset,last-1);
                      end;

                $c0 : begin
                        description:='exchange and add';
                        lastdisassembledata.opcode:='xadd';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $c1 : begin
                        description:='exchange and add';
                        lastdisassembledata.opcode:='xadd';
                        if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else

                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $c2 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='compare scalar dpuble-precision floating-point values';
                          lastdisassembledata.opcode:='cmpsd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);

                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='packed single-fp compare';
                          lastdisassembledata.opcode:='cmpss';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='compare packed double-precision floating-point values';
                            lastdisassembledata.opcode:='cmppd';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                            lastdisassembledata.parametervaluetype:=dvtvalue;
                            lastdisassembledata.parametervalue:=memory[last];
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                            inc(offset,last);
                          end
                          else
                          begin
                            description:='packed single-fp compare';
                            lastdisassembledata.opcode:='cmpps';
                            lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last,128);
                            lastdisassembledata.parametervaluetype:=dvtvalue;
                            lastdisassembledata.parametervalue:=memory[last];
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                            inc(offset,last);
                          end;
                        end;
                      end;

                $c3 : begin
                        description:='store doubleword using non-temporal hint';
                        lastdisassembledata.opcode:='movnti';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last);
                      end;

                $c4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='insert word';
                          lastdisassembledata.opcode:='pinsrw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,0,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='insert word';
                          lastdisassembledata.opcode:='pinsrw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,0,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end;
                      end;

                $c5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='extract word';
                          lastdisassembledata.opcode:='pextrw';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,3);
                        end
                        else
                        begin
                          description:='extract word';
                          lastdisassembledata.opcode:='pextrw';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,3);
                        end;
                      end;

                $c6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='shuffle double-fp';
                          lastdisassembledata.opcode:='shufpd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='shuffle single-fp';
                          lastdisassembledata.opcode:='shufps';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                          inc(offset,last);
                        end;
                      end;

                $c7 : begin
                        case getreg(memory[2]) of
                          1:  begin
                                description:='compare and exchange 8 bytes';
                                lastdisassembledata.opcode:='cmpxchg8b';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          6:  begin
                                if $66 in prefix2 then
                                begin
                                  description:='copy vmcs data to vmcs region in memory';
                                  lastdisassembledata.opcode:='vmclear';
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                if $f3 in prefix2 then
                                begin
                                  description:='enter vmx root operation';
                                  lastdisassembledata.opcode:='vmxon';
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='loads ther current vmcs pointer from memory';
                                  lastdisassembledata.opcode:='vmptrld';
                                  lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end;
                              end;

                          7:  begin
                                description:='stores the current vmcs pointer into memory';
                                lastdisassembledata.opcode:='vmptrst';
                                lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                inc(offset,last-1);
                              end;
                        end;

                      end;

                $c8..$cf : begin
                        //bswap
                        description:='byte swap';
                        lastdisassembledata.opcode:='bswap';
                        lastdisassembledata.parameters:=rd(memory[1]-$c8);

                        inc(offset);
                      end;

                $d1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrlw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrlw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrld';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrld';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrlq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          lastdisassembledata.opcode:='psrlq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='add packed quadwprd integers';
                          lastdisassembledata.opcode:='paddq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='add packed quadwprd integers';
                          lastdisassembledata.opcode:='paddq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;


                $d5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply low';
                          lastdisassembledata.opcode:='pmullw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply low';
                          lastdisassembledata.opcode:='pmullw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $d6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='move low quadword from xmm to mmx technology register';
                          lastdisassembledata.opcode:='movdq2q';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='move low quadword from xmm to mmx technology register';
                          lastdisassembledata.opcode:='movq2dq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move low quadword from xmm to mmx technology register';
                          lastdisassembledata.opcode:='movq';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move quadword from mmx technology to xmm register';
                          lastdisassembledata.opcode:='movq2dq';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;


                $d7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move byte mask to integer';
                          lastdisassembledata.opcode:='pmovmskb';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move byte mask to integer';
                          lastdisassembledata.opcode:='pmovmskb';
                          lastdisassembledata.parameters:=r32(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $d8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract unsigned with saturation';
                          lastdisassembledata.opcode:='psubusb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract unsigned with saturation';
                          lastdisassembledata.opcode:='psubusb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $d9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract unsigned with saturation';
                          lastdisassembledata.opcode:='psubusw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract unsigned with saturation';
                          lastdisassembledata.opcode:='psubusw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $da : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed unsigned integer byte minimum';
                          lastdisassembledata.opcode:='pminub';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed unsigned integer byte minimum';
                          lastdisassembledata.opcode:='pminub';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $db : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical and';
                          lastdisassembledata.opcode:='pand';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical and';
                          lastdisassembledata.opcode:='pand';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $dc : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add unsigned with saturation';
                          lastdisassembledata.opcode:='paddusb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add unsigned with saturation';
                          lastdisassembledata.opcode:='paddusb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $dd : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add unsigned with saturation';
                          lastdisassembledata.opcode:='paddusw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add unsigned with saturation';
                          lastdisassembledata.opcode:='paddusw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $de : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed unsigned integer byte maximum';
                          lastdisassembledata.opcode:='pmaxub';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed unsigned integer byte maximum';
                          lastdisassembledata.opcode:='pmaxub';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $df : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical and not';
                          lastdisassembledata.opcode:='pandn';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical and not';
                          lastdisassembledata.opcode:='pandn';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e0 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed average';
                          lastdisassembledata.opcode:='pavgb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed average';
                          lastdisassembledata.opcode:='pavgb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right arithmetic';
                          lastdisassembledata.opcode:='psraw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right arithmetic';
                          lastdisassembledata.opcode:='psraw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psrad';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psrad';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed average';
                          lastdisassembledata.opcode:='pavgw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed average';
                          lastdisassembledata.opcode:='pavgw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply high unsigned';
                          lastdisassembledata.opcode:='pmulhuw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply high unsigned';
                          lastdisassembledata.opcode:='pmulhuw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply high';
                          lastdisassembledata.opcode:='pmulhw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply high';
                          lastdisassembledata.opcode:='pmulhw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $e6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                          lastdisassembledata.opcode:='cvtpd2dq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                          lastdisassembledata.opcode:='cvtdq2pd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert with truncation packed double-precision floating-point values to packed doubleword integers';
                            lastdisassembledata.opcode:='cvttpd2dq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $e7 : begin
                        if $66 in prefix2 then
                        begin
                          lastdisassembledata.opcode:='movntdq';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move double quadword using non-temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          lastdisassembledata.opcode:='movntq';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          description:='move 64 bits non temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $e8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract with saturation';
                          lastdisassembledata.opcode:='psubsb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract with saturation';
                          lastdisassembledata.opcode:='psubsb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $e9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract with saturation';
                          lastdisassembledata.opcode:='psubsw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract with saturation';
                          lastdisassembledata.opcode:='psubsw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $ea : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed signed integer word minimum';
                          lastdisassembledata.opcode:='pminsw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed signed integer word minimum';
                          lastdisassembledata.opcode:='pminsw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $eb : begin
                        if $66 in prefix2 then
                        begin
                          description:='bitwise logical or';
                          lastdisassembledata.opcode:='por';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bitwise logical or';
                          lastdisassembledata.opcode:='por';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ec : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add with saturation';
                          lastdisassembledata.opcode:='paddsb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add with saturation';
                          lastdisassembledata.opcode:='paddsb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ed : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add with saturation';
                          lastdisassembledata.opcode:='paddsw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add with saturation';
                          lastdisassembledata.opcode:='paddsw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ee : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed signed integer word maximum';
                          lastdisassembledata.opcode:='pmaxsw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed signed integer word maximum';
                          lastdisassembledata.opcode:='pmaxsw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $ef : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical exclusive or';
                          lastdisassembledata.opcode:='pxor';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical exclusive or';
                          lastdisassembledata.opcode:='pxor';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $f1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psllw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psllw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $f2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='pslld';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='pslld';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $f3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psllq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          lastdisassembledata.opcode:='psllq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);
                          inc(offset,last-1);
                        end;
                      end;

                $f4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='multiply packed unsigned doubleword integers';
                          lastdisassembledata.opcode:='pmuludq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='multiply packed unsigned doubleword integers';
                          lastdisassembledata.opcode:='pmuludq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                $f5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply and add';
                          lastdisassembledata.opcode:='pmaddwd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply and add';
                          lastdisassembledata.opcode:='pmaddwd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $f6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed sum of absolute differences';
                          lastdisassembledata.opcode:='psadbw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed sum of absolute differences';
                          lastdisassembledata.opcode:='psadbw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $f7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='store selected bytes of double quadword';
                          lastdisassembledata.opcode:='maskmovdqu';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='byte mask write';
                          lastdisassembledata.opcode:='maskmovq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $f8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $f9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fa : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fb : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubq';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          lastdisassembledata.opcode:='psubq';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fc : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddb';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddb';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fd : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddw';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddw';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;

                $fe : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddd';
                          lastdisassembledata.parameters:=xmm(memory[2])+','+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          lastdisassembledata.opcode:='paddd';
                          lastdisassembledata.parameters:=mm(memory[2])+','+modrm(memory,prefix2,2,3,last);

                          inc(offset,last-1);
                        end;
                      end;


                end;



            end;

//

//

      $10 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $11 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              if $66 in prefix2 then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $12 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last,8);

              inc(offset,last-1);
            end;

      $13 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $14 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,2);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              inc(offset);
            end;

      $15 : begin
              description:='add with carry';
              lastdisassembledata.opcode:='adc';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8);
                inc(offset,4);
              end;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

            end;

      $16 : begin
              description:='place ss on the stack';
              lastdisassembledata.opcode:='push';
              lastdisassembledata.parameters:=colorreg+'ss'+endcolor;
            end;

      $17 : begin
              description:='remove ss from the stack';
              lastdisassembledata.opcode:='pop';
              lastdisassembledata.parameters:=colorreg+'ss'+endcolor;
            end;

      $18 : begin
              description:='integer subtraction with borrow';
              lastdisassembledata.opcode:='sbb';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $19 : begin
              description:='integer subtraction with borrow';
              lastdisassembledata.opcode:='sbb';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $1a : begin
              description:='integer subtraction with borrow';
              lastdisassembledata.opcode:='sbb';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last,8);

              inc(offset,last-1);
            end;

      $1b : begin
              description:='integer subtraction with borrow';
              lastdisassembledata.opcode:='sbb';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);


              inc(offset,last-1);
            end;

      $1c : begin
              description:='integer subtraction with borrow';
              lastdisassembledata.opcode:='sbb';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);


              inc(offset);
            end;

      $1d : begin
              lastdisassembledata.opcode:='sbb';
              description:='integer subtraction with borrow';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8);

                inc(offset,4);
              end;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

            end;

      $1e : begin
              description:='place ds on the stack';
              lastdisassembledata.opcode:='push';
              lastdisassembledata.parameters:=colorreg+'ds'+endcolor;
            end;

      $1f : begin
              description:='remove ds from the stack';
              lastdisassembledata.opcode:='pop';
              lastdisassembledata.parameters:=colorreg+'ds'+endcolor;
            end;

      $20 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $21 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $22 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);
              inc(offset,last-1);
            end;

      $23 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;


      $24 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              inc(offset);
            end;

      $25 : begin
              description:='logical and';
              lastdisassembledata.opcode:='and';
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;
                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8);
                inc(offset,4);
              end;
            end;

      $27 : begin
              description:='decimal adjust al after addition';
              lastdisassembledata.opcode:='daa';
            end;

      $28 : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $29 : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $2a : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $2b : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $2c : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);



              inc(offset);
            end;

      $2d : begin
              description:='subtract';
              lastdisassembledata.opcode:='sub';


              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $2f : begin
              description:='decimal adjust al after subtraction';
              lastdisassembledata.opcode:='das';
            end;

      $30 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $31 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $32 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $33 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $34 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $35 : begin
              description:='logical exclusive or';
              lastdisassembledata.opcode:='xor';


              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $37 : begin  //aaa
              lastdisassembledata.opcode:='aaa';
              description:='ascii adjust al after addition'
            end;

//---------
      $38 : begin//cmp
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $39 : begin
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $3a : begin
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $3b : begin
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

//---------

      $3c : begin
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $3d : begin
              description:='compare two operands';
              lastdisassembledata.opcode:='cmp';
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_x then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

            //prefix bytes need fixing
      $3f : begin  //aas
              lastdisassembledata.opcode:='aas';
              description:='ascii adjust al after subtraction';
            end;

      $40..$47 :
            begin
              description:='increment by 1';
              lastdisassembledata.opcode:='inc';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=rd16(memory[0]-$40) else
                lastdisassembledata.parameters:=rd(memory[0]-$40);
            end;

      $48..$4f :
            begin
              description:='decrement by 1';
              lastdisassembledata.opcode:='dec';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=rd16(memory[0]-$48) else
                lastdisassembledata.parameters:=rd(memory[0]-$48);
            end;

      $50..$57 :
            begin
              description:='push word or doubleword onto the stack';
              if processhandler.is64bit then rexprefix:=rexprefix or bit_rex_w; //so rd will pick the 64-bit version

              lastdisassembledata.opcode:='push';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=rd16(memory[0]-$50) else
                lastdisassembledata.parameters:=rd(memory[0]-$50);
            end;

      $58..$5f :
            begin
              description:='pop a value from the stack';
              if processhandler.is64bit then rexprefix:=rexprefix or bit_rex_w; //so rd will pick the 64-bit version
              lastdisassembledata.opcode:='pop';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=rd16(memory[0]-$58) else
                lastdisassembledata.parameters:=rd(memory[0]-$58);
            end;

      $60 : begin
              description:='push all general-purpose registers';
              if processhandler.is64bit then description:=description+' (invalid)';
              if $66 in prefix2 then lastdisassembledata.opcode:='pusha' else
                                     lastdisassembledata.opcode:='pushad';

              if processhandler.is64bit then
              begin
                description:=description+' (invalid)';
                lastdisassembledata.opcode:='pushad (invalid)';
              end;
            end;

      $61 : begin
              description:='pop all general-purpose registers';
              if $66 in prefix2 then lastdisassembledata.opcode:='popa' else
                                     lastdisassembledata.opcode:='popad';

              if processhandler.is64bit then
              begin
                description:=description+' (invalid)';
                lastdisassembledata.opcode:='popad (invalid)';
              end;

            end;

      $62 : begin
              //bound
              description:='check array index against bounds';
              lastdisassembledata.opcode:='bound';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);

            end;

      $63 : begin
              //arpl
              lastdisassembledata.opcode:='arpl';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]);
              inc(offset,last-1);
              description:='adjust rpl field of segment selector';
            end;

      $68 : begin
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.opcode:='push';
                lastdisassembledata.parameters:=inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                lastdisassembledata.opcode:='push';
                lastdisassembledata.parameters:=inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
              description:='push word or doubleword onto the stack (sign extended)';
            end;

      $69 : begin
              description:='signed multiply';
              if $66 in prefix2 then
              begin
                lastdisassembledata.opcode:='imul';
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last);
                wordptr:=@memory[last];

                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                inc(offset,last-1+2);
              end
              else
              begin
                lastdisassembledata.opcode:='imul';
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);
                dwordptr:=@memory[last];

                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                inc(offset,last-1+4);
              end;
            end;

      $6a : begin
              lastdisassembledata.opcode:='push';

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];

              lastdisassembledata.parameters:=inttohexs(memory[1],2,true,1);
              inc(offset);
              description:='push byte onto the stack';
            end;

      $6b : begin

              description:='signed multiply';
              lastdisassembledata.opcode:='imul';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last)+inttohexs(memory[last],2) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last)+inttohexs(memory[last],2);
              inc(offset,last-1+1);
            end;

      $6c : begin
              //m8, dx
              description:='input from port to string';
              lastdisassembledata.opcode:='insb';
            end;

      $6d : begin
              //m8, dx
              description:='input from port to string';
              if $66 in prefix2 then lastdisassembledata.opcode:='insw' else
                                     lastdisassembledata.opcode:='insd';
            end;

      $6e : begin
              //m8, dx
              description:='output string to port';
              lastdisassembledata.opcode:='outsb';
            end;

      $6f : begin
              //m8, dx
              description:='output string to port';
              if $66 in prefix2 then lastdisassembledata.opcode:='outsw' else
                                     lastdisassembledata.opcode:='outsd';
            end;


      $70 : begin
              description:='jump short if overflow';
              lastdisassembledata.opcode:='jo';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);



            end;

      $71 : begin
              description:='jump short if not overflow';
              lastdisassembledata.opcode:='jno';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $72 : begin
              description:='jump short if below/carry';
              lastdisassembledata.opcode:='jb';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $73 : begin
              description:='jump short if above or equal';
              lastdisassembledata.opcode:='jae';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $74 : begin
              description:='jump short if equal';
              lastdisassembledata.opcode:='je';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));



              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $75 : begin
              description:='jump short if not equal';
              lastdisassembledata.opcode:='jne';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $76 : begin
              description:='jump short if not above';
              lastdisassembledata.opcode:='jna';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $77 : begin
              description:='jump short if above';
              lastdisassembledata.opcode:='ja';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $78 : begin
              description:='jump short if sign';
              lastdisassembledata.opcode:='js';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $79 : begin
              description:='jump short if not sign';
              lastdisassembledata.opcode:='jns';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $7a : begin
              description:='jump short if parity';
              lastdisassembledata.opcode:='jp';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $7b : begin
              description:='jump short if not parity';
              lastdisassembledata.opcode:='jnp';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $7c : begin
              description:='jump short if not greater or equal';
              lastdisassembledata.opcode:='jnge';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $7d : begin
              description:='jump short if not less (greater or equal)';
              lastdisassembledata.opcode:='jnl';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

            end;

      $7e : begin
              description:='jump short if less or equal';
              lastdisassembledata.opcode:='jle';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
              lastdisassembledata.parameterValueType:=dvtAddress;
            end;

      $7f : begin
              description:='jump short if greater';
              lastdisassembledata.opcode:='jg';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;
              inc(offset);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+shortint(memory[1]))
              else
                lastdisassembledata.parametervalue:=dword(offset+shortint(memory[1]));

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
              lastdisassembledata.parameterValueType:=dvtAddress;
            end;

      $80,$82 : begin
              case getreg(memory[1]) of
                0:  begin
                      //add
                      lastdisassembledata.opcode:='add';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='add x to y';
                    end;

                1:  begin
                      //adc
                      lastdisassembledata.opcode:='or';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical inclusive or';
                    end;


                2:  begin
                      //adc
                      lastdisassembledata.opcode:='adc';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='add with carry';
                    end;

                3:  begin
                      //sbb
                      lastdisassembledata.opcode:='sbb';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='integer subtraction with borrow';
                    end;

                4:  begin
                      //and
                      lastdisassembledata.opcode:='and';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical and';
                    end;

                5:  begin
                      lastdisassembledata.opcode:='sub';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='subtract';
                    end;

                6:  begin
                      lastdisassembledata.opcode:='xor';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      //and
                      lastdisassembledata.opcode:='cmp';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='compare two operands';
                    end;

              end;
            end;

      $81 : begin
              case getreg(memory[1]) of
                0:  begin
                      //add
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='add';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='add';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

//                      offset:=offset+last;
                      description:='add x to y';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='or';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='or';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='logical inclusive or';
                    end;

                2:  begin
                      //adc
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='adc';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='adc';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='add with carry';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='sbb';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='sbb';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='integer subtraction with borrow';
                    end;


                4:  begin
                      //and
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='and';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='and';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='logical and';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='sub';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='sub';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='xor';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1);
                      end else
                      begin
                        lastdisassembledata.opcode:='xor';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+2);
                      end;

                      offset:=offset+last;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      //cmp
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='cmp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        lastdisassembledata.opcode:='cmp';
                        if rex_w then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=dwordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

                      description:='compare two operands';
                    end;


              end;
            end;

      $83 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='add';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='add';

                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];

                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      inc(offset,last);
                      description:='add (sign extended)';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='or';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='or';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='add (sign extended)';
                    end;


                2:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='adc';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='adc';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      inc(offset,last);
                      description:='add with carry (sign extended)';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='sbb';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='sbb';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='integer subtraction with borrow (sign extended)';
                    end;

                4:  begin
                      //and
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='and';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='and';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      offset:=offset+last;
                      description:='logical and (sign extended)';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='sub';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='sub';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='xor';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='xor';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      //cmp
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='cmp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        lastdisassembledata.opcode:='cmp';
                        if rex_w then
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=memory[last];
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='compare two operands';
                    end;


              end;
            end;

      $84 : begin
              description:='logical compare';
              lastdisassembledata.opcode:='test';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $85 : begin
              description:='logical compare';
              lastdisassembledata.opcode:='test';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $86 : begin
              description:='exchage memory with register';
              lastdisassembledata.opcode:='xchg';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $87 : begin
              description:='exchage memory with register';
              lastdisassembledata.opcode:='xchg';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $88 : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $89 : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $8a : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parameters:=r8(memory[1])+','+modrm(memory,prefix2,1,2,last);

              inc(offset,last-1);
            end;

      $8b : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $8c : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+sreg(memory[1]);
              inc(offset,last-1);
            end;

      $8d : begin
              description:='load effective address';
              lastdisassembledata.opcode:='lea';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $8e : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parameters:=sreg(memory[1])+','+modrm(memory,prefix2,1,1,last);

              inc(offset,last-1);
            end;

      $8f : begin
              case getreg(memory[1]) of
               0:  begin
                     description:='pop a value from the stack';
                     lastdisassembledata.opcode:='pop';
                     if $66 in prefix2 then
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16) else
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                     inc(offset,last-1);
                   end;

               else
               begin
                 lastdisassembledata.opcode:='db';
                 lastdisassembledata.parameters:=colorhex+'8f'+endcolor;
                 description:='undefined by the intel specification';
               end;
              end;
            end;


      $90 : begin
              description:='no operation';
              lastdisassembledata.opcode:='nop';
            end;

      $91..$97:
            begin
              description:='exchange register with register';
              lastdisassembledata.opcode:='xchg';

              if $66 in prefix2 then
                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+rd16(memory[0]-$90)
              else
              begin
                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+rd(memory[0]-$90)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+rd(memory[0]-$90);
              end;
            end;


      $98 : begin
              //cbw/cwde
              if $66 in prefix2 then
              begin
                lastdisassembledata.opcode:='cbw';
                description:='convert byte to word';
              end else
              begin
                if rex_w then
                begin
                  lastdisassembledata.opcode:='cdqe';
                  description:='convert doubleword to quadword';
                end
                else
                begin
                  lastdisassembledata.opcode:='cwde';
                  description:='convert word to doubleword';
                end;
              end;
            end;

      $99 : begin
              if $66 in prefix2 then
              begin
                description:='convert word to doubleword';
                lastdisassembledata.opcode:='cwd';
              end
              else
              begin

                if rex_w then
                begin
                  lastdisassembledata.opcode:='cqo';
                  description:='convert quadword to octword';
                end
                else
                begin
                  lastdisassembledata.opcode:='cdq';
                  description:='convert doubleword to quadword';
                end;
              end;
            end;

      $9a : begin
              description:='call procedure';
              wordptr:=@memory[5];

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=5;
              inc(lastdisassembledata.seperatorcount);



              if processhandler.is64bit then
                lastdisassembledata.opcode:='call (invalid)'
              else
                lastdisassembledata.opcode:='call';


              lastdisassembledata.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;

              inc(offset,6);
            end;

      $9b : begin
              case memory[1] of

               $d9 : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='store fpu environment';
                                 lastdisassembledata.opcode:='wait:fstenv';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;


                         7:  begin
                                 description:='store control word';
                                 lastdisassembledata.opcode:='wait:fstcw';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='wait';
                            lastdisassembledata.opcode:='wait';
                         end;

                       end;
                     end;

               $db : begin
                       case memory[2] of
                         $e2 : begin
                                 description:='clear exceptions';
                                 lastdisassembledata.opcode:='wait:fclex';
                                 inc(offset,2);
                               end;

                         $e3 : begin
                                 description:='initialize floaring-point unit';
                                 lastdisassembledata.opcode:='wait:finit';
                                 inc(offset,2);
                               end;
                         else
                         begin
                            description:='wait';
                            lastdisassembledata.opcode:='wait';
                         end;
                       end;
                     end;

               $dd : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='store fpu state';
                                 lastdisassembledata.opcode:='wait:fsave';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         7:  begin
                                 description:='store status word';
                                 lastdisassembledata.opcode:='wait:fstsw';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='wait';
                            lastdisassembledata.opcode:='wait';
                         end;
                       end;
                     end;

               $df : begin
                       case memory[2] of
                         $e0 : begin
                                 description:='store status word';
                                 lastdisassembledata.opcode:='wait:fstsw ax';
                                 inc(offset,2);
                               end;

                         else
                         begin
                            description:='wait';
                            lastdisassembledata.opcode:='wait';
                         end;
                       end;
                     end;

               else  begin
                       description:='wait';
                       lastdisassembledata.opcode:='wait';
                     end;

              end;

            end;

      $9c : begin
              description:='push eflags register onto the stack';
              if $66 in prefix2 then lastdisassembledata.opcode:='pushf' else
              begin
                if processhandler.is64bit then
                  lastdisassembledata.opcode:='pushfq'
                else
                  lastdisassembledata.opcode:='pushfd';
              end;
            end;

      $9d : begin
              description:='pop stack into eflags register';
              if $66 in prefix2 then lastdisassembledata.opcode:='popf' else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='popfq'
                else
                  lastdisassembledata.opcode:='popfd';
              end;
            end;

      $9e : begin
              description:='store ah into flags';
              lastdisassembledata.opcode:='sahf';
            end;

      $9f : begin
              description:='load status flag into ah register';
              lastdisassembledata.opcode:='lahf';
            end;

      $a0 : begin
              description:='copy memory';
              dwordptr:=@memory[1];
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              inc(offset,4);
            end;

      $a1 : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              dwordptr:=@memory[1];


              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if $66 in prefix2 then
              begin
                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              end
              else
              begin
                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+',';

                lastdisassembledata.parameters:=lastdisassembledata.parameters+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';

              end;
              inc(offset,4);
            end;

      $a2 : begin
              description:='copy memory';
              dwordptr:=@memory[1];
              lastdisassembledata.opcode:='mov';

              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              lastdisassembledata.parameters:='byte ptr '+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],al';
              inc(offset,4);
            end;

      $a3 : begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              dwordptr:=@memory[1];

              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=lastdisassembledata.parameters+'ax'
              else
              begin
                if rex_w then
                  lastdisassembledata.parameters:=lastdisassembledata.parameters+'rax'
                else
                  lastdisassembledata.parameters:=lastdisassembledata.parameters+'eax';
              end;
              inc(offset,4);
            end;

      $a4 : begin
              description:='move data from string to string';
              lastdisassembledata.opcode:='movsb';
            end;

      $a5 : begin
              description:='move data from string to string';
              if $66 in prefix2 then
                lastdisassembledata.opcode:='movsw'
              else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='movsq'
                else
                  lastdisassembledata.opcode:='movsd';
              end;
            end;

      $a6 : begin
              description:='compare string operands';
              lastdisassembledata.opcode:='cmpsb';
            end;

      $a7 : begin
              description:='compare string operands';
              if $66 in prefix2 then
                lastdisassembledata.opcode:='cmpsw'
              else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='cmpsq'
                else
                  lastdisassembledata.opcode:='cmpsd';
              end;
            end;

      $a8 : begin
              description:='logical compare';
              lastdisassembledata.opcode:='test';

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $a9 : begin
              description:='logical compare';
              lastdisassembledata.opcode:='test';

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=dwordptr^;

                if rex_w then
                  lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $aa : begin
              description:='store string';
              lastdisassembledata.opcode:='stosb';
            end;

      $ab : begin
              description:='store string';
              if $66 in prefix2 then lastdisassembledata.opcode:='stosw' else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='stosq'
                else
                  lastdisassembledata.opcode:='stosd';
              end;
            end;

      $ac : begin
              description:='load string';
              lastdisassembledata.opcode:='lodsb';
            end;

      $ad : begin
              description:='load string';
              if $66 in prefix2 then lastdisassembledata.opcode:='lodsw' else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='lodsq'
                else
                  lastdisassembledata.opcode:='lodsd';
              end;
            end;

      $ae : begin
              description:='compare al with byte at es:edi and set status flag';
              lastdisassembledata.opcode:='scasb';
            end;

      $af : begin
              description:='scan string';
              if $66 in prefix2 then lastdisassembledata.opcode:='scasw' else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='scasq'
                else
                  lastdisassembledata.opcode:='scasd';
              end;
            end;

      $b0..$b7:
            begin
              description:='copy memory';
              lastdisassembledata.opcode:='mov';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=rd8(memory[0]-$b0)+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $b8..$bf:
            begin
              description:='copy memory';

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;
                lastdisassembledata.parametervalue:=wordptr^;

                lastdisassembledata.opcode:='mov';
                lastdisassembledata.parameters:=rd16(memory[0]-$b8)+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                lastdisassembledata.parametervaluetype:=dvtvalue;


                if rex_w then
                begin
                  lastdisassembledata.opcode:='mov';
                  lastdisassembledata.parametervalue:=pqword(dwordptr)^;
                  lastdisassembledata.parameters:=rd(memory[0]-$b8)+','+inttohexs(pqword(dwordptr)^,16);
                  inc(offset,8);
                end
                else
                begin
                  lastdisassembledata.opcode:='mov';
                  lastdisassembledata.parametervalue:=dwordptr^;

                  lastdisassembledata.parameters:=rd(memory[0]-$b8)+','+inttohexs(dwordptr^,8);
                  inc(offset,4);
                end;
              end;
            end;

      $c0 : begin
              case getreg(memory[1]) of
                0:  begin
                      lastdisassembledata.opcode:='rol';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];

                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                1:  begin
                      lastdisassembledata.opcode:='ror';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                2:  begin
                      lastdisassembledata.opcode:='rcl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='rotate nine bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                3:  begin
                      lastdisassembledata.opcode:='rcr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='rotate nine bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                4:  begin
                      lastdisassembledata.opcode:='shl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='multiply by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                5:  begin
                      lastdisassembledata.opcode:='shr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='unsigned divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

{not in intel spec}
                6:  begin
                      lastdisassembledata.opcode:='rol';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;
{^^^^^^^^^^^^^^^^^^}

                7:  begin
                      lastdisassembledata.opcode:='sar';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=memory[last];
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                      description:='signed divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

              end;
            end;

      $c1 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 16 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 32 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 16 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 32 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 17 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 33 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 17 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='rotate 33 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        lastdisassembledata.parametervaluetype:=dvtvalue;
                        lastdisassembledata.parametervalue:=memory[last];
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                        description:='signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

              end;
            end;

      $c2 : begin
              description:='near return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=wordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.opcode:='ret';
              LastDisassembleData.isret:=true;
              lastdisassembledata.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);


            end;

      $c3 : begin
              description:='near return to calling procedure';
              lastdisassembledata.opcode:='ret';
              LastDisassembleData.isret:=true;
            end;

      $c4 : begin
              description:='load far pointer';
              lastdisassembledata.opcode:='les';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $c5 : begin
              description:='load far pointer';
              lastdisassembledata.opcode:='lds';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=r16(memory[1])+','+modrm(memory,prefix2,1,1,last) else
                lastdisassembledata.parameters:=r32(memory[1])+','+modrm(memory,prefix2,1,0,last);

              inc(offset,last-1);
            end;

      $c6 : begin
              case getreg(memory[1]) of
              0 : begin
                    description:='copy memory';
                    lastdisassembledata.opcode:='mov';
                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                    lastdisassembledata.parametervaluetype:=dvtvalue;
                    lastdisassembledata.parametervalue:=memory[last];

                    lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                    inc(offset,last);
                  end;

              else begin
                     description:='not defined by the intel documentation';
                     lastdisassembledata.opcode:='db';
                     lastdisassembledata.parameters:=inttohexs(memory[0],2);
                   end;
              end;
            end;

      $c7 : begin
              case getreg(memory[1]) of
              0 : begin
                    description:='copy memory';
                    if $66 in prefix2 then
                    begin
                      lastdisassembledata.opcode:='mov';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);

                      wordptr:=@memory[last];
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=wordptr^;

                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                      inc(offset,last+1);
                    end
                    else
                    begin
                      lastdisassembledata.opcode:='mov';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      dwordptr:=@memory[last];
                      lastdisassembledata.parametervaluetype:=dvtvalue;
                      lastdisassembledata.parametervalue:=dwordptr^;

                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                      inc(offset,last+3);
                    end;
                  end;

             else begin
                    description:='not defined by the intel documentation';
                    lastdisassembledata.opcode:='db';
                    lastdisassembledata.parameters:=inttohexs(memory[0],2);
                  end;

              end;
            end;

      $c8 : begin
              description:='make stack frame for procedure parameters';
              wordptr:=@memory[1];
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=wordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=3;
              inc(lastdisassembledata.seperatorcount);


              lastdisassembledata.opcode:='enter';
              lastdisassembledata.parameters:=inttohexs(wordptr^,4)+','+inttohexs(memory[3],2);
              inc(offset,3);
            end;

      $c9 : begin
              description:='high level procedure exit';
              lastdisassembledata.opcode:='leave';
            end;

      $ca : begin
              description:='far return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              lastdisassembledata.opcode:='ret';
              LastDisassembleData.isret:=true;

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=wordptr^;
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $cb : begin
              description:='far return to calling procedure';
              lastdisassembledata.opcode:='ret';
              LastDisassembleData.isret:=true;
            end;

      $cc : begin
              //should not be shown if its being debugged using int 3'
              description:='call to interrupt procedure-3:trap to debugger';
              lastdisassembledata.opcode:='int 3';
            end;

      $cd : begin
              description:='call to interrupt procedure';
              lastdisassembledata.opcode:='int';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=inttohexs(memory[1],2);
              inc(offset);
            end;

      $ce : begin
              description:='call to interrupt procedure-4:if overflow flag=1';
              lastdisassembledata.opcode:='into';
            end;

      $cf : begin
              description:='interrupt return';
              if $66 in prefix2 then lastdisassembledata.opcode:='iret' else
              begin
                if rex_w then
                  lastdisassembledata.opcode:='iretq'
                else
                  lastdisassembledata.opcode:='iretd';
              end;
            end;

      $d0 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='rotate eight bits left once';
                      lastdisassembledata.opcode:='rol';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='rotate eight bits right once';
                      lastdisassembledata.opcode:='ror';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='rotate nine bits left once';
                      lastdisassembledata.opcode:='rcl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='rotate nine bits right once';
                      lastdisassembledata.opcode:='rcr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='multiply by 2, once';
                      lastdisassembledata.opcode:='shl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='unsigned devide by 2, once';
                      lastdisassembledata.opcode:='shr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='not defined by the intel documentation';
                      lastdisassembledata.opcode:='db';
                      lastdisassembledata.parameters:=inttohexs(memory[0],2)+' '+inttohexs(memory[1],2);
                    end;

                7:  begin
                      description:='signed devide by 2, once';
                      lastdisassembledata.opcode:='sar';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

              end;
            end;

      $d1 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits left once';
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits left once';
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits right once';
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits right once';
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits left once';
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits left once';
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits right once';
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits right once';
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='multiply by 2, once';
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='multiply by 2, once';
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='unsigned divide by 2, once';
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='unsigned divide by 2, once';
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                6:  begin
                      description:='undefined by the intel documentation';
                      lastdisassembledata.opcode:='db';
                      lastdisassembledata.parameters:=inttohexs(memory[0],2);
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='signed divide by 2, once';
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='signed divide by 2, once';
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d2 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='rotate eight bits left cl times';
                      lastdisassembledata.opcode:='rol';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='rotate eight bits right cl times';
                      lastdisassembledata.opcode:='ror';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='rotate nine bits left cl times';
                      lastdisassembledata.opcode:='rcl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='rotate nine bits right cl times';
                      lastdisassembledata.opcode:='rcr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='multiply by 2, cl times';
                      lastdisassembledata.opcode:='shl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='unsigned devide by 2, cl times';
                      lastdisassembledata.opcode:='shr';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='multiply by 2, cl times';
                      lastdisassembledata.opcode:='shl';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed devide by 2, cl times';
                      lastdisassembledata.opcode:='sar';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;


              end;
            end;

      $d3 : begin
              case getreg(memory[1]) of
                0:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits left cl times';
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits left cl times';
                        lastdisassembledata.opcode:='rol';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits right cl times';
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits right cl times';
                        lastdisassembledata.opcode:='ror';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits left cl times';
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits left cl times';
                        lastdisassembledata.opcode:='rcl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits right cl times';
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits right cl times';
                        lastdisassembledata.opcode:='rcr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='multiply by 2, cl times';
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='multiply by 2, cl times';
                        lastdisassembledata.opcode:='shl';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='unsigned divide by 2, cl times';
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='unsigned divide by 2, cl times';
                        lastdisassembledata.opcode:='shr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='signed divide by 2, cl times';
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='signed divide by 2, cl times';
                        lastdisassembledata.opcode:='sar';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d4 : begin  // aam
              inc(offset);
              lastdisassembledata.opcode:='aam';
              description:='ascii adjust ax after multiply';

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if memory[1]<>$0a then
                lastdisassembledata.parameters:=inttohexs(memory[1],2);
            end;

      $d5 : begin  // aad
              inc(offset);
              lastdisassembledata.opcode:='aad';
              description:='ascii adjust ax before division';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              if memory[1]<>$0a then lastdisassembledata.parameters:=inttohexs(memory[1],2);
            end;

      $d7 : begin
              description:='table look-up translation';
              lastdisassembledata.opcode:='xlatb';
            end;

      $d8 : begin
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='add';
                      lastdisassembledata.opcode:='fadd';
                      last:=2;
                      if memory[1]>=$c0 then
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')' else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        lastdisassembledata.opcode:='fmul';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fmul';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='compare real';
                      last:=2;
                      if memory[1]>=$d0 then
                      begin
                        lastdisassembledata.opcode:='fcom';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')'
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fcom';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='compare real and pop register stack';
                      last:=2;
                      if memory[1]>=$d8 then
                      begin
                        lastdisassembledata.opcode:='fcomp';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')'
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fcomp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='substract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        lastdisassembledata.opcode:='fsub';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$e0)+')';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fsub';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='reverse substract';
                      last:=2;
                      if (memory[1]>=$e8) then
                      begin
                        lastdisassembledata.opcode:='fsubr';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$e0)+')';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fsubr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        lastdisassembledata.opcode:='fdiv';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fdiv';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='reverse divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        lastdisassembledata.opcode:='fdivr';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fdivr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;
              end;

            end;

      $d9 : begin
              case memory[1] of
              $00..$bf : begin
                           case getreg(memory[1]) of
                           0:  begin
                                 description:='load floating point value';
                                 lastdisassembledata.opcode:='fld';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           2:  begin
                                 description:='store real';
                                 lastdisassembledata.opcode:='fst';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           3:  begin
                                 description:='store real';
                                 lastdisassembledata.opcode:='fstp';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           4:  begin
                                 description:='load fpu environment';
                                 lastdisassembledata.opcode:='fldenv';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           5:  begin
                                 description:='load control word';
                                 lastdisassembledata.opcode:='fldcw';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           6:  begin
                                 description:='store fpu environment';
                                 lastdisassembledata.opcode:='fnstenv';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           7:  begin
                                 description:='store control word';
                                 lastdisassembledata.opcode:='fnstcw';
                                 lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;


                           end;
                         end;

              $c0..$c7 : begin
                           description:='push st(i) onto the fpu register stack';
                           lastdisassembledata.opcode:='fld';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $c8..$cf : begin
                           description:='exchange register contents';
                           lastdisassembledata.opcode:='fxch';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c8)+')';
                           inc(offset);
                         end;


              $d9..$df : begin
                           description:='exchange register contents';
                           lastdisassembledata.opcode:='fxch';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$d9)+')';
                           inc(offset);
                         end;


              $d0 : begin
                      description:='no operation';
                      lastdisassembledata.opcode:='fnop';
                      inc(offset);
                    end;

              $e0 : begin
                      description:='change sign';
                      lastdisassembledata.opcode:='fchs';
                      inc(offset);
                    end;

              $e1 : begin
                      description:='absolute value';
                      lastdisassembledata.opcode:='fabs';
                      inc(offset);
                    end;

              $e4 : begin
                      description:='test';
                      lastdisassembledata.opcode:='ftst';
                      inc(offset);
                    end;

              $e5 : begin
                      description:='examine';
                      lastdisassembledata.opcode:='fxam';
                      inc(offset);
                    end;



              $e8 : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fld1';
                      inc(offset);
                    end;

              $e9 : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fldl2t';
                      inc(offset);
                    end;

              $ea : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fld2e';
                      inc(offset);
                    end;

              $eb : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fldpi';
                      inc(offset);
                    end;

              $ec : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fldlg2';
                      inc(offset);
                    end;

              $ed : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fldln2';
                      inc(offset);
                    end;

              $ee : begin
                      description:='load constant';
                      lastdisassembledata.opcode:='fldz';
                      inc(offset);
                    end;


              $f0 : begin
                      description:='compute 2^x–1';
                      lastdisassembledata.opcode:='f2xm1';
                      inc(offset);
                    end;

              $f1 : begin
                      description:='compute y*log(2)x';
                      lastdisassembledata.opcode:='fyl2x';
                      inc(offset);
                    end;

              $f2 : begin
                      description:='partial tangent';
                      lastdisassembledata.opcode:='fptan';
                      inc(offset);
                    end;

              $f3 : begin
                      description:='partial arctangent';
                      lastdisassembledata.opcode:='fpatan';
                      inc(offset);
                    end;

              $f4 : begin
                      description:='extract exponent and significand';
                      lastdisassembledata.opcode:='fxtract';
                      inc(offset);
                    end;

              $f5 : begin
                      description:='partial remainder';
                      lastdisassembledata.opcode:='fprem1';
                      inc(offset);
                    end;

              $f6 : begin
                      description:='decrement stack-top pointer';
                      lastdisassembledata.opcode:='fdecstp';
                      inc(offset);
                    end;

              $f7 : begin
                      description:='increment stack-top pointer';
                      lastdisassembledata.opcode:='fincstp';
                      inc(offset);
                    end;

              $f8 : begin
                      description:='partial remainder';
                      lastdisassembledata.opcode:='fprem';
                      inc(offset);
                    end;

              $f9 : begin
                      description:='compute y*log(2)(x+1)';
                      lastdisassembledata.opcode:='fyl2xp1';
                      inc(offset);
                    end;

              $fa : begin
                      description:='square root';
                      lastdisassembledata.opcode:='fsqrt';
                      inc(offset);
                    end;

              $fb : begin
                      description:='sine and cosine';
                      lastdisassembledata.opcode:='fsincos';
                      inc(offset);
                    end;


              $fc : begin
                      description:='round to integer';
                      lastdisassembledata.opcode:='frndint';
                      inc(offset);
                    end;

              $fd : begin
                      description:='scale';
                      lastdisassembledata.opcode:='fscale';
                      inc(offset);
                    end;

              $fe : begin
                      description:='sine';
                      lastdisassembledata.opcode:='fsin';
                      inc(offset);
                    end;

              $ff : begin
                      description:='cosine';
                      lastdisassembledata.opcode:='fcos';
                      inc(offset);
                    end;
              end;
            end;

      $da : begin
              if memory[1]<$bf then
              begin
                case getreg(memory[1]) of
                  0:  begin
                        description:='add';
                        lastdisassembledata.opcode:='fiadd';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  1:  begin
                        description:='multiply';
                        lastdisassembledata.opcode:='fimul';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  2:  begin
                        description:='compare integer';
                        lastdisassembledata.opcode:='ficom';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  3:  begin
                        description:='compare integer';
                        lastdisassembledata.opcode:='ficomp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  4:  begin
                        description:='subtract';
                        lastdisassembledata.opcode:='fisub';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  5:  begin
                        description:='reverse subtract';
                        lastdisassembledata.opcode:='fisubr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;


                  6:  begin
                        description:='devide';
                        lastdisassembledata.opcode:='fidiv';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  7:  begin
                        description:='reverse devide';
                        lastdisassembledata.opcode:='fidivr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;
                end;
              end
              else
              begin
                case getreg(memory[1]) of
                  0:  begin
                        description:='floating-point: move if below';
                        lastdisassembledata.opcode:='fcmovb';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')';
                        inc(offset);
                      end;

                  1:  begin
                        description:='floating-point: move if equal';
                        lastdisassembledata.opcode:='fcmove';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                        inc(offset);
                      end;

                  2:  begin
                        description:='floating-point: move if below or equal';
                        lastdisassembledata.opcode:='fcmovbe';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')';
                        inc(offset);
                      end;

                  3:  begin
                        description:='floating-point: move if unordered';
                        lastdisassembledata.opcode:='fcmovu';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                        inc(offset);
                      end;

                  5:  begin
                        case memory[1] of
                        $e9 : begin
                                description:='unordered compare real';
                                lastdisassembledata.opcode:='fucompp';
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
                            case getreg(memory[1]) of
                              0:  begin
                                    description:='load integer';
                                    lastdisassembledata.opcode:='fild';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              2:  begin
                                    description:='store integer';
                                    lastdisassembledata.opcode:='fist';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              3:  begin
                                    description:='store integer';
                                    lastdisassembledata.opcode:='fistp';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    description:='load floating point value';
                                    lastdisassembledata.opcode:='fld';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                    inc(offset,last-1);
                                  end;

                              7:  begin
                                    description:='store real';
                                    lastdisassembledata.opcode:='fstp';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,80);

                                    inc(offset,last-1);
                                  end;

                            end;
                          end;

                $c0..$c7 : begin
                             description:='floating-point: move if not below';
                             lastdisassembledata.opcode:='fcmovnb';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')';
                             inc(offset);
                           end;

                $c8..$cf : begin
                             description:='floating-point: move if not equal';
                             lastdisassembledata.opcode:='fcmovne';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                             inc(offset);
                           end;

                $d0..$d7 : begin
                             description:='floating-point: move if not below or equal';
                             lastdisassembledata.opcode:='fcmovnbe';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')';
                             inc(offset);
                           end;

                $d8..$df : begin
                             description:='floating-point: move if not unordered';
                             lastdisassembledata.opcode:='fcmovnu';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                             inc(offset);
                           end;

                $e2 : begin
                        description:='clear exceptions';
                        lastdisassembledata.opcode:='fnclex';
                        inc(offset);
                      end;

                $e3 : begin
                        description:='initialize floating-point unit';
                        lastdisassembledata.opcode:='fninit';
                        inc(offset);
                      end;

                $e8..$ef : begin
                             description:='floating-point: compare real and set eflags';
                             lastdisassembledata.opcode:='fucomi';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
                             inc(offset);
                           end;

                $f0..$f7 : begin
                             description:='floating-point: compare real and set eflags';
                             lastdisassembledata.opcode:='fcomi';
                             lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
                             inc(offset);
                           end;
              end;


            end;

      $dc : begin
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='add';
                      last:=2;
                      if memory[1]>=$c0 then
                      begin
                        lastdisassembledata.opcode:='fadd';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)'
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fadd';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        lastdisassembledata.opcode:='fmul';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c8)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fmul';
                         lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='compare real';
                      last:=2;
                      lastdisassembledata.opcode:='fcom';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='compare real';
                      last:=2;
                      lastdisassembledata.opcode:='fcomp';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='subtract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        lastdisassembledata.opcode:='fsubr';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e0)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fsub';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='reverse subtract';
                      last:=2;
                      if memory[1]>=$e8 then
                      begin
                        lastdisassembledata.opcode:='fsub';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e8)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fsubr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;


                      inc(offset,last-1);
                    end;


                6:  begin
                      description:='divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        lastdisassembledata.opcode:='fdivr';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$f0)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fdiv';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='reverse divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        lastdisassembledata.opcode:='fdiv';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$f8)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fdivr';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;
              end;
            end;

      $dd : begin
              case memory[1] of
              $0..$bf :  begin
                           case getreg(memory[1]) of
                             0:  begin
                                   description:='load floating point value';
                                   lastdisassembledata.opcode:='fld';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             2:  begin
                                   description:='store real';
                                   lastdisassembledata.opcode:='fst';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             3:  begin
                                   description:='store real';
                                   lastdisassembledata.opcode:='fstp';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             4:  begin
                                   description:='restore fpu state';
                                   lastdisassembledata.opcode:='frstor';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             6:  begin
                                   description:='store fpu state';
                                   lastdisassembledata.opcode:='fsave';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             7:  begin
                                   description:='store status word';
                                   lastdisassembledata.opcode:='fnstsw';
                                   lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                           end;

                         end;

              $c0..$c7 : begin
                           description:='free floating-point register';
                           lastdisassembledata.opcode:='ffree';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $d0..$d7 : begin
                           description:='store real';
                           lastdisassembledata.opcode:='fst';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$d0)+')';
                           inc(offset);
                         end;

              $d8..$df : begin
                           description:='store real';
                           lastdisassembledata.opcode:='fstp';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$d8)+')';
                           inc(offset);
                         end;

              $e0..$e7 : begin
                           description:='unordered compare real';
                           lastdisassembledata.opcode:='fucom';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e0)+')';
                           inc(offset);
                         end;

              $e8..$ef : begin
                           description:='unordered compare real';
                           lastdisassembledata.opcode:='fucomp';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e0)+')';
                           inc(offset);
                         end;
                else
                begin
                  lastdisassembledata.opcode:='db';
                  lastdisassembledata.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $de : begin
              case getreg(memory[1]) of
                0:  begin
                      //faddp
                      description:='add and pop';
                      last:=2;
                      if (memory[1]=$c1) then lastdisassembledata.opcode:='faddp'
                      else
                      if memory[1]>$c0 then
                      begin
                        lastdisassembledata.opcode:='faddp';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)';
                      end
                      else
                      begin
                        description:='add';
                        lastdisassembledata.opcode:='fiadd';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      end;
                      inc(offset,last-1);
                    end;

                1: begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        lastdisassembledata.opcode:='fmulp';
                        lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fimul';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      end;

                      inc(offset,last-1);
                   end;

                2: begin
                     description:='compare integer';
                     last:=2;
                     lastdisassembledata.opcode:='ficom';
                     lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                     inc(offset,last-1);
                   end;


                3: begin
                     if memory[1]<$c0 then
                     begin
                       description:='compare integer';
                       lastdisassembledata.opcode:='ficomp';
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                       inc(offset,last-1);
                     end;

                     if memory[1]=$d9 then
                     begin
                       description:='compare real and pop register stack twice';
                       lastdisassembledata.opcode:='fcompp';
                       inc(offset);
                     end;
                   end;

                4: begin
                     description:='subtract';
                     last:=2;
                     if memory[1]>=$e0 then
                     begin
                       lastdisassembledata.opcode:='fsubrp';
                       lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)';
                     end
                     else
                     begin
                       lastdisassembledata.opcode:='fisub';
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;


                5: begin
                     description:='reverse devide';
                     last:=2;
                     if memory[1]>=$e8 then
                     begin
                       description:='subtract and pop from stack';
                       lastdisassembledata.opcode:='fsubp';
                       lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e8)+'),st(0)'
                     end
                     else
                     begin
                       lastdisassembledata.opcode:='fisubr';
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                     end;

                     inc(offset,last-1);
                   end;


                6: begin
                     description:='reverse devide';
                     last:=2;
                     if memory[1]>=$f0 then
                     begin
                       lastdisassembledata.opcode:='fdivrp';
                       lastdisassembledata.parameters:='st('+inttostr(memory[1]-$f0)+'),st(0)';
                       inc(offset,last-1);
                     end
                     else
                     begin
                       lastdisassembledata.opcode:='db';
                       lastdisassembledata.parameters:=inttohexs(memory[0],2);
                     end;
                   end;

                7: begin
                     description:='devide';
                     last:=2;
                     if memory[1]>=$f8 then
                     begin
                       lastdisassembledata.opcode:='fdivp';
                       lastdisassembledata.parameters:='st('+inttostr(memory[1]-$f8)+'),st(0)';
                     end
                     else
                     begin
                       lastdisassembledata.opcode:='fdivr';
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;

              end;
            end;

      $df : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='load integer';
                      lastdisassembledata.opcode:='fild';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='store integer';
                      lastdisassembledata.opcode:='fist';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='store integer';
                      lastdisassembledata.opcode:='fistp';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='load binary coded decimal';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        lastdisassembledata.opcode:='fnstsw';
                        lastdisassembledata.parameters:='ax';
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='fbld';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,80);

                      end;
                      inc(offset,last-1);
                    end;

               5:  begin
                     if memory[1]<$c0 then
                     begin
                       description:='load integer';
                       lastdisassembledata.opcode:='fild';
                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                       inc(offset,last-1);
                     end;

                     if memory[1]>=$e8 then
                     begin
                       description:='compare real and set eflags';
                       lastdisassembledata.opcode:='fucomip';
                       lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
                       inc(offset);
                     end;
                   end;

               6:  begin
                      if memory[1]>=$f0 then
                      begin
                        description:='compare real and set eflags';
                        lastdisassembledata.opcode:='fcomip';
                        lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
                        inc(offset)
                      end
                      else
                      begin
                        description:='store bcd integer and pop';
                        lastdisassembledata.opcode:='fbstp';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,80);

                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      description:='store integer';
                      lastdisassembledata.opcode:='fistp';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                else
                begin
                  lastdisassembledata.opcode:='db';
                  lastdisassembledata.parameters:=inttohexs(memory[0],2);
                end;
              end;

            end;

      $e0 : begin
              description:='loop according to ecx counter';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;

              lastdisassembledata.parametervaluetype:=dvtaddress;
              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+pshortint(@memory[1])^)
              else
                lastdisassembledata.parametervalue:=dword(offset+pshortint(@memory[1])^);


              if $66 in prefix2 then
                lastdisassembledata.opcode:='loopne'
              else
                lastdisassembledata.opcode:='loopnz';

              inc(offset);
              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $e1 : begin
              description:='loop according to ecx counter';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;

              if $66 in prefix2 then
              begin
                lastdisassembledata.opcode:='loope';
              end
              else
              begin
                lastdisassembledata.opcode:='loopz';
              end;
              inc(offset);

              lastdisassembledata.parametervaluetype:=dvtaddress;
              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+pshortint(@memory[1])^)
              else
                lastdisassembledata.parametervalue:=dword(offset+pshortint(@memory[1])^);

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $e2 : begin
              description:='loop according to ecx counting';
              lastdisassembledata.opcode:='loop';
              lastdisassembledata.isjump:=true;
              inc(offset);

              lastdisassembledata.parametervaluetype:=dvtaddress;
              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+pshortint(@memory[1])^)
              else
                lastdisassembledata.parametervalue:=dword(offset+pshortint(@memory[1])^);

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $e3 : begin
              description:='jump short if cx=0';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.isconditionaljump:=true;

              if $66 in prefix2 then
                lastdisassembledata.opcode:='jcxz'
              else
                lastdisassembledata.opcode:='jecxz';
              inc(offset);

              lastdisassembledata.parametervaluetype:=dvtaddress;



              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+pshortint(@memory[1])^)
              else
                lastdisassembledata.parametervalue:=dword(offset+pshortint(@memory[1])^);

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $e4 : begin
              description:='input from port';
              lastdisassembledata.opcode:='in';
              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e5 : begin
              description:='input from port';
              lastdisassembledata.opcode:='in';

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);


              if $66 in prefix2 then lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+inttohexs(memory[1],2)
                                else lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e6 : begin
              description:='output to port';
              lastdisassembledata.opcode:='out';
              lastdisassembledata.parameters:=inttohexs(memory[1],2)+','+colorreg+'al'+endcolor;
              inc(offset);

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount)
            end;

      $e7 : begin
              description:='output toport';
              lastdisassembledata.opcode:='out';
              if $66 in prefix2 then
                lastdisassembledata.parameters:=inttohexs(memory[1],2)+','+colorreg+'ax'+endcolor else
                lastdisassembledata.parameters:=inttohexs(memory[1],2)+','+colorreg+'eax'+endcolor;

              inc(offset);

              lastdisassembledata.parametervaluetype:=dvtvalue;
              lastdisassembledata.parametervalue:=memory[1];
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
            end;

      $e8 : begin
              //call
              //this time no $66 prefix because it will only run in win32
              description:='call procedure';
              lastdisassembledata.opcode:='call';
              lastdisassembledata.isjump:=true;
              lastdisassembledata.iscall:=true;

              inc(offset,4);
              lastdisassembledata.parametervaluetype:=dvtaddress;

              if processhandler.is64bit then
                  lastdisassembledata.parametervalue:=qword(offset+pinteger(@memory[1])^)
                else
                  lastdisassembledata.parametervalue:=dword(offset+pinteger(@memory[1])^);

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

            end;

      $e9 : begin
              description:='jump near';
              lastdisassembledata.isjump:=true;

              if $66 in prefix2 then
              begin
                lastdisassembledata.opcode:='jmp';

                inc(offset,2);
                lastdisassembledata.parametervaluetype:=dvtaddress;
                lastdisassembledata.parametervalue:=dword(offset+psmallint(@memory[1])^);
                lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
              end
              else
              begin
                lastdisassembledata.opcode:='jmp';
                inc(offset,4);
                lastdisassembledata.parametervaluetype:=dvtaddress;

                if processhandler.is64bit then
                  lastdisassembledata.parametervalue:=qword(offset+pinteger(@memory[1])^)
                else
                  lastdisassembledata.parametervalue:=dword(offset+pinteger(@memory[1])^);

                lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8)
              end;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

            end;

      $ea : begin
              description:='jump far';
              lastdisassembledata.isjump:=true;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);
              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=5;
              inc(lastdisassembledata.seperatorcount);


              wordptr:=@memory[5];
              lastdisassembledata.opcode:='jmp';
              lastdisassembledata.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              lastdisassembledata.parametervaluetype:=dvtaddress;
              lastdisassembledata.parametervalue:=dwordptr^;


              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $eb : begin
              description:='jump short';
              lastdisassembledata.opcode:='jmp';
              lastdisassembledata.isjump:=true;

              inc(offset);

              if processhandler.is64bit then
                lastdisassembledata.parametervalue:=qword(offset+pshortint(@memory[1])^)
              else
                lastdisassembledata.parametervalue:=dword(offset+pshortint(@memory[1])^);

              lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

              lastdisassembledata.parametervaluetype:=dvtAddress;

              lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
              inc(lastdisassembledata.seperatorcount);

            end;

      $ec : begin
              description:='input from port';
              lastdisassembledata.opcode:='in';
              lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+colorreg+'dx'+endcolor;
            end;

      $ed : begin
              description:='input from port';
              lastdisassembledata.opcode:='in';
              if $66 in prefix2 then lastdisassembledata.parameters:=colorreg+'ax'+endcolor+','+colorreg+'dx'+endcolor else
                                     lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+colorreg+'dx'+endcolor;
            end;

      $ee : begin
              description:='input from port';
              lastdisassembledata.opcode:='out';
              lastdisassembledata.parameters:=colorreg+'dx'+endcolor+','+colorreg+'al'+endcolor
            end;

      $ef : begin
              description:='input from port';
              lastdisassembledata.opcode:='out';
              if $66 in prefix2 then lastdisassembledata.parameters:=colorreg+'dx'+endcolor+','+colorreg+'ax'+endcolor  else
                                     lastdisassembledata.parameters:=colorreg+'dx'+endcolor+','+colorreg+'eax'+endcolor ;
            end;

      $f3 : begin

            end;

      $f4 : begin
              description:='halt';
              lastdisassembledata.opcode:='hlt';
            end;

      $f5 : begin
              description:='complement carry flag';
              lastdisassembledata.opcode:='cmc';
            end;

      $f6 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='logical compare';
                      lastdisassembledata.opcode:='test';
                      lastdisassembledata.parametervaluetype:=dvtaddress;
                      lastdisassembledata.parametervalue:=memory[last];

                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+inttohexs(memory[last],2);
                      inc(offset,last);
                    end;

                2:  begin
                      description:='one''s complement negation';
                      lastdisassembledata.opcode:='not';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='two''s complement negation';
                      lastdisassembledata.opcode:='neg';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='unsigned multiply';
                      lastdisassembledata.opcode:='mul';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='signed multiply';
                      lastdisassembledata.opcode:='imul';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='unsigned divide';
                      lastdisassembledata.opcode:='div';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed divide';
                      lastdisassembledata.opcode:='idiv';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;
                else
                begin
                  lastdisassembledata.opcode:='db';
                  lastdisassembledata.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $f7 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='logical compare';
                      if $66 in prefix2 then
                      begin
                        lastdisassembledata.opcode:='test';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        lastdisassembledata.parametervalue:=wordptr^;

                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(wordptr^,4);
                        inc(offset,last+1);
                      end
                      else
                      begin
                        lastdisassembledata.opcode:='test';
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        lastdisassembledata.parametervaluetype:=dvtaddress;
                        lastdisassembledata.parametervalue:=dwordptr^;
                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,4);
                        inc(offset,last+3);
                      end;
                    end;

                2:  begin
                      description:='one''s complement negation';
                      lastdisassembledata.opcode:='not';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16) else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='two''s complement negation';
                      lastdisassembledata.opcode:='neg';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='unsigned multiply';
                      lastdisassembledata.opcode:='mul';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='signed multiply';
                      lastdisassembledata.opcode:='imul';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='unsigned divide';
                      lastdisassembledata.opcode:='div';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed divide';
                      lastdisassembledata.opcode:='idiv';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                  else
                  begin
                    lastdisassembledata.opcode:='db';
                    lastdisassembledata.parameters:=inttohexs(memory[0],2);
                  end;
                end;
            end;

      $f8 : begin
              description:='clear carry flag';
              lastdisassembledata.opcode:='clc';
            end;

      $f9 : begin
              description:='set carry flag';
              lastdisassembledata.opcode:='stc';
            end;

      $fa : begin
              description:='clear interrupt flag';
              lastdisassembledata.opcode:='cli';
            end;

      $fb : begin
              description:='set interrupt flag';
              lastdisassembledata.opcode:='sti';
            end;

      $fc : begin
              description:='clear direction flag';
              lastdisassembledata.opcode:='cld';
            end;

      $fd : begin
              description:='set direction flag';
              lastdisassembledata.opcode:='std';
            end;

      $fe : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='increment by 1';
                      lastdisassembledata.opcode:='inc';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='decrement by 1';
                      lastdisassembledata.opcode:='dec';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,7);

                      inc(offset,last-1);
                    end;

                else
                begin
                  lastdisassembledata.opcode:='db';
                  lastdisassembledata.parameters:=inttohexs(memory[0],2);
                end;
              end;
            end;

      $ff : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='increment by 1';
                      lastdisassembledata.opcode:='inc';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='decrement by 1';
                      lastdisassembledata.opcode:='dec';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                2:  begin
                      //call
                      description:='call procedure';
                      lastdisassembledata.opcode:='call';
                      lastdisassembledata.isjump:=true;
                      lastdisassembledata.iscall:=true;

                      if memory[1]>=$c0 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last) else
                      begin
                        if processhandler.is64bit then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                      end;

                      inc(offset,last-1);
                    end;

                3:  begin
                      //call
                      description:='call procedure';
                      lastdisassembledata.opcode:='call';
                      lastdisassembledata.isjump:=true;
                      lastdisassembledata.iscall:=true;

                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                4:  begin
                      //jmp
                      description:='jump near';
                      lastdisassembledata.opcode:='jmp';
                      lastdisassembledata.isjump:=true;

                      if memory[1]>=$c0 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last) else
                      begin
                        if processhandler.is64bit then
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                      end;

                      inc(offset,last-1);
                    end;

                5:  begin
                      //jmp
                      description:='jump far';
                      lastdisassembledata.opcode:='jmp far';
                      lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                      lastdisassembledata.isjump:=true;

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='push word or doubleword onto the stack';
                      lastdisassembledata.opcode:='push';
                      if $66 in prefix2 then
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)
                      else
                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;
                else
                begin
                  lastdisassembledata.opcode:='db';
                  lastdisassembledata.parameters:=inttohexs(memory[0],2);
                end;

              end;

            end;

      else  begin
              lastdisassembledata.opcode:='db';
              lastdisassembledata.parameters:=inttohex(memory[0],2);
            end;
    end;

   // if dataonly then exit;     //no need to handle the other stuff, dataonly means I'm only interested in the addresses, not bytes or extra parameters

    //strip off the , if it has one
    if (LastDisassembleData.parameters<>'') and (LastDisassembleData.parameters[length(LastDisassembleData.parameters)]=',') then
      LastDisassembleData.parameters:=copy(LastDisassembleData.parameters,0,length(LastDisassembleData.parameters)-1);




   // tempresult:=tempresult+LastDisassembleData.opcode+' '+LastDisassembleData.parameters;



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
      for i:=0 to LastDisassembleData.SeperatorCount-1 do
        inc(LastDisassembleData.Seperators[i],prefixsize);

    //todo: Next time the disassembler is getting an averhaul, do something about the prefix counting and the unnecesary readprocessmemorys associated with it


  //  result:=result+'- '+tempresult;


    if riprelative then
    begin
      //add the current offset to the code between []
      LastDisassembleData.modrmValue:=offset+integer(LastDisassembleData.modrmValue); //sign extended increase

      i:=pos('[',LastDisassembleData.parameters);
      j:=PosEx(']',LastDisassembleData.parameters,i);
     // tempresult:=copy(LastDisassembleData.parameters,i+1,j-i-1);


      tempaddress:=LastDisassembleData.modrmValue;

      tempresult:=copy(LastDisassembleData.parameters,1,i);
      tempresult:=tempresult+inttohexs(tempaddress,8);
      LastDisassembleData.parameters:=tempresult+copy(LastDisassembleData.parameters,j,length(LastDisassembleData.parameters));
    end;





  end
  else
  begin
    LastDisassembleData.opcode:='??';
    inc(offset);


  end;

  if not dataonly then
  begin
    result:=inttohex(LastDisassembleData.address,8)+' - '+getLastBytestring;
    result:=result+' - ';
    result:=result+LastDisassembleData.prefix+LastDisassembleData.opcode;
    result:=result+' ';
    result:=result+LastDisassembleData.parameters;
  end;
end;

function TDisassembler.getLastBytestring: string;
var i,j: integer;
begin
  result:='';
  for i:=0 to length(LastDisassembleData.Bytes)-1 do
  begin
    result:=result+inttohex(LastDisassembleData.Bytes[i],2);

    if i<LastDisassembleData.prefixsize then
      result:=result+' '
    else
      for j:=0 to LastDisassembleData.SeperatorCount-1 do
        if (LastDisassembleData.Seperators[j]=i+1) then  //followed by a seperator
          result:=result+' ';
  end;
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

function tdisassembler.DecodeLastParametersToString: string;
{
use the last disassembled data to show some detailed data
}
var
  jumpAddress: ptrUint;
  buffer: array [0..63] of byte;
  x: dword;

  value: ptrUint;
  vtype: TVariableType;
  a: boolean;
begin
  result:='';

  if LastDisassembleData.isjump then
  begin

    if LastDisassembleData.modrmValueType=dvtAddress then
    begin
      jumpAddress:=LastDisassembleData.modrmValue;

      if not ReadProcessMemory(processhandle, pointer(jumpAddress), @jumpAddress, processhandler.pointersize,x) then exit;
    end
    else
    begin

      jumpAddress:=LastDisassembleData.parameterValue;
    end;

    //check if the bytes at jumpAddress is ff 25 (jmp [xxxxxxxx])
    if ReadProcessMemory(processhandle, pointer(jumpAddress), @buffer[0], 6,x) then
    begin

      if (buffer[0]=$ff) and (buffer[1]=$25) then
      begin
        result:=result+'->';  //double, so ->->


        if processhandler.is64Bit then
          jumpAddress:=jumpaddress+6+pinteger(@buffer[2])^ //jumpaddress+6 because of relative addressing
        else
          jumpAddress:=pdword(@buffer[2])^;

        //jumpaddress now contains the address of the address to jump to
        //so, get the address it actually jumps to
        if not ReadProcessMemory(processhandle, pointer(jumpAddress), @jumpAddress, processhandler.pointersize,x) then exit;

        result:='->'+symhandler.getNameFromAddress(jumpAddress);
      end;

    end;


  end
  else
  begin
    if (LastDisassembleData.modrmValueType<>dvtNone) or (LastDisassembleData.parameterValueType<>dvtNone) then
    begin
      a:=false;
      if LastDisassembleData.modrmValueType=dvtAddress then
        value:=LastDisassembleData.modrmValue
      else
      begin
        value:=LastDisassembleData.parameterValue;
      end;

      if isAddress(value) then
      begin
        a:=true;
        x:=0;
        readprocessmemory(processhandle, pointer(value), @buffer[0], 63,x);
        if x>0 then
          vtype:=FindTypeOfData(value, @buffer[0], x);
      end
      else
      begin
        x:=sizeof(value);
        pptruint(@buffer[0])^:=value; //assign it so I don't have to make two compare routines
        vtype:=FindTypeOfData(value, @buffer[0], x);
      end;



      case vtype of
        vtByte: result:=inttostr(buffer[0]);
        vtWord: result:=inttostr(pshortint(@buffer[0])^);
        vtDword: if a then result:=inttohex(pdword(@buffer[0])^,8) else result:=inttostr(pinteger(@buffer[0])^);
        vtQword: result:=inttostr(pInt64(@buffer[0])^);
        vtSingle: result:=format('%.2f',[psingle(@buffer[0])^]);
        vtDouble: result:=format('%.2f',[pdouble(@buffer[0])^]);
        vtString:
        begin
          buffer[x]:=0;
          result:=pchar(@buffer[0]);
        end;


        vtUnicodeString:
        begin
          buffer[x]:=0;
          if x>0 then
            buffer[x-1]:=0;

          result:=pwidechar(@buffer[0]);
        end;
      end;
     // result:=VariableTypeToString(vtype);
      if a and (result<>'') then
        result:='['+result+']';
    end;



  end;
end;


procedure tdisassembler.splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);
//obsolete
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
  result:=colorhex+sysutils.IntToHex(address,chars)+endcolor;
end;

function TDisAssembler.inttohexs_withsymbols(address:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
var found: boolean;
begin
  if showsymbols and (chars>=8) then
  begin
    result:=symhandler.getNameFromAddress(address,found,chars);
    if syntaxhighlighting then
    begin
      if not found then
        result:=colorhex+result+endcolor
      else
        result:=colorsymbol+result+endcolor;
    end;

  end
  else
  begin
    result:=colorhex+sysutils.IntToHex(address,chars)+endcolor;
  end;


end;


procedure TDisAssembler.setSyntaxHighlighting(state: boolean);
begin
  fsyntaxhighlighting:=state;
  if state then
  begin
    endcolor:='{N}';
    colorhex:='{H}';
    colorreg:='{R}';
    colorsymbol:='{S}';
  end
  else
  begin
    //no color codes
    endcolor:='';
    colorhex:='';
    colorreg:='';
    colorsymbol:='';
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



