unit disassembler;

{$MODE Delphi}

interface

{$ifdef jni}
uses unixporthelper, sysutils, byteinterpreter, symbolhandler, NewKernelHandler,
 ProcessHandlerUnit, LastDisassembleData, DisassemblerArm, commonTypeDefs;
{$endif}

{$ifdef windows}
uses windows, imagehlp,sysutils,LCLIntf,byteinterpreter, symbolhandler, symbolhandlerstructs,
  CEFuncProc, NewKernelHandler, ProcessHandlerUnit, LastDisassembleData, disassemblerarm,
  commonTypeDefs, maps, math,vextypedef;
{$endif}

//translation: There is no fucking way I change the descriptions to resource strings
//if you're bored, go do this

type Tprefix = set of byte;
type TMemory = array [0..64] of byte;
type TIntToHexS=function(address:ptrUInt;chars: integer; signed: boolean=false; signedsize: integer=0):string of object;

type TMRPos=(mLeft,mRight, mNone);



const BIT_REX_W=8; //1000
const BIT_REX_R=4;
const BIT_REX_X=2;
const BIT_REX_B=1;

  {$ifdef UNIX}
const
  EFLAGS_CF=(1 shl 0);
  EFLAGS_PF=(1 shl 2);
  EFLAGS_AF=(1 shl 4);
  EFLAGS_ZF=(1 shl 6);
  EFLAGS_SF=(1 shl 7);
  EFLAGS_TF=(1 shl 8);
  EFLAGS_IF=(1 shl 9);
  EFLAGS_DF=(1 shl 10);
  EFLAGS_OF=(1 shl 11);
  EFLAGS_NT=(1 shl 14);
  EFLAGS_RF=(1 shl 16);
  EFLAGS_VM=(1 shl 17);
  EFLAGS_AC=(1 shl 18);
  EFLAGS_ID=(1 shl 21);
  {$endif}

type

  TDisassembleEvent=function(sender: TObject; address: ptruint; var ldd: TLastDisassembleData; var output: string; var description: string): boolean of object;
  TRegisterType=(rt64, rt32, rt16, rt8, rtYMM, rtXMM,rtMM, rtSegment, rtControlRegister, rtDebugRegister);

  TDisassembler=class
  private
    opcodeflags: record
      pp: integer;
      L: boolean;
      vvvv: integer;
      W: boolean;
      mmmmm: integer;
      B: boolean;
      X: boolean;
      R: boolean;
      skipExtraRegOnMemoryAccess: boolean;
      skipExtraReg: boolean;
    end;
    inttohexs: TIntToHexS;
    RexPrefix: byte;
    riprelative: boolean;
    hasvex: boolean;

    colorhex: string;
    colorreg: string;
    colorsymbol: string;
    endcolor: string;

    _memory: TMemory;
    prefix: TPrefix;
    prefix2: TPrefix;

    fsyntaxhighlighting: boolean;
    fOnDisassembleOverride: TDisassembleEvent;
    fOnPostDisassemble: TDisassembleEvent;


    modrmposition: TMRPos;

    ArmDisassembler: TArmDisassembler;

    function SIB(memory:PByteArray; sibbyte: integer; var last: dword; addresssize: integer=0): string;
    function MODRM(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword; position: TMRPos=mLeft): string; overload;
    function MODRM(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer; addresssize: integer=0; position:TMRPos=mLeft): string; overload;

    function MODRM2(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer=0;addresssize: integer=0;position: TMRPos=mLeft): string;

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

    function regnrtostr(listtype: TRegisterType; nr: integer): string;

    function rd(bt:byte):string;
    function rd8(bt:byte): string;
    function rd16(bt:byte): string;
    function r8(bt:byte): string;
    function r16(bt:byte): string;
    function r32(bt:byte): string;
    function r64(bt:byte): string;  //for a few specific ones
    function xmm(bt:byte): string;
    function mm(bt:byte): string;
    function sreg(bt:byte): string;
    function cr(bt:byte): string;
    function dr(bt:byte): string;
    function GetBitOf(Bt: qword; bit: integer): byte;
    function getsegmentoverride(prefix: TPrefix): string;


    function inttohexs_withoutsymbols(value:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
    function inttohexs_withsymbols(value:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;

    procedure setSyntaxHighlighting(state: boolean);

  protected
    function readMemory(address: ptruint; destination: pointer; size: integer): integer; virtual;
  public
    isdefault: boolean;
    showsymbols: boolean;
    showmodules: boolean;
    dataOnly: boolean;

    is64bit: boolean;
    is64bitOverride: boolean;
    is64BitOverrideState: boolean;

    LastDisassembleData: TLastDisassembleData;
    MarkIPRelativeInstructions: boolean;


    context: PCONTEXT;

//    showvalues: boolean;
    function disassemble(var offset: ptrUint; var description: string): string;
    procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);

    function DecodeLastParametersToString: string; //returns the special line splitDisassemblerstring used to return

    function getLastBytestring: string;


  published
    property syntaxhighlighting: boolean read fsyntaxhighlighting write setSyntaxHighlighting;
    property OnDisassembleOverride: TDisassembleEvent read fOnDisassembleOverride write fOnDisassembleOverride;
    property OnPostDisassemble: TDisassembleEvent read fOnPostDisassemble write fOnPostDisassemble;
  end;


  TCR3Disassembler=class(TDisassembler)
  private
    fcr3: QWORD;
    procedure setCR3(c: QWORD);
  protected
    function readMemory(address: ptruint; destination: pointer; size: integer): integer; override;
  published
    property CR3: QWORD read fCR3 write setCR3;

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
function previousopcode(address: ptrUint; d: Tdisassembler=nil):ptrUint;
function has4ByteHexString(d: string; var hexstring: string): boolean;
function hasAddress(d: string; var address: ptrUint; context: PContext=nil):boolean;

//function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean):string;
//function translatestring(disassembled: string; numberofbytes: integer; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string):string;
procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);

function registerGlobalDisassembleOverride(m: TDisassembleEvent): integer;
procedure unregisterGlobalDisassembleOverride(id: integer);

var visibleDisassembler: TDisassembler; //this disassembler is used to render the disassembly output. Each update it gets synced with the default
    defaultDisassembler: TDisassembler;
    GlobalDisassembleOverrides: array of TDisassembleEvent;

implementation

{$ifdef jni}
uses Assemblerunit, StrUtils, Parsers, memoryQuery;
{$endif}

{$ifdef windows}
uses Assemblerunit,CEDebugger, debughelper, StrUtils, debuggertypedefinitions,
  Parsers, memoryQuery, binutils, luacaller, vmxfunctions, frmcodefilterunit;
{$endif}


function registerGlobalDisassembleOverride(m: TDisassembleEvent): integer;
var i: integer;
begin
  for i:=0 to length(GlobalDisassembleOverrides)-1 do
  begin
    if assigned(GlobalDisassembleOverrides[i])=false then
    begin
      GlobalDisassembleOverrides[i]:=m;
      result:=i;
      exit;
    end
  end;

  result:=length(GlobalDisassembleOverrides);
  setlength(GlobalDisassembleOverrides, result+1);
  GlobalDisassembleOverrides[result]:=m;
end;

procedure unregisterGlobalDisassembleOverride(id: integer);
begin
  if id<length(GlobalDisassembleOverrides) then
  begin
    {$ifndef unix}
    CleanupLuaCall(TMethod(GlobalDisassembleOverrides[id]));
    {$endif}
    GlobalDisassembleOverrides[id]:=nil;
  end;
end;


function TDisassembler.regnrtostr(listtype: TRegisterType; nr: integer): string;
begin
  result:='Error';
  case listtype of
    rt8:
    begin
      case nr of
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
    end;

    rt16:
    begin
      case nr of
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
    end;

    rt32:
    begin
      case nr of
        0: result:='eax';
        1: result:='ecx';
        2: result:='edx';
        3: result:='ebx';
        4: result:='esp';
        5: result:='ebp';
        6: result:='esi';
        7: result:='edi';
        8: result:='r8d';
        9: result:='r9d';
        10: result:='r10d';
        11: result:='r11d';
        12: result:='r12d';
        13: result:='r13d';
        14: result:='r14d';
        15: result:='r15d';
      end;
    end;

    rt64:
    begin
      case nr of
        0: result:='rax';
        1: result:='rcx';
        2: result:='rdx';
        3: result:='rbx';
        4: result:='rsp';
        5: result:='rbp';
        6: result:='rsi';
        7: result:='rdi';
        8: result:='r8';
        9: result:='r9';
        10: result:='r10';
        11: result:='r11';
        12: result:='r12';
        13: result:='r13';
        14: result:='r14';
        15: result:='r15';
      end;
    end;

    rtDebugRegister:
    begin
      case nr of
        0: result:='dr0';
        1: result:='dr1';
        2: result:='dr2';
        3: result:='dr3';
        4: result:='dr4';
        5: result:='dr5';
        6: result:='dr6';
        7: result:='dr7';
        8: result:='dr8';//Do not exist, but let's implement the encoding
        9: result:='dr9';
        10: result:='dr10';
        11: result:='dr11';
        12: result:='dr12';
        13: result:='dr13';
        14: result:='dr14';
        15: result:='dr15';
      end;
    end;

    rtControlRegister:
    begin
      case nr of
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
    end;
    rtSegment:
    begin
      case nr of
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
    end;

    rtMM:
    begin
      case nr of
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
    end;

    rtXMM:
    begin
      case nr of
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
    end;

    rtYMM:
    begin
      case nr of
        0: result:='ymm0';
        1: result:='ymm1';
        2: result:='ymm2';
        3: result:='ymm3';
        4: result:='ymm4';
        5: result:='ymm5';
        6: result:='ymm6';
        7: result:='ymm7';
        8: result:='ymm8';
        9: result:='ymm9';
        10: result:='ymm10';
        11: result:='ymm11';
        12: result:='ymm12';
        13: result:='ymm13';
        14: result:='ymm14';
        15: result:='ymm15';
      end;
    end;
  end;
end;



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
   else result:='';
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
  if rex_B then bt:=bt or 8;
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
    else result:='';
  end;

  result:=colorreg+result+endcolor;
end;


function TDisassembler.rd16(bt:byte):string;
begin
  if rex_B then bt:=bt or 8;
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
    else result:='';

  end;
  result:=colorreg+result+endcolor;
end;


function TDisassembler.r8(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rt8, regnr)+endcolor;
end;


function TDisassembler.r16(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rt16, regnr)+endcolor;
end;


function TDisassembler.r32(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  if rex_w then
    result:=colorreg+regnrtostr(rt64, regnr)+endcolor
  else
    result:=colorreg+regnrtostr(rt32, regnr)+endcolor;
end;

function TDisassembler.r64(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rt64, regnr)+endcolor;
end;



function TDisassembler.xmm(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  if opcodeflags.L then
    result:=colorreg+regnrtostr(rtYMM, regnr)+endcolor
  else
    result:=colorreg+regnrtostr(rtXMM, regnr)+endcolor;
end;

function TDisassembler.mm(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rtMM, regnr)+endcolor;
end;

function TDisassembler.sreg(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rtSegment, regnr)+endcolor;
end;

function TDisassembler.CR(bt:byte):string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rtControlRegister, regnr)+endcolor;
end;

function TDisassembler.DR(bt:byte):string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rtDebugRegister, regnr)+endcolor;
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
  result:='';
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

  //if this instruction does NOT have a SIB byte, only then apply the rex_B bit
  //It has an SIB byte if RM==4 and mod!=3
  if rex_b and (not ((result=4) and (getmod(bt)<>3))) then
    result:=result or 8;
end;

function TDisassembler.getREG(bt: byte): byte;
begin
  result:=(bt shr 3) and 7;
  if Rex_r then
    result:=result or 8; //extend the reg field
end;



function TDisassembler.Rex_B: boolean; inline;
begin
  exit(opcodeflags.B);
end;

function TDisassembler.Rex_X: boolean; inline;
begin
  exit(opcodeflags.X);
end;

function TDisassembler.Rex_R: boolean; inline;
begin
  exit(opcodeflags.R);
end;

function TDisassembler.Rex_W: boolean; inline;
begin
  exit(opcodeflags.W);
end;


function TDisassembler.MODRM2(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer=0; addresssize:integer=0; position:TMRPos=mLeft): string;
var dwordptr: ^dword;
    regprefix: char;
    i: integer;

    ep: string='';

    prestr: string='';
    poststr: string='';

    operandstring: string='';

    showextrareg: boolean;
begin
  modrmposition:=position;
  result:='';
  showextrareg:=hasvex and (opcodeflags.skipExtraReg=false);

  if is64bit then
    regprefix:='r'
  else
    regprefix:='e';

  case position of
    mLeft,mNone:
    begin
      prestr:='';
      poststr:=',';
    end;

    mRight:
    begin
      prestr:=',';
      poststr:='';
    end;
  end;

  case opperandsize of
    8 : operandstring:='byte ptr ';
    16: operandstring:='word ptr ';
    32: operandstring:='dword ptr ';
    64: operandstring:='qword ptr ';
    80: operandstring:='tword ptr ';
    128: operandstring:='dqword ptr ';
    256: operandstring:='YMMword ptr ';
    else
      operandstring:='';
  end;



  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=modrmbyte;
  inc(LastDisassembleData.SeperatorCount);

  dwordptr:=@memory[modrmbyte+1];
  last:=modrmbyte+1;

  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
  inc(LastDisassembleData.SeperatorCount);


  if (not processhandler.is64Bit) and ($67 in prefix) then
  begin
    // put some 16-bit stuff in here
    // but since this is a 32-bit debugger only ,forget it...

  end
  else
  begin
    case getmod(memory[modrmbyte]) of
      0:
      begin
        if showextrareg and opcodeflags.skipExtraRegOnMemoryAccess then showextrareg:=false;

        case getrm(memory[modrmbyte]) of
            0:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'ax'+endcolor+']';
            1:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'cx'+endcolor+']';
            2:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'dx'+endcolor+']';
            3:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bx'+endcolor+']';
            4:
            begin
              //has an sib
              result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last, addresssize)+']';
            end;

            5:
            begin
              //followed by a disp32
              if is64bit then
              begin
                riprelative:=true;
                result:=getsegmentoverride(prefix)+'['+inttohexs_withoutsymbols(dwordptr^,8)+']';


                LastDisassembleData.modrmValueType:=dvtAddress;
                LastDisassembleData.modrmValue:=dwordptr^;

                LastDisassembleData.riprelative:=modrmbyte+1;

              end
              else
              begin
                result:=getsegmentoverride(prefix)+'['+inttohexs(dwordptr^,8)+']';
                LastDisassembleData.modrmValueType:=dvtAddress;
                LastDisassembleData.modrmValue:=dwordptr^;
              end;
              last:=last+4;
            end;

            6:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'si'+endcolor+']';
            7:  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'di'+endcolor+']';
            8:  result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+']';
            9:  result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+']';
           10:  result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+']';
           11:  result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+']';
           12:  result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+']';
           13:  result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+']';
           14:  result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+']';
           15:  result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+']';
        end;

        LastDisassembleData.datasize:=opperandsize div 8;
        result:=operandstring+result;

      end;
      1:  begin
            if showextrareg and opcodeflags.skipExtraRegOnMemoryAccess then showextrareg:=false;

            if getrm(memory[modrmbyte])<>4 then
            begin
              LastDisassembleData.modrmValueType:=dvtValue;
              LastDisassembleData.modrmValue:=ptruint(shortint(memory[modrmbyte+1]));
            end;

            case getrm(memory[modrmbyte]) of
              0:
              begin
                if shortint(memory[modrmbyte+1])>=0 then
                  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'ax'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                  result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'ax'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2)+']';
              end;

              1:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'cx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'cx'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2)+']';


              2:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'dx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'dx'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2)+']';


              3:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bx'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bx'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2)+']';


              4:
              begin
                result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last, addressSize)+']';
                dec(last);

                {
                if shortint(memory[last])>=0 then
                  result:=result+'+'+inttohexs(memory[last],2)+'],'
                else
                  result:=result+'-'+inttohexs(-shortint(memory[last]),2)+'],';
                                               }

              end;

              5:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bp'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bp'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';


              6:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'si'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'si'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';


              7:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'di'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'di'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';


              8:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';


              9:
              if shortint(memory[modrmbyte+1])>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

             10:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';


             11:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

             12:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

             13:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

             14:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

             15:
             if shortint(memory[modrmbyte+1])>=0 then
               result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'+'+inttohexs(memory[modrmbyte+1],2)+']' else
               result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+inttohexs(ptruint(shortint(memory[modrmbyte+1])),2, true, 2)+']';

            end;

            inc(last);
            LastDisassembleData.datasize:=opperandsize div 8;
            result:=operandstring+result;
          end;

      2:  begin
            if showextrareg and opcodeflags.skipExtraRegOnMemoryAccess then showextrareg:=false;

            if getrm(memory[modrmbyte])<>4 then
            begin
              LastDisassembleData.modrmValueType:=dvtValue;
              LastDisassembleData.modrmValue:=ptruint(pinteger(dwordptr)^);
            end;

            case getrm(memory[modrmbyte]) of
              0:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'ax'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'ax'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

              1:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'cx'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'cx'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

              2:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'dx'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'dx'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';


              3:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bx'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bx'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';


              4:
              begin
                result:=getsegmentoverride(prefix)+'['+sib(memory,modrmbyte+1,last, addresssize)+']';
                dec(last,4);
              end;

              5:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bp'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'bp'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';


              6:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'si'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'si'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';


              7:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'di'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+regprefix+'di'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';


              8:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r8'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

              9:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r9'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             10:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r10'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             11:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r11'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             12:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r12'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             13:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r13'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             14:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r14'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

             15:
              if integer(dwordptr^)>=0 then
                result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'+'+inttohexs(dwordptr^,8)+']' else
                result:=getsegmentoverride(prefix)+'['+colorreg+'r15'+endcolor+'-'+inttohexs(-integer(dwordptr^),8)+']';

            end;
            inc(last,4);

            LastDisassembleData.datasize:=opperandsize div 8;
            result:=operandstring+result;
          end;

      3:  begin
            LastDisassembleData.datasize:=0;
            case getrm(memory[modrmbyte]) of
              0:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rax' else result:='eax';
                    1: result:='ax';
                    2: result:='al';
                    3: result:='mm0';
                    4: result:='xmm0';
                  end;

              1:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rcx' else result:='ecx';
                    1: result:='cx';
                    2: result:='cl';
                    3: result:='mm1';
                    4: result:='xmm1';
                  end;

              2:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rdx' else result:='edx';
                    1: result:='dx';
                    2: result:='dl';
                    3: result:='mm2';
                    4: result:='xmm2';
                  end;

              3:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rbx' else result:='ebx';
                    1: result:='bx';
                    2: result:='bl';
                    3: result:='mm3';
                    4: result:='xmm3';
                  end;

              4:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rsp' else result:='esp';
                    1: result:='sp';
                    2: if rexprefix<>0 then result:='spl' else result:='ah';
                    3: result:='mm4';
                    4: result:='xmm4';
                  end;

              5:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rbp' else result:='ebp';
                    1: result:='bp';
                    2: if rexprefix<>0 then result:='bpl' else result:='ch';
                    3: result:='mm5';
                    4: result:='xmm5';
                  end;

              6:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rsi' else result:='esi';
                    1: result:='si';
                    2: if rexprefix<>0 then result:='sil' else result:='dh';
                    3: result:='mm6';
                    4: result:='xmm6';
                  end;

              7:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rdi' else result:='edi';
                    1: result:='di';
                    2: if rexprefix<>0 then result:='dil' else result:='bh';
                    3: result:='mm7';
                    4: result:='xmm7';
                  end;

              8: case inst of
                    0: if rex_w or (opperandsize=64) then result:='r8' else result:='r8d';
                    1: result:='r8w';
                    2: result:='r8l';
                    3: result:='mm8';
                    4: result:='xmm8';
                 end;

              9: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r9' else result:='r9d';
                   1: result:='r9w';
                   2: result:='r9l';
                   3: result:='mm9';
                   4: result:='xmm9';
                 end;

             10: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r10' else result:='r10d';
                   1: result:='r10w';
                   2: result:='r10l';
                   3: result:='mm10';
                   4: result:='xmm10';
                 end;

             11: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r11' else result:='r11d';
                   1: result:='r11w';
                   2: result:='r11l';
                   3: result:='mm11';
                   4: result:='xmm11';
                 end;

             12: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r12' else result:='r12d';
                   1: result:='r12w';
                   2: result:='r12l';
                   3: result:='mm12';
                   4: result:='xmm12';
                 end;

             13: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r13' else result:='r13d';
                   1: result:='r13w';
                   2: result:='r13l';
                   3: result:='mm13';
                   4: result:='xmm13';
                 end;

             14: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r14' else result:='r14d';
                   1: result:='r14w';
                   2: result:='r14l';
                   3: result:='mm14';
                   4: result:='xmm14';
                 end;

             15: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r15' else result:='r15d';
                   1: result:='r15w';
                   2: result:='r15l';
                   3: result:='mm15';
                   4: result:='xmm15';
                 end;
            end;


            result:=colorreg+result+endcolor;
          end;
    end;
    if showextrareg then
    begin
      case inst of
        0: if rex_w then ep:=regnrtostr(rt64, not opcodeflags.vvvv and $f) else ep:=regnrtostr(rt32, not opcodeflags.vvvv and $f);
        1: ep:=regnrtostr(rt16, not opcodeflags.vvvv and $f);
        2: ep:=regnrtostr(rt8, not opcodeflags.vvvv and $f);
        3: ep:=regnrtostr(rtMM, not opcodeflags.vvvv and $f);
        4: if opcodeflags.L then ep:=regnrtostr(rtYMM, not opcodeflags.vvvv and $f) else ep:=regnrtostr(rtXMM, not opcodeflags.vvvv and $f);
      end;

      case position of
        mLeft,mNone: result:=result+','+colorreg+ep+endcolor;
        mRight: result:=colorreg+ep+endcolor+','+result;
      end;
    end;

    result:=prestr+result+poststr;
  end;

  if last<>(modrmbyte+1) then //add an extra seperator since some bytes have been added, usually the last one, except when the opcode has a immeadiate value followed, which this seperator will then separate
  begin
    LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
    inc(LastDisassembleData.SeperatorCount);
  end;
end;


function tdisassembler.MODRM(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword; position: TMRPos=mLeft): string;
begin
  case inst of
    0: LastDisassembleData.datasize:=processhandler.pointersize;
    1: LastDisassembleData.datasize:=2;
    2: LastDisassembleData.datasize:=1;
    3: LastDisassembleData.datasize:=4;
    4: LastDisassembleData.datasize:=8
  end;
  result:=modrm2(memory,prefix,modrmbyte,inst,last,0,0,position);
end;

function TDisassembler.MODRM(memory:PByteArray; prefix: TPrefix; modrmbyte: integer; inst: integer; out last: dword;opperandsize:integer; addressSize: integer=0; position: TMRPos=mLeft): string;
begin
  result:=modrm2(memory,prefix,modrmbyte,inst,last, opperandsize, addressSize, position);
end;

function TDisassembler.SIB(memory:PByteArray; sibbyte: integer; var last: dword; addresssize: integer=0): string;
var
  dwordptr: ^dword;
  byteptr: ^byte absolute dwordptr;
  ss,index,base, _mod,_rm: integer;
  offset: string;

  indexstring: string;
  displacementstring: string;
  pref: char;
begin
  result:='';

  if is64bit then pref:='r' else pref:='e';

  dwordptr:=@memory[sibbyte+1];
  inc(last);  //sib byte

  LastDisassembleData.Seperators[LastDisassembleData.SeperatorCount]:=last;
  inc(LastDisassembleData.SeperatorCount);


  ss:=(memory[sibbyte] shr 6) and 3;
  index:=(memory[sibbyte] shr 3) and 7;
  if Rex_X then index:=index or 8;

  _mod:=getmod(memory[sibbyte-1]);
  _rm:=getrm(memory[sibbyte-1]);

  base:=memory[sibbyte] and 7;
  if Rex_B {and (_mod<>0)} then base:=base or 8;



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
  if is64bit then
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
    4: indexstring:='';//'esp';
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
   else
     indexstring:='';
  end;



  if is64bit and (addresssize<>32) then
  begin
    if indexstring<>'' then indexstring[1]:='r'; //quick replace

  end;
  if indexstring<>'' then
    indexstring:=colorreg+indexstring+endcolor;

  if (is64bit) and ((base and 7)=5) and (index=4) and (_mod=0) then //disp32
  begin
    //special case for 64-bit
    //sib has a 32-bit displacement value (starting at 0000000000000000)
    LastDisassembleData.modrmValueType:=dvtAddress;
    LastDisassembleData.modrmValue:=dwordptr^;

    result:=inttohexs(dwordptr^,8);
    last:=last+4;
    exit;
  end
  else
  begin

    case ss of
      0: LastDisassembleData.sibScaler:=1;
      1: LastDisassembleData.sibScaler:=2;
      2: LastDisassembleData.sibScaler:=4;
      3: LastDisassembleData.sibScaler:=8;
    end;

    if (ss>0) and (index<>4) then
      indexstring:=indexstring+'*'+colorhex+inttostr(LastDisassembleData.sibScaler)+endcolor;

    if indexstring<>'' then
    begin
      if result='' then
        result:=indexstring
      else
        result:=result+'+'+indexstring;
    end;
  end;

 //
  begin
    //mod 0 : [scaled index]+disp32
    //mod 1 : [scaled index]+disp8+ebp
    //mod 2 : [scaled index]+disp32+ebp

    displacementstring:='';
    case _mod of
      0: //sib with a mod of 0. scaled index + disp32
      begin
        //
        if base=5 then
        begin
          LastDisassembleData.modrmValueType:=dvtValue;
          LastDisassembleData.modrmValue:=ptruint(pinteger(dwordptr)^);

          if pinteger(dwordptr)^<0 then
            displacementstring:='-'+inttohexs(-pinteger(dwordptr)^,8)
          else
            displacementstring:=inttohexs(pinteger(dwordptr)^,8);

          last:=last+4;
        end;
      end;

      1: //scaled index + ebp+ disp 8
      begin
        //displacementstring:=colorreg+'EBP'+endcolor;

        LastDisassembleData.modrmValueType:=dvtValue;
        LastDisassembleData.modrmValue:=ptruint(pshortint(dwordptr)^);


        if pshortint(dwordptr)^<0 then
          displacementstring:='-'+inttohexs(-pshortint(dwordptr)^,2)
        else
          displacementstring:=inttohexs(pshortint(dwordptr)^,2);

        last:=last+1;
      end;

      2: //scaled index + ebp+disp 32
      begin
        //displacementstring:=colorreg+'EBP'+endcolor;
        LastDisassembleData.modrmValueType:=dvtValue;
        LastDisassembleData.modrmValue:=ptruint(pinteger(dwordptr)^);

        if pinteger(dwordptr)^<0 then
          displacementstring:='-'+inttohexs(-pinteger(dwordptr)^,8)
        else
          displacementstring:=inttohexs(pinteger(dwordptr)^,8);

        last:=last+4;
      end;

    end;

    if result='' then
      result:=displacementstring
    else
    begin
      if (displacementstring<>'') then
      begin
        if (displacementstring[1] = '-') then
          result:=result+displacementstring //already starts with a sign
        else
          result:=result+'+'+displacementstring;
      end;
    end;


  end;

  LastDisassembleData.hasSib:=true;
  LastDisassembleData.sibIndex:=index;


{$ifdef disassemblerdebug}
  result:=result+' ss='+inttostr(ss)+' index='+inttostr(index)+' base='+inttostr(base);
{$endif}


end;

{$ifndef jni}
procedure repairbreakbyte(address: ptruint; var b: byte);
{changes the given byte to the original byte if it is in fact a int3 breakpoint}
//pre: debuggerthread is valid
var bp: pbreakpoint;
begin
  debuggerthread.lockbplist;
  bp:=debuggerthread.isBreakpoint(address);
  if bp<>nil then
  begin
    if bp.breakpointMethod=bpmInt3 then
      b:=bp.originalbyte;
  end;
  debuggerthread.unlockbplist;

  if (frmCodeFilter<>nil) then frmcodefilter.isBreakpoint(address, b);

end;
{$endif}

function disassemble(var offset: ptrUint; var description: string): string; overload;
begin
  result:=defaultDisassembler.disassemble(offset,description);
end;

procedure TCR3Disassembler.setCR3(c: QWORD);
begin
  fcr3:=c and MAXPHYADDRMASKPB;
end;

function TCR3Disassembler.readMemory(address: ptruint; destination: pointer; size: integer): integer;
var actualread: ptruint;
begin
  ReadProcessMemoryCR3(fcr3,pointer(address), destination, size, actualread);
  result:=actualread;
end;


function TDisassembler.readMemory(address: ptruint; destination: pointer; size: integer): integer;
//reads the bytes at the given address and returns the number of bytes read
//in regards to the cloaked memory support, this is ONLY for the disassembler, to show the fuckery that's going on
var
  actualread: ptruint;

  i,p1,p2,p3: integer;
begin
  actualread:=0;

  ReadProcessMemoryWithCloakSupport(processhandle,pointer(address),destination,size,actualread);
  if (actualread=0) and ((address+size and qword($fffffffffffff000))>(address and qword($fffffffffffff000))) then //did not read a single byte and overlaps a pageboundary
  begin
    p1:=0;
    repeat
      i:=min(size, 4096-(address and $fff));
      actualread:=0;
      ReadProcessMemoryWithCloakSupport(processhandle,pointer(address),destination,i,actualread);

      inc(p1,actualread);
      address:=address+actualread;
      size:=size-actualread;
      destination:=pointer(ptruint(destination)+actualread);
    until (actualread=0) or (size=0);

    exit(p1);
  end
  else
    result:=actualread;
end;



function TDisassembler.disassemble(var offset: ptrUint; var description: string): string;
var
    actualread: PtrUInt;
    startoffset, initialoffset: ptrUint;
    tempresult, tempdescription: string;
    tempst: string;
    wordptr: ^word;
    dwordptr: ^dword;
    dwordptr2: ^dword;
    singleptr: ^single;
    doubleptr: ^double;
    extenedptr: ^extended;
    int64ptr: ^int64;
    i,j,k: integer;


    isprefix: boolean;

    last: dword;
    foundit: boolean;

    tempaddress: ptrUint;
    prefixsize: integer;
    mi: TModuleInfo;
    VA,PA: QWORD;
    noVEXPossible: boolean=false;
    bytestomove: integer;

    memory: PBytearray;

    td: dword;
    breaknow: boolean;
    disassembleData: TLastDisassembleData;
begin
  try
  DisassembleData.isfloat:=false;
  DisassembleData.iscloaked:=false;
  DisassembleData.commentsoverride:='';
  {$ifndef unix}
  if defaultBinutils<>nil then
  begin
    //use this
    DisassembleData.address:=offset;
    DisassembleData.SeperatorCount:=0;
    defaultBinutils.disassemble(DisassembleData);

    result:=inttohex(DisassembleData.address,8);
    result:=result+' - ';
    for i:=0 to length(DisassembleData.bytes)-1 do
      result:=result+inttohex(DisassembleData.Bytes[i],2)+' ';

    result:=result+' - ';
    result:=result+DisassembleData.opcode;
    result:=result+' ';
    result:=result+DisassembleData.parameters;

    if length(DisassembleData.bytes)>0 then
      inc(offset,length(DisassembleData.bytes))
    else
    begin
      if processhandler.SystemArchitecture=archArm then
      begin
        if (offset or 1)=1 then
          inc(offset,2)
        else
          inc(offset,4);
      end
      else
        inc(offset,1);
    end;

    exit;
  end;
  {$endif}
  if is64bitOverride then
    is64bit:=is64BitOverrideState
  else
  begin
    is64bit:=processhandler.is64bit;
    {$ifdef cpu64}
    if offset>=QWORD($100000000) then
      is64bit:=true;
    {$endif}

    if symhandler.getmodulebyaddress(offset, mi) then
      is64bit:=mi.is64bitmodule;

  end;


  if processhandler.SystemArchitecture=archarm then
  begin
    result:=ArmDisassembler.disassemble(offset);
    LastDisassembleData:=armdisassembler.LastDisassembleData;
    exit;
  end;


  modrmposition:=mNone;


  last:=0;
  tempresult:='';
  setlength(DisassembleData.bytes,0);
  DisassembleData.address:=Offset;
  DisassembleData.SeperatorCount:=0;
  DisassembleData.prefix:='';
  DisassembleData.PrefixSize:=0;
  DisassembleData.opcode:='';
  DisassembleData.parameters:='';
  DisassembleData.isjump:=false;
  DisassembleData.iscall:=false;
  DisassembleData.isret:=false;
  DisassembleData.isconditionaljump:=false;
  DisassembleData.modrmValueType:=dvtNone;
  DisassembleData.parameterValueType:=dvtNone;
  DisassembleData.hasSib:=false;
  DisassembleData.datasize:=0;
  DisassembleData.riprelative:=0;

  if assigned(OnDisassembleOverride) then //check if the user has defined it's own disassembler
  begin
    //if so, call the OnDisassemble propery, and if it returns true don't handle the original
    if OnDisassembleOverride(self, offset, DisassembleData, result, description) then
    begin
      if length(DisassembleData.Bytes)=0 then //BAD!
        setlength(DisassembleData.Bytes,1);

      inc(offset, length(DisassembleData.Bytes));

      LastDisassembleData:=disassembleData;
      exit;
    end;
  end;

  //also check global overrides
  for i:=0 to length(GlobalDisassembleOverrides)-1 do
  begin
    if assigned(GlobalDisassembleOverrides[i]) then
    begin
      if GlobalDisassembleOverrides[i](self, offset, DisassembleData, result, description) then
      begin
        if length(DisassembleData.Bytes)=0 then //BAD!
          setlength(DisassembleData.Bytes,1);

        inc(offset, length(DisassembleData.Bytes));
        LastDisassembleData:=disassembleData;
        exit;
      end;
    end;
  end;




  if isdefault then
  begin
    showsymbols:=symhandler.showsymbols;
    showmodules:=symhandler.showmodules;
  end;

  if showsymbols or showmodules then
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
  initialoffset:=offset;

  for i:=32 to 63 do //debug code
    _memory[i]:=$ce;

  actualRead:=readMemory(offset, @_memory[0], 32);
  memory:=@_memory[0];

  if actualread>0 then
  begin
    //I HATE THESE...   (I propably will not add them all, but I'll see how far I get)

    {$ifndef jni}
    if debuggerthread<>nil then
      for i:=0 to actualread-1 do
        if memory[i]=$cc then
          memory[i]:=debuggerthread.getrealbyte(offset+i);
    {$endif}


    while isprefix do
    begin
      inc(offset); //offset will always inc by 1
      if memory[0] in prefix then
      begin
        if length(DisassembleData.bytes)>10 then
        begin
          //prevent a too long prefix from crashing the disassembler (e.g 12GB filled with one prefix....)
          isprefix:=false;
          break;
        end;
        setlength(DisassembleData.bytes,length(DisassembleData.bytes)+1);
        DisassembleData.bytes[length(DisassembleData.bytes)-1]:=memory[0];


        if not dataonly then
          result:=result+inttohexs(memory[0],2)+' ';

        isprefix:=true;
        inc(startoffset);
        prefix2:=prefix2+[memory[0]];

        memory:=@memory[1];
        if offset>initialoffset+24 then //too long
        begin
          description:='';
          DisassembleData.opcode:='??';
          offset:=initialoffset+1;
          LastDisassembleData:=disassembleData;
          exit;
        end;

      end else isprefix:=false;
    end;

    if $F0 in prefix2 then
    begin
      tempresult:='lock ';
      noVEXPossible:=true;
    end;

    if $F2 in prefix2 then
    begin
      tempresult:=tempresult+'repne ';
      noVEXPossible:=true;
    end;

    if $f3 in prefix2 then
    begin
      tempresult:=tempresult+'repe ';
      noVEXPossible:=true;
    end;

    DisassembleData.prefix:=tempresult;

    zeromemory(@opcodeflags, sizeof(opcodeflags));

    RexPrefix:=0;
    if is64bit then
    begin
      if memory[0] in [$40..$4f] then //does it start with a rex prefix ?
      begin
        setlength(DisassembleData.bytes,length(DisassembleData.bytes)+1);
        DisassembleData.bytes[length(DisassembleData.bytes)-1]:=memory[0];

        RexPrefix:=memory[0];
        opcodeflags.B:=(RexPrefix and BIT_REX_B)=BIT_REX_B;
        opcodeflags.X:=(RexPrefix and BIT_REX_X)=BIT_REX_X;
        opcodeflags.R:=(RexPrefix and BIT_REX_R)=BIT_REX_R;
        opcodeflags.W:=(RexPrefix and BIT_REX_W)=BIT_REX_W;


        if not dataonly then
          result:=result+inttohexs(RexPrefix,2)+' ';

        inc(offset);
        inc(startoffset);
        prefix2:=prefix2+[RexPrefix];

        memory:=@memory[1];
        noVEXPossible:=true;

        if offset>initialoffset+24 then
        begin
          description:='';
          DisassembleData.opcode:='??';
          offset:=initialoffset+1;
          LastDisassembleData:=disassembleData;
          exit;
        end;
      end

    end;

    {$ifdef windows}
    if (memory[0]=$cc) and (debuggerthread<>nil) then //if it's a int3 breakpoint and there is a debugger attached check if it's a bp
      repairbreakbyte(startoffset, memory[0]);
    {$endif}


    prefixsize:=length(DisassembleData.bytes);
    DisassembleData.prefixsize:=prefixsize;

    if (noVEXPossible=false) and (memory[0] in [$c4,$c5]) then
    begin
      hasVEX:=true;

      if memory[0]=$c5 then
      begin
        //2 byte VEX
        inc(prefixsize,2);
        opcodeflags.pp:=PVex2Byte(@memory[1])^.pp;
        opcodeflags.L:=PVex2Byte(@memory[1])^.L=1;
        opcodeflags.vvvv:=PVex2Byte(@memory[1])^.vvvv;
        opcodeflags.R:=PVex2Byte(@memory[1])^.R=0;
        opcodeflags.mmmmm:=1;

        i:=length(DisassembleData.bytes);

        setlength(DisassembleData.bytes,length(DisassembleData.bytes)+2);
        DisassembleData.bytes[i]:=memory[0];
        DisassembleData.bytes[i+1]:=memory[1];

        memory[1]:=$0f;
        bytestomove:=1;
        memory:=@memory[1];

        inc(offset,1);
      end
      else
      begin
        //3 byte vex
        inc(prefixsize,3);
        opcodeflags.pp:=PVex3Byte(@memory[1])^.pp;
        opcodeflags.L:=PVex3Byte(@memory[1])^.L=1;
        opcodeflags.vvvv:=PVex3Byte(@memory[1])^.vvvv;
        opcodeflags.W:=PVex3Byte(@memory[1])^.W=1; //this one is NOT inverted
        opcodeflags.mmmmm:=PVex3Byte(@memory[1])^.mmmmm;
        opcodeflags.B:=PVex3Byte(@memory[1])^.B=0;
        opcodeflags.X:=PVex3Byte(@memory[1])^.X=0;
        opcodeflags.R:=PVex3Byte(@memory[1])^.R=0;

        i:=length(DisassembleData.bytes);
        setlength(DisassembleData.bytes,i+3);
        DisassembleData.bytes[i]:=memory[0];
        DisassembleData.bytes[i+1]:=memory[1];
        DisassembleData.bytes[i+2]:=memory[2];

        { mmmmm:
        00000: Reserved for future use (will #UD)
        00001: implied 0F leading opcode byte
        00010: implied 0F 38 leading opcode bytes
        00011: implied 0F 3A leading opcode bytes
        00100-11111: Reserved for future use (will #UD)
        }

        bytestomove:=3; //number of bytes to shift
        case opcodeflags.mmmmm of
          1:
          begin
            bytestomove:=2;
            memory[2]:=$0f;
          end;

          2:
          begin
            bytestomove:=1;
            memory[1]:=$0f;
            memory[2]:=$38;
          end;

          3:
          begin
            bytestomove:=1;
            memory[1]:=$0f;
            memory[2]:=$3a;
          end; //else invalid
        end;

        memory:=@memory[bytestomove];
        inc(offset,2);
      end;

      case opcodeflags.pp of
        1: prefix2:=prefix2+[$66];
        2: prefix2:=prefix2+[$f3];
        3: prefix2:=prefix2+[$f2];
      end;
    end else hasVEX:=false;


    //compatibility fix for code that still checks for rex.* or sets it as a temporary flag replacement
    RexPrefix:=ifthen(opcodeflags.B, rexprefix or BIT_REX_B, rexprefix);
    RexPrefix:=ifthen(opcodeflags.X, rexprefix or BIT_REX_X, rexprefix);
    RexPrefix:=ifthen(opcodeflags.R, rexprefix or BIT_REX_R, rexprefix);
    RexPrefix:=ifthen(opcodeflags.W, rexprefix or BIT_REX_W, rexprefix);

    case memory[0] of  //opcode
      $00 : begin
              if (memory[1]=$55) and (memory[2]=$89) and (memory[3]=$e5) then
              begin
                description:='Filler';
                DisassembleData.opcode:='db';
                DisassembleData.parameters:=inttohex(memory[0],2);
              end
              else
              begin
                description:='Add';

                DisassembleData.opcode:='add';
                DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);

                inc(offset,last-1);
              end;
            end;

      $01 : begin
              description:='Add';

              DisassembleData.opcode:='add';
              if $66 in prefix2 then DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);


              inc(offset,last-1);
            end;

      $02 : begin
              description:='Add';

              DisassembleData.opcode:='add';
              DisassembleData.parameters:=r8(memory[1])+MODRM(memory,prefix2,1,2,last, mRight);

              inc(offset,last-1);
            end;

      $03 : begin
              description:='Add';
              DisassembleData.opcode:='add';
              if $66 in prefix2 then DisassembleData.parameters:=r16(memory[1])+MODRM(memory,prefix2,1,1,last, mright) else
                                     DisassembleData.parameters:=r32(memory[1])+MODRM(memory,prefix2,1,0,last, mright);


              inc(offset,last-1);
            end;



      $04 : begin
              description:='Add '+inttohex(memory[1],2)+' to AL';
              DisassembleData.opcode:='add';
              DisassembleData.parameterValueType:=dvtValue;
              DisassembleData.parameterValue:=memory[1];
              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);

              DisassembleData.Seperators[DisassembleData.SeperatorCount]:=1;
              inc(DisassembleData.SeperatorCount);

              inc(offset);
            end;

      $05 : begin
              DisassembleData.opcode:='add';
              DisassembleData.parametervaluetype:=dvtvalue;


              wordptr:=@memory[1];
              dwordptr:=@memory[1];
              if $66 in prefix2 then
              begin
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;
                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);

                description:='add '+inttohex(wordptr^,4)+' to ax';



                inc(offset,2);
              end else
              begin
                if rex_w then
                begin
                  DisassembleData.parametervaluetype:=dvtvalue;
                  DisassembleData.parametervalue:=dwordptr^;
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8);

                  description:='add '+inttohex(dwordptr^,8)+' to rax (sign extended)';
                end
                else
                begin
                  DisassembleData.parametervaluetype:=dvtvalue;
                  DisassembleData.parametervalue:=dwordptr^;
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);

                  description:='add '+inttohex(dwordptr^,8)+' to eax';
                end;
                inc(offset,4);
              end;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $06 : begin
              DisassembleData.opcode:='push';
              DisassembleData.parameters:=colorreg+'es'+endcolor;
              description:='place es on the stack';
            end;

      $07 : begin
              DisassembleData.opcode:='pop';
              DisassembleData.parameters:=colorreg+'es'+endcolor;
              description:='remove es from the stack';
            end;

      $08 : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $09 : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              if $66 in prefix2 then DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                                     DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $0a : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last, mRight);
              inc(offset,last-1);
            end;

      $0b : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              if $66 in prefix2 then DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last, mRight) else
                                     DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last, mRight);

              inc(offset,last-1);
            end;

      $0c : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              inc(offset);
            end;

      $0d : begin
              description:='logical inclusive or';
              DisassembleData.opcode:='or';
              DisassembleData.parametervaluetype:=dvtvalue;

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                DisassembleData.parametervalue:=wordptr^;
                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(DisassembleData.parametervalue,4);

                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                begin
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(DisassembleData.parametervalue,4);
                  description:=description+' (sign-extended)';
                end
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(DisassembleData.parametervalue,4);


                DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
                inc(DisassembleData.seperatorcount);
                inc(offset,4);
              end;
            end;

      $0e : begin
              description:='place cs on the stack';
              DisassembleData.opcode:='push';
              DisassembleData.parameters:=colorreg+'cs'+endcolor;
            end;

      $0f : begin  //simd extensions
              if $f0 in prefix2 then
                DisassembleData.prefix:='lock '
              else
                DisassembleData.prefix:=''; //these usually treat the f2/f3 prefix differently

              case memory[1] of
                $00 : begin
                        case getreg(memory[2]) of
                         0:  begin
                               DisassembleData.opcode:='sldt';
                               description:='store local descriptor table register';
                               if $66 in prefix2 then DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16) else
                                                      DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                               inc(offset,last-1);
                             end;

                         1:  begin
                               description:='store task register';
                               DisassembleData.opcode:='str';
                               DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         2:  begin
                               description:='load local descriptor table register';
                               DisassembleData.opcode:='lldt';
                               DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         3:  begin
                               description:='load task register';
                               DisassembleData.opcode:='ltr';
                               DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);   ;
                             end;

                         4:  begin
                               description:='verify a segment for reading';
                               DisassembleData.opcode:='verr';
                               DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;

                         5:  begin
                               description:='verify a segment for writing';
                               DisassembleData.opcode:='verw';
                               DisassembleData.parameters:=modrm(memory,prefix2,2,1,last,16);
                               inc(offset,last-1);
                             end;
                         else
                         begin
                           DisassembleData.opcode:='db';
                           DisassembleData.parameters:=inttohex(memory[0],2);
                           description:='not specified by the intel documentation';
                         end;

                        end;

                      end;

                $01 : begin
                        case memory[2] of
                          $c1:
                          begin
                            description:='call to vm monitor by causing vm exit';
                            DisassembleData.opcode:='vmcall';
                            inc(offset,2);
                          end;

                          $c2:
                          begin
                            description:='launch virtual machine managed by current vmcs';
                            DisassembleData.opcode:='vmlaunch';
                            inc(offset,2);
                          end;

                          $c3:
                          begin
                            description:='resume virtual machine managed by current vmcs';
                            DisassembleData.opcode:='vmresume';
                            inc(offset,2);
                          end;

                          $c4:
                          begin
                            description:='leaves vmx operation';
                            DisassembleData.opcode:='vmxoff';
                            inc(offset,2);
                          end;

                          $c8:
                          begin
                            description:='set up monitor address';
                            DisassembleData.opcode:='monitor';
                            inc(offset,2);
                          end;

                          $c9:
                          begin
                            description:='Monitor wait';
                            DisassembleData.opcode:='mwait';
                            inc(offset,2);
                          end;

                          $ca:
                          begin
                            description:='Clear AC flag in EFLAGS register';
                            DisassembleData.opcode:='clac';
                            inc(offset,2);
                          end;

                          $d0:
                          begin
                            description:='Get value of extended control register';
                            DisassembleData.opcode:='xgetbv';
                            inc(offset,2);
                          end;

                          $d1:
                          begin
                            description:='Set value of extended control register';
                            DisassembleData.opcode:='xsetbv';
                            inc(offset,2);
                          end;

                          $d5:
                          begin
                            description:='Transactional end';
                            DisassembleData.opcode:='xend';
                            inc(offset,2);
                          end;

                          $d6:
                          begin
                            description:='Test if in transactional execution';
                            DisassembleData.opcode:='xtest';
                            inc(offset,2);
                          end;

                          $f8:
                          begin
                            description:='Swap GS base register';
                            DisassembleData.opcode:='swapgs';
                            inc(offset,2);
                          end;

                          $f9:
                          begin
                            description:='Read time-stamp counter and processor ID';
                            DisassembleData.opcode:='rdtscp';
                            inc(offset,2);
                          end;

                          else
                          begin
                            case getreg(memory[2]) of
                              0:
                              begin
                                description:='store global descriptor table register';
                                DisassembleData.opcode:='sgdt';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                              1:
                              begin
                                description:='store interrupt descriptor table register';
                                DisassembleData.opcode:='sidt';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                              2:
                              begin
                                description:='load global descriptor table register';
                                DisassembleData.opcode:='lgdt';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                              3:
                              begin
                                description:='load interupt descriptor table register';
                                DisassembleData.opcode:='lidt';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                              4:
                              begin
                                description:='store machine status word';
                                DisassembleData.opcode:='smsw';

                                if $66 in prefix2 then DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)
                                                  else DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                              6:
                              begin
                                description:='load machine status word';
                                DisassembleData.opcode:='lmsw';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,1,last);
                                inc(offset,last-1);
                              end;

                              7:
                              begin
                                description:='invalidate tlb entry';
                                DisassembleData.opcode:='invplg';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;
                            end;
                          end;
                        end;
                      end;

                $02 : begin
                        description:='load access rights byte';
                        DisassembleData.opcode:='lar';
                        if $66 in prefix2 then DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                               DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last, mRight);

                        inc(offset,last-1);
                      end;

            {0f}$03 : begin
                        description:='load segment limit';
                        DisassembleData.opcode:='lsl';
                        if $66 in prefix2 then DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                               DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last, mRight);

                        inc(offset,last-1);
                      end;

                $05 : begin
                        description:='fast system call';
                        DisassembleData.opcode:='syscall';
                        inc(offset);
                      end;

                $06 : begin
                        description:='clear task-switched flag in cr0';
                        DisassembleData.opcode:='clts';
                        inc(offset);
                      end;

                $07 : begin
                        description:='return from fast system call';
                        DisassembleData.opcode:='sysret';
                        inc(offset);
                      end;

                $08 : begin
                        description:='invalidate internal caches';
                        DisassembleData.opcode:='incd';
                        inc(offset);
                      end;

                $09 : begin
                        description:='write back and invalidate cache';
                        DisassembleData.opcode:='wbinvd';
                        inc(offset);
                      end;

                $0b : begin
                        description:='undefined instruction(yes, this one really excists..)';
                        DisassembleData.opcode:='ud2';
                        inc(offset);
                      end;

                $0d : begin
                        case getreg(memory[2]) of
                          1:  begin
                                description:='Prefetch Data into Caches in Anticipation of a Write';
                                DisassembleData.opcode:='prefetchw';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                 description:='Prefetch Vector Data Into Caches with Intent to Write and T1 Hint';
                                 DisassembleData.opcode:='prefetchwt1';
                                 DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                 inc(offset,last-1);
                               end;
                        end;
                      end;


                $10 : begin
                        DisassembleData.isfloat:=true;

                        if $f2 in prefix2 then
                        begin
                          description:='move scalar double-fp';
                          opcodeflags.L:=false; //LIG
                          opcodeflags.skipExtraRegOnMemoryAccess:=true;

                          if hasvex then
                            DisassembleData.opcode:='vmovsd'
                          else
                            DisassembleData.opcode:='movsd';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='move scalar single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovss'
                          else
                            DisassembleData.opcode:='movss';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move unaligned packed double-fp';
                          if hasvex then
                            DisassembleData.opcode:='lmovupd'
                          else
                            DisassembleData.opcode:='movupd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move unaligned four packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovups'
                          else
                            DisassembleData.opcode:='movups';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $11 : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          description:='move scalar double-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovsd'
                          else
                            DisassembleData.opcode:='movsd';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last,mLeft)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='move scalar single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovss'
                          else
                            DisassembleData.opcode:='movss';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move unaligned packed double-fp';
                          if hasvex then
                            DisassembleData.opcode:='lmovupd'
                          else
                            DisassembleData.opcode:='movupd';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move unaligned four packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovups'
                          else
                            DisassembleData.opcode:='movups';

                          opcodeflags.skipExtraRegOnMemoryAccess:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;

                      end;

            {0f}$12 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='move one double-fp and duplicate';
                          if hasvex then
                            DisassembleData.opcode:='vmovddup'
                          else
                            DisassembleData.opcode:='movddup';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='move packed single-fp Low and duplicate';
                          if hasvex then
                            DisassembleData.opcode:='vmovsldup'
                          else
                            DisassembleData.opcode:='movsldup';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move low packed double-precision floating-point value';
                          if hasvex then
                            DisassembleData.opcode:='vmovlpd'
                          else
                            DisassembleData.opcode:='movlpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';

                          if getmod(memory[2])=3 then
                            DisassembleData.opcode:='movhlps'
                          else
                            DisassembleData.opcode:='movlps';

                          if hasvex then
                            DisassembleData.opcode:='v'+DisassembleData.opcode;

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $13 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='move low packed double-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovlpd'
                          else
                            DisassembleData.opcode:='movlpd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move low packed single-fp';

                          if hasvex then
                            DisassembleData.opcode:='vmovlps'
                          else
                            DisassembleData.opcode:='movlps';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

            {0f}$14 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vunpcklpd'
                          else
                            DisassembleData.opcode:='unpcklpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vunpcklps'
                          else
                            DisassembleData.opcode:='unpcklps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $15 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='unpack and interleave high packed double-fp';
                          if hasvex then
                            DisassembleData.opcode:='vunpckhpd'
                          else
                            DisassembleData.opcode:='unpckhpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='unpckhps'
                          else
                            DisassembleData.opcode:='unpckhps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $16 : begin
                        DisassembleData.isfloat:=true;
                        if $f3 in prefix2 then
                        begin
                          description:='move packed single-fp high and duplicate';
                          if hasvex then
                            DisassembleData.opcode:='vmovshdup'
                          else
                            DisassembleData.opcode:='movshdup';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move high packed double-precision floating-point value';
                          if hasvex then
                            DisassembleData.opcode:='vmovhpd'
                          else
                            DisassembleData.opcode:='movhpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';

                          if getmod(memory[2])=3 then
                            DisassembleData.opcode:='movlhps'
                          else
                            DisassembleData.opcode:='movhps';

                          if hasvex then
                            DisassembleData.opcode:='v'+DisassembleData.opcode;

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $17 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='move high packed double-precision floating-point value';
                          if hasvex then
                            DisassembleData.opcode:='vmovhpd'
                          else
                            DisassembleData.opcode:='movhpd';

                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='high to low packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovhps'
                          else
                            DisassembleData.opcode:='movhps';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $18 : begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='prefetch';
                                DisassembleData.opcode:='prefetchnta';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          1:  begin
                                description:='prefetch';
                                DisassembleData.opcode:='prefetchto';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          2:  begin
                                description:='prefetch';
                                DisassembleData.opcode:='prefetcht1';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='prefetch';
                                DisassembleData.opcode:='prefetcht2';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,2,last);
                                inc(offset,last-1);
                              end;

                        end;
                      end;

                $1f:  begin
                        case getreg(memory[2]) of
                          0:  begin
                                description:='multibyte nop';
                                DisassembleData.opcode:='nop';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;
                        end;
                      end;



                $20 : begin
                        description:='move from control register';
                        DisassembleData.opcode:='mov';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+cr(memory[2]);
                        inc(offset,last-1);
                      end;

                $21 : begin
                        description:='move from debug register';
                        DisassembleData.opcode:='mov';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+dr(memory[2]);
                        inc(offset,last-1);
                      end;

                $22 : begin
                        description:='move to control register';
                        DisassembleData.opcode:='mov';
                        DisassembleData.parameters:=cr(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                        inc(offset,last-1);
                      end;

                $23 : begin
                        description:='move to debug register';
                        DisassembleData.opcode:='mov';
                        DisassembleData.parameters:=dr(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                        inc(offset,last-1);
                      end;

                $28 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move aligned packed double-fp values';
                          if hasvex then
                            DisassembleData.opcode:='vmovapd'
                          else
                            DisassembleData.opcode:='movapd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move aligned four packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovaps'
                          else
                            DisassembleData.opcode:='movaps';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $29 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move aligned packed double-fp values';
                          if hasvex then
                            DisassembleData.opcode:='vmovapd'
                          else
                            DisassembleData.opcode:='movapd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move aligned four packed single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vmovaps'
                          else
                            DisassembleData.opcode:='movaps';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;


                $2a : begin
                        if $f2 in prefix2 then
                        begin

                          description:='convert doubleword integer to scalar doubleprecision floating-point value';
                          if hasvex then
                            DisassembleData.opcode:='vcvtsi2sd'
                          else
                            DisassembleData.opcode:='cvtsi2sd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='scalar signed int32 to single-fp conversion';
                          if hasvex then
                            DisassembleData.opcode:='vcvtsi2ss'
                          else
                            DisassembleData.opcode:='cvtsi2ss';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert packed dword''s to packed dp-fp''s';
                            DisassembleData.opcode:='cvtpi2pd';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed signed int32 to packed single-fp conversion';
                            DisassembleData.opcode:='cvtpi2ps';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2b : begin
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmovntpd'
                          else
                            DisassembleData.opcode:='movntpd';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move packed double-precision floating-point using non-temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmovntps'
                          else
                            DisassembleData.opcode:='movntps';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move aligned four packed single-fp non temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $2c : begin
                        if $f2 in prefix2 then
                        begin

                          description:='convert with truncation scalar double-precision floating point value to signed doubleword integer';
                          if hasvex then
                            DisassembleData.opcode:='vcvttsd2si'
                          else
                            DisassembleData.opcode:='cvttsd2si';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='scalar single-fp to signed int32 conversion (truncate)';
                          if hasvex then
                            DisassembleData.opcode:='vcvttss2si'
                          else
                            DisassembleData.opcode:='cvttss2si';

                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='packed doubleprecision-fp to packed dword conversion (truncate)';
                            DisassembleData.opcode:='cvttpd2pi';
                            DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp to packed int32 conversion (truncate)';
                            DisassembleData.opcode:='cvttps2pi';
                            DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2d : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          description:='convert scalar double-precision floating-point value to doubleword integer';
                          if hasvex then
                            DisassembleData.opcode:='vcvtsd2si'
                          else
                            DisassembleData.opcode:='cvtsd2si';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='scalar single-fp to signed int32 conversion';
                          if hasvex then
                            DisassembleData.opcode:='vcvtss2si'
                          else
                            DisassembleData.opcode:='cvtss2si';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert 2 packed dp-fp''s from param 2 to packed signed dword in param1';
                            DisassembleData.opcode:='cvtpi2ps';
                            DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp to packed int32 conversion';
                            DisassembleData.opcode:='cvtps2pi';
                            DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                            DisassembleData.datasize:=4;
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $2e : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='unordered scalar double-fp compare and set eflags';
                          if hasvex then
                            DisassembleData.opcode:='vucomisd'
                          else
                            DisassembleData.opcode:='ucomisd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unordered scalar single-fp compare and set eflags';
                          if hasvex then
                            DisassembleData.opcode:='vucomiss'
                          else
                            DisassembleData.opcode:='ucomiss';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;


                $2f : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='compare scalar ordered double-precision floating point values and set eflags';
                          if hasvex then
                            DisassembleData.opcode:='vcomisd'
                          else
                            DisassembleData.opcode:='comisd';
                          opcodeflags.skipExtraReg:=true;

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='scalar ordered single-fp compare and set eflags';
                          if hasvex then
                            DisassembleData.opcode:='vcomiss'
                          else
                            DisassembleData.opcode:='comiss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $30 : begin
                        description:='write to model specific register';
                        DisassembleData.opcode:='wrmsr';
                        inc(offset);
                      end;

                $31 : begin
                        description:='read time-stamp counter';
                        DisassembleData.opcode:='rdtsc';
                        inc(offset);
                      end;

                $32 : begin
                        description:='read from model specific register';
                        DisassembleData.opcode:='rdmsr';
                        inc(offset);
                      end;

                $33: begin
                        description:='read performance-monitoring counters';
                        DisassembleData.opcode:='rdpmc';
                        inc(offset);
                      end;

                $34: begin
                        description:='fast transistion to system call entry point';
                        DisassembleData.opcode:='sysenter';
                        DisassembleData.isret:=true;
                        inc(offset);
                      end;

                $35: begin
                        description:='fast transistion from system call entry point';
                        DisassembleData.opcode:='sysexit';
                        inc(offset);
                      end;

                $37: begin
                        description:='Safermode multipurpose function';
                        DisassembleData.opcode:='getsec';
                        inc(offset);
                      end;

           {0f} $38:  begin
                        case memory[2] of
                          $00: begin
                                 description:='Packed shuffle bytes';
                                 DisassembleData.opcode:='pshufb';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $01: begin
                                 description:='Packed horizontal add';
                                 DisassembleData.opcode:='phaddw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

        {0f}{38}          $02: begin
                                 description:='Packed horizontal add';
                                 DisassembleData.opcode:='phaddd';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $03: begin
                                 description:='Packed horizontal add and saturate';
                                 DisassembleData.opcode:='phaddsw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $04: begin
                                 description:='Multiply and add signed and unsigned bytes';
                                 DisassembleData.opcode:='pmaddubsw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

            {0f}{38}      $05: begin
                                 description:='Packed horizontal subtract';
                                 DisassembleData.opcode:='phsubw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $06: begin
                                 description:='Packed horizontal subtract';
                                 DisassembleData.opcode:='phsubd';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $07: begin
                                 description:='Packed horizontal subtract';
                                 DisassembleData.opcode:='phsubsw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $08: begin
                                 description:='Packed SIGN';
                                 DisassembleData.opcode:='psignb';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $09: begin
                                 description:='Packed SIGN';
                                 DisassembleData.opcode:='psignw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

                          $0a: begin
                                 description:='Packed SIGN';
                                 DisassembleData.opcode:='psignd';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

          {0f}{38}        $0b: begin
                                 description:='Packed multiply high with round and scale';
                                 DisassembleData.opcode:='phmulhrsw';

                                 if $66 in prefix2 then
                                 begin
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;
                                 end
                                 else
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                 inc(offset,last-1);
                               end;

          {0f}{38}        $0c: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='permute single-precision floating-point values';
                                     DisassembleData.opcode:='vpermilps';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                     inc(offset,last-1);
                                   end
                                 end;

                               end;

          {0f}{38}        $0d: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='permute double-precision floating-point values';
                                     DisassembleData.opcode:='vpermilpd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                     inc(offset,last-1);
                                   end
                                 end;

                               end;

          {0f}{38}        $0e: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Packed bit test';
                                     DisassembleData.opcode:='vtestps';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                     inc(offset,last-1);
                                   end
                                 end;
                               end;

          {0f}{38}        $0f: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Packed bit test';
                                     DisassembleData.opcode:='vtestpd';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                     inc(offset,last-1);
                                   end
                                 end;
                               end;

                          $10: begin
                                 description:='Variable blend packed bytes';
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     DisassembleData.opcode:='vpblendvb';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     DisassembleData.parameters:=DisassembleData.parameters+','+regnrtostr(rtXMM,memory[last]);
                                     inc(offset,1);
                                   end
                                   else
                                   begin
                                     DisassembleData.opcode:='pblendvb';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+regnrtostr(rtXMM,0);
                                   end;
                                   inc(offset,last-1);
                                 end;
                               end;

                {0f}{38}  $13: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Convert 16-bit FP values to single-precision FP values';
                                   DisassembleData.opcode:='vcvtph2ps';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;


                          $14: begin
                                 description:='Variable blend packed single precision floating-point values';
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     DisassembleData.opcode:='vblendvps';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     DisassembleData.parameters:=DisassembleData.parameters+','+regnrtostr(rtXMM,memory[last]);
                                     inc(offset,1);
                                   end
                                   else
                                   begin
                                     DisassembleData.opcode:='blendvps';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+regnrtostr(rtXMM,0);
                                   end;
                                   inc(offset,last-1);
                                 end;
                               end;

                          $15: begin
                                 description:='Variable blend packed double precision floating-point values';
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     DisassembleData.opcode:='vblendvpd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     DisassembleData.parameters:=DisassembleData.parameters+','+regnrtostr(rtXMM,memory[last]);
                                     inc(offset,1);
                                   end
                                   else
                                   begin
                                     DisassembleData.opcode:='blendvpd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+regnrtostr(rtXMM,0);
                                   end;
                                   inc(offset,last-1);
                                 end;
                               end;

                          $16: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Permute single-precision floating-point elements';
                                     DisassembleData.opcode:='vpermps';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end
                               end;

                          $17: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Logical compare';
                                   if hasvex then
                                     DisassembleData.opcode:='vptest'
                                   else
                                     DisassembleData.opcode:='ptest';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end
                               end;

                          $18: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast floating-point-data';
                                     DisassembleData.opcode:='vbroadcastss';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end
                               end;

                          $19: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast floating-point-data';
                                     DisassembleData.opcode:='vbroadcastsd';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end
                               end;

                          $1a: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast floating-point-data';
                                     DisassembleData.opcode:='vbroadcastf128';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end
                               end;

          {0f}{38}        $1c: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed absolute value';
                                   if hasvex then
                                     DisassembleData.opcode:='vpabsb'
                                   else
                                     DisassembleData.opcode:='pabsb';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Packed absolute value';
                                   DisassembleData.opcode:='pabsb';
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $1d: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed absolute value';
                                   if hasvex then
                                     DisassembleData.opcode:='vpabsw'
                                   else
                                     DisassembleData.opcode:='pabsw';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Packed absolute value';
                                   DisassembleData.opcode:='pabsw';
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $1e: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed absolute value';
                                   if hasvex then
                                     DisassembleData.opcode:='vpabsd'
                                   else
                                     DisassembleData.opcode:='pabsd';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Packed absolute value';
                                   DisassembleData.opcode:='pabsd';
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $20: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxbw'
                                   else
                                     DisassembleData.opcode:='pmovsxbw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $21: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxbd'
                                   else
                                     DisassembleData.opcode:='pmovsxbd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $22: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxbq'
                                   else
                                     DisassembleData.opcode:='pmovsxbq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $23: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxwd'
                                   else
                                     DisassembleData.opcode:='pmovsxwd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $24: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxwq'
                                   else
                                     DisassembleData.opcode:='pmovsxwq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $25: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with sign extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovsxdq'
                                   else
                                     DisassembleData.opcode:='pmovsxdq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $28: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Multiple packed signed dword integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmuldq'
                                   else
                                     DisassembleData.opcode:='pmuldq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $29: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Compare packed qword data for equal';
                                   if hasvex then
                                     DisassembleData.opcode:='vpcmpeqq'
                                   else
                                     DisassembleData.opcode:='pcmpeqq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $2a: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Load double quadword non-temporal aligned hint';
                                   if hasvex then
                                     DisassembleData.opcode:='vmovntdqa'
                                   else
                                     DisassembleData.opcode:='movntdqa';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $2b: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Pack with unsigned saturation';
                                   if hasvex then
                                     DisassembleData.opcode:='vpackusdw'
                                   else
                                     DisassembleData.opcode:='packusdw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $2c: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Conditional SIMD packed loads and stores';
                                     DisassembleData.opcode:='vmaskmovps';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

          {0f}{38}        $2d: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Conditional SIMD packed loads and stores';
                                     DisassembleData.opcode:='vmaskmovpd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

          {0f}{38}        $2e: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Conditional SIMD packed loads and stores';
                                     DisassembleData.opcode:='vmaskmovps';
                                     DisassembleData.parameters:=modrm(memory,prefix2,3,4,last, mLeft)+xmm(memory[3]);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

          {0f}{38}        $2f: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Conditional SIMD packed loads and stores';
                                     DisassembleData.opcode:='vmaskmovpd';
                                     DisassembleData.parameters:=modrm(memory,prefix2,3,4,last, mLeft)+xmm(memory[3]);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

          {0f}{38}        $30: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxbw'
                                   else
                                     DisassembleData.opcode:='pmovzxbw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $31: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxbd'
                                   else
                                     DisassembleData.opcode:='pmovzxbd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $32: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxbq'
                                   else
                                     DisassembleData.opcode:='pmovzxbq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $33: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxwd'
                                   else
                                     DisassembleData.opcode:='pmovzxwd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $34: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxwq'
                                   else
                                     DisassembleData.opcode:='pmovzxwq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $35: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed move with zero extend';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmovzxdq'
                                   else
                                     DisassembleData.opcode:='pmovzxdq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $36: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Full doublewords element permutation';
                                     DisassembleData.opcode:='vpermd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;


          {0f}{38}        $37: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Compare packed data for greater than';
                                   if hasvex then
                                     DisassembleData.opcode:='vpcmpgtq'
                                   else
                                     DisassembleData.opcode:='pcmpgtq';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $38: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Minimum of packed signed byte integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpminsb'
                                   else
                                     DisassembleData.opcode:='pminsb';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $39: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Minimum of packed dword integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpminsd'
                                   else
                                     DisassembleData.opcode:='pminsd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3a: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Minimum of packed word integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpminuw'
                                   else
                                     DisassembleData.opcode:='pminuw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3b: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Minimum of packed dword integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpminud'
                                   else
                                     DisassembleData.opcode:='pminud';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3c: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Maximum of packed signed byte integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmaxsb'
                                   else
                                     DisassembleData.opcode:='pmaxsb';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3d: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Maximum of packed signed dword integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmaxsd'
                                   else
                                     DisassembleData.opcode:='pmaxsd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3e: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Maximum of packed word integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmaxuw'
                                   else
                                     DisassembleData.opcode:='pmaxuw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $3f: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Maximum of packed unsigned dword integers';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmaxud'
                                   else
                                     DisassembleData.opcode:='pmaxud';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

           {0f}{38}       $40: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Multiply Packed Signed Dword Integers and Store Low Result';
                                   if hasvex then
                                     DisassembleData.opcode:='vpmulld'
                                   else
                                     DisassembleData.opcode:='pmulld';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

           {0f}{38}       $41: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed horitontal word minimum';
                                   if hasvex then
                                     DisassembleData.opcode:='phminposuw'
                                   else
                                     DisassembleData.opcode:='vphminposuw';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $45: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Variable Bit Shift Right Logical';

                                     if Rex_W then
                                       DisassembleData.opcode:='vpsrlvq'
                                     else
                                       DisassembleData.opcode:='vpsrlvd';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

           {0f}{38}       $46: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Variable bit shift right arithmetic';
                                     DisassembleData.opcode:='vpsravd';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $47: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Variable Bit Shift Left Logical';

                                     if Rex_W then
                                       DisassembleData.opcode:='vpsllvq'
                                     else
                                       DisassembleData.opcode:='vpsllvd';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;


           {0f}{38}       $58: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast integer data';
                                     DisassembleData.opcode:='vpbroadcastd';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

           {0f}{38}       $59: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast integer data';
                                     DisassembleData.opcode:='vpbroadcastq';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

           {0f}{38}       $5a: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast integer data';
                                     DisassembleData.opcode:='vpbroadcasti128';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

           {0f}{38}       $78: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast integer data';
                                     DisassembleData.opcode:='vpbroadcastb';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

           {0f}{38}       $79: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Broadcast integer data';
                                     DisassembleData.opcode:='vpbroadcastw';
                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $82: begin
                                 description:='Invalidate process-context-identifier';
                                 DisassembleData.opcode:='invpcid';
                                 if processhandler.is64Bit then
                                   DisassembleData.parameters:=r64(memory[3])+modrm(memory,prefix2,3,0,last,128,0, mRight)
                                 else
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,128,0,mRight);

                                 inc(offset,last-1);
                               end;

                          $8c: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Conditional SIMD Integer Packed Loads and Stores';
                                       DisassembleData.opcode:='vpmaskmovq';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Conditional SIMD Integer Packed Loads and Stores';
                                       DisassembleData.opcode:='vpmaskmovd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $8e: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Conditional SIMD Integer Packed Loads and Stores';
                                       DisassembleData.opcode:='vpmaskmovq';
                                       DisassembleData.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3]);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Conditional SIMD Integer Packed Loads and Stores';
                                       DisassembleData.opcode:='vpmaskmovd';
                                       DisassembleData.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3]);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;


                          $96: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-alnterating add/subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-alnterating add/subtract of precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $97: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-alnterating subtract/add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-alnterating subtract/add of precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $98: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $99: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd132sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd132ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $9a: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $9B: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

           {0f}{38}       $9c: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $9D: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd132sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd132ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $9e: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $9f: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub132ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $a6: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiply-alternating add/subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $a7: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiply-alternating subtract/add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiply-alternating subtract/add of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $a8: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

           {0f}{38}       $a9: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd213sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd213ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $aa: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;


                          $ab: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $ac: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $ad: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd213sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd213ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

               {0f}{38}   $ae: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub213ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $af: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmsub213sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfnmsub213ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $b6: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiply-alternating add/subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmaddsub231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $b7: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiply-alternating subtract/add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsubadd231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $b8: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $b9: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd231sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmadd231ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $ba: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if rex_w then
                                     begin
                                       description:='Fused multiple-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $bb: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused multiple-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

            {0f}{38}      $bc: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $bd: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-add of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd231sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfnmadd231ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;


                          $be: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of packed double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231pd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231ps';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;

                          $bf: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     if Rex_W then
                                     begin
                                       description:='Fused negative multiply-subtract of scalar double precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231sd';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                       DisassembleData.opcode:='vfmsub231ss';
                                       DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;
                                 end;
                               end;


                          $db: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Perform the AES InvMixColumn transformation';
                                   if hasvex then
                                     DisassembleData.opcode:='vaesimc'
                                   else
                                     DisassembleData.opcode:='aesimc';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $dc: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Perform one round of an AES encryption flow';
                                   if hasvex then
                                     DisassembleData.opcode:='vaesenc'
                                   else
                                     DisassembleData.opcode:='aesenc';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $dd: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Perform last round of an AES encryption flow';
                                   if hasvex then
                                     DisassembleData.opcode:='caesenclast'
                                   else
                                     DisassembleData.opcode:='aesenclast';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

                {0f}{38}  $de: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Perform one round of an AES decryption flow';
                                   if hasvex then
                                     DisassembleData.opcode:='vaesdec'
                                   else
                                     DisassembleData.opcode:='aesdec';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $df: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Perform last round of an AES decryption flow';
                                   if hasvex then
                                     DisassembleData.opcode:='caesdeclast'
                                   else
                                     DisassembleData.opcode:='aesdeclast';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                   inc(offset,last-1);
                                 end;
                               end;

            {0f}{38}     $f0: begin
                                 if $f2 in prefix then
                                 begin
                                   description:='Accumulate CRC32 value';
                                   DisassembleData.opcode:='crc32';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,2,last,mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Move data after swapping bytes';
                                   DisassembleData.opcode:='movbe';
                                   if $66 in prefix then
                                     DisassembleData.parameters:=r16(memory[3])+modrm(memory,prefix2,3,2,last,mRight)
                                   else
                                     DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $f1: begin
                                 if $f2 in prefix then
                                 begin
                                   description:='Accumulate CRC32 value';
                                   DisassembleData.opcode:='crc32';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Move data after swapping bytes';
                                   DisassembleData.opcode:='movbe';
                                   DisassembleData.parameters:=modrm(memory,prefix2,3,0,last, mLeft)+r32(memory[3]);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $f2: begin
                                 if hasvex then
                                 begin
                                   description:='Logical AND NOT';
                                   DisassembleData.opcode:='andn';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                   inc(offset,last-1);
                                 end;
                               end;

          {0f}{38}        $f3: begin
                                 case getreg(memory[3]) of
                                   1:
                                   begin
                                     description:='Reset lowerst set bit';
                                     DisassembleData.opcode:='blsr';
                                     DisassembleData.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                     inc(offset,last-1);
                                   end;

                                   2:
                                   begin
                                     description:='Get mask up to lowest set bit';
                                     DisassembleData.opcode:='blsmsk';
                                     DisassembleData.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                     inc(offset,last-1);
                                   end;

                                   3:
                                   begin
                                     description:='Extract lowest set isolated bit';
                                     DisassembleData.opcode:='blsi';
                                     DisassembleData.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $f5: begin
                                 if $f2 in prefix2 then
                                 begin
                                   description:='Parallel bits deposit';
                                   DisassembleData.opcode:='pdep';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mLeft);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   description:='Zero high bits starting with specified bit position';
                                   DisassembleData.opcode:='bzhi';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mLeft);
                                   inc(offset,last-1);
                                 end;
                               end;


                          $f6: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='ADX: Unsigned Integer Addition of Two Operands with Carry Flag';
                                   DisassembleData.opcode:='adcx';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 if $f3 in prefix2 then
                                 begin
                                   description:='ADX: Unsigned Integer Addition of Two Operands with Overflow Flag';
                                   DisassembleData.opcode:='adox';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Unsigned multiple without affecting flags';
                                     DisassembleData.opcode:='mulx';
                                     DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

          {0f}{38}        $f7: begin
                                 if hasvex then
                                 begin
                                   if $f3 in prefix2 then
                                   begin
                                     description:='Shift arithmetically right without affecting flags';
                                     DisassembleData.opcode:='SARX';
                                     DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                     inc(offset,last-1);
                                   end
                                   else
                                   if $f2 in prefix2 then
                                   begin
                                     description:='Shift logically right without affecting flags';
                                     DisassembleData.opcode:='SHRX';
                                     DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                     inc(offset,last-1);
                                   end
                                   else
                                   if $66 in prefix2 then
                                   begin
                                     description:='Shift logically left without affecting flags';
                                     DisassembleData.opcode:='SHLX';
                                     DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                     inc(offset,last-1);
                                   end;
                                 end
                                 else
                                 begin
                                   description:='Bit field extract';
                                   DisassembleData.opcode:='BEXTR';
                                   DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                   inc(offset,last-1);
                                 end;
                               end;
                        end;
                      end;

                $3a : begin
                        case memory[2] of
                          $00: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Qwords element permutation';
                                     DisassembleData.opcode:='vpermq';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $01: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Permute double-precision floating-point elements';
                                     DisassembleData.opcode:='vpermpd';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $02: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Blend packed dwords';
                                     DisassembleData.opcode:='vblenddd';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $04: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Permute single-prevision floating-point values';
                                     DisassembleData.opcode:='vpermilps';

                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;


                          $05: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Permute double-prevision floating-point values';
                                     DisassembleData.opcode:='vpermilpd';

                                     opcodeflags.skipExtraReg:=true;
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

               {0f}{3a}   $06: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Permute floating-point values';
                                     DisassembleData.opcode:='vperm2f128';

                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;


           {0f}{3a}       $08: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Round scalar single precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vroundps'
                                   else
                                     DisassembleData.opcode:='roundps';

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $09: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Round packed double precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vroundpd'
                                   else
                                     DisassembleData.opcode:='roundpd';

                                   opcodeflags.skipExtraReg:=true;

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $0a: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Round scalar single precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vroundss'
                                   else
                                     DisassembleData.opcode:='roundss';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $0b: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Round packed single precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vroundsd'
                                   else
                                     DisassembleData.opcode:='roundsd';



                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

               {0f}{3a}   $0c: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Blend packed single precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vblendps'
                                   else
                                     DisassembleData.opcode:='blendps';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

               {0f}{3a}   $0d: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Blend packed double precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vblendpd'
                                   else
                                     DisassembleData.opcode:='blendpd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

               {0f}{3a}   $0e: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Blend packed words';
                                   if hasvex then
                                     DisassembleData.opcode:='vpblendw'
                                   else
                                     DisassembleData.opcode:='pblendw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

               {0f}{3a}   $0f: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Packed align right';
                                   if hasvex then
                                     DisassembleData.opcode:='vpalignr'
                                   else
                                     DisassembleData.opcode:='palignr';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end
                                 else
                                 begin
                                   DisassembleData.opcode:='palignr';
                                   DisassembleData.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                 {0f}{3a} $14: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Extract byte';
                                   if hasvex then
                                     DisassembleData.opcode:='vpextrb'
                                   else
                                     DisassembleData.opcode:='pextrb';

                                   DisassembleData.parameters:=modrm(memory,prefix2,3,2,last,mleft)+xmm(memory[3])+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                 {0f}{3a} $15: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Extract word';
                                   if hasvex then
                                     DisassembleData.opcode:='vpextrw'
                                   else
                                     DisassembleData.opcode:='pextrw';

                                   DisassembleData.parameters:=modrm(memory,prefix2,3,1,last,mleft)+xmm(memory[3])+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                 {0f}{3a} $16: begin
                                 if $66 in prefix2 then
                                 begin
                                   if Rex_W then
                                   begin
                                     description:='Extract qword';
                                     DisassembleData.opcode:='pextrq';
                                   end
                                   else
                                   begin
                                     description:='Extract dword';
                                     DisassembleData.opcode:='pextrd';
                                   end;

                                   if hasvex then DisassembleData.opcode:='v'+DisassembleData.opcode;

                                   opcodeflags.skipExtraReg:=true;
                                   DisassembleData.parameters:=modrm(memory,prefix2,3,2,last,mleft)+xmm(memory[3])+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $17: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Extract packed single precision floating-point value';
                                   if hasvex then
                                     DisassembleData.opcode:='vextractps'
                                   else
                                     DisassembleData.opcode:='extractps';

                                   DisassembleData.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3])+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;


                          $18: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Insert packed floating-point values';
                                     DisassembleData.opcode:='vinsertf128';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $19: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Extract packed floating-point values';
                                     DisassembleData.opcode:='vextractf128';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;


                {0f}{3a}  $1d: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Convert single-precision FP value to 16-bit FP value';
                                     DisassembleData.opcode:='vcvtps2ph';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

               {0f}{3a}   $20: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Insert Byte';
                                   if hasvex then
                                     DisassembleData.opcode:='vpinsrb'
                                   else
                                     DisassembleData.opcode:='pinsrb';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,0,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $22: begin
                                 if $66 in prefix2 then
                                 begin
                                   if Rex_W then
                                   begin
                                     description:='Insert qword';
                                     DisassembleData.opcode:='pinsrq';
                                   end
                                   else
                                   begin
                                     description:='Insert dword';
                                     DisassembleData.opcode:='pinsrd';
                                   end;

                                   if hasvex then
                                     DisassembleData.opcode:='v'+DisassembleData.opcode;

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,0,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $38: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Insert packed integer values';
                                     DisassembleData.opcode:='vinserti128';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $39: begin
                                 if $66 in prefix2 then
                                 begin
                                   if hasvex then
                                   begin
                                     description:='Extract packed integer values';
                                     DisassembleData.opcode:='vextracti128';
                                     DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                     DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                     inc(last);
                                     inc(offset,last-1);
                                   end;
                                 end;
                               end;

                          $40: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Dot product of packed single precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vdpps'
                                   else
                                     DisassembleData.opcode:='dpps';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

                          $41: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Dot product of packed double precision floating-point values';
                                   if hasvex then
                                     DisassembleData.opcode:='vdppd'
                                   else
                                     DisassembleData.opcode:='dppd';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

              {0f}{3a}    $42: begin
                                 if $66 in prefix2 then
                                 begin
                                   description:='Compute multiple packed sums of absolute difference';
                                   if hasvex then
                                     DisassembleData.opcode:='vmpsadbw'
                                   else
                                     DisassembleData.opcode:='mpsadbw';

                                   DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                   DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                   inc(last);
                                   inc(offset,last-1);
                                 end;
                               end;

              {0f}{3a}   $44: begin
                                if $66 in prefix2 then
                                begin
                                  description:='Carry-less multiplication quadword';
                                  if hasvex then
                                    DisassembleData.opcode:='vpclmulqdq'
                                  else
                                    DisassembleData.opcode:='pclmulqdq';

                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

              {0f}{3a}   $46: begin
                                if $66 in prefix2 then
                                begin
                                  if hasvex then
                                  begin
                                    description:='Permute integer values';
                                    DisassembleData.opcode:='vperm2i128';

                                    DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                    DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                    inc(last);
                                    inc(offset,last-1);
                                  end;
                                end;
                              end;

              {0f}{3a}   $60: begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed compare explicit length string, return mask';
                                  if hasvex then
                                    DisassembleData.opcode:='vpcmpestrm'
                                  else
                                    DisassembleData.opcode:='pcmpestrm';

                                  opcodeflags.skipExtraReg:=true;
                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

              {0f}{3a}   $61: begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed compare explicit length string, return index';
                                  if hasvex then
                                    DisassembleData.opcode:='vpcmpestri'
                                  else
                                    DisassembleData.opcode:='pcmpestri';

                                  opcodeflags.skipExtraReg:=true;
                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

              {0f}{3a}   $62: begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed compare implicit length string, return mask';
                                  if hasvex then
                                    DisassembleData.opcode:='vpcmpistrm'
                                  else
                                    DisassembleData.opcode:='pcmpistrm';

                                  opcodeflags.skipExtraReg:=true;
                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

              {0f}{3a}   $63: begin
                                if $66 in prefix2 then
                                begin
                                  description:='Packed compare implicit length string, return index';
                                  if hasvex then
                                    DisassembleData.opcode:='vpcmpistri'
                                  else
                                    DisassembleData.opcode:='pcmpistri';

                                  opcodeflags.skipExtraReg:=true;
                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

                         $df: begin
                                if $66 in prefix2 then
                                begin
                                  description:='AES round key generation assist';
                                  if hasvex then
                                    DisassembleData.opcode:='vaeskeygenassist'
                                  else
                                    DisassembleData.opcode:='aeskeygenassist';

                                  DisassembleData.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;

                         $f0: begin
                                if $f2 in prefix2 then
                                begin
                                  if hasvex then
                                  begin
                                    description:='Rotate right logical without affecting flags';
                                    DisassembleData.opcode:='rorx';
                                    opcodeflags.skipExtraReg:=true;
                                    DisassembleData.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight)+',';
                                    DisassembleData.parameters:=DisassembleData.parameters+','+inttohex(memory[last],2);
                                    inc(last);
                                    inc(offset,last-1);
                                  end;
                                end;
                              end;



                        end;
                      end;


                $40 : begin
                        description:='move if overflow';
                        DisassembleData.opcode:='cmovo';
                        if $66 in prefix2 then DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                               DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                        inc(offset,last-1);
                      end;

                $41 : begin
                        description:='move if not overflow';
                        DisassembleData.opcode:='cmovno';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $42 : begin
                        description:='move if below/ move if carry';
                        DisassembleData.opcode:='cmovb';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $43 : begin
                        description:='move if above or equal/ move if not carry';
                        DisassembleData.opcode:='cmovae';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $44 : begin
                        description:='move if equal/move if zero';
                        DisassembleData.opcode:='cmove';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $45 : begin
                        description:='move if not equal/move if not zero';
                        DisassembleData.opcode:='cmovne';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $46 : begin
                        description:='move if below or equal';
                        DisassembleData.opcode:='cmovbe';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;


                $47 : begin
                        description:='move if above';
                        DisassembleData.opcode:='cmova';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $48 : begin
                        description:='move if sign';
                        DisassembleData.opcode:='cmovs';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $49 : begin
                        description:='move if not sign';
                        DisassembleData.opcode:='cmovns';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $4a : begin
                        description:='move if parity even';
                        DisassembleData.opcode:='cmovpe';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $4b : begin
                        description:='move if not parity/move if parity odd';
                        DisassembleData.opcode:='cmovnp';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $4c : begin
                        description:='move if less';
                        DisassembleData.opcode:='cmovl';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $4d : begin
                        description:='move if greater or equal';
                        DisassembleData.opcode:='cmovge';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $4e : begin
                        description:='move if less or equal';
                        DisassembleData.opcode:='cmovle';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);


                        inc(offset,last-1);
                      end;

                $4f : begin
                        description:='move if greater';
                        DisassembleData.opcode:='cmovg';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                        inc(offset,last-1);
                      end;

                $50 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmovmskpd'
                          else
                            DisassembleData.opcode:='movmskpd';

                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          description:='extract packed double-precision floating-point sign mask';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmovmskps'
                          else
                            DisassembleData.opcode:='movmskps';

                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='move mask to integer';
                          inc(offset,last-1);
                        end;
                      end;

                $51 : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsqrtsd'
                          else
                            DisassembleData.opcode:='sqrtsd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          description:='scalar double-fp square root';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsqrtss'
                          else
                            DisassembleData.opcode:='sqrtss';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          description:='scalar single-fp square root';
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsqrtpd'
                          else
                            DisassembleData.opcode:='sqrtpd';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          description:='packed double-fp square root';

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsqrtps'
                          else
                            DisassembleData.opcode:='sqrtps';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          description:='packed single-fp square root';

                          inc(offset,last-1);
                        end;
                      end;

                $52 : begin
                        DisassembleData.isfloat:=true;
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vrsqrtss'
                          else
                            DisassembleData.opcode:='rsqrtss';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='packed single-fp square root reciprocal';
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vrsqrtps'
                          else
                            DisassembleData.opcode:='rsqrtps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='scalar single-fp square root reciprocal';
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $53 : begin
                        DisassembleData.isfloat:=true;
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vrcpss'
                          else
                            DisassembleData.opcode:='rcpss';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='Compute Reciprocal of Scalar Single-Precision Floating-Point Values';
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vrcpps'
                          else
                            DisassembleData.opcode:='rcpps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='Compute Reciprocals of Packed Single-Precision Floating-Point Values';
                          DisassembleData.datasize:=4;
                          inc(offset,last-1);
                        end;
                      end;

                $54 : begin
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vandpd'
                          else
                            DisassembleData.opcode:='andpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='bit-wise logical and of xmm2/m128 and xmm1';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vandps'
                          else
                            DisassembleData.opcode:='andps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='bit-wise logical and for single fp';
                          inc(offset,last-1);
                        end;
                      end;

                $55 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical and not of packed double-precision fp values';
                          if hasvex then
                            DisassembleData.opcode:='vandnpd'
                          else
                            DisassembleData.opcode:='andnpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical and not for single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vandnps'
                          else
                            DisassembleData.opcode:='andnps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          inc(offset,last-1);
                        end;
                      end;

                $56 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical or of double-fp';
                          if hasvex then
                            DisassembleData.opcode:='vorpd'
                          else
                            DisassembleData.opcode:='orpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical or for single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vorps'
                          else
                            DisassembleData.opcode:='orps';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          inc(offset,last-1);
                        end;
                      end;

                $57 : begin
                        if $66 in prefix2 then
                        begin
                          description:='bit-wise logical xor for double-fp data';
                          if hasvex then
                            DisassembleData.opcode:='vxorpd'
                          else
                            DisassembleData.opcode:='xorpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);


                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bit-wise logical xor for single-fp data';
                          if hasvex then
                            DisassembleData.opcode:='vxorps'
                          else
                            DisassembleData.opcode:='xorps';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          inc(offset,last-1);
                        end;
                      end;

                $58 : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          //delete the repne from the tempresult
                          if hasvex then
                            DisassembleData.opcode:='vaddsd'
                          else
                            DisassembleData.opcode:='addsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,64,0,mRight);

                          description:='add the lower sp fp number from xmm2/mem to xmm1.';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          //delete the repe from the tempresult

                          if hasvex then
                            DisassembleData.opcode:='vaddss'
                          else
                            DisassembleData.opcode:='addss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,32,0,mRight);
                          DisassembleData.datasize:=4;

                          description:='add the lower sp fp number from xmm2/mem to xmm1.';
                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vaddpd'
                            else
                              DisassembleData.opcode:='addpd';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                            description:='add packed double-precision floating-point values from xmm2/mem to xmm1';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vaddps'
                            else
                              DisassembleData.opcode:='addps';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                            DisassembleData.datasize:=4;

                            description:='add packed sp fp numbers from xmm2/mem to xmm1';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $59 : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin

                          if hasvex then
                            DisassembleData.opcode:='vmulsd'
                          else
                            DisassembleData.opcode:='mulsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='scalar double-fp multiply';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          if hasvex then
                            DisassembleData.opcode:='vmulss'
                          else
                            DisassembleData.opcode:='mulss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='scalar single-fp multiply';
                          inc(offset,last-1);
                        end else
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmulpd'
                          else
                            DisassembleData.opcode:='mulpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='packed double-fp multiply';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vmulps'
                          else
                            DisassembleData.opcode:='mulps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='packed single-fp multiply';
                          inc(offset,last-1);
                        end;
                      end;

                $5a : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin

                          if hasvex then
                            DisassembleData.opcode:='vcvtsd2ss'
                          else
                            DisassembleData.opcode:='cvtsd2ss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                          description:='convert scalar double-precision floating-point value to scalar single-precision floating-point value';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          if hasvex then
                            DisassembleData.opcode:='cvtss2sd'
                          else
                            DisassembleData.opcode:='cvtss2sd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='convert scalar single-precision floating-point value to scalar double-precision floating-point value';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            if hasvex then
                              DisassembleData.opcode:='cvtpd2ps'
                            else
                              DisassembleData.opcode:='cvtpd2ps';
                            opcodeflags.skipExtraReg:=true;

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                            description:='convert packed double precision fp values to packed single precision fp values';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            if hasvex then
                              DisassembleData.opcode:='cvtps2pd'
                            else
                              DisassembleData.opcode:='vcvtps2pd';

                            opcodeflags.skipExtraReg:=true;
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                            DisassembleData.datasize:=4;

                            description:='convert packed single precision fp values to packed double precision fp values';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5b : begin

                        if $66 in prefix2 then
                        begin
                          DisassembleData.isfloat:=true;
                          if hasvex then
                            DisassembleData.opcode:='vcvtps2dq'
                          else
                            DisassembleData.opcode:='cvtps2dq';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='convert ps-precision fpoint values to packed dword''s ';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vcvttps2dq'
                          else
                            DisassembleData.opcode:='cvttps2dq';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='Convert with Truncation Packed Single-Precision FP Values to Packed Dword Integers';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vcvtdq2ps'
                          else
                            DisassembleData.opcode:='cvtdq2ps';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='convert packed dword''s to ps-precision fpoint values';
                          inc(offset,last-1);
                        end;
                      end;

                $5c : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsubsd'
                          else
                            DisassembleData.opcode:='subsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='scalar double-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsubss'
                          else
                            DisassembleData.opcode:='subss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='scalar single-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsubpd'
                          else
                            DisassembleData.opcode:='subpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='packed double-fp subtract';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vsubps'
                          else
                            DisassembleData.opcode:='subps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4; //4*4 actually

                          description:='packed single-fp subtract';
                          inc(offset,last-1);
                        end;
                      end;


                $5d : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vminsd'
                          else
                            DisassembleData.opcode:='minsd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='scalar single-fp minimum';
                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vminss'
                          else
                            DisassembleData.opcode:='minss';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='scalar single-fp minimum';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vminpd'
                            else
                              DisassembleData.opcode:='minpd';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                            description:='packed double-fp minimum';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vminps'
                            else
                              DisassembleData.opcode:='minps';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                            description:='packed single-fp minimum';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5e : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='divsd'
                          else
                            DisassembleData.opcode:='divsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          description:='scalar double-precision-fp divide';
                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          if hasvex then
                            DisassembleData.opcode:='vdivss'
                          else
                            DisassembleData.opcode:='divss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          description:='scalar single-fp divide';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vdivpd'
                            else
                              DisassembleData.opcode:='divpd';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                            description:='packed double-precision fp divide';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            if hasvex then
                              DisassembleData.opcode:='vdivps'
                            else
                              DisassembleData.opcode:='divps';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                            DisassembleData.datasize:=4;

                            description:='packed single-fp divide';
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $5f : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin

                          description:='scalar double-fp maximum';
                          if hasvex then
                            DisassembleData.opcode:='vmaxsd'
                          else
                            DisassembleData.opcode:='maxsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end else
                        if $f3 in prefix2 then
                        begin

                          description:='scalar single-fp maximum';
                          if hasvex then
                            DisassembleData.opcode:='vmaxss'
                          else
                            DisassembleData.opcode:='maxss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.datasize:=4;

                          inc(offset,last-1);
                        end else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='packed double-fp maximum';
                            if hasvex then
                              DisassembleData.opcode:='vmaxpd'
                            else
                              DisassembleData.opcode:='maxpd';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='packed single-fp maximum';
                            if hasvex then
                              DisassembleData.opcode:='vmaxps'
                            else
                              DisassembleData.opcode:='maxps';

                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                            DisassembleData.datasize:=4;

                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $60 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          if hasvex then
                            DisassembleData.opcode:='vpunpcklbw'
                          else
                            DisassembleData.opcode:='punpcklbw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          DisassembleData.opcode:='punpcklbw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $61 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          if hasvex then
                            DisassembleData.opcode:='punpcklwd'
                          else
                            DisassembleData.opcode:='punpcklwd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          DisassembleData.opcode:='punpcklwd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $62 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          if hasvex then
                            DisassembleData.opcode:='vpunpckldq'
                          else
                            DisassembleData.opcode:='punpckldq';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack low packed data';
                          DisassembleData.opcode:='punpckldq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $63 : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with signed saturation';
                          if hasvex then
                            DisassembleData.opcode:='packsswb'
                          else
                            DisassembleData.opcode:='vpacksswb';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with signed saturation';
                          DisassembleData.opcode:='packsswb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $64 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpgtb'
                          else
                            DisassembleData.opcode:='pcmpgtb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          DisassembleData.opcode:='pcmpgtb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $65 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpgtw'
                          else
                            DisassembleData.opcode:='pcmpgtw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          DisassembleData.opcode:='pcmpgtw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $66 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for greater than';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpgtd'
                          else
                            DisassembleData.opcode:='pcmpgtd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for greater than';
                          DisassembleData.opcode:='pcmpgtd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;


                $67 : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with unsigned saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpackuswb'
                          else
                            DisassembleData.opcode:='packuswb';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with unsigned saturation';
                          DisassembleData.opcode:='packuswb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $68 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          if hasvex then
                            DisassembleData.opcode:='vpunpckhbw'
                          else
                            DisassembleData.opcode:='punpckhbw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          DisassembleData.opcode:='punpckhbw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $69 : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          if hasvex then
                            DisassembleData.opcode:='vpunpckhwd'
                          else
                            DisassembleData.opcode:='punpckhwd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          DisassembleData.opcode:='punpckhwd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $6a : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          if hasvex then
                            DisassembleData.opcode:='vpunpckhdq'
                          else
                            DisassembleData.opcode:='punpckhdq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='unpack high packed data';
                          DisassembleData.opcode:='punpckhdq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $6b : begin
                        if $66 in prefix2 then
                        begin
                          description:='pack with signed saturation';
                          if hasvex then
                            DisassembleData.opcode:='packssdw'
                          else
                            DisassembleData.opcode:='packssdw';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='pack with signed saturation';
                          DisassembleData.opcode:='packssdw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $6c : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack low packed data';
                          DisassembleData.opcode:='punpcklqdq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                      end;

                $6d : begin
                        if $66 in prefix2 then
                        begin
                          description:='unpack high packed data';
                          DisassembleData.opcode:='punpckhqdq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                      end;


                $6e : begin
                        //DisassembleData.isfloat:=true; //not sure
                        if rex_w then
                        begin
                          description:='move quadword';
                          DisassembleData.opcode:='movq';
                        end
                        else
                        begin
                          description:='move doubleword';
                          DisassembleData.opcode:='movd';
                        end;

                        if hasvex then
                          DisassembleData.opcode:='v'+DisassembleData.opcode;

                        opcodeflags.skipExtraReg:=true;
                        if $66 in prefix2 then
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight)
                        else
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $6f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='move unaligned double quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmovdqu'
                          else
                            DisassembleData.opcode:='movdqu';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move aligned double quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmovdqa'
                          else
                            DisassembleData.opcode:='movdqa';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move 64 bits';
                          DisassembleData.opcode:='movq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last);

                          inc(offset,last-1);
                        end;
                      end;

                $70 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='shuffle packed low words';
                          if hasvex then
                            DisassembleData.opcode:='vpshuflw'
                          else
                            DisassembleData.opcode:='pshuflw';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='shuffle packed high words';
                          if hasvex then
                            DisassembleData.opcode:='vpshufhw'
                          else
                            DisassembleData.opcode:='pshufhw';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='packed shuffle doubleword';
                          DisassembleData.opcode:='pshufd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='packed shuffle word';
                          if hasvex then
                            DisassembleData.opcode:='vpshufw'
                          else
                            DisassembleData.opcode:='pshufw';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end;
                      end;

                $71 : begin
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[3];
                        DisassembleData.seperators[DisassembleData.seperatorcount]:=3;
                        inc(DisassembleData.seperatorcount);


                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsrlw'
                                  else
                                    DisassembleData.opcode:='psrlw';

                                  DisassembleData.parameters:=xmm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  DisassembleData.opcode:='psrlw';
                                  DisassembleData.parameters:=mm(memory[2])+','+inttohexs(memory[3],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='shift packed data right arithmetic';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsraw'
                                  else
                                    DisassembleData.opcode:='psraw';

                                  DisassembleData.parameters:=modrm(memory,prefix2,2,4,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,last-1+1);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  DisassembleData.opcode:='psraw';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,last-1+1);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsllw'
                                  else
                                    DisassembleData.opcode:='psllw';

                                  DisassembleData.parameters:=modrm(memory,prefix2,2,4,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,last-1+1);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  DisassembleData.opcode:='psllw';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,last-1+1);
                                end;
                              end;
                        end;
                      end;

                $72 : begin
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[3];
                        DisassembleData.seperators[DisassembleData.seperatorcount]:=3;
                        inc(DisassembleData.seperatorcount);

                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsrld'
                                  else
                                    DisassembleData.opcode:='psrld';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  DisassembleData.opcode:='psrld';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;

                          4 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right arithmetic';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsrad'
                                  else
                                    DisassembleData.opcode:='psrad';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right arithmetic';
                                  DisassembleData.opcode:='psrad';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  if hasvex then
                                    DisassembleData.opcode:='pslld'
                                  else
                                    DisassembleData.opcode:='pslld';

                                  DisassembleData.parameters:=modrm(memory,prefix2,2,4,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+','+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  DisassembleData.opcode:='pslld';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;

                $73 : begin
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[3];
                        DisassembleData.seperators[DisassembleData.seperatorcount]:=3;
                        inc(DisassembleData.seperatorcount);

                        case getreg(memory[2]) of
                          2 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift right logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsrlq'
                                  else
                                    DisassembleData.opcode:='psrlq';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift right logical';
                                  DisassembleData.opcode:='psrlq';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;

                          3 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='shift double quadword right logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsrldq'
                                  else
                                    DisassembleData.opcode:='psrldq';

                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;

                          6 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='packed shift left logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpsllq'
                                  else
                                    DisassembleData.opcode:='psllq';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end
                                else
                                begin
                                  description:='packed shift left logical';
                                  DisassembleData.opcode:='psllq';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,3,last);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;

                          7 : begin
                                if $66 in prefix2 then
                                begin
                                  description:='shift double quadword left logical';
                                  if hasvex then
                                    DisassembleData.opcode:='vpslldq'
                                  else
                                    DisassembleData.opcode:='pslldq';
                                  DisassembleData.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                  DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                  inc(offset,3);
                                end;
                              end;
                        end;
                      end;



                $74 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpeqb'
                          else
                            DisassembleData.opcode:='pcmpeqb';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          DisassembleData.opcode:='pcmpeqb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $75 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpeqw'
                          else
                            DisassembleData.opcode:='pcmpeqw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          DisassembleData.opcode:='pcmpeqw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $76 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed compare for equal';
                          if hasvex then
                            DisassembleData.opcode:='vpcmpeqd'
                          else
                            DisassembleData.opcode:='pcmpeqd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed compare for equal';
                          DisassembleData.opcode:='pcmpeqd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;


                $77 : begin
                        if hasvex then
                        begin
                          if opcodeflags.L then
                          begin
                            description:='Zero all YMM registers';
                            DisassembleData.opcode:='vzeroall';
                            inc(offset);
                          end
                          else
                          begin
                            description:='Zero upper bits of YMM registers';
                            DisassembleData.opcode:='vzeroupper';
                            inc(offset);
                          end;
                        end
                        else
                        begin
                          description:='empty mmx™ state';
                          DisassembleData.opcode:='emms';
                          inc(offset);
                        end;
                      end;

                $78 : begin
                        description:='reads a specified vmcs field (32 bits)';
                        DisassembleData.opcode:='vmread';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $79 : begin
                        description:='writes a specified vmcs field (32 bits)';
                        DisassembleData.opcode:='vmwrite';
                        DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $7c : begin
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vhaddpd'
                          else
                            DisassembleData.opcode:='haddpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                          description:='packed double-fp horizontal add';
                          inc(offset,last-1);
                        end
                        else
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vhaddps'
                          else
                            DisassembleData.opcode:='haddps';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                          description:='packed single-fp horizontal add';
                          inc(offset,last-1);
                        end;
                      end;

                $7d : begin
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vhsubpd'
                          else
                            DisassembleData.opcode:='hsubpd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                          description:='packed double-fp horizontal subtract';
                          inc(offset,last-1);
                        end
                        else
                        if $f2 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='vhsubps'
                          else
                            DisassembleData.opcode:='hsubps';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                          description:='packed single-fp horizontal subtract';
                          inc(offset,last-1);
                        end;
                      end;

                $7e : begin

                        if $f3 in prefix2 then
                        begin

                          description:='move quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmovq'
                          else
                            DisassembleData.opcode:='movq';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          if Rex_W then
                          begin
                            description:='move 64 bits';
                            DisassembleData.opcode:='movq';
                          end
                          else
                          begin
                            description:='move 32 bits';
                            DisassembleData.opcode:='movd';
                          end;

                          if hasvex then
                            DisassembleData.opcode:='v'+DisassembleData.opcode;

                          opcodeflags.skipExtraReg:=true;

                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+xmm(memory[2]);  //r32/rm32,xmm
                          inc(offset,last-1);
                        end
                        else
                        begin
                          if Rex_W then
                          begin
                            description:='move 64 bits';
                            DisassembleData.opcode:='movq';
                          end
                          else
                          begin
                            description:='move 32 bits';
                            DisassembleData.opcode:='movd';
                          end;


                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+mm(memory[2]); //r32/rm32,mm
                          inc(offset,last-1);
                        end;
                      end;

                $7f : begin
                        if $f3 in prefix2 then
                        begin

                          description:='move unaligned double quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmovdqu'
                          else
                            DisassembleData.opcode:='movdqu';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move aligned double quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmovdqa'
                          else
                            DisassembleData.opcode:='movdqa';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move 64 bits';
                          DisassembleData.opcode:='movq';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;
                      end;

                $80 : begin
                        description:='jump near if overflow';
                        DisassembleData.opcode:='jo';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $81 : begin
                        description:='jump near if not overflow';
                        DisassembleData.opcode:='jno';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)=0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

                      end;

                $82 : begin
                        description:='jump near if below/carry';

                        DisassembleData.opcode:='jb';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;

                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

                      end;

                $83 : begin
                        description:='jump near if above or equal';
                        DisassembleData.opcode:='jae';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)=0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $84 : begin
                        description:='jump near if equal';

                        DisassembleData.opcode:='je';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;


                $85 : begin
                        description:='jump near if not equal';
                        DisassembleData.opcode:='jne';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

                      end;

                $86 : begin
                        description:='jump near if below or equal';
                        DisassembleData.opcode:='jbe';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;


                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $87 : begin
                        description:='jump near if above';
                        DisassembleData.opcode:='ja';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                             DisassembleData.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))=0;


                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $88 : begin
                        description:='jump near if sign';
                        DisassembleData.opcode:='js';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $89 : begin
                        description:='jump near if not sign';
                        DisassembleData.opcode:='jns';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8a : begin
                        description:='jump near if parity';
                        DisassembleData.opcode:='jp';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)<>0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8b : begin
                        description:='jump near if not parity';
                        DisassembleData.opcode:='jnp';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)=0;

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8c : begin
                        description:='jump near if less';
                        DisassembleData.opcode:='jl';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF);

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8d : begin
                        description:='jump near if not less';
                        DisassembleData.opcode:='jnl';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF);

                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8e : begin
                        description:='jump near if not greater';
                        DisassembleData.opcode:='jng';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF)) or ((context^.EFlags and EFLAGS_ZF)<>0);


                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $8f : begin
                        description:='jump near if greater';
                        DisassembleData.opcode:='jg';
                        DisassembleData.isjump:=true;
                        DisassembleData.isconditionaljump:=true;
                        if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF)) and ((context^.EFlags and EFLAGS_ZF)=0);


                        inc(offset,1+4);
                        if MarkIPRelativeInstructions then
                        begin
                          DisassembleData.riprelative:=2;
                          riprelative:=true;
                        end;

                        DisassembleData.parametervaluetype:=dvtaddress;
                        if is64bit then
                          DisassembleData.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                        else
                          DisassembleData.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                        DisassembleData.seperators[DisassembleData.seperatorcount]:=2;
                        inc(DisassembleData.seperatorcount);



                        DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
                      end;

                $90 : begin
                        description:='set byte if overflow';
                        DisassembleData.opcode:='seto';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $91 : begin
                        description:='set byte if not overfloww';
                        DisassembleData.opcode:='setno';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $92 : begin
                        description:='set byte if below/carry';
                        DisassembleData.opcode:='setb';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $93 : begin
                        description:='set byte if above or equal';
                        DisassembleData.opcode:='setae';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $94 : begin
                        description:='set byte if equal';
                        DisassembleData.opcode:='sete';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $95 : begin
                        description:='set byte if not equal';
                        DisassembleData.opcode:='setne';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $96 : begin
                        description:='set byte if below or equal';
                        DisassembleData.opcode:='setbe';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $97 : begin
                        description:='set byte if above';
                        DisassembleData.opcode:='seta';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $98 : begin
                        description:='set byte if sign';
                        DisassembleData.opcode:='sets';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $99 : begin
                        description:='set byte if not sign';
                        DisassembleData.opcode:='setns';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $9a : begin
                        description:='set byte if parity';
                        DisassembleData.opcode:='setp';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $9b : begin
                        description:='set byte if not parity';
                        DisassembleData.opcode:='setnp';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $9c : begin
                        description:='set byte if less';
                        DisassembleData.opcode:='setl';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);

                        inc(offset,last-1);
                      end;

                $9d : begin
                        description:='set byte if greater or equal';
                        DisassembleData.opcode:='setge';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);
                        inc(offset,last-1);

                      end;

                $9e : begin
                        description:='set byte if less or equal';
                        DisassembleData.opcode:='setle';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);
                        inc(offset,last-1);

                      end;

                $9f : begin
                        description:='set byte if greater';
                        DisassembleData.opcode:='setg';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last,8);
                        inc(offset,last-1);


                      end;

                $a0 : begin
                        description:='push word or doubleword onto the stack';
                        DisassembleData.opcode:='push';
                        DisassembleData.parameters:='fs';
                        inc(offset);
                      end;

                $a1 : begin
                        description:='pop a value from the stack';
                        DisassembleData.opcode:='pop';
                        DisassembleData.parameters:='fs';
                        inc(offset);
                      end;


                $a2 : begin
                        description:='cpu identification';
                        DisassembleData.opcode:='cpuid';
                        inc(offset);
                      end;

                $a3 : begin
                        description:='bit test';
                        DisassembleData.opcode:='bt';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);

                        inc(offset,last-1);
                      end;

                $a4 : begin
                        description:='double precision shift left';
                        DisassembleData.opcode:='shld';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);

                        DisassembleData.parameters:=DisassembleData.parameters+','+inttohex(memory[last],2);
                        inc(last);
                        inc(offset,last-1);

                      end;

                $a5 : begin
                        description:='double precision shift left';
                        DisassembleData.opcode:='shld';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2])+','+colorreg+'cl'+endcolor else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2])+','+colorreg+'cl'+endcolor;
                        inc(offset,last-1);

                      end;

                $a8 : begin
                        description:='push word or doubleword onto the stack';
                        DisassembleData.opcode:='push';
                        DisassembleData.parameters:='gs';
                        inc(offset);
                      end;

                $a9 : begin
                        description:='pop a value from the stack';
                        DisassembleData.opcode:='pop';
                        DisassembleData.parameters:='gs';
                        inc(offset);
                      end;

                $aa : begin
                        description:='resume from system management mode';
                        DisassembleData.opcode:='rsm';
                        inc(offset);
                      end;

                $ab : begin
                        description:='bit test and set';
                        DisassembleData.opcode:='bts';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $ac : begin
                        description:='double precision shift right';
                        DisassembleData.opcode:='shrd';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);

                        DisassembleData.parameters:=DisassembleData.parameters+','+inttohex(memory[last],2);
                        inc(last);
                        inc(offset,last-1);
                      end;

                $ad : begin
                        description:='double precision shift right';
                        DisassembleData.opcode:='shrd';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2])+','+colorreg+'cl'+endcolor else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2])+','+colorreg+'cl'+endcolor;
                        inc(offset,last-1);

                      end;

                $ae : begin
                        case memory[2] of
                          $f0: begin
                                 description:='memory fence';
                                 DisassembleData.opcode:='mfence';
                                 inc(offset,1);
                               end;

                          $f8: begin
                                 description:='store fence';
                                 DisassembleData.opcode:='sfence';
                                 inc(offset,1);
                               end;

                          else
                          case getreg(memory[2]) of
                          0:  begin
                                if $f3 in prefix2 then
                                begin
                                  description:='read fs base address';
                                  DisassembleData.opcode:='rdfsbase';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='store fp and mmx state and streaming simd extension state';
                                  DisassembleData.opcode:='fxsave';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end;
                              end;

                          1:  begin
                                if $f3 in prefix2 then
                                begin
                                  description:='read gs base address';
                                  DisassembleData.opcode:='rdgsbase';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='restore fp and mmx state and streaming simd extension state';
                                  DisassembleData.opcode:='fxrstor';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end;
                              end;

                          2:  begin
                                if $f3 in prefix2 then
                                begin
                                  description:='write fs base address';
                                  DisassembleData.opcode:='wrfsbase';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='load streaming simd extension control/status';
                                  if hasvex then
                                    DisassembleData.opcode:='vldmxcsr'
                                  else
                                    DisassembleData.opcode:='ldmxcsr';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end;
                              end;

                          3:  begin
                                if $f3 in prefix2 then
                                begin
                                  description:='write gs base address';
                                  DisassembleData.opcode:='wrgsbase';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='store streaming simd extension control/status';
                                  if hasvex then
                                    DisassembleData.opcode:='stmxcsr'
                                  else
                                    DisassembleData.opcode:='stmxcsr';

                                  opcodeflags.skipExtraReg:=true;
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end;
                              end;

                          4:  begin
                                description:='save processor extended state';
                                if Rex_W then
                                  DisassembleData.opcode:='xsave64'
                                else
                                  DisassembleData.opcode:='xsave';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          5:  begin
                                if getmod(memory[2])=3 then
                                begin
                                  description:='Load Fence';
                                  DisassembleData.opcode:='lfence';
                                  inc(offset,2);
                                end
                                else
                                begin
                                  description:='restore processor extended state';
                                  if Rex_W then
                                    DisassembleData.opcode:='xrstor64'
                                  else
                                    DisassembleData.opcode:='xrstor';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                  inc(offset,last-1);
                                end;
                              end;

                          6:  begin
                                description:='save processor extended status optimized';
                                if Rex_W then
                                  DisassembleData.opcode:='xsaveopt64'
                                else
                                  DisassembleData.opcode:='xsaveopt';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          7:  begin

                              end;

                        end;

                        end;



                      end;

                $af : begin
                        description:='signed multiply';
                        DisassembleData.opcode:='imul';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $b0 : begin
                        description:='compare and exchange';
                        DisassembleData.opcode:='cmpxchg';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $b1 : begin
                        description:='compare and exchange';
                        DisassembleData.opcode:='cmpxchg';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $b2 : begin
                        description:='load far pointer';
                        DisassembleData.opcode:='lss';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $b3 : begin
                        description:='bit test and reset';
                        DisassembleData.opcode:='btr';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;

                $b4 : begin
                        description:='load far pointer';
                        DisassembleData.opcode:='lfs';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $b5 : begin
                        description:='load far pointer';
                        DisassembleData.opcode:='lgs';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                        inc(offset,last-1);
                      end;

                $b6 : begin
                        description:='Move with zero-extend';
                        DisassembleData.opcode:='movzx';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight);


                        inc(offset,last-1);
                      end;

                $b7 : begin
                        description:='Move with zero-extend';
                        DisassembleData.opcode:='movzx';
                        if $66 in prefix2 then
                        DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight) else
                        DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight);


                        inc(offset,last-1);
                      end;

                $b8 : begin
                        if $f3 in prefix2 then
                        begin
                          description:='Return the Count of Number of Bits Set to 1';
                          DisassembleData.opcode:='popcnt';
                          if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;


                $ba : begin
                        DisassembleData.parametervaluetype:=dvtvalue;


                        case getreg(memory[2]) of
                          4:  begin
                                //bt
                                description:='bit test';
                                DisassembleData.opcode:='bt';
                                if $66 in prefix2 then
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,1,last) else
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                DisassembleData.parametervalue:=memory[last];
                                DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                inc(offset,last-1+1);
                              end;

                          5:  begin
                                //bts
                                description:='bit test and set';
                                DisassembleData.opcode:='bts';
                                if $66 in prefix2 then
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,1,last) else
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                DisassembleData.parametervalue:=memory[last];
                                DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                                inc(offset,last-1+1);
                              end;

                          6:  begin
                                //btr
                                description:='bit test and reset';
                                DisassembleData.opcode:='btr';
                                if $66 in prefix2 then
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,1,last) else
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                DisassembleData.parametervalue:=memory[last];
                                DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                inc(offset,last-1+1);
                              end;

                          7:  begin
                                //btc
                                description:='bit test and complement';
                                DisassembleData.opcode:='btc';
                                if $66 in prefix2 then
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,1,last) else
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                DisassembleData.parametervalue:=memory[last];
                                DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);

                                inc(offset,last-1+1);
                              end;

                        end;

                      end;

                $bb : begin
                        description:='bit test and complement';
                        DisassembleData.opcode:='btc';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);

                      end;


                $bc : begin
                        if $f3 in prefix2 then
                        begin
                          description:='count the number of trailing zero bits';
                          DisassembleData.opcode:='tzcnt';
                          if $66 in prefix2 then
                            DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                            DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          //bsf
                          description:='bit scan forward';
                          DisassembleData.opcode:='bsf';
                          if $66 in prefix2 then
                            DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                            DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);


                          inc(offset,last-1);
                        end;
                      end;

                $bd : begin
                        if $f3 in prefix2 then
                        begin
                          description:='count the number of leading zero bits';
                          DisassembleData.opcode:='lzcnt';
                          if $66 in prefix2 then
                            DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                            DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          //bsf
                          description:='bit scan reverse';
                          DisassembleData.opcode:='bsr';
                          if $66 in prefix2 then
                            DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                            DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);


                          inc(offset,last-1);
                        end;
                      end;

                $be : begin
                        description:='move with sign-extension';
                        DisassembleData.opcode:='movsx';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=r16(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight) else
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight);



                        inc(offset,last-1);
                      end;

                $bf : begin
                        description:='move with sign-extension';
                        DisassembleData.opcode:='movsx';
                        DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight);

                        inc(offset,last-1);
                      end;

                $c0 : begin
                        description:='exchange and add';
                        DisassembleData.opcode:='xadd';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,2,last)+r8(memory[2]);
                        inc(offset,last-1);
                      end;

                $c1 : begin
                        description:='exchange and add';
                        DisassembleData.opcode:='xadd';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2]) else

                          DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last-1);
                      end;

                $c2 : begin
                        DisassembleData.isfloat:=true;
                        if $f2 in prefix2 then
                        begin

                          description:='compare scalar dpuble-precision floating-point values';
                          if hasvex then
                            DisassembleData.opcode:='vcmpsd'
                          else
                            DisassembleData.opcode:='cmpsd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);

                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        if $f3 in prefix2 then
                        begin
                          description:='packed single-fp compare';
                          if hasvex then
                            DisassembleData.opcode:='vcmpss'
                          else
                            DisassembleData.opcode:='cmpss';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          DisassembleData.datasize:=4;
                          inc(offset,last);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='compare packed double-precision floating-point values';
                            if hasvex then
                              DisassembleData.opcode:='vcmppd'
                            else
                              DisassembleData.opcode:='cmppd';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                            DisassembleData.parametervaluetype:=dvtvalue;
                            DisassembleData.parametervalue:=memory[last];
                            DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                            inc(offset,last);
                          end
                          else
                          begin
                            description:='packed single-fp compare';
                            if hasvex then
                              DisassembleData.opcode:='vcmpps'
                            else
                              DisassembleData.opcode:='cmpps';
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                            DisassembleData.parametervaluetype:=dvtvalue;
                            DisassembleData.parametervalue:=memory[last];
                            DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                            DisassembleData.datasize:=4;
                            inc(offset,last);
                          end;
                        end;
                      end;

                $c3 : begin
                        description:='store doubleword using non-temporal hint';
                        DisassembleData.opcode:='movnti';
                        DisassembleData.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                        inc(offset,last);
                      end;

                $c4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='insert word';
                          if hasvex then
                            DisassembleData.opcode:='vpinsrw'
                          else
                            DisassembleData.opcode:='pinsrw';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='insert word';
                          DisassembleData.opcode:='pinsrw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end;
                      end;

                $c5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='extract word';
                          DisassembleData.opcode:='pextrw';
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,3);
                        end
                        else
                        begin
                          description:='extract word';
                          DisassembleData.opcode:='pextrw';
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,3);
                        end;
                      end;

                $c6 : begin
                        DisassembleData.isfloat:=true;
                        if $66 in prefix2 then
                        begin
                          description:='shuffle double-fp';
                          if hasvex then
                            DisassembleData.opcode:='vshufpd'
                          else
                            DisassembleData.opcode:='shufpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+','+inttohexs(DisassembleData.parametervalue,2);
                          inc(offset,last);
                        end
                        else
                        begin
                          description:='shuffle single-fp';
                          if hasvex then
                            DisassembleData.opcode:='vshufps'
                          else
                            DisassembleData.opcode:='shufps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+','+inttohexs(DisassembleData.parametervalue,2);
                          DisassembleData.datasize:=4;
                          inc(offset,last);
                        end;
                      end;

                $c7 : begin
                        case getreg(memory[2]) of
                          1:  begin
                                description:='compare and exchange 8 bytes';
                                DisassembleData.opcode:='cmpxchg8b';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          3:  begin
                                description:='restore processor extended status supervisor';
                                if Rex_W then
                                  DisassembleData.opcode:='xrstors64'
                                else
                                  DisassembleData.opcode:='xrstors';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          4:  begin
                                description:='save processor extended state with compaction';
                                if Rex_W then
                                  DisassembleData.opcode:='xsavec'
                                else
                                  DisassembleData.opcode:='xsavec64';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;

                          5:  begin
                                description:='save processor extended state supervisor';
                                if Rex_W then
                                  DisassembleData.opcode:='xsaves'
                                else
                                  DisassembleData.opcode:='xsaves64';
                                DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                inc(offset,last-1);
                              end;


                          6:  begin
                                if $66 in prefix2 then
                                begin
                                  if getmod(memory[2])=3 then //reg
                                  begin
                                    description:='read random numer';
                                    DisassembleData.opcode:='rdrand';
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,1,last);
                                    inc(offset,last-1);
                                  end
                                  else
                                  begin
                                    description:='copy vmcs data to vmcs region in memory';
                                    DisassembleData.opcode:='vmclear';
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                    inc(offset,last-1);
                                  end;
                                end
                                else
                                if $f3 in prefix2 then
                                begin
                                  description:='enter vmx root operation';
                                  DisassembleData.opcode:='vmxon';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  //check if it's a memory or register access
                                  //if register it's rdrand else vmptrld
                                  if getmod(memory[2])=3 then //reg
                                  begin
                                    description:='read random numer';
                                    DisassembleData.opcode:='rdrand';
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end
                                  else
                                  begin
                                    description:='loads the current vmcs pointer from memory';
                                    DisassembleData.opcode:='vmptrld';
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                    inc(offset,last-1);
                                  end;


                                end;
                              end;

                          7:  begin
                                if getmod(memory[2])=3 then //reg
                                begin
                                  description:='read random SEED';
                                  DisassembleData.opcode:='rdseed';
                                  if $66 in prefix then
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,1,last)
                                  else
                                    DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end
                                else
                                begin
                                  description:='stores the current vmcs pointer into memory';
                                  DisassembleData.opcode:='vmptrst';
                                  DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);

                                  inc(offset,last-1);
                                end;
                              end;
                        end;

                      end;

                $c8..$cf : begin
                        //bswap
                        description:='byte swap';
                        DisassembleData.opcode:='bswap';
                        if $66 in prefix2 then
                          DisassembleData.parameters:=rd16(memory[1]-$c8)
                        else
                          DisassembleData.parameters:=rd(memory[1]-$c8);

                        inc(offset);
                      end;

                $d0 : begin
                        if $66 in prefix2 then
                        begin
                          description:='Packed Double-FP Add/Subtract';
                          if hasvex then
                            DisassembleData.opcode:='vaddsubpd'
                          else
                            DisassembleData.opcode:='addsubpd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        if $f2 in prefix2 then
                        begin
                          description:='Packed Single-FP Add/Subtract';
                          if hasvex then
                            DisassembleData.opcode:='vaddsubps'
                          else
                            DisassembleData.opcode:='addsubps';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $d1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          if hasvex then
                            DisassembleData.opcode:='vpsrlw'
                          else
                            DisassembleData.opcode:='psrlw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          DisassembleData.opcode:='psrlw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $d2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          if hasvex then
                            DisassembleData.opcode:='vpsrld'
                          else
                            DisassembleData.opcode:='psrld';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          DisassembleData.opcode:='psrld';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $d3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right logical';
                          if hasvex then
                            DisassembleData.opcode:='vpsrlq'
                          else
                            DisassembleData.opcode:='psrlq';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right logical';
                          DisassembleData.opcode:='psrlq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $d4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='add packed quadword integers';
                          if hasvex then
                            DisassembleData.opcode:='vpaddq'
                          else
                            DisassembleData.opcode:='paddq';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='add packed quadword integers';
                          DisassembleData.opcode:='paddq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;


                $d5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply low';
                          if hasvex then
                            DisassembleData.opcode:='vpmullw'
                          else
                            DisassembleData.opcode:='pmullw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply low';
                          DisassembleData.opcode:='pmullw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $d6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='move low quadword from xmm to mmx technology register';
                          DisassembleData.opcode:='movdq2q';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='move low quadword from xmm to mmx technology register';
                          DisassembleData.opcode:='movq2dq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $66 in prefix2 then
                        begin
                          description:='move low quadword from xmm to mmx technology register';
                          if hasvex then
                            DisassembleData.opcode:='vmovq'
                          else
                            DisassembleData.opcode:='movq';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move quadword from mmx technology to xmm register';
                          DisassembleData.opcode:='movq2dq';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+mm(memory[2]);
                          inc(offset,last-1);
                        end;

                      end;


                $d7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='move byte mask to integer';
                          if hasvex then
                            DisassembleData.opcode:='vpmovmskb'
                          else
                            DisassembleData.opcode:='pmovmskb';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='move byte mask to integer';
                          DisassembleData.opcode:='pmovmskb';
                          DisassembleData.parameters:=r32(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $d8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract unsigned with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpsubusb'
                          else
                            DisassembleData.opcode:='psubusb';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract unsigned with saturation';
                          DisassembleData.opcode:='psubusb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $d9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract unsigned with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpsubusw'
                          else
                            DisassembleData.opcode:='psubusw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract unsigned with saturation';
                          DisassembleData.opcode:='psubusw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $da : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed unsigned integer byte minimum';
                          if hasvex then
                            DisassembleData.opcode:='vpminub'
                          else
                            DisassembleData.opcode:='pminub';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed unsigned integer byte minimum';
                          DisassembleData.opcode:='pminub';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $db : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical and';
                          if hasvex then
                            DisassembleData.opcode:='vpand'
                          else
                            DisassembleData.opcode:='pand';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical and';
                          DisassembleData.opcode:='pand';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $dc : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add unsigned with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpaddusb'
                          else
                            DisassembleData.opcode:='paddusb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add unsigned with saturation';
                          DisassembleData.opcode:='paddusb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $dd : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add unsigned with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpaddusw'
                          else
                            DisassembleData.opcode:='paddusw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add unsigned with saturation';
                          DisassembleData.opcode:='paddusw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $de : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed unsigned integer byte maximum';
                          if hasvex then
                            DisassembleData.opcode:='vpmaxub'
                          else
                            DisassembleData.opcode:='pmaxub';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mLeft);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed unsigned integer byte maximum';
                          DisassembleData.opcode:='pmaxub';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $df : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical and not';
                          if hasvex then
                            DisassembleData.opcode:='vpandn'
                          else
                            DisassembleData.opcode:='pandn';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical and not';
                          DisassembleData.opcode:='pandn';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $e0 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed average';
                          if hasvex then
                            DisassembleData.opcode:='vpavgb'
                          else
                            DisassembleData.opcode:='pavgb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed average';
                          DisassembleData.opcode:='pavgb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $e1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift right arithmetic';
                          if hasvex then
                            DisassembleData.opcode:='vpsraw'
                          else
                            DisassembleData.opcode:='psraw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift right arithmetic';
                          DisassembleData.opcode:='psraw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $e2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          if hasvex then
                            DisassembleData.opcode:='vpsrad'
                          else
                            DisassembleData.opcode:='psrad';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          DisassembleData.opcode:='psrad';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $e3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed average';
                          if hasvex then
                            DisassembleData.opcode:='vpavgw'
                          else
                            DisassembleData.opcode:='pavgw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed average';
                          DisassembleData.opcode:='pavgw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $e4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply high unsigned';
                          if hasvex then
                            DisassembleData.opcode:='vpmulhuw'
                          else
                            DisassembleData.opcode:='pmulhuw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply high unsigned';
                          DisassembleData.opcode:='pmulhuw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $e5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply high';
                          if hasvex then
                            DisassembleData.opcode:='vpmulhw'
                          else
                            DisassembleData.opcode:='pmulhw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply high';
                          DisassembleData.opcode:='pmulhw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $e6 : begin
                        if $f2 in prefix2 then
                        begin

                          description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                          if hasvex then
                            DisassembleData.opcode:='vcvtpd2dq'
                          else
                            DisassembleData.opcode:='cvtpd2dq';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        if $f3 in prefix2 then
                        begin

                          description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                          if hasvex then
                            DisassembleData.opcode:='vcvtdq2pd'
                          else
                            DisassembleData.opcode:='cvtdq2pd';
                          DisassembleData.parameters:=xmm(memory[2])+',';
                          opcodeflags.L:=false;
                          DisassembleData.parameters:=DisassembleData.parameters+xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          if $66 in prefix2 then
                          begin
                            description:='convert with truncation packed double-precision floating-point values to packed doubleword integers';
                            if hasvex then
                              DisassembleData.opcode:='vcvttpd2dq'
                            else
                              DisassembleData.opcode:='cvttpd2dq';

                            opcodeflags.skipExtraReg:=true;
                            DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                            inc(offset,last-1);
                          end;
                        end;
                      end;

                $e7 : begin
                        if $66 in prefix2 then
                        begin
                          if hasvex then
                            DisassembleData.opcode:='movntdq'
                          else
                            DisassembleData.opcode:='vmovntdq';

                          DisassembleData.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                          description:='move double quadword using non-temporal hint';
                          inc(offset,last-1);
                        end
                        else
                        begin
                          DisassembleData.opcode:='movntq';
                          DisassembleData.parameters:=modrm(memory,prefix2,2,3,last)+mm(memory[2]);
                          description:='move 64 bits non temporal';
                          inc(offset,last-1);
                        end;
                      end;

                $e8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpsubsb'
                          else
                            DisassembleData.opcode:='psubsb';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract with saturation';
                          DisassembleData.opcode:='psubsb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $e9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpsubsw'
                          else
                            DisassembleData.opcode:='psubsw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract with saturation';
                          DisassembleData.opcode:='psubsw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $ea : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed signed integer word minimum';
                          if hasvex then
                            DisassembleData.opcode:='vpminsw'
                          else
                            DisassembleData.opcode:='pminsw';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed signed integer word minimum';
                          DisassembleData.opcode:='pminsw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $eb : begin
                        if $66 in prefix2 then
                        begin
                          description:='bitwise logical or';
                          if hasvex then
                            DisassembleData.opcode:='vpor'
                          else
                            DisassembleData.opcode:='por';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='bitwise logical or';
                          DisassembleData.opcode:='por';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $ec : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpaddsb'
                          else
                            DisassembleData.opcode:='paddsb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add with saturation';
                          DisassembleData.opcode:='paddsb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $ed : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add with saturation';
                          if hasvex then
                            DisassembleData.opcode:='vpaddsw'
                          else
                            DisassembleData.opcode:='paddsw';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add with saturation';
                          DisassembleData.opcode:='paddsw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $ee : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed signed integer word maximum';
                          if hasvex then
                            DisassembleData.opcode:='vpmaxsw'
                          else
                            DisassembleData.opcode:='pmaxsw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed signed integer word maximum';
                          DisassembleData.opcode:='pmaxsw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $ef : begin
                        if $66 in prefix2 then
                        begin
                          description:='logical exclusive or';
                          if hasvex then
                            DisassembleData.opcode:='vpxor'
                          else
                            DisassembleData.opcode:='pxor';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='logical exclusive or';
                          DisassembleData.opcode:='pxor';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $f0 : begin
                        if $f2 in prefix2 then
                        begin
                          description:='load unaligned integer 128 bits';
                          if hasvex then
                            DisassembleData.opcode:='vlddqu'
                          else
                            DisassembleData.opcode:='lddqu';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                          inc(offset);
                      end;


                $f1 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          DisassembleData.opcode:='psllw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          DisassembleData.opcode:='psllw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $f2 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          if hasvex then
                            DisassembleData.opcode:='vpslld'
                          else
                            DisassembleData.opcode:='pslld';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          DisassembleData.opcode:='pslld';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $f3 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed shift left logical';
                          if hasvex then
                            DisassembleData.opcode:='vpsllq'
                          else
                            DisassembleData.opcode:='psllq';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed shift left logical';
                          DisassembleData.opcode:='psllq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                          inc(offset,last-1);
                        end;
                      end;

                $f4 : begin
                        if $66 in prefix2 then
                        begin
                          description:='multiply packed unsigned doubleword integers';
                          if hasvex then
                            DisassembleData.opcode:='pmuludq'
                          else
                            DisassembleData.opcode:='vpmuludq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='multiply packed unsigned doubleword integers';
                          DisassembleData.opcode:='pmuludq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;


                $f5 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed multiply and add';
                          if hasvex then
                            DisassembleData.opcode:='vpmaddwd'
                          else
                            DisassembleData.opcode:='pmaddwd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed multiply and add';
                          DisassembleData.opcode:='pmaddwd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $f6 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed sum of absolute differences';
                          if hasvex then
                            DisassembleData.opcode:='vpsadbw'
                          else
                            DisassembleData.opcode:='psadbw';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed sum of absolute differences';
                          DisassembleData.opcode:='psadbw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $f7 : begin
                        if $66 in prefix2 then
                        begin
                          description:='store selected bytes of double quadword';
                          if hasvex then
                            DisassembleData.opcode:='vmaskmovdqu'
                          else
                            DisassembleData.opcode:='maskmovdqu';

                          opcodeflags.skipExtraReg:=true;
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='byte mask write';
                          DisassembleData.opcode:='maskmovq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $f8 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          if hasvex then
                            DisassembleData.opcode:='vpsubb'
                          else
                            DisassembleData.opcode:='psubb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          DisassembleData.opcode:='psubb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $f9 : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          if hasvex then
                            DisassembleData.opcode:='vpsubw'
                          else
                            DisassembleData.opcode:='psubw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          DisassembleData.opcode:='psubw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $fa : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          if hasvex then
                            DisassembleData.opcode:='vpsubd'
                          else
                            DisassembleData.opcode:='psubd';

                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          DisassembleData.opcode:='psubd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $fb : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed subtract';
                          DisassembleData.opcode:='psubq';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed subtract';
                          DisassembleData.opcode:='psubq';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $fc : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          if hasvex then
                            DisassembleData.opcode:='vpaddb'
                          else
                            DisassembleData.opcode:='paddb';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          DisassembleData.opcode:='paddb';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $fd : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          if hasvex then
                            DisassembleData.opcode:='vpaddw'
                          else
                            DisassembleData.opcode:='paddw';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          DisassembleData.opcode:='paddw';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;

                $fe : begin
                        if $66 in prefix2 then
                        begin
                          description:='packed add';
                          if hasvex then
                            DisassembleData.opcode:='vpaddd'
                          else
                            DisassembleData.opcode:='paddd';
                          DisassembleData.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                          inc(offset,last-1);
                        end
                        else
                        begin
                          description:='packed add';
                          DisassembleData.opcode:='paddd';
                          DisassembleData.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                          inc(offset,last-1);
                        end;
                      end;


                end;



            end;

//

//

      $10 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $11 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              if $66 in prefix2 then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $12 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,8,0,mRight);

              inc(offset,last-1);
            end;

      $13 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;

      $14 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(DisassembleData.parametervalue,2);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              inc(offset);
            end;

      $15 : begin
              description:='add with carry';
              DisassembleData.opcode:='adc';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(DisassembleData.parametervalue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8);
                inc(offset,4);
              end;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

            end;

      $16 : begin
              description:='place ss on the stack';
              DisassembleData.opcode:='push';
              DisassembleData.parameters:=colorreg+'ss'+endcolor;
            end;

      $17 : begin
              description:='remove ss from the stack';
              DisassembleData.opcode:='pop';
              DisassembleData.parameters:=colorreg+'ss'+endcolor;
            end;

      $18 : begin
              description:='integer subtraction with borrow';
              DisassembleData.opcode:='sbb';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $19 : begin
              description:='integer subtraction with borrow';
              DisassembleData.opcode:='sbb';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $1a : begin
              description:='integer subtraction with borrow';
              DisassembleData.opcode:='sbb';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,8,0,mRight);

              inc(offset,last-1);
            end;

      $1b : begin
              description:='integer subtraction with borrow';
              DisassembleData.opcode:='sbb';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);


              inc(offset,last-1);
            end;

      $1c : begin
              description:='integer subtraction with borrow';
              DisassembleData.opcode:='sbb';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);


              inc(offset);
            end;

      $1d : begin
              DisassembleData.opcode:='sbb';
              description:='integer subtraction with borrow';
              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];

                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8);

                inc(offset,4);
              end;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

            end;

      $1e : begin
              description:='place ds on the stack';
              DisassembleData.opcode:='push';
              DisassembleData.parameters:=colorreg+'ds'+endcolor;
            end;

      $1f : begin
              description:='remove ds from the stack';
              DisassembleData.opcode:='pop';
              DisassembleData.parameters:=colorreg+'ds'+endcolor;
            end;

      $20 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $21 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $22 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);
              inc(offset,last-1);
            end;

      $23 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;


      $24 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              inc(offset);
            end;

      $25 : begin
              description:='logical and';
              DisassembleData.opcode:='and';
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;
                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(DisassembleData.parametervalue,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(DisassembleData.parametervalue,8);
                inc(offset,4);
              end;
            end;

      $27 : begin
              description:='decimal adjust al after addition';
              DisassembleData.opcode:='daa';
            end;

      $28 : begin
              description:='subtract';
              DisassembleData.opcode:='sub';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $29 : begin
              description:='subtract';
              DisassembleData.opcode:='sub';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $2a : begin
              description:='subtract';
              DisassembleData.opcode:='sub';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

              inc(offset,last-1);
            end;

      $2b : begin
              description:='subtract';
              DisassembleData.opcode:='sub';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;

      $2c : begin
              description:='subtract';
              DisassembleData.opcode:='sub';

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);



              inc(offset);
            end;

      $2d : begin
              description:='subtract';
              DisassembleData.opcode:='sub';


              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $2f : begin
              description:='decimal adjust al after subtraction';
              DisassembleData.opcode:='das';
            end;

      $30 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $31 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $32 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

              inc(offset,last-1);
            end;

      $33 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;

      $34 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $35 : begin
              description:='logical exclusive or';
              DisassembleData.opcode:='xor';


              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;


      $37 : begin  //aaa
              DisassembleData.opcode:='aaa';
              description:='ascii adjust al after addition'
            end;

//---------
      $38 : begin//cmp
              description:='compare two operands';
              DisassembleData.opcode:='cmp';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $39 : begin
              description:='compare two operands';
              DisassembleData.opcode:='cmp';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);

            end;

      $3a : begin
              description:='compare two operands';
              DisassembleData.opcode:='cmp';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

              inc(offset,last-1);
            end;

      $3b : begin
              description:='compare two operands';
              DisassembleData.opcode:='cmp';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;

//---------

      $3c : begin
              description:='compare two operands';
              DisassembleData.opcode:='cmp';

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $3d : begin
              description:='compare two operands';
              DisassembleData.opcode:='cmp';
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;


                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(qword(integer(dwordptr^)),8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

            //prefix bytes need fixing
      $3f : begin  //aas
              DisassembleData.opcode:='aas';
              description:='ascii adjust al after subtraction';
            end;

      $40..$47 :
            begin
              description:='increment by 1';
              DisassembleData.opcode:='inc';
              if $66 in prefix2 then
                DisassembleData.parameters:=rd16(memory[0]-$40) else
                DisassembleData.parameters:=rd(memory[0]-$40);
            end;

      $48..$4f :
            begin
              description:='decrement by 1';
              DisassembleData.opcode:='dec';
              if $66 in prefix2 then
                DisassembleData.parameters:=rd16(memory[0]-$48) else
                DisassembleData.parameters:=rd(memory[0]-$48);
            end;

      $50..$57 :
            begin
              description:='push word or doubleword onto the stack';

              if is64bit then opcodeflags.w:=true;

              DisassembleData.opcode:='push';
              if $66 in prefix2 then
                DisassembleData.parameters:=rd16(memory[0]-$50) else
                DisassembleData.parameters:=rd(memory[0]-$50);
            end;

      $58..$5f :
            begin
              description:='pop a value from the stack';
              if is64bit then opcodeflags.w:=true; //so rd will pick the 64-bit version
              DisassembleData.opcode:='pop';
              if $66 in prefix2 then
                DisassembleData.parameters:=rd16(memory[0]-$58) else
                DisassembleData.parameters:=rd(memory[0]-$58);
            end;

      $60 : begin
              description:='push all general-purpose registers';
              if is64bit then description:=description+' (invalid)';
              if $66 in prefix2 then DisassembleData.opcode:='pusha' else
                                     DisassembleData.opcode:='pushad';

              if is64bit then
              begin
                description:=description+' (invalid)';
                DisassembleData.opcode:='pushad (invalid)';
              end;
            end;

      $61 : begin
              description:='pop all general-purpose registers';
              if $66 in prefix2 then DisassembleData.opcode:='popa' else
                                     DisassembleData.opcode:='popad';

              if is64bit then
              begin
                description:=description+' (invalid)';
                DisassembleData.opcode:='popad (invalid)';
              end;

            end;

      $62 : begin
              //bound
              description:='check array index against bounds';
              DisassembleData.opcode:='bound';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);

            end;

      $63 : begin
              //arpl or movsxd
              if is64bit then
              begin
                DisassembleData.opcode:='movsxd';
                opcodeflags.w:=false;

                DisassembleData.parameters:=' '+r64(memory[1])+modrm(memory,prefix2,1,0,last,32,0,mRight);
                inc(offset,last-1);
                description:='Move doubleword to quadword with signextension'
              end
              else
              begin
                DisassembleData.opcode:='arpl';
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]);
                inc(offset,last-1);
                description:='adjust rpl field of segment selector';
              end;
            end;

      $68 : begin
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.opcode:='push';
                DisassembleData.parameters:=inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                DisassembleData.opcode:='push';
                DisassembleData.parameters:=inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
              description:='push word or doubleword onto the stack (sign extended)';
            end;

      $69 : begin
              description:='signed multiply';
              if $66 in prefix2 then
              begin
                DisassembleData.opcode:='imul';
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight);
                wordptr:=@memory[last];

                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                inc(offset,last-1+2);
              end
              else
              begin
                DisassembleData.opcode:='imul';
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);
                dwordptr:=@memory[last];
                DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);

                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                inc(offset,last-1+4);
              end;
            end;

      $6a : begin
              DisassembleData.opcode:='push';

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];

              DisassembleData.parameters:=inttohexs(memory[1],2,true,1);
              inc(offset);
              description:='push byte onto the stack';
            end;

      $6b : begin

              description:='signed multiply';
              DisassembleData.opcode:='imul';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              DisassembleData.parametervalue:=memory[last];
              DisassembleData.parameters:=DisassembleData.parameters+','+inttohexs(memory[last],2);
              inc(offset,last-1+1);
            end;

      $6c : begin
              //m8, dx
              description:='input from port to string';
              DisassembleData.opcode:='insb';
            end;

      $6d : begin
              //m8, dx
              description:='input from port to string';
              if $66 in prefix2 then DisassembleData.opcode:='insw' else
                                     DisassembleData.opcode:='insd';
            end;

      $6e : begin
              //m8, dx
              description:='output string to port';
              DisassembleData.opcode:='outsb';
            end;

      $6f : begin
              //m8, dx
              description:='output string to port';
              if $66 in prefix2 then DisassembleData.opcode:='outsw' else
                                     DisassembleData.opcode:='outsd';
            end;


      $70 : begin
              description:='jump short if overflow';
              DisassembleData.opcode:='jo';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)<>0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);



            end;

      $71 : begin
              description:='jump short if not overflow';
              DisassembleData.opcode:='jno';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)=0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $72 : begin
              description:='jump short if below/carry';
              DisassembleData.opcode:='jb';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)<>0;
              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $73 : begin
              description:='jump short if above or equal';
              DisassembleData.opcode:='jae';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)=0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $74 : begin
              description:='jump short if equal';
              DisassembleData.opcode:='je';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));



              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $75 : begin
              description:='jump short if not equal';
              DisassembleData.opcode:='jne';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;
              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $76 : begin
              description:='jump short if not above';
              DisassembleData.opcode:='jna';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                DisassembleData.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))<>0;


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $77 : begin
              description:='jump short if above';
              DisassembleData.opcode:='ja';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                DisassembleData.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))=0;


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $78 : begin
              description:='jump short if sign';
              DisassembleData.opcode:='js';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $79 : begin
              description:='jump short if not sign';
              DisassembleData.opcode:='jns';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $7a : begin
              description:='jump short if parity';
              DisassembleData.opcode:='jp';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)<>0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $7b : begin
              description:='jump short if not parity';
              DisassembleData.opcode:='jnp';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)=0;

              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $7c : begin
              description:='jump short if not greater or equal';
              DisassembleData.opcode:='jl'; //jnge
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF);


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $7d : begin
              description:='jump short if not less (greater or equal)';
              DisassembleData.opcode:='jnl';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF);


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parametervaluetype:=dvtAddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

            end;

      $7e : begin
              description:='jump short if less or equal';
              DisassembleData.opcode:='jle';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF)) or ((context^.EFlags and EFLAGS_ZF)<>0);


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
              DisassembleData.parameterValueType:=dvtAddress;
            end;

      $7f : begin
              description:='jump short if greater';
              DisassembleData.opcode:='jg';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF)) and ((context^.EFlags and EFLAGS_ZF)=0);


              inc(offset);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(shortint(memory[1])))
              else
                DisassembleData.parametervalue:=dword(offset+qword(shortint(memory[1])));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
              DisassembleData.parameterValueType:=dvtAddress;
            end;

      $80,$82 : begin
              case getreg(memory[1]) of
                0:  begin
                      //add
                      DisassembleData.opcode:='add';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='add x to y';
                    end;

                1:  begin
                      //adc
                      DisassembleData.opcode:='or';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical inclusive or';
                    end;


                2:  begin
                      //adc
                      DisassembleData.opcode:='adc';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='add with carry';
                    end;

                3:  begin
                      //sbb
                      DisassembleData.opcode:='sbb';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='integer subtraction with borrow';
                    end;

                4:  begin
                      //and
                      DisassembleData.opcode:='and';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical and';
                    end;

                5:  begin
                      DisassembleData.opcode:='sub';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='subtract';
                    end;

                6:  begin
                      DisassembleData.opcode:='xor';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      offset:=offset+last;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      DisassembleData.opcode:='cmp';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
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
                        DisassembleData.opcode:='add';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='add';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;

//                      offset:=offset+last;
                      description:='add x to y';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='or';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='or';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='logical inclusive or';
                    end;

                2:  begin
                      //adc
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='adc';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='adc';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='add with carry';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='sbb';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='sbb';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='integer subtraction with borrow';
                    end;


                4:  begin
                      //and
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='and';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='and';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='logical and';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='sub';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='sub';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;


                      description:='subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='xor';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='xor';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
                        inc(offset,last-1+4);
                      end;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      //cmp
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='cmp';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last-1+2);
                      end else
                      begin
                        DisassembleData.opcode:='cmp';
                        if rex_w then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=dwordptr^;

                        if rex_w then
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(qword(integer(dwordptr^)),8)
                        else
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
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
                        DisassembleData.opcode:='add';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2, true);
                      end else
                      begin
                        DisassembleData.opcode:='add';

                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2, true)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];

                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2, true);
                        end;

                      end;

                      inc(offset,last);
                      description:='add (sign extended)';
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='or';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='or';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='add (sign extended)';
                    end;


                2:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='adc';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='adc';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      inc(offset,last);
                      description:='add with carry (sign extended)';
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='sbb';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='sbb';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      inc(offset,last);
                      description:='integer subtraction with borrow (sign extended)';
                    end;

                4:  begin
                      //and
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='and';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='and';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;

                      end;

                      offset:=offset+last;
                      description:='logical and (sign extended)';
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='sub';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='sub';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='subtract';
                    end;

                6:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='xor';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='xor';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='logical exclusive or';
                    end;

                7:  begin
                      //cmp
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='cmp';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      end else
                      begin
                        DisassembleData.opcode:='cmp';
                        if rex_w then
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2)
                        end
                        else
                        begin
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                          DisassembleData.parametervaluetype:=dvtvalue;
                          DisassembleData.parametervalue:=memory[last];
                          DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        end;
                      end;

                      offset:=offset+last;
                      description:='compare two operands';
                    end;


              end;
            end;

      $84 : begin
              description:='logical compare';
              DisassembleData.opcode:='test';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $85 : begin
              description:='logical compare';
              DisassembleData.opcode:='test';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $86 : begin
              description:='exchange memory with register';
              DisassembleData.opcode:='xchg';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $87 : begin
              description:='exchange memory with register';
              DisassembleData.opcode:='xchg';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $88 : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              DisassembleData.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
              inc(offset,last-1);
            end;

      $89 : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              if $66 in prefix2 then
                DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);
              inc(offset,last-1);
            end;

      $8a : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              DisassembleData.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

              inc(offset,last-1);
            end;

      $8b : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              if $66 in prefix2 then
                DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

              inc(offset,last-1);
            end;

      $8c : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)+sreg(memory[1]);
              inc(offset,last-1);
            end;

      $8d : begin
              description:='load effective address';
              DisassembleData.opcode:='lea';
              if $66 in prefix2 then
              begin
                if processhandler.is64Bit and ($67 in prefix2) then
                  DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,0,32,mRight)
                else
                  DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,0,0,mRight);
              end
              else
              begin
                if processhandler.is64Bit and ($67 in prefix2) then
                  DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,0,32,mRight)
                else
                  DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,0,0,mRight)
              end;

              inc(offset,last-1);
            end;

      $8e : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              DisassembleData.parameters:=sreg(memory[1])+modrm(memory,prefix2,1,1,last,mRight);

              inc(offset,last-1);
            end;

      $8f : begin
              case getreg(memory[1]) of
               0:  begin
                     description:='pop a value from the stack';
                     DisassembleData.opcode:='pop';
                     if $66 in prefix2 then
                       DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16) else
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     inc(offset,last-1);
                   end;

               else
               begin
                 DisassembleData.opcode:='db';
                 DisassembleData.parameters:=colorhex+'8f'+endcolor;
                 description:='undefined by the intel specification';
               end;
              end;
            end;


      $90 : begin
              description:='no operation';
              DisassembleData.opcode:='nop';
            end;

      $91..$97:
            begin
              description:='exchange register with register';
              DisassembleData.opcode:='xchg';

              if $66 in prefix2 then
                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+rd16(memory[0]-$90)
              else
              begin
                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+rd(memory[0]-$90)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+rd(memory[0]-$90);
              end;
            end;


      $98 : begin
              //cbw/cwde
              if $66 in prefix2 then
              begin
                DisassembleData.opcode:='cbw';
                description:='convert byte to word';
              end else
              begin
                if rex_w then
                begin
                  DisassembleData.opcode:='cdqe';
                  description:='convert doubleword to quadword';
                end
                else
                begin
                  DisassembleData.opcode:='cwde';
                  description:='convert word to doubleword';
                end;
              end;
            end;

      $99 : begin
              if $66 in prefix2 then
              begin
                description:='convert word to doubleword';
                DisassembleData.opcode:='cwd';
              end
              else
              begin

                if rex_w then
                begin
                  DisassembleData.opcode:='cqo';
                  description:='convert quadword to octword';
                end
                else
                begin
                  DisassembleData.opcode:='cdq';
                  description:='convert doubleword to quadword';
                end;
              end;
            end;

      $9a : begin
              description:='call procedure';
              wordptr:=@memory[5];

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=5;
              inc(DisassembleData.seperatorcount);



              if is64bit then
                DisassembleData.opcode:='call (invalid)'
              else
                DisassembleData.opcode:='call';


              DisassembleData.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;

              inc(offset,6);
            end;

      $9b : begin
              case memory[1] of

               $d9 : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='store fpu environment';
                                 DisassembleData.opcode:='wait:fstenv';
                                 DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;


                         7:  begin
                                 description:='store control word';
                                 DisassembleData.opcode:='wait:fstcw';
                                 DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='wait';
                            DisassembleData.opcode:='wait';
                         end;

                       end;
                     end;

               $db : begin
                       case memory[2] of
                         $e2 : begin
                                 description:='clear exceptions';
                                 DisassembleData.opcode:='wait:fclex';
                                 inc(offset,2);
                               end;

                         $e3 : begin
                                 description:='initialize floaring-point unit';
                                 DisassembleData.opcode:='wait:finit';
                                 inc(offset,2);
                               end;
                         else
                         begin
                            description:='wait';
                            DisassembleData.opcode:='wait';
                         end;
                       end;
                     end;

               $dd : begin
                       case getreg(memory[2]) of
                         6:  begin
                                 description:='store fpu state';
                                 DisassembleData.opcode:='wait:fsave';
                                 DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         7:  begin
                                 description:='store status word';
                                 DisassembleData.opcode:='wait:fstsw';
                                 DisassembleData.parameters:=modrm(memory,prefix2,2,0,last);
                                 inc(offset,last-1);
                             end;

                         else
                         begin
                            description:='wait';
                            DisassembleData.opcode:='wait';
                         end;
                       end;
                     end;

               $df : begin
                       case memory[2] of
                         $e0 : begin
                                 description:='store status word';
                                 DisassembleData.opcode:='wait:fstsw ax';
                                 inc(offset,2);
                               end;

                         else
                         begin
                            description:='wait';
                            DisassembleData.opcode:='wait';
                         end;
                       end;
                     end;

               else  begin
                       description:='wait';
                       DisassembleData.opcode:='wait';
                     end;

              end;

            end;

      $9c : begin
              description:='push eflags register onto the stack';
              if $66 in prefix2 then DisassembleData.opcode:='pushf' else
              begin
                if is64bit then
                  DisassembleData.opcode:='pushfq'
                else
                  DisassembleData.opcode:='pushfd';
              end;
            end;

      $9d : begin
              description:='pop stack into eflags register';
              if $66 in prefix2 then DisassembleData.opcode:='popf' else
              begin
                if is64bit then
                  DisassembleData.opcode:='popfq'
                else
                  DisassembleData.opcode:='popfd';
              end;
            end;

      $9e : begin
              description:='store ah into flags';
              DisassembleData.opcode:='sahf';
            end;

      $9f : begin
              description:='load status flag into ah register';
              DisassembleData.opcode:='lahf';
            end;

      $a0 : begin
              description:='copy memory';
              dwordptr:=@memory[1];
              DisassembleData.opcode:='mov';
              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              if processhandler.is64bit then
              begin
                DisassembleData.parameters:=colorreg+'al'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+']';
                inc(offset,8);
              end
              else
              begin
                DisassembleData.parameters:=colorreg+'al'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
                inc(offset,4);
              end;


            end;

      $a1 : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              dwordptr:=@memory[1];


              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if $66 in prefix2 then
              begin
                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
              end
              else
              begin
                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+',';


                if processhandler.is64bit then
                  DisassembleData.parameters:=DisassembleData.parameters+getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+']'
                else
                  DisassembleData.parameters:=DisassembleData.parameters+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';

              end;

              if processhandler.is64bit then
                inc(offset, 8)
              else
                inc(offset, 4);

            end;

      $a2 : begin
              description:='copy memory';
              dwordptr:=@memory[1];
              DisassembleData.opcode:='mov';

              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if processhandler.is64bit then
                DisassembleData.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+'],'+colorreg+'al'+endcolor
              else
                DisassembleData.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],'+colorreg+'al'+endcolor;

              if processhandler.is64bit then
                inc(offset, 8)
              else
                inc(offset, 4);
            end;

      $a3 : begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              dwordptr:=@memory[1];

              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if processhandler.is64bit then
                DisassembleData.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+'],'
              else
                DisassembleData.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],';

              if $66 in prefix2 then
                DisassembleData.parameters:=DisassembleData.parameters+colorreg+'ax'+endcolor
              else
              begin
                if rex_w then
                  DisassembleData.parameters:=DisassembleData.parameters+colorreg+'rax'+endcolor
                else
                  DisassembleData.parameters:=DisassembleData.parameters+colorreg+'eax'+endcolor;
              end;

              if processhandler.is64bit then
                inc(offset, 8)
              else
                inc(offset, 4);
            end;

      $a4 : begin
              description:='move data from string to string';
              DisassembleData.opcode:='movsb';
            end;

      $a5 : begin
              description:='move data from string to string';
              if $66 in prefix2 then
                DisassembleData.opcode:='movsw'
              else
              begin
                if rex_w then
                  DisassembleData.opcode:='movsq'
                else
                  DisassembleData.opcode:='movsd';
              end;
            end;

      $a6 : begin
              description:='compare string operands';
              DisassembleData.opcode:='cmpsb';
            end;

      $a7 : begin
              description:='compare string operands';
              if $66 in prefix2 then
                DisassembleData.opcode:='cmpsw'
              else
              begin
                if rex_w then
                  DisassembleData.opcode:='cmpsq'
                else
                  DisassembleData.opcode:='cmpsd';
              end;
            end;

      $a8 : begin
              description:='logical compare';
              DisassembleData.opcode:='test';

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $a9 : begin
              description:='logical compare';
              DisassembleData.opcode:='test';

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=dwordptr^;

                if rex_w then
                  DisassembleData.parameters:=colorreg+'rax'+endcolor+','+inttohexs(dwordptr^,8)
                else
                  DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                inc(offset,4);
              end;
            end;

      $aa : begin
              description:='store string';
              DisassembleData.opcode:='stosb';
            end;

      $ab : begin
              description:='store string';
              if $66 in prefix2 then DisassembleData.opcode:='stosw' else
              begin
                if rex_w then
                  DisassembleData.opcode:='stosq'
                else
                  DisassembleData.opcode:='stosd';
              end;
            end;

      $ac : begin
              description:='load string';
              DisassembleData.opcode:='lodsb';
            end;

      $ad : begin
              description:='load string';
              if $66 in prefix2 then DisassembleData.opcode:='lodsw' else
              begin
                if rex_w then
                  DisassembleData.opcode:='lodsq'
                else
                  DisassembleData.opcode:='lodsd';
              end;
            end;

      $ae : begin
              description:='compare al with byte at es:edi and set status flag';
              DisassembleData.opcode:='scasb';
            end;

      $af : begin
              description:='scan string';
              if $66 in prefix2 then DisassembleData.opcode:='scasw' else
              begin
                if rex_w then
                  DisassembleData.opcode:='scasq'
                else
                  DisassembleData.opcode:='scasd';
              end;
            end;

      $b0..$b7:
            begin
              description:='copy memory';
              DisassembleData.opcode:='mov';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

//              if Rex_B

              DisassembleData.parameters:=rd8(memory[0]-$b0)+','+inttohexs(memory[1],2);
              inc(offset);
            end;

      $b8..$bf:
            begin
              description:='copy memory';

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              if $66 in prefix2 then
              begin
                wordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=wordptr^;

                DisassembleData.opcode:='mov';
                DisassembleData.parameters:=rd16(memory[0]-$b8)+','+inttohexs(wordptr^,4);
                inc(offset,2);
              end
              else
              begin
                dwordptr:=@memory[1];
                DisassembleData.parametervaluetype:=dvtvalue;


                if rex_w then
                begin
                  DisassembleData.opcode:='mov';
                  DisassembleData.parametervalue:=pqword(dwordptr)^;
                  DisassembleData.parameters:=rd(memory[0]-$b8)+','+inttohexs(pqword(dwordptr)^,16);
                  inc(offset,8);
                end
                else
                begin
                  DisassembleData.opcode:='mov';
                  DisassembleData.parametervalue:=dwordptr^;

                  DisassembleData.parameters:=rd(memory[0]-$b8)+','+inttohexs(dwordptr^,8);
                  inc(offset,4);
                end;
              end;
            end;

      $c0 : begin
              case getreg(memory[1]) of
                0:  begin
                      DisassembleData.opcode:='rol';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];

                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                1:  begin
                      DisassembleData.opcode:='ror';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                2:  begin
                      DisassembleData.opcode:='rcl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='rotate nine bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                3:  begin
                      DisassembleData.opcode:='rcr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='rotate nine bits right '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                4:  begin
                      DisassembleData.opcode:='shl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='multiply by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

                5:  begin
                      DisassembleData.opcode:='shr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='unsigned divide by 2, '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;

{not in intel spec}
                6:  begin
                      DisassembleData.opcode:='rol';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      description:='rotate eight bits left '+inttostr(memory[last])+' times';
                      inc(offset,last);
                    end;
{^^^^^^^^^^^^^^^^^^}

                7:  begin
                      DisassembleData.opcode:='sar';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
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
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 16 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 32 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 16 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 32 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 17 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 33 bits left '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 17 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='rotate 33 bits right '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='multiply by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='unsigned divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end
                      else
                      begin
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        DisassembleData.parametervaluetype:=dvtvalue;
                        DisassembleData.parametervalue:=memory[last];
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                        description:='signed divide by 2 '+inttostr(memory[last])+' times';
                        inc(offset,last);
                      end;
                    end;

              end;
            end;

      $c2 : begin

              wordptr:=@memory[1];
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=wordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.opcode:='ret';
              DisassembleData.isret:=true;
              DisassembleData.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);

              description:='near return to calling procedure and pop '+inttostr(DisassembleData.parametervalue)+' bytes from stack';


            end;

      $c3 : begin
              description:='near return to calling procedure';
              DisassembleData.opcode:='ret';
              DisassembleData.isret:=true;
            end;

      $c4 : begin
              if processhandler.is64Bit=false then
              begin
                description:='load far pointer';
                DisassembleData.opcode:='les';
                if $66 in prefix2 then
                  DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                  DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                inc(offset,last-1);
              end;
            end;

      $c5 : begin
              if processhandler.is64Bit=false then
              begin
                description:='load far pointer';
                DisassembleData.opcode:='lds';
                if $66 in prefix2 then
                  DisassembleData.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                  DisassembleData.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                inc(offset,last-1);
              end;
            end;

      $c6 : begin
              if memory[1]=$f8 then
              begin
                inc(offset);
                DisassembleData.opcode:='xabort';
                description:='transactional abort';

                DisassembleData.parametervaluetype:=dvtvalue;
                DisassembleData.parametervalue:=memory[2];
                DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
                inc(DisassembleData.seperatorcount);
                DisassembleData.parameters:=inttohexs(memory[2],2);

              end
              else
              case getreg(memory[1]) of
                0 : begin
                      description:='copy memory';
                      DisassembleData.opcode:='mov';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=memory[last];

                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      inc(offset,last);
                    end;

                else begin
                       description:='not defined by the intel documentation';
                       DisassembleData.opcode:='db';
                       DisassembleData.parameters:=inttohexs(memory[0],2);
                     end;
              end;
            end;

      $c7 : begin
              if memory[1]=$f8 then
              begin
                description:='Transactional Begin';
                DisassembleData.opcode:='xbegin';

                if MarkIPRelativeInstructions then
                begin
                  DisassembleData.riprelative:=1;
                  riprelative:=true;
                end;
                inc(offset,4);
                DisassembleData.parametervaluetype:=dvtaddress;



                if is64bit then
                  DisassembleData.parametervalue:=qword(offset+pinteger(@memory[2])^)
                else
                  DisassembleData.parametervalue:=dword(offset+pinteger(@memory[2])^);

                DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

                DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
                inc(DisassembleData.seperatorcount);

              end
              else
              case getreg(memory[1]) of
              0 : begin
                    description:='copy memory';
                    if $66 in prefix2 then
                    begin
                      DisassembleData.opcode:='mov';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);

                      wordptr:=@memory[last];
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=wordptr^;

                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                      inc(offset,last+1);
                    end
                    else
                    begin
                      DisassembleData.opcode:='mov';

                      if Rex_W then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      dwordptr:=@memory[last];
                      DisassembleData.parametervaluetype:=dvtvalue;
                      DisassembleData.parametervalue:=dwordptr^;


                      if rex_w then
                        DisassembleData.parameters:=DisassembleData.parameters+IntToHexs(integer(dwordptr^),8)
                      else
                        DisassembleData.parameters:=DisassembleData.parameters+IntToHexs(dwordptr^,8);

                      inc(offset,last+3);
                    end;
                  end;

             else begin
                    description:='not defined by the intel documentation';
                    DisassembleData.opcode:='db';
                    DisassembleData.parameters:=inttohexs(memory[0],2);
                  end;

              end;
            end;

      $c8 : begin
              description:='make stack frame for procedure parameters';
              wordptr:=@memory[1];
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=wordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
              DisassembleData.seperators[DisassembleData.seperatorcount]:=3;
              inc(DisassembleData.seperatorcount);


              DisassembleData.opcode:='enter';
              DisassembleData.parameters:=inttohexs(wordptr^,4)+','+inttohexs(memory[3],2);
              inc(offset,3);
            end;

      $c9 : begin
              description:='high level procedure exit';
              DisassembleData.opcode:='leave';
            end;

      $ca : begin
              description:='far return to calling procedure and pop 2 bytes from stack';
              wordptr:=@memory[1];
              DisassembleData.opcode:='ret';
              DisassembleData.isret:=true;

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=wordptr^;
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=inttohexs(wordptr^,4);
              inc(offset,2);
            end;

      $cb : begin
              description:='far return to calling procedure';
              DisassembleData.opcode:='ret';
              DisassembleData.isret:=true;
            end;

      $cc : begin
              //should not be shown if its being debugged using int 3'
              description:='call to interrupt procedure-3:trap to debugger';
              DisassembleData.opcode:='int 3';
            end;

      $cd : begin
              description:='call to interrupt procedure';
              DisassembleData.opcode:='int';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=inttohexs(memory[1],2);
              inc(offset);
            end;

      $ce : begin
              description:='call to interrupt procedure-4:if overflow flag=1';
              DisassembleData.opcode:='into';
            end;

      $cf : begin
              description:='interrupt return';
              if $66 in prefix2 then DisassembleData.opcode:='iret' else
              begin
                if rex_w then
                  DisassembleData.opcode:='iretq'
                else
                  DisassembleData.opcode:='iretd';
              end;
            end;

      $d0 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='rotate eight bits left once';
                      DisassembleData.opcode:='rol';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='rotate eight bits right once';
                      DisassembleData.opcode:='ror';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='rotate nine bits left once';
                      DisassembleData.opcode:='rcl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='rotate nine bits right once';
                      DisassembleData.opcode:='rcr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='multiply by 2, once';
                      DisassembleData.opcode:='shl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='unsigned divide by 2, once';
                      DisassembleData.opcode:='shr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='not defined by the intel documentation';
                      DisassembleData.opcode:='db';
                      DisassembleData.parameters:=inttohexs(memory[0],2)+' '+inttohexs(memory[1],2);
                    end;

                7:  begin
                      description:='signed divide by 2, once';
                      DisassembleData.opcode:='sar';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
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
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits left once';
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits right once';
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits right once';
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits left once';
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits left once';
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits right once';
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits right once';
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='multiply by 2, once';
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='multiply by 2, once';
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='unsigned divide by 2, once';
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='unsigned divide by 2, once';
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

                6:  begin
                      description:='undefined by the intel documentation';
                      DisassembleData.opcode:='db';
                      DisassembleData.parameters:=inttohexs(memory[0],2);
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='signed divide by 2, once';
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='signed divide by 2, once';
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+'1';
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d2 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='rotate eight bits left cl times';
                      DisassembleData.opcode:='rol';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='rotate eight bits right cl times';
                      DisassembleData.opcode:='ror';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='rotate nine bits left cl times';
                      DisassembleData.opcode:='rcl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='rotate nine bits right cl times';
                      DisassembleData.opcode:='rcr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='multiply by 2, cl times';
                      DisassembleData.opcode:='shl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='unsigned divide by 2, cl times';
                      DisassembleData.opcode:='shr';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='multiply by 2, cl times';
                      DisassembleData.opcode:='shl';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed divide by 2, cl times';
                      DisassembleData.opcode:='sar';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8)+colorreg+'cl'+endcolor;
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
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits left cl times';
                        DisassembleData.opcode:='rol';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                1:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 16 bits right cl times';
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 32 bits right cl times';
                        DisassembleData.opcode:='ror';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                2:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits left cl times';
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits left cl times';
                        DisassembleData.opcode:='rcl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                3:  begin
                      if $66 in prefix2 then
                      begin
                        description:='rotate 17 bits right cl times';
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='rotate 33 bits right cl times';
                        DisassembleData.opcode:='rcr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                4:  begin
                      if $66 in prefix2 then
                      begin
                        description:='multiply by 2, cl times';
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='multiply by 2, cl times';
                        DisassembleData.opcode:='shl';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                5:  begin
                      if $66 in prefix2 then
                      begin
                        description:='unsigned divide by 2, cl times';
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='unsigned divide by 2, cl times';
                        DisassembleData.opcode:='shr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      if $66 in prefix2 then
                      begin
                        description:='signed divide by 2, cl times';
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end
                      else
                      begin
                        description:='signed divide by 2, cl times';
                        DisassembleData.opcode:='sar';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last)+colorreg+'cl'+endcolor;
                        inc(offset,last-1);
                      end;
                    end;

              end;
            end;


      $d4 : begin  // aam
              inc(offset);
              DisassembleData.opcode:='aam';
              description:='ascii adjust ax after multiply';

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if memory[1]<>$0a then
                DisassembleData.parameters:=inttohexs(memory[1],2);
            end;

      $d5 : begin  // aad
              inc(offset);
              DisassembleData.opcode:='aad';
              description:='ascii adjust ax before division';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              if memory[1]<>$0a then DisassembleData.parameters:=inttohexs(memory[1],2);
            end;

      $d7 : begin
              description:='table look-up translation';
              DisassembleData.opcode:='xlatb';
            end;

      $d8 : begin
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='add';
                      DisassembleData.opcode:='fadd';
                      last:=2;
                      if memory[1]>=$c0 then
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')' else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        DisassembleData.opcode:='fmul';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fmul';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;


                2:  begin
                      description:='compare real';
                      last:=2;
                      if memory[1]>=$d0 then
                      begin
                        DisassembleData.opcode:='fcom';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')'
                      end
                      else
                      begin
                        DisassembleData.opcode:='fcom';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='compare real and pop register stack';
                      last:=2;
                      if memory[1]>=$d8 then
                      begin
                        DisassembleData.opcode:='fcomp';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')'
                      end
                      else
                      begin
                        DisassembleData.opcode:='fcomp';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='substract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        DisassembleData.opcode:='fsub';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$e0)+')';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fsub';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='reverse substract';
                      last:=2;
                      if (memory[1]>=$e8) then
                      begin
                        DisassembleData.opcode:='fsubr';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fsubr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        DisassembleData.opcode:='fdiv';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fdiv';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='reverse divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        DisassembleData.opcode:='fdivr';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$f8)+')';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fdivr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end;
                      inc(offset,last-1);
                    end;
              end;

            end;

      $d9 : begin
              DisassembleData.isfloat:=true;
              case memory[1] of
              $00..$bf : begin

                           case getreg(memory[1]) of
                           0:  begin
                                 description:='load floating point value';
                                 DisassembleData.opcode:='fld';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           2:  begin
                                 description:='store single';
                                 DisassembleData.opcode:='fst';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           3:  begin
                                 description:='store single';
                                 DisassembleData.opcode:='fstp';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                 inc(offset,last-1);
                               end;

                           4:  begin
                                 description:='load fpu environment';
                                 DisassembleData.opcode:='fldenv';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           5:  begin
                                 description:='load control word';
                                 DisassembleData.opcode:='fldcw';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           6:  begin
                                 description:='store fpu environment';
                                 DisassembleData.opcode:='fnstenv';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;

                           7:  begin
                                 description:='store control word';
                                 DisassembleData.opcode:='fnstcw';
                                 DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                 inc(offset,last-1);
                               end;


                           end;
                         end;

              $c0..$c7 : begin
                           description:='push st(i) onto the fpu register stack';
                           DisassembleData.opcode:='fld';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $c8..$cf : begin
                           description:='exchange register contents';
                           DisassembleData.opcode:='fxch';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$c8)+')';
                           inc(offset);
                         end;


              $d9..$df : begin
                           description:='exchange register contents';
                           DisassembleData.opcode:='fxch';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$d9)+')';
                           inc(offset);
                         end;


              $d0 : begin
                      description:='no operation';
                      DisassembleData.opcode:='fnop';
                      inc(offset);
                    end;

              $e0 : begin
                      description:='change sign';
                      DisassembleData.opcode:='fchs';
                      inc(offset);
                    end;

              $e1 : begin
                      description:='absolute value';
                      DisassembleData.opcode:='fabs';
                      inc(offset);
                    end;

              $e4 : begin
                      description:='test';
                      DisassembleData.opcode:='ftst';
                      inc(offset);
                    end;

              $e5 : begin
                      description:='examine';
                      DisassembleData.opcode:='fxam';
                      inc(offset);
                    end;



              $e8 : begin
                      description:='Push +1.0 onto the FPU register stack';
                      DisassembleData.opcode:='fld1';
                      inc(offset);
                    end;

              $e9 : begin
                      description:='Push log2(10) onto the FPU register stack';
                      DisassembleData.opcode:='fldl2t';
                      inc(offset);
                    end;

              $ea : begin
                      description:='Push log2(e) onto the FPU register stack';
                      DisassembleData.opcode:='fldl2e';
                      inc(offset);
                    end;

              $eb : begin
                      description:='Push "pi" onto the FPU register stackload constant';
                      DisassembleData.opcode:='fldpi';
                      inc(offset);
                    end;

              $ec : begin
                      description:='Push log10(2) onto the FPU register stack';
                      DisassembleData.opcode:='fldlg2';
                      inc(offset);
                    end;

              $ed : begin
                      description:='Push log e(2) onto the FPU register stack';
                      DisassembleData.opcode:='fldln2';
                      inc(offset);
                    end;

              $ee : begin
                      description:='Push +0.0 onto the FPU register stack';
                      DisassembleData.opcode:='fldz';
                      inc(offset);
                    end;


              $f0 : begin
                      description:='compute 2^x–1';
                      DisassembleData.opcode:='f2xm1';
                      inc(offset);
                    end;

              $f1 : begin
                      description:='compute y*log(2)x';
                      DisassembleData.opcode:='fyl2x';
                      inc(offset);
                    end;

              $f2 : begin
                      description:='partial tangent';
                      DisassembleData.opcode:='fptan';
                      inc(offset);
                    end;

              $f3 : begin
                      description:='partial arctangent';
                      DisassembleData.opcode:='fpatan';
                      inc(offset);
                    end;

              $f4 : begin
                      description:='extract exponent and significand';
                      DisassembleData.opcode:='fxtract';
                      inc(offset);
                    end;

              $f5 : begin
                      description:='partial remainder';
                      DisassembleData.opcode:='fprem1';
                      inc(offset);
                    end;

              $f6 : begin
                      description:='decrement stack-top pointer';
                      DisassembleData.opcode:='fdecstp';
                      inc(offset);
                    end;

              $f7 : begin
                      description:='increment stack-top pointer';
                      DisassembleData.opcode:='fincstp';
                      inc(offset);
                    end;

              $f8 : begin
                      description:='partial remainder';
                      DisassembleData.opcode:='fprem';
                      inc(offset);
                    end;

              $f9 : begin
                      description:='compute y*log(2)(x+1)';
                      DisassembleData.opcode:='fyl2xp1';
                      inc(offset);
                    end;

              $fa : begin
                      description:='square root';
                      DisassembleData.opcode:='fsqrt';
                      inc(offset);
                    end;

              $fb : begin
                      description:='sine and cosine';
                      DisassembleData.opcode:='fsincos';
                      inc(offset);
                    end;


              $fc : begin
                      description:='round to integer';
                      DisassembleData.opcode:='frndint';
                      inc(offset);
                    end;

              $fd : begin
                      description:='scale';
                      DisassembleData.opcode:='fscale';
                      inc(offset);
                    end;

              $fe : begin
                      description:='sine';
                      DisassembleData.opcode:='fsin';
                      inc(offset);
                    end;

              $ff : begin
                      description:='cosine';
                      DisassembleData.opcode:='fcos';
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
                        DisassembleData.opcode:='fiadd';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  1:  begin
                        description:='multiply';
                        DisassembleData.opcode:='fimul';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  2:  begin
                        description:='compare integer';
                        DisassembleData.opcode:='ficom';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  3:  begin
                        description:='compare integer';
                        DisassembleData.opcode:='ficomp';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  4:  begin
                        description:='subtract';
                        DisassembleData.opcode:='fisub';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;

                  5:  begin
                        description:='reverse subtract';
                        DisassembleData.opcode:='fisubr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;


                  6:  begin
                        description:='divide';
                        DisassembleData.opcode:='fidiv';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                        inc(offset,last-1);
                      end;

                  7:  begin
                        description:='reverse divide';
                        DisassembleData.opcode:='fidivr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                        inc(offset,last-1);
                      end;
                end;
              end
              else
              begin
                case getreg(memory[1]) of
                  0:  begin
                        description:='floating-point: move if below';
                        DisassembleData.opcode:='fcmovb';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')';
                        inc(offset);
                      end;

                  1:  begin
                        description:='floating-point: move if equal';
                        DisassembleData.opcode:='fcmove';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                        inc(offset);
                      end;

                  2:  begin
                        description:='floating-point: move if below or equal';
                        DisassembleData.opcode:='fcmovbe';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')';
                        inc(offset);
                      end;

                  3:  begin
                        description:='floating-point: move if unordered';
                        DisassembleData.opcode:='fcmovu';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                        inc(offset);
                      end;

                  5:  begin
                        case memory[1] of
                        $e9 : begin
                                description:='unordered compare real';
                                DisassembleData.opcode:='fucompp';
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
                                    DisassembleData.opcode:='fild';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              1:  begin
                                    description:='store integer with truncation';
                                    DisassembleData.opcode:='fisttp';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              2:  begin
                                    description:='store integer';
                                    DisassembleData.opcode:='fist';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              3:  begin
                                    description:='store integer';
                                    DisassembleData.opcode:='fistp';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    DisassembleData.isfloat:=true;
                                    description:='load floating point value';
                                    DisassembleData.opcode:='fld';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                                    inc(offset,last-1);
                                  end;

                              7:  begin
                                    DisassembleData.isfloat:=true;
                                    description:='store extended';
                                    DisassembleData.opcode:='fstp';
                                    DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                                    inc(offset,last-1);
                                  end;

                            end;
                          end;

                $c0..$c7 : begin
                             description:='floating-point: move if not below';
                             DisassembleData.opcode:='fcmovnb';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c0)+')';
                             inc(offset);
                           end;

                $c8..$cf : begin
                             description:='floating-point: move if not equal';
                             DisassembleData.opcode:='fcmovne';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$c8)+')';
                             inc(offset);
                           end;

                $d0..$d7 : begin
                             description:='floating-point: move if not below or equal';
                             DisassembleData.opcode:='fcmovnbe';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d0)+')';
                             inc(offset);
                           end;

                $d8..$df : begin
                             description:='floating-point: move if not unordered';
                             DisassembleData.opcode:='fcmovnu';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$d8)+')';
                             inc(offset);
                           end;

                $e2 : begin
                        description:='clear exceptions';
                        DisassembleData.opcode:='fnclex';
                        inc(offset);
                      end;

                $e3 : begin
                        description:='initialize floating-point unit';
                        DisassembleData.opcode:='fninit';
                        inc(offset);
                      end;

                $e8..$ef : begin
                             description:='floating-point: compare real and set eflags';
                             DisassembleData.opcode:='fucomi';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
                             inc(offset);
                           end;

                $f0..$f7 : begin
                             description:='floating-point: compare real and set eflags';
                             DisassembleData.opcode:='fcomi';
                             DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
                             inc(offset);
                           end;
              end;


            end;

      $dc : begin
              DisassembleData.isfloat:=true;
              case getreg(memory[1]) of
                0:  begin
                      //fadd
                      description:='add';
                      last:=2;
                      if memory[1]>=$c0 then
                      begin
                        DisassembleData.opcode:='fadd';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)'
                      end
                      else
                      begin
                        DisassembleData.opcode:='fadd';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        DisassembleData.opcode:='fmul';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$c8)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fmul';
                         DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='compare real';
                      last:=2;
                      DisassembleData.opcode:='fcom';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='compare real';
                      last:=2;
                      DisassembleData.opcode:='fcomp';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='subtract';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        DisassembleData.opcode:='fsubr';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$e0)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fsub';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='reverse subtract';
                      last:=2;
                      if memory[1]>=$e8 then
                      begin
                        DisassembleData.opcode:='fsub';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$e8)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fsubr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;


                      inc(offset,last-1);
                    end;


                6:  begin
                      description:='divide';
                      last:=2;
                      if memory[1]>=$f0 then
                      begin
                        DisassembleData.opcode:='fdivr';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$f0)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fdiv';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      end;
                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='reverse divide';
                      last:=2;
                      if memory[1]>=$f8 then
                      begin
                        DisassembleData.opcode:='fdiv';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$f8)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fdivr';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

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
                                   DisassembleData.isfloat:=true;
                                   description:='load floating point value';
                                   DisassembleData.opcode:='fld';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             1:  begin
                                   description:='store integer with truncation';
                                   DisassembleData.opcode:='fisttp';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             2:  begin
                                   DisassembleData.isfloat:=true;
                                   description:='store double';
                                   DisassembleData.opcode:='fst';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             3:  begin
                                   DisassembleData.isfloat:=true;
                                   description:='store double';
                                   DisassembleData.opcode:='fstp';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                                   inc(offset,last-1);
                                 end;

                             4:  begin
                                   description:='restore fpu state';
                                   DisassembleData.opcode:='frstor';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             6:  begin
                                   description:='store fpu state';
                                   DisassembleData.opcode:='fnsave';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                             7:  begin
                                   description:='store status word';
                                   DisassembleData.opcode:='fnstsw';
                                   DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                                   inc(offset,last-1);
                                 end;

                           end;

                         end;

              $c0..$c7 : begin
                           description:='free floating-point register';
                           DisassembleData.opcode:='ffree';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$c0)+')';
                           inc(offset);
                         end;

              $d0..$d7 : begin
                           description:='store real';
                           DisassembleData.opcode:='fst';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$d0)+')';
                           inc(offset);
                         end;

              $d8..$df : begin
                           description:='store real';
                           DisassembleData.opcode:='fstp';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$d8)+')';
                           inc(offset);
                         end;

              $e0..$e7 : begin
                           description:='unordered compare real';
                           DisassembleData.opcode:='fucom';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$e0)+')';
                           inc(offset);
                         end;

              $e8..$ef : begin
                           description:='unordered compare real';
                           DisassembleData.opcode:='fucomp';
                           DisassembleData.parameters:='st('+inttostr(memory[1]-$e8)+')';
                           inc(offset);
                         end;
                else
                begin
                  DisassembleData.opcode:='db';
                  DisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $de : begin
              case getreg(memory[1]) of
                0:  begin
                      //faddp
                      description:='add and pop';
                      last:=2;
                      if (memory[1]=$c1) then DisassembleData.opcode:='faddp'
                      else
                      if memory[1]>=$c0 then
                      begin
                        DisassembleData.opcode:='faddp';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$c0)+'),st(0)';
                      end
                      else
                      begin
                        description:='add';
                        DisassembleData.opcode:='fiadd';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      end;
                      inc(offset,last-1);
                    end;

                1: begin
                      description:='multiply';
                      last:=2;
                      if memory[1]>=$c8 then
                      begin
                        DisassembleData.opcode:='fmulp';
                        DisassembleData.parameters:='st('+inttostr(memory[1]-$c8)+'),st(0)';
                      end
                      else
                      begin
                        DisassembleData.opcode:='fimul';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      end;

                      inc(offset,last-1);
                   end;

                2: begin
                     description:='compare integer';
                     last:=2;
                     DisassembleData.opcode:='ficom';
                     DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                     inc(offset,last-1);
                   end;


                3: begin
                     if memory[1]<$c0 then
                     begin
                       description:='compare integer';
                       DisassembleData.opcode:='ficomp';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                       inc(offset,last-1);
                     end;

                     if memory[1]=$d9 then
                     begin
                       description:='compare real and pop register stack twice';
                       DisassembleData.opcode:='fcompp';
                       inc(offset);
                     end;
                   end;

                4: begin
                     description:='subtract';
                     last:=2;
                     if memory[1]>=$e0 then
                     begin
                       DisassembleData.opcode:='fsubrp';
                       DisassembleData.parameters:='st('+inttostr(memory[1]-$e0)+'),st(0)';
                     end
                     else
                     begin
                       DisassembleData.opcode:='fisub';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;


                5: begin
                     description:='reverse divide';
                     last:=2;
                     if memory[1]>=$e8 then
                     begin
                       description:='subtract and pop from stack';
                       DisassembleData.opcode:='fsubp';
                       DisassembleData.parameters:='st('+inttostr(memory[1]-$e8)+'),st(0)'
                     end
                     else
                     begin
                       DisassembleData.opcode:='fisubr';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;

                     inc(offset,last-1);
                   end;


                6: begin
                     description:='reverse divide';
                     last:=2;
                     if memory[1]>=$f0 then
                     begin
                       DisassembleData.opcode:='fdivrp';
                       DisassembleData.parameters:='st('+inttostr(memory[1]-$f0)+'),st(0)';
                       inc(offset,last-1);
                     end
                     else
                     begin
                       description:='divide';
                       DisassembleData.opcode:='fidiv';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);

                       inc(offset,last-1);
                     end;
                   end;

                7: begin
                     description:='divide';
                     last:=2;
                     if memory[1]>=$f8 then
                     begin
                       DisassembleData.opcode:='fdivp';
                       DisassembleData.parameters:='st('+inttostr(memory[1]-$f8)+'),st(0)';
                     end
                     else
                     begin
                       DisassembleData.opcode:='fdivr';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                     end;
                     inc(offset,last-1);
                   end;

              end;
            end;

      $df : begin
              if memory[1] in [$c0..$c7] then
              begin
                description:='free floating-point register and pop (might not work)';
                DisassembleData.opcode:='ffreep';
                DisassembleData.parameters:='st('+inttostr(memory[1]-$c0)+')';
                inc(offset);
              end
              else
              case getreg(memory[1]) of
                0:  begin
                      description:='load integer';
                      DisassembleData.opcode:='fild';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='store integer with truncation';
                      DisassembleData.opcode:='fisttp';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                2:  begin
                      description:='store integer';
                      DisassembleData.opcode:='fist';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='store integer';
                      DisassembleData.opcode:='fistp';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,16);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='load binary coded decimal';
                      last:=2;
                      if memory[1]>=$e0 then
                      begin
                        DisassembleData.opcode:='fnstsw';
                        DisassembleData.parameters:=colorreg+'ax'+endcolor;
                      end
                      else
                      begin
                        DisassembleData.opcode:='fbld';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                      end;
                      inc(offset,last-1);
                    end;

               5:  begin
                     if memory[1]<$c0 then
                     begin
                       description:='load integer';
                       DisassembleData.opcode:='fild';
                       DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                       inc(offset,last-1);
                     end;

                     if memory[1]>=$e8 then
                     begin
                       description:='compare real and set eflags';
                       DisassembleData.opcode:='fucomip';
                       DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
                       inc(offset);
                     end;
                   end;

               6:  begin
                      if memory[1]>=$f0 then
                      begin
                        description:='compare real and set eflags';
                        DisassembleData.opcode:='fcomip';
                        DisassembleData.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
                        inc(offset)
                      end
                      else
                      begin
                        description:='store bcd integer and pop';
                        DisassembleData.opcode:='fbstp';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,80);

                        inc(offset,last-1);
                      end;
                    end;

                7:  begin
                      description:='store integer';
                      DisassembleData.opcode:='fistp';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);

                      inc(offset,last-1);
                    end;

                else
                begin
                  DisassembleData.opcode:='db';
                  DisassembleData.parameters:=inttohexs(memory[0],2);
                end;
              end;

            end;

      $e0 : begin
              description:='loop according to ecx counter';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;

              DisassembleData.opcode:='loopne';

              inc(offset);

              DisassembleData.parametervaluetype:=dvtaddress;
              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $e1 : begin
              description:='loop according to ecx counter';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;
              if context<>nil then
                          DisassembleData.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

              DisassembleData.opcode:='loope';
              inc(offset);

              DisassembleData.parametervaluetype:=dvtaddress;
              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $e2 : begin
              description:='loop according to ecx counting';
              DisassembleData.opcode:='loop';
              if context<>nil then
                DisassembleData.willJumpAccordingToContext:=context^.{$ifdef CPU64}RCX{$else}ECX{$endif}<>0;

              DisassembleData.isjump:=true;
              inc(offset);

              DisassembleData.parametervaluetype:=dvtaddress;
              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $e3 : begin
              description:='jump short if cx=0';
              DisassembleData.isjump:=true;
              DisassembleData.isconditionaljump:=true;

              if $66 in prefix2 then
              begin
                DisassembleData.opcode:='jcxz';
                if context<>nil then
                  DisassembleData.willJumpAccordingToContext:=((context^.{$ifdef CPU64}RCX{$else}ECX{$endif}) and $ffff)=0;

              end
              else
              begin
                DisassembleData.opcode:='jecxz';
                if context<>nil then
                  DisassembleData.willJumpAccordingToContext:=context^.{$ifdef CPU64}RCX{$else}ECX{$endif}=0;

              end;
              inc(offset);

              DisassembleData.parametervaluetype:=dvtaddress;



              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $e4 : begin
              description:='input from port';
              DisassembleData.opcode:='in';
              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

              DisassembleData.parameters:=colorreg+'al'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e5 : begin
              description:='input from port';
              DisassembleData.opcode:='in';

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);


              if $66 in prefix2 then DisassembleData.parameters:=colorreg+'ax'+endcolor+','+inttohexs(memory[1],2)
                                else DisassembleData.parameters:=colorreg+'eax'+endcolor+','+inttohexs(memory[1],2);
              inc(offset);

            end;

      $e6 : begin
              description:='output to port';
              DisassembleData.opcode:='out';
              DisassembleData.parameters:=inttohexs(memory[1],2)+','+colorreg+'al'+endcolor;
              inc(offset);

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount)
            end;

      $e7 : begin
              description:='output toport';
              DisassembleData.opcode:='out';
              if $66 in prefix2 then
                DisassembleData.parameters:=inttohexs(memory[1],2)+','+colorreg+'ax'+endcolor else
                DisassembleData.parameters:=inttohexs(memory[1],2)+','+colorreg+'eax'+endcolor;

              inc(offset);

              DisassembleData.parametervaluetype:=dvtvalue;
              DisassembleData.parametervalue:=memory[1];
              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
            end;

      $e8 : begin
              //call
              //this time no $66 prefix because it will only run in win32
              description:='call procedure';
              DisassembleData.opcode:='call';
              DisassembleData.isjump:=true;
              DisassembleData.iscall:=true;

              if MarkIPRelativeInstructions then
              begin
                DisassembleData.riprelative:=1;
                riprelative:=true;
              end;
              inc(offset,4);
              DisassembleData.parametervaluetype:=dvtaddress;

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pinteger(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pinteger(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

            end;

      $e9 : begin
              description:='jump near';
              DisassembleData.isjump:=true;

              if $66 in prefix2 then
              begin
                DisassembleData.opcode:='jmp';

                inc(offset,2);
                DisassembleData.parametervaluetype:=dvtaddress;
                DisassembleData.parametervalue:=dword(offset+qword(psmallint(@memory[1])^));
                DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);
              end
              else
              begin
                DisassembleData.opcode:='jmp';

                if MarkIPRelativeInstructions then
                begin
                  DisassembleData.riprelative:=1;
                  riprelative:=true;
                end;

                inc(offset,4);
                DisassembleData.parametervaluetype:=dvtaddress;

                if is64bit then
                  DisassembleData.parametervalue:=qword(offset+qword(pinteger(@memory[1])^))
                else
                  DisassembleData.parametervalue:=dword(offset+qword(pinteger(@memory[1])^));

                DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8)
              end;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

            end;

      $ea : begin
              description:='jump far';
              DisassembleData.isjump:=true;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);
              DisassembleData.seperators[DisassembleData.seperatorcount]:=5;
              inc(DisassembleData.seperatorcount);


              wordptr:=@memory[5];
              DisassembleData.opcode:='jmp';
              DisassembleData.parameters:=inttohexs(wordptr^,4)+':';
              dwordptr:=@memory[1];

              DisassembleData.parametervaluetype:=dvtaddress;
              DisassembleData.parametervalue:=dwordptr^;


              DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,8);
              inc(offset,6);
            end;

      $eb : begin
              description:='jump short';
              DisassembleData.opcode:='jmp';
              DisassembleData.isjump:=true;

              inc(offset);

              if is64bit then
                DisassembleData.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
              else
                DisassembleData.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

              DisassembleData.parameters:=inttohexs(DisassembleData.parametervalue,8);

              DisassembleData.parametervaluetype:=dvtAddress;

              DisassembleData.seperators[DisassembleData.seperatorcount]:=1;
              inc(DisassembleData.seperatorcount);

            end;

      $ec : begin
              description:='input from port';
              DisassembleData.opcode:='in';
              DisassembleData.parameters:=colorreg+'al'+endcolor+','+colorreg+'dx'+endcolor;
            end;

      $ed : begin
              description:='input from port';
              DisassembleData.opcode:='in';
              if $66 in prefix2 then DisassembleData.parameters:=colorreg+'ax'+endcolor+','+colorreg+'dx'+endcolor else
                                     DisassembleData.parameters:=colorreg+'eax'+endcolor+','+colorreg+'dx'+endcolor;
            end;

      $ee : begin
              description:='input from port';
              DisassembleData.opcode:='out';
              DisassembleData.parameters:=colorreg+'dx'+endcolor+','+colorreg+'al'+endcolor
            end;

      $ef : begin
              description:='input from port';
              DisassembleData.opcode:='out';
              if $66 in prefix2 then DisassembleData.parameters:=colorreg+'dx'+endcolor+','+colorreg+'ax'+endcolor  else
                                     DisassembleData.parameters:=colorreg+'dx'+endcolor+','+colorreg+'eax'+endcolor ;
            end;

      $f3 : begin

            end;

      $f4 : begin
              description:='halt';
              DisassembleData.opcode:='hlt';
            end;

      $f5 : begin
              description:='complement carry flag';
              DisassembleData.opcode:='cmc';
            end;

      $f6 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='logical compare';
                      DisassembleData.opcode:='test';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);
                      DisassembleData.parameters:=DisassembleData.parameters+inttohexs(memory[last],2);
                      DisassembleData.parametervaluetype:=dvtValue;
                      DisassembleData.parametervalue:=memory[last];


                      inc(offset,last);
                    end;

                2:  begin
                      description:='one''s complement negation';
                      DisassembleData.opcode:='not';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='two''s complement negation';
                      DisassembleData.opcode:='neg';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='unsigned multiply';
                      DisassembleData.opcode:='mul';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='signed multiply';
                      DisassembleData.opcode:='imul';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='unsigned divide';
                      DisassembleData.opcode:='div';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed divide';
                      DisassembleData.opcode:='idiv';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;
                else
                begin
                  DisassembleData.opcode:='db';
                  DisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;
            end;

      $f7 : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='logical compare';
                      if $66 in prefix2 then
                      begin
                        DisassembleData.opcode:='test';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16);
                        wordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtaddress;
                        DisassembleData.parametervalue:=wordptr^;

                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(wordptr^,4);
                        inc(offset,last+1);
                      end
                      else
                      begin
                        DisassembleData.opcode:='test';
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                        dwordptr:=@memory[last];
                        DisassembleData.parametervaluetype:=dvtaddress;
                        DisassembleData.parametervalue:=dwordptr^;
                        DisassembleData.parameters:=DisassembleData.parameters+inttohexs(dwordptr^,4);
                        inc(offset,last+3);
                      end;
                    end;

                2:  begin
                      description:='one''s complement negation';
                      DisassembleData.opcode:='not';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16) else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                3:  begin
                      description:='two''s complement negation';
                      DisassembleData.opcode:='neg';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                4:  begin
                      description:='unsigned multiply';
                      DisassembleData.opcode:='mul';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                5:  begin
                      description:='signed multiply';
                      DisassembleData.opcode:='imul';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='unsigned divide';
                      DisassembleData.opcode:='div';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                7:  begin
                      description:='signed divide';
                      DisassembleData.opcode:='idiv';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                  else
                  begin
                    DisassembleData.opcode:='db';
                    DisassembleData.parameters:=inttohexs(memory[0],2);
                  end;
                end;
            end;

      $f8 : begin
              description:='clear carry flag';
              DisassembleData.opcode:='clc';
            end;

      $f9 : begin
              description:='set carry flag';
              DisassembleData.opcode:='stc';
            end;

      $fa : begin
              description:='clear interrupt flag';
              DisassembleData.opcode:='cli';
            end;

      $fb : begin
              description:='set interrupt flag';
              DisassembleData.opcode:='sti';
            end;

      $fc : begin
              description:='clear direction flag';
              DisassembleData.opcode:='cld';
            end;

      $fd : begin
              description:='set direction flag';
              DisassembleData.opcode:='std';
            end;

      $fe : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='increment by 1';
                      DisassembleData.opcode:='inc';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,8);

                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='decrement by 1';
                      DisassembleData.opcode:='dec';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,2,last,7);

                      inc(offset,last-1);
                    end;

                else
                begin
                  DisassembleData.opcode:='db';
                  DisassembleData.parameters:=inttohexs(memory[0],2);
                end;
              end;
            end;

      $ff : begin
              case getreg(memory[1]) of
                0:  begin
                      description:='increment by 1';
                      DisassembleData.opcode:='inc';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;

                1:  begin
                      description:='decrement by 1';
                      DisassembleData.opcode:='dec';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last,16)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);

                      inc(offset,last-1);
                    end;

                2:  begin
                      //call
                      description:='call procedure';
                      DisassembleData.opcode:='call';
                      DisassembleData.isjump:=true;
                      DisassembleData.iscall:=true;

                      if memory[1]>=$c0 then
                      begin
                        if is64bit then
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64) else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      end
                      else
                      begin
                        if is64bit then
                        begin

                          if (memory[1]=$15) and (pdword(@memory[2])^=2) and (pword(@memory[6])^=$8eb) then //special 16 byte call
                          begin
                            DisassembleData.parameters:=inttohexs(pqword(@memory[8])^,8);
                            inc(last,8+4+2+2);

                            DisassembleData.Seperators[0]:=2;
                            DisassembleData.Seperators[1]:=2+4;
                            DisassembleData.Seperators[2]:=2+4+2;
                            DisassembleData.SeperatorCount:=3;

                          end
                          else
                            DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                        end
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);
                      end;

                      inc(offset,last-1);
                    end;

                3:  begin
                      //call
                      description:='call procedure';
                      DisassembleData.opcode:='call';
                      DisassembleData.isjump:=true;
                      DisassembleData.iscall:=true;

                      if is64bit then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);

                      inc(offset,last-1);
                    end;

                4:  begin
                      //jmp
                      description:='jump near';
                      DisassembleData.opcode:='jmp';
                      DisassembleData.isjump:=true;


                      if is64bit then
                      begin
                        if (memory[1]=$25) and (pdword(@memory[2])^=0) then //special 14 byte jmp
                        begin
                          DisassembleData.parameterValue:=pqword(@memory[6])^;
                          DisassembleData.parameterValueType:=dvtAddress;

                          DisassembleData.parameters:=inttohexs(pqword(@memory[6])^,8);
                          inc(last,8+4+2);

                          DisassembleData.Seperators[0]:=2;
                          DisassembleData.Seperators[1]:=2+4;
                          DisassembleData.SeperatorCount:=2;

                        end
                        else
                          DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,64);
                      end
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last,32);


                      inc(offset,last-1);
                    end;

                5:  begin
                      //jmp
                      description:='jump far';
                      DisassembleData.opcode:='jmp far';
                      DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);
                      DisassembleData.isjump:=true;

                      inc(offset,last-1);
                    end;

                6:  begin
                      description:='push word or doubleword onto the stack';
                      DisassembleData.opcode:='push';
                      if $66 in prefix2 then
                        DisassembleData.parameters:=modrm(memory,prefix2,1,1,last)
                      else
                        DisassembleData.parameters:=modrm(memory,prefix2,1,0,last);


                      inc(offset,last-1);
                    end;
                else
                begin
                  DisassembleData.opcode:='db';
                  DisassembleData.parameters:=inttohexs(memory[0],2);
                end;

              end;

            end;

      else  begin
              DisassembleData.opcode:='db';
              DisassembleData.parameters:=inttohex(memory[0],2);
            end;
    end;

   // if dataonly then exit;     //no need to handle the other stuff, dataonly means I'm only interested in the addresses, not bytes or extra parameters

   if (DisassembleData.parameters<>'') and (DisassembleData.parameters[length(DisassembleData.parameters)]=',') then
     setlength(DisassembleData.parameters, length(DisassembleData.parameters)-1);


   // tempresult:=tempresult+DisassembleData.opcode+' '+DisassembleData.parameters;

    DisassembleData.description:=description;

    //copy the remaining bytes
    k:=length(DisassembleData.Bytes);
    setlength(DisassembleData.Bytes,offset-initialoffset);

    if (k>=32) or (k<0) then
    begin
      MessageBox(0,pchar(inttohex(startoffset,8)+'disassembler error 1'),'debug here',MB_OK);
    end;

    td:=dword(offset-initialoffset-qword(k));
    i:=k+td;
    if (td>=32) or (i>=32) or (i<0) then
    begin
      MessageBox(0,pchar(inttohex(startoffset,8)+'disassembler error 2'),'debug here',MB_OK);
    end;

    if td>0 then
    begin
      breaknow:=false;
      try

        copymemory(@DisassembleData.Bytes[k], @_memory[k], td);
      except
        breaknow:=true;
      end;

      if breaknow then
      begin
        asm
        nop
        end;
        MessageBox(0,pchar(inttohex(startoffset,8)+'disassembler error 3'),'debug here',MB_OK);
      end;
    end;


    //adjust for the prefix.
    if k<>0 then
    begin
      for i:=0 to DisassembleData.SeperatorCount-1 do
        inc(DisassembleData.Seperators[i],prefixsize);

      if DisassembleData.riprelative<>0 then
        inc(DisassembleData.riprelative, prefixsize);
    end;




    //todo: Next time the disassembler is getting an averhaul, do something about the prefix counting and the unnecessary readprocessmemorys associated with it


  //  result:=result+'- '+tempresult;


    if riprelative then
    begin
      //add the current offset to the code between []
      DisassembleData.modrmValue:=offset+ptrint(integer(DisassembleData.modrmValue)); //sign extended increase

      i:=pos('[',DisassembleData.parameters);
      j:=PosEx(']',DisassembleData.parameters,i);
     // tempresult:=copy(DisassembleData.parameters,i+1,j-i-1);


      tempaddress:=DisassembleData.modrmValue;

      tempresult:=copy(DisassembleData.parameters,1,i);
      tempresult:=tempresult+inttohexs(tempaddress,8);
      DisassembleData.parameters:=tempresult+copy(DisassembleData.parameters,j,length(DisassembleData.parameters));
    end;





  end
  else
  begin
    DisassembleData.opcode:='??';
    inc(offset);
  end;

  DisassembleData.iscloaked:=hasCloakedRegionInRange(DisassembleData.address, length(DisassembleData.Bytes), VA, PA);


  if not dataonly then
  begin
    result:=inttohex(DisassembleData.address,8)+' - '+getLastBytestring;
    result:=result+' - ';
    result:=result+DisassembleData.prefix+DisassembleData.opcode;
    result:=result+' ';
    result:=result+DisassembleData.parameters;
  end;

  if assigned(OnPostDisassemble) then
  begin
    tempresult:=result;
    tempdescription:=description;

    if OnPostDisassemble(self, initialoffset, DisassembleData, tempresult, tempdescription) then
    begin
      result:=tempresult;
      description:=tempdescription;

      if length(DisassembleData.Bytes)>0 then
        offset:=initialoffset+length(DisassembleData.Bytes);
    end;
  end;

  for i:=32 to 63 do
    if _memory[i]<>$ce then
      raise exception.create('Memory corruption in the disassembler');
  except
    on e:exception do
    begin
      outputdebugstring(inttohex(startoffset,8)+':disassembler exception:'+e.message);
      MessageBox(0,pchar('disassembler exception at '+inttohex(startoffset,8)+#13#10+e.message+#13#10+#13#10+'Please provide dark byte the bytes that are at this address so he can fix it'#13#10'(Open another CE instance and in the hexadecimal view go to this address)'),'debug here',MB_OK);
    end;
  end;

  LastDisassembleData:=disassembleData;
end;

function TDisassembler.getLastBytestring: string;
var
  i,j: integer;
  cloaked:boolean=false;
  VA,PA: qword;
begin
  result:='';
  for i:=0 to length(LastDisassembleData.Bytes)-1 do
  begin
    if syntaxhighlighting and LastDisassembleData.iscloaked then
    begin
      //check if this byte is cloaked (due to pageboundaries)
      cloaked:=hasCloakedRegionInRange(LastDisassembleData.address+i, 1, VA, PA);
      if (cloaked) then result:=result+'{C00FF00}'; //green
    end;

    result:=result+inttohex(LastDisassembleData.Bytes[i],2);

    if i<LastDisassembleData.prefixsize then
      result:=result+' '
    else
      for j:=0 to LastDisassembleData.SeperatorCount-1 do
        if (LastDisassembleData.Seperators[j]=i+1) then  //followed by a seperator
          result:=result+' ';

    if syntaxhighlighting and LastDisassembleData.iscloaked and cloaked then
    begin
      result:=result+'{N}'; //back to default
      cloaked:=false;
    end;
  end;
end;


function disassemble(var offset: ptrUint): string; overload;
var ignore: string;
begin
  result:=disassemble(offset,ignore);
end;


function previousOpcodeHelp(d: Tdisassembler; address: ptruint; distance:integer; var result2: ptruint): ptruint;
var x,y: ptruint;
    s: string;
    i: integer;
begin
  x:=address-distance;
  while x<address do
  begin
    y:=x;
    d.disassemble(x,s);
  end;
  result:=x;
  result2:=y;
end;


function previousopcode(address: ptrUint; d: Tdisassembler=nil):ptrUint;
var
  x,y: ptrUint;
  s: string;
  found: boolean;
  i: ptrUint;

begin
  if d=nil then
    d:=defaultDisassembler;

  x:=previousOpcodeHelp(d, address,80, result);
  if x<>address then
  begin
    //no match found 80 bytes from the start
    //try 40
    x:=previousOpcodeHelp(d, address,40, result);

    if x<>address then
    begin
      //nothing with 40, try 20
      x:=previousOpcodeHelp(d, address,20,result);
      if x<>address then
      begin
        //no 20, try 10
        x:=previousOpcodeHelp(d, address,10,result);
        if x<>address then
        begin
          //and if all else fails try to find the closest one
          result:=address-1;
          for i:=1 to 20 do
          begin
            x:=address-i;
            d.disassemble(x,s);
            if x=address then
            begin
              result:=address-i;
              exit;
            end;
          end;
        end;

      end;
    end;
  end;
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
  lastmatch:=0;

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
  br: ptruint;

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
      address:=StrToQWordEx(s); //s already has the $ in front

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
  x: PtrUInt;

  values: array [0..1] of record
    value: ptruint;
    vtype: TVariableType;
    isAddress: boolean;
    s: string;
  end;
  //value: ptrUint;
  //vtype: TVariableType;
  a: boolean;
  s: string;


  parametercount: integer;
  sv1,sv2: string;

  i: integer;
begin
  if LastDisassembleData.commentsoverride<>'' then
    exit(LastDisassembleData.commentsoverride);

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
      if LastDisassembleData.parameterValueType=dvtNone then
        exit; //jump with no address (e.g reg)

      jumpAddress:=LastDisassembleData.parameterValue;
    end;

    //check if the bytes at jumpAddress is ff 25 (jmp [xxxxxxxx])
    if ReadProcessMemory(processhandle, pointer(jumpAddress), @buffer[0], 6,x) then
    begin

      if (buffer[0]=$ff) and (buffer[1]=$25) then
      begin
        result:=result+'->';  //double, so ->->


        if is64bit then
          jumpAddress:=jumpaddress+6+pinteger(@buffer[2])^ //jumpaddress+6 because of relative addressing
        else
          jumpAddress:=pdword(@buffer[2])^;

        //jumpaddress now contains the address of the address to jump to
        //so, get the address it actually jumps to
        if not ReadProcessMemory(processhandle, pointer(jumpAddress), @jumpAddress, processhandler.pointersize,x) then exit;
      end;


      s:=symhandler.getNameFromAddress(jumpAddress);
      if pos(s, LastDisassembleData.parameters)=0 then //no need to show a comment if it's exactly the same
        result:=result+'->'+s;
    end;


  end
  else
  begin
    if (LastDisassembleData.modrmValueType=dvtAddress) or (LastDisassembleData.parameterValueType<>dvtNone) then
    begin
      a:=false;

      parametercount:=0;

      if LastDisassembleData.parameterValueType<>dvtNone then inc(parametercount);
      if LastDisassembleData.modrmValueType=dvtAddress then inc(parametercount);

      if (LastDisassembleData.modrmValueType=dvtAddress) then
      begin
        if (parametercount>1) and (modrmposition=mRight) then values[1].value:=LastDisassembleData.modrmValue else values[0].value:=LastDisassembleData.modrmValue;
      end;

      if LastDisassembleData.parameterValueType<>dvtNone then
      begin
        if (parametercount>1) and (modrmposition<>mRight) then values[1].value:=LastDisassembleData.parameterValue else values[0].value:=LastDisassembleData.parameterValue;
      end;

      for i:=0 to parametercount-1 do
      begin
        values[i].s:='';

        if isAddress(values[i].value) then
        begin
          values[i].isAddress:=true;
          x:=0;

          values[i].vtype:=vtDword;
          readprocessmemory(processhandle, pointer(values[i].value), @buffer[0], 63,x);
          if x>0 then
          begin
            if LastDisassembleData.isfloat then
            begin
              case LastDisassembleData.datasize of
                4: values[i].vtype:=vtSingle;
                8: values[i].vtype:=vtDouble;
                10: values[i].vtype:=vtQword; //('ext>'); //exit(format('%.2f',[pextended(@buffer[0])^]);
              end;
            end
            else
              values[i].vtype:=FindTypeOfData(values[i].value, @buffer[0], x)
          end
          else
          begin
            values[i].s:='';
            continue;
          end;


        end
        else
        begin
          x:=sizeof(values[i].value);
          pptruint(@buffer[0])^:=values[i].value; //assign it so I don't have to make two compare routines
          values[i].vtype:=FindTypeOfData(0, @buffer[0], x);
          values[i].IsAddress:=false;
        end;



        case values[i].vtype of
          vtByte: values[i].s:=inttostr(buffer[0]);
          vtWord: values[i].s:=inttostr(PSmallInt(@buffer[0])^);
          vtDword: if a then values[i].s:=inttohex(pdword(@buffer[0])^,8) else values[i].s:=inttostr(pinteger(@buffer[0])^);
          vtQword: values[i].s:=inttostr(pInt64(@buffer[0])^);
          vtSingle: values[i].s:=format('%.2f',[psingle(@buffer[0])^]);
          vtDouble: values[i].s:=format('%.2f',[pdouble(@buffer[0])^]);
          vtString:
          begin
            buffer[x]:=0;
            values[i].s:='"'+pchar(@buffer[0])+'"';
          end;


          vtUnicodeString:
          begin
            buffer[x]:=0;
            if x>0 then
              buffer[x-1]:=0;

            values[i].s:='"'+pwidechar(@buffer[0])+'"';
          end;

          vtPointer:
          begin
            if processhandler.is64Bit then
              values[i].s:=inttohex(PQWord(@buffer[0])^,8)
            else
              values[i].s:=inttohex(PDWord(@buffer[0])^,8);

          end;
        end;
       // result:=VariableTypeToString(vtype);

        if values[i].isAddress and (values[i].s<>'') then
          values[i].s:='('+values[i].s+')';

        if i=0 then
          result:=result+values[i].s
        else
          result:=result+','+values[i].s;
      end;



    end;



  end;
end;


procedure tdisassembler.splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);
//obsolete
var offset,value:ptrUint;
    e: integer;
    i,j,j2,k,l: integer;
    ts,ts2,ts3: string;
    actualread: PtrUInt;
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
                if is64bit then
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

function TDisAssembler.inttohexs_withoutsymbols(value:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
begin
  if chars=2 then
  begin
    signed:=true;
    signedsize:=2;
  end;

  if signed then
  begin
    case signedsize of
      2:
       begin
         if Shortint(value)<0 then
           result:=colorhex+'-'+sysutils.IntToHex(-Shortint(value),chars)+endcolor
         else
           result:=colorhex+sysutils.IntToHex(Shortint(value),chars)+endcolor;
       end;

      4:
       begin
         if Smallint(value)<0 then
           result:=colorhex+'-'+sysutils.IntToHex(-Smallint(value),chars)+endcolor
         else
           result:=colorhex+sysutils.IntToHex(Smallint(value),chars)+endcolor;
       end;

      8:
       begin
         if Longint(value)<0 then
           result:=colorhex+'-'+sysutils.IntToHex(-Longint(value),chars)+endcolor
         else
           result:=colorhex+sysutils.IntToHex(Longint(value),chars)+endcolor;
       end;

      else result:=colorhex+sysutils.IntToHex(value,chars)+endcolor;
    end;
  end
  else
  result:=colorhex+sysutils.IntToHex(value,chars)+endcolor;
end;

function TDisAssembler.inttohexs_withsymbols(value:ptrUint;chars: integer; signed: boolean=false; signedsize: integer=0):string;
var found: boolean;
    b: byte;
    w: word;
    d: dword;
    i: integer;
begin
  if (showsymbols or showmodules) and (chars>=8) then
  begin
    found:=false;
    result:=symhandler.getNameFromAddress(value,showsymbols, showmodules, nil, @found,chars);

    //when found, and the symbol contains a space or comma, put the symbolname in quotes

    if found and ((pos(' ', result)>0) or (pos(',', result)>0)) then
    begin
      for i:=length(result) downto 1 do
      begin
        if (result[i] in ['-','+']) or (i=1) then
        begin
          if i>1 then
            result:='"'+copy(result, 1, i-1)+'"'+copy(result, i, length(result))
          else
            result:='"'+result+'"';
          break;
        end;
      end;
    end;

    if syntaxhighlighting then
    begin
      if not found then
        result:=colorhex+result+endcolor
      else
        result:=colorsymbol+result+endcolor;
    end;
  end
  else
    result:=inttohexs_withoutsymbols(value, chars, signed, signedsize);

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



