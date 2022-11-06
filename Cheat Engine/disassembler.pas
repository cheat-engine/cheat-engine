// Copyright Cheat Engine. All Rights Reserved.

unit disassembler;

{$MODE Delphi}

interface

{$ifdef jni}
uses unixporthelper, sysutils, byteinterpreter, symbolhandler, NewKernelHandler,
 ProcessHandlerUnit, LastDisassembleData, DisassemblerArm, commonTypeDefs;
{$endif}

{$ifdef windows}
uses windows, classes, imagehlp,sysutils,LCLIntf,byteinterpreter, symbolhandler, symbolhandlerstructs,
  CEFuncProc, NewKernelHandler, ProcessHandlerUnit, LastDisassembleData, disassemblerarm,
  commonTypeDefs, maps, math,vextypedef, betterControls, syncobjs;
{$endif}

{$ifdef darwin}
uses LCLIntf, LCLType, macport, macportdefines, classes,sysutils,byteinterpreter, symbolhandler, symbolhandlerstructs,
  CEFuncProc, ProcessHandlerUnit, LastDisassembleData, disassemblerarm,
  commonTypeDefs, maps, math,vextypedef, syncobjs, NewKernelHandler;
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
      ignoreLForExtraReg: boolean;
    end;
    inttohexs: TIntToHexS;
    RexPrefix: byte;
    riprelative: boolean;
    hasvex: boolean;
    hasvsib: boolean;

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
    faggressivealignment: boolean;


    modrmposition: TMRPos;

   // cs: TCriticalSection;

    ArmDisassembler: TArmDisassembler;

   // firstThread: dword;

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
    function xmm_ignoreL(bt:byte): string;
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
    showsections: boolean;
    dataOnly: boolean;

    is64bit: boolean;
    is64bitOverride: boolean;
    is64BitOverrideState: boolean;

    architecture: (darchAutoDetect, darchX86, darchArm);

    LastDisassembleData: TLastDisassembleData;
    MarkIPRelativeInstructions: boolean;

    debugpart: integer;

    context: PCONTEXT;

//    showvalues: boolean;
    function disassemble(var offset: ptrUint; var description: string): string; overload;
    function disassemble(var offset: ptrUint): string; overload;
    procedure splitDisassembledString(disassembled: string; showvalues: boolean; var address: string; var bytes: string; var opcode: string; var special:string; context: PContext=nil);

    function DecodeLastParametersToString: string; //returns the special line splitDisassemblerstring used to return

    function getLastBytestring: string;

    constructor create;
    destructor destroy; override;

  published
    property syntaxhighlighting: boolean read fsyntaxhighlighting write setSyntaxHighlighting;
    property OnDisassembleOverride: TDisassembleEvent read fOnDisassembleOverride write fOnDisassembleOverride;
    property OnPostDisassemble: TDisassembleEvent read fOnPostDisassemble write fOnPostDisassemble;
    property aggressivealignment: boolean read faggressivealignment write faggressivealignment;
  end;


  {$ifdef windows}
  TCR3Disassembler=class(TDisassembler)
  private
    fcr3: QWORD;
    procedure setCR3(c: QWORD);
  protected
    function readMemory(address: ptruint; destination: pointer; size: integer): integer; override;
  published
    property CR3: QWORD read fCR3 write setCR3;

  end;
  {$endif}




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
  Parsers, memoryQuery, binutils, luacaller, vmxfunctions, frmcodefilterunit,
  BreakpointTypeDef, frmEditHistoryUnit, dialogs;
{$endif}

{$ifdef darwin}
uses Assemblerunit,CEDebugger, debughelper, StrUtils, debuggertypedefinitions,
  Parsers, memoryQuery, (*binutils,*) LuaCaller, (*vmxfunctions, frmcodefilterunit, *)
  BreakpointTypeDef, frmEditHistoryUnit, dialogs;
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
    {$ifndef jni}
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
        8: result:='r8b';
        9: result:='r9b';
        10: result:='r10b';
        11: result:='r11b';
        12: result:='r12b';
        13: result:='r13b';
        14: result:='r14b';
        15: result:='r15b';
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
    8: result:='r8b';
    9: result:='r9b';
    10: result:='r10b';
    11: result:='r11b';
    12: result:='r12b';
    13: result:='r13b';
    14: result:='r14b';
    15: result:='r15b';
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


function TDisassembler.xmm_ignoreL(bt:byte): string;
var regnr: integer;
begin
  regnr:=getreg(bt);
  result:=colorreg+regnrtostr(rtXMM, regnr)+endcolor;
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

        if opperandsize<>0 then
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
                    4: if opcodeflags.L then result:='ymm0' else result:='xmm0';
                  end;

              1:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rcx' else result:='ecx';
                    1: result:='cx';
                    2: result:='cl';
                    3: result:='mm1';
                    4: if opcodeflags.L then result:='ymm1' else result:='xmm1';
                  end;

              2:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rdx' else result:='edx';
                    1: result:='dx';
                    2: result:='dl';
                    3: result:='mm2';
                    4: if opcodeflags.L then result:='ymm2' else result:='xmm2';
                  end;

              3:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rbx' else result:='ebx';
                    1: result:='bx';
                    2: result:='bl';
                    3: result:='mm3';
                    4: if opcodeflags.L then result:='ymm3' else result:='xmm3';
                  end;

              4:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rsp' else result:='esp';
                    1: result:='sp';
                    2: if rexprefix<>0 then result:='spl' else result:='ah';
                    3: result:='mm4';
                    4: if opcodeflags.L then result:='ymm4' else result:='xmm4';
                  end;

              5:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rbp' else result:='ebp';
                    1: result:='bp';
                    2: if rexprefix<>0 then result:='bpl' else result:='ch';
                    3: result:='mm5';
                    4: if opcodeflags.L then result:='ymm5' else result:='xmm5';
                  end;

              6:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rsi' else result:='esi';
                    1: result:='si';
                    2: if rexprefix<>0 then result:='sil' else result:='dh';
                    3: result:='mm6';
                    4: if opcodeflags.L then result:='ymm6' else result:='xmm6';
                  end;

              7:  case inst of
                    0: if rex_w or (opperandsize=64) then result:='rdi' else result:='edi';
                    1: result:='di';
                    2: if rexprefix<>0 then result:='dil' else result:='bh';
                    3: result:='mm7';
                    4: if opcodeflags.L then result:='ymm7' else result:='xmm7';
                  end;

              8: case inst of
                    0: if rex_w or (opperandsize=64) then result:='r8' else result:='r8d';
                    1: result:='r8w';
                    2: result:='r8l';
                    3: result:='mm8';
                    4: if opcodeflags.L then result:='ymm8' else result:='xmm8';
                 end;

              9: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r9' else result:='r9d';
                   1: result:='r9w';
                   2: result:='r9l';
                   3: result:='mm9';
                   4: if opcodeflags.L then result:='ymm9' else result:='xmm9';
                 end;

             10: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r10' else result:='r10d';
                   1: result:='r10w';
                   2: result:='r10l';
                   3: result:='mm10';
                   4: if opcodeflags.L then result:='ymm10' else result:='xmm10';
                 end;

             11: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r11' else result:='r11d';
                   1: result:='r11w';
                   2: result:='r11l';
                   3: result:='mm11';
                   4: if opcodeflags.L then result:='ymm11' else result:='xmm11';
                 end;

             12: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r12' else result:='r12d';
                   1: result:='r12w';
                   2: result:='r12b';
                   3: result:='mm12';
                   4: if opcodeflags.L then result:='ymm12' else result:='xmm12';
                 end;

             13: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r13' else result:='r13d';
                   1: result:='r13w';
                   2: result:='r13l';
                   3: result:='mm13';
                   4: if opcodeflags.L then result:='ymm13' else result:='xmm13';
                 end;

             14: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r14' else result:='r14d';
                   1: result:='r14w';
                   2: result:='r14l';
                   3: result:='mm14';
                   4: if opcodeflags.L then result:='ymm14' else result:='xmm14';
                 end;

             15: case inst of
                   0: if rex_w or (opperandsize=64) then result:='r15' else result:='r15d';
                   1: result:='r15w';
                   2: result:='r15l';
                   3: result:='mm15';
                   4: if opcodeflags.L then result:='ymm15' else result:='xmm15';
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
        4: if opcodeflags.L and (not opcodeflags.ignoreLForExtraReg) then ep:=regnrtostr(rtYMM, not opcodeflags.vvvv and $f) else ep:=regnrtostr(rtXMM, not opcodeflags.vvvv and $f);
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

  if hasvsib=false then
  begin
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

    if is64bit and (addresssize<>32) and (indexstring<>'') then
      indexstring[1]:='r'; //quick replace
  end
  else
  begin
    if opcodeflags.L then
      indexstring:='ymm'+inttostr(index)
    else
      indexstring:='xmm'+inttostr(index);
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
var
  bp: pbreakpoint;
  PA: qword;
  BO: integer;
begin
  if debuggerthread<>nil then
  begin
    debuggerthread.lockbplist;
    bp:=debuggerthread.isBreakpoint(address);
    if bp<>nil then
    begin
      if bp.breakpointMethod=bpmInt3 then
        b:=bp.originalbyte;
    end;
    debuggerthread.unlockbplist;

    {$ifdef windows}
    if (frmCodeFilter<>nil) then frmcodefilter.isBreakpoint(address, b);
    {$endif}
  end;

  {$ifdef windows}
  dbvm_isbreakpoint(address,PA,BO,b);
  {$endif}
end;
{$endif}

function disassemble(var offset: ptrUint; var description: string): string; overload;
begin
  result:=defaultDisassembler.disassemble(offset,description);
end;

{$ifdef windows}
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
 {$endif}

function TDisassembler.readMemory(address: ptruint; destination: pointer; size: integer): integer;
//reads the bytes at the given address and returns the number of bytes read
//in regards to the cloaked memory support, this is ONLY for the disassembler, to show the fuckery that's going on
var
  actualread: ptruint;

  i,p1,p2,p3: integer;
begin
  actualread:=0;

  {$ifdef windows}
  ReadProcessMemoryWithCloakSupport(processhandle,pointer(address),destination,size,actualread);
  {$else}
  ReadProcessMemory(processhandle,pointer(address),destination,size,actualread);
  {$endif}
  if (actualread=0) and ((address+size and qword($fffffffffffff000))>(address and qword($fffffffffffff000))) then //did not read a single byte and overlaps a pageboundary
  begin
    p1:=0;
    repeat
      i:=min(size, integer(4096-(address and $fff)));
      actualread:=0;
      {$ifdef windows}
      ReadProcessMemoryWithCloakSupport(processhandle,pointer(address),destination,i,actualread);
      {$else}
      ReadProcessMemory(processhandle,pointer(address),destination,i,actualread);
      {$endif}

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

function TDisassembler.disassemble(var offset: ptrUint): string;
var ignore: string;
begin
  result:=disassemble(offset, ignore);
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
begin

  hasvsib:=false;
  ZeroMemory(@opcodeflags,sizeof(opcodeflags));

  if (self = visibleDisassembler) and (GetCurrentThreadId<>MainThreadID) then
  begin
    offset:=1;
    description:='Should not happen';
    exit('Visible disassembler used in an external thread');
  end;

 {
  if firstthread=0 then
    firstthread:=GetCurrentThreadId;

  if firstthread<>GetCurrentThreadId then
  asm
  nop
  end;  }

  try
    {if cs.TryEnter=false then
    begin
      if GetCurrentThreadId=MainThreadID then
        MessageDlg('Multiple threads have accessed the same disassembler object at the same time. Do not do this', mtError,[mbok],0);

      OutputDebugString('Multiple threads are using the disassembler');
      cs.enter;
    end; }

   // cs.enter;

    //try
      debugpart:=0;

      LastDisassembleData.isfloat:=false;
      LastDisassembleData.isfloat64:=false;
      LastDisassembleData.iscloaked:=false;
      LastDisassembleData.commentsoverride:='';
      {$ifdef windows}
      if defaultBinutils<>nil then
      begin
        //use this
        LastDisassembleData.address:=offset;
        LastDisassembleData.SeperatorCount:=0;
        defaultBinutils.disassemble(LastDisassembleData);

        result:=inttohex(LastDisassembleData.address,8);
        result:=result+' - ';
        for i:=0 to length(LastDisassembleData.bytes)-1 do
          result:=result+inttohex(LastDisassembleData.Bytes[i],2)+' ';

        result:=result+' - ';
        result:=result+LastDisassembleData.opcode;
        result:=result+' ';
        result:=result+LastDisassembleData.parameters;

        if length(LastDisassembleData.bytes)>0 then
          inc(offset,length(LastDisassembleData.bytes))
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
        begin
          is64bit:=mi.is64bitmodule;
          if not is64bit then
          asm
          nop
          end;
        end;

      end;


      if (processhandler.SystemArchitecture=archarm) or (architecture=darchARM) then
      begin


        result:=ArmDisassembler.disassemble(offset);
        LastDisassembleData:=armdisassembler.LastDisassembleData;
        exit;
      end;

      modrmposition:=mNone;

      debugpart:=1;

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
      LastDisassembleData.isrep:=false;
      lastdisassembledata.isconditionaljump:=false;
      lastdisassembledata.modrmValueType:=dvtNone;
      lastdisassembledata.parameterValueType:=dvtNone;
      LastDisassembleData.hasSib:=false;
      LastDisassembleData.datasize:=0;
      LastDisassembleData.riprelative:=0;

      if assigned(OnDisassembleOverride) then //check if the user has defined it's own disassembler
      begin
        //if so, call the OnDisassemble propery, and if it returns true don't handle the original
        if OnDisassembleOverride(self, offset, LastDisassembleData, result, description) then
        begin
          if length(lastdisassembledata.Bytes)=0 then //BAD!
            setlength(lastdisassembledata.Bytes,1);

          inc(offset, length(lastdisassembledata.Bytes));
          exit;
        end;
      end;

      //also check global overrides
      for i:=0 to length(GlobalDisassembleOverrides)-1 do
      begin
        if assigned(GlobalDisassembleOverrides[i]) then
        begin
          if GlobalDisassembleOverrides[i](self, offset, LastDisassembleData, result, description) then
          begin
            if length(lastdisassembledata.Bytes)=0 then //BAD!
              setlength(lastdisassembledata.Bytes,1);

            inc(offset, length(lastdisassembledata.Bytes));
            exit;
          end;
        end;
      end;
      debugpart:=2;



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

      debugpart:=3;

      if (actualread>0) and (actualread<=32) then
      begin

        {$ifndef jni}
        if debuggerthread<>nil then
          for i:=0 to actualread-1 do
            if memory[i]=$cc then
            begin
              //memory[i]:=debuggerthread.getrealbyte(offset+i);

              repairbreakbyte(offset+i, memory[i]);
            end;
        {$endif}

        debugpart:=4;

        while isprefix do
        begin
          inc(offset); //offset will always inc by 1
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

            memory:=@memory[1];
            if offset>initialoffset+24 then //too long
            begin
              description:='';
              LastDisassembleData.opcode:='??';
              offset:=initialoffset+1;
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
          LastDisassembleData.isrep:=true;
        end;

        if $f3 in prefix2 then
        begin
          tempresult:=tempresult+'repe ';
          noVEXPossible:=true;
          LastDisassembleData.isrep:=true;
        end;

        LastDisassembleData.prefix:=tempresult;

        debugpart:=5;

        zeromemory(@opcodeflags, sizeof(opcodeflags));

        RexPrefix:=0;
        if is64bit then
        begin
          if memory[0] in [$40..$4f] then //does it start with a rex prefix ?
          begin
            debugpart:=6;
            setlength(LastDisassembleData.bytes,length(LastDisassembleData.bytes)+1);
            LastDisassembleData.bytes[length(LastDisassembleData.bytes)-1]:=memory[0];

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
            debugpart:=7;

            if offset>initialoffset+24 then
            begin
              description:='';
              LastDisassembleData.opcode:='??';
              offset:=initialoffset+1;
              exit;
            end;
          end

        end;




        prefixsize:=length(LastDisassembleData.bytes);
        LastDisassembleData.prefixsize:=prefixsize;

        debugpart:=8;
        if (noVEXPossible=false) and (memory[0] in [$c4,$c5]) then
        begin
          debugpart:=9;
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

            i:=length(LastDisassembleData.bytes);

            setlength(LastDisassembleData.bytes,length(LastDisassembleData.bytes)+2);
            LastDisassembleData.bytes[i]:=memory[0];
            LastDisassembleData.bytes[i+1]:=memory[1];

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

            i:=length(LastDisassembleData.bytes);
            setlength(LastDisassembleData.bytes,i+3);
            LastDisassembleData.bytes[i]:=memory[0];
            LastDisassembleData.bytes[i+1]:=memory[1];
            LastDisassembleData.bytes[i+2]:=memory[2];

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
            inc(offset,bytestomove);
          end;

          case opcodeflags.pp of
            1: prefix2:=prefix2+[$66];
            2: prefix2:=prefix2+[$f3];
            3: prefix2:=prefix2+[$f2];
          end;
        end else hasVEX:=false;

        debugpart:=10;

        //compatibility fix for code that still checks for rex.* or sets it as a temporary flag replacement
        RexPrefix:=ifthen(opcodeflags.B, rexprefix or BIT_REX_B, rexprefix);
        RexPrefix:=ifthen(opcodeflags.X, rexprefix or BIT_REX_X, rexprefix);
        RexPrefix:=ifthen(opcodeflags.R, rexprefix or BIT_REX_R, rexprefix);
        RexPrefix:=ifthen(opcodeflags.W, rexprefix or BIT_REX_W, rexprefix);

        debugpart:=11;
        case memory[0] of  //opcode
          $00 : begin


                  if (aggressivealignment and (((offset) and $f)=0) and (memory[1]<>0) ) or ((memory[1]=$55) and (memory[2]=$89) and (memory[3]=$e5)) then
                  begin
                    description:='Filler';
                    lastdisassembledata.opcode:='db';
                    LastDisassembleData.parameters:=inttohex(memory[0],2);
                  end
                  else
                  begin
                    description:='Add';

                    lastdisassembledata.opcode:='add';
                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);

                    inc(offset,last-1);
                  end;
                end;

          $01 : begin
                  description:='Add';

                  lastdisassembledata.opcode:='add';
                  if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]) else
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last)+r32(memory[1]);


                  inc(offset,last-1);
                end;

          $02 : begin
                  description:='Add';

                  LastDisassembleData.opcode:='add';
                  LastDisassembleData.parameters:=r8(memory[1])+MODRM(memory,prefix2,1,2,last, mRight);

                  inc(offset,last-1);
                end;

          $03 : begin
                  description:='Add';
                  LastDisassembleData.opcode:='add';
                  if $66 in prefix2 then LastDisassembleData.parameters:=r16(memory[1])+MODRM(memory,prefix2,1,1,last, mright) else
                                         LastDisassembleData.parameters:=r32(memory[1])+MODRM(memory,prefix2,1,0,last, mright);


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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(dwordptr^),8);

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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last, mRight);
                  inc(offset,last-1);
                end;

          $0b : begin
                  description:='logical inclusive or';
                  lastdisassembledata.opcode:='or';
                  if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last, mRight) else
                                         lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last, mRight);

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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(lastdisassembledata.parametervalue),8);
                      description:=description+' (sign-extended)';
                    end
                    else
                      lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(lastdisassembledata.parametervalue,8);


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
                  if $f0 in prefix2 then
                    lastdisassembledata.prefix:='lock '
                  else
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
                             else
                             begin
                               lastdisassembledata.opcode:='db';
                               LastDisassembleData.parameters:=inttohex(memory[0],2);
                               description:='not specified by the intel documentation';
                             end;

                            end;

                          end;

                    $01 : begin
                            case memory[2] of
                              $c1:
                              begin
                                description:='call to vm monitor by causing vm exit';
                                lastdisassembledata.opcode:='vmcall';
                                inc(offset,2);
                              end;

                              $c2:
                              begin
                                description:='launch virtual machine managed by current vmcs';
                                lastdisassembledata.opcode:='vmlaunch';
                                inc(offset,2);
                              end;

                              $c3:
                              begin
                                description:='resume virtual machine managed by current vmcs';
                                lastdisassembledata.opcode:='vmresume';
                                inc(offset,2);
                              end;

                              $c4:
                              begin
                                description:='leaves vmx operation';
                                lastdisassembledata.opcode:='vmxoff';
                                inc(offset,2);
                              end;

                              $c8:
                              begin
                                description:='set up monitor address';
                                lastdisassembledata.opcode:='monitor';
                                inc(offset,2);
                              end;

                              $c9:
                              begin
                                description:='Monitor wait';
                                lastdisassembledata.opcode:='mwait';
                                inc(offset,2);
                              end;

                              $ca:
                              begin
                                description:='Clear AC flag in EFLAGS register';
                                lastdisassembledata.opcode:='clac';
                                inc(offset,2);
                              end;

                              $d0:
                              begin
                                description:='Get value of extended control register';
                                lastdisassembledata.opcode:='xgetbv';
                                inc(offset,2);
                              end;

                              $d1:
                              begin
                                description:='Set value of extended control register';
                                lastdisassembledata.opcode:='xsetbv';
                                inc(offset,2);
                              end;

                              $d5:
                              begin
                                description:='Transactional end';
                                lastdisassembledata.opcode:='xend';
                                inc(offset,2);
                              end;

                              $d6:
                              begin
                                description:='Test if in transactional execution';
                                lastdisassembledata.opcode:='xtest';
                                inc(offset,2);
                              end;

                              $f8:
                              begin
                                description:='Swap GS base register';
                                lastdisassembledata.opcode:='swapgs';
                                inc(offset,2);
                              end;

                              $f9:
                              begin
                                description:='Read time-stamp counter and processor ID';
                                lastdisassembledata.opcode:='rdtscp';
                                inc(offset,2);
                              end;

                              else
                              begin
                                case getreg(memory[2]) of
                                  0:
                                  begin
                                    description:='store global descriptor table register';
                                    lastdisassembledata.opcode:='sgdt';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                                  1:
                                  begin
                                    description:='store interrupt descriptor table register';
                                    lastdisassembledata.opcode:='sidt';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                                  2:
                                  begin
                                    description:='load global descriptor table register';
                                    lastdisassembledata.opcode:='lgdt';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                                  3:
                                  begin
                                    description:='load interupt descriptor table register';
                                    lastdisassembledata.opcode:='lidt';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                                  4:
                                  begin
                                    description:='store machine status word';
                                    lastdisassembledata.opcode:='smsw';

                                    if $66 in prefix2 then lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)
                                                      else lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                                  6:
                                  begin
                                    description:='load machine status word';
                                    lastdisassembledata.opcode:='lmsw';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last);
                                    inc(offset,last-1);
                                  end;

                                  7:
                                  begin
                                    description:='invalidate tlb entry';
                                    lastdisassembledata.opcode:='invplg';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;
                                end;
                              end;
                            end;
                          end;

                    $02 : begin
                            description:='load access rights byte';
                            lastdisassembledata.opcode:='lar';
                            if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                                   lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last, mRight);

                            inc(offset,last-1);
                          end;

                {0f}$03 : begin
                            description:='load segment limit';
                            lastdisassembledata.opcode:='lsl';
                            if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                                   lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last, mRight);

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
                            lastdisassembledata.opcode:='invd';
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

                    $0d : begin
                            case getreg(memory[2]) of
                              1:  begin
                                    description:='Prefetch Data into Caches in Anticipation of a Write';
                                    lastdisassembledata.opcode:='prefetchw';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                    inc(offset,last-1);
                                  end;

                              2:  begin
                                     description:='Prefetch Vector Data Into Caches with Intent to Write and T1 Hint';
                                     lastdisassembledata.opcode:='prefetchwt1';
                                     lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last);
                                     inc(offset,last-1);
                                   end;
                            end;
                          end;


                    $10 : begin
                            lastdisassembledata.isfloat:=true;

                            if $f2 in prefix2 then
                            begin
                              description:='move scalar double-fp';
                              opcodeflags.L:=false; //LIG
                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.isfloat64:=true;

                              if hasvex then
                                lastdisassembledata.opcode:='vmovsd'
                              else
                                lastdisassembledata.opcode:='movsd';

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='move scalar single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovss'
                              else
                                lastdisassembledata.opcode:='movss';

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move unaligned packed double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='lmovupd'
                              else
                                lastdisassembledata.opcode:='movupd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move unaligned four packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovups'
                              else
                                lastdisassembledata.opcode:='movups';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $11 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              description:='move scalar double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovsd'
                              else
                                lastdisassembledata.opcode:='movsd';

                              lastdisassembledata.isfloat64:=true;

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last,mLeft)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='move scalar single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovss'
                              else
                                lastdisassembledata.opcode:='movss';

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move unaligned packed double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='lmovupd'
                              else
                                lastdisassembledata.opcode:='movupd';

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move unaligned four packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovups'
                              else
                                lastdisassembledata.opcode:='movups';

                              opcodeflags.skipExtraRegOnMemoryAccess:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;

                          end;

                {0f}$12 : begin
                            if $f2 in prefix2 then
                            begin
                              description:='move one double-fp and duplicate';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovddup'
                              else
                                lastdisassembledata.opcode:='movddup';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='move packed single-fp Low and duplicate';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovsldup'
                              else
                                lastdisassembledata.opcode:='movsldup';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move low packed double-precision floating-point value';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovlpd'
                              else
                                lastdisassembledata.opcode:='movlpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='high to low packed single-fp';

                              if getmod(memory[2])=3 then
                                lastdisassembledata.opcode:='movhlps'
                              else
                                lastdisassembledata.opcode:='movlps';

                              if hasvex then
                                lastdisassembledata.opcode:='v'+lastdisassembledata.opcode;

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $13 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='move low packed double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovlpd'
                              else
                                lastdisassembledata.opcode:='movlpd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move low packed single-fp';

                              if hasvex then
                                lastdisassembledata.opcode:='vmovlps'
                              else
                                lastdisassembledata.opcode:='movlps';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                {0f}$14 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='unpack low packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vunpcklpd'
                              else
                                lastdisassembledata.opcode:='unpcklpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack low packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vunpcklps'
                              else
                                lastdisassembledata.opcode:='unpcklps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $15 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='unpack and interleave high packed double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vunpckhpd'
                              else
                                lastdisassembledata.opcode:='unpckhpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack high packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='unpckhps'
                              else
                                lastdisassembledata.opcode:='unpckhps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $16 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f3 in prefix2 then
                            begin
                              description:='move packed single-fp high and duplicate';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovshdup'
                              else
                                lastdisassembledata.opcode:='movshdup';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move high packed double-precision floating-point value';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovhpd'
                              else
                                lastdisassembledata.opcode:='movhpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='high to low packed single-fp';

                              if getmod(memory[2])=3 then
                                lastdisassembledata.opcode:='movlhps'
                              else
                                lastdisassembledata.opcode:='movhps';

                              if hasvex then
                                lastdisassembledata.opcode:='v'+lastdisassembledata.opcode;

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $17 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='move high packed double-precision floating-point value';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovhpd'
                              else
                                lastdisassembledata.opcode:='movhpd';

                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='high to low packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovhps'
                              else
                                lastdisassembledata.opcode:='movhps';
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              lastdisassembledata.datasize:=4;
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


                                    if Rex_W then
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last,64)
                                    else
                                    begin
                                      if $66 in prefix2 then
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last,16)
                                      else
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last,32)
                                    end;

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
                            lastdisassembledata.parameters:=cr(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                            inc(offset,last-1);
                          end;

                    $23 : begin
                            description:='move to debug register';
                            lastdisassembledata.opcode:='mov';
                            lastdisassembledata.parameters:=dr(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                            inc(offset,last-1);
                          end;

                    $28 : begin
                            if $66 in prefix2 then
                            begin
                              description:='move aligned packed double-fp values';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovapd'
                              else
                                lastdisassembledata.opcode:='movapd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move aligned four packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovaps'
                              else
                                lastdisassembledata.opcode:='movaps';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $29 : begin
                            if $66 in prefix2 then
                            begin
                              description:='move aligned packed double-fp values';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovapd'
                              else
                                lastdisassembledata.opcode:='movapd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move aligned four packed single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovaps'
                              else
                                lastdisassembledata.opcode:='movaps';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end;
                          end;


                    $2a : begin
                            if $f2 in prefix2 then
                            begin

                              description:='convert doubleword integer to scalar doubleprecision floating-point value';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtsi2sd'
                              else
                                lastdisassembledata.opcode:='cvtsi2sd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin

                              description:='scalar signed int32 to single-fp conversion';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtsi2ss'
                              else
                                lastdisassembledata.opcode:='cvtsi2ss';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='convert packed dword''s to packed dp-fp''s';
                                lastdisassembledata.opcode:='cvtpi2pd';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                                inc(offset,last-1);
                              end
                              else
                              begin
                                description:='packed signed int32 to packed single-fp conversion';
                                lastdisassembledata.opcode:='cvtpi2ps';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $2b : begin
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmovntpd'
                              else
                                lastdisassembledata.opcode:='movntpd';
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              description:='move packed double-precision floating-point using non-temporal hint';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmovntps'
                              else
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
                              if hasvex then
                                lastdisassembledata.opcode:='vcvttsd2si'
                              else
                                lastdisassembledata.opcode:='cvttsd2si';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='scalar single-fp to signed int32 conversion (truncate)';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvttss2si'
                              else
                                lastdisassembledata.opcode:='cvttss2si';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='packed doubleprecision-fp to packed dword conversion (truncate)';
                                lastdisassembledata.opcode:='cvttpd2pi';
                                lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                                inc(offset,last-1);
                              end
                              else
                              begin
                                description:='packed single-fp to packed int32 conversion (truncate)';
                                lastdisassembledata.opcode:='cvttps2pi';
                                lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $2d : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              description:='convert scalar double-precision floating-point value to doubleword integer';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtsd2si'
                              else
                                lastdisassembledata.opcode:='cvtsd2si';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='scalar single-fp to signed int32 conversion';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtss2si'
                              else
                                lastdisassembledata.opcode:='cvtss2si';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='convert 2 packed dp-fp''s from param 2 to packed signed dword in param1';
                                lastdisassembledata.opcode:='cvtpi2ps';
                                lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                                inc(offset,last-1);
                              end
                              else
                              begin
                                description:='packed single-fp to packed int32 conversion';
                                lastdisassembledata.opcode:='cvtps2pi';
                                lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                                lastdisassembledata.datasize:=4;
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $2e : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='unordered scalar double-fp compare and set eflags';
                              if hasvex then
                                lastdisassembledata.opcode:='vucomisd'
                              else
                                lastdisassembledata.opcode:='ucomisd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unordered scalar single-fp compare and set eflags';
                              if hasvex then
                                lastdisassembledata.opcode:='vucomiss'
                              else
                                lastdisassembledata.opcode:='ucomiss';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;


                    $2f : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='compare scalar ordered double-precision floating point values and set eflags';
                              if hasvex then
                                lastdisassembledata.opcode:='vcomisd'
                              else
                                lastdisassembledata.opcode:='comisd';
                              opcodeflags.skipExtraReg:=true;

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='scalar ordered single-fp compare and set eflags';
                              if hasvex then
                                lastdisassembledata.opcode:='vcomiss'
                              else
                                lastdisassembledata.opcode:='comiss';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;
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
                            lastdisassembledata.isret:=true;
                            inc(offset);
                          end;

                    $35: begin
                            description:='fast transistion from system call entry point';
                            lastdisassembledata.opcode:='sysexit';
                            inc(offset);
                          end;

                    $37: begin
                            description:='Safermode multipurpose function';
                            lastdisassembledata.opcode:='getsec';
                            inc(offset);
                          end;

               {0f} $38:  begin
                            case memory[2] of
                              $00: begin
                                     description:='Packed shuffle bytes';
                                     LastDisassembleData.opcode:='pshufb';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $01: begin
                                     description:='Packed horizontal add';
                                     LastDisassembleData.opcode:='phaddw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

            {0f}{38}          $02: begin
                                     description:='Packed horizontal add';
                                     LastDisassembleData.opcode:='phaddd';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $03: begin
                                     description:='Packed horizontal add and saturate';
                                     LastDisassembleData.opcode:='phaddsw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $04: begin
                                     description:='Multiply and add signed and unsigned bytes';
                                     LastDisassembleData.opcode:='pmaddubsw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                {0f}{38}      $05: begin
                                     description:='Packed horizontal subtract';
                                     LastDisassembleData.opcode:='phsubw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $06: begin
                                     description:='Packed horizontal subtract';
                                     LastDisassembleData.opcode:='phsubd';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $07: begin
                                     description:='Packed horizontal subtract';
                                     LastDisassembleData.opcode:='phsubsw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $08: begin
                                     description:='Packed SIGN';
                                     LastDisassembleData.opcode:='psignb';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $09: begin
                                     description:='Packed SIGN';
                                     LastDisassembleData.opcode:='psignw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

                              $0a: begin
                                     description:='Packed SIGN';
                                     LastDisassembleData.opcode:='psignd';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

              {0f}{38}        $0b: begin
                                     description:='Packed multiply high with round and scale';
                                     LastDisassembleData.opcode:='phmulhrsw';

                                     if $66 in prefix2 then
                                     begin
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;
                                     end
                                     else
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                     inc(offset,last-1);
                                   end;

              {0f}{38}        $0c: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         description:='permute single-precision floating-point values';
                                         LastDisassembleData.opcode:='vpermilps';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

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
                                         LastDisassembleData.opcode:='vpermilpd';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

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
                                         LastDisassembleData.opcode:='vtestps';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

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
                                         LastDisassembleData.opcode:='vtestpd';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);

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
                                         LastDisassembleData.opcode:='vpblendvb';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+','+regnrtostr(rtXMM,memory[last]);
                                         inc(offset,1);
                                       end
                                       else
                                       begin
                                         LastDisassembleData.opcode:='pblendvb';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+regnrtostr(rtXMM,0);
                                       end;
                                       inc(offset,last-1);
                                     end;
                                   end;

                    {0f}{38}  $13: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Convert 16-bit FP values to single-precision FP values';
                                       LastDisassembleData.opcode:='vcvtph2ps';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;


                              $14: begin
                                     description:='Variable blend packed single precision floating-point values';
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         LastDisassembleData.opcode:='vblendvps';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+','+regnrtostr(rtXMM,memory[last]);
                                         inc(offset,1);
                                       end
                                       else
                                       begin
                                         LastDisassembleData.opcode:='blendvps';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+regnrtostr(rtXMM,0);
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
                                         LastDisassembleData.opcode:='vblendvpd invalid';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+','+regnrtostr(rtXMM,memory[last]);
                                         inc(offset,1);
                                       end
                                       else
                                       begin
                                         LastDisassembleData.opcode:='blendvpd';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight)+','+colorreg+regnrtostr(rtXMM,0)+endcolor;
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
                                         LastDisassembleData.opcode:='vpermps';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                         inc(offset,last-1);
                                       end;
                                     end
                                   end;

                              $17: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Logical compare';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vptest'
                                       else
                                         LastDisassembleData.opcode:='ptest';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end
                                   end;

                              $18: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         description:='Broadcast floating-point-data';
                                         LastDisassembleData.opcode:='vbroadcastss';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
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
                                         LastDisassembleData.opcode:='vbroadcastsd';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
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
                                         LastDisassembleData.opcode:='vbroadcastf128';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                         inc(offset,last-1);
                                       end;
                                     end
                                   end;

              {0f}{38}        $1c: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed absolute value';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpabsb'
                                       else
                                         LastDisassembleData.opcode:='pabsb';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Packed absolute value';
                                       LastDisassembleData.opcode:='pabsb';
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $1d: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed absolute value';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpabsw'
                                       else
                                         LastDisassembleData.opcode:='pabsw';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Packed absolute value';
                                       LastDisassembleData.opcode:='pabsw';
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $1e: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed absolute value';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpabsd'
                                       else
                                         LastDisassembleData.opcode:='pabsd';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Packed absolute value';
                                       LastDisassembleData.opcode:='pabsd';
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $20: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxbw'
                                       else
                                         LastDisassembleData.opcode:='pmovsxbw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $21: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxbd'
                                       else
                                         LastDisassembleData.opcode:='pmovsxbd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $22: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxbq'
                                       else
                                         LastDisassembleData.opcode:='pmovsxbq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $23: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxwd'
                                       else
                                         LastDisassembleData.opcode:='pmovsxwd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $24: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxwq'
                                       else
                                         LastDisassembleData.opcode:='pmovsxwq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $25: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with sign extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovsxdq'
                                       else
                                         LastDisassembleData.opcode:='pmovsxdq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $28: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Multiple packed signed dword integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmuldq'
                                       else
                                         LastDisassembleData.opcode:='pmuldq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $29: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Compare packed qword data for equal';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpcmpeqq'
                                       else
                                         LastDisassembleData.opcode:='pcmpeqq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $2a: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Load double quadword non-temporal aligned hint';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vmovntdqa'
                                       else
                                         LastDisassembleData.opcode:='movntdqa';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $2b: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Pack with unsigned saturation';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpackusdw'
                                       else
                                         LastDisassembleData.opcode:='packusdw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $2c: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         description:='Conditional SIMD packed loads and stores';
                                         LastDisassembleData.opcode:='vmaskmovps';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
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
                                         LastDisassembleData.opcode:='vmaskmovpd';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
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
                                         LastDisassembleData.opcode:='vmaskmovps';
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,3,4,last, mLeft)+xmm(memory[3]);
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
                                         LastDisassembleData.opcode:='vmaskmovpd';
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,3,4,last, mLeft)+xmm(memory[3]);
                                         inc(offset,last-1);
                                       end;
                                     end;
                                   end;

              {0f}{38}        $30: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxbw'
                                       else
                                         LastDisassembleData.opcode:='pmovzxbw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $31: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxbd'
                                       else
                                         LastDisassembleData.opcode:='pmovzxbd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $32: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxbq'
                                       else
                                         LastDisassembleData.opcode:='pmovzxbq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $33: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxwd'
                                       else
                                         LastDisassembleData.opcode:='pmovzxwd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $34: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxwq'
                                       else
                                         LastDisassembleData.opcode:='pmovzxwq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $35: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed move with zero extend';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmovzxdq'
                                       else
                                         LastDisassembleData.opcode:='pmovzxdq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $36: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         description:='Full doublewords element permutation';
                                         LastDisassembleData.opcode:='vpermd';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                         inc(offset,last-1);
                                       end;
                                     end;
                                   end;


              {0f}{38}        $37: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Compare packed data for greater than';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpcmpgtq'
                                       else
                                         LastDisassembleData.opcode:='pcmpgtq';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $38: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Minimum of packed signed byte integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpminsb'
                                       else
                                         LastDisassembleData.opcode:='pminsb';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $39: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Minimum of packed dword integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpminsd'
                                       else
                                         LastDisassembleData.opcode:='pminsd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3a: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Minimum of packed word integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpminuw'
                                       else
                                         LastDisassembleData.opcode:='pminuw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3b: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Minimum of packed dword integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpminud'
                                       else
                                         LastDisassembleData.opcode:='pminud';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3c: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Maximum of packed signed byte integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmaxsb'
                                       else
                                         LastDisassembleData.opcode:='pmaxsb';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3d: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Maximum of packed signed dword integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmaxsd'
                                       else
                                         LastDisassembleData.opcode:='pmaxsd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3e: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Maximum of packed word integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmaxuw'
                                       else
                                         LastDisassembleData.opcode:='pmaxuw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $3f: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Maximum of packed unsigned dword integers';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmaxud'
                                       else
                                         LastDisassembleData.opcode:='pmaxud';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

               {0f}{38}       $40: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Multiply Packed Signed Dword Integers and Store Low Result';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpmulld'
                                       else
                                         LastDisassembleData.opcode:='pmulld';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

               {0f}{38}       $41: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed horitontal word minimum';
                                       if hasvex then
                                         LastDisassembleData.opcode:='phminposuw'
                                       else
                                         LastDisassembleData.opcode:='vphminposuw';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vpsrlvq'
                                         else
                                           LastDisassembleData.opcode:='vpsrlvd';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
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
                                         LastDisassembleData.opcode:='vpsravd';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vpsllvq'
                                         else
                                           LastDisassembleData.opcode:='vpsllvd';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
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
                                         LastDisassembleData.opcode:='vpbroadcastd';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                         LastDisassembleData.opcode:='vpbroadcastq';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                         LastDisassembleData.opcode:='vpbroadcasti128';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                         LastDisassembleData.opcode:='vpbroadcastb';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                         LastDisassembleData.opcode:='vpbroadcastw';
                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                         inc(offset,last-1);
                                       end;
                                     end;
                                   end;

                              $82: begin
                                     description:='Invalidate process-context-identifier';
                                     LastDisassembleData.opcode:='invpcid';
                                     if processhandler.is64Bit then
                                       lastdisassembledata.parameters:=r64(memory[3])+modrm(memory,prefix2,3,0,last,128,0, mRight)
                                     else
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,128,0,mRight);

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
                                           LastDisassembleData.opcode:='vpmaskmovq';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Conditional SIMD Integer Packed Loads and Stores';
                                           LastDisassembleData.opcode:='vpmaskmovd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vpmaskmovq';
                                           lastdisassembledata.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3]);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Conditional SIMD Integer Packed Loads and Stores';
                                           LastDisassembleData.opcode:='vpmaskmovd';
                                           lastdisassembledata.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3]);
                                           inc(offset,last-1);
                                         end;
                                       end;
                                     end;
                                   end;

                              $92: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         if Rex_W then
                                         begin
                                           description:='Gather Packed DP FP Values Using Signed Dword/Qword Indices';
                                           hasvsib:=true;
                                           LastDisassembleData.opcode:='vgatherdpd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+','+modrm(memory,prefix2,3,4,last,32);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Gather Packed SP FP values Using Signed Dword/Qword Indices';
                                           hasvsib:=true;
                                           LastDisassembleData.opcode:='vgatherdps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+','+modrm(memory,prefix2,3,4,last,32);
                                           inc(offset,last-1);
                                         end

                                       end;
                                     end;
                                   end;

                              $93: begin
                                     if $66 in prefix2 then
                                     begin
                                       if hasvex then
                                       begin
                                         if Rex_W then
                                         begin
                                           description:='Gather Packed SP FP values Using Signed Dword/Qword Indices';
                                           hasvsib:=true;
                                           LastDisassembleData.opcode:='vgatherqpd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+','+modrm(memory,prefix2,3,4,last,mLeft);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Gather Packed SP FP values Using Signed Dword/Qword Indices';
                                           hasvsib:=true;
                                           LastDisassembleData.opcode:='vgatherqps';
                                           opcodeflags.ignoreLForExtraReg:=true;
                                           lastdisassembledata.parameters:=xmm_ignoreL(memory[3])+','+modrm(memory,prefix2,3,4,last,mLeft);
                                           inc(offset,last-1);
                                         end

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
                                           LastDisassembleData.opcode:='vfmaddsub132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-alnterating add/subtract of precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmaddsub132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsubadd132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-alnterating subtract/add of precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsubadd132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd132sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd132ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub132sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub132ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd132sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd132ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub132pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub132ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub132sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub132ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmaddsub213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmaddsub213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsubadd213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiply-alternating subtract/add of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsubadd213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd213sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd213ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub213sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub213ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd213sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd213ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub213pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub213ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmsub213sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmsub213ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmaddsub231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmaddsub231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsubadd231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiply-alternating add/subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsubadd231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmadd231sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmadd231ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub231sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused multiple-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub231ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of packed single-precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfnmadd231sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-add of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfnmadd231ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub231pd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused negative multiply-subtract of packed single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub231ps';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                           LastDisassembleData.opcode:='vfmsub231sd';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                           inc(offset,last-1);
                                         end
                                         else
                                         begin
                                           description:='Fused begative multiply-subtract of scalar single precision floating-point-values';
                                           LastDisassembleData.opcode:='vfmsub231ss';
                                           lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
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
                                         LastDisassembleData.opcode:='vaesimc'
                                       else
                                         LastDisassembleData.opcode:='aesimc';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $dc: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Perform one round of an AES encryption flow';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vaesenc'
                                       else
                                         LastDisassembleData.opcode:='aesenc';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $dd: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Perform last round of an AES encryption flow';
                                       if hasvex then
                                         LastDisassembleData.opcode:='caesenclast'
                                       else
                                         LastDisassembleData.opcode:='aesenclast';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

                    {0f}{38}  $de: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Perform one round of an AES decryption flow';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vaesdec'
                                       else
                                         LastDisassembleData.opcode:='aesdec';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $df: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Perform last round of an AES decryption flow';
                                       if hasvex then
                                         LastDisassembleData.opcode:='caesdeclast'
                                       else
                                         LastDisassembleData.opcode:='aesdeclast';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mright);
                                       inc(offset,last-1);
                                     end;
                                   end;

                {0f}{38}     $f0: begin
                                     if $f2 in prefix2 then
                                     begin
                                       description:='Accumulate CRC32 value';
                                       LastDisassembleData.opcode:='crc32';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,2,last,mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Move data after swapping bytes';
                                       LastDisassembleData.opcode:='movbe';
                                       if $66 in prefix2 then
                                         lastdisassembledata.parameters:=r16(memory[3])+modrm(memory,prefix2,3,2,last,mRight)
                                       else
                                         lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $f1: begin
                                     if $f2 in prefix2 then
                                     begin
                                       description:='Accumulate CRC32 value';
                                       LastDisassembleData.opcode:='crc32';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Move data after swapping bytes';
                                       LastDisassembleData.opcode:='movbe';
                                       lastdisassembledata.parameters:=modrm(memory,prefix2,3,0,last, mLeft)+r32(memory[3]);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $f2: begin
                                     if hasvex then
                                     begin
                                       description:='Logical AND NOT';
                                       LastDisassembleData.opcode:='andn';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

              {0f}{38}        $f3: begin
                                     case getreg(memory[3]) of
                                       1:
                                       begin
                                         description:='Reset lowerst set bit';
                                         LastDisassembleData.opcode:='blsr';
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                         inc(offset,last-1);
                                       end;

                                       2:
                                       begin
                                         description:='Get mask up to lowest set bit';
                                         LastDisassembleData.opcode:='blsmsk';
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                         inc(offset,last-1);
                                       end;

                                       3:
                                       begin
                                         description:='Extract lowest set isolated bit';
                                         LastDisassembleData.opcode:='blsi';
                                         lastdisassembledata.parameters:=modrm(memory,prefix2,3,0,last, mRight);
                                         inc(offset,last-1);
                                       end;
                                     end;
                                   end;

                              $f5: begin
                                     if $f2 in prefix2 then
                                     begin
                                       description:='Parallel bits deposit';
                                       LastDisassembleData.opcode:='pdep';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mLeft);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       description:='Zero high bits starting with specified bit position';
                                       LastDisassembleData.opcode:='bzhi';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mLeft);
                                       inc(offset,last-1);
                                     end;
                                   end;


                              $f6: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='ADX: Unsigned Integer Addition of Two Operands with Carry Flag';
                                       LastDisassembleData.opcode:='adcx';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     if $f3 in prefix2 then
                                     begin
                                       description:='ADX: Unsigned Integer Addition of Two Operands with Overflow Flag';
                                       LastDisassembleData.opcode:='adox';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       if hasvex then
                                       begin
                                         description:='Unsigned multiple without affecting flags';
                                         LastDisassembleData.opcode:='mulx';
                                         lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight);
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
                                         LastDisassembleData.opcode:='SARX';
                                         lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                         inc(offset,last-1);
                                       end
                                       else
                                       if $f2 in prefix2 then
                                       begin
                                         description:='Shift logically right without affecting flags';
                                         LastDisassembleData.opcode:='SHRX';
                                         lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                         inc(offset,last-1);
                                       end
                                       else
                                       if $66 in prefix2 then
                                       begin
                                         description:='Shift logically left without affecting flags';
                                         LastDisassembleData.opcode:='SHLX';
                                         lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                         inc(offset,last-1);
                                       end;
                                     end
                                     else
                                     begin
                                       description:='Bit field extract';
                                       LastDisassembleData.opcode:='BEXTR';
                                       lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last,mRight);
                                       inc(offset,last-1);
                                     end;
                                   end;

                                  else
                                  begin
                                    if hasvex then
                                    begin
                                      LastDisassembleData.opcode:='unknown avx 0F38 '+inttohex(memory[2],2);
                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);

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
                                         LastDisassembleData.opcode:='vpermq';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vpermpd';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vblenddd';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vpermilps';

                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vpermilpd';

                                         opcodeflags.skipExtraReg:=true;
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vperm2f128';

                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vroundps'
                                       else
                                         LastDisassembleData.opcode:='roundps';

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $09: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Round packed double precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vroundpd'
                                       else
                                         LastDisassembleData.opcode:='roundpd';

                                       opcodeflags.skipExtraReg:=true;

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $0a: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Round scalar single precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vroundss'
                                       else
                                         LastDisassembleData.opcode:='roundss';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $0b: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Round packed single precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vroundsd'
                                       else
                                         LastDisassembleData.opcode:='roundsd';



                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                   {0f}{3a}   $0c: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Blend packed single precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vblendps'
                                       else
                                         LastDisassembleData.opcode:='blendps';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                   {0f}{3a}   $0d: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Blend packed double precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vblendpd'
                                       else
                                         LastDisassembleData.opcode:='blendpd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                   {0f}{3a}   $0e: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Blend packed words';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpblendw'
                                       else
                                         LastDisassembleData.opcode:='pblendw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                   {0f}{3a}   $0f: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Packed align right';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpalignr'
                                       else
                                         LastDisassembleData.opcode:='palignr';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end
                                     else
                                     begin
                                       LastDisassembleData.opcode:='palignr';
                                       lastdisassembledata.parameters:=mm(memory[3])+modrm(memory,prefix2,3,3,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                     {0f}{3a} $14: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Extract byte';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpextrb'
                                       else
                                         LastDisassembleData.opcode:='pextrb';

                                       lastdisassembledata.parameters:=modrm(memory,prefix2,3,2,last,mleft)+xmm(memory[3])+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                     {0f}{3a} $15: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Extract word';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vpextrw'
                                       else
                                         LastDisassembleData.opcode:='pextrw';

                                       lastdisassembledata.parameters:=modrm(memory,prefix2,3,1,last,mleft)+xmm(memory[3])+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='pextrq';
                                       end
                                       else
                                       begin
                                         description:='Extract dword';
                                         LastDisassembleData.opcode:='pextrd';
                                       end;

                                       if hasvex then LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;

                                       opcodeflags.skipExtraReg:=true;
                                       lastdisassembledata.parameters:=modrm(memory,prefix2,3,2,last,mleft)+xmm(memory[3])+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $17: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Extract packed single precision floating-point value';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vextractps'
                                       else
                                         LastDisassembleData.opcode:='extractps';

                                       lastdisassembledata.parameters:=modrm(memory,prefix2,3,4,last,mleft)+xmm(memory[3])+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vinsertf128';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vextractf128';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vcvtps2ph';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vpinsrb'
                                       else
                                         LastDisassembleData.opcode:='pinsrb';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,0,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                   {0f}{3a}   $21: begin    //C4 E3 79 21 80 B8 00 00 00 20
                                     if $66 in prefix2 then
                                     begin
                                       description:='Insert Scalar Single-Precision Floating-Point Value';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vinsertps'
                                       else
                                         LastDisassembleData.opcode:='insertps';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='pinsrq';
                                       end
                                       else
                                       begin
                                         description:='Insert dword';
                                         LastDisassembleData.opcode:='pinsrd';
                                       end;

                                       if hasvex then
                                         LastDisassembleData.opcode:='v'+LastDisassembleData.opcode;

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,0,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vinserti128';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vextracti128';
                                         lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                         lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                         LastDisassembleData.opcode:='vdpps'
                                       else
                                         LastDisassembleData.opcode:='dpps';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                              $41: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Dot product of packed double precision floating-point values';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vdppd'
                                       else
                                         LastDisassembleData.opcode:='dppd';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                  {0f}{3a}    $42: begin
                                     if $66 in prefix2 then
                                     begin
                                       description:='Compute multiple packed sums of absolute difference';
                                       if hasvex then
                                         LastDisassembleData.opcode:='vmpsadbw'
                                       else
                                         LastDisassembleData.opcode:='mpsadbw';

                                       lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                       lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                       inc(last);
                                       inc(offset,last-1);
                                     end;
                                   end;

                  {0f}{3a}   $44: begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='Carry-less multiplication quadword';
                                      if hasvex then
                                        LastDisassembleData.opcode:='vpclmulqdq'
                                      else
                                        LastDisassembleData.opcode:='pclmulqdq';

                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                        LastDisassembleData.opcode:='vperm2i128';

                                        lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                        lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                        inc(last);
                                        inc(offset,last-1);
                                      end;
                                    end;
                                  end;

                  {0f}{3a}   $4a: begin
                                    if $66 in prefix2 then
                                    begin
                                      if hasvex then
                                      begin
                                        description:='Variable Blend Packed Single Precision Floating-Point Values';
                                        LastDisassembleData.opcode:='vblendvps';

                                        lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                        if opcodeflags.L then
                                          lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+regnrtostr(rtYMM, memory[last] shr 4 and $f)+endcolor
                                        else
                                          lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+regnrtostr(rtXMM, memory[last] shr 4 and $f)+endcolor;

                                        inc(last);
                                        inc(offset,last-1);
                                      end;
                                    end;
                                  end;

                  {0f}{3a}   $4b: begin
                                    if $66 in prefix2 then
                                    begin
                                      if hasvex then
                                      begin
                                        description:='Variable Blend Packed Double Precision Floating-Point Values';
                                        LastDisassembleData.opcode:='vblendvpd';

                                        lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                        if opcodeflags.L then
                                          lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+regnrtostr(rtYMM, memory[last] shr 4 and $f)+endcolor
                                        else
                                          lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+regnrtostr(rtXMM, memory[last] shr 4 and $f)+endcolor;

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
                                        LastDisassembleData.opcode:='vpcmpestrm'
                                      else
                                        LastDisassembleData.opcode:='pcmpestrm';

                                      opcodeflags.skipExtraReg:=true;
                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                      inc(last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                  {0f}{3a}   $61: begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='Packed compare explicit length string, return index';
                                      if hasvex then
                                        LastDisassembleData.opcode:='vpcmpestri'
                                      else
                                        LastDisassembleData.opcode:='pcmpestri';

                                      opcodeflags.skipExtraReg:=true;
                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                      inc(last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                  {0f}{3a}   $62: begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='Packed compare implicit length string, return mask';
                                      if hasvex then
                                        LastDisassembleData.opcode:='vpcmpistrm'
                                      else
                                        LastDisassembleData.opcode:='pcmpistrm';

                                      opcodeflags.skipExtraReg:=true;
                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                      inc(last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                  {0f}{3a}   $63: begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='Packed compare implicit length string, return index';
                                      if hasvex then
                                        LastDisassembleData.opcode:='vpcmpistri'
                                      else
                                        LastDisassembleData.opcode:='pcmpistri';

                                      opcodeflags.skipExtraReg:=true;
                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
                                      inc(last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                             $df: begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='AES round key generation assist';
                                      if hasvex then
                                        LastDisassembleData.opcode:='vaeskeygenassist'
                                      else
                                        LastDisassembleData.opcode:='aeskeygenassist';

                                      lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight)+',';
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohex(memory[last],2);
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
                                        LastDisassembleData.opcode:='rorx';
                                        opcodeflags.skipExtraReg:=true;
                                        lastdisassembledata.parameters:=r32(memory[3])+modrm(memory,prefix2,3,0,last, mRight)+',';
                                        lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohex(memory[last],2);
                                        inc(last);
                                        inc(offset,last-1);
                                      end;
                                    end;
                                  end;



                              else
                              begin
                                if hasvex then
                                begin
                                  LastDisassembleData.opcode:='unknown avx 0F3A '+inttohex(memory[2],2);
                                  lastdisassembledata.parameters:=xmm(memory[3])+modrm(memory,prefix2,3,4,last,mRight);
                                  lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohex(memory[last],2);
                                  inc(last);
                                  inc(offset,last-1);
                                end;
                              end;
                            end;
                          end;


                    $40 : begin
                            description:='move if overflow';
                            lastdisassembledata.opcode:='cmovo';
                            if $66 in prefix2 then lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                                                   lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);
                            inc(offset,last-1);
                          end;

                    $41 : begin
                            description:='move if not overflow';
                            lastdisassembledata.opcode:='cmovno';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $42 : begin
                            description:='move if below/ move if carry';
                            lastdisassembledata.opcode:='cmovb';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $43 : begin
                            description:='move if above or equal/ move if not carry';
                            lastdisassembledata.opcode:='cmovae';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $44 : begin
                            description:='move if equal/move if zero';
                            lastdisassembledata.opcode:='cmove';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $45 : begin
                            description:='move if not equal/move if not zero';
                            lastdisassembledata.opcode:='cmovne';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $46 : begin
                            description:='move if below or equal';
                            lastdisassembledata.opcode:='cmovbe';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;


                    $47 : begin
                            description:='move if above';
                            lastdisassembledata.opcode:='cmova';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $48 : begin
                            description:='move if sign';
                            lastdisassembledata.opcode:='cmovs';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $49 : begin
                            description:='move if not sign';
                            lastdisassembledata.opcode:='cmovns';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $4a : begin
                            description:='move if parity even';
                            lastdisassembledata.opcode:='cmovpe';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $4b : begin
                            description:='move if not parity/move if parity odd';
                            lastdisassembledata.opcode:='cmovnp';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $4c : begin
                            description:='move if less';
                            lastdisassembledata.opcode:='cmovl';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $4d : begin
                            description:='move if greater or equal';
                            lastdisassembledata.opcode:='cmovge';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $4e : begin
                            description:='move if less or equal';
                            lastdisassembledata.opcode:='cmovle';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);


                            inc(offset,last-1);
                          end;

                    $4f : begin
                            description:='move if greater';
                            lastdisassembledata.opcode:='cmovg';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last, mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last, mRight);

                            inc(offset,last-1);
                          end;

                    $50 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmovmskpd'
                              else
                                lastdisassembledata.opcode:='movmskpd';

                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              description:='extract packed double-precision floating-point sign mask';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmovmskps'
                              else
                                lastdisassembledata.opcode:='movmskps';

                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='move mask to integer';
                              inc(offset,last-1);
                            end;
                          end;

                    $51 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsqrtsd'
                              else
                                lastdisassembledata.opcode:='sqrtsd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              description:='scalar double-fp square root';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsqrtss'
                              else
                                lastdisassembledata.opcode:='sqrtss';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              description:='scalar single-fp square root';
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsqrtpd'
                              else
                                lastdisassembledata.opcode:='sqrtpd';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              description:='packed double-fp square root';

                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsqrtps'
                              else
                                lastdisassembledata.opcode:='sqrtps';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              description:='packed single-fp square root';

                              inc(offset,last-1);
                            end;
                          end;

                    $52 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vrsqrtss'
                              else
                                lastdisassembledata.opcode:='rsqrtss';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='packed single-fp square root reciprocal';
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vrsqrtps'
                              else
                                lastdisassembledata.opcode:='rsqrtps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='scalar single-fp square root reciprocal';
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $53 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vrcpss'
                              else
                                lastdisassembledata.opcode:='rcpss';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='Compute Reciprocal of Scalar Single-Precision Floating-Point Values';
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vrcpps'
                              else
                                lastdisassembledata.opcode:='rcpps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='Compute Reciprocals of Packed Single-Precision Floating-Point Values';
                              lastdisassembledata.datasize:=4;
                              inc(offset,last-1);
                            end;
                          end;

                    $54 : begin
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vandpd'
                              else
                                lastdisassembledata.opcode:='andpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='bit-wise logical and of xmm2/m128 and xmm1';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vandps'
                              else
                                lastdisassembledata.opcode:='andps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='bit-wise logical and for single fp';
                              inc(offset,last-1);
                            end;
                          end;

                    $55 : begin
                            if $66 in prefix2 then
                            begin
                              description:='bit-wise logical and not of packed double-precision fp values';
                              if hasvex then
                                lastdisassembledata.opcode:='vandnpd'
                              else
                                lastdisassembledata.opcode:='andnpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='bit-wise logical and not for single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vandnps'
                              else
                                lastdisassembledata.opcode:='andnps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              inc(offset,last-1);
                            end;
                          end;

                    $56 : begin
                            if $66 in prefix2 then
                            begin
                              description:='bit-wise logical or of double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vorpd'
                              else
                                lastdisassembledata.opcode:='orpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);


                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='bit-wise logical or for single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vorps'
                              else
                                lastdisassembledata.opcode:='orps';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              inc(offset,last-1);
                            end;
                          end;

                    $57 : begin
                            if $66 in prefix2 then
                            begin
                              description:='bit-wise logical xor for double-fp data';
                              if hasvex then
                                lastdisassembledata.opcode:='vxorpd'
                              else
                                lastdisassembledata.opcode:='xorpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);


                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='bit-wise logical xor for single-fp data';
                              if hasvex then
                                lastdisassembledata.opcode:='vxorps'
                              else
                                lastdisassembledata.opcode:='xorps';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              inc(offset,last-1);
                            end;
                          end;

                    $58 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              //delete the repne from the tempresult
                              if hasvex then
                                lastdisassembledata.opcode:='vaddsd'
                              else
                                lastdisassembledata.opcode:='addsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='add the lower sp fp number from xmm2/mem to xmm1.';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              //delete the repe from the tempresult

                              if hasvex then
                                lastdisassembledata.opcode:='vaddss'
                              else
                                lastdisassembledata.opcode:='addss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='add the lower sp fp number from xmm2/mem to xmm1.';
                              inc(offset,last-1);
                            end else
                            begin
                              if $66 in prefix2 then
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vaddpd'
                                else
                                  lastdisassembledata.opcode:='addpd';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                                description:='add packed double-precision floating-point values from xmm2/mem to xmm1';
                                inc(offset,last-1);
                              end
                              else
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vaddps'
                                else
                                  lastdisassembledata.opcode:='addps';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                                lastdisassembledata.datasize:=4;

                                description:='add packed sp fp numbers from xmm2/mem to xmm1';
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $59 : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin

                              if hasvex then
                                lastdisassembledata.opcode:='vmulsd'
                              else
                                lastdisassembledata.opcode:='mulsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='scalar double-fp multiply';
                              inc(offset,last-1);
                            end else
                            if $f3 in prefix2 then
                            begin

                              if hasvex then
                                lastdisassembledata.opcode:='vmulss'
                              else
                                lastdisassembledata.opcode:='mulss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='scalar single-fp multiply';
                              inc(offset,last-1);
                            end else
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmulpd'
                              else
                                lastdisassembledata.opcode:='mulpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='packed double-fp multiply';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vmulps'
                              else
                                lastdisassembledata.opcode:='mulps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='packed single-fp multiply';
                              inc(offset,last-1);
                            end;
                          end;

                    $5a : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin

                              if hasvex then
                                lastdisassembledata.opcode:='vcvtsd2ss'
                              else
                                lastdisassembledata.opcode:='cvtsd2ss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                              description:='convert scalar double-precision floating-point value to scalar single-precision floating-point value';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin

                              if hasvex then
                                lastdisassembledata.opcode:='vcvtss2sd'
                              else
                                lastdisassembledata.opcode:='cvtss2sd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='convert scalar single-precision floating-point value to scalar double-precision floating-point value';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vcvtpd2ps'
                                else
                                  lastdisassembledata.opcode:='cvtpd2ps';
                                opcodeflags.skipExtraReg:=true;

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                                description:='convert packed double precision fp values to packed single precision fp values';
                                inc(offset,last-1);
                              end
                              else
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vcvtps2pd'
                                else
                                  lastdisassembledata.opcode:='cvtps2pd';

                                opcodeflags.skipExtraReg:=true;
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                                lastdisassembledata.datasize:=4;

                                description:='convert packed single precision fp values to packed double precision fp values';
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $5b : begin

                            if $66 in prefix2 then
                            begin
                              lastdisassembledata.isfloat:=true;
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtps2dq'
                              else
                                lastdisassembledata.opcode:='cvtps2dq';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='convert ps-precision fpoint values to packed dword''s ';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vcvttps2dq'
                              else
                                lastdisassembledata.opcode:='cvttps2dq';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='Convert with Truncation Packed Single-Precision FP Values to Packed Dword Integers';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtdq2ps'
                              else
                                lastdisassembledata.opcode:='cvtdq2ps';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='convert packed dword''s to ps-precision fpoint values';
                              inc(offset,last-1);
                            end;
                          end;

                    $5c : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsubsd'
                              else
                                lastdisassembledata.opcode:='subsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='scalar double-fp subtract';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsubss'
                              else
                                lastdisassembledata.opcode:='subss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='scalar single-fp subtract';
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsubpd'
                              else
                                lastdisassembledata.opcode:='subpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='packed double-fp subtract';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vsubps'
                              else
                                lastdisassembledata.opcode:='subps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4; //4*4 actually

                              description:='packed single-fp subtract';
                              inc(offset,last-1);
                            end;
                          end;


                    $5d : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vminsd'
                              else
                                lastdisassembledata.opcode:='minsd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='scalar single-fp minimum';
                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vminss'
                              else
                                lastdisassembledata.opcode:='minss';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='scalar single-fp minimum';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vminpd'
                                else
                                  lastdisassembledata.opcode:='minpd';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                                description:='packed double-fp minimum';
                                inc(offset,last-1);
                              end
                              else
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vminps'
                                else
                                  lastdisassembledata.opcode:='minps';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                                description:='packed single-fp minimum';
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $5e : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='divsd'
                              else
                                lastdisassembledata.opcode:='divsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              description:='scalar double-precision-fp divide';
                              inc(offset,last-1);
                            end else
                            if $f3 in prefix2 then
                            begin

                              if hasvex then
                                lastdisassembledata.opcode:='vdivss'
                              else
                                LastDisassembleData.opcode:='divss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              description:='scalar single-fp divide';
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vdivpd'
                                else
                                  lastdisassembledata.opcode:='divpd';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                                description:='packed double-precision fp divide';
                                inc(offset,last-1);
                              end
                              else
                              begin
                                if hasvex then
                                  lastdisassembledata.opcode:='vdivps'
                                else
                                  lastdisassembledata.opcode:='divps';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                                lastdisassembledata.datasize:=4;

                                description:='packed single-fp divide';
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $5f : begin
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin

                              description:='scalar double-fp maximum';
                              if hasvex then
                                lastdisassembledata.opcode:='vmaxsd'
                              else
                                lastdisassembledata.opcode:='maxsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end else
                            if $f3 in prefix2 then
                            begin

                              description:='scalar single-fp maximum';
                              if hasvex then
                                lastdisassembledata.opcode:='vmaxss'
                              else
                                lastdisassembledata.opcode:='maxss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.datasize:=4;

                              inc(offset,last-1);
                            end else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='packed double-fp maximum';
                                if hasvex then
                                  lastdisassembledata.opcode:='vmaxpd'
                                else
                                  lastdisassembledata.opcode:='maxpd';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                                inc(offset,last-1);
                              end
                              else
                              begin
                                description:='packed single-fp maximum';
                                if hasvex then
                                  lastdisassembledata.opcode:='vmaxps'
                                else
                                  lastdisassembledata.opcode:='maxps';

                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                                lastdisassembledata.datasize:=4;

                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $60 : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack low packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='vpunpcklbw'
                              else
                                lastdisassembledata.opcode:='punpcklbw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack low packed data';
                              lastdisassembledata.opcode:='punpcklbw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $61 : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack low packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='punpcklwd'
                              else
                                lastdisassembledata.opcode:='punpcklwd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack low packed data';
                              lastdisassembledata.opcode:='punpcklwd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $62 : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack low packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='vpunpckldq'
                              else
                                lastdisassembledata.opcode:='punpckldq';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack low packed data';
                              lastdisassembledata.opcode:='punpckldq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $63 : begin
                            if $66 in prefix2 then
                            begin
                              description:='pack with signed saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='packsswb'
                              else
                                lastdisassembledata.opcode:='vpacksswb';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='pack with signed saturation';
                              lastdisassembledata.opcode:='packsswb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $64 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed compare for greater than';
                              if hasvex then
                                lastdisassembledata.opcode:='vpcmpgtb'
                              else
                                lastdisassembledata.opcode:='pcmpgtb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for greater than';
                              lastdisassembledata.opcode:='pcmpgtb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $65 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed compare for greater than';
                              if hasvex then
                                lastdisassembledata.opcode:='vpcmpgtw'
                              else
                                lastdisassembledata.opcode:='pcmpgtw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for greater than';
                              lastdisassembledata.opcode:='pcmpgtw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $66 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed compare for greater than';
                              if hasvex then
                                lastdisassembledata.opcode:='vpcmpgtd'
                              else
                                lastdisassembledata.opcode:='pcmpgtd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for greater than';
                              lastdisassembledata.opcode:='pcmpgtd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;


                    $67 : begin
                            if $66 in prefix2 then
                            begin
                              description:='pack with unsigned saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpackuswb'
                              else
                                lastdisassembledata.opcode:='packuswb';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='pack with unsigned saturation';
                              lastdisassembledata.opcode:='packuswb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $68 : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack high packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='vpunpckhbw'
                              else
                                lastdisassembledata.opcode:='punpckhbw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack high packed data';
                              lastdisassembledata.opcode:='punpckhbw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $69 : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack high packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='vpunpckhwd'
                              else
                                lastdisassembledata.opcode:='punpckhwd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack high packed data';
                              lastdisassembledata.opcode:='punpckhwd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $6a : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack high packed data';
                              if hasvex then
                                lastdisassembledata.opcode:='vpunpckhdq'
                              else
                                lastdisassembledata.opcode:='punpckhdq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='unpack high packed data';
                              lastdisassembledata.opcode:='punpckhdq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $6b : begin
                            if $66 in prefix2 then
                            begin
                              description:='pack with signed saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='packssdw'
                              else
                                lastdisassembledata.opcode:='packssdw';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='pack with signed saturation';
                              lastdisassembledata.opcode:='packssdw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $6c : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack low packed data';
                              lastdisassembledata.opcode:='punpcklqdq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                          end;

                    $6d : begin
                            if $66 in prefix2 then
                            begin
                              description:='unpack high packed data';
                              lastdisassembledata.opcode:='punpckhqdq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                          end;


                    $6e : begin
                            //lastdisassembledata.isfloat:=true; //not sure
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

                            if hasvex then
                              lastdisassembledata.opcode:='v'+lastdisassembledata.opcode;

                            opcodeflags.skipExtraReg:=true;
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight)
                            else
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                            inc(offset,last-1);
                          end;

                    $6f : begin
                            if $f3 in prefix2 then
                            begin

                              description:='move unaligned double quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovdqu'
                              else
                                lastdisassembledata.opcode:='movdqu';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move aligned double quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovdqa'
                              else
                                lastdisassembledata.opcode:='movdqa';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move 64 bits';
                              lastdisassembledata.opcode:='movq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,4,last);

                              inc(offset,last-1);
                            end;
                          end;

                    $70 : begin
                            if $f2 in prefix2 then
                            begin

                              description:='shuffle packed low words';
                              if hasvex then
                                lastdisassembledata.opcode:='vpshuflw'
                              else
                                lastdisassembledata.opcode:='pshuflw';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,last);
                            end
                            else
                            if $f3 in prefix2 then
                            begin

                              description:='shuffle packed high words';
                              if hasvex then
                                lastdisassembledata.opcode:='vpshufhw'
                              else
                                lastdisassembledata.opcode:='pshufhw';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
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

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,last);
                            end
                            else
                            begin
                              description:='packed shuffle word';
                              if hasvex then
                                lastdisassembledata.opcode:='vpshufw'
                              else
                                lastdisassembledata.opcode:='pshufw';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
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
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsrlw'
                                      else
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
                                      description:='shift packed data right arithmetic';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsraw'
                                      else
                                        LastDisassembleData.opcode:='psraw';

                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,last-1+1);
                                    end
                                    else
                                    begin
                                      description:='packed shift left logical';
                                      LastDisassembleData.opcode:='psraw';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,3,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,last-1+1);
                                    end;
                                  end;

                              6 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='packed shift left logical';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsllw'
                                      else
                                        LastDisassembleData.opcode:='psllw';

                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,last-1+1);
                                    end
                                    else
                                    begin
                                      description:='packed shift left logical';
                                      LastDisassembleData.opcode:='psllw';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,3,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,last-1+1);
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
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsrld'
                                      else
                                        lastdisassembledata.opcode:='psrld';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end
                                    else
                                    begin
                                      description:='packed shift right logical';
                                      lastdisassembledata.opcode:='psrld';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,3,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end;
                                  end;

                              4 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='packed shift right arithmetic';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsrad'
                                      else
                                        lastdisassembledata.opcode:='psrad';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,3);
                                    end
                                    else
                                    begin
                                      description:='packed shift right arithmetic';
                                      lastdisassembledata.opcode:='psrad';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,3,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                      inc(offset,3);
                                    end;
                                  end;

                              6 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='packed shift left logical';
                                      if hasvex then
                                        lastdisassembledata.opcode:='pslld'
                                      else
                                        lastdisassembledata.opcode:='pslld';

                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end
                                    else
                                    begin
                                      description:='packed shift left logical';
                                      lastdisassembledata.opcode:='pslld';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,3,last);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
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
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsrlq'
                                      else
                                        lastdisassembledata.opcode:='psrlq';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end
                                    else
                                    begin
                                      description:='packed shift right logical';
                                      lastdisassembledata.opcode:='psrlq';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,3,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end;
                                    delete(lastdisassembledata.parameters,1,1);
                                  end;

                              3 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='shift double quadword right logical';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsrldq'
                                      else
                                        lastdisassembledata.opcode:='psrldq';

                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end;
                                    delete(lastdisassembledata.parameters,1,1);
                                  end;

                              6 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='packed shift left logical';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpsllq'
                                      else
                                        lastdisassembledata.opcode:='psllq';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end
                                    else
                                    begin
                                      description:='packed shift left logical';
                                      lastdisassembledata.opcode:='psllq';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,3,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
                                      inc(offset,3);
                                    end;
                                    delete(lastdisassembledata.parameters,1,1);
                                  end;

                              7 : begin
                                    if $66 in prefix2 then
                                    begin
                                      description:='shift double quadword left logical';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vpslldq'
                                      else
                                        lastdisassembledata.opcode:='pslldq';
                                      lastdisassembledata.parameters:= modrm(memory,prefix2,2,4,last,mRight);
                                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);

                                      delete(lastdisassembledata.parameters,1,1);
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
                                lastdisassembledata.opcode:='vpcmpeqb'
                              else
                                lastdisassembledata.opcode:='pcmpeqb';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for equal';
                              lastdisassembledata.opcode:='pcmpeqb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $75 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed compare for equal';
                              if hasvex then
                                lastdisassembledata.opcode:='vpcmpeqw'
                              else
                                lastdisassembledata.opcode:='pcmpeqw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for equal';
                              lastdisassembledata.opcode:='pcmpeqw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $76 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed compare for equal';
                              if hasvex then
                                lastdisassembledata.opcode:='vpcmpeqd'
                              else
                                lastdisassembledata.opcode:='pcmpeqd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed compare for equal';
                              lastdisassembledata.opcode:='pcmpeqd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;


                    $77 : begin
                            if hasvex then
                            begin
                              if opcodeflags.L then
                              begin
                                description:='Zero all YMM registers';
                                lastdisassembledata.opcode:='vzeroall';
                                inc(offset);
                              end
                              else
                              begin
                                description:='Zero upper bits of YMM registers';
                                lastdisassembledata.opcode:='vzeroupper';
                                inc(offset);
                              end;
                            end
                            else
                            begin
                              description:='empty mmx™ state';
                              lastdisassembledata.opcode:='emms';
                              inc(offset);
                            end;
                          end;

                    $78 : begin
                            description:='reads a specified vmcs field (32 bits)';
                            lastdisassembledata.opcode:='vmread';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2]);
                            inc(offset,last-1);
                          end;

                    $79 : begin
                            description:='writes a specified vmcs field (32 bits)';
                            lastdisassembledata.opcode:='vmwrite';
                            lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                            inc(offset,last-1);
                          end;

                    $7c : begin
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vhaddpd'
                              else
                                lastdisassembledata.opcode:='haddpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                              description:='packed double-fp horizontal add';
                              inc(offset,last-1);
                            end
                            else
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vhaddps'
                              else
                                lastdisassembledata.opcode:='haddps';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                              description:='packed single-fp horizontal add';
                              inc(offset,last-1);
                            end;
                          end;

                    $7d : begin
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vhsubpd'
                              else
                                lastdisassembledata.opcode:='hsubpd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                              description:='packed double-fp horizontal subtract';
                              inc(offset,last-1);
                            end
                            else
                            if $f2 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='vhsubps'
                              else
                                lastdisassembledata.opcode:='hsubps';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mright);

                              description:='packed single-fp horizontal subtract';
                              inc(offset,last-1);
                            end;
                          end;

                    $7e : begin

                            if $f3 in prefix2 then
                            begin

                              description:='move quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovq'
                              else
                                lastdisassembledata.opcode:='movq';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              if Rex_W then
                              begin
                                description:='move 64 bits';
                                lastdisassembledata.opcode:='movq';
                              end
                              else
                              begin
                                description:='move 32 bits';
                                lastdisassembledata.opcode:='movd';
                              end;

                              if hasvex then
                                lastdisassembledata.opcode:='v'+lastdisassembledata.opcode;

                              opcodeflags.skipExtraReg:=true;

                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+xmm(memory[2]);  //r32/rm32,xmm
                              inc(offset,last-1);
                            end
                            else
                            begin
                              if Rex_W then
                              begin
                                description:='move 64 bits';
                                lastdisassembledata.opcode:='movq';
                              end
                              else
                              begin
                                description:='move 32 bits';
                                lastdisassembledata.opcode:='movd';
                              end;


                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+mm(memory[2]); //r32/rm32,mm
                              inc(offset,last-1);
                            end;
                          end;

                    $7f : begin
                            if $f3 in prefix2 then
                            begin

                              description:='move unaligned double quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovdqu'
                              else
                                lastdisassembledata.opcode:='movdqu';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,4,last)+xmm(memory[2]);
                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move aligned double quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovdqa'
                              else
                                lastdisassembledata.opcode:='movdqa';

                              opcodeflags.skipExtraReg:=true;
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
                            description:='jump near if overflow (OF=1)';
                            lastdisassembledata.opcode:='jo';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $81 : begin
                            description:='jump near if not overflow (OF=0)';
                            lastdisassembledata.opcode:='jno';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)=0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                          end;

                    $82 : begin
                            description:='jump near if below/carry (CF=1)';

                            lastdisassembledata.opcode:='jb';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;

                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                          end;

                    $83 : begin
                            description:='jump near if above or equal (CF=0)';
                            lastdisassembledata.opcode:='jae';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)=0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $84 : begin
                            description:='jump near if equal (ZF=1)';

                            lastdisassembledata.opcode:='je';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;


                    $85 : begin
                            description:='jump near if not equal (ZF=0)';
                            lastdisassembledata.opcode:='jne';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                          end;

                    $86 : begin
                            description:='jump near if below or equal (CF=1 or ZF=1)';
                            lastdisassembledata.opcode:='jbe';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;


                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $87 : begin
                            description:='jump near if above (CF=0 and ZF=0)';
                            lastdisassembledata.opcode:='ja';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                                 lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))=0;


                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $88 : begin
                            description:='jump near if sign (SF=1)';
                            lastdisassembledata.opcode:='js';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $89 : begin
                            description:='jump near if not sign (SF=0)';
                            lastdisassembledata.opcode:='jns';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8a : begin
                            description:='jump near if parity (PF=1)';
                            lastdisassembledata.opcode:='jp';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)<>0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8b : begin
                            description:='jump near if not parity (PF=0)';
                            lastdisassembledata.opcode:='jnp';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)=0;

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8c : begin
                            description:='jump near if less (SF~=OF)';
                            lastdisassembledata.opcode:='jl';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF);

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8d : begin
                            description:='jump near if not less (SF=OF)';
                            lastdisassembledata.opcode:='jnl';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF);

                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8e : begin
                            description:='jump near if not greater (ZF=1 or SF~=OF)';
                            lastdisassembledata.opcode:='jng';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF)) or ((context^.EFlags and EFLAGS_ZF)<>0);


                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $8f : begin
                            description:='jump near if greater (ZF=0 and SF=OF)';
                            lastdisassembledata.opcode:='jg';
                            lastdisassembledata.isjump:=true;
                            lastdisassembledata.isconditionaljump:=true;
                            if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF)) and ((context^.EFlags and EFLAGS_ZF)=0);


                            inc(offset,1+4);
                            if MarkIPRelativeInstructions then
                            begin
                              LastDisassembleData.riprelative:=2;
                              riprelative:=true;
                            end;

                            lastdisassembledata.parametervaluetype:=dvtaddress;
                            if is64bit then
                              lastdisassembledata.parametervalue:=qword(offset+qword(pint(@memory[2])^))
                            else
                              lastdisassembledata.parametervalue:=dword(offset+qword(pint(@memory[2])^));

                            lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=2;
                            inc(lastdisassembledata.seperatorcount);



                            lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                          end;

                    $90 : begin
                            description:='set byte if overflow';
                            lastdisassembledata.opcode:='seto';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $91 : begin
                            description:='set byte if not overfloww';
                            lastdisassembledata.opcode:='setno';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $92 : begin
                            description:='set byte if below/carry';
                            lastdisassembledata.opcode:='setb';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $93 : begin
                            description:='set byte if above or equal';
                            lastdisassembledata.opcode:='setae';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $94 : begin
                            description:='set byte if equal';
                            lastdisassembledata.opcode:='sete';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $95 : begin
                            description:='set byte if not equal';
                            lastdisassembledata.opcode:='setne';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $96 : begin
                            description:='set byte if below or equal';
                            lastdisassembledata.opcode:='setbe';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $97 : begin
                            description:='set byte if above';
                            lastdisassembledata.opcode:='seta';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $98 : begin
                            description:='set byte if sign';
                            lastdisassembledata.opcode:='sets';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $99 : begin
                            description:='set byte if not sign';
                            lastdisassembledata.opcode:='setns';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $9a : begin
                            description:='set byte if parity';
                            lastdisassembledata.opcode:='setp';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $9b : begin
                            description:='set byte if not parity';
                            lastdisassembledata.opcode:='setnp';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $9c : begin
                            description:='set byte if less';
                            lastdisassembledata.opcode:='setl';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);

                            inc(offset,last-1);
                          end;

                    $9d : begin
                            description:='set byte if greater or equal';
                            lastdisassembledata.opcode:='setge';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);
                            inc(offset,last-1);

                          end;

                    $9e : begin
                            description:='set byte if less or equal';
                            lastdisassembledata.opcode:='setle';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);
                            inc(offset,last-1);

                          end;

                    $9f : begin
                            description:='set byte if greater';
                            lastdisassembledata.opcode:='setg';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,2,2,last,8);
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

                            lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohex(memory[last],2);
                            inc(last);
                            inc(offset,last-1);

                          end;

                    $a5 : begin
                            description:='double precision shift left';
                            lastdisassembledata.opcode:='shld';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)+r16(memory[2])+','+colorreg+'cl'+endcolor else
                              lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last)+r32(memory[2])+','+colorreg+'cl'+endcolor;
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
                            case memory[2] of
                              $f0: begin
                                     description:='memory fence';
                                     lastdisassembledata.opcode:='mfence';
                                     inc(offset,1);
                                   end;

                              $f8: begin
                                     description:='store fence';
                                     lastdisassembledata.opcode:='sfence';
                                     inc(offset,1);
                                   end;

                              else
                              case getreg(memory[2]) of
                              0:  begin
                                    if $f3 in prefix2 then
                                    begin
                                      description:='read fs base address';
                                      lastdisassembledata.opcode:='rdfsbase';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end
                                    else
                                    begin
                                      description:='store fp and mmx state and streaming simd extension state';
                                      lastdisassembledata.opcode:='fxsave';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                              1:  begin
                                    if $f3 in prefix2 then
                                    begin
                                      description:='read gs base address';
                                      lastdisassembledata.opcode:='rdgsbase';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end
                                    else
                                    begin
                                      description:='restore fp and mmx state and streaming simd extension state';
                                      lastdisassembledata.opcode:='fxrstor';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                              2:  begin
                                    if $f3 in prefix2 then
                                    begin
                                      description:='write fs base address';
                                      lastdisassembledata.opcode:='wrfsbase';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end
                                    else
                                    begin
                                      description:='load streaming simd extension control/status';
                                      if hasvex then
                                        lastdisassembledata.opcode:='vldmxcsr'
                                      else
                                        lastdisassembledata.opcode:='ldmxcsr';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                              3:  begin
                                    if $f3 in prefix2 then
                                    begin
                                      description:='write gs base address';
                                      lastdisassembledata.opcode:='wrgsbase';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end
                                    else
                                    begin
                                      description:='store streaming simd extension control/status';
                                      if hasvex then
                                        lastdisassembledata.opcode:='stmxcsr'
                                      else
                                        lastdisassembledata.opcode:='stmxcsr';

                                      opcodeflags.skipExtraReg:=true;
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                              4:  begin
                                    description:='save processor extended state';
                                    if Rex_W then
                                      lastdisassembledata.opcode:='xsave64'
                                    else
                                      lastdisassembledata.opcode:='xsave';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    if getmod(memory[2])=3 then
                                    begin
                                      description:='Load Fence';
                                      lastdisassembledata.opcode:='lfence';
                                      inc(offset,2);
                                    end
                                    else
                                    begin
                                      description:='restore processor extended state';
                                      if Rex_W then
                                        lastdisassembledata.opcode:='xrstor64'
                                      else
                                        lastdisassembledata.opcode:='xrstor';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                      inc(offset,last-1);
                                    end;
                                  end;

                              6:  begin
                                    description:='save processor extended status optimized';
                                    if Rex_W then
                                      lastdisassembledata.opcode:='xsaveopt64'
                                    else
                                      lastdisassembledata.opcode:='xsaveopt';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                              7:  begin

                                  end;

                            end;

                            end;



                          end;

                    $af : begin
                            description:='signed multiply';
                            lastdisassembledata.opcode:='imul';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

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
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

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
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                            inc(offset,last-1);
                          end;

                    $b5 : begin
                            description:='load far pointer';
                            lastdisassembledata.opcode:='lgs';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                            inc(offset,last-1);
                          end;

                    $b6 : begin
                            description:='Move with zero-extend';
                            lastdisassembledata.opcode:='movzx';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight);


                            inc(offset,last-1);
                          end;

                    $b7 : begin
                            description:='Move with zero-extend';
                            lastdisassembledata.opcode:='movzx';
                            if $66 in prefix2 then
                            lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight) else
                            lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight);


                            inc(offset,last-1);
                          end;

                    $b8 : begin
                            if $f3 in prefix2 then
                            begin
                              description:='Return the Count of Number of Bits Set to 1';
                              lastdisassembledata.opcode:='popcnt';
                              if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;


                    $ba : begin
                            lastdisassembledata.parametervaluetype:=dvtvalue;


                            case getreg(memory[2]) of
                              4:  begin
                                    //bt
                                    description:='bit test';
                                    lastdisassembledata.opcode:='bt';
                                    if $66 in prefix2 then
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last) else
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                    lastdisassembledata.parametervalue:=memory[last];
                                    lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                    inc(offset,last-1+1);
                                  end;

                              5:  begin
                                    //bts
                                    description:='bit test and set';
                                    lastdisassembledata.opcode:='bts';
                                    if $66 in prefix2 then
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last) else
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                    lastdisassembledata.parametervalue:=memory[last];
                                    lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                                    inc(offset,last-1+1);
                                  end;

                              6:  begin
                                    //btr
                                    description:='bit test and reset';
                                    lastdisassembledata.opcode:='btr';
                                    if $66 in prefix2 then
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last) else
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                    lastdisassembledata.parametervalue:=memory[last];
                                    lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

                                    inc(offset,last-1+1);
                                  end;

                              7:  begin
                                    //btc
                                    description:='bit test and complement';
                                    lastdisassembledata.opcode:='btc';
                                    if $66 in prefix2 then
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last) else
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);     //notice the difference in the modrm 4th parameter

                                    lastdisassembledata.parametervalue:=memory[last];
                                    lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);

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
                            if $f3 in prefix2 then
                            begin
                              description:='count the number of trailing zero bits';
                              lastdisassembledata.opcode:='tzcnt';
                              if $66 in prefix2 then
                                lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                                lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              //bsf
                              description:='bit scan forward';
                              lastdisassembledata.opcode:='bsf';
                              if $66 in prefix2 then
                                lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                                lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);


                              inc(offset,last-1);
                            end;
                          end;

                    $bd : begin
                            if $f3 in prefix2 then
                            begin
                              description:='count the number of leading zero bits';
                              lastdisassembledata.opcode:='lzcnt';
                              if $66 in prefix2 then
                                lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                                lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              //bsf
                              description:='bit scan reverse';
                              lastdisassembledata.opcode:='bsr';
                              if $66 in prefix2 then
                                lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,1,last,mRight) else
                                lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,0,last,mRight);


                              inc(offset,last-1);
                            end;
                          end;

                    $be : begin
                            description:='move with sign-extension';
                            lastdisassembledata.opcode:='movsx';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=r16(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight) else
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,2,last,8,0,mRight);



                            inc(offset,last-1);
                          end;

                    $bf : begin
                            description:='move with sign-extension';
                            lastdisassembledata.opcode:='movsx';
                            lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,1,last,16,0,mRight);

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
                            lastdisassembledata.isfloat:=true;
                            if $f2 in prefix2 then
                            begin

                              description:='compare scalar dpuble-precision floating-point values';
                              if hasvex then
                                lastdisassembledata.opcode:='vcmpsd'
                              else
                                lastdisassembledata.opcode:='cmpsd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);

                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,last);
                            end
                            else
                            if $f3 in prefix2 then
                            begin
                              description:='packed single-fp compare';
                              if hasvex then
                                lastdisassembledata.opcode:='vcmpss'
                              else
                                lastdisassembledata.opcode:='cmpss';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                              lastdisassembledata.datasize:=4;
                              inc(offset,last);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='compare packed double-precision floating-point values';
                                if hasvex then
                                  lastdisassembledata.opcode:='vcmppd'
                                else
                                  lastdisassembledata.opcode:='cmppd';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                                lastdisassembledata.parametervaluetype:=dvtvalue;
                                lastdisassembledata.parametervalue:=memory[last];
                                lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                                inc(offset,last);
                              end
                              else
                              begin
                                description:='packed single-fp compare';
                                if hasvex then
                                  lastdisassembledata.opcode:='vcmpps'
                                else
                                  lastdisassembledata.opcode:='cmpps';
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,128,0,mRight);
                                lastdisassembledata.parametervaluetype:=dvtvalue;
                                lastdisassembledata.parametervalue:=memory[last];
                                lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                                lastdisassembledata.datasize:=4;
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
                              if hasvex then
                                lastdisassembledata.opcode:='vpinsrw'
                              else
                                lastdisassembledata.opcode:='pinsrw';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,last);
                            end
                            else
                            begin
                              description:='insert word';
                              lastdisassembledata.opcode:='pinsrw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,0,last,mRight);
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
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,3);
                            end
                            else
                            begin
                              description:='extract word';
                              lastdisassembledata.opcode:='pextrw';
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,3);
                            end;
                          end;

                    $c6 : begin
                            lastdisassembledata.isfloat:=true;
                            if $66 in prefix2 then
                            begin
                              description:='shuffle double-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vshufpd'
                              else
                                lastdisassembledata.opcode:='shufpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                              inc(offset,last);
                            end
                            else
                            begin
                              description:='shuffle single-fp';
                              if hasvex then
                                lastdisassembledata.opcode:='vshufps'
                              else
                                lastdisassembledata.opcode:='shufps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(lastdisassembledata.parametervalue,2);
                              lastdisassembledata.datasize:=4;
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

                              3:  begin
                                    description:='restore processor extended status supervisor';
                                    if Rex_W then
                                      lastdisassembledata.opcode:='xrstors64'
                                    else
                                      lastdisassembledata.opcode:='xrstors';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                              4:  begin
                                    description:='save processor extended state with compaction';
                                    if Rex_W then
                                      lastdisassembledata.opcode:='xsavec'
                                    else
                                      lastdisassembledata.opcode:='xsavec64';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;

                              5:  begin
                                    description:='save processor extended state supervisor';
                                    if Rex_W then
                                      lastdisassembledata.opcode:='xsaves'
                                    else
                                      lastdisassembledata.opcode:='xsaves64';
                                    lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                    inc(offset,last-1);
                                  end;


                              6:  begin
                                    if $66 in prefix2 then
                                    begin
                                      if getmod(memory[2])=3 then //reg
                                      begin
                                        description:='read random numer';
                                        lastdisassembledata.opcode:='rdrand';
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last);
                                        inc(offset,last-1);
                                      end
                                      else
                                      begin
                                        description:='copy vmcs data to vmcs region in memory';
                                        lastdisassembledata.opcode:='vmclear';
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                        inc(offset,last-1);
                                      end;
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
                                      //check if it's a memory or register access
                                      //if register it's rdrand else vmptrld
                                      if getmod(memory[2])=3 then //reg
                                      begin
                                        description:='read random numer';
                                        lastdisassembledata.opcode:='rdrand';
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);
                                        inc(offset,last-1);
                                      end
                                      else
                                      begin
                                        description:='loads the current vmcs pointer from memory';
                                        lastdisassembledata.opcode:='vmptrld';
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                        inc(offset,last-1);
                                      end;


                                    end;
                                  end;

                              7:  begin
                                    if getmod(memory[2])=3 then //reg
                                    begin
                                      description:='read random SEED';
                                      lastdisassembledata.opcode:='rdseed';
                                      if $66 in prefix2 then
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,1,last)
                                      else
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                      inc(offset,last-1);
                                    end
                                    else
                                    begin
                                      description:='stores the current vmcs pointer into memory';
                                      lastdisassembledata.opcode:='vmptrst';
                                      lastdisassembledata.parameters:=modrm(memory,prefix2,2,0,last);

                                      inc(offset,last-1);
                                    end;
                                  end;
                            end;

                          end;

                    $c8..$cf : begin
                            //bswap
                            description:='byte swap';
                            lastdisassembledata.opcode:='bswap';
                            if $66 in prefix2 then
                              lastdisassembledata.parameters:=rd16(memory[1]-$c8)
                            else
                              lastdisassembledata.parameters:=rd(memory[1]-$c8);

                            inc(offset);
                          end;

                    $d0 : begin
                            if $66 in prefix2 then
                            begin
                              description:='Packed Double-FP Add/Subtract';
                              if hasvex then
                                lastdisassembledata.opcode:='vaddsubpd'
                              else
                                lastdisassembledata.opcode:='addsubpd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            if $f2 in prefix2 then
                            begin
                              description:='Packed Single-FP Add/Subtract';
                              if hasvex then
                                lastdisassembledata.opcode:='vaddsubps'
                              else
                                lastdisassembledata.opcode:='addsubps';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $d1 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift right logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsrlw'
                              else
                                lastdisassembledata.opcode:='psrlw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift right logical';
                              lastdisassembledata.opcode:='psrlw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $d2 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift right logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsrld'
                              else
                                lastdisassembledata.opcode:='psrld';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift right logical';
                              lastdisassembledata.opcode:='psrld';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $d3 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift right logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsrlq'
                              else
                                lastdisassembledata.opcode:='psrlq';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift right logical';
                              lastdisassembledata.opcode:='psrlq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $d4 : begin
                            if $66 in prefix2 then
                            begin
                              description:='add packed quadword integers';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddq'
                              else
                                lastdisassembledata.opcode:='paddq';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='add packed quadword integers';
                              lastdisassembledata.opcode:='paddq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;


                    $d5 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed multiply low';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmullw'
                              else
                                lastdisassembledata.opcode:='pmullw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed multiply low';
                              lastdisassembledata.opcode:='pmullw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $d6 : begin
                            if $f2 in prefix2 then
                            begin

                              description:='move low quadword from xmm to mmx technology register';
                              lastdisassembledata.opcode:='movdq2q';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin

                              description:='move low quadword from xmm to mmx technology register';
                              lastdisassembledata.opcode:='movq2dq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $66 in prefix2 then
                            begin
                              description:='move low quadword from xmm to mmx technology register';
                              if hasvex then
                                lastdisassembledata.opcode:='vmovq'
                              else
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
                              if hasvex then
                                lastdisassembledata.opcode:='vpmovmskb'
                              else
                                lastdisassembledata.opcode:='pmovmskb';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='move byte mask to integer';
                              lastdisassembledata.opcode:='pmovmskb';
                              lastdisassembledata.parameters:=r32(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $d8 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract unsigned with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubusb'
                              else
                                lastdisassembledata.opcode:='psubusb';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract unsigned with saturation';
                              lastdisassembledata.opcode:='psubusb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $d9 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract unsigned with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubusw'
                              else
                                lastdisassembledata.opcode:='psubusw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract unsigned with saturation';
                              lastdisassembledata.opcode:='psubusw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $da : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed unsigned integer byte minimum';
                              if hasvex then
                                lastdisassembledata.opcode:='vpminub'
                              else
                                lastdisassembledata.opcode:='pminub';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed unsigned integer byte minimum';
                              lastdisassembledata.opcode:='pminub';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $db : begin
                            if $66 in prefix2 then
                            begin
                              description:='logical and';
                              if hasvex then
                                lastdisassembledata.opcode:='vpand'
                              else
                                lastdisassembledata.opcode:='pand';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='logical and';
                              lastdisassembledata.opcode:='pand';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $dc : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add unsigned with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddusb'
                              else
                                lastdisassembledata.opcode:='paddusb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add unsigned with saturation';
                              lastdisassembledata.opcode:='paddusb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $dd : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add unsigned with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddusw'
                              else
                                lastdisassembledata.opcode:='paddusw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add unsigned with saturation';
                              lastdisassembledata.opcode:='paddusw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $de : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed unsigned integer byte maximum';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmaxub'
                              else
                                lastdisassembledata.opcode:='pmaxub';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mLeft);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed unsigned integer byte maximum';
                              lastdisassembledata.opcode:='pmaxub';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $df : begin
                            if $66 in prefix2 then
                            begin
                              description:='logical and not';
                              if hasvex then
                                lastdisassembledata.opcode:='vpandn'
                              else
                                lastdisassembledata.opcode:='pandn';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='logical and not';
                              lastdisassembledata.opcode:='pandn';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $e0 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed average';
                              if hasvex then
                                lastdisassembledata.opcode:='vpavgb'
                              else
                                lastdisassembledata.opcode:='pavgb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed average';
                              lastdisassembledata.opcode:='pavgb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $e1 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift right arithmetic';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsraw'
                              else
                                lastdisassembledata.opcode:='psraw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift right arithmetic';
                              lastdisassembledata.opcode:='psraw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $e2 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift left logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsrad'
                              else
                                lastdisassembledata.opcode:='psrad';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift left logical';
                              lastdisassembledata.opcode:='psrad';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $e3 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed average';
                              if hasvex then
                                lastdisassembledata.opcode:='vpavgw'
                              else
                                lastdisassembledata.opcode:='pavgw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed average';
                              lastdisassembledata.opcode:='pavgw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $e4 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed multiply high unsigned';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmulhuw'
                              else
                                lastdisassembledata.opcode:='pmulhuw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed multiply high unsigned';
                              lastdisassembledata.opcode:='pmulhuw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $e5 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed multiply high';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmulhw'
                              else
                                lastdisassembledata.opcode:='pmulhw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed multiply high';
                              lastdisassembledata.opcode:='pmulhw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $e6 : begin
                            if $f2 in prefix2 then
                            begin

                              description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtpd2dq'
                              else
                                lastdisassembledata.opcode:='cvtpd2dq';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            if $f3 in prefix2 then
                            begin

                              description:='convert two packed signed dwords from param2 to two packed dp-floating point values in param1';
                              if hasvex then
                                lastdisassembledata.opcode:='vcvtdq2pd'
                              else
                                lastdisassembledata.opcode:='cvtdq2pd';
                              lastdisassembledata.parameters:=xmm(memory[2])+',';
                              opcodeflags.L:=false;
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              if $66 in prefix2 then
                              begin
                                description:='convert with truncation packed double-precision floating-point values to packed doubleword integers';
                                if hasvex then
                                  lastdisassembledata.opcode:='vcvttpd2dq'
                                else
                                  lastdisassembledata.opcode:='cvttpd2dq';

                                opcodeflags.skipExtraReg:=true;
                                lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                                inc(offset,last-1);
                              end;
                            end;
                          end;

                    $e7 : begin
                            if $66 in prefix2 then
                            begin
                              if hasvex then
                                lastdisassembledata.opcode:='movntdq'
                              else
                                lastdisassembledata.opcode:='vmovntdq';

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
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubsb'
                              else
                                lastdisassembledata.opcode:='psubsb';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract with saturation';
                              lastdisassembledata.opcode:='psubsb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $e9 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubsw'
                              else
                                lastdisassembledata.opcode:='psubsw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract with saturation';
                              lastdisassembledata.opcode:='psubsw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $ea : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed signed integer word minimum';
                              if hasvex then
                                lastdisassembledata.opcode:='vpminsw'
                              else
                                lastdisassembledata.opcode:='pminsw';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed signed integer word minimum';
                              lastdisassembledata.opcode:='pminsw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $eb : begin
                            if $66 in prefix2 then
                            begin
                              description:='bitwise logical or';
                              if hasvex then
                                lastdisassembledata.opcode:='vpor'
                              else
                                lastdisassembledata.opcode:='por';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='bitwise logical or';
                              lastdisassembledata.opcode:='por';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $ec : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddsb'
                              else
                                lastdisassembledata.opcode:='paddsb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add with saturation';
                              lastdisassembledata.opcode:='paddsb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $ed : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add with saturation';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddsw'
                              else
                                lastdisassembledata.opcode:='paddsw';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add with saturation';
                              lastdisassembledata.opcode:='paddsw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $ee : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed signed integer word maximum';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmaxsw'
                              else
                                lastdisassembledata.opcode:='pmaxsw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed signed integer word maximum';
                              lastdisassembledata.opcode:='pmaxsw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $ef : begin
                            if $66 in prefix2 then
                            begin
                              description:='logical exclusive or';
                              if hasvex then
                                lastdisassembledata.opcode:='vpxor'
                              else
                                lastdisassembledata.opcode:='pxor';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='logical exclusive or';
                              lastdisassembledata.opcode:='pxor';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $f0 : begin
                            if $f2 in prefix2 then
                            begin
                              description:='load unaligned integer 128 bits';
                              if hasvex then
                                lastdisassembledata.opcode:='vlddqu'
                              else
                                lastdisassembledata.opcode:='lddqu';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                              inc(offset);
                          end;


                    $f1 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift left logical';
                              lastdisassembledata.opcode:='psllw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift left logical';
                              lastdisassembledata.opcode:='psllw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $f2 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift left logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpslld'
                              else
                                lastdisassembledata.opcode:='pslld';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift left logical';
                              lastdisassembledata.opcode:='pslld';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $f3 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed shift left logical';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsllq'
                              else
                                lastdisassembledata.opcode:='psllq';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed shift left logical';
                              lastdisassembledata.opcode:='psllq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);
                              inc(offset,last-1);
                            end;
                          end;

                    $f4 : begin
                            if $66 in prefix2 then
                            begin
                              description:='multiply packed unsigned doubleword integers';
                              if hasvex then
                                lastdisassembledata.opcode:='pmuludq'
                              else
                                lastdisassembledata.opcode:='vpmuludq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='multiply packed unsigned doubleword integers';
                              lastdisassembledata.opcode:='pmuludq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;


                    $f5 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed multiply and add';
                              if hasvex then
                                lastdisassembledata.opcode:='vpmaddwd'
                              else
                                lastdisassembledata.opcode:='pmaddwd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed multiply and add';
                              lastdisassembledata.opcode:='pmaddwd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $f6 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed sum of absolute differences';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsadbw'
                              else
                                lastdisassembledata.opcode:='psadbw';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed sum of absolute differences';
                              lastdisassembledata.opcode:='psadbw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $f7 : begin
                            if $66 in prefix2 then
                            begin
                              description:='store selected bytes of double quadword';
                              if hasvex then
                                lastdisassembledata.opcode:='vmaskmovdqu'
                              else
                                lastdisassembledata.opcode:='maskmovdqu';

                              opcodeflags.skipExtraReg:=true;
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='byte mask write';
                              lastdisassembledata.opcode:='maskmovq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $f8 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubb'
                              else
                                lastdisassembledata.opcode:='psubb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract';
                              lastdisassembledata.opcode:='psubb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $f9 : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubw'
                              else
                                lastdisassembledata.opcode:='psubw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract';
                              lastdisassembledata.opcode:='psubw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $fa : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract';
                              if hasvex then
                                lastdisassembledata.opcode:='vpsubd'
                              else
                                lastdisassembledata.opcode:='psubd';

                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract';
                              lastdisassembledata.opcode:='psubd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $fb : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed subtract';
                              lastdisassembledata.opcode:='psubq';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed subtract';
                              lastdisassembledata.opcode:='psubq';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $fc : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddb'
                              else
                                lastdisassembledata.opcode:='paddb';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last,mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add';
                              lastdisassembledata.opcode:='paddb';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $fd : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddw'
                              else
                                lastdisassembledata.opcode:='paddw';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);
                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add';
                              lastdisassembledata.opcode:='paddw';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

                              inc(offset,last-1);
                            end;
                          end;

                    $fe : begin
                            if $66 in prefix2 then
                            begin
                              description:='packed add';
                              if hasvex then
                                lastdisassembledata.opcode:='vpaddd'
                              else
                                lastdisassembledata.opcode:='paddd';
                              lastdisassembledata.parameters:=xmm(memory[2])+modrm(memory,prefix2,2,4,last, mRight);

                              inc(offset,last-1);
                            end
                            else
                            begin
                              description:='packed add';
                              lastdisassembledata.opcode:='paddd';
                              lastdisassembledata.parameters:=mm(memory[2])+modrm(memory,prefix2,2,3,last,mRight);

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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,8,0,mRight);

                  inc(offset,last-1);
                end;

          $13 : begin
                  description:='add with carry';
                  lastdisassembledata.opcode:='adc';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(lastdisassembledata.parametervalue),8)
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,8,0,mRight);

                  inc(offset,last-1);
                end;

          $1b : begin
                  description:='integer subtraction with borrow';
                  lastdisassembledata.opcode:='sbb';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);


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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(lastdisassembledata.parametervalue),8)
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);
                  inc(offset,last-1);
                end;

          $23 : begin
                  description:='logical and';
                  lastdisassembledata.opcode:='and';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(lastdisassembledata.parametervalue),8)
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

                  inc(offset,last-1);
                end;

          $2b : begin
                  description:='subtract';
                  lastdisassembledata.opcode:='sub';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+IntToHexs(longint(dwordptr^),8)
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

                  inc(offset,last-1);
                end;

          $33 : begin
                  description:='logical exclusive or';
                  lastdisassembledata.opcode:='xor';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(dwordptr^),8)
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

                  inc(offset,last-1);
                end;

          $3b : begin
                  description:='compare two operands';
                  lastdisassembledata.opcode:='cmp';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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


                    if rex_w then
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(dwordptr^),8)
                    else
                      lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+inttohexs(dwordptr^,8);
                    inc(offset,4);
                  end;
                end;

                //prefix bytes need fixing
          $3f : begin  //aas
                  if processhandler.is64bit then
                  begin
                    lastdisassembledata.opcode:='db';
                    lastdisassembledata.parameters:=inttohexs($3f,1);
                  end
                  else
                  begin
                    lastdisassembledata.opcode:='aas';
                    description:='ascii adjust al after subtraction';
                  end;
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

                  if is64bit then opcodeflags.w:=true;

                  lastdisassembledata.opcode:='push';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=rd16(memory[0]-$50) else
                    lastdisassembledata.parameters:=rd(memory[0]-$50);
                end;

          $58..$5f :
                begin
                  description:='pop a value from the stack';
                  if is64bit then opcodeflags.w:=true; //so rd will pick the 64-bit version
                  lastdisassembledata.opcode:='pop';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=rd16(memory[0]-$58) else
                    lastdisassembledata.parameters:=rd(memory[0]-$58);
                end;

          $60 : begin
                  description:='push all general-purpose registers';
                  if is64bit then description:=description+' (invalid)';
                  if $66 in prefix2 then lastdisassembledata.opcode:='pusha' else
                                         lastdisassembledata.opcode:='pushad';

                  if is64bit then
                  begin
                    description:=description+' (invalid)';
                    lastdisassembledata.opcode:='pushad (invalid)';
                  end;
                end;

          $61 : begin
                  description:='pop all general-purpose registers';
                  if $66 in prefix2 then lastdisassembledata.opcode:='popa' else
                                         lastdisassembledata.opcode:='popad';

                  if is64bit then
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
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                  inc(offset,last-1);

                end;

          $63 : begin
                  //arpl or movsxd
                  if is64bit then
                  begin
                    lastdisassembledata.opcode:='movsxd';
                    opcodeflags.w:=false;

                    lastdisassembledata.parameters:=' '+r64(memory[1])+modrm(memory,prefix2,1,0,last,32,0,mRight);
                    inc(offset,last-1);
                    description:='Move doubleword to quadword with signextension'
                  end
                  else
                  begin
                    lastdisassembledata.opcode:='arpl';
                    lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last)+r16(memory[1]);
                    inc(offset,last-1);
                    description:='adjust rpl field of segment selector';
                  end;
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
                    if processhandler.is64Bit then
                      lastdisassembledata.parameters:=inttohexs(longint(dwordptr^),8)
                    else
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
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight);
                    wordptr:=@memory[last];

                    lastdisassembledata.parametervaluetype:=dvtvalue;
                    lastdisassembledata.parametervalue:=wordptr^;

                    inc(offset,last-1+2);
                  end
                  else
                  begin
                    lastdisassembledata.opcode:='imul';
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);
                    dwordptr:=@memory[last];
                    if rex_w then
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(longint(dwordptr^),8)
                    else
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(dwordptr^,8);

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
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                  lastdisassembledata.parametervalue:=memory[last];
                  lastdisassembledata.parameters:=lastdisassembledata.parameters+','+inttohexs(memory[last],2);
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
                  description:='jump short if overflow (OF=1)';
                  lastdisassembledata.opcode:='jo';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                    lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)<>0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);



                end;

          $71 : begin
                  description:='jump short if not overflow (OF=0)';
                  lastdisassembledata.opcode:='jno';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_OF)=0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $72 : begin
                  description:='jump short if below/carry (CF=1)';
                  lastdisassembledata.opcode:='jb';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)<>0;
                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $73 : begin
                  description:='jump short if above or equal (CF=0)';
                  lastdisassembledata.opcode:='jae';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_CF)=0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $74 : begin
                  description:='jump short if equal (ZF=1)';
                  lastdisassembledata.opcode:='je';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));



                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $75 : begin
                  description:='jump short if not equal (ZF=0)';
                  lastdisassembledata.opcode:='jne';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;
                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $76 : begin
                  description:='jump short if not above (ZF=1 or CF=1)';
                  lastdisassembledata.opcode:='jna';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                    Lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))<>0;


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $77 : begin
                  description:='jump short if above (ZF=0 and CF=0)';
                  lastdisassembledata.opcode:='ja';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                    Lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and (EFLAGS_CF or EFLAGS_ZF))=0;


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $78 : begin
                  description:='jump short if sign (SF=1)';
                  lastdisassembledata.opcode:='js';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $79 : begin
                  description:='jump short if not sign (SF=0)';
                  lastdisassembledata.opcode:='jns';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $7a : begin
                  description:='jump short if parity (PF=1)';
                  lastdisassembledata.opcode:='jp';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)<>0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $7b : begin
                  description:='jump short if not parity (PF=0)';
                  lastdisassembledata.opcode:='jnp';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_PF)=0;

                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $7c : begin
                  description:='jump short if not greater or equal (SF~=OF)';
                  lastdisassembledata.opcode:='jl'; //jnge
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF);


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $7d : begin
                  description:='jump short if not less (greater or equal) (SF=OF)';
                  lastdisassembledata.opcode:='jnl';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF);


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.parametervaluetype:=dvtAddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                end;

          $7e : begin
                  description:='jump short if less or equal (ZF=1 or SF~=OF)';
                  lastdisassembledata.opcode:='jle';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)<>(context^.EFlags and EFLAGS_OF)) or ((context^.EFlags and EFLAGS_ZF)<>0);


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                  lastdisassembledata.parameterValueType:=dvtAddress;
                end;

          $7f : begin
                  description:='jump short if greater (ZF=0 or SF=OF)';
                  lastdisassembledata.opcode:='jg';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=((context^.EFlags and EFLAGS_SF)=(context^.EFlags and EFLAGS_OF)) and ((context^.EFlags and EFLAGS_ZF)=0);


                  inc(offset);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(shortint(memory[1])))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(shortint(memory[1])));

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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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
                            inc(offset,last-1+2);
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

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
                            inc(offset,last-1+4);
                          end;
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
                            begin
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                              lastdisassembledata.datasize:=8;;
                            end
                            else
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                            dwordptr:=@memory[last];
                            lastdisassembledata.parametervaluetype:=dvtvalue;
                            lastdisassembledata.parametervalue:=dwordptr^;

                            if Rex_W then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
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

                            lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2, true);
                          end else
                          begin
                            lastdisassembledata.opcode:='add';

                            if rex_w then
                            begin
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2, true)
                            end
                            else
                            begin
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);
                              lastdisassembledata.parametervaluetype:=dvtvalue;
                              lastdisassembledata.parametervalue:=memory[last];

                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2, true);
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
                  description:='exchange memory with register';
                  lastdisassembledata.opcode:='xchg';
                  lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last)+r8(memory[1]);
                  inc(offset,last-1);
                end;

          $87 : begin
                  description:='exchange memory with register';
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
                  lastdisassembledata.parameters:=r8(memory[1])+modrm(memory,prefix2,1,2,last,mRight);

                  inc(offset,last-1);
                end;

          $8b : begin
                  description:='copy memory';
                  lastdisassembledata.opcode:='mov';
                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                    lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

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
                  begin
                    if processhandler.is64Bit and ($67 in prefix2) then
                      lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,0,32,mRight)
                    else
                      lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,0,0,mRight);
                  end
                  else
                  begin
                    if processhandler.is64Bit and ($67 in prefix2) then
                      lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,0,32,mRight)
                    else
                      lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,0,0,mRight)
                  end;

                  inc(offset,last-1);
                end;

          $8e : begin
                  description:='copy memory';
                  lastdisassembledata.opcode:='mov';
                  lastdisassembledata.parameters:=sreg(memory[1])+modrm(memory,prefix2,1,1,last,mRight);

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
                  if prefixsize>0 then
                  begin
                    if RexPrefix<>0 then
                    begin

                      description:='Exchange '+ifthen(processhandler.is64bit,'R','E')+'AX with register';
                      lastdisassembledata.opcode:='xchg';
                      if rex_w then
                        lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+rd(memory[0]-$90)
                      else
                        lastdisassembledata.parameters:=colorreg+'eax'+endcolor+','+rd(memory[0]-$90);
                    end
                    else
                      lastdisassembledata.parameters:=inttohexs(prefixsize+1,1);
                  end;
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



                  if is64bit then
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
                    if is64bit then
                      lastdisassembledata.opcode:='pushfq'
                    else
                      lastdisassembledata.opcode:='pushfd';
                  end;
                end;

          $9d : begin
                  description:='pop stack into eflags register';
                  if $66 in prefix2 then lastdisassembledata.opcode:='popf' else
                  begin
                    if is64bit then
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


                  if processhandler.is64bit then
                  begin
                    lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+']';
                    inc(offset,8);
                  end
                  else
                  begin
                    lastdisassembledata.parameters:=colorreg+'al'+endcolor+','+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';
                    inc(offset,4);
                  end;


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


                    if processhandler.is64bit then
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+']'
                    else
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+']';

                  end;

                  if processhandler.is64bit then
                    inc(offset, 8)
                  else
                    inc(offset, 4);

                end;

          $a2 : begin
                  description:='copy memory';
                  dwordptr:=@memory[1];
                  lastdisassembledata.opcode:='mov';

                  lastdisassembledata.parametervaluetype:=dvtaddress;
                  lastdisassembledata.parametervalue:=dwordptr^;
                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  if processhandler.is64bit then
                    lastdisassembledata.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+'],'+colorreg+'al'+endcolor
                  else
                    lastdisassembledata.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],'+colorreg+'al'+endcolor;

                  if processhandler.is64bit then
                    inc(offset, 8)
                  else
                    inc(offset, 4);
                end;

          $a3 : begin
                  description:='copy memory';
                  lastdisassembledata.opcode:='mov';
                  dwordptr:=@memory[1];

                  lastdisassembledata.parametervaluetype:=dvtaddress;
                  lastdisassembledata.parametervalue:=dwordptr^;
                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  if processhandler.is64bit then
                    lastdisassembledata.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(pqword(dwordptr)^,8)+'],'
                  else
                    lastdisassembledata.parameters:=getsegmentoverride(prefix2)+'['+inttohexs(dwordptr^,8)+'],';

                  if $66 in prefix2 then
                    lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+'ax'+endcolor
                  else
                  begin
                    if rex_w then
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+'rax'+endcolor
                    else
                      lastdisassembledata.parameters:=lastdisassembledata.parameters+colorreg+'eax'+endcolor;
                  end;

                  if processhandler.is64bit then
                    inc(offset, 8)
                  else
                    inc(offset, 4);
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
                      lastdisassembledata.parameters:=colorreg+'rax'+endcolor+','+inttohexs(longint(dwordptr^),8)
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

    //              if Rex_B

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

{not in intel spec}
                    6:  begin
                          if $66 in prefix2 then
                          begin
                            lastdisassembledata.opcode:='sal';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);
                            lastdisassembledata.parametervaluetype:=dvtvalue;
                            lastdisassembledata.parametervalue:=memory[last];
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                            description:='signed multiply by 2 '+inttostr(memory[last])+' times';
                            inc(offset,last);
                          end
                          else
                          begin
                            lastdisassembledata.opcode:='sal';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);
                            lastdisassembledata.parametervaluetype:=dvtvalue;
                            lastdisassembledata.parametervalue:=memory[last];
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                            description:='signed multiply by 2 '+inttostr(memory[last])+' times';
                            inc(offset,last);
                          end;
                        end;
{^^^^^^^^^^^^^^^^^^}
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

                  wordptr:=@memory[1];
                  lastdisassembledata.parametervaluetype:=dvtvalue;
                  lastdisassembledata.parametervalue:=wordptr^;
                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);

                  lastdisassembledata.opcode:='ret';
                  LastDisassembleData.isret:=true;
                  lastdisassembledata.parameters:=inttohexs(wordptr^,4);
                  inc(offset,2);

                  description:='near return to calling procedure and pop '+inttostr(lastdisassembledata.parametervalue)+' bytes from stack';


                end;

          $c3 : begin
                  description:='near return to calling procedure';
                  lastdisassembledata.opcode:='ret';
                  LastDisassembleData.isret:=true;
                end;

          $c4 : begin
                  if processhandler.is64Bit=false then
                  begin
                    description:='load far pointer';
                    lastdisassembledata.opcode:='les';
                    if $66 in prefix2 then
                      lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                      lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                    inc(offset,last-1);
                  end;
                end;

          $c5 : begin
                  if processhandler.is64Bit=false then
                  begin
                    description:='load far pointer';
                    lastdisassembledata.opcode:='lds';
                    if $66 in prefix2 then
                      lastdisassembledata.parameters:=r16(memory[1])+modrm(memory,prefix2,1,1,last,mRight) else
                      lastdisassembledata.parameters:=r32(memory[1])+modrm(memory,prefix2,1,0,last,mRight);

                    inc(offset,last-1);
                  end;
                end;

          $c6 : begin
                  if memory[1]=$f8 then
                  begin
                    inc(offset);
                    lastdisassembledata.opcode:='xabort';
                    description:='transactional abort';

                    lastdisassembledata.parametervaluetype:=dvtvalue;
                    lastdisassembledata.parametervalue:=memory[2];
                    lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                    inc(lastdisassembledata.seperatorcount);
                    lastdisassembledata.parameters:=inttohexs(memory[2],2);

                  end
                  else
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
                  if memory[1]=$f8 then
                  begin
                    description:='Transactional Begin';
                    lastdisassembledata.opcode:='xbegin';

                    if MarkIPRelativeInstructions then
                    begin
                      LastDisassembleData.riprelative:=1;
                      riprelative:=true;
                    end;
                    inc(offset,4);
                    lastdisassembledata.parametervaluetype:=dvtaddress;



                    if is64bit then
                      lastdisassembledata.parametervalue:=qword(offset+pinteger(@memory[2])^)
                    else
                      lastdisassembledata.parametervalue:=dword(offset+pinteger(@memory[2])^);

                    lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                    lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                    inc(lastdisassembledata.seperatorcount);

                  end
                  else
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

                          if Rex_W then
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                          else
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                          dwordptr:=@memory[last];
                          lastdisassembledata.parametervaluetype:=dvtvalue;
                          lastdisassembledata.parametervalue:=dwordptr^;


                          if rex_w then
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+IntToHexs(longint(dwordptr^),8)
                          else
                            lastdisassembledata.parameters:=lastdisassembledata.parameters+IntToHexs(dwordptr^,8);

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
                          description:='unsigned divide by 2, once';
                          lastdisassembledata.opcode:='shr';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                          inc(offset,last-1);
                        end;

                    6:  begin
{undefined by intel}
                          description:='signed multiply by 2, once';
                          lastdisassembledata.opcode:='sal';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8)+'1';
                          inc(offset,last-1);
{^^^^^^^^^^^^^^^^^}
                        end;

                    7:  begin
                          description:='signed divide by 2, once';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0) )+'1';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
                            inc(offset,last-1);
                          end;
                        end;

                    6:  begin
{undefined by intel}
                          if $66 in prefix2 then
                          begin
                            description:='signed multiply by 2, once';
                            lastdisassembledata.opcode:='sal';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+'1';
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='signed multiply by 2, once';
                            lastdisassembledata.opcode:='sal';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
                            inc(offset,last-1);
                          end;
{^^^^^^^^^^^^^^^^^}
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last, ifthen(rex_w,64,0))+'1';
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
                          description:='unsigned divide by 2, cl times';
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
                          description:='signed divide by 2, cl times';
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

                    6:  begin
                          if $66 in prefix2 then
                          begin
                            description:='signed multiply by 2, cl times';
                            lastdisassembledata.opcode:='sal';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)+colorreg+'cl'+endcolor;
                            inc(offset,last-1);
                          end
                          else
                          begin
                            description:='signed multiply by 2, cl times';
                            lastdisassembledata.opcode:='sal';
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
                            lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$e8)+')';
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
                            lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$f0)+')';
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
                            lastdisassembledata.parameters:='st(0),st('+inttostr(memory[1]-$f8)+')';
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
                  lastdisassembledata.isfloat:=true;
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
                                     description:='store single';
                                     lastdisassembledata.opcode:='fst';
                                     lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                                     inc(offset,last-1);
                                   end;

                               3:  begin
                                     description:='store single';
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
                          description:='Push +1.0 onto the FPU register stack';
                          lastdisassembledata.opcode:='fld1';
                          inc(offset);
                        end;

                  $e9 : begin
                          description:='Push log2(10) onto the FPU register stack';
                          lastdisassembledata.opcode:='fldl2t';
                          inc(offset);
                        end;

                  $ea : begin
                          description:='Push log2(e) onto the FPU register stack';
                          lastdisassembledata.opcode:='fldl2e';
                          inc(offset);
                        end;

                  $eb : begin
                          description:='Push "pi" onto the FPU register stackload constant';
                          lastdisassembledata.opcode:='fldpi';
                          inc(offset);
                        end;

                  $ec : begin
                          description:='Push log10(2) onto the FPU register stack';
                          lastdisassembledata.opcode:='fldlg2';
                          inc(offset);
                        end;

                  $ed : begin
                          description:='Push log e(2) onto the FPU register stack';
                          lastdisassembledata.opcode:='fldln2';
                          inc(offset);
                        end;

                  $ee : begin
                          description:='Push +0.0 onto the FPU register stack';
                          lastdisassembledata.opcode:='fldz';
                          inc(offset);
                        end;


                  $f0 : begin
                          description:='compute 2^x-1';
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
                            description:='divide';
                            lastdisassembledata.opcode:='fidiv';
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                            inc(offset,last-1);
                          end;

                      7:  begin
                            description:='reverse divide';
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

                                  1:  begin
                                        description:='store integer with truncation';
                                        lastdisassembledata.opcode:='fisttp';
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
                                        lastdisassembledata.isfloat:=true;
                                        description:='load floating point value';
                                        lastdisassembledata.opcode:='fld';
                                        lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,80);

                                        inc(offset,last-1);
                                      end;

                                  7:  begin
                                        lastdisassembledata.isfloat:=true;
                                        description:='store extended';
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
                  lastdisassembledata.isfloat:=true;
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
                                       lastdisassembledata.isfloat:=true;
                                       description:='load floating point value';
                                       lastdisassembledata.opcode:='fld';
                                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                       inc(offset,last-1);
                                     end;

                                 1:  begin
                                       description:='store integer with truncation';
                                       lastdisassembledata.opcode:='fisttp';
                                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                       inc(offset,last-1);
                                     end;

                                 2:  begin
                                       lastdisassembledata.isfloat:=true;
                                       description:='store double';
                                       lastdisassembledata.opcode:='fst';
                                       lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);

                                       inc(offset,last-1);
                                     end;

                                 3:  begin
                                       lastdisassembledata.isfloat:=true;
                                       description:='store double';
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
                                       lastdisassembledata.opcode:='fnsave';
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
                               lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e8)+')';
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
                          if memory[1]>=$c0 then
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
                            lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c8)+'),st(0)';
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
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$e0)+'),st(0)';
                         end
                         else
                         begin
                           lastdisassembledata.opcode:='fisub';
                           lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last);

                         end;
                         inc(offset,last-1);
                       end;


                    5: begin
                         description:='reverse divide';
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
                         description:='reverse divide';
                         last:=2;
                         if memory[1]>=$f0 then
                         begin
                           lastdisassembledata.opcode:='fdivrp';
                           lastdisassembledata.parameters:='st('+inttostr(memory[1]-$f0)+'),st(0)';
                           inc(offset,last-1);
                         end
                         else
                         begin
                           description:='divide';
                           lastdisassembledata.opcode:='fidiv';
                           lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16);

                           inc(offset,last-1);
                         end;
                       end;

                    7: begin
                         description:='divide';
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
                  if memory[1] in [$c0..$c7] then
                  begin
                    description:='free floating-point register and pop (might not work)';
                    lastdisassembledata.opcode:='ffreep';
                    lastdisassembledata.parameters:='st('+inttostr(memory[1]-$c0)+')';
                    inc(offset);
                  end
                  else
                  case getreg(memory[1]) of
                    0:  begin
                          description:='load integer';
                          lastdisassembledata.opcode:='fild';
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,16);

                          inc(offset,last-1);
                        end;

                    1:  begin
                          description:='store integer with truncation';
                          lastdisassembledata.opcode:='fisttp';
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
                            lastdisassembledata.parameters:=colorreg+'ax'+endcolor;
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
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)=0;

                  lastdisassembledata.opcode:='loopne';

                  inc(offset);

                  lastdisassembledata.parametervaluetype:=dvtaddress;
                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);
                end;

          $e1 : begin
                  description:='loop according to ecx counter';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;
                  if context<>nil then
                              lastdisassembledata.willJumpAccordingToContext:=(context^.EFlags and EFLAGS_ZF)<>0;

                  lastdisassembledata.opcode:='loope';
                  inc(offset);

                  lastdisassembledata.parametervaluetype:=dvtaddress;
                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);
                end;

          $e2 : begin
                  description:='loop according to ecx counting';
                  lastdisassembledata.opcode:='loop';
                  if context<>nil then
                    lastdisassembledata.willJumpAccordingToContext:=context^.{$ifdef CPU64}RCX{$else}ECX{$endif}<>0;

                  lastdisassembledata.isjump:=true;
                  inc(offset);

                  lastdisassembledata.parametervaluetype:=dvtaddress;
                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

                  lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);

                  lastdisassembledata.seperators[lastdisassembledata.seperatorcount]:=1;
                  inc(lastdisassembledata.seperatorcount);
                end;

          $e3 : begin
                  description:='jump short if cx=0';
                  lastdisassembledata.isjump:=true;
                  lastdisassembledata.isconditionaljump:=true;

                  if $66 in prefix2 then
                  begin
                    lastdisassembledata.opcode:='jcxz';
                    if context<>nil then
                      lastdisassembledata.willJumpAccordingToContext:=((context^.{$ifdef CPU64}RCX{$else}ECX{$endif}) and $ffff)=0;

                  end
                  else
                  begin
                    lastdisassembledata.opcode:='jecxz';
                    if context<>nil then
                      lastdisassembledata.willJumpAccordingToContext:=context^.{$ifdef CPU64}RCX{$else}ECX{$endif}=0;

                  end;
                  inc(offset);

                  lastdisassembledata.parametervaluetype:=dvtaddress;



                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

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

                  if MarkIPRelativeInstructions then
                  begin
                    LastDisassembleData.riprelative:=1;
                    riprelative:=true;
                  end;
                  inc(offset,4);
                  lastdisassembledata.parametervaluetype:=dvtaddress;

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pinteger(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pinteger(@memory[1])^));

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
                    lastdisassembledata.parametervalue:=dword(offset+qword(psmallint(@memory[1])^));
                    lastdisassembledata.parameters:=inttohexs(lastdisassembledata.parametervalue,8);
                  end
                  else
                  begin
                    lastdisassembledata.opcode:='jmp';

                    if MarkIPRelativeInstructions then
                    begin
                      LastDisassembleData.riprelative:=1;
                      riprelative:=true;
                    end;

                    inc(offset,4);
                    lastdisassembledata.parametervaluetype:=dvtaddress;

                    if is64bit then
                      lastdisassembledata.parametervalue:=qword(offset+qword(pinteger(@memory[1])^))
                    else
                      lastdisassembledata.parametervalue:=dword(offset+qword(pinteger(@memory[1])^));

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

                  if is64bit then
                    lastdisassembledata.parametervalue:=qword(offset+qword(pshortint(@memory[1])^))
                  else
                    lastdisassembledata.parametervalue:=dword(offset+qword(pshortint(@memory[1])^));

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
                          lastdisassembledata.parameters:=modrm(memory,prefix2,1,2,last,8);
                          lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(memory[last],2);
                          lastdisassembledata.parametervaluetype:=dvtValue;
                          lastdisassembledata.parametervalue:=memory[last];


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
                            if rex_w then
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(longint(dwordptr^),8)
                            else
                              lastdisassembledata.parameters:=lastdisassembledata.parameters+inttohexs(dwordptr^,8);
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
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,ifthen(Rex_W,64,0));


                          inc(offset,last-1);
                        end;

                    1:  begin
                          description:='decrement by 1';
                          lastdisassembledata.opcode:='dec';
                          if $66 in prefix2 then
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,1,last,16)
                          else
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,ifthen(Rex_W,64,0));

                          inc(offset,last-1);
                        end;

                    2:  begin
                          //call
                          description:='call procedure';
                          lastdisassembledata.opcode:='call';
                          lastdisassembledata.isjump:=true;
                          lastdisassembledata.iscall:=true;

                          if memory[1]>=$c0 then
                          begin
                            if is64bit then
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64) else
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                          end
                          else
                          begin
                            if is64bit then
                            begin

                              if (memory[1]=$15) and (pdword(@memory[2])^=2) and (pword(@memory[6])^=$8eb) then //special 16 byte call
                              begin
                                lastdisassembledata.parameters:=inttohexs(pqword(@memory[8])^,8);
                                LastDisassembleData.parameterValue:=pqword(@memory[8])^;
                                LastDisassembleData.parameterValueType:=dvtAddress;

                                inc(last,8+4+2+2);

                                LastDisassembleData.Seperators[0]:=2;
                                LastDisassembleData.Seperators[1]:=2+4;
                                LastDisassembleData.Seperators[2]:=2+4+2;
                                LastDisassembleData.SeperatorCount:=3;

                              end
                              else
                                lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                            end
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

                          if is64bit then
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64)
                          else
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);

                          inc(offset,last-1);
                        end;

                    4:  begin
                          //jmp
                          description:='jump near';
                          lastdisassembledata.opcode:='jmp';
                          lastdisassembledata.isjump:=true;


                          if is64bit then
                          begin
                            if (memory[1]=$25) and (pdword(@memory[2])^=0) then //special 14 byte jmp
                            begin
                              LastDisassembleData.parameterValue:=pqword(@memory[6])^;
                              LastDisassembleData.parameterValueType:=dvtAddress;

                              lastdisassembledata.parameters:=inttohexs(pqword(@memory[6])^,8);
                              inc(last,8+4+2);

                              LastDisassembleData.Seperators[0]:=2;
                              LastDisassembleData.Seperators[1]:=2+4;
                              LastDisassembleData.SeperatorCount:=2;

                            end
                            else
                              lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,64);
                          end
                          else
                            lastdisassembledata.parameters:=modrm(memory,prefix2,1,0,last,32);


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


        debugpart:=12;
        // if dataonly then exit;     //no need to handle the other stuff, dataonly means I'm only interested in the addresses, not bytes or extra parameters

        if (lastdisassembledata.parameters<>'') and (lastdisassembledata.parameters[length(lastdisassembledata.parameters)]=',') then
          setlength(lastdisassembledata.parameters, length(lastdisassembledata.parameters)-1);


       // tempresult:=tempresult+LastDisassembleData.opcode+' '+LastDisassembleData.parameters;

        LastDisassembleData.description:=description;

        //copy the remaining bytes
        k:=length(LastDisassembleData.Bytes);

        if int64(offset-initialoffset)<k then
          offset:=initialoffset+k;

        setlength(LastDisassembleData.Bytes,offset-initialoffset);
        debugpart:=13;

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

        debugpart:=14;

        if td>0 then
        begin
          breaknow:=false;
          try

            copymemory(@LastDisassembleData.Bytes[k], @_memory[k], td);
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

        debugpart:=15;


        //adjust for the prefix.
        if k<>0 then
        begin
          for i:=0 to LastDisassembleData.SeperatorCount-1 do
            inc(LastDisassembleData.Seperators[i],prefixsize);

          if LastDisassembleData.riprelative<>0 then
            inc(LastDisassembleData.riprelative, prefixsize);
        end;




        //todo: Next time the disassembler is getting an averhaul, do something about the prefix counting and the unnecessary readprocessmemorys associated with it


      //  result:=result+'- '+tempresult;

        debugpart:=16;

        if riprelative then
        begin
          debugpart:=17;
          //add the current offset to the code between []
          LastDisassembleData.modrmValue:=offset+ptrint(integer(LastDisassembleData.modrmValue)); //sign extended increase

          i:=pos('[',LastDisassembleData.parameters);
          j:=PosEx(']',LastDisassembleData.parameters,i);
         // tempresult:=copy(LastDisassembleData.parameters,i+1,j-i-1);


          tempaddress:=LastDisassembleData.modrmValue;

          tempresult:=copy(LastDisassembleData.parameters,1,i);
          tempresult:=tempresult+inttohexs(tempaddress,8);
          LastDisassembleData.parameters:=tempresult+copy(LastDisassembleData.parameters,j,length(LastDisassembleData.parameters));
        end;

        debugpart:=18;





      end
      else
      begin
        LastDisassembleData.opcode:='??';
        inc(offset);
      end;
    {$ifdef windows}
      LastDisassembleData.iscloaked:=hasCloakedRegionInRange(LastDisassembleData.address, length(LastDisassembleData.Bytes), VA, PA);
    {$else}
    LastDisassembleData.iscloaked:=false;
    {$endif}
      debugpart:=19;

      if not dataonly then
      begin
        result:=inttohex(LastDisassembleData.address,8)+' - '+getLastBytestring;
        result:=result+' - ';
        result:=result+LastDisassembleData.prefix+LastDisassembleData.opcode;
        result:=result+' ';
        result:=result+LastDisassembleData.parameters;
      end;

      debugpart:=20;

      if assigned(OnPostDisassemble) then
      begin
        debugpart:=21;
        tempresult:=result;
        tempdescription:=description;

        if OnPostDisassemble(self, initialoffset, LastDisassembleData, tempresult, tempdescription) then
        begin
          result:=tempresult;
          description:=tempdescription;

          if length(LastDisassembleData.Bytes)>0 then
            offset:=initialoffset+length(LastDisassembleData.Bytes);
        end;
      end;

      debugpart:=22;
    //finally
    //  cs.Leave;
    //end
  {  for i:=32 to 63 do
      if _memory[i]<>$ce then
        raise exception.create('Memory corruption in the disassembler'); }
  except
    on e:exception do
    begin
      outputdebugstring(inttohex(startoffset,8)+':disassembler exception:'+e.message);
      MessageBox(0,pchar('disassembler exception at '+inttohex(startoffset,8)+#13#10+'Debugpart='+inttostr(debugpart)+#13#10+e.message+#13#10+#13#10+'Please provide dark byte the bytes that are at this address so he can fix it'#13#10'(Open another CE instance and in the hexadecimal view go to this address)'),'debug here',MB_OK);
    end;
  end;
end;

function TDisassembler.getLastBytestring: string;
var
  i,j: integer;
  cloaked:boolean=false;
  changed:boolean=false;
  VA,PA: qword;
begin
  result:='';
  for i:=0 to length(LastDisassembleData.Bytes)-1 do
  begin
    if syntaxhighlighting then
    begin
      if LastDisassembleData.iscloaked then
      begin
        //check if this byte is cloaked (due to pageboundaries)
        {$ifdef windows}
        cloaked:=hasCloakedRegionInRange(LastDisassembleData.address+i, 1, VA, PA);
        if (cloaked) then result:=result+'{C00FF00}'; //green
        {$endif}
      end;

      changed:=hasAddressBeenChanged(LastDisassembleData.address+i);
      if (changed) then result:=result+'{C0000FF}'; //red
    end;

    result:=result+inttohex(LastDisassembleData.Bytes[i],2);

    if i<LastDisassembleData.prefixsize then
      result:=result+' '
    else
      for j:=0 to LastDisassembleData.SeperatorCount-1 do
        if (LastDisassembleData.Seperators[j]=i+1) then  //followed by a seperator
          result:=result+' ';

    if syntaxhighlighting and ((LastDisassembleData.iscloaked and cloaked) or changed ) then
    begin
      result:=result+'{N}'; //back to default
      cloaked:=false;
      changed:=false;
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

  aggressive: boolean;
begin
  if d=nil then
    d:=defaultDisassembler;

  aggressive:=d.aggressivealignment;
  d.aggressivealignment:=true;

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

  d.aggressivealignment:=aggressive;
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


      s:=symhandler.getNameFromAddress(jumpAddress, symhandler.showsymbols, symhandler.showmodules, symhandler.showsections, nil,nil,8,false);
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

    try  //could cause a parser exception if a weird symbol is used
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
                    ts:='->'+symhandler.getNameFromAddress(value,symhandler.showsymbols, symhandler.showmodules,symhandler.showsections, nil,nil,8,false);
                  end;
                end;
              end
              else
              begin
                ts:=symhandler.getNameFromAddress(value,symhandler.showsymbols, symhandler.showmodules,symhandler.showsections,nil,nil,8,false);
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
    except
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
  if (showsymbols or showmodules or showsections) and (chars>=8) then
  begin
    found:=false;
    result:=symhandler.getNameFromAddress(value,showsymbols, showmodules, showsections, nil, @found,chars, false);

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


constructor TDisassembler.create;
begin
 // cs:=TCriticalSection.Create;
end;

destructor TDisassembler.destroy;
begin
 // freeandnil(cs);
  if self=visibleDisassembler then
    visibleDisassembler:=nil;

  if self=defaultDisassembler then
    defaultDisassembler:=nil;

  inherited destroy;
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



