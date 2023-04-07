unit contexthandler;

//helper for dealing with different context types

//{$mode ObjFPC}{$H+}
{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  {$ifdef darwin}macport,{$endif}{$ifdef windows}windows, {$endif}Classes, SysUtils,StringHashList, newkernelhandler,math;

type

  TContextElement_register=record
    entrytype: integer;  //0=normal, 1=bit (size is now bitlength)
    name: string;
    size: integer;
    displayType: integer;
    ContextOffset: integer;
    BitStart: integer; //when entrytype<>0
  public
    function getPointer(context: pointer): pointer;
    function getDword(context: pointer): dword;
    function getQword(context: pointer): qword;
    function getValue(context: pointer): qword;
    function getValueString(context: pointer): string; //up to 8 0's
    function getFullValueString(context: pointer): string;  //all the 0's you want
    function getFlag(context: pointer): dword;

    procedure setValue(context: pointer; value:string); overload;
    procedure setValue(context: pointer; value:qword); overload;
  end;
  PContextElement_register=^TContextElement_register;


  TContextElementRegisterList=array of TContextElement_register;
  PContextElementRegisterList=^TContextElementRegisterList;

  TContextInfo=class
  private
    nameToEntryLookup: TStringHashList;

    fcontextsize: integer;
    general: PContextElementRegisterList;
    altgeneral: PContextElementRegisterList; //alt names and subfields (like AX, AH, SIL, R15=PC, etc... Won't be shown, but text lookups will work with this)
    specialized: PContextElementRegisterList; //CS, DS, FS, fsbase, gsbase, etc...
    flags: PContextElementRegisterList;
    mainfpu: PContextElementRegisterList;
    mainfpuname: string;
    secondaryfpu: PContextElementRegisterList; //for obsolete types like FP(#)
    secondaryfpuname: string;


    fGeneralPurposeRegisterMaxCharCount: integer;
    fGeneralPurposeFlagMaxCharCount: integer;
    name: string;

    fInstructionPointerRegister: PContextElement_register;
    fStackPointerRegister: PContextElement_register;
    fContextFlagsField: PContextElement_register;

    procedure setGeneralPurposeRegisters(list: PContextElementRegisterList);
    procedure setGeneralPurposeFlags(list: PContextElementRegisterList);
    procedure setFloatingPointRegisters(list: PContextElementRegisterList);
    procedure setSpecializedRegisters(list: PContextElementRegisterList);
    procedure setSecondaryFloatingPointRegisters(list: PContextElementRegisterList);
    procedure setAltnameRegisters(list: PContextElementRegisterList);
  public
    function getGeneralPurposeRegisters: PContextElementRegisterList; //the main ones (so not the parts that are subparts of the main ones)
    function getGeneralPurposeFlags: PContextElementRegisterList;
    function getFloatingPointRegisters: PContextElementRegisterList;
    function getSpecializedRegisters: PContextElementRegisterList;
    function getAlternateFloatingPointRegisters: PContextElementRegisterList;
    function getRegister(regname: string): PContextElement_register;

    function getFloatingPointRegisterSetName: string;
    function getAlternateFloatingPointRegisterSetName: string;

    function getCopy(context: pointer): pointer;
    function isDifferent(context1, context2: pointer): boolean;
    procedure setcontextflags(context: pointer; contextflags: qword);   //if the context structure has context flags, this will set them, else ignored (get/set all)


    property GeneralPurposeRegisterMaxCharCount: integer read fGeneralPurposeRegisterMaxCharCount;
    property GeneralPurposeFlagMaxCharCount: integer read fGeneralPurposeFlagMaxCharCount;
    property InstructionPointerRegister: PContextElement_register read fInstructionPointerRegister;
    property StackPointerRegister: PContextElement_register read fStackPointerRegister;
    property ContextSize: integer read fcontextsize;

    constructor create;
    destructor destroy; override;
  end;

  TSmartContext=Class //todo: replace all PContext and TContext with a SmartContext instead
  private
    context: pointer;
    contextinfo: TContextInfo;
  public
    //getReg, setReg, getRegList, ...
  end;

var
  ContextInfo_X86_32: TContextInfo;
  {$ifdef cpu64}
  ContextInfo_X86_64: TContextInfo;
  {$endif}

  ContextInfo_ARM_32: TContextInfo;
  ContextInfo_ARM_64: TContextInfo;


function getBestContextHandler:TContextInfo;

implementation

uses ProcessHandlerUnit, parsers, symbolhandler;


{$ifdef cpu32}
const X86_32Context: array of TContextElement_register=(
  (entrytype:0; name:'EAX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Eax); BitStart:0),
  (entrytype:0; name:'EBX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Ebx); BitStart:0),
  (entrytype:0; name:'ECX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Ecx); BitStart:0),
  (entrytype:0; name:'EDX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Edx); BitStart:0),
  (entrytype:0; name:'ESI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Esi); BitStart:0),
  (entrytype:0; name:'EDI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Edi); BitStart:0),
  (entrytype:0; name:'EBP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Ebp); BitStart:0),
  (entrytype:0; name:'ESP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Esp); BitStart:0),
  (entrytype:0; name:'EIP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.Eip); BitStart:0)
);

const X86_32Context_flags:  array of TContextElement_register=(
  (entrytype:1; name:'CF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 0),
  (entrytype:1; name:'PF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 2),
  (entrytype:1; name:'AF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 4),
  (entrytype:1; name:'ZF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 6),
  (entrytype:1; name:'SF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 7),
  (entrytype:1; name:'DF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 10),
  (entrytype:1; name:'OF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.EFlags); bitstart: 11)
);

var X86_32Context_fpu, X86_32Context_fpu2: array of TContextElement_register;

const X86_32Context_specialized: array of TContextElement_register=(
 (entrytype:0; name:'CS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegCs); BitStart:0),
 (entrytype:0; name:'SS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegSs); BitStart:0),
 (entrytype:0; name:'DS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegDs); BitStart:0),
 (entrytype:0; name:'ES'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegEs); BitStart:0),
 (entrytype:0; name:'FS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegFs); BitStart:0),
 (entrytype:0; name:'GS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.SegGs); BitStart:0)
);

const X86_32Context_altnames: array of TContextElement_register=(
  (entrytype:0; name:'AX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.eax); BitStart:0),
  (entrytype:0; name:'AH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.eax)+1; BitStart:0),
  (entrytype:0; name:'AL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.eax); BitStart:0),

  (entrytype:0; name:'CX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ecx); BitStart:0),
  (entrytype:0; name:'CH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ecx)+1; BitStart:0),
  (entrytype:0; name:'CL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ecx); BitStart:0),

  (entrytype:0; name:'DX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.edx); BitStart:0),
  (entrytype:0; name:'DH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.edx)+1; BitStart:0),
  (entrytype:0; name:'DL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.edx); BitStart:0),


  (entrytype:0; name:'BX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ebx); BitStart:0),
  (entrytype:0; name:'BH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ebx)+1; BitStart:0),
  (entrytype:0; name:'BL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ebx); BitStart:0),

  (entrytype:0; name:'SP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.esp); BitStart:0),
  (entrytype:0; name:'BP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ebp); BitStart:0),
  (entrytype:0; name:'SI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.esi); BitStart:0),
  (entrytype:0; name:'DI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.edi); BitStart:0)
);

const X86_32Context_controlreg: TContextElement_register=(entrytype:0; name:'ContextFlags'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT32(nil)^.ContextFlags); BitStart:0);
{$endif}


{$ifdef cpu64}
//wow still uses a 64-bit context
const X86_32Context: array of TContextElement_register=(
  (entrytype:0; name:'EAX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rax); BitStart:0),
  (entrytype:0; name:'EBX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rbx); BitStart:0),
  (entrytype:0; name:'ECX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rcx); BitStart:0),
  (entrytype:0; name:'EDX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rdx); BitStart:0),
  (entrytype:0; name:'ESI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rsi); BitStart:0),
  (entrytype:0; name:'EDI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rdi); BitStart:0),
  (entrytype:0; name:'EBP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rbp); BitStart:0),
  (entrytype:0; name:'ESP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rsp); BitStart:0),
  (entrytype:0; name:'EIP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rip); BitStart:0)
);

const X86_32Context_flags:  array of TContextElement_register=(
  (entrytype:1; name:'CF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 0),
  (entrytype:1; name:'PF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 2),
  (entrytype:1; name:'AF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 4),
  (entrytype:1; name:'ZF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 6),
  (entrytype:1; name:'SF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 7),
  (entrytype:1; name:'DF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 10),
  (entrytype:1; name:'OF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 11)
);


const X86_32Context_specialized: array of TContextElement_register=(
 (entrytype:0; name:'CS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegCs); BitStart:0),
 (entrytype:0; name:'SS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegSs); BitStart:0),
 (entrytype:0; name:'DS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegDs); BitStart:0),
 (entrytype:0; name:'ES'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegEs); BitStart:0),
 (entrytype:0; name:'FS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegFs); BitStart:0),
 (entrytype:0; name:'GS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegGs); BitStart:0)
);

const X86_32Context_altnames: array of TContextElement_register=(
  (entrytype:0; name:'AX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax); BitStart:0),
  (entrytype:0; name:'AH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax)+1; BitStart:0),
  (entrytype:0; name:'AL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax); BitStart:0),

  (entrytype:0; name:'CX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx); BitStart:0),
  (entrytype:0; name:'CH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx)+1; BitStart:0),
  (entrytype:0; name:'CL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx); BitStart:0),

  (entrytype:0; name:'DX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx); BitStart:0),
  (entrytype:0; name:'DH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx)+1; BitStart:0),
  (entrytype:0; name:'DL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx); BitStart:0),


  (entrytype:0; name:'BX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx); BitStart:0),
  (entrytype:0; name:'BH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx)+1; BitStart:0),
  (entrytype:0; name:'BL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx); BitStart:0),

  (entrytype:0; name:'SP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsp); BitStart:0),
  (entrytype:0; name:'BP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbp); BitStart:0),
  (entrytype:0; name:'SI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsi); BitStart:0),
  (entrytype:0; name:'DI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdi); BitStart:0)
);


const X86_32Context_controlreg: TContextElement_register=(entrytype:0; name:'ContextFlags'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.ContextFlags); BitStart:0);

var X86_32Context_fpu, X86_32Context_fpu2: array of TContextElement_register;

const X86_64Context: array of TContextElement_register=(
  (entrytype:0; name:'RAX'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax); BitStart:0),
  (entrytype:0; name:'RBX'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx); BitStart:0),
  (entrytype:0; name:'RCX'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx); BitStart:0),
  (entrytype:0; name:'RDX'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx); BitStart:0),
  (entrytype:0; name:'RSI'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsi); BitStart:0),
  (entrytype:0; name:'RDI'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdi); BitStart:0),
  (entrytype:0; name:'RBP'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbp); BitStart:0),
  (entrytype:0; name:'RSP'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsp); BitStart:0),
  (entrytype:0; name:'R8'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R8); BitStart:0),
  (entrytype:0; name:'R9'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R9); BitStart:0),
  (entrytype:0; name:'R10'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R10); BitStart:0),
  (entrytype:0; name:'R11'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R11); BitStart:0),
  (entrytype:0; name:'R12'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R12); BitStart:0),
  (entrytype:0; name:'R13'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R13); BitStart:0),
  (entrytype:0; name:'R14'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R14); BitStart:0),
  (entrytype:0; name:'R15'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R15); BitStart:0),
  (entrytype:0; name:'RIP'; size:8; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rip); BitStart:0)
);

const X86_64Context_altnames: array of TContextElement_register=(
  (entrytype:0; name:'EAX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rax); BitStart:0),
  (entrytype:0; name:'EBX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rbx); BitStart:0),
  (entrytype:0; name:'ECX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rcx); BitStart:0),
  (entrytype:0; name:'EDX'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rdx); BitStart:0),
  (entrytype:0; name:'ESI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rsi); BitStart:0),
  (entrytype:0; name:'EDI'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rdi); BitStart:0),
  (entrytype:0; name:'EBP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rbp); BitStart:0),
  (entrytype:0; name:'ESP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rsp); BitStart:0),
  (entrytype:0; name:'EIP'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.rip); BitStart:0),

  (entrytype:0; name:'AX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax); BitStart:0),
  (entrytype:0; name:'AH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax)+1; BitStart:0),
  (entrytype:0; name:'AL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rax); BitStart:0),

  (entrytype:0; name:'CX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx); BitStart:0),
  (entrytype:0; name:'CH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx)+1; BitStart:0),
  (entrytype:0; name:'CL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rcx); BitStart:0),

  (entrytype:0; name:'DX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx); BitStart:0),
  (entrytype:0; name:'DH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx)+1; BitStart:0),
  (entrytype:0; name:'DL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdx); BitStart:0),


  (entrytype:0; name:'BX';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx); BitStart:0),
  (entrytype:0; name:'BH';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx)+1; BitStart:0),
  (entrytype:0; name:'BL';  size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbx); BitStart:0),

  (entrytype:0; name:'SP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsp); BitStart:0),
  (entrytype:0; name:'SPL'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsp); BitStart:0),
  (entrytype:0; name:'BP';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbp); BitStart:0),
  (entrytype:0; name:'BPL'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rbp); BitStart:0),
  (entrytype:0; name:'SI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsi); BitStart:0),
  (entrytype:0; name:'SIL'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rsi); BitStart:0),
  (entrytype:0; name:'DI';  size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdi); BitStart:0),
  (entrytype:0; name:'DIL'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.Rdi); BitStart:0),
  (entrytype:0; name:'R8B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R8); BitStart:0),
  (entrytype:0; name:'R8L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R8); BitStart:0),
  (entrytype:0; name:'R8W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R8); BitStart:0),
  (entrytype:0; name:'R8D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R8); BitStart:0),

  (entrytype:0; name:'R9B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R9); BitStart:0),
  (entrytype:0; name:'R9L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R9); BitStart:0),
  (entrytype:0; name:'R9W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R9); BitStart:0),
  (entrytype:0; name:'R9D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R9); BitStart:0),

  (entrytype:0; name:'R10B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R10); BitStart:0),
  (entrytype:0; name:'R10L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R10); BitStart:0),
  (entrytype:0; name:'R10W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R10); BitStart:0),
  (entrytype:0; name:'R10D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R10); BitStart:0),

  (entrytype:0; name:'R11B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R11); BitStart:0),
  (entrytype:0; name:'R11L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R11); BitStart:0),
  (entrytype:0; name:'R11W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R11); BitStart:0),
  (entrytype:0; name:'R11D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R11); BitStart:0),

  (entrytype:0; name:'R12B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R12); BitStart:0),
  (entrytype:0; name:'R12L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R12); BitStart:0),
  (entrytype:0; name:'R12W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R12); BitStart:0),
  (entrytype:0; name:'R12D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R12); BitStart:0),

  (entrytype:0; name:'R13B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R13); BitStart:0),
  (entrytype:0; name:'R13L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R13); BitStart:0),
  (entrytype:0; name:'R13W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R13); BitStart:0),
  (entrytype:0; name:'R13D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R13); BitStart:0),

  (entrytype:0; name:'R14B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R14); BitStart:0),
  (entrytype:0; name:'R14L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R14); BitStart:0),
  (entrytype:0; name:'R14W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R14); BitStart:0),
  (entrytype:0; name:'R14D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R14); BitStart:0),

  (entrytype:0; name:'R15B'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R15); BitStart:0),
  (entrytype:0; name:'R15L'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R15); BitStart:0),
  (entrytype:0; name:'R15W'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R15); BitStart:0),
  (entrytype:0; name:'R15D'; size:4; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.R15); BitStart:0)
);


const X86_64Context_flags:  array of TContextElement_register=(

  (entrytype:1; name:'CF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 0),
  (entrytype:1; name:'PF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 2),
  (entrytype:1; name:'AF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 4),
  (entrytype:1; name:'ZF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 6),
  (entrytype:1; name:'SF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 7),
  (entrytype:1; name:'DF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 10),
  (entrytype:1; name:'OF'; size:1; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.EFlags); bitstart: 11)
);

var X86_64Context_fpu: array of TContextElement_register; //needs to be added manually:  integer(@PCONTEXT(nil)^.FltSave.XmmRegisters[0]) is not allowed

const X86_64Context_specialized: array of TContextElement_register=(
 (entrytype:0; name:'CS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegCs); BitStart:0),
 (entrytype:0; name:'SS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegSs); BitStart:0),
 (entrytype:0; name:'DS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegDs); BitStart:0),
 (entrytype:0; name:'ES'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegEs); BitStart:0),
 (entrytype:0; name:'FS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegFs); BitStart:0),
 (entrytype:0; name:'GS'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.SegGs); BitStart:0)
);
const X86_64Context_controlreg: TContextElement_register=(entrytype:0; name:'ContextFlags'; size:2; displayType: 0; ContextOffset: integer(@PCONTEXT(nil)^.ContextFlags); BitStart:0);

{$endif}

const ARM_32Context: array of TContextElement_register=(
  (entrytype:0; name:'ORIG_R0'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.ORIG_R0); BitStart:0),
  (entrytype:0; name:'R0'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R0); BitStart:0),
  (entrytype:0; name:'R1'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R1); BitStart:0),
  (entrytype:0; name:'R2'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R2); BitStart:0),
  (entrytype:0; name:'R3'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R3); BitStart:0),
  (entrytype:0; name:'R4'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R4); BitStart:0),
  (entrytype:0; name:'R5'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R5); BitStart:0),
  (entrytype:0; name:'R6'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R6); BitStart:0),
  (entrytype:0; name:'R7'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R7); BitStart:0),
  (entrytype:0; name:'R8'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R8); BitStart:0),
  (entrytype:0; name:'R9'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R9); BitStart:0),
  (entrytype:0; name:'R10'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.R10); BitStart:0),
  (entrytype:0; name:'FP'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.FP); BitStart:0),
  (entrytype:0; name:'IP'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.IP); BitStart:0),
  (entrytype:0; name:'SP'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.SP); BitStart:0),
  (entrytype:0; name:'LR'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.LR); BitStart:0),
  (entrytype:0; name:'PC'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.PC); BitStart:0)
);

const ARM_32Context_flags: array of TContextElement_register=(
  (entrytype:1; name:'N'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 31),
  (entrytype:1; name:'Z'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 30),
  (entrytype:1; name:'C'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 29),
  (entrytype:1; name:'V'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 28),
  (entrytype:1; name:'Q'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 27),
//  (entrytype:1; name:'IT'; size:2; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 25),
  (entrytype:1; name:'J'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 24),
  (entrytype:1; name:'GE'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 16),
//  (entrytype:1; name:'IT'; size:6; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 10),
  (entrytype:1; name:'E'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 9),
  (entrytype:1; name:'A'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 8),
  (entrytype:1; name:'I'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 7),
  (entrytype:1; name:'F'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 6),
  (entrytype:1; name:'T'; size:1; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 5),
  (entrytype:1; name:'M'; size:4; displayType: 0; ContextOffset: integer(@PARMCONTEXT(nil)^.CPSR); bitstart: 0)
);

var ARM_32Context_fpu:  array of TContextElement_register;

// ARM64:
var ARM_64Context: array of TContextElement_register;
const ARM_64Context_flags: array of TContextElement_register=(
  (entrytype:1; name:'N'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 31),
  (entrytype:1; name:'Z'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 30),
  (entrytype:1; name:'C'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 29),
  (entrytype:1; name:'V'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 28),
  (entrytype:1; name:'SS'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 21),
  (entrytype:1; name:'IL'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 20),
  (entrytype:1; name:'D'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 9),
  (entrytype:1; name:'A'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 8),
  (entrytype:1; name:'I'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 7),
  (entrytype:1; name:'F'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 6),
  (entrytype:1; name:'M32'; size:1; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 4),
  (entrytype:1; name:'M'; size:4; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.PSTATE); bitstart: 0)
);

{$ifdef darwin}
const ARM_64Context_controlreg: TContextElement_register=(entrytype:0; name:'ContextFlags'; size:2; displayType: 0; ContextOffset: integer(@PARM64CONTEXT(nil)^.ContextFlags); BitStart:0);
{$endif}


var ARM_64Context_fpu:  array of TContextElement_register;


function TContextElement_register.getPointer(context: pointer): pointer;
begin
  result:=pointer(ptruint(context)+contextOffset);
end;

function TContextElement_register.getDword(context: pointer): dword;
begin
  if entrytype=0 then
    result:=pdword(getPointer(context))^
  else
    result:=getflag(context);
end;

function TContextElement_register.getQword(context: pointer): qword;
begin
  if entrytype=0 then
    result:=pqword(getPointer(context))^
  else
    result:=getflag(context);
end;

function TContextElement_register.getValue(context: pointer): qword;
var
  mask: qword;
  v: qword;
begin
  if entrytype=0 then
  begin
    case size of
      1: exit(getDword(context) and $ff);
      2: exit(getDword(context) and $ffff);
      4: exit(getDword(context));
      8: exit(getQword(context));
      16: exit(getQword(context));
      else
      begin
        mask:=(1 shl (size*8))-1;
        v:=getQword(context);
        v:=v and mask;
        exit(v);
      end;
    end;
  end
  else
    exit(getFlag(context));
end;

function TContextElement_register.getValueString(context: pointer): string; //up to 8 0's
var v: qword;
begin
  v:=getValue(context);
  if entrytype=0 then
  begin
    if size>=4 then
      result:=inttohex(v,8)
    else
    if size>=2 then
      result:=inttohex(v,4)
    else
      result:=inttohex(v,2)
  end
  else
    result:=inttostr(v);
end;

function TContextElement_register.getFullValueString(context: pointer): string;  //all the 0's
var v: qword;
begin
  v:=getValue(context);
  if entrytype=0 then
  begin
    if size=8 then
      result:=inttohex(v,16)
    else
    if size=4 then
      result:=inttohex(v,8)
    else
    if size>=2 then
      result:=inttohex(v,4)
    else
      result:=inttohex(v,2)
  end
  else
    result:=IntToStr(v);
end;

procedure TContextElement_register.setValue(context: pointer; value:qword);
var
  p: pointer;
  v: qword;
  m: qword;
begin
  p:=getPointer(context);

  if entrytype=0 then
  begin
    case size of
      1: pbyte(p)^:=byte(value);
      2: pword(p)^:=word(value);
      4: pdword(p)^:=dword(value);
      8: pqword(p)^:=qword(value);
      else
      begin
        v:=pqword(p)^;
        m:=(1 shl (size*8))-1;
        v:=v and (not m); //strip out changed bits
        value:=value or m;
        pqword(p)^:=v or value;
      end;
    end;
  end
  else
  begin
    v:=pqword(p)^;
    m:=(1 shl size)-1;
    m:=m shl BitStart;
    v:=v and (not m);

    value:=value shl bitstart;
    pqword(p)^:=v or value;
  end;
end;

procedure TContextElement_register.setValue(context: pointer; value:string);
var v: QWORD;
begin
  if entrytype=0 then
    v:=symhandler.getAddressFromName(value)
  else
    v:=StrToInt64(value);

  setValue(context,v);
end;



function TContextElement_register.getFlag(context: pointer): dword;
var
  p: pointer;
  v: dword;
begin
  if entrytype=0 then //used the wrong type...
  begin
    if size=4 then exit(getDword(context))
    else
    if size=8 then exit(getQword(context))
    else exit(0);
  end
  else
  begin
    v:=pdword(getPointer(context))^;
    exit((v shr bitstart) and ((1 shl size)-1)); //as long as the flags register is never stored at the last byte of context that spans exactly a page, this is fine. But do watch it
  end;
end;

procedure TContextInfo.setGeneralPurposeRegisters(list: PContextElementRegisterList);
var i: integer;
begin
  general:=list;
  for i:=0 to length(list^)-1 do
  begin
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
    fGeneralPurposeRegisterMaxCharCount:=max(fGeneralPurposeRegisterMaxCharCount, length(list^[i].name));
  end;
end;

procedure TContextInfo.setAltnameRegisters(list: PContextElementRegisterList);
var i: integer;
begin
  //this list does not get enumerated, so no need to keep a charcount
  altgeneral:=list;
  for i:=0 to length(list^)-1 do
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
end;

procedure TContextInfo.setGeneralPurposeFlags(list: PContextElementRegisterList);
var i: integer;
begin
  flags:=list;
  for i:=0 to length(list^)-1 do
  begin
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
    fGeneralPurposeFlagMaxCharCount:=max(fGeneralPurposeFlagMaxCharCount, length(list^[i].name));
  end;
end;

procedure TContextInfo.setFloatingPointRegisters(list: PContextElementRegisterList);
var i: integer;
begin
  mainfpu:=list;
  for i:=0 to length(list^)-1 do
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
end;

procedure TContextInfo.setSpecializedRegisters(list: PContextElementRegisterList);
var i: integer;
begin
  specialized:=list;
  for i:=0 to length(list^)-1 do
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
end;

procedure TContextInfo.setSecondaryFloatingPointRegisters(list: PContextElementRegisterList);
var i: integer;
begin
  secondaryfpu:=list;
  for i:=0 to length(list^)-1 do
    nameToEntryLookup.Add(list^[i].name, @list^[i]);
end;

function TContextInfo.getGeneralPurposeRegisters: PContextElementRegisterList;
begin
  result:=general;
end;

function TContextInfo.getGeneralPurposeFlags: PContextElementRegisterList;
begin
  result:=flags;
end;

function TContextInfo.getFloatingPointRegisters: PContextElementRegisterList;
begin
  result:=mainfpu;
end;

function TContextInfo.getSpecializedRegisters: PContextElementRegisterList;
begin
  result:=specialized;
end;

function TContextInfo.getAlternateFloatingPointRegisters: PContextElementRegisterList;
begin
  result:=secondaryfpu;
end;

function TContextInfo.getFloatingPointRegisterSetName: string;
begin
  result:=mainfpuname;
end;

function TContextInfo.getAlternateFloatingPointRegisterSetName: string;
begin
  result:=secondaryfpuname;
end;

function TContextInfo.getRegister(regname: string): PContextElement_register;
begin
  result:=nameToEntryLookup.Data[regname];
end;



function TContextInfo.getCopy(context: pointer): pointer;
{
copies the given context in case the pointed at is volatile (debuggerthread and threads can get destroyed)
Caller must free it (not aligned, only for reference purposes)
}
begin
  getmem(result,contextsize+64);
  copymemory(result, context, contextsize);
end;

function TContextInfo.isDifferent(context1, context2: pointer): boolean;
begin
  result:=not CompareMem(context1,context2,contextsize);
end;

procedure TContextInfo.setcontextflags(context: pointer; contextflags: qword);   //if the context structure has context flags, this will set them, else ignored (get/set all)
begin
  if fContextFlagsField<>nil then
    fContextFlagsField^.setValue(context, contextflags);

end;


constructor TContextInfo.create;
begin
  nameToEntryLookup:=TStringHashList.Create(false);
end;

destructor TContextInfo.destroy;
begin
  if nameToEntryLookup<>nil then
    freeandnil(nameToEntryLookup);
end;

function getBestContextHandler:TContextInfo;
begin
  result:=nil;
  if processhandler.SystemArchitecture=archX86 then
  begin
    {$ifdef cpu64}
    if processhandler.is64Bit then
      result:=ContextInfo_X86_64
    else
    {$endif}
      result:=ContextInfo_X86_32;
  end
  else
  begin
    if processhandler.is64Bit then
      result:=ContextInfo_ARM_64
    else
      result:=ContextInfo_ARM_32;
  end;
end;

procedure InitContextInfos;
var i: integer;
    e: TContextElement_register;
begin
  {$ifdef cpu32}
  ContextInfo_X86_32:=TContextInfo.Create;
  ContextInfo_X86_32.Name:='ContextInfo_X86_64';
  ContextInfo_X86_32.fcontextsize:=sizeof(CONTEXT32);
  ContextInfo_X86_32.setGeneralPurposeRegisters(@X86_32Context);
  ContextInfo_X86_32.setGeneralPurposeFlags(@X86_32Context_flags);
  ContextInfo_X86_32.setSpecializedRegisters(@X86_32Context_specialized);
  ContextInfo_X86_32.setAltnameRegisters(@X86_32Context_altnames);


  setlength(X86_32Context_fpu, 8);

  e.entrytype:=0;
  e.size:=16;
  e.displayType:=0;


  for i:=0 to 7 do
  begin
    e.name:='XMM'+inttostr(i);
    e.ContextOffset:=integer(@PCONTEXT32(nil)^.Ext.XmmRegisters[i]);
  end;

  ContextInfo_X86_32.setFloatingPointRegisters(@X86_32Context_fpu);

  e.entrytype:=0;
  e.size:=10;
  e.displayType:=0;
  setlength(X86_32Context_fpu2,8);

  for i:=0 to 7 do
  begin
    e.name:='FP('+inttostr(i)+')';
    e.ContextOffset:=integer(@PCONTEXT32(nil)^.FloatSave.RegisterArea[i*10]);
    X86_32Context_fpu2[i]:=e;
  end;
  ContextInfo_X86_32.setSecondaryFloatingPointRegisters(@X86_32Context_fpu2);
  ContextInfo_X86_32.secondaryfpuname:='FPU';


  ContextInfo_X86_32.fInstructionPointerRegister:=ContextInfo_X86_32.getRegister('EIP');
  ContextInfo_X86_32.fStackPointerRegister:=ContextInfo_X86_32.getRegister('ESP');
  ContextInfo_X86_32.fContextFlagsField:=@X86_32Context_controlreg;

  {$endif}

  {$ifdef cpu64}
  //windows32 on windows64
  ContextInfo_X86_32:=TContextInfo.Create;
  ContextInfo_X86_32.Name:='ContextInfo_X86_32';
  ContextInfo_X86_32.fContextSize:=sizeof(CONTEXT);
  ContextInfo_X86_32.setGeneralPurposeRegisters(@X86_32Context);
  ContextInfo_X86_32.setGeneralPurposeFlags(@X86_32Context_flags);
  ContextInfo_X86_32.setSpecializedRegisters(@X86_32Context_specialized);
  ContextInfo_X86_32.setAltnameRegisters(@X86_32Context_altnames);


  setlength(X86_32Context_fpu, 8);

  e.entrytype:=0;
  e.size:=16;
  e.displayType:=0;

  for i:=0 to 7 do
  begin
    e.name:='XMM'+inttostr(i);
    e.ContextOffset:=integer(@PCONTEXT(nil)^.FltSave.XmmRegisters[i]);
    X86_32Context_fpu[i]:=e;
  end;
  ContextInfo_X86_32.setFloatingPointRegisters(@X86_32Context_fpu);
  ContextInfo_X86_32.mainfpuname:='XMM';



  setlength(X86_32Context_fpu2, 8);

  e.entrytype:=0;
  e.size:=10;
  e.displayType:=0;

  for i:=0 to 7 do
  begin
    e.name:='FP('+inttostr(i)+')';
    e.ContextOffset:=integer(@PCONTEXT(nil)^.FltSave.FloatRegisters[i]);
    X86_32Context_fpu2[i]:=e;
    ContextInfo_X86_32.nameToEntryLookup.Add(e.name,@X86_32Context_fpu2[i]);
  end;
  ContextInfo_X86_32.setSecondaryFloatingPointRegisters(@X86_32Context_fpu2);
  ContextInfo_X86_32.secondaryfpuname:='FPU';
  ContextInfo_X86_32.fInstructionPointerRegister:=ContextInfo_X86_32.getRegister('EIP');
  ContextInfo_X86_32.fStackPointerRegister:=ContextInfo_X86_32.getRegister('ESP');
  ContextInfo_X86_32.fContextFlagsField:=@X86_32Context_controlreg;

  //----normal----
  ContextInfo_X86_64:=TContextInfo.Create;
  ContextInfo_X86_64.Name:='ContextInfo_X86_64';
  ContextInfo_X86_64.fContextSize:=sizeof(CONTEXT);
  ContextInfo_X86_64.setGeneralPurposeRegisters(@X86_64Context);
  ContextInfo_X86_64.setGeneralPurposeFlags(@X86_64Context_flags);
  ContextInfo_X86_64.setSpecializedRegisters(@X86_64Context_specialized);
  ContextInfo_X86_64.setAltnameRegisters(@X86_64Context_altnames);

  setlength(X86_64Context_fpu, 16);

  e.entrytype:=0;
  e.size:=16;
  e.displayType:=0;


  for i:=0 to 15 do
  begin
    e.name:='XMM'+inttostr(i);
    e.ContextOffset:=integer(@PCONTEXT(nil)^.FltSave.XmmRegisters[i]);
    X86_64Context_fpu[i]:=e;

    ContextInfo_X86_64.nameToEntryLookup.Add('YMM'+inttostr(i),@X86_64Context_fpu[i]); //I currently don't support YMM, but lookups can still use the basic part
  end;

  ContextInfo_X86_64.setFloatingPointRegisters(@X86_64Context_fpu);

  ContextInfo_X86_64.setSecondaryFloatingPointRegisters(@X86_32Context_fpu2);  //not a bug, X86_32Context_fpu2 is t he same for ContextInfo_X86_64
  ContextInfo_X86_64.secondaryfpuname:='FPU';
  ContextInfo_X86_64.fInstructionPointerRegister:=ContextInfo_X86_64.getRegister('RIP');
  ContextInfo_X86_64.fStackPointerRegister:=ContextInfo_X86_64.getRegister('RSP');
  ContextInfo_X86_64.fContextFlagsField:=@X86_64Context_controlreg;
  {$endif}


  //ARM32
  ContextInfo_ARM_32:=TContextInfo.create;
  ContextInfo_ARM_32.Name:='ContextInfo_ARM_32';
  ContextInfo_ARM_32.fContextSize:=sizeof(TARMCONTEXT);
  ContextInfo_ARM_32.setGeneralPurposeRegisters(@ARM_32Context);
  ContextInfo_ARM_32.setGeneralPurposeFlags(@ARM_32Context_flags);
  setlength(ARM_32Context_fpu, 32);

  e.entrytype:=0;
  e.size:=8;
  e.displayType:=0;
  for i:=0 to 31 do
  begin
    e.name:='VFP-D32'+inttostr(i);
    e.ContextOffset:=integer(@PARMCONTEXT(nil)^.fpu[i]);
    ARM_32Context_fpu[i]:=e;
  end;
  ContextInfo_ARM_32.setFloatingPointRegisters(@ARM_32Context_fpu);
  ContextInfo_ARM_32.fInstructionPointerRegister:=ContextInfo_ARM_32.getRegister('PC');
  ContextInfo_ARM_32.fStackPointerRegister:=ContextInfo_ARM_32.getRegister('SP');

  //ARM64
  ContextInfo_ARM_64:=TContextInfo.create;
  ContextInfo_ARM_64.Name:='ContextInfo_ARM_64';
  ContextInfo_ARM_64.fcontextsize:=sizeof(TARM64CONTEXT);

  setlength(ARM_64Context,33);
  ZeroMemory(@e, sizeof(e));
  e.entrytype:=0;
  e.size:=8;
  e.displayType:=0;
  for i:=0 to 30 do
  begin
    e.name:='X'+inttostr(i);
    e.ContextOffset:=integer(@PARM64CONTEXT(nil)^.regs.X[i]);
    ARM_64Context[i]:=e;
  end;

  e.name:='SP';
  e.ContextOffset:=integer(@PARM64CONTEXT(nil)^.SP);
  ARM_64Context[31]:=e;

  e.name:='PC';
  e.ContextOffset:=integer(@PARM64CONTEXT(nil)^.PC);
  ARM_64Context[32]:=e;

  ContextInfo_ARM_64.setGeneralPurposeRegisters(@ARM_64Context);
  ContextInfo_ARM_64.setGeneralPurposeFlags(@ARM_64Context_flags);

  setlength(ARM_64Context_fpu,32);
  e.size:=16;
  for i:=0 to 30 do
  begin
    e.name:='V'+inttostr(i);
    e.ContextOffset:=integer(@PARM64CONTEXT(nil)^.fp.vregs[i]);
    ARM_64Context_fpu[i]:=e;
  end;
  ContextInfo_ARM_64.setFloatingPointRegisters(@ARM_64Context_fpu);
  ContextInfo_ARM_64.fInstructionPointerRegister:=ContextInfo_ARM_64.getRegister('PC');
  ContextInfo_ARM_64.fStackPointerRegister:=ContextInfo_ARM_64.getRegister('SP');
  {$ifdef darwin}
  ContextInfo_ARM_64.fContextFlagsField:=@ARM_64Context_controlreg;
  {$endif}
end;


initialization
  InitContextInfos;

end.

