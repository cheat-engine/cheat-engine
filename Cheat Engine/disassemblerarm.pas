unit DisassemblerArm;

{$mode objfpc}{$H+}

interface

{$ifdef JNI}
uses unixporthelper, classes, sysutils, NewKernelHandler, LastDisassembleData, DisassemblerThumb;
{$endif}

{$ifdef windows}
uses
  windows, Classes, SysUtils{$ifndef ARMDEV}, newkernelhandler, cefuncproc{$endif},
  LastDisassembleData, DisassemblerThumb;
{$endif}

const ArmConditions: array [0..15] of string=('EQ','NE','CS', 'CC', 'MI', 'PL', 'VS', 'VC', 'HI', 'LS', 'GE', 'LT', 'GT', 'LE', '','NV');
const DataProcessingOpcodes: array [0..15] of string=('AND','EOR','SUB', 'RSB', 'ADD', 'ADC', 'SBC', 'RSC', 'TST', 'TEQ', 'CMP', 'CMN', 'ORR', 'MOV', 'BIC','MVN');
const ArmRegisters : array [0..15] of string=('R0','R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','FP','IP','SP','LR','PC');
const ArmRegistersNoName : array [0..15] of string=('R0','R1','R2','R3','R4','R5','R6','R7','R8','R9','R10','R11','R12','R13','R14','R15');
function SignExtend(value: int32; mostSignificantBit: integer): int32;

type
  TArmDisassembler=object

  private
    opcode: uint32;
    function Condition: string;
    procedure Branch;
    procedure BX;
    procedure DataProcessing;
    procedure MRS;
    procedure MSR;
    procedure MSR_flg;
    procedure Mul;
    procedure SingleDataTransfer;
    procedure LDM_STM;
    procedure SWP;
    procedure SWI;
    procedure CDP;


  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var address: ptrUint): string;
  end;




implementation

 {$ifndef ARMDEV}
uses processhandlerunit;
{$endif}


function SignExtend(value: int32; mostSignificantBit: integer): int32;
{
Signextends a given offset. mostSignificant bit defines what bit determines if it should be sign extended or not
}
begin
  if (value shr mostSignificantBit)=1 then //needs to be sign extended
  begin
    //set bits 31 to mostSignificantBit to 1
    result:=value or ($fffffff shl mostSignificantBit);
  end
  else
    result:=value;
end;

function TArmDisassembler.Condition: string;
begin
  result:=ArmConditions[opcode shr 28];
end;

procedure TArmDisassembler.Branch;
var offset: int32;
    _condition: string;
begin
  _condition:=condition;
  if (opcode shr 24) and 1=1 then //Link bit set
  begin
    LastDisassembleData.opcode:='BL'+Condition;
    LastDisassembleData.iscall:=true;
  end
  else
  begin
    LastDisassembleData.opcode:='B'+Condition;
    LastDisassembleData.isjump:=true;
    LastDisassembleData.isconditionaljump:=_condition<>'';
  end;

  offset:=signextend(opcode and $FFFFFF, 23) shl 2;

  LastDisassembleData.parameters:=inttohex(dword(LastDisassembleData.address+8+offset),8);
end;

procedure TArmDisassembler.BX;
var
  Rn: integer;
  _Rn: string;
  _condition: string;
begin
  _condition:=condition;
  LastDisassembleData.opcode:='BX'+Condition;
  LastDisassembleData.isjump:=true;
  LastDisassembleData.isconditionaljump:=_condition<>'';

  Rn:=opcode and $F;
  _Rn:=ArmRegisters[Rn];

  LastDisassembleData.parameters:=_Rn;
end;

procedure TArmDisassembler.DataProcessing;
var
  ImmediateOperand: integer;
  OpcodeIndex: integer;
  SetCondition: integer;
  Operand1: integer;
  Destination: integer;
  Operand2: integer;

  shift: integer;
  rm: integer;

  Rotate: integer;
  Imm: integer;

  shiftformat: integer;
  shiftType: integer;
  shiftammount: integer;
  shiftRegister: integer;

  shiftname: string='';
  _shift: string;


  _opcode: string;
  _cond: string;
  _S: string;
  _Rd: string;
  _Rn: string;
  _rm: string;
  _rs: string;
  _op2: string;
begin
  _opcode:='';
  _cond:='';
  _S:='';
  _Rd:='';
  _Rn:='';
  _op2:='';

  ImmediateOperand:=(opcode shr 25) and 1;
  OpcodeIndex:=(opcode shr 21) and $f;
  SetCondition:=(opcode shr 20) and 1;
  Operand1:=(opcode shr 16) and $f;
  Destination:=(opcode shr 12) and $f;
  Operand2:=opcode and $fff;

  _opcode:=DataProcessingOpcodes[OpcodeIndex];
  _cond:=Condition;
  if SetCondition=1 then
    _s:='S';


  _Rn:=ArmRegisters[Operand1];
  _Rd:=ArmRegisters[Destination];


  //build _op2
  if ImmediateOperand=0 then //operand2 is a register
  begin
    shift:=Operand2 shr 4;
    rm:=operand2 and $f;

    _rm:=ArmRegisters[rm];

    shiftformat:=shift and 1;
    shiftType:=(shift shr 1) and 3;

    case shifttype of
      0: shiftname:='LSL';
      1: shiftname:='LSR';
      2: shiftname:='ASR';
      3: shiftname:='ROR';
    end;

    _shift:=shiftname;



    if shiftformat=0 then
    begin
      shiftAmmount:=(shift shr (7-4)) and $1f;    //7-4 because the doc describes the shift field starting from bit 4
      if shiftAmmount>0 then
        _shift:=_shift+' '+inttohex(shiftammount,1)
      else
        _shift:='';

    end
    else
    begin
      shiftregister:=(shift shr (8-4)) and $f;
      _rs:=ArmRegisters[shiftregister];
      _shift:=_shift+' '+_rs;
    end;

    if _shift<>'' then
      _op2:=_rm+','+_shift
    else
      _op2:=_rm;

  end
  else
  begin
    //operand2 is an immediate value
    rotate:=Operand2 shr 8;
    imm:=Operand2 and $ff;


    _op2:=inttohex(RorDWord(imm, rotate*2),1);
  end;

  if _op2<>'' then
    _op2:=','+_op2;

  case OpcodeIndex of
    13,15:
    begin
      LastDisassembleData.opcode:=_opcode+_cond+_S;
      LastDisassembleData.parameters:=_Rd+_op2;
    end;


    8,9,10,11:
    begin
      LastDisassembleData.opcode:=_opcode+_cond;
      LastDisassembleData.parameters:=_Rn+_op2;
    end
    else
    begin
      LastDisassembleData.opcode:=_opcode+_cond+_S;
      LastDisassembleData.parameters:=_Rd+','+_Rn+_op2;
    end;

  end;
end;

procedure TArmDisassembler.MRS;
var
  _cond: string;
  _rd: string;
  _psr: string;

  Ps: integer;
  Rd: integer;
begin
  _cond:=Condition;

  Ps:=(opcode shr 22) and 1;
  if (Ps=0) then
    _psr:='CPSR'
  else
    _psr:='SPSR';

  Rd:=(opcode shr 12) and $F;
  _rd:=ArmRegisters[rd];

  LastDisassembleData.opcode:='MRS'+_cond;
  LastDisassembleData.parameters:=_Rd+','+_psr;
end;

procedure TArmDisassembler.MSR;
var
  _cond: string;
  _psr: string;
  _Rm: string;

  Pd: integer;
  Rm: integer;

  I: integer;
  imm, rotate: integer;
begin
  _cond:=Condition;

  Pd:=(opcode shr 22) and 1;
  if (Pd=0) then
    _psr:='CPSR'
  else
    _psr:='SPSR';

  Rm:=opcode and $F;
  _rm:=ArmRegisters[rm];

  LastDisassembleData.opcode:='MSR'+_cond;
  LastDisassembleData.parameters:=_psr+'_all,'+_Rm;
end;

procedure TArmDisassembler.MSR_flg;
var
  _cond: string;
  _psr: string;
  _Rm: string;

  Pd: integer;
  Rm: integer;

  I: integer;
  imm, rotate: integer;
begin
  _cond:=Condition;

  Pd:=(opcode shr 22) and 1;
  if (Pd=0) then
    _psr:='CPSR'
  else
    _psr:='SPSR';

  i:=(opcode shr 25) and 1;
  if i=0 then
  begin
    Rm:=opcode and $F;
    _rm:=ArmRegisters[rm];

    LastDisassembleData.opcode:='MSR'+_cond;
    LastDisassembleData.parameters:=_psr+'_flg,'+_Rm;
  end
  else
  begin
    rotate:=(opcode and $fff) shr 8;
    imm:=(opcode and $fff) and $ff;

    LastDisassembleData.opcode:='MSR'+_cond;
    LastDisassembleData.parameters:=_psr+'_flg,'+inttohex(RorDWord(imm, rotate*2),1);
  end;
end;

procedure TArmDisassembler.Mul;
var
  _cond: string;
  _S: string;
  _rd: string;
  _rm: string;
  _rs: string;
  _rn: string;
begin
  if (opcode shl 20) and 1=1 then
    _S:='S'
  else
    _S:='';

  _cond:=Condition;

  _rd:=ArmRegisters[opcode shl 16 and $f];
  _rn:=ArmRegisters[opcode shl 12 and $f];
  _rs:=ArmRegisters[opcode shl 8 and $f];
  _rm:=ArmRegisters[opcode and $f];


  if (opcode shl 21) and 1=1 then
  begin
    LastDisassembleData.opcode:='MLA'+_cond+_S;
    LastDisassembleData.parameters:=_Rd+','+_Rm+','+_Rs+','+_Rn
  end
  else
  begin
    LastDisassembleData.opcode:='MUL'+_cond+_S;
    LastDisassembleData.parameters:=_Rd+','+_Rm+','+_Rs;
  end;
end;

procedure TArmDisassembler.SingleDataTransfer;
var
  _cond: string;
  _opcode: string;
  _B: string;
  _T: string;
  _Rd: string;
  _Rn: string;
  _Rm: string;
  _shift: string;
  _U: string;

  I: integer;
  P: integer;
  U: integer;
  B: integer;
  W: integer;
  L: integer;
  Rn: integer;
  Rd: integer;
  Rm: integer;
  Offset: integer;


  shift: integer;
  shifttype: integer;

  _shiftname: string='';
  _shiftAmount: string;
  shiftamount: integer;


  _address: string;
begin
  I:=(opcode shr 25) and 1;
  P:=(opcode shr 24) and 1;
  U:=(opcode shr 23) and 1;
  B:=(opcode shr 22) and 1;
  W:=(opcode shr 21) and 1;
  L:=(opcode shr 20) and 1;
  Rn:=(opcode shr 16) and $f;
  Rd:=(opcode shr 12) and $f;
  offset:=opcode and $fff;

  if L=1 then
    _opcode:='LDR'
  else
    _opcode:='STR';

  if u=0 then
    _U:='-'
  else
    _U:='';

  _cond:=Condition;

  if b=1 then
    _B:='B'
  else
    _B:='';

  if (W=1) and (p=0) then //w is set and p=0 (post indexed)
    _T:='T'
  else
    _T:='';

  _Rd:=ArmRegisters[Rd];

  _Rn:=ArmRegisters[Rn];

  if i=0 then
  begin
    //offset is an immediate value
    if Rn=15 then //pc
    begin
      if u=0 then //decrease
        _address:='['+inttohex(dword(LastDisassembleData.address-offset+8),8)+']'
      else
        _address:='['+inttohex(dword(LastDisassembleData.address+offset+8),8)+']'
    end
    else
    begin
      if offset=0 then
        _address:='['+_Rn+']'
      else
        _address:='['+_Rn+','+_U+inttohex(dword(offset),8)+']';
    end;

  end
  else
  begin
    //offset is a register
    Rm:=offset and $f;
    Shift:=offset shr 4;

    _Rm:=ArmRegisters[Rm];

    shiftType:=(shift shr 1) and 3;

    case shifttype of
      0: _shiftname:='LSL';
      1: _shiftname:='LSR';
      2: _shiftname:='ASR';
      3: _shiftname:='ROR';
    end;


    shiftAmount:=(shift shr (7-4)) and $1f;
    if shiftAmount>0 then
    begin
      _shiftAmount:=inttohex(shiftamount,1);

      _shift:=', '+_U+_Rm+' '+_shiftname+' '+_shiftamount;
    end
    else
    begin
      _shiftAmount:='';
      _shift:=', '+_Rm;
    end;

    if p=0 then //post index
      _address:='['+_Rn+']'+_shift
    else //preindexed
      _address:='['+_Rn+_shift+']';
  end;

  if (w=1) and (p=1) then
    _address:=_address+'!';


  LastDisassembleData.opcode:=_opcode+_cond+_B+_T;
  LastDisassembleData.parameters:=_Rd+','+_Address;
end;

procedure TArmDisassembler.LDM_STM;
var
  P: integer;
  U: integer;
  S: integer;
  W: integer;
  L: integer;
  Rn: integer;
  RegisterList: integer;

  _opcode: string;
  _cond: string;
  _addressingmode: string;
  _rn: string;
  _ex: string;
  _exp: string;
  _rlist: string;


  i: integer;
  rcount: integer;



begin

  _cond:=Condition;
  p:=(opcode shr 24) and 1;
  u:=(opcode shr 23) and 1;
  s:=(opcode shr 22) and 1;
  w:=(opcode shr 21) and 1;
  l:=(opcode shr 20) and 1;
  Rn:=(opcode shr 16) and $f;
  RegisterList:=opcode and $ffff;


  _rlist:='{';
  rcount:=0;

  for i:=0 to 15 do
  begin
    if (RegisterList shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';


  if L=0 then
    _opcode:='STM'
  else
    _opcode:='LDM';

  _rn:=ArmRegisters[Rn];

  if w=1 then
    _ex:='!'
  else
    _ex:='';

  if s=1 then
    _exp:='^'
  else
    _exp:='';


  _addressingmode:='';
  if (L=1) and (P=1) and (U=1) then
  begin
    if rn=13 then
      _addressingmode:='ED'
    else
      _addressingmode:='IB';
  end;

  if (L=1) and (P=0) and (U=1) then
  begin
    if rn=13 then
      _addressingmode:='FD'
    else
      _addressingmode:='IA';
  end;

  if (L=1) and (P=1) and (U=0) then
  begin
    if rn=13 then
      _addressingmode:='EA'
    else
      _addressingmode:='DB';
  end;

  if (L=1) and (P=0) and (U=0) then
  begin
    if rn=13 then
      _addressingmode:='FA'
    else
      _addressingmode:='DA';
  end;

  if (L=0) and (P=1) and (U=1) then
  begin
    if rn=13 then
      _addressingmode:='FA'
    else
      _addressingmode:='IB';
  end;

  if (L=0) and (P=0) and (U=1) then
  begin
    if rn=13 then
      _addressingmode:='EA'
    else
      _addressingmode:='IA';
  end;

  if (L=0) and (P=1) and (U=0) then
  begin
    if rn=13 then
      _addressingmode:='FD'
    else
      _addressingmode:='DB';
  end;

  if (L=0) and (P=0) and (U=0) then
  begin
    if rn=13 then
      _addressingmode:='ED'
    else
      _addressingmode:='DA';
  end;

  LastDisassembleData.opcode:=_opcode+_cond+_addressingmode;
  LastDisassembleData.parameters:=_rn+_ex+','+_rlist+_exp;
end;

procedure TArmDisassembler.SWP;
var
  _cond: string;
  _B: string;
  _rd: string;
  _rm: string;
  _rn: string;
begin
  _cond:=Condition;
  if (opcode shr 22) and 1=1 then
    _B:='1'
  else
    _B:='';

  _rd:=ArmRegisters[(opcode shr 12) and $f];
  _rm:=ArmRegisters[opcode and $f];
  _rn:=ArmRegisters[(opcode shr 16) and $f];

  LastDisassembleData.opcode:='SWP'+_cond+_B;
  LastDisassembleData.parameters:=_rd+','+_rm+',['+_rn+']';
end;

procedure TArmDisassembler.SWI;
begin
  LastDisassembleData.opcode:='SWI'+Condition;
  LastDisassembleData.parameters:=inttohex(opcode and $FFFFFF,1);
end;

procedure FloatingPointDataInstruction(var LastDisassembleData: TLastDisassembleData);
var
  opcode: dword;
  cond: byte;
  t: byte;

  opc1, opc2, opc3, opc4: byte;
  condition: string;

  D, Vn, Vd, sz, N, M, Vm: byte;
  cc: byte;
begin
  opcode:=pdword(LastDisassembleData.Bytes)^;

  if lastdisassembledata.Disassembler=dcArm then
  begin
    cond:=(opcode shr 28) and $f;
    if (cond=$f) then
      t:=1
    else
      t:=0;

    condition:=ArmConditions[cond];
  end
  else
  begin
    opcode:=(opcode shr 16) or (opcode shl 16);
    t:=(opcode shr 28) and 1;

    pdword(@lastdisassembledata.Bytes[0])^:=opcode;
    condition:='';
  end;



  opc1:=(opcode shr 20) and $f;
  opc2:=(opcode shr 16) and $f;
  opc3:=(opcode shr 6) and 3;
  opc4:=opcode and $f;

  sz:=(opcode shr 8) and 1;

  D:=(opcode shr 22) and 1;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  N:=(opcode shr 7) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;


  if sz=0 then
  begin
    Vn:=(Vn shl 1) or N;
    Vm:=(Vm shl 1) or M;
    Vd:=(Vd shl 1) or D;
    LastDisassembleData.parameters:='S'+inttostr(vd)+', S'+inttostr(vn)+', S'+inttostr(Vm);
  end
  else
  begin
    Vn:=(N shl 4) or Vn;
    Vm:=(M shl 4) or Vm;
    Vd:=(D shl 4) or Vd;
    LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
  end;

  case (t shl 4) or opc1 of
    0,4: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VMLS' else LastDisassembleData.opcode:='VMLA';
    1,5: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VNMLA' else LastDisassembleData.opcode:='VNMLS';
    2,6: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VNMUL' else LastDisassembleData.opcode:='VMUL';
    3,7: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VADD' else LastDisassembleData.opcode:='VSUB';
    8,12: if (opc3 and 1)=0 then LastDisassembleData.opcode:='VDIV';
    9,13: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VFNMA' else LastDisassembleData.opcode:='VFNMS';
    10,14: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VFMS' else LastDisassembleData.opcode:='VFMA';

    16..23:
    begin
      case (opcode shr 20) and 3 of
        0: LastDisassembleData.opcode:='VSELEQ';
        1: LastDisassembleData.opcode:='VSELVS';
        2: LastDisassembleData.opcode:='VSELGE';
        3: LastDisassembleData.opcode:='VSELGT';
      end;
    end;

    24,28: if (opc3 and 1)=1 then LastDisassembleData.opcode:='VMINNM' else LastDisassembleData.opcode:='VMAXNM';

    11,15, 27, 31:
    begin
      //other:
      exit;

    end;
  end;

  if sz=0 then
    LastDisassembleData.opcode:=LastDisassembleData.opcode+condition+'.F32'
  else
    LastDisassembleData.opcode:=LastDisassembleData.opcode+condition+'.F64';

  if lastdisassembledata.Disassembler=dcThumb then  //undo the wordswap
  begin
    opcode:=(opcode shr 16) or (opcode shl 16);
    pdword(@lastdisassembledata.Bytes[0])^:=opcode;
  end;
end;

procedure TArmDisassembler.CDP;
var
  CP_Opc: integer;
  CRn: integer;
  CRd: integer;
  CPn: integer;
  CP: integer;
  CRm: integer;

  _pn: string;

  _cd,_cn,_cm: string;

  _expression1,_expression2: string;
begin

  CP_Opc:=(opcode shr 20) and $f;       //
  CRd:=(opcode shr 12) and $f;    //
  CRn:=(opcode shr 16) and $f;      //
  CRm:=opcode and $f;               //
  CPn:=(opcode shr 8) and $f;
  CP:=(opcode shr 5) and $7;  //

  if (CPn in [10,11]) and ((opcode and $10)=$10) then
  begin
    FloatingPointDataInstruction(LastDisassembleData);
    exit;
  end;


  _expression1:=inttohex(CP_OPC,1);
  if cp<>0 then
    _expression2:=inttohex(CP,1)
  else
    _expression2:='';

  _pn:=inttohex(cp,1);

  _cd:='c'+inttostr(CRd);
  _cn:='c'+inttostr(CRn);
  _cm:='c'+inttostr(CRm);




  //CDP{cond} p#,<expression1>,cd,cn,cm{,<expression2>}

  LastDisassembleData.opcode:='CDP'+Condition;
  LastDisassembleData.parameters:=_pn+','+_expression1+','+_cd+','+_cn+','+_cm+_expression2;
end;

procedure ASIMD_3RegSL(var LastDisassembleData: TLastDisassembleData);
var
  opcode: dword;
  u,c,a,b: byte;
  d: byte;
  size: byte;
  Vn, Vd, op, N, Q, M, Vm: byte;
  _datatype: string;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  if LastDisassembleData.Disassembler=dcThumb then
    u:=(opcode shr 28) and 1
  else
    u:=(opcode shr 24) and 1;

  a:=(opcode shr 8) and $f;
  b:=(opcode shr 4) and 1;
  c:=(opcode shr 20) and 3;

  D:=(opcode shr 22) and 1;
  size:=(opcode shr 20) and 3;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  op:=(opcode shr 9) and 1;
  N:=(opcode shr 7) and 1;
  Q:=(opcode shr 6) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  if u=0 then
    _datatype:='S'
  else
    _datatype:='U';

  case size of
    0: _datatype:=_datatype+'8';
    1: _datatype:=_datatype+'16';
    2: _datatype:=_datatype+'32';
    3: _datatype:=_datatype+'64'; //?
  end;


  if q=1 then
    LastDisassembleData.parameters:='.'+_datatype+' '+'D'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm)
  else
    LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', Q'+inttostr(Vm);


  case a of
    0: if b=0 then LastDisassembleData.opcode:='VHADD' else LastDisassembleData.opcode:='VQADD';
    1:
    begin
      if b=0 then LastDisassembleData.opcode:='VRHADD' else
      begin
        case (u shl 2) or c of
          0: LastDisassembleData.opcode:='VAND';
          1: LastDisassembleData.opcode:='VBIC';
          2: LastDisassembleData.opcode:='VORR';
          3: LastDisassembleData.opcode:='VORN';
          4: LastDisassembleData.opcode:='VEOR';
          5: LastDisassembleData.opcode:='VBSL';
          6: LastDisassembleData.opcode:='VBIT';
          7: LastDisassembleData.opcode:='VBIF';
        end;
      end;
    end;

    2: if b=0 then LastDisassembleData.opcode:='VHSUB' else LastDisassembleData.opcode:='VQSUB';
    3: if b=0 then LastDisassembleData.opcode:='VCGT' else LastDisassembleData.opcode:='VCGE';
    4: if b=0 then LastDisassembleData.opcode:='VSHL' else LastDisassembleData.opcode:='VQSHL';
    5: if b=0 then LastDisassembleData.opcode:='VRSHL' else LastDisassembleData.opcode:='VQRSHL';
    6: if b=0 then LastDisassembleData.opcode:='VMAX' else LastDisassembleData.opcode:='VMIN';
    7: if b=0 then LastDisassembleData.opcode:='VABD' else LastDisassembleData.opcode:='VABA';
    8: if b=0 then
       begin
         if u=0 then LastDisassembleData.opcode:='VADD' else LastDisassembleData.opcode:='VSUB';
       end
       else
       begin
         if u=0 then LastDisassembleData.opcode:='VTST' else LastDisassembleData.opcode:='VCEQ';
       end;

    9: if b=0 then
       begin
         if u=0 then LastDisassembleData.opcode:='VMLA' else LastDisassembleData.opcode:='VMLS';
       end
       else LastDisassembleData.opcode:='VMUL';

    10: if b=0 then LastDisassembleData.opcode:='VPMAX' else LastDisassembleData.opcode:='VPMIN';
    11: if b=0 then
        begin
          if u=0 then LastDisassembleData.opcode:='VQDMULH' else LastDisassembleData.opcode:='VQRDMULH';
        end
        else LastDisassembleData.opcode:='VPADD';

    12: case (b shl 3) or (u shl 2) or c of
          0: LastDisassembleData.opcode:='SHA1C';
          1: LastDisassembleData.opcode:='SHA1P';
          2: LastDisassembleData.opcode:='SHA1M';
          3: LastDisassembleData.opcode:='SHA1SU0';
          4: LastDisassembleData.opcode:='SHA256H';
          5: LastDisassembleData.opcode:='SHA256H2';
          6: LastDisassembleData.opcode:='SHA256SU1';
          8: LastDisassembleData.opcode:='VFMA';
          10:LastDisassembleData.opcode:='VFMS';
        end;

    13: case (b shl 3) or (u shl 2) or c of
          0,1: LastDisassembleData.opcode:='VADD';
          2,3: LastDisassembleData.opcode:='VSUB';
          4,5: LastDisassembleData.opcode:='VPADD';
          6,7: LastDisassembleData.opcode:='VABD';
          8:   LastDisassembleData.opcode:='VMLA';
          10:  LastDisassembleData.opcode:='VMLS';
          12,13: LastDisassembleData.opcode:='VMUL';
        end;

    14: case (b shl 3) or (u shl 2) or c of
         0,1: LastDisassembleData.opcode:='VCEQ';
         4,5: LastDisassembleData.opcode:='VCGE';
         6,7: LastDisassembleData.opcode:='VCGT';
         12:  LastDisassembleData.opcode:='VACGE';
         14:  LastDisassembleData.opcode:='VACGT';
        end;

    15: case (b shl 3) or (u shl 2) or c of
         0: LastDisassembleData.opcode:='VMAX';
         2: LastDisassembleData.opcode:='VMIN';
         4: LastDisassembleData.opcode:='VPMAX';
         6: LastDisassembleData.opcode:='VPMIN';
         8,9: LastDisassembleData.opcode:='VRECPS';
         10,11: LastDisassembleData.opcode:='VRSQRTS';
         12:  LastDisassembleData.opcode:='VMAXNM';
         14:  LastDisassembleData.opcode:='VMINNM';
        end;


  end;

end;

procedure ASIMD_1Reg_ModImm(var LastDisassembleData: TLastDisassembleData);
var opcode: dword;
  a,b,c,d,cmode,op,e,f,g,h: byte;
  b2: byte;

  q: byte;

  Vd: byte;


  abcdefgh: byte;
  constant: qword;
  _dt: string='';

  a32: dword;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  if LastDisassembleData.Disassembler=dcThumb then
    a:=(opcode shr 28) and 1
  else
    a:=(opcode shr 24) and 1;

  b:=(opcode shr 18) and 1;
  c:=(opcode shr 17) and 1;
  d:=(opcode shr 16) and 1;

  cmode:=(opcode shr 8) and $f;
  Q:=(opcode shr 6) and 1;
  op:=(opcode shr 5) and 1;

  e:=(opcode shr 3) and 1;
  f:=(opcode shr 2) and 1;
  g:=(opcode shr 1) and 1;
  h:=opcode and 1;

  Vd:=((opcode shr (22-4)) and $1) or (opcode shr 12) and $f;


  abcdefgh:=(a shl 7) or (b shl 6) or (c shl 5) or (d shl 4) or (e shl 3) or (f shl 2) or (g shl 1) or h;

  constant:=0;
  case cmode of
    0:
    begin
      constant:=(qword(abcdefgh) shl 32) or abcdefgh;
      _dt:='I32';
    end;

    2:
    begin
      constant:=(qword(abcdefgh) shl 40) or (qword(abcdefgh) shl 8);
      _dt:='I32';
    end;

    4:
    begin
      constant:=(qword(abcdefgh) shl 48) or (qword(abcdefgh) shl 16);
      _dt:='I32';
    end;


    6:
    begin
      constant:=(qword(abcdefgh) shl 56) or (qword(abcdefgh) shl 24);
      _dt:='I32';
    end;

    8:
    begin
      constant:=(qword(abcdefgh) shl 48) or (qword(abcdefgh) shl 32) or (qword(abcdefgh) shl 16) or abcdefgh;
      _dt:='I16';
    end;

    10:
    begin
      constant:=(qword(abcdefgh) shl 56) or (qword(abcdefgh) shl 40) or (qword(abcdefgh) shl 24) or (abcdefgh shl 8);
      _dt:='I16';
    end;

    12:
    begin
      constant:=(qword(abcdefgh) shl 40) or (qword($ff) shl 32) or (qword(abcdefgh) shl 8) or ($ff);
      _dt:='I32';
    end;

    13:
    begin
      constant:=(qword(abcdefgh) shl 48) or (qword($ff) shl 40) or (qword($ff) shl 32) or (qword(abcdefgh) shl 16) or (qword($ff) shl 8) or ($ff);
      _dt:='I32';
    end;

    14:
    begin
      if op=0 then
      begin
        constant:=(qword(abcdefgh) shl 56) or (qword(abcdefgh) shl 48) or (qword(abcdefgh) shl 40) or (qword(abcdefgh) shl 32) or (qword(abcdefgh) shl 24) or (abcdefgh shl 16) or (abcdefgh shl 8);
        _dt:='I8';
      end
      else
      begin
        a:=(a shl 7) or (a shl 6) or (a shl 5) or (a shl 4) or (a shl 3) or (a shl 2) or (a shl 1) or a;
        b:=(b shl 7) or (b shl 6) or (b shl 5) or (b shl 4) or (b shl 3) or (b shl 2) or (b shl 1) or b;
        c:=(c shl 7) or (c shl 6) or (c shl 5) or (c shl 4) or (c shl 3) or (c shl 2) or (c shl 1) or c;
        d:=(d shl 7) or (d shl 6) or (d shl 5) or (d shl 4) or (d shl 3) or (d shl 2) or (d shl 1) or d;
        e:=(e shl 7) or (e shl 6) or (e shl 5) or (e shl 4) or (e shl 3) or (e shl 2) or (e shl 1) or e;
        f:=(f shl 7) or (f shl 6) or (f shl 5) or (f shl 4) or (f shl 3) or (f shl 2) or (f shl 1) or f;
        g:=(g shl 7) or (g shl 6) or (g shl 5) or (g shl 4) or (g shl 3) or (g shl 2) or (g shl 1) or g;
        h:=(h shl 7) or (h shl 6) or (h shl 5) or (h shl 4) or (h shl 3) or (h shl 2) or (h shl 1) or h;

        constant:=(qword(a) shl 56) or (qword(b) shl 48) or (qword(c) shl 40) or (qword(d) shl 32) or (qword(e) shl 24) or (qword(f) shl 16) or (qword(g) shl 16) or qword(h);
        _dt:='I64';

      end;
    end;

    15:
    begin
      if op=0 then
      begin
        b2:=(not b) and 1;
        a32:=0;
        a32:=(a shl 31) or (b2 shl 30) or (b shl 29) or (b shl 28) or (b shl 27) or (b shl 26) or (b shl 25) or (c shl 24) or (d shl 23) or (e shl 22)  or (e shl 21) or (f shl 20) or (g shl 19) or (h shl 18);
        constant:=(qword(a32) shl 32) or a32;
      end
      else
      begin
        constant:=qword($BADBADBADBADBAD)
      end;
    end;
  end;

  case (op shl 4) or cmode of
    0,2,4,6: LastDisassembleData.opcode:='VMOV.'+_dt;
    1,3,5,7: LastDisassembleData.opcode:='VORR.'+_dt;
    8,10: LastDisassembleData.opcode:='VMOV.'+_dt;
    9,11: LastDisassembleData.opcode:='VORR.'+_dt;
    12,13,14,15: LastDisassembleData.opcode:='VMOV.'+_dt;

    16,18,20,22: LastDisassembleData.opcode:='VMVN.'+_dt;
    17,19,21,23: LastDisassembleData.opcode:='VBIC.'+_dt;
    24,26: LastDisassembleData.opcode:='VMVN.'+_dt;
    25,27: LastDisassembleData.opcode:='VBIC.'+_dt;
    28,29: LastDisassembleData.opcode:='VMVN.'+_dt;
    30: LastDisassembleData.opcode:='VMOV.'+_dt;
  end;


  if Q=0 then
    LastDisassembleData.parameters:='D'+inttostr(vd)+', '+inttohex(constant,1)
  else
    LastDisassembleData.parameters:='Q'+inttostr(vd)+', '+inttohex(constant,1);


end;

procedure ASIMD_2Reg_Shift(var LastDisassembleData: TLastDisassembleData);
var opcode: dword;
  u: byte;
  imm3, imm6: byte;
  M, D,A,L,B,Q: byte;
  Vd, Vm: byte;
  size: integer;
  _type, _size: string;
  limm3: byte;
  imm: byte;
  esize: byte;
  elements: byte;

  op: byte;

  _dt1: string='';
  _dt2: string='';
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;
  imm:=0;

  if LastDisassembleData.Disassembler=dcThumb then
    u:=(opcode shr 28) and 1
  else
    u:=(opcode shr 24) and 1;

  d:=(opcode shr 22) and 1;
  imm3:=(opcode shr 19) and $7;
  imm6:=(opcode shr 16) and $3f;
  Vd:=(opcode shr 12) and $f;
  a:=(opcode shr 8) and $f;
  op:=(opcode shr 8) and 1;
  l:=(opcode shr 7) and 1;
  b:=(opcode shr 6) and 1;
  q:=b;
  m:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vm:=(m shl 4) or vm;
  Vd:=(d shl 4) or vd;


  limm3:=(l shl 3) or imm3;

  if (limm3=0) then ASIMD_1Reg_ModImm(LastDisassembleData);

  _type:='';

  //size:
  size:=8;
  if (limm3 and 1)=1 then size:=8 else
  if (limm3 and 2)=2 then size:=16 else
  if (limm3 and 4)=4 then size:=32 else
  if (limm3 and 8)=8 then size:=64;

  esize:=size;

  _size:=inttostr(size);

  //imm:
  if a in [0,1,2,3,4] then
  begin
    if size=32 then size:=64;
    imm:=size-imm6;
  end;

  if a in [5,6,7] then
  begin
    case size of
      8: imm:=imm6-8;
      16: imm:=imm6-16;
      32: imm:=imm6-32;
      64: imm:=imm6;
    end;
  end;


  //_type:
  if u=0 then _type:='S' else _type:='U';


  if Q=0 then
    LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(vm)+', '+inttohex(imm,1)
  else
    LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(vm)+', '+inttohex(imm,1);


  //instruction:
  case A of
    0: LastDisassembleData.opcode:='VSHR'+_type+_size;
    1: LastDisassembleData.opcode:='VSRA'+_type+_size;
    2: LastDisassembleData.opcode:='VRSHR'+_type+_size;
    3: LastDisassembleData.opcode:='VRSRA'+_type+_size;
    4: if u=1 then LastDisassembleData.opcode:='VSRI'+_size;
    5: if u=0 then LastDisassembleData.opcode:='VSHL.I'+_size else LastDisassembleData.opcode:='VSLI'+_type+_size;
    6: LastDisassembleData.opcode:='VQSHLU'+_type+_size;
    7: LastDisassembleData.opcode:='VQSHL'+_type+_size;
    8:
    begin
      size:=size*2;
      _size:=inttostr(size);
      imm:=size-imm6;
      LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(vm)+', '+inttohex(imm,1);

      case (u shl 3) or (b shl 2) or l of
        0: LastDisassembleData.opcode:='VSHRN.I'+_size;
        2: LastDisassembleData.opcode:='VRSHRN.I'+_size;
        4: LastDisassembleData.opcode:='VQSHRUN.I'+_type+_size;
        6: LastDisassembleData.opcode:='VQRSHRUN.I'+_type+_size;
      end;
    end;

    9:
    begin
      size:=size*2;
      _size:=inttostr(size);
      imm:=size-imm6;
      LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(vm)+', '+inttohex(imm,1);

      case (b shl 2) or l of
        0: LastDisassembleData.opcode:='VSHRN.I'+_size;
        2: LastDisassembleData.opcode:='VRSHRN.I'+_size;
      end;
    end;

    10:
    begin
      if (b=0) and (l=0) then
      begin
        imm:=imm6-esize;
        if imm=0 then
        begin
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(vm);
          LastDisassembleData.opcode:='VMOVL';
        end
        else
        begin
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(vm)+', '+inttohex(imm,1);
          LastDisassembleData.opcode:='VSHLL'+_type+_size;
        end;
      end;
    end;

    14,15:
    begin
      case (op shl 1) or U of
        0:
        begin
          _dt1:='F32';
          _dt2:='S32';
        end;

        1:
        begin
          _dt1:='F32';
          _dt2:='U32';
        end;

        2:
        begin
          _dt1:='S32';
          _dt2:='F32';
        end;

        3:
        begin
          _dt1:='U32';
          _dt2:='F32';
        end;
      end;
      LastDisassembleData.opcode:='VCVT.'+_dt1+'.'+_dt2;

      if Q=0 then
        LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(vm)+', '+inttohex(64-imm6,1)
      else
        LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(vm)+', '+inttohex(64-imm6,1)


    end;

  end;



end;

procedure ASIMD_3Reg_DL(var LastDisassembleData: TLastDisassembleData);
var opcode: dword;
  u,b,a: byte;
  d, size, Vn, Vd, op, N,M, Vm: byte;
  _datatype: string;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  if LastDisassembleData.Disassembler=dcThumb then
    u:=(opcode shr 28) and 1
  else
    u:=(opcode shr 24) and 1;

  b:=(opcode shr 20) and 3;
  a:=(opcode shr 8) and $f;

  D:=(opcode shr 22) and 1;
  size:=(opcode shr 20) and 3;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  op:=(opcode shr 9) and 1;
  N:=(opcode shr 7) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  if u=0 then
    _datatype:='S'
  else
    _datatype:='U';

  case size of
    0: _datatype:=_datatype+'8';
    1: _datatype:=_datatype+'16';
    2: _datatype:=_datatype+'32';
    3: _datatype:=_datatype+'64'; //?
  end;

  LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);

  case a of
     0: LastDisassembleData.opcode:='VADDL';
     1:
     begin
       LastDisassembleData.opcode:='VADDW';
       LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', D'+inttostr(Vm);
     end;
     2: LastDisassembleData.opcode:='VSUBL';
     3:
     begin
       LastDisassembleData.opcode:='VSUBL';
       LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', D'+inttostr(Vm);
     end;

     4: if u=0 then LastDisassembleData.opcode:='VADDHN' else LastDisassembleData.opcode:='VRADDHN';
     5: LastDisassembleData.opcode:='VABAL';
     6: if u=0 then LastDisassembleData.opcode:='VSUBHN' else LastDisassembleData.opcode:='VRSUBHN';
     7: LastDisassembleData.opcode:='VABDL';
     8: LastDisassembleData.opcode:='VMLAL';
     9: if u=0 then LastDisassembleData.opcode:='VQDMLAL';
     10: LastDisassembleData.opcode:='VMLSL';
     11: LastDisassembleData.opcode:='VQDMLSL';
     12,14: LastDisassembleData.opcode:='VMULL';
     13: LastDisassembleData.opcode:='VQDMULL';
  end;

end;

procedure ASIMD_2Reg_Scalar(var LastDisassembleData: TLastDisassembleData);
var opcode: dword;
  q,u,b,a: byte;
  d, size, Vn, Vd, op, F,N,M, Vm: byte;
  _datatype: string='';
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  if LastDisassembleData.Disassembler=dcThumb then
    u:=(opcode shr 28) and 1
  else
    u:=(opcode shr 24) and 1;

  q:=u;
  b:=(opcode shr 20) and 3;
  a:=(opcode shr 8) and $f;

  D:=(opcode shr 22) and 1;
  size:=(opcode shr 20) and 3;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  op:=(opcode shr 9) and 1;
  F:=(opcode shr 8) and 1;
  N:=(opcode shr 7) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;



  case size of
    0: _datatype:='8';
    1: _datatype:='16';
    2: _datatype:='32';
    3: _datatype:='64'; //?
  end;

  case a of
    0,1:
    begin
      if F=0 then _datatype:='I'+_datatype else _datatype:='F'+_datatype;

      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VMLA';
    end;

    2:
    begin
      if U=0 then _datatype:='S'+_datatype else _datatype:='U'+_datatype;

      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VMLAL';
    end;

    3:
    begin
      _datatype:='S'+_datatype;
      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VQDMLAL';
    end;

    4,5:
    begin
      if F=0 then _datatype:='I'+_datatype else _datatype:='F'+_datatype;

      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VMLA';
    end;

    6:
    begin
      if U=0 then _datatype:='S'+_datatype else _datatype:='U'+_datatype;

      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VMLSL';
    end;

    7:
    begin
      _datatype:='S'+_datatype;
      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VQDMLSL';
    end;

    8,9:
    begin
      if F=0 then _datatype:='I'+_datatype else _datatype:='F'+_datatype;

      if q=1 then
        LastDisassembleData.parameters:='.'+_datatype+' '+'D'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm)
      else
        LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', D'+inttostr(Vm);

      LastDisassembleData.opcode:='VMUL';
    end;

    10:
    begin
      if U=0 then _datatype:='S'+_datatype else _datatype:='U'+_datatype;
      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);

      LastDisassembleData.opcode:='VMULL';
    end;

    11:
    begin
      _datatype:='S'+_datatype;
      LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VQDMULL';
    end;

    12:
    begin
      _datatype:='S'+_datatype;
      if q=1 then
        LastDisassembleData.parameters:='.'+_datatype+' '+'D'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm)
      else
        LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VQDMULH';
    end;

    13:
    begin
      _datatype:='S'+_datatype;
      if q=1 then
        LastDisassembleData.parameters:='.'+_datatype+' '+'D'+inttostr(vd)+', D'+inttostr(vn)+', D'+inttostr(Vm)
      else
        LastDisassembleData.parameters:='.'+_datatype+' '+'Q'+inttostr(vd)+', Q'+inttostr(vn)+', D'+inttostr(Vm);
      LastDisassembleData.opcode:='VQRDMULH';
    end;
  end;


end;

procedure VEXT(var LastDisassembleData: TLastDisassembleData);
var
  opcode: dword;
  D, Vn, Vd, imm4, N, Q, M, Vm: byte;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;
  D:=(opcode shr 22) and 1;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;

  imm4:=(opcode shr 8) and $f;

  N:=(opcode shr 7) and 1;
  Q:=(opcode shr 6) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  if q=0 then
    LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vn)+', D'+inttostr(Vm)+' '+inttohex(imm4,1)
  else
    LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vn)+', D'+inttostr(Vm)+' '+inttohex(imm4,1);

  LastDisassembleData.opcode:='VEXT.8';
end;

procedure ASIMD_2Reg_Misc(var LastDisassembleData: TLastDisassembleData);
var opcode: dword;
  u,b,a: byte;
  q,d, size, Vn, Vd, op, F,N,M, Vm: byte;
  sz: byte;
  _dt: string='';
  _dt2: string='';

  _sU, _sf: string;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  if LastDisassembleData.Disassembler=dcThumb then
    u:=(opcode shr 28) and 1
  else
    u:=(opcode shr 24) and 1;


  b:=(opcode shr 6) and $1f;
  a:=(opcode shr 16) and $3;

  D:=(opcode shr 22) and 1;
  size:=(opcode shr 18) and 3;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  op:=(opcode shr 7) and 1;
  F:=(opcode shr 10) and 1;
  N:=(opcode shr 7) and 1;
  Q:=(opcode shr 6) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  case size of
    0: _dt:='8';
    1: _dt:='16';
    2: _dt:='32';
    3: _dt:='64';
  end;

  case size of
    0: _dt2:='16';
    1: _dt2:='32';
    2: _dt2:='64';
  end;



  if op=0 then _SU:='S' else _sU:='U';
  if f=0 then _sf:='S' else _sf:='F';


  case a of
    0:
    begin
      case b of
        0..11,
        16..31: if q=0 then
                  LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)
                else
                  LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);

        12..15: LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);
      end;

      case b of
          0,1: LastDisassembleData.opcode:='VREV64.'+_dt;
          2,3: LastDisassembleData.opcode:='VREV32.'+_dt;
          4,5: LastDisassembleData.opcode:='VREV16.'+_dt;
          8..11: LastDisassembleData.opcode:='VPADDL.'+_sU+_dt;
          12: LastDisassembleData.opcode:='AESE.'+_dt;
          13: LastDisassembleData.opcode:='AESD.'+_dt;
          14: LastDisassembleData.opcode:='AESMC.'+_dt;
          15: LastDisassembleData.opcode:='AESIMC.'+_dt;

          16,17: LastDisassembleData.opcode:='VCLS.S'+_dt;
          18,19: LastDisassembleData.opcode:='VCLZ.I'+_dt;
          20,21: LastDisassembleData.opcode:='VCNT.8';
          22,23: LastDisassembleData.opcode:='VMVN';
          24,27: LastDisassembleData.opcode:='VPADAL.'+_sU+_dt;
          28,29: LastDisassembleData.opcode:='VQABS.S'+_dt;
          30,31: LastDisassembleData.opcode:='VQNEG.S'+_dt;
        end;

    end;

    1:
    begin
      case (b shr 1) and 7 of
        0..4:  if q=0 then
                  LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)+', 0'
                else
                  LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm)+', 0';

        5:     LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);

        6,7:   if q=0 then
                  LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)
               else
                  LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);



      end;

      case (b shr 1) and 7 of
        0: LastDisassembleData.opcode:='VCGT.'+_sf+_dt;
        1: LastDisassembleData.opcode:='VCGE.'+_sf+_dt;
        2: LastDisassembleData.opcode:='VCEQ.'+_sf+_dt;
        3: LastDisassembleData.opcode:='VCLE.'+_sf+_dt;
        4: LastDisassembleData.opcode:='VCLT.'+_sf+_dt;
        5: LastDisassembleData.opcode:='SHA1H.32';
        6: LastDisassembleData.opcode:='VABS'+_sf+_dt;
        7: LastDisassembleData.opcode:='VNEG.'+_sf+_dt;
      end;

    end;

    2:
    begin
      if q=0 then
        LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)
      else
        LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);

      case b of
        0,1: LastDisassembleData.opcode:='VSWP';
        2,3: LastDisassembleData.opcode:='VTRN.'+_dt;
        4,5: LastDisassembleData.opcode:='VUZP.'+_dt;
        6,7: LastDisassembleData.opcode:='VZIP.'+_dt;
        8:
        begin
          LastDisassembleData.opcode:='VMOVN.I'+_dt;
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        9:
        begin
          LastDisassembleData.opcode:='VQMOVUN.S'+_dt2;
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        10:
        begin
          LastDisassembleData.opcode:='VQMOVN.S'+_dt2;
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        11:
        begin
          if size=0 then _dt:='8';
          LastDisassembleData.opcode:='VQMOVN.U'+_dt2;
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        12:
        begin
          LastDisassembleData.opcode:='VSHLL';
          LastDisassembleData.parameters:='Q'+inttostr(vd)+', D'+inttostr(Vm)+', '+inttohex(size,1);
        end;

        14:
        begin
          LastDisassembleData.opcode:='SHA1SU1.32';
          LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        15:
        begin
          LastDisassembleData.opcode:='SHA1SU0.32';
          LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(vn)+', Q'+inttostr(Vm);
        end;

        16, 17: LastDisassembleData.opcode:='VRINTN.F32.F32';
        18, 19: LastDisassembleData.opcode:='VRINTX.F32.F32';
        20, 21: LastDisassembleData.opcode:='VRINTA.F32.F32';
        22, 23: LastDisassembleData.opcode:='VRINTZ.F32.F32';
        26, 27: LastDisassembleData.opcode:='VRINTM.F32.F32';
        30, 31: LastDisassembleData.opcode:='VRINTP.F32.F32';
        24:
        begin
          LastDisassembleData.opcode:='VCVT.F16.F32';
          LastDisassembleData.parameters:='D'+inttostr(vd)+', Q'+inttostr(Vm);
        end;

        28:
        begin
          LastDisassembleData.opcode:='VCVT.F32.F16';
          LastDisassembleData.parameters:='Q'+inttostr(vd)+', D'+inttostr(Vm);
        end;
      end;
    end;

    3:
    begin
      if q=0 then
        LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)
      else
        LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm);

      case b shr 2 of
        0: LastDisassembleData.opcode:='VCVTA.F32';
        1: LastDisassembleData.opcode:='VCVTN.F32';
        2: LastDisassembleData.opcode:='VCVTP.F32';
        3: LastDisassembleData.opcode:='VCVTN.F32';
        4,5:
        begin
          if ((b shr 1) and 1)=0 then  LastDisassembleData.opcode:='VRECPE' else LastDisassembleData.opcode:='VRSQRTE';

          if f=1 then
            LastDisassembleData.opcode:=LastDisassembleData.opcode+'F32'
          else
            LastDisassembleData.opcode:=LastDisassembleData.opcode+'S32';
        end;

        6,7:
        begin
          case (opcode shr 7) and 3 of
            0: LastDisassembleData.opcode:='VCVT.F32.S32';
            1: LastDisassembleData.opcode:='VCVT.F32.U32';
            2: LastDisassembleData.opcode:='VCVT.S32.F32';
            3: LastDisassembleData.opcode:='VCVT.U32.F32';
          end;
        end;
      end;
    end;
  end;




end;

procedure VTBL_VTBLX(var LastDisassembleData: TLastDisassembleData);
var
  opcode: dword;
  D, Vn, Vd, len, N, op, M, Vm: byte;
  _list: string;
  i: integer;
begin
  _list:='';
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  D:=(opcode shr 22) and 1;
  Vn:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;
  len:=(opcode shr 8) and 3;

  N:=(opcode shr 7) and 1;
  op:=(opcode shr 6) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vn:=(N shl 4) or Vn;
  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  for i:=0 to len-1 do
  begin
    _list:=_list+'D'+inttostr(Vn+i);
    if i<len-1 then
      _list:=_list+', ';
  end;

  LastDisassembleData.parameters:='D'+inttostr(vd)+', {'+_list+'} D'+inttostr(Vm);


  if op=0 then
    LastDisassembleData.opcode:='VTBL.8'
  else
    LastDisassembleData.opcode:='VTBX.8';
end;


procedure VDUP_Scalar(var LastDisassembleData: TLastDisassembleData);
var
  opcode: dword;
  D, imm4, Vd, Q, M, Vm: byte;

  size, index: byte;
begin
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;

  D:=(opcode shr 22) and 1;
  imm4:=(opcode shr 16) and $f;
  Vd:=(opcode shr 12) and $f;

  Q:=(opcode shr 6) and 1;
  M:=(opcode shr 5) and 1;
  Vm:=opcode and $f;

  Vm:=(M shl 4) or Vm;
  Vd:=(D shl 4) or Vd;

  size:=0;
  index:=0;
  if (imm4 and 1)=1 then
  begin
    size:=8;
    index:=imm4 shr 1;
  end
  else
  if (imm4 and 2)=2 then
  begin
    size:=16;
    index:=imm4 shr 2;
  end
  else
  if (imm4 and 4)=4 then
  begin
    size:=32;
    index:=imm4 shr 3;
  end;

  if q=0 then
    LastDisassembleData.parameters:='D'+inttostr(vd)+', D'+inttostr(Vm)+'['+inttostr(index)+']'
  else
    LastDisassembleData.parameters:='Q'+inttostr(vd)+', Q'+inttostr(Vm)+'['+inttostr(index)+']';

  LastDisassembleData.opcode:='VDUP.'+inttostr(size);
end;

procedure Advanced_SIMD_Data_Processing(var LastDisassembleData: TLastDisassembleData);
//called by both the arm and thumb disassembler
var opcode: dword; u,a,b,c:byte;
begin
  opcode:=pdword(LastDisassembleData.Bytes)^;

  if lastdisassembledata.Disassembler=dcArm then
    u:=(opcode shr 24) and 1
  else
  begin
    opcode:=(opcode shr 16) or (opcode shl 16);
    u:=(opcode shr 28) and 1;

    pdword(@lastdisassembledata.Bytes[0])^:=opcode;
  end;

  a:=(opcode shr 19) and $1f;
  b:=(opcode shr 8) and $f;
  c:=opcode and $f;

  if (a shr 4) and 1=0 then ASIMD_3RegSL(LastDisassembleData)
  else
  begin
    if ((c and 9)=1) then
    begin
      case a and $7 of
        0: ASIMD_1Reg_ModImm(LastDisassembleData);
        1..7: ASIMD_2Reg_Shift(LastDisassembleData);
      end;
    end
    else
    if ((c and 9)=9) then
    begin
      ASIMD_2Reg_Shift(LastDisassembleData);
    end
    else
    if ((a and $7) in [0..5]) and ((c and 5)=0) then
    begin
      ASIMD_3Reg_DL(LastDisassembleData);
    end
    else
    if ((a and $7) in [0..5]) and ((c and 5)=4) then
    begin
      ASIMD_2Reg_Scalar(LastDisassembleData);
    end
    else
    if (u=0) and ((a and $16)=$16) and ((c and 1)=0) then
    begin
      VEXT(LastDisassembleData);
    end
    else
    if (u=1) and ((a and $16)=$16) and ((c and 1)=0) then
    begin
      if (b and 8)=0 then
      begin
        ASIMD_2Reg_Misc(LastDisassembleData);
      end
      else
      if (b and $c)=8 then
      begin
        VTBL_VTBLX(LastDisassembleData);
      end
      else
      if (b=$c) then
      begin
        VDUP_Scalar(LastDisassembleData);
      end;

    end;
  end;


  if lastdisassembledata.Disassembler=dcThumb then  //undo the wordswap
  begin
    opcode:=(opcode shr 16) or (opcode shl 16);
    pdword(@lastdisassembledata.Bytes[0])^:=opcode;
  end;


end;

function TArmDisassembler.Disassemble(var address: ptrUint): string;
var
  i: integer;
  x: ptruint;
  thumbdisassembler: TThumbDisassembler;
begin
  result:='';

  if (address and 1) = 1 then //thumb
  begin
    result:=thumbdisassembler.Disassemble(address);
    LastDisassembleData:=thumbdisassembler.LastDisassembleData;
    exit;
  end;

  {$ifdef ARMDEV}
  opcode:=pdword(address)^;
  setlength(LastDisassembleData.Bytes,4);
  pdword(@LastDisassembleData.Bytes[0])^:=opcode;

  address:=0;
  x:=sizeof(opcode);
  {$else}
  x:=0;
  setlength(LastDisassembleData.Bytes,4);

  readprocessmemory(processhandle, pointer(address), @LastDisassembleData.Bytes[0], 4, x);
  opcode:=pdword(@LastDisassembleData.Bytes[0])^;
  {$endif}

  LastDisassembleData.address:=address;
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
  lastdisassembledata.Disassembler:=dcArm;



  if (x=sizeof(opcode)) then
  begin
    if ((opcode shr 4) and $ffffff)=$12FFF1 then
      BX
    else
    if (((opcode shr 2) and $3f)=0) and (((opcode shr 4) and  $f)=$9) then
      MUL
    else
    if (((opcode shr 23) and $1f)=2) and (((opcode shr 16) and  $3f)=$f) and ((opcode and $FFF)=0) then
      MRS
    else
    if (((opcode shr 23) and $1f)=2) and (((opcode shr 4) and $3FFFF)=$29F00) then
      MSR
    else
    if (((opcode shr 23) and $3)=2) and (((opcode shr 26) and $3)=2)  and (((opcode shr 12) and $3ff)=$28f) then
      MSR_flg
    else
    if ((opcode shr 25) and 7)=5 then
      Branch
    else
    if (((opcode shr 26) and 3)=0) and (((opcode shr 4) and $f)<>9) then
      DataProcessing
    else
    if (opcode shr 26) and 3=1 then
      SingleDataTransfer
    else
    if (opcode shr 25) and 7=4 then
      LDM_STM
    else
    if ((opcode shr 23) and $1f=2) and ((opcode shr 20) and $3=0) and ((opcode shr 4) and $ff=9) then
      SWP
    else
    if (opcode shr 24) and $F=$F then
      SWI
    else
    if (((opcode shr 24) and $F)=$E) and (((opcode shr 4) and 1)=1) then
      CDP
    else
    if (opcode shr 25) and $7f=$79 then
      Advanced_SIMD_Data_Processing(LastDisassembleData);

    //if (((opcode shr 24) and $f)=$E) and ((opcode shr 9) and $3=5) and ((opcode shr 4) and 1) = 1

  end
  else
    LastDisassembleData.opcode:='??';


  result:=inttohex(LastDisassembleData.address,8);
  result:=result+' - ';
  if x>0 then
  begin
    for i:=0 to length(LastDisassembleData.bytes)-1 do
      result:=result+inttohex(LastDisassembleData.Bytes[i],2)+' ';
  end
  else
  begin
    for i:=0 to length(LastDisassembleData.bytes)-1 do
      result:=result+'?? ';
  end;

  result:=result+' - ';
  result:=result+LastDisassembleData.opcode;
  result:=result+' ';
  result:=result+LastDisassembleData.parameters;

  inc(address,4);

end;

initialization
  outputdebugstring('arm disassembler');

end.

