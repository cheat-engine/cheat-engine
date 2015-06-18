unit DisassemblerArm;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils{$ifndef ARMDEV}, newkernelhandler, cefuncproc{$endif},
  LastDisassembleData, DisassemblerThumb;

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

uses processhandlerunit;


function SignExtend(value: int32; mostSignificantBit: integer): int32;
{
Signextends a given offset. mostSignificant bit defines what bit determines if it should be sign extended or not
}
begin
  if (value shr mostSignificantBit)=1 then //needs to be sign extended
  begin
    //set bits 31 to mostSignificantBit to 1
    result:=value or ($fffffff shl (mostSignificantBit+1));
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

  shiftname: string;
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

  _shiftname: string;
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

function TArmDisassembler.Disassemble(var address: ptrUint): string;
var
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

  setlength(LastDisassembleData.bytes,0);

  {$ifdef ARMDEV}
  opcode:=pdword(address)^;
  setlength(LastDisassembleData.Bytes,4);
  pdword(@LastDisassembleData.Bytes[0])^:=opcode;

  address:=0;
  x:=sizeof(opcode);
  {$else}
  x:=0;
  if readprocessmemory(processhandle, pointer(address), @opcode, sizeof(opcode), x) then
  begin
    setlength(LastDisassembleData.Bytes,4);
    pdword(@LastDisassembleData.Bytes[0])^:=opcode;
  end;
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
      CDP;

  end
  else
    LastDisassembleData.opcode:='??';


  result:=inttohex(LastDisassembleData.address,8);
  result:=result+' - ';
  if x=sizeof(opcode) then result:=result+inttohex(LastDisassembleData.Bytes[0],2)+' '+inttohex(LastDisassembleData.Bytes[1],2)+' '+inttohex(LastDisassembleData.Bytes[2],2)+' '+inttohex(LastDisassembleData.Bytes[3],2);
  result:=result+' - ';
  result:=result+LastDisassembleData.opcode;
  result:=result+' ';
  result:=result+LastDisassembleData.parameters;

  inc(address,4);

end;

end.

