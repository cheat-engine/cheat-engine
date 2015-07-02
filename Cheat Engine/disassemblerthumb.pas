unit DisassemblerThumb;

{$mode objfpc}{$H+}

{
This disassembler is build based on the information in chapter F of the ARM DDI 0487A.e ID121714 Documentation
}

interface

uses
  Classes, SysUtils, LastDisassembleData;

type
  TThumbDisassembler=object
  private
    a: ptruint;
    opcode: uint16;
    opcode2: uint16; //for 32-bit thumb2
    procedure t32;

    procedure ADC_R_T1;
    procedure AND_R_T1;
    procedure EOR_R_T1;
    procedure SBC_R_T1;
    procedure TST_R_T1;
    procedure RSB_I_T1;
    procedure CMP_R_T1;
    procedure CMP_R_T2;
    procedure CMN_R_T1;
    procedure ORR_R_T1;
    procedure MUL_R_T1;
    procedure BIC_R_T1;
    procedure MVN_R_T1;

    procedure MOV_R_T1;
    procedure MOV_R_T2;
    procedure MOV_RSR_T1;

    procedure ADD_R_T2;
    procedure ADD_SP_R_T1;
    procedure ADD_SP_R_T2;

    procedure ADD_R_T1;
    procedure SUB_R_T1;

    procedure ADD_I_T1;
    procedure ADD_I_T2;

    procedure SUB_I_T1;
    procedure SUB_I_T2;

    procedure MOV_I_T1;
    procedure CMP_I_T1;

    procedure B_T1;
    procedure B_T2;
    procedure BX_T1;
    procedure BLX_R_T1;

    procedure LDR_L_T1;

    procedure STR_R_T1;
    procedure STRH_R_T1;
    procedure STRB_R_T1;
    procedure LDRSB_R_T1;
    procedure LDR_R_T1;

    procedure LDRH_R_T1;
    procedure LDRB_R_T1;
    procedure LDRSH_R_T1;

    procedure STR_I_T1;
    procedure LDR_I_T1;
    procedure STR_I_T2;
    procedure LDR_I_T2;
    procedure STRB_I_T1;
    procedure LDRB_I_T1;
    procedure STRH_I_T1;
    procedure LDRH_I_T1;

    procedure ADR_T1;
    procedure ADD_SP_I_T1;
    procedure ADD_SP_I_T2;
    procedure SUB_SP_I_T1;
    procedure CBZ_CBNZ;
    procedure SXTH_T1;
    procedure SXTB_T1;
    procedure UXTH_T1;
    procedure UXTB_T1;
    procedure PUSH_T1;
    procedure POP_T1;

    procedure SETEND_T1;
    procedure CPS_T1;
    procedure REV_T1;
    procedure REV16_T1;
    procedure REVSH_T1;
    procedure HLT_T1;
    procedure BKPT_T1;
    procedure UDF_T1;
    procedure SVC_T1;
    procedure IT_T1;

    procedure STM_T1;
    procedure LDM_T1;



  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var address: ptrUint): string;
  end;

implementation

uses
  DisassemblerArm
{$ifndef armdev}
,ProcessHandlerUnit, NewKernelHandler
{$endif}
  ;


procedure TThumbDisassembler.t32;
var x: ptruint;
begin
  //read opcode2
  {$ifdef armdev}
  opcode:=pword(a)^;
  setlength(LastDisassembleData.Bytes,4);
  pword(@LastDisassembleData.Bytes[2])^:=opcode;
  {$else}
  if readprocessmemory(processhandle, pointer(ptruint(a)-1+2), @opcode2, sizeof(opcode2), x) then
  begin
    setlength(LastDisassembleData.Bytes,4);
    puint16(@LastDisassembleData.Bytes[2])^:=opcode2;
  end;
  {$endif}

  //todo: implement me
end;


procedure TThumbDisassembler.STM_T1;
var
  i: integer;
  rn: byte;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  rn:=(opcode shr 8) and 7;

  _rlist:='{';
  rcount:=0;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='STM';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+_rlist;
end;

procedure TThumbDisassembler.LDM_T1;
var
  i: integer;
  rn: byte;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  rn:=(opcode shr 8) and 7;

  _rlist:='{';
  rcount:=0;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='LDM';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+_rlist;
end;

procedure TThumbDisassembler.REVSH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REVSH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;


procedure TThumbDisassembler.IT_T1;
var
  firstcond: byte;
  mask: byte;
  c: string;
  i: integer;

  m: string;
begin
  firstcond:=(opcode shr 4) and $f;
  mask:=opcode and $f;
  c:=ArmConditions[firstcond];

  case mask of
    1: m:='TTT';
    2: m:='TT';
    3: m:='TTE';
    4: m:='T';
    5: m:='TET';
    6: m:='TE';
    7: m:='TEE';
    8: m:='';
    9: m:='ETT';
    10: m:='ET';
    11: m:='ETE';
    12: m:='E';
    13: m:='EET';
    14: m:='EE';
    15: m:='EEE';
  end;

  LastDisassembleData.opcode:='IT'+m;
  LastDisassembleData.parameters:=c;
end;

procedure TThumbDisassembler.UDF_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='UDF';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.SVC_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='SVC';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.BKPT_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='BKPT';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.HLT_T1;
var imm6: byte;
begin
  imm6:=opcode and $3f;
  LastDisassembleData.opcode:='HLT';
  LastDisassembleData.parameters:=inttohex(imm6,1);
end;

procedure TThumbDisassembler.REV_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REV';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.REV16_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REV16';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.CPS_T1;
var _F,_I,_A, IM: byte;
  p: string;
begin
  _f:=opcode and 1;
  _i:=(opcode shr 1) and 1;
  _a:=(opcode shr 2) and 1;
  im:=(opcode shr 4) and 1;

  if im=1 then
    LastDisassembleData.opcode:='CPSID'
  else
    LastDisassembleData.opcode:='CPSIE';

  p:='';
  if _a<>0 then
    p:='a';

  if _i<>0 then
    p:=p+'i';

  if _f<>0 then
    p:=p+'f';
end;



procedure TThumbDisassembler.SETEND_T1;
begin
  LastDisassembleData.opcode:='SETEND';

  if (opcode shr 3) and 1 =0 then
    LastDisassembleData.parameters:='LE'
  else
    LastDisassembleData.parameters:='BE';
end;

procedure TThumbDisassembler.PUSH_T1;
var
  i: integer;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  m:=(opcode shr 8) and 1;


  if m=1 then
  begin
    _rlist:='{'+ArmRegisters[14];
    rcount:=1;
  end
  else
  begin
    _rlist:='{';
    rcount:=0;
  end;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='PUSH';
  LastDisassembleData.parameters:=_rlist;
end;

procedure TThumbDisassembler.POP_T1;
var
  i: integer;
  reglist: word;
  rcount: integer;
  p: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  p:=(opcode shr 8) and 1;

  if p=1 then
  begin
    _rlist:='{'+ArmRegisters[15];
    rcount:=1;
  end
  else
  begin
    _rlist:='{';
    rcount:=0;
  end;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='PUSH';
  LastDisassembleData.parameters:=_rlist;
end;

procedure TThumbDisassembler.SXTH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='SXTH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.SXTB_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='SXTB';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.UXTH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='UXTH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.UXTB_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='UXTB';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;


procedure TThumbDisassembler.CBZ_CBNZ;
var rs, imm5, i, op: byte;
  imm32: dword;
begin
  rs:=opcode and $7;
  imm5:=(opcode shr 3) and $1f;
  i:=(opcode shr 9) and 1;
  op:=(opcode shr 11) and 1;
  if (op=0) then
    LastDisassembleData.opcode:='CBZ'
  else
    LastDisassembleData.opcode:='CBNZ';

  imm32:=((i shl 6) or imm5) * 2;

  LastDisassembleData.parameters:=ArmRegisters[rs]+','+inttohex(a+2+imm32,1);
end;

procedure TThumbDisassembler.ADD_SP_I_T1;
var rd, imm8: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:=ArmRegisters[rd]+',SP,'+inttohex(imm8*4,1);
end;

procedure TThumbDisassembler.ADD_SP_I_T2;
var imm7: byte;
begin
  imm7:=opcode and $7f;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+inttohex(imm7*4,1);
end;

procedure TThumbDisassembler.SUB_SP_I_T1;
var imm7: byte;
begin
  imm7:=opcode and $7f;

  LastDisassembleData.opcode:='SUB';
  LastDisassembleData.parameters:='SP,'+inttohex(imm7*4,1);
end;

procedure TThumbDisassembler.ADR_T1;
var rd, imm8: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='ADR';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+inttohex(Align(a+2,4)+imm8*4,1);
end;

procedure TThumbDisassembler.STR_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDR_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;


procedure TThumbDisassembler.STR_I_T2;
var rt, imm8: byte;
  _imm: string;
begin
  rt:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  if imm8<>0 then
    _imm:=','+inttohex(imm8*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',[SP'+_imm+']';
end;

procedure TThumbDisassembler.LDR_I_T2;
var rt, imm8: byte;
  _imm: string;
begin
  rt:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  if imm8<>0 then
    _imm:=','+inttohex(imm8*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',[SP'+_imm+']';
end;


procedure TThumbDisassembler.STRB_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDRB_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.STRH_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDRH_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.STR_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;


procedure TThumbDisassembler.STRH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.STRB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRSB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRSB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDR_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRSH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRSH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;




procedure TThumbDisassembler.LDR_L_T1;
var rt, imm8: byte;
begin
  rt:=(opcode shr 8) and $7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+inttohex(align(a+2,4)+imm8*4,1)+']';
end;

procedure TThumbDisassembler.B_T1;
var cond, imm8: byte;
begin
  cond:=(opcode shr 8) and $7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='B'+ArmConditions[cond];
  LastDisassembleData.parameters:=inttohex(a+2+imm8*2,1);
end;

procedure TThumbDisassembler.B_T2;
var imm11: byte;
begin
  imm11:=opcode and $7ff;
  LastDisassembleData.opcode:='B';
  LastDisassembleData.parameters:=inttohex(a+2+imm11*2,1);
end;

procedure TThumbDisassembler.BX_T1;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='BX';
  LastDisassembleData.parameters:=ArmRegisters[rm];
end;

procedure TThumbDisassembler.BLX_R_T1;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='BLX';
  LastDisassembleData.parameters:=ArmRegisters[rm];
end;

procedure TThumbDisassembler.ADD_I_T1;
var
  imm3: byte;
  rn: byte;
  rd: byte;
begin
  imm3:=(opcode shr 6) and 7;
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm3,1);
end;

procedure TThumbDisassembler.ADD_I_T2;
var
  imm8: byte;
  rdn: byte;
begin
  imm8:=opcode and $ff;
  rdn:=(opcode shr 8) and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+IntToHex(imm8,1);
end;




procedure TThumbDisassembler.SUB_I_T1;
var
  imm3: byte;
  rn: byte;
  rd: byte;
begin
  imm3:=(opcode shr 6) and 7;
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm3,1);
end;

procedure TThumbDisassembler.SUB_I_T2;
var
  imm8: byte;
  rdn: byte;
begin
  imm8:=opcode and $ff;
  rdn:=(opcode shr 8) and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+IntToHex(imm8,1);
end;

procedure TThumbDisassembler.MOV_I_T1;
var imm8, rd: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='MOVS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm8,1);
end;

procedure TThumbDisassembler.CMP_I_T1;
var imm8, rn: byte;
begin
  rn:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+IntToHex(imm8,1);
end;


procedure TThumbDisassembler.ADD_SP_R_T1;
var
  dm: byte;
  rdm: byte;
begin
  dm:=(opcode shr 7) and 1;
  rdm:=opcode and 7;
  rdm:=(dm shl 4) or rdm;


  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+ArmRegisters[rdm];
end;

procedure TThumbDisassembler.ADD_SP_R_T2;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ADD_R_T1;
var rd, rn, rm: byte;
begin
  rd:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.ADD_R_T2;
var rdn, rm,dn: byte;
begin
  rdn:=opcode and 7;
  rm:=(opcode shr 3) and $f;
  dn:=(opcode shr 7) and 1;

  rdn:=(dn shl 4) or rdn;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.SUB_R_T1;
var rd, rn, rm: byte;
begin
  rd:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ADC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ADCS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.AND_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ANDS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.EOR_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='EORS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.SBC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='SBCS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.TST_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='TST';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.RSB_I_T1;
var
  rd,rn: byte;
begin
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;
  LastDisassembleData.opcode:='RSBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+',0';
end;

procedure TThumbDisassembler.CMP_R_T1;
var
  rn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rn:=opcode and 7;
  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.CMP_R_T2;
var
  rn,rm,N: byte;
begin
  rm:=(opcode shr 3) and $f;
  rn:=opcode and 7;
  n:=(opcode shr 7) and 1;

  rn:=(n shl 4) or rn;

  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.CMN_R_T1;
var
  rn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rn:=opcode and 7;
  LastDisassembleData.opcode:='CMN';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ORR_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ORRS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.MUL_R_T1;
var
  rdm,rn: byte;
begin
  rn:=(opcode shr 3) and 7;
  rdm:=opcode and 7;
  LastDisassembleData.opcode:='MULS';
  LastDisassembleData.parameters:=ArmRegisters[rdm]+','+ArmRegisters[rn];
end;

procedure TThumbDisassembler.BIC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='BICS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MVN_R_T1;
var
  rd,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rd:=opcode and 7;
  LastDisassembleData.opcode:='MVN';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MOV_R_T1;
var
  rd,rm,D: byte;
begin
  rd:=(opcode and 7);
  rm:=(opcode shr 3) and $f;
  d:=(opcode shr 7) and 1;

  rd:=(d shl 4) or rd;

  LastDisassembleData.opcode:='MOV';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MOV_RSR_T1;
var
  op: byte;
  rs,rdm: byte;
  shiftname: string;
begin
  op:=(opcode shr 6) and 3;
  rs:=(opcode shr 3) and 7;
  rdm:=opcode and 7;

  case op of
    2: shiftname:='LSL';
    3: shiftname:='LSR';
    4: shiftname:='ASR';
    7: shiftname:='ROR';
  end;


  LastDisassembleData.opcode:='MOVS';
  LastDisassembleData.parameters:=ArmRegisters[rdm]+','+ArmRegisters[rdm]+','+shiftname+' '+ArmRegisters[rs];
end;

procedure TThumbDisassembler.MOV_R_T2;
var
  op: byte;
  rn,rd: byte;
  imm5: byte;
  shiftname: string;
begin
  if ((opcode shr 13)=0) and (((opcode shr 11) and 3)<>3) then
  begin
    op:=(opcode shr 11) and 3;
    imm5:=(opcode shr 6) and $1f;
    rn:=(opcode shr 3) and 7;
    rd:=opcode and 7;

    if imm5>0 then
    begin
      case op of
        0: shiftname:='LSL';
        1: shiftname:='LSR';
        2: shiftname:='ASR';
        3: shiftname:='ROR';
      end;
      shiftname:=','+shiftname+' '+inttohex(imm5,1);
    end
    else
      shiftname:='';

    LastDisassembleData.opcode:='MOVS';
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+shiftname;
  end;
end;




function TThumbDisassembler.disassemble(var address: ptruint): string;
var x: ptruint;
  i: integer;
begin

  a:=address;
  {$ifdef armdev}
  opcode:=pword(address)^;
  setlength(LastDisassembleData.Bytes,2);
  pword(@LastDisassembleData.Bytes[0])^:=opcode;
  {$else}
  x:=0;
  if readprocessmemory(processhandle, pointer(address-1), @opcode, sizeof(opcode), x) then
  begin
    setlength(LastDisassembleData.Bytes,2);
    puint16(@LastDisassembleData.Bytes[0])^:=opcode;
  end;
  {$endif}

  LastDisassembleData.address:=a;

  case (opcode shr 10) of
    $00..$0f:
    begin
      //Shift, Add, Subtract, Move and Compare
      case (opcode shr 9) and $1f of
        $0..$b: MOV_R_T2;     //000 0x xx -  000 10 xx
        $c: ADD_R_T1;
        $d: SUB_R_T1;
        $e: ADD_I_T1;
        $f: SUB_I_T1;
        $10..$13: MOV_I_T1;
        $14..$17: CMP_I_T1;
        $18..$1b: ADD_I_T2;
        $1c..$1f: SUB_I_T2;
      end;
    end;


    $10:
    begin
      //data Processing 010000
      case (opcode shr 6) and $f of //4 bits (bit 6-9)
        0: AND_R_T1; //0100000000******
        1: EOR_R_T1;
        2,3,4,7: MOV_RSR_T1; //010000001x - 0100000100 - 0100000111
        5: ADC_R_T1;
        6: SBC_R_T1;
        8: TST_R_T1;
        9: RSB_I_T1;
        10: CMP_R_T1;
        11: CMN_R_T1;
        12: ORR_R_T1;
        13: MUL_R_T1;
        14: BIC_R_T1;
        15: MVN_R_T1;
      end;
    end;

    $11:
    begin
      //special Data Instructions And Branch And Exchange;
      case (opcode shr 6) and $f of
        0..3:
        begin
          case (opcode shr 3) and $f of
            $D: ADD_SP_R_T1;
            0,1,2,3,4,6,7:
            begin
              if ((opcode shr 7) and 1)=0 then
                ADD_R_T2
              else
              begin
                if (opcode and 7)=5 then
                  ADD_SP_R_T2
                else
                  ADD_R_T2;
              end;
            end;

            8,9,$a,$b,$c,$e,$f:
            begin
              if ((opcode shr 7) and 1)=0 then
                ADD_R_T2
              else
              begin
                if (opcode and 7)=5 then
                  ADD_SP_R_T2
                else
                  ADD_R_T2;
              end;

            end;
          end;
        end;

        4..7: CMP_R_T2;
        8..11: MOV_R_T1;
        12..13: BX_T1;
        14..15: BLX_R_T1;
      end;

    end;

    $12..$13: LDR_L_T1;

    $14..$27:
    begin
      //Load Store Single Data Item;
      case (opcode shr 9) and $7f of
        $28: STR_R_T1;
        $29: STRH_R_T1;
        $2a: STRB_R_T1;
        $2b: LDRSB_R_T1;
        $2c: LDR_R_T1;
        $2d: LDRH_R_T1;
        $2e: LDRB_R_T1;
        $2f: LDRSH_R_T1;
        $30..$33: STR_I_T1;
        $34..$37: LDR_I_T1;
        $38..$3b: STRB_I_T1;
        $3c..$3f: LDRB_I_T1;
        $40..$43: STRH_I_T1;
        $44..$47: LDRH_I_T1;
        $48..$4b: STR_I_T2;
        $4c..$4f: LDR_I_T2;
      end;
    end;
    $28..$29: ADR_T1;
    $2a..$2b: ADD_SP_I_T1;
    $2c..$2f:
    begin
      //misc16
      case (opcode shr 5) and $7f of
        $0..$3: ADD_SP_I_T2;
        $4..$7: SUB_SP_I_T1;
        $8..$f: CBZ_CBNZ;
        $10..$11: SXTH_T1;
        $12..$13: SXTB_T1;
        $14..$15: UXTH_T1;
        $16..$17: UXTB_T1;
        $18..$1f: CBZ_CBNZ;
        $20..$2f: PUSH_T1;
        $32: SETEND_T1;
        $33: CPS_T1;
        $48..$4f: CBZ_CBNZ;
        $50..$51: REV_T1;
        $52..$53: REV16_T1;

        $54..$55: HLT_T1;
        $56..$57: REVSH_T1;
        $58..$5f: CBZ_CBNZ;
        $60..$6f: POP_T1;
        $70..$77: BKPT_T1;
        $78..$7f:
        begin
          if (opcode and $f)<>0 then IT_T1
          else
          case ((opcode shr 4) and $7) of
            0: LastDisassembleData.opcode:='NOP';
            1: LastDisassembleData.opcode:='YIELD';
            2: LastDisassembleData.opcode:='WFE';
            3: LastDisassembleData.opcode:='WFI';
            4: LastDisassembleData.opcode:='SEV';
            5: LastDisassembleData.opcode:='SEVL';
          end;

        end;
      end;

    end;
    $30..$31: STM_T1;
    $32..$33: LDM_T1;
    $34..$37:
    begin
      case (opcode shr 8) and $7 of
        $e: UDF_T1;
        $f: SVC_T1;
        else
          B_T1;
      end;
    end;
    $38..$39: B_T2;
    $3a..$3f: T32;
  end;

  result:=inttohex(LastDisassembleData.address,8);
  result:=result+' - ';
  for i:=0 to length(LastDisassembleData.bytes)-1 do
    result:=result+inttohex(LastDisassembleData.Bytes[i],2)+' ';
  result:=result+' - ';
  result:=result+LastDisassembleData.opcode;
  result:=result+' ';
  result:=result+LastDisassembleData.parameters;

  inc(address, length(LastDisassembleData.Bytes));
end;

end.

