unit assemblerArm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, strutils;

function BranchParser(address: int32; instruction:string):int32;
function CMPParser(address: int32; instruction:string):int32;
function CDPPArser(address: int32; instruction:string): int32;
function SingleDataParser(address: int32; instruction: string): int32;
function DataProcessingParser(address: int32; instruction:string):int32;
function MultiDataParser(address: int32; instruction:string):int32;
function MRSParser(address: int32; instruction:string): int32;
function MSRParser(address: int32; instruction:string): int32;
function MULParser(address: int32; instruction:string): int32;
function SWPParser(address: int32; instruction:string): int32;
function SWIParser(address: int32; instruction:string): int32;

function getParam(instruction: string; var parserpos: integer): string;
function getRegNumber(regstring: string): integer;

function Assemble(address: int32; instruction: string): Int32;

implementation

uses DisassemblerArm;



type Tparser=function(address: int32; instruction: string): int32;

type
  TOpcodeData=record
    opcode: string;
    parser: TParser;
  end;




const OpcodeCount=28;

const OpcodeList : array [0..OpcodeCount-1] of TOpcodeData= (
  (opcode: 'ADC'; parser:@DataProcessingParser),
  (opcode: 'ADD'; parser:@DataProcessingParser),
  (opcode: 'AND'; parser:@DataProcessingParser),
  (opcode: 'B'; parser:@BranchParser),
  (opcode: 'BIC'; parser:@DataProcessingParser),
  (opcode: 'BL'; parser:@BranchParser),

  (opcode: 'CDP'; parser:@CDPPArser),
  (opcode: 'CMP'; parser:@DataProcessingParser),
  (opcode: 'CMN'; parser:@DataProcessingParser),

  (opcode: 'EOR'; parser:@DataProcessingParser),

  (opcode: 'LDR'; parser:@SingleDataParser),
  (opcode: 'LDM'; parser:@MultiDataParser),
  (opcode: 'MOV'; parser:@DataProcessingParser),
  (opcode: 'MVN'; parser:@DataProcessingParser),
  (opcode: 'MRS'; parser:@MRSParser),
  (opcode: 'MSR'; parser:@MSRParser),
  (opcode: 'MLA'; parser:@MULParser),
  (opcode: 'MUL'; parser:@MULParser),
  (opcode: 'ORR'; parser:@DataProcessingParser),
  (opcode: 'RSB'; parser:@DataProcessingParser),
  (opcode: 'RSC'; parser:@DataProcessingParser),
  (opcode: 'SUB'; parser:@DataProcessingParser),
  (opcode: 'SBC'; parser:@DataProcessingParser),
  (opcode: 'STR'; parser:@SingleDataParser),
  (opcode: 'STM'; parser:@MultiDataParser),
  (opcode: 'SWP'; parser:@SWPParser),
  (opcode: 'SWI'; parser:@SWIParser),

  (opcode: 'TST'; parser:@DataProcessingParser)
  );

var lookupindex: array ['A'..'Z'] of integer;

procedure generateRotateAndImm(value: integer; var rotate: integer; var imm: integer);
var
  tempv: uint32;
  smallest: record
    rolcount: integer;
    value: uint32;
  end;
  i: integer;

begin
  if value<=255 then
  begin
    imm:=value;
    rotate:=0;
  end
  else
  begin
    //i'm sure there is some sort of calculation for this. For now just brute force it
    smallest.rolcount:=0;
    smallest.value:=value;

    for i:=0 to 15 do
    begin
      tempv:=RolDWord(value, i*2);
      if tempv<smallest.value then
      begin
        smallest.value:=tempv;
        smallest.rolcount:=i;
      end;
    end;

    if smallest.value>255 then raise exception.create('The value '+inttohex(value,1)+' can not be encoded using 8 bits and double rotate');

    rotate:=smallest.rolcount;
    imm:=smallest.value;
  end;
end;

function ShiftTypeToNumber(shifttype: string): integer;
begin
  if shifttype='LSL' then
    result:=0
  else
  if shifttype='LSR' then
    result:=1
  else
  if shifttype='ASR' then
    result:=2
  else
  if shifttype='ROR' then
    result:=3
  else
    result:=-1;
end;

function ParseShift(instruction: string; var parserpos: integer; var _result: int32): boolean;
var
  shiftnumber: integer;
  _param: string;
  _Rs: string;

  rs: integer;
  shiftamount: integer;

  oldparserpos: integer;
begin
  result:=true;
  oldparserpos:=parserpos;

  _param:=GetParam(instruction, parserpos);
  if _param<>'' then //there could be a <shift> part
  begin
    shiftnumber:=ShiftTypeToNumber(_param);
    if shiftnumber=-1 then
    begin
      parserpos:=oldparserpos;
      result:=false;
      exit;
    end;

    _result:=_result or (shiftnumber shl 5);

    _param:=GetParam(instruction, parserpos);
    if _param='' then raise exception.create('invalid shift parameters');

    if _param[1]='R' then
    begin
      //shift with register
      _result:=_result or (1 shl 4);
      _Rs:=_param;
      Rs:=GetRegNumber(_rs);
      if (rs=-1) then
        raise exception.create('Invalid shift register');

      _result:=_result or (rs shl 8);

    end
    else
    begin
      //shift with number
      shiftamount:=strtoint('$'+_param) and $ff;
      _result:=_result or (shiftamount shl 7);
    end;

  end;

end;

function getRegNumber(regstring: string): integer;
var i: integer;
begin
  result:=-1;
  for i:=0 to 15 do
  begin
    if (regstring=ArmRegisters[i]) or (regstring=ArmRegistersNoName[i]) then
    begin
      result:=i;
      break;
    end;
  end;
end;

function getParam(instruction: string; var parserpos: integer): string;
var
  i: integer;
  startpos: integer;

  insideblock: boolean;
begin
  result:='';
  startpos:=0;
  //find the start of the next param
  for i:=parserpos to length(instruction) do
  begin
    if not (instruction[i] in [' ',',']) then
    begin
      startpos:=i;
      break;
    end;
  end;

  //copy till the end or till another seperator is encountered and update parserpos
  if startpos<>0 then
  begin
    insideblock:=false;
    for i:=startpos to length(instruction) do
    begin
      if instruction[i] in ['{','['] then
        insideBlock:=not insideBlock;

      if (insideblock) or (not (instruction[i] in [' ',','])) then
        result:=result+instruction[i]
      else
      begin
        parserpos:=i;
        exit;
      end;

    end;

    parserpos:=length(instruction)+1;
  end;




end;

function getCondition(instruction: string; var parserpos: integer): integer;
var
  i: integer;
  s: string;
begin
  //check if at the current parserpos there's a condition
  result:=14;

  s:=copy(instruction, parserpos,2);
  for i:=0 to 15 do
  begin
    if s=ArmConditions[i] then
    begin
      inc(parserpos,length(s));
      result:=i;
      exit;
    end;
  end;
end;

function BranchParser(address: int32; instruction:string):int32;
var
  _param: string;
  parserpos: integer;
  destinationaddress: uint32;
  offset: int32;
begin
  //B{L}{cond} <expression>
  result:=5 shl 25;
  if length(instruction)<3 then exit; //invalid, there's just not enough space for this to be valid  (B 0)

  parserpos:=2;
  if instruction[2]='L' then
  begin
    //might be branch if Less than (BLT) wich is NOT a BL
    if instruction[3]<>'T' then //Not BLT  , so BLxx X
    begin
      parserpos:=3;
      result:=result or (1 shl 24); //set the L flag
    end;
  end;

  result:=result or (getCondition(instruction, parserpos) shl 28);  //set the condition bits

  //and set the offset
  _param:=getParam(instruction, parserpos);


  destinationaddress:=strtoint('$'+_param);

  if destinationaddress mod 4<>0 then raise exception.create('The destination address must be dividable by 4');

  offset:=destinationaddress-(address+8);
  offset:=offset shr 2;

  offset:=signextend(offset, 29);

  if abs(offset)>16777215 then raise exception.create('Distance is too big');

  result:=result or (offset and $ffffff);


end;

function CMPParser(address: int32; instruction:string):int32;
begin
  result:=$ffffffff;
end;

function CDPPArser(address: int32; instruction:string): int32;
begin

end;

function SingleDataParser(address: int32; instruction: string): int32;
begin

end;

procedure DataProcessingParser_OP2(address: int32; instruction: string; var parserpos: integer; var result: int32);
{
parse the <op2> part
}
var
  _param: string;
  _rm: string;
  rm: integer;
  ImmediateOperand: integer;
  v: integer;

  imm, rotate: integer;
begin
  //<Op2>
  _param:=GetParam(instruction, parserpos);
  if _param='' then raise exception.create('invalid parameters');

  if _param[1]='R' then
  begin
    //register  (Rm,{<shift>}
    _rm:=_param;
    rm:=GetRegNumber(_rm);
    if rm=-1 then
      raise exception.create('Invalid second operand register');

    result:=result or rm;

    if ParseShift(instruction, parserpos, result)=false then
      raise exception.create('Invalid shift'); //ParseShift returns false if there IS a parameter, but it's not a shift

    //else leave shift 0
  end
  else
  begin
    //number   (Op2=number)
    //Operand2 is an imm with a rotate
    ImmediateOperand:=1;
    result:=result or (ImmediateOperand shl 25);

    v:=strtoint('$'+_param);
    //calculate an value that gets to here
    generateRotateAndImm(v, Rotate, Imm);

    result:=result or (imm and $ff);
    result:=result or (rotate shl 8);
  end;
end;

function DataProcessingParser(address: int32; instruction:string):int32;
var
  _opcode: string;
  opcode: integer;
  condition: integer;
  parserpos: integer;

  S: integer;
  Rd: integer;
  Rs: integer;
  Rm: integer;
  Rn: integer;
  _Rd: string;
  _Rm: string;
  _Rs: string;
  _Rn: string;

  _ShiftOp: string;

  _param: string;
  i: integer;

  token: integer;
begin
  result:=0;
  _opcode:=copy(instruction,1,3);

  opcode:=-1;
  for i:=0 to 15 do
    if _opcode=DataProcessingOpcodes[i] then
    begin
      opcode:=i;
      break;
    end;

  if opcode=-1 then raise exception.create('Invalid opcode');

  result:=result or (opcode shl 21);

  parserpos:=4;
  condition:=getCondition(instruction, parserpos);
  result:=result or (condition shl 28);


  S:=0;


  if (_opcode='MOV') or (_opcode='MVN') then
  begin
    //<opcode>{cond}{S} Rd,<Op2>

    if instruction[parserpos]='S' then
    begin
      S:=1;
      inc(parserpos);
      result:=result or (s shl 20);
    end;

    _Rd:=GetParam(instruction, parserpos);
    rd:=GetRegNumber(_Rd);
    if rd=-1 then
      raise exception.create('Invalid destination register');

    result:=result or (rd shl 12);

    DataProcessingParser_OP2(address, instruction, parserpos, result);
  end
  else
  if (_opcode='CMP') or (_opcode='CMN') or (_opcode='TEQ') or (_opcode='TST') then
  begin
    //<opcode>{cond} Rn,<Op2>
    S:=1;
    result:=result or (s shl 20);

    _Rn:=GetParam(instruction, parserpos);
    rn:=GetRegNumber(_Rn);
    if rd=-1 then
      raise exception.create('Invalid first operand register');

    result:=result or (rn shl 16);
    DataProcessingParser_OP2(address, instruction, parserpos, result);


  end
  else
  begin
    //<opcode>{cond}{S} Rd,Rn,<Op2>
    if instruction[parserpos]='S' then
    begin
      S:=1;
      inc(parserpos);
      result:=result or (s shl 20);
    end;

    _Rd:=GetParam(instruction, parserpos);
    rd:=GetRegNumber(_Rd);
    if rd=-1 then
      raise exception.create('Invalid destination register');

    result:=result or (rd shl 12);

    _Rn:=GetParam(instruction, parserpos);
    rn:=GetRegNumber(_Rn);
    if rd=-1 then
      raise exception.create('Invalid first operand register');

    result:=result or (rd shl 12);

    DataProcessingParser_OP2(address, instruction, parserpos, result);

  end;
end;

function MultiDataParser(address: int32; instruction:string):int32;
begin

end;

function MRSParser(address: int32; instruction:string): int32;
//MRS{cond} Rd,<psr>
//<psr> is CPSR, CPSR_all, SPSR or SPSR_all.
var
  parserpos: integer;

  _psr: string;
begin
  result:=($f shl 16) or (2 shl 23);

  parserpos:=4;
  //cond
  result:=result or (getCondition(instruction, parserpos) shl 28);

  //rd
  result:=result or (getRegNumber(getParam(instruction, parserpos)) shl 12);

  _psr:=getParam(instruction, parserpos);

  if _psr='CPSR' then
    exit; //done

  if copy(_psr, 1, 4)='SPSR' then
    result:=result or (1 shl 22);
end;

function MSRParser(address: int32; instruction:string): int32;
//MSR{cond} <psr>,Rm
//MSR{cond} <psrf>,Rm
//MSR{cond} <psrf>,<#expression>
{
<psr> is CPSR, CPSR_all, SPSR or SPSR_all. (CPSR and CPSR_all are synonyms as are SPSR and SPSR_all)
<psrf> is CPSR_flg or SPSR_flg
}
var
  p1, p2: string;
  parserpos: integer;
  rm: integer;

  v: integer;
  rotate: integer;
  imm: integer;
begin
  result:=(2 shl 23);


  parserpos:=4;
  result:=result or (getCondition(instruction, parserpos) shl 28);

  p1:=uppercase(getParam(instruction, parserpos));
  p2:=getParam(instruction, parserpos);

  if copy(p1, 1, 4)='SPSR' then
    result:=result or (1 shl 22);

  if copy(p1, 5, 4)='_FLG' then
    result:=result or ($28F shl 12)
  else
    result:=result or ($29F shl 12);

  rm:=getRegNumber(p2);


  if (rm<>-1) then
  begin
    //register
    result:=result or Rm;
  end
  else
  begin
    //expression
    result:=result or (1 shl 25);

    v:=strtoint('$'+p2);
    generateRotateAndImm(v, rotate, imm);

    result:=(result or (rotate shl 8)) or imm;
  end;


end;

function MULParser(address: int32; instruction:string): int32;
begin

end;

function SWPParser(address: int32; instruction:string): int32;
begin

end;

function SWIParser(address: int32; instruction:string): int32;
begin

end;


function Assemble(address: int32; instruction: string): Int32;
var
  opcode: string;
  i: integer;
  searchstart,searchstop: integer;
begin
  result:=$ffffffff;

  instruction:=uppercase(trim(instruction));
  if instruction='' then exit;

  if instruction[1]='B' then
  begin
    //B/BL/BIC
    if copy(instruction,1,3)='BIC' then
      result:=DataProcessingParser(address, instruction)
    else
      result:=BranchParser(address, instruction);
  end
  else
  begin
    opcode:=copy(instruction,1,3);

    //find the opcode in the array and call the specific parser
    if length(opcode)<3 then exit;

    searchstart:=lookupindex[instruction[1]];
    if searchstart=-1 then
      exit;


    for i:=searchstart to OpcodeCount-1 do
    begin
      if OpcodeList[i].opcode[1]>opcode[1] then exit; //too far

      if OpcodeList[i].opcode=opcode then
      begin
        result:=opcodelist[i].parser(address, instruction);
        exit;
      end;
    end;

  end;



end;

procedure InitLookupIndex;
var
  i: char;
  j: integer;

  currentpos: integer;
begin
  currentpos:=0;
  for i:='A' to 'Z' do
  begin
    lookupindex[i]:=-1;

    for j:=currentpos to OpcodeCount-1 do
    begin
      if OpcodeList[j].opcode[1]=i then
      begin
        //found it
        lookupindex[i]:=j;
        currentpos:=j+1;
        break;
      end;

      if OpcodeList[j].opcode[1]>i then
      begin
        //too far, not found
        currentpos:=j;
        break;
      end;
    end;

  end;


end;

initialization
  InitLookupIndex;

end.

