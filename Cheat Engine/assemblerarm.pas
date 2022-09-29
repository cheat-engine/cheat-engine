unit assemblerArm;

{$mode objfpc}{$H+}
{$warn 2005 off}

interface


{$ifdef jni}
uses classes, SysUtils, StrUtils, assemblerunit, symbolhandler;
{$else}
uses
  Classes, SysUtils, strutils{$ifndef ARMDEV}, assemblerunit{$endif}, dialogs,
  symbolhandler;
{$endif}

{$ifdef ARMDEV}
type TAssemblerBytes=array of byte;
{$endif}

type ENeedRewrite=class(exception)
  public
    useinstead: Tstringlist;
    constructor Create(const msg: String);
    destructor Destroy; override;
end;

  function BranchParser(address: int32; instruction:string):int32;
  function BranchExchangeParser(address: int32; instruction:string):int32;
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

  function DefineDwordParser(address: int32; instruction:string):int32;

  function getParam(instruction: string; var parserpos: integer): string;
  function getRegNumber(regstring: string): integer;



  function ArmAssemble(address: ptruint; instruction: string; var bytes: TAssemblerBytes): boolean;

implementation

uses DisassemblerArm, ProcessHandlerUnit, DisassemblerARM32, DisAssemblerARM64, disassemblerArm32Thumb;

resourcestring
  rsTheValue = 'The value ';
  rsCanNotBeEncoded = ' can not be encoded using 8 bits and double rotate';
  rsInvalidShiftParameters = 'invalid shift parameters';
  rsThisInstructionClassDoesNotAllow = 'This instruction class does not allow a register based shift';
  rsInvalidShiftRegister = 'Invalid shift register';
  rsTheDestinationAddressMustBeDividableBy4 = 'The destination address must be dividable by 4';
  rsDistanceIsTooBig = 'Distance is too big';
  rsTodoChangeThisToA12ByteInstruction = 'Todo: Change this to a 12 byte instruction: LDR/STR [PC,#16] - B PC - DD offset';
  rsInvalidRegister = 'Invalid register';
  rsInvalidShift = 'Invalid shift';
  rsTheDistanceIsTooBig = 'The distance is too big';
  rsInvalidParameter1 = 'Invalid parameter 1';
  rsInvalidParameters = 'invalid parameters';
  rsInvalidOpcode = 'Invalid opcode';
  rsInvalidDestinationRegister = 'Invalid destination register';
  rsInvalidFirstOperandRegister = 'Invalid first operand register';
  rsInvalidRegisterList = 'Invalid register list';
  rsInvalidRegisterInRegisterList = 'Invalid register in register list:';
  rsInvalidParameter2 = 'Invalid parameter 2';
  rsInvalidParameter3 = 'Invalid parameter 3';
  rsInvalidParameter4 = 'Invalid parameter 4';

type Tparser=function(address: int32; instruction: string): int32;

type
  TOpcodeData=record
    opcode: string;
    parser: TParser;
    //scanner
    //bitmask
    //bits
  end;




const OpcodeCount=30;

const OpcodeList : array [0..OpcodeCount-1] of TOpcodeData= (
  (opcode: 'ADC'; parser:@DataProcessingParser),
  (opcode: 'ADD'; parser:@DataProcessingParser),
  (opcode: 'AND'; parser:@DataProcessingParser),
  (opcode: 'B'; parser:@BranchParser),
  (opcode: 'BIC'; parser:@DataProcessingParser),
  (opcode: 'BL'; parser:@BranchParser),
  (opcode: 'BX'; parser:@BranchExchangeParser),

  (opcode: 'CDP'; parser:@CDPPArser),
  (opcode: 'CMP'; parser:@DataProcessingParser),
  (opcode: 'CMN'; parser:@DataProcessingParser),
  (opcode: 'DD'; parser:@DefineDwordParser),

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
 // (opcode: 'PUSH'; parser:@PushParser),
 // (opcode: 'POP'; parser:@PushParser),

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

constructor ENeedRewrite.Create(const msg: string);
begin
  useinstead:=TStringList.create;

  inherited Create(msg);
end;

destructor ENeedRewrite.Destroy;
begin
  useinstead.free;
  inherited destroy;
end;

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

    if smallest.value>255 then raise ENeedRewrite.create(rsTheValue+inttohex(value,1)+rsCanNotBeEncoded);

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

function ParseShift(instruction: string; var parserpos: integer; var _result: int32; noregisterbasedshift: boolean=false): boolean;
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
    if _param='' then raise exception.create(rsInvalidShiftParameters);

    if _param[1]='R' then
    begin
      //shift with register
      if noregisterbasedshift then
        raise exception.create(rsThisInstructionClassDoesNotAllow);

      _result:=_result or (1 shl 4);
      _Rs:=_param;
      Rs:=GetRegNumber(_rs);
      if (rs=-1) then
        raise exception.create(rsInvalidShiftRegister);

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
    if not (instruction[i] in [' ',',','[',']','{', '}', '!','^']) then
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
      if not (instruction[i] in [' ',',','[',']','{','}','!','^']) then
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

function DefineDwordParser(address: int32; instruction:string):int32;
var
  _param: string;
  parserpos: integer;
begin
  result:=0;
  parserpos:=3;

  _param:=getParam(instruction, parserpos);

  result:=symhandler.getAddressFromName(_param);
end;

function BranchExchangeParser(address: int32; instruction:string):int32;
var
  _param: string;
  rn: integer;
  parserpos: integer;
begin
  //BX{cond} <Rn>
  result:=$12FFF10;
  if length(instruction)<3 then exit; //invalid, there's just not enough space for this to be valid  (B 0)

  parserpos:=3;
  result:=result or (getCondition(instruction, parserpos) shl 28);  //set the condition bits

  //and set the register
  _param:=getParam(instruction, parserpos);
  rn:=getRegNumber(_param);

  if rn=-1 then
    raise exception.create(rsInvalidRegister);

  result:=result or rn; //set the register bits



end;

function BranchParser(address: int32; instruction:string):int32;
var
  _param: string;
  parserpos: integer;
  destinationaddress: uint32;
  offset: int32;

  e: ENeedRewrite;

  islink: boolean;
  condition: integer;

  _cond: string;
begin
  //B{L}{cond} <expression>
  result:=5 shl 25;
  if length(instruction)<3 then exit; //invalid, there's just not enough space for this to be valid  (B 0)

  parserpos:=2;
  if instruction[2]='L' then
  begin
    islink:=true;

    //might be branch if Less than (BLT) wich is NOT a BL
    if instruction[3]<>'T' then //Not BLT  , so BLxx X
    begin
      parserpos:=3;
      result:=result or (1 shl 24); //set the L flag
    end;
  end
  else
    islink:=false;

  condition:=getCondition(instruction, parserpos);
  result:=result or (condition shl 28);  //set the condition bits

  //and set the offset
  _param:=getParam(instruction, parserpos);


  destinationaddress:=symhandler.getAddressFromName(_param);

  if destinationaddress mod 4<>0 then raise exception.create(rsTheDestinationAddressMustBeDividableBy4);

  offset:=destinationaddress-(address+8);
  offset:=offset shr 2;

  offset:=signextend(offset, 29);

  if abs(offset)>16777215 then
  begin
    e:=ENeedRewrite.create(rsDistanceIsTooBig);
    //push the destination into the stack and pop it out into pc



    if condition<>14 then
    begin
      //not always
      //reminder: ADD<cond> and not ADDS<cond> (or ADD<cond>S): The S would mean the flags get updated after execution

      if islink then
        e.useinstead.add('ADD'+ArmConditions[condition]+' LR, PC, 8'); //LR=PC+8 :  PC is at +8  (B XXXXXXXX) so +8 to get over those B and DD instructions

      e.useinstead.add('LDR'+ArmConditions[condition]+' PC, [PC, -4]');
      e.useinstead.add('B '+inttohex(address+12,8));
      e.useinstead.add('DD '+inttohex(destinationaddress,8));
    end
    else
    begin
      if islink then
        e.useinstead.add('ADD LR, PC, 4'); //LR=PC+4 : PC is at +8 (DD xxxxxxxx) so +4 to get over that

      e.useinstead.add('LDR PC, [PC, -4]');
      e.useinstead.add('DD '+inttohex(destinationaddress,8));
    end;
    raise e;

  end;

  result:=result or (offset and $ffffff);


end;

function CMPParser(address: int32; instruction:string):int32;
begin
  result:=int32($ffffffff);
end;

function CDPPArser(address: int32; instruction:string): int32;
begin
  result:=0; //NYI
end;


procedure SingleDataParser_AddressRegister(address: int32; instruction: string; var parserpos: integer; var result: int32);
//either twe preindexed:        (p=1)
//  [Rn]
//  [Rn, offset] {!}
//  [Rn,{+/-}Rm{,<shift>}]{!}

//or postindexed          (p=0)
//[Rn],<#expression>
//[Rn],{+/-}Rm{,<shift>}
var _Rn, _Rm: string;

  Rn, Rm: integer;
  _param: string;

  offset_, offset: int32;
  e: ENeedRewrite;

  oldparserpos: integer;

begin
  if instruction[length(instruction)]='!' then
    result:=result or (1 shl 21);

  oldparserpos:=parserpos;
  inc(parserpos);


  _Rn:=getParam(instruction, parserpos);

  rn:=getRegNumber(_rn);
  if rn=-1 then
  begin
    //if hexadecimal value then convert to a pc,offset

    offset_:=symhandler.getAddressFromName(_rn);
    //convert this to a pc relative address
    offset:=offset_-(address+8);

    if abs(offset)>$fff then
    begin
      e:=ENeedRewrite.create(rsTodoChangeThisToA12ByteInstruction);
      e.useinstead.add(copy(instruction, 1, oldparserpos)+'PC,4]');

      if instruction[length(instruction)]='!' then
        e.useinstead.add(copy(instruction, 1, oldparserpos)+'R'+inttostr((result shr 12) and $f)+',0]!')
      else
        e.useinstead.add(copy(instruction, 1, oldparserpos)+'R'+inttostr((result shr 12) and $f)+',0]');

      e.useinstead.add('B '+inttohex(address+16,8));
      e.useinstead.add('DD '+inttohex(offset_,8));
      raise e;
    end;

    instruction:=StringReplace(instruction,'['+_rn+']', '[PC,'+inttohex(offset,1)+']', [rfIgnoreCase]);
    dec(parserpos, length(_rn));
    inc(parserpos, 2);

    rn:=15;
  end;

  result:=result or (rn shl 16);

  if not ((instruction[parserpos]=']') and (length(instruction)<>parserpos)) then
  begin
    //not postindexed, (so preindexed)
    //set P=1
    result:=result or (1 shl 24);
  end;


  _param:=getParam(instruction, parserpos);
  if _param<>'' then
  begin
    if (length(_param)>=2) and ((_param[1]='R') or (_param[2]='R')) then
    begin
      //  [Rn,{+/-}Rm{,<shift>}]{!}
      result:=result or (1 shl 25); //set I to 1 to mark it's a register

      if _param[1]<>'-' then
      begin
        //+ or R, add to base
        result:=result or (1 shl 23);


        if _param[1]='+' then
          _param:=copy(_param, 2, length(_param)-1); //strip the plus from the register
      end;


      _rm:=_param;
      rm:=GetRegNumber(_rm);
      if rm=-1 then
        raise exception.create(rsInvalidRegister);

      if ParseShift(instruction, parserpos, result)=false then
        raise exception.create(rsInvalidShift); //ParseShift returns false if there IS a parameter, but it's not a shift


    end
    else
    begin
      //  [Rn, offset] {!}

      if _param='' then offset:=0
      else
      begin
        if _param[1]='-' then
          offset:=-strtoint('$'+copy(_param, 2, length(_param)-1))
        else
          offset:=strtoint('$'+_param);
      end;

      if offset>0 then //set the U bit
        result:=result or (1<<23)
      else
        offset:=abs(offset);


      result:=result or (offset and $fff);
    end;


  end;

end;

procedure SingleDataParser_AddressExpression(address: int32; instruction: string; var parserpos: integer; var result: int32);
//  <expression>
var
  _destination: string;
  destination: uint32;
  offset: int32;
begin

  _destination:=getParam(instruction, parserpos);
  destination:=symhandler.getAddressFromName(_destination);


  offset:=destination-(address+8);

  if abs(offset)>$fff then raise ENeedRewrite.create(rsTheDistanceIsTooBig);

  result:=result or $fff;
end;

function SingleDataParser(address: int32; instruction: string): int32;
//<LDR|STR>{cond}{B}{T} Rd,<Address>

//<Address>:
//  <expression>
//  [Rn]
//  [Rn, offset] {!}
//  [Rn,{+/-}Rm{,<shift>}]{!}

var
  parserpos: integer;
  T: boolean;

  rd: integer;
  i: integer;
begin
  result:=1 shl 26;
  if instruction[1]='L' then
    result:=result or (1 shl 20);

  parserpos:=4;
  result:=result or (getCondition(instruction, parserpos) shl 28);

  if instruction[parserpos]='B' then
  begin
    result:=result+(1 shl 22);
    inc(parserpos);
  end;

  if instruction[parserpos]='T' then
  begin
    T:=true;
    inc(parserpos);
  end
  else
    T:=false;

  //rd

  rd:=getRegNumber(getParam(instruction, parserpos));
  if rd=-1 then
    raise exception.create(rsInvalidParameter1);

  result:=result or (rd shl 12);


  //parse <Address>
  {
  //  <expression>
  //  [Rn]
  //  [Rn, offset] {!}
  //  [Rn,{+/-}Rm{,<shift>}]{!}
  }

  while parserpos<length(instruction) do
  begin
    if not (instruction[parserpos] in [',', ' ']) then
    begin
      if instruction[parserpos]='[' then
        SingleDataParser_AddressRegister(address, instruction, parserpos, result)
      else
        SingleDataParser_AddressExpression(address, instruction, parserpos, result);

      exit;
    end;

    inc(parserpos);
  end;

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
  if _param='' then raise exception.create(rsInvalidParameters);

  rm:=getRegNumber(_param);
  if rm<>-1 then
  begin
    //register  (Rm,{<shift>}
    result:=result or rm;

    if ParseShift(instruction, parserpos, result)=false then
      raise exception.create(rsInvalidShift); //ParseShift returns false if there IS a parameter, but it's not a shift

    //else leave shift 0
  end
  else
  begin
    //number   (Op2=number)
    //Operand2 is an imm with a rotate
    ImmediateOperand:=1;
    result:=result or (ImmediateOperand shl 25);

    v:=symhandler.getAddressFromName(_param);
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

  if opcode=-1 then raise exception.create(rsInvalidOpcode);

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
      raise exception.create(rsInvalidDestinationRegister);

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
    if rn=-1 then
      raise exception.create(rsInvalidFirstOperandRegister);

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
    //destination
    _Rd:=GetParam(instruction, parserpos);
    rd:=GetRegNumber(_Rd);
    if rd=-1 then
      raise exception.create(rsInvalidDestinationRegister);

    result:=result or (rd shl 12);

    //1st op reg
    _Rn:=GetParam(instruction, parserpos);
    rn:=GetRegNumber(_Rn);
    if rn=-1 then
      raise exception.create(rsInvalidFirstOperandRegister);

    result:=result or (rn shl 16);

    DataProcessingParser_OP2(address, instruction, parserpos, result);

  end;
end;

function MultiDataParser(address: int32; instruction:string):int32;
//<LDM|STM>{cond}<FD|ED|FA|EA|IA|IB|DA|DB> Rn{!},<Rlist>{^}
const
  bP=1 shl 24;
  bU=1 shl 23;

var
  LDM: boolean;
  _modename: string;
  parserpos: integer;

  rn: integer;
  _param: string;

  reg, reg2: integer;
  i: integer;
begin
  result:=4 shl 25;

  //<LDM|STM>
  if instruction[1]='L' then //LDM
  begin
    ldm:=true;
    result:=result or (1 shl 20);      //set bit L
  end;

  parserpos:=4;


  //<cond>
  result:=result or (getCondition(instruction, parserpos) shl 28);

  //<FD|ED|FA|EA|IA|IB|DA|DB>
  _modename:=copy(instruction, parserpos, 2);
  inc(parserpos,2);

  case _modename[1] of
    'D':
      case _modename[2] of
        'A': ;   // 0 - 0
        'B': result:=result or bP;
        else raise exception.create(rsInvalidOpcode);
      end;

    'E':
      case _modename[2] of
        'A':
        begin
          if instruction[1]='S' then
            result:=result or bU
          else
            result:=result or bP;
        end;

        'D':
        begin
          if instruction[1]='L' then
          begin
            result:=result or bU;
            result:=result or bP;
          end;
        end

        else raise exception.create(rsInvalidOpcode);
      end;

    'F':
      case _modename[2] of
        'A' :
        begin
          if instruction[1]='S' then
          begin
            result:=result or bU;
            result:=result or bP;
          end;
        end;
        'D' :
        begin
          if instruction[1]='L' then
            result:=result or bU
          else
            result:=result or bP;
        end;

        else raise exception.create(rsInvalidOpcode);
      end;

    'I':
      case _modename[2] of
        'A': result:=result or bU;
        'B': result:=result or bU or bP;
        else raise exception.create(rsInvalidOpcode);
      end;

    else raise exception.create(rsInvalidOpcode);
  end;

  rn:=getRegNumber(getParam(instruction, parserpos));
  if rn=-1 then
    raise exception.create(rsInvalidRegister);

  result:=result or (rn shl 16);

  if instruction[parserpos]='!' then //set Write back
  begin
    result:=result or (1 shl 21);
    inc(parserpos);
  end;

  //todo, add some checks to see that brackets {} are used
  _param:=getParam(instruction, parserpos);
  reg:=-1;
  while (_param<>'') do
  begin
    if _param='^' then //set S
    begin
      result:=result or (1 shl 22)
    end
    else
    begin
      i:=pos('-',_param);

      if i>0 then //register range
      begin
        //get the first regisster

        reg:=getRegNumber(trim(copy(_param,1,i-1)));
        reg2:=getRegNumber(trim(copy(_param,i+1,length(_param))));

        if (reg=-1) or (reg2=-1) then raise exception.create(rsInvalidRegisterList);

        for i:=reg to reg2 do
          result:=result or (1 shl i);

        reg:=-1;

      end
      else
      begin
        reg:=getRegNumber(_param);
        if reg=-1 then
          raise exception.create(rsInvalidRegisterInRegisterList+_param);

        result:=result or (1 shl reg);
      end;
    end;
    _param:=getParam(instruction, parserpos);
  end;




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

    v:=symhandler.getAddressFromName(p2);
    generateRotateAndImm(v, rotate, imm);

    result:=(result or (rotate shl 8)) or imm;
  end;


end;

function MULParser(address: int32; instruction:string): int32;
//MUL/MLA
//MUL{cond}{S} Rd,Rm,Rs
//MLA{cond}{S} Rd,Rm,Rs,Rn

var _opcode: string;
  parserpos: integer;
  Rd, Rm, Rs, Rn: integer;
begin
  result:=9 shl 4;
  _opcode:=copy(instruction,1,3);

  //fill in condition
  parserpos:=4;
  result:=result or (getCondition(instruction, parserpos) shl 28);

  if instruction[parserpos]='S' then
  begin
    result:=result or (1 shl 21);
    inc(parserpos);
  end;

  rd:=getRegNumber(getParam(instruction, parserpos));
  if rd=-1 then
    raise exception.create(rsInvalidParameter1);

  result:=result or (rd shl 16);


  rm:=getRegNumber(getParam(instruction, parserpos));
  if rm=-1 then
    raise exception.create(rsInvalidParameter2);

  result:=result or rm;

  rs:=getRegNumber(getParam(instruction, parserpos));
  if rs=-1 then
    raise exception.create(rsInvalidParameter3);

  result:=result or (rs shl 8);

  //finally check which opcode it is
  if _opcode='MLA' then
  begin
    //fill in Rn and set bit 21
    result:=result or (1 shl 21);

    rn:=getRegNumber(getParam(instruction, parserpos));
    if rn=-1 then
      raise exception.create(rsInvalidParameter4);

    result:=result or (rn shl 12);

  end;

end;

function SWPParser(address: int32; instruction:string): int32;
//<SWP>{cond}{B} Rd,Rm,[Rn]
var
  rd, rm, rn: integer;
  parserpos: integer;
begin
  result:=$1000090;

  parserpos:=4;
  result:=result or (getCondition(instruction, parserpos) shl 28);

  if instruction[parserpos]='B' then
  begin
    result:=result or (1 shl 22);
    inc(parserpos);
  end;

  rd:=getRegNumber(getParam(instruction, parserpos));
  rm:=getRegNumber(getParam(instruction, parserpos));
  rn:=getRegNumber(getParam(instruction, parserpos));

  if (rd=-1) or (rm=-1) or (rn=-1) then
    raise exception.create(rsInvalidRegister);

  result:=result or (rd shl 12);
  result:=result or rm;
  result:=result or (rn shl 16);
end;

function SWIParser(address: int32; instruction:string): int32;
//SWI{cond} <expression>
var
  parserpos: integer;
  v: integer;
begin
  result:=$f shl 24;
  parserpos:=4;
  result:=result or (getCondition(instruction, parserpos) shl 28);

  v:=symhandler.getAddressFromName(getParam(instruction, parserpos));
  result:=result or (v and $FFFFFF);


end;


function ArmAssemble(address: ptruint; instruction: string; var bytes: TAssemblerBytes): boolean;
var
  opcode: string;
  i,j: integer;
  searchstart,searchstop: integer;

  r: uint32;
  b: Tassemblerbytes;

  oldlength: integer;
  d32: TArm32Instructionset;
  d64: TArm64Instructionset;
  dThumb: TThumbInstructionset;
  len: integer;
begin
  result:=false;

  if processhandler.is64Bit then
  begin
    try
      r:=d64.assemble(address, instruction);
      setlength(bytes,4);
      pdword(@bytes[0])^:=r;
      exit(true);
    except
      exit(false);
    end;

  end
  else
  begin
    if (address and 1) = 1 then
    begin
      try
        dThumb.assemble(address, instruction);

        bytes:=dthumb.LastDisassembleData.Bytes;
        {setlength(bytes,len);
        if len=2 then
          pword(@bytes[0])^:=r
        else
          pdword(@bytes[0])^:=r;   }

        exit(true);

      except
      end;
    end
    else
    begin
      try
        r:=d32.assemble(address, instruction);
        setlength(bytes,4);
        pdword(@bytes[0])^:=r;
        exit(true);
      except
      end;
    end;
  end;

  if (address and 1) = 1 then exit(FalsE);  //no thumb supported yet

  r:=$ffffffff;
  setlength(bytes,0);

  instruction:=uppercase(trim(instruction));
  if instruction='' then exit;

  if instruction='NOP' then
    instruction:='MOV R0,R0';

  try
    if instruction[1]='B' then
    begin
      //B/BL/BIC/BX
      if copy(instruction,1,3)='BIC' then
        r:=DataProcessingParser(address, instruction)
      else
      if copy(instruction,1,2)='BX' then
        r:=BranchExchangeParser(address, instruction)
      else
        r:=BranchParser(address, instruction);
    end
    else
    if instruction[1]='D' then
    begin
      if copy(instruction,1,2)='DD' then
        r:=DefineDwordParser(address, instruction);
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
          r:=opcodelist[i].parser(address, instruction);
          break;
        end;
      end;

    end;


  except
    on e: ENeedRewrite do
    begin
      //reformat this instruction
      //The ENeedRewrite contains a list of instructions that should be used instead

      if e.useinstead.count>0 then
      begin
        //assemble with this instead
        for i:=0 to e.useinstead.count-1 do
        begin
          setlength(b,0);
          //the useinstead code should be a program counter independant code so no need to watch it
          if armassemble(address+i*4, e.useinstead[i], b) then
          begin
            oldlength:=length(bytes);
            setlength(bytes, oldlength+length(b));
            for j:=oldlength to oldlength+length(b)-1 do
              bytes[j]:=b[j-oldlength];
          end
          else
            raise ENeedRewrite.create(e.message);

        end;

        //still here
        result:=true;
        exit;
      end
      else
        raise exception.Create(e.Message);

    end;
  end;


  if r=$ffffffff then exit;

  setlength(bytes, 4);
  pdword(@bytes[0])^:=r;
  result:=true;
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

