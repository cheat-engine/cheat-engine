unit addressparser;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}macport,{$endif}
  LCLIntf,SysUtils,dialogs,symbolhandler, NewKernelHandler;

resourcestring
  rsAPThisIsNotAValidAddress = 'This is not a valid address';

type
  TSeperator = (plus,minus,multiply); //the things the disassembler shows (NO DIVIDE)

  TAddressParser=class
  private
    STR: string;
    blurb: string;
    ch: integer;
    address: ptruint;
    increase: boolean;
    total: array of byte; //0=seperator 1=value
    values: array of int64;
    seperators: array of TSeperator;

    SpecialContext: PContext;



    procedure seperator;
    procedure value;

    procedure aregister;
    procedure e;

    function simplify3: boolean;
    function simplify2: boolean;
    function simplify1: boolean;
  public
    procedure setSpecialContext(c: PContext);
    function getaddress(S: string;skipsymhandler: boolean=false):ptrUint;
    function getBaseAddress(s: string):ptruint;
  end;


var mainthreadAddressParser: TAddressParser;

function getaddress(S: string):ptrUint; //for old code

implementation

uses memorybrowserformunit;


procedure TAddressParser.seperator;
//sets the seperator
begin

  setlength(total,length(total)+1);
  total[length(total)-1]:=0;

  setlength(seperators,length(seperators)+1);
  case str[ch] of
   '+'  : seperators[length(seperators)-1]:=plus;
   '-'  : seperators[length(seperators)-1]:=minus;
   '*'  : seperators[length(seperators)-1]:=multiply;
  end;
  inc(ch);
end;

procedure TAddressParser.value;
//copy all characters of 0 to 9 in tempstr
var tmp: ptruint;
    strp: pchar;
    count: integer;
begin

  strp:=@str[ch];
  val('$'+string(strp),tmp,count);

  setlength(total,length(total)+1);
  total[length(total)-1]:=1;  //value

  setlength(values,length(values)+1);
  values[length(values)-1]:=tmp;

  inc(ch,count-2);
  if count=0 then ch:=length(str)+1;   //it went to the end
end;

procedure TAddressParser.aregister;
var tmp: ptruint;
    tmps: string;
    c: Pcontext;
begin

  tmp:=0;
  tmps:=copy(str,ch,3);
  if SpecialContext<>nil then
    c:=SpecialContext
  else
    c:=@memorybrowser.lastdebugcontext;

  if tmps='EAX' then tmp:=c.{$ifdef cpu64}rax{$else}eax{$endif} else
  if tmps='EBX' then tmp:=c.{$ifdef cpu64}rbx{$else}ebx{$endif} else
  if tmps='ECX' then tmp:=c.{$ifdef cpu64}rcx{$else}ecx{$endif} else
  if tmps='EDX' then tmp:=c.{$ifdef cpu64}rdx{$else}edx{$endif} else
  if tmps='ESI' then tmp:=c.{$ifdef cpu64}rsi{$else}esi{$endif} else
  if tmps='EDI' then tmp:=c.{$ifdef cpu64}rdi{$else}edi{$endif} else
  if tmps='EBP' then tmp:=c.{$ifdef cpu64}rbp{$else}ebp{$endif} else
  if tmps='ESP' then tmp:=c.{$ifdef cpu64}rsp{$else}esp{$endif} else
  if tmps='EIP' then tmp:=c.{$ifdef cpu64}rip{$else}eip{$endif}
  {$ifdef cpu64}
  else
  if tmps='RAX' then tmp:=c.rax else
  if tmps='RBX' then tmp:=c.rbx else
  if tmps='RCX' then tmp:=c.rcx else
  if tmps='RDX' then tmp:=c.rdx else
  if tmps='RSI' then tmp:=c.rsi else
  if tmps='RDI' then tmp:=c.rdi else
  if tmps='RBP' then tmp:=c.rbp else
  if tmps='RSP' then tmp:=c.rsp else
  if tmps='RIP' then tmp:=c.rip else
  if tmps='R10' then tmp:=c.r10 else
  if tmps='R11' then tmp:=c.r11 else
  if tmps='R12' then tmp:=c.r12 else
  if tmps='R13' then tmp:=c.r13 else
  if tmps='R14' then tmp:=c.r14 else
  if tmps='R15' then tmp:=c.r15 else
  begin
    tmps:=copy(str,ch,2);
    if tmps='R8' then tmp:=c.r8 else
    if tmps='R9' then tmp:=c.r9;
  end
  {$endif}
  ;

  setlength(total,length(total)+1);
  total[length(total)-1]:=1;  //value

  setlength(values,length(values)+1);
  values[length(values)-1]:=tmp;

  inc(ch,length(tmps));
end;


procedure TAddressParser.E;
//find out if this is a register or a value
begin
  if (ch+2<=length(str)) then  //the 3th char of a reg is a X,I or P , so no A to F
  begin
    case str[ch+2] of
      'X','I','P' :aregister;
      else         value;
    end;
  end
  else value;
end;

function TAddressParser.simplify3: boolean;
var scount,vcount: integer;
    i,j: integer;
begin
  //calculate the *'s
  result:=false;
  scount:=0;
  vcount:=0;
  for i:=0 to length(total)-1 do
  begin
    if total[i]=0 then
    begin  //at a * multiplie the value before and after. (start from left to right)
      if seperators[scount]=multiply then
      begin
        for j:=scount to length(seperators)-2 do
          seperators[j]:=seperators[j+1];
        setlength(seperators,length(seperators)-1);

        values[vcount-1]:=values[vcount-1]*values[vcount];
        for j:=vcount to length(values)-2 do
          values[j]:=values[j+1];
        setlength(values,length(values)-1);

        //now remove the seperator and value after it
        for j:=i to length(total)-4 do      //DUH!!!!!!! 0101010101  (i could just cut the last 2 of...)
        begin
          total[j]:=total[j+2];
          total[j+1]:=total[j+3];
        end;

        setlength(total,length(total)-2);

        result:=true;
        exit;
      end;
      inc(scount);
    end else inc(vcount);

  end;
end;


function TAddressParser.simplify2: boolean;
var scount: integer;
    i,j: integer;
begin
  //find the spots that have 2 seperators next to eachother and remove the + from it
  scount:=0;
  result:=false;
  for i:=0 to length(total)-2 do
  begin
    if total[i]=0 then
    begin
      if total[i+1]=0 then //2 separators next to eachother
      begin
        if seperators[scount]=plus then
        begin
          for j:=scount to length(seperators)-2 do
            seperators[j]:=seperators[j+1];
          setlength(seperators,length(seperators)-1);

          for j:=i to length(total)-2 do
            total[j]:=total[j+1];
          setlength(total,length(total)-1);
          result:=true;
          exit;
        end else
        begin  //this'll take care of ** too
          for j:=scount+1 to length(seperators)-2 do
            seperators[j]:=seperators[j+1];
          setlength(seperators,length(seperators)-1);

          for j:=i+1 to length(total)-2 do
            total[j]:=total[j+1];
          setlength(total,length(total)-1);
          result:=true;
          exit;
        end;
      end;
      inc(scount);
    end;
  end;
end;


function TAddressParser.simplify1: boolean;
var scount,vcount: integer;
    i,j: integer;
begin
  //find a - and remove it
  //then do valueafter:=-valueafter
  scount:=0;
  vcount:=0;
  for i:=0 to length(total)-1 do
  begin
    if total[i]=0 then
    begin
      if seperators[scount]=minus then
      begin
        values[vcount]:=-values[vcount];
        seperators[scount]:=plus;
        result:=true;
        exit;
      end;
      inc(scount);
    end else inc(vcount);
  end;
  result:=false;
end;


function TAddressParser.getaddress(s: string;skipsymhandler: boolean=false): ptrUint;
var i,j: integer;
    scount,vcount: integer;
    tempstr: string;
    haserror: boolean;

    c: Pcontext;
begin
  if skipsymhandler=false then
  begin
    if SpecialContext<>nil then
      c:=SpecialContext
    else
      c:=@memorybrowser.lastdebugcontext;

    result:=symhandler.getaddressfromname(s,false,haserror, c);
    if (result<>0) and (not haserror) then exit;
  end;

  if s='' then s:='0';
  setlength(total,2);
  setlength(values,1);
  setlength(seperators,1);
  values[0]:=0;             //0+
  seperators[0]:=plus;
  total[0]:=1; //value
  total[1]:=0; //seperator


  ch:=1;
  address:=0;
  increase:=true;
  str:=uppercase(s);


  while ch<=length(s) do
  begin
    case str[ch] of
    '$'      :  begin
                  inc(ch);
                  value;
                end;
    '0'..'9' :  value;
    'A'..'D' :  value;
    'E'      :  E;
    'F'      :  value;
    'R'      :  aregister;
    '+','-','*' :  seperator;
    else        raise exception.Create(rsAPThisIsNotAValidAddress);

    end;
  end;

  //total should now be filled with a string consisting of 0 and 1
  while simplify1 do ; //remove the - seperators
  while simplify2 do ; //remove double seperators
  while simplify3 do ; //remove the * by replacing the values with the multiplied value

  for i:=0 to length(values)-1 do inc(address,values[i]);
  result:=address;

end;

function TAddressParser.getBaseAddress(s: string):ptruint;
var maxvalue: ptruint;
    i: integer;
begin
  getaddress(s, true);
  maxvalue:=0;
  for i:=0 to length(values)-1 do
  begin
    if values[i]>0 then
    begin
      if maxvalue<ptruint(values[i]) then
        maxvalue:=ptruint(values[i]);
    end;
  end;

  result:=maxvalue;
end;

procedure TAddressParser.setSpecialContext(c: PContext);
begin
  SpecialContext:=c;
end;

function getaddress(S: string):ptrUint;
begin
  result:=mainthreadAddressParser.getaddress(s);
end;

initialization
  mainthreadAddressParser:=TAddressParser.create;

finalization
  mainthreadAddressParser.free;

end.
