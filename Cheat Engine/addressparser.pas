unit addressparser;

interface

uses windows,SysUtils,dialogs,symbolhandler;

type TSeperator = (plus,minus,multiply); //the things the disassembler shows (NO DIVIDE)

var STR: string;
    ch: integer;
    address: dword;
    increase: boolean;
    total: array of byte; //0=seperator 1=value
    values: array of int64;
    seperators: array of TSeperator;

function getaddress(S: string):dword;

implementation

uses memorybrowserformunit;

procedure seperator;
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

procedure value;
//copy all characters of 0 to 9 in tempstr
var tmp: dword;
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

procedure aregister;
var tmp: dword;
    tmps: string;
begin
  tmp:=0;
  tmps:=copy(str,ch,3);
  if tmps='EAX' then tmp:=memorybrowser.eaxv else
  if tmps='EBX' then tmp:=memorybrowser.ebxv else
  if tmps='ECX' then tmp:=memorybrowser.ecxv else
  if tmps='EDX' then tmp:=memorybrowser.edxv else
  if tmps='ESI' then tmp:=memorybrowser.esiv else
  if tmps='EDI' then tmp:=memorybrowser.ediv else
  if tmps='EBP' then tmp:=memorybrowser.ebpv else
  if tmps='ESP' then tmp:=memorybrowser.espv else
  if tmps='EIP' then tmp:=memorybrowser.eipv;

  setlength(total,length(total)+1);
  total[length(total)-1]:=1;  //value

  setlength(values,length(values)+1);
  values[length(values)-1]:=tmp;

  inc(ch,3);
end;

procedure E;
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

function simplify3: boolean;
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


function simplify2: boolean;
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
      if total[i+1]=0 then //2 seperaters next to eachother
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


function simplify1: boolean;
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

function getaddress(S: string):dword;
var i,j: integer;
    scount,vcount: integer;
    tempstr: string;
begin
  try
    result:=0;
    result:=symhandler.getaddressfromname(s);
    if result<>0 then exit;
  except
    //it couldn't be found by getaddressfromname, so let's see if it's a more complex calculation or registers are involved
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
    '+','-','*' :  seperator;
    else        raise exception.Create('This is not a valid address');

    end;
  end;

  //total should now be filled with a string consisting of 0 and 1
  while simplify1 do ; //remove the - seperators
  while simplify2 do ; //remove double seperators
  while simplify3 do ; //remove the * by replacing the values with the multiplied value

  for i:=0 to length(values)-1 do inc(address,values[i]);
  result:=address;
end;

end.
