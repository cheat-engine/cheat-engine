unit byteinterpreter;

interface

uses windows, sysutils, symbolhandler, cefuncproc, newkernelhandler;

function FindTypeOfData(address: dword; buf: pbytearray; size: integer):TVariableType;

implementation

function FindTypeOfData(address: dword; buf: pbytearray; size: integer):TVariableType;
{
takes the given
}
var x: string;
    i: integer;
    isstring: boolean;
    v: dword;
    e: integer;
begin
  i:=address mod 4;
  case i of
    1: //1 byte
    begin
      result:=vtByte;
      exit;
    end;

    2,3: //2 byte
    begin
      result:=vtWord;
      exit;
    end;
  end;

  if size>=4 then
  begin
    //named addresses 
    val('$'+symhandler.getNameFromAddress(pdword(@buf[0])^,true,true),v,e);
    if e>0 then //named
    begin
      result:=vtPointer;
      exit;
    end;
  end;


  if size>=8 then  //check if a double can be used
  begin
    if pdouble(@buf[0])^<>0 then
    begin
      x:=floattostr(pdouble(@buf[0])^);
      if (pos('E',x)=0) then  //no exponent
      begin
        //check if the value isn't bigger or smaller than 1000000 or smaller than -1000000
        if (pdouble(@buf[0])^<1000000) and (pdouble(@buf[0])^>-1000000) then
        begin
          result:=vtDouble;
          exit;
        end;
      end;
    end;
  end;

  if (size>=2) and (size<4) then
  begin
    result:=vtWord;
    exit;
  end
  else
  if (size=1) then
  begin
    result:=vtByte;
    exit;
  end
  else
  if psingle(@buf[0])^<>0 then
  begin
    x:=floattostr(psingle(@buf[0])^);
    if (pos('E',x)=0) then  //no exponent
    begin
      //check if the value isn't bigger or smaller than 1000000 or smaller than -1000000
      if (psingle(@buf[0])^<1000000) and (psingle(@buf[0])^>-1000000) then
      begin
        result:=vtSingle;
        exit;
      end;
    end;
  end;

  //still here, so check if it matches a string
  isstring:=true;
  i:=0;
  while i<4 do
  begin
    //check if the first 4 characters match with a standard ascii values (32 to 127)
    if (buf[i]<32) or (buf[i]>127) then
    begin
      isstring:=false;
      break;
    end;
    inc(i);
  end;

  if isstring then
  begin
    result:=vtString;
    exit;
  end;

  //check if unicode
  isstring:=false;
  i:=0;
  if size>=8 then
  begin
    while i<8 do
    begin
      //check if the first 4 characters match with a standard ascii values (32 to 127)
      if (buf[i]<32) or (buf[i]>127) then
      begin
        isstring:=false;
        break;
      end;
      inc(i,2);
    end;
  end;

  if isstring then
  begin
    result:=vtUnicodeString;
    exit;
  end;

  //check if it's a pointer
  if isreadable(pdword(@buf[0])^) then
  begin
    result:=vtPointer;
    exit;
  end;

  result:=vtDword; //if nothing else
end;

end.
