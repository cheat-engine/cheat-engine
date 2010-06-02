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

    floathasseperator: boolean;
    couldbestringcounter: boolean;
begin
  //check if it matches a string
  result:=vtDword;
  floathasseperator:=false;

  isstring:=true;
  couldbestringcounter:=true;
  i:=0;

  while i<4 do
  begin
    //check if the first 4 characters match with a standard ascii values (32 to 127)
    if (buf[i]<32) or (buf[i]>127) then
    begin
      isstring:=false;
      if i>0 then
        couldbestringcounter:=false;

      if not couldbestringcounter then break;
    end;
    inc(i);
  end;

  if isstring then
  begin
    result:=vtString;
    exit;
  end;

  if couldbestringcounter then //check if the 4th byte of the 'string' is a char or not
    if (buf[5]>=32) or (buf[i]<=127) then
    begin
      //this is a string counter
      result:=vtByte;
      exit;
    end;



  //check if unicode
  isstring:=true;
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
      inc(i);
      if buf[i]<>0 then
      begin
        isstring:=false;
        break;
      end;
      inc(i);
    end;
  end else isstring:=false;

  if isstring then
  begin
    result:=vtUnicodeString;
    exit;
  end;  
  

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
      //check if the value isn't bigger or smaller than 100000 or smaller than -100000
      if (psingle(@buf[0])^<100000) and (psingle(@buf[0])^>-100000) then
      begin

        if pos(DecimalSeparator,x)>0 then
          floathasseperator:=true;

        result:=vtSingle;
        if not floathasseperator then exit;  //it's a full floating point value
      end;
    end;
  end;

  if size>=8 then  //check if a double can be used
  begin
    if pdouble(@buf[0])^<>0 then
    begin
      x:=floattostr(pdouble(@buf[0])^);
      if (pos('E',x)=0) then  //no exponent
      begin
        //check if the value isn't bigger or smaller than 100000 or smaller than -100000
        if (pdouble(@buf[0])^<100000) and (pdouble(@buf[0])^>-100000) then
        begin
          if result=vtSingle then
          begin
            if pdouble(@buf[0])^>psingle(@buf[0])^ then exit; //float has a smaller value
          end;

          result:=vtDouble;
          exit;
        end;
      end;
    end;
  end;

  //check if it's a pointer
  if ((pdword(@buf[0])^ mod 4)=0) and isreadable(pdword(@buf[0])^) then
  begin
    result:=vtPointer;
    exit;
  end;

  //still here
  result:=vtDword; //if nothing else
end;

end.
