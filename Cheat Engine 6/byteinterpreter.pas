unit byteinterpreter;

{$MODE Delphi}

interface

uses windows, LCLIntf, sysutils, symbolhandler, CEFuncProc, NewKernelHandler, math, CustomTypeHandler;

function FindTypeOfData(address: ptrUint; buf: pbytearray; size: integer):TVariableType;
function DataToString(buf: PByteArray; size: integer; vartype: TVariableType): string;
function readAndParseAddress(address: ptrUint; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false): string;

implementation

function readAndParseAddress(address: ptrUint; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false): string;
var buf: array [0..7] of byte;
    buf2: pbytearray;
    x: dword;
begin
  result:='???';
  case variableType of
    vtByte:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],1,x) then
        if showashexadecimal then
          result:=inttohex(buf[0],2)
        else
          result:=inttostr(buf[0]);
    end;

    vtWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],2,x) then
        if showashexadecimal then
          result:=inttohex(pword(@buf[0])^,4)
        else
          result:=inttostr(pword(@buf[0])^);
    end;

    vtDWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        if showashexadecimal then
          result:=inttohex(pdword(@buf[0])^,8)
        else
          result:=inttostr(pdword(@buf[0])^);
    end;

    vtSingle:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        if showashexadecimal then
          result:=inttohex(pdword(@buf[0])^,8)
        else
          result:=floattostr(psingle(@buf[0])^);
    end;

    vtDouble:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],8,x) then
        if showashexadecimal then
          result:=inttohex(pqword(@buf[0])^,16)
        else
          result:=floattostr(pdouble(@buf[0])^);
    end;

    vtCustom:
    begin
      if customtype<>nil then
      begin
        getmem(buf2, customtype.bytesize);
        try
          if ReadProcessMemory(processhandle,pointer(address),buf2,customtype.bytesize,x) then
          begin
            try
              if showashexadecimal then
                result:=inttohex(customtype.ConvertDataToInteger(buf2),8)
              else
                result:=IntToStr(customtype.ConvertDataToInteger(buf2));
            except //no need to flood the user with meaningless error messages
            end;
          end;
        finally
          freemem(buf2);
        end;
      end;
    end;
  end;
end;


function DataToString(buf: PByteArray; size: integer; vartype: TVariableType): string;
{note: If type is of stringo unicode, the last 2 bytes will get set to 0, so watch what you're calling}
var tr: Widestring;
    i: integer;
    a: ptruint;

    tempbuf: pbytearray;
begin
  case vartype of
    vtByte: result:='(byte)'+inttohex(buf[0],2) + '('+inttostr(buf[0])+')';
    vtWord: result:='(word)'+inttohex(pword(buf)^,4) + '('+inttostr(pword(buf)^)+')';
    vtDword: result:='(dword)'+inttohex(pdword(buf)^,8) + '('+inttostr(pdword(buf)^)+')';
    vtQword: result:='(qword)'+inttohex(pqword(buf)^,16) + '('+inttostr(pqword(buf)^)+')';
    vtSingle: result:='(float)'+format('%.2f',[psingle(buf)^]);
    vtDouble: result:='(double)'+format('%.2f',[pdouble(buf)^]);
    vtString:
    begin
      getmem(tempbuf,size+1);
      copymemory(tempbuf,buf,size);

      try
        tempbuf[size]:=0;
        result:=pchar(tempbuf);
      finally
        freemem(tempbuf);
      end;
    end;

    vtUnicodeString:
    begin
      getmem(tempbuf,size+2);
      copymemory(tempbuf,buf,size);

      try
        tempbuf[size]:=0;
        tempbuf[size+1]:=0;
        tr:=PWideChar(tempbuf);
        result:=tr;

      finally
        freemem(tempbuf);
      end;
    end;

    vtPointer:
    begin
      if processhandler.is64bit then
        a:=ptruint(pqword(buf)^)
      else
        a:=ptruint(pdword(buf)^);

      result:='(pointer)'+symhandler.getNameFromAddress(a);


//      result:='(pointer)'+inttohex(pqword(buf)^,16) else result:='(pointer)'+inttohex(pdword(buf)^,8);
    end;

    else
    begin
      result:='(...)';
      for i:=0 to min(size,8)-1 do
        result:=result+inttohex(buf[i],2)+' ';

    end;
  end;
end;

function FindTypeOfData(address: ptrUint; buf: pbytearray; size: integer):TVariableType;
{
takes the given
}
var x: string;
    i: integer;
    isstring: boolean;
    e: integer;
    v: qword;

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
      if (pword(@buf[0])^<255) or ((pword(@buf[0])^ mod 10)>0) then //less than 2 byte or not dividable by 10
        result:=vtByte
      else
        result:=vtWord;
      exit;
    end;
  end;

  if size>=processhandler.pointersize then
  begin
    //named addresses

    if processhandler.is64bit then
    begin
      if (address mod 8) = 0 then
        val('$'+symhandler.getNameFromAddress(pqword(@buf[0])^,true,true),v,e)
      else
        e:=0;
    end
    else
    begin
      val('$'+symhandler.getNameFromAddress(pdword(@buf[0])^,true,true),v,e);
    end;

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

  if processhandler.is64Bit then
  begin

    if (address mod 8 = 0) and isreadable(pqword(@buf[0])^) then
    begin
      result:=vtPointer;
      exit;
    end;
  end
  else
  begin
    if isreadable(pdword(@buf[0])^) then
    begin
      result:=vtPointer;
      exit;
    end;
  end;

  if result=vtDword then
  begin
    //check if the value is a human usable value (between 0 and 10000 or dividable by at least 100)
    if pdword(@buf[0])^ > 10000 then
    begin
      if (pdword(@buf[0])^ mod 100) > 0 then
        result:=vtByte;
    end;

  end;
  //result:=vtDword; //if nothing else
end;

end.
