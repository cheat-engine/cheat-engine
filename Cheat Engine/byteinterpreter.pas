unit byteinterpreter;

{$MODE Delphi}
{$WARN 4105 off : Implicit string type conversion with potential data loss from "$1" to "$2"}
{$WARN 4104 off : Implicit string type conversion from "$1" to "$2"}
interface

{$ifdef windows}
uses windows, LCLIntf, sysutils, symbolhandler, CEFuncProc, NewKernelHandler, math,
  CustomTypeHandler, ProcessHandlerUnit, commonTypeDefs, LazUTF8;
{$endif}

{$ifdef unix}
uses unixporthelper, sysutils, symbolhandler, ProcessHandlerUnit, NewKernelHandler, math,
  CustomTypeHandler, commonTypeDefs;
{$endif}

resourcestring
  rsBIByte = '(byte)';
  rsBIWord = '(word)';
  rsBIDword = '(dword)';
  rsBIQword = '(qword)';
  rsBIFloat = '(float)';
  rsBIDouble = '(double)';


type TAutoGuessEvent=function (address: ptruint; originalVariableType: TVariableType): TVariableType of object;

type
  TFindTypeOption=(biNoString, biNoDouble);
  TFindTypeOptions=set of TFindTypeOption;

function isHumanReadableInteger(v: integer): boolean; //returns false if it's not an easy readable integer

function FindTypeOfData(address: ptrUint; buf: pbytearray; size: integer; CustomType: PCustomType=nil; FindOption: TFindTypeOptions=[]):TVariableType;
function DataToString(buf: PByteArray; size: integer; vartype: TVariableType; clean: boolean=false): string;
function readAndParsePointer(address: ptruint; buf: pbytearray; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false; showAsSigned: boolean=false; bytesize:integer=1): string;
function readAndParseAddress(address: ptrUint; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false; showAsSigned: boolean=false; bytesize:integer=1): string;
procedure ParseStringAndWriteToAddress(value: string; address: ptruint; variabletype: TVariabletype; hexadecimal: boolean=false; customtype: TCustomType=nil);

var onAutoGuessRoutine: TAutoGuessEvent;


implementation

uses parsers;

{$ifdef unix}
function isreadable(address: ptruint): boolean;
var x: dword;
    t: byte;
begin
  result:=ReadProcessMemory(processhandle, pointer(address), @t, 1, x);
end;

{$endif}

procedure ParseStringAndWriteToAddress(value: string; address: ptruint; variabletype: TVariabletype; hexadecimal: boolean=false; customtype: TCustomType=nil);
{
Function to wrap all the occasional writing in
}
var v: qword;
    s: single;
    d: double;
    x: PTRUINT;

    i: integer;
    ba: PByteArray;

    b: tbytes;
    us: Widestring;
begin
  if hexadecimal and (variabletype in [vtsingle, vtDouble]) then
  begin
    if variabletype=vtSingle then
      variabletype:=vtDword
    else
      variabletype:=vtQword;
  end;

  if variabletype=vtByteArray then
  begin
    setlength(b,0);
    ConvertStringToBytes(value, hexadecimal, b);
    getmem(ba, length(b));
    try
      for i:=0 to length(b)-1 do
      begin
        if (b[i]>=0) then
          WriteProcessMemory(processhandle, pointer(address+i), @b[i], 1, x);
      end;
    finally
      freememandnil(ba);

    end;

    setlength(b,0);
  end
  else
  begin
    if variabletype in [vtSingle, vtDouble] then
    begin
      d:=StrToFloat(value);
      s:=d;
    end
    else
    begin
      if not (variabletype in [vtString, vtUnicodeString]) then
      begin
        if hexadecimal then
          value:='$'+value;

        v:=StrToQWordEx(value);

        if (variabletype=vtCustom) and customtype.scriptUsesFloat then
          s:=StrToFloat(value);
      end;
    end;

    case variabletype of
      vtByte: WriteProcessMemory(processhandle, pointer(address), @v, 1, x);
      vtWord: WriteProcessMemory(processhandle, pointer(address), @v, 2, x);
      vtDWord: WriteProcessMemory(processhandle, pointer(address), @v, 4, x);
      vtQWord: WriteProcessMemory(processhandle, pointer(address), @v, 8, x);
      vtSingle: WriteProcessMemory(processhandle, pointer(address), @s, 4, x);
      vtDouble: WriteProcessMemory(processhandle, pointer(address), @d, 8, x);

      vtString: WriteProcessMemory(processhandle, pointer(address), @value[1], length(value), x);
      vtUnicodeString:
      begin
        us:=value;
        WriteProcessMemory(processhandle, pointer(address), @us[1], length(us)*2, x);
      end;

      vtCustom:
      begin
        getmem(ba, customtype.bytesize);
        try
          if ReadProcessMemory(processhandle, pointer(address), ba, customtype.bytesize, x) then
          begin
            if customtype.scriptUsesFloat then
              customtype.ConvertFloatToData(s, ba, address)
            else
              customtype.ConvertIntegerToData(v, ba, address);

            WriteProcessMemory(processhandle, pointer(address), ba, customtype.bytesize, x);
          end;
        finally
          freememandnil(ba);

        end;
      end;
    end;

  end;



end;

function readAndParsePointer(address: ptruint; buf: pbytearray; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false; showAsSigned: boolean=false; bytesize:integer=1): string;
var
    s: pchar;
    ws: PWideChar;
    i: integer;
begin
  result:='???';
  case variableType of
    vtByte:
    begin
      if showashexadecimal then
        result:=inttohex(buf[0],2)
      else
      begin
        if showAsSigned then
          result:=inttostr(shortint(buf[0]))
        else
          result:=inttostr(buf[0]);
      end;
    end;

    vtWord:
    begin
      if showashexadecimal then
        result:=inttohex(pword(@buf[0])^,4)
      else
      begin
        if showAsSigned then
          result:=inttostr(pSmallInt(@buf[0])^)
        else
          result:=inttostr(pword(@buf[0])^);
      end;
    end;

    vtDWord:
    begin
      if showashexadecimal then
        result:=inttohex(pdword(@buf[0])^,8)
      else
      begin
        if showAsSigned then
          result:=inttostr(pinteger(@buf[0])^)
        else
          result:=inttostr(pdword(@buf[0])^);
      end;
    end;

    vtQword:
    begin
      if showashexadecimal then
        result:=inttohex(PQWord(@buf[0])^,8)
      else
      begin
        if showAsSigned then
          result:=inttostr(PInt64(@buf[0])^)
        else
          result:=inttostr(pqword(@buf[0])^);
      end;
    end;

    vtSingle:
    begin
      if showashexadecimal then
        result:=inttohex(pdword(@buf[0])^,8)
      else
        result:=floattostr(psingle(@buf[0])^);
    end;

    vtDouble:
    begin
      if showashexadecimal then
        result:=inttohex(pqword(@buf[0])^,16)
      else
        result:=floattostr(pdouble(@buf[0])^);
    end;

    vtString, vtCodePageString:
    begin
      getmem(s, bytesize+1);
      CopyMemory(s, buf, bytesize);
      s[bytesize]:=#0;

      if variableType=vtCodePageString then
        result:=WinCPToUTF8(s)
      else
        result:=s;
    end;

    vtUnicodeString:
    begin
      getmem(ws, bytesize+2);
      copymemory(ws, buf, bytesize);

      try
        pbytearray(ws)[bytesize+1]:=0;
        pbytearray(ws)[bytesize]:=0;
        result:=utf16toutf8(ws);
      finally
        freememandnil(ws);

      end;
    end;

    vtByteArray:
    begin
      result:='';
      if showashexadecimal then
      begin
        for i:=0 to bytesize-1 do
          result:=result+inttohex(buf[i],2)+' ';
      end
      else
      begin
        for i:=0 to bytesize-1 do
        begin
          if showAsSigned then
            result:=result+IntToStr(shortint(buf[i]))+' '
          else
            result:=result+IntToStr(byte(buf[i]))+' '
        end;
      end;
    end;

    vtCustom:
    begin
      if customtype<>nil then
      begin
        if showashexadecimal and (customtype.scriptUsesFloat=false) then
          result:=inttohex(customtype.ConvertDataToInteger(buf, address),8)
        else
        begin
          if customtype.scriptUsesFloat then
            result:=FloatToStr(customtype.ConvertDataToFloat(buf, address))
          else
            result:=IntToStr(customtype.ConvertDataToInteger(buf, address));
        end;
      end;
    end;
  end;
end;

function readAndParseAddress(address: ptrUint; variableType: TVariableType; customtype: TCustomType=nil; showashexadecimal: Boolean=false; showAsSigned: boolean=false; bytesize:integer=1): string;
var buf: array [0..7] of byte;
    buf2: pbytearray;
    x: ptruint;
begin
  result:='???';
  case variableType of
    vtByte:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],1,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);

    end;

    vtWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],2,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
    end;

    vtDWord:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
    end;

    vtQword:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],8,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
    end;

    vtSingle:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],4,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
    end;

    vtDouble:
    begin
      if ReadProcessMemory(processhandle,pointer(address),@buf[0],8,x) then
        result:=readAndParsePointer(address, @buf[0], variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
    end;

    vtString, vtCodePageString:
    begin
      getmem(buf2, bytesize+1);
      try
        if ReadProcessMemory(processhandle,pointer(address),buf2,bytesize,x) then
          result:=readAndParsePointer(address, buf2, variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
      finally
        freememandnil(buf2);

      end;
    end;

    vtUnicodeString:
    begin
      getmem(buf2, bytesize+2);
      try

        if ReadProcessMemory(processhandle,pointer(address),buf2,bytesize,x) then
          result:=readAndParsePointer(address, buf2, variabletype, customtype, showashexadecimal, showAsSigned, bytesize);


      finally
        freememandnil(buf2);

      end;
    end;



    vtByteArray:
    begin
      getmem(buf2, bytesize);
      try
        if ReadProcessMemory(processhandle,pointer(address),buf2,bytesize,x) then
          result:=readAndParsePointer(address, buf2, variabletype, customtype, showashexadecimal, showAsSigned, bytesize);
      finally
        freememandnil(buf2);

      end;
    end;

    vtCustom:
    begin
      if customtype<>nil then
      begin
        getmem(buf2, customtype.bytesize);
        try
          if ReadProcessMemory(processhandle,pointer(address),buf2,customtype.bytesize,x) then
            result:=readAndParsePointer(address, buf2, variabletype, customtype, showashexadecimal, showAsSigned, bytesize);

        finally
          freememandnil(buf2);

        end;
      end;
    end;
  end;
end;


function DataToString(buf: PByteArray; size: integer; vartype: TVariableType; clean: boolean=false): string;
{note: If type is of string unicode, the last 2 bytes will get set to 0, so watch what you're calling}
var tr: Widestring;
    i: integer;
    a: ptruint;

    tempbuf: pbytearray;
begin
  case vartype of
    vtByte: if clean then result:=inttohex(buf[0],2) else result:=rsBIByte+inttohex(buf[0],2) + '('+inttostr(buf[0])+')';
    vtWord: if clean then result:=inttohex(pword(buf)^,4) else result:=rsBIWord+inttohex(pword(buf)^,4) + '('+inttostr(pword(buf)^)+')';
    vtDword: if clean then result:=inttohex(pdword(buf)^,8) else result:=rsBIDword+inttohex(pdword(buf)^,8) + '('+inttostr(pdword(buf)^)+')';
    vtQword: if clean then result:=inttohex(pqword(buf)^,16) else result:=rsBIQword+inttohex(pqword(buf)^,16) + '('+inttostr(pqword(buf)^)+')';
    vtSingle: if clean then result:=format('%.2f',[psingle(buf)^]) else result:=rsBIFloat+format('%.2f',[psingle(buf)^]);
    vtDouble: if clean then result:=format('%.2f',[pdouble(buf)^]) else result:=rsBIDouble+format('%.2f',[pdouble(buf)^]);
    vtString:
    begin
      getmem(tempbuf,size+1);
      copymemory(tempbuf,buf,size);

      try
        tempbuf[size]:=0;
        result:=pchar(tempbuf);
      finally
        freememandnil(tempbuf);

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
        freememandnil(tempbuf);

      end;
    end;

    vtPointer:
    begin
      if processhandler.is64bit then
        a:=ptruint(pqword(buf)^)
      else
        a:=ptruint(pdword(buf)^);

      if clean then result:='' else result:='(pointer)';

      result:=result+symhandler.getNameFromAddress(a,true,true);

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

function isHumanReadableInteger(v: integer): boolean;
begin
  //check if the value is a human usable value (between 0 and 10000 or dividable by at least 100)

  //Human readable if:
  //The value is in the range of -10000 and 10000
  //The value is dividable by 100

  result:=inrange(v, -10000, 10000) or ((v mod 100)=0);
end;

function FindTypeOfData(address: ptrUint; buf: pbytearray; size: integer; CustomType: PCustomType=nil; FindOption: TFindTypeOptions=[]):TVariableType;
{
takes the given address and memoryblock and converts it to a variable type based on some guesses

if CustomType is not nil it will also evaluate using the provided custom types (if the result is an unreadable dword)
}
var x: string;
    i: integer;
    isstring: boolean;
    e: integer;
    v: qword;
    f: single;

    floathasseperator: boolean;
    couldbestringcounter: boolean;
begin
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);


  //check if it matches a string
  result:=vtDword;

  try

    floathasseperator:=false;

    if (biNoString in FindOption)=false then
    begin
      isstring:=true;
      couldbestringcounter:=true;
      i:=0;
      while i<4 do
      begin
        //check if the first 4 characters match with a standard ascii values (32 to 127)
        if i<size then
        begin
          if (buf[i]<32) or (buf[i]>127) then
          begin
            isstring:=false;
            if i>0 then
              couldbestringcounter:=false;

            if not couldbestringcounter then break;
          end;
        end
        else
        begin
          isstring:=false;
          couldbestringcounter:=false;
        end;

        inc(i);
      end;

      if isstring then
      begin
        result:=vtString;
        exit;
      end;

      if couldbestringcounter and (size>4) and ((buf[4]>=32) or (buf[4]<=127)) then //check if the 4th byte of the 'string' is a char or not
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

    end;




    i:=address mod 4;
    if size in [4,8] then i:=0; //skip this test, it's a known size datablock

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
          val('$'+symhandler.getNameFromAddress(pqword(@buf[0])^,true,true,nil,nil,8,false),v,e)
        else
          e:=0;
      end
      else
      begin
        val('$'+symhandler.getNameFromAddress(pdword(@buf[0])^,true,true,nil,nil,8,false),v,e);
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
        if InRange(psingle(@buf[0])^, -100000.0, 100000.0) then
        begin

          if pos(DefaultFormatSettings.DecimalSeparator,x)>0 then
            floathasseperator:=true;

          result:=vtSingle;

          if (length(x)<=4) or (not floathasseperator) then exit;  //it's a full floating point value or small enough to fit in 3 digits and a seperator (1.01, 1.1 ....)
        end;
      end;
    end;

    if (biNoDouble in FindOption)=false then
    begin
      if (size>=8) then  //check if a double can be used
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

              if Pdword(@buf[0])^<>0 then
              begin
                x:=floattostr(PSingle(@buf[0])^);
                if (pos('E',x)=0) then
                begin
                  //if 4 bytes after this address is a float then override thise double to a single type
                  if FindTypeOfData(address+4, @buf[4], size-4)=vtSingle then
                    result:=vtSingle;
                end;
              end;

              exit;
            end;
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

       // if inrange(pdword(@buf[0])^, $3d000000, $44800000)=false then //if it's not in this range, assume it's a pointer. Otherwise, could be a float
        exit;
      end;
    end;

    //if customtype is not nil check if the dword is humanreadable or not
    if (customtype<>nil) and (result=vtDword) and (isHumanReadableInteger(pdword(@buf[0])^)=false) then
    begin
      //not human readable, see if there is a custom type that IS human readable
      for i:=0 to customTypes.count-1 do
      begin
        if TCustomType(customtypes[i]).scriptUsesFloat then
        begin
          //float check
          f:=TCustomType(customtypes[i]).ConvertDataToFloat(@buf[0], address);
          x:=floattostr(f);

          if (pos('E',x)=0) and (f<>0) and InRange(f, -100000.0, 100000.0) then
          begin
            result:=vtCustom;
            CustomType^:=customtypes[i];

            if (pos(DefaultFormatSettings.DecimalSeparator,x)=0) then
              break; //found one that has no decimal seperator

          end;
        end
        else
        begin
          //dword check
          if isHumanReadableInteger(TCustomType(customtypes[i]).ConvertDataToInteger(@buf[0], address)) then
          begin
            result:=vtCustom;
            CustomType^:=customtypes[i];
            break;
          end;
        end;
      end;
    end;

  finally
    if assigned(onAutoGuessRoutine) then
      result:=onAutoGuessRoutine(address, result);

  end;
end;

end.
