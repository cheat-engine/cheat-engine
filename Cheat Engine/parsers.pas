// Copyright Cheat Engine. All Rights Reserved.

unit Parsers;
{General parsers}


{$mode delphi}

interface

uses
  Classes, SysUtils, strutils, commonTypeDefs, math;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes; canHandleNibbleWildcards: boolean=false);
function BinToInt(s: string): int64;
function IntToBin(i: qword): string;
function StrToFloatEx(s: string): double;
function StrToQWordEx(s: string): qword;
function getbit(bitnr: integer; bt: qword):integer; inline;
procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;
procedure setbit(bitnr: integer; var bt: qword;state:integer); overload;

function GetBitCount(value: qword): integer;

function ConvertHexStrToRealStr(const s: string): string;
function HexStrToInt(const S: string): Integer;
function HexStrToInt64(const S: string): Int64;

function IntToHexSigned(v: INT64; digits: integer): string;

procedure getRegisterListFromParams(params: string; registerlist: Tstrings);


implementation

uses windows, symbolhandler, assemblerunit;

resourcestring
   rsInvalidInteger = 'Invalid integer';

procedure getRegisterListFromParams(params: string; registerlist: Tstrings);
{
//returns RAX, RBX, even for EAX,EBX regs
}
var
  tokens: TTokens;
  i: integer;
  isrnumber: boolean;
  seplist: TSysCharSet;
begin
  //check for each known base register and add it to the registerlist
  params:=uppercase(params);

  seplist:=[' ',',','[',']','+','-'];
  setlength(tokens, WordCount(params,seplist));
  for i:=0 to length(tokens)-1 do
    tokens[i]:=ExtractWord(i+1,params,seplist);

  //if assembler_tokenizer(params, tokens) then
  begin
    for i:=0 to length(tokens)-1 do
    begin
      if length(tokens[i])>=2 then
      case tokens[i][1] of
        'E','R':
        begin
          if length(tokens[i])>=3 then
          begin
            isrnumber:=true;
            case tokens[i][2] of
              '1':
              case tokens[i][3] of
                '0': registerlist.add('R10');
                '1': registerlist.add('R11');
                '2': registerlist.add('R12');
                '3': registerlist.add('R13');
                '4': registerlist.add('R14');
                '5': registerlist.add('R15');
                else
                  isrnumber:=false;
              end;
              else
                isrnumber:=false;
            end;

            if isrnumber then continue;
          end;

          case length(tokens[i]) of
            2:
            begin
              case tokens[i][2] of
                '8': registerlist.add('R8');
                '9': registerlist.add('R9');
              end;
            end;
            3:
            begin
              case tokens[i][3] of
                'X':
                case tokens[i][2] of
                  'A': registerlist.add('RAX');
                  'B': registerlist.add('RBX');
                  'C': registerlist.add('RCX');
                  'D': registerlist.add('RDX');
                end;

                'I':
                case tokens[i][2] of
                  'D': registerlist.add('RDI');
                  'S': registerlist.add('RSI');
                end;

                'P':
                case tokens[i][2] of
                  'B': registerlist.add('RBP');
                  'S': registerlist.add('RSP');
                  'I': registerlist.add('RIP');
                end;
              end;
            end;
          end;
        end;

        'A':
        begin
          case tokens[i][2] of
            'X','H','L': registerlist.add('RAX');
          end;
        end;

        'B':
        begin
          case tokens[i][2] of
            'X','H','L': registerlist.add('RBX');
            'P': registerlist.add('RBP');
          end;
        end;

        'C':
        begin
          case tokens[i][2] of
            'X','H','L': registerlist.add('RCX');
          end;
        end;

        'D':
        begin
          case tokens[i][2] of
            'X','H','L': registerlist.add('RDX');
            'I': registerlist.add('RDI');
          end;
        end;

        'S':
        begin
          case tokens[i][2] of
            'I': registerlist.add('RSI');
            'P': registerlist.add('RSP');
          end;
        end;

      end;


    end;
  end;
end;

function GetBitCount(value: qword): integer;
begin
  result:=0;
  while value>0 do
  begin
    if (value mod 2)=1 then inc(result);
    value:=value shr 1;
  end;
end;

function getbit(bitnr: integer; bt: qword):integer; inline;
begin
  result:=(bt shr bitnr) and 1;
end;

procedure setbit(bitnr: integer; var bt: qword;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
begin
  bt:=bt and (not (1 shl bitnr));
  bt:=bt or (state shl bitnr);
end;

procedure setbit(bitnr: integer; var bt: dword;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
begin
  bt:=bt and (not (1 shl bitnr));
  bt:=bt or (state shl bitnr);
end;

procedure setbit(bitnr: integer; var bt: Byte;state:integer); overload;
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
var d: dword;
begin
  d:=bt;
  setbit(bitnr,d,state);
  bt:=d;
end;

function StrToFloatEx(s: string): double;
var fs: TFormatSettings;
begin
  try
    fs:=DefaultFormatSettings;
    result:=StrToFloat(s, fs);
  except
    if fs.DecimalSeparator='.' then
      fs.DecimalSeparator:=','
    else
      fs.DecimalSeparator:='.';

    result:=StrToFloat(s, fs);
  end;
end;

function StrToQWordEx(s: string): qword;
{
This routine will use StrToQword unless it is a negative value, in which case it will use StrToInt64
}
begin
  s:=trim(s);
  if length(s)=0 then
    raise EParserError.create(rsInvalidInteger)
  else
  begin
    try
      if s[1]='-' then
        result:=StrToInt64(s)
      else
        result:=StrToQWord(s);
    except
      result:=trunc(StrToFloatEx(s)); //in case the user decided to give a float.
    end;
  end;
end;

function BinToInt(s: string): int64;
var i: integer;
begin
  result:=0;
  for i:=length(s) downto 1 do
    if s[i]='1' then result:=result+trunc(power(2,length(s)-i ));
end;

function IntToBin(i: qword): string;
var temp,temp2: string;
    j: integer;
begin
  temp:='';
  while i>0 do
  begin
    if (i mod 2)>0 then temp:=temp+'1'
                   else temp:=temp+'0';
    i:=i div 2;
  end;

  temp2:='';
  for j:=length(temp) downto 1 do
    temp2:=temp2+temp[j];
  result:=temp2;
end;



procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes; canHandleNibbleWildcards: boolean=false);
{
Converts a given string into a array of TBytes.
TBytes are not pure bytes, they can hold -1, which indicates a wildcard
}
var i,j,k: integer;
    helpstr,helpstr2:string;
    delims: TSysCharSet;
begin
  setlength(bytes,0);
  if length(scanvalue)=0 then exit;

  delims:=[' ',',','-']; //[#0..#255] - ['a'..'f','A'..'F','1'..'9','0','*']; //everything except hexadecimal and wildcard

  scanvalue:=trim(scanvalue);


  for i:=1 to WordCount(scanvalue, delims) do
  begin
    helpstr:=ExtractWord(i, scanvalue, delims);

    if helpstr<>'' then
    begin
      if not hex then
      begin
        setlength(bytes,length(bytes)+1);
        try
          bytes[length(bytes)-1]:=strtoint(helpstr);
        except
          bytes[length(bytes)-1]:=-1; //wildcard
        end;
      end
      else
      begin
        j:=1;
        while j<=length(helpstr) do
        begin
          helpstr2:=copy(helpstr, j,2);
          setlength(bytes,length(bytes)+1);
          try
            bytes[length(bytes)-1]:=strtoint('$'+helpstr2);
          except
            bytes[length(bytes)-1]:=-1; //wildcard

            if canHandleNibbleWildcards and (length(helpstr2)=2) then
            begin
              //see if it can be salvaged into nibble information
              //how it's stored:
              //the most significant bit is set (so negative) as an indicator it's wildcard

              //bits 8 to 11 contains a mask for the nibble ranging from bit 0 to 3
              //bits 12 to 15 contain a mask for nibble part ranging from bit 4 to 7

              //so: 7* will be $80000000 or f000 or 70=$8000f070
              //when compared to another value e.g 79  79 gets AND'ed with f0, leaving 70, which then matches 70

              try
                k:=strtoint('$'+helpstr2[1]); // (x*) bit 0 to 3 are wildcard
                bytes[length(bytes)-1]:=dword($80000000) or ($0000f000) or (k shl 4);
              except
                try
                  k:=strtoint('$'+helpstr2[2]); //(*x) bit 4 to 7 are wildcard
                  bytes[length(bytes)-1]:=dword($80000000) or ($00000f00) or k;
                except
                end;
              end;

            end;

          end;

          inc(j,2);
        end;
      end;

    end;
  end;
end;


function ConvertHexStrToRealStr(const s: string): string;
{
Converts a string meant to be a hexadeimcal string to the real way delphi reads
it
e.g:
123 > $123
-123 > -$123
+123 > +$123
#123 > 123
+#123 > +123
}
var ishex: string;
    start: integer;
    i,j,k: integer;

    bytes: string;
    t: string;
    q: qword;
    f: single;
    d: double;
    err: boolean;
begin
  if s='' then exit('');

  start:=1;

  ishex:='$';
  for i:=start to length(s) do
    case s[i] of
      {'[':
      begin
        err:=false;
        result:='$'+inttohex(symhandler.GetAddressFromPointer(s,err),8);
        if err then
          exit('')
        else
          exit;
      end;  }


      '''' , '"' :
      begin
        //char
        if (i+2)<=length(s) then
        begin
          bytes:='';
          for j:=i+2 to length(s) do
            if s[j] in ['''','"'] then
            begin
              bytes:=copy(s,i+1,j-(i+1));

              //implement a bug of 6.6 that allowed a pointer to be written as a string

              err:=false;
              result:='$'+inttohex(symhandler.GetAddressFromPointer(bytes,err),8);
              if err then
              begin
                result:='$';
                for k:=length(bytes) downto 1 do
                  result:=result+inttohex(byte(bytes[k]),2);
              end;

              //result := '$'+inttohex(byte(s[i+1]),2);
              exit; //this is it, no further process required, or appreciated...

            end;



        end;
      end;

      '#' :
      begin
        ishex:='';
        start:=2;
        break;
      end;

      '(' :
      begin
        if copy(s,1,5)='(INT)' then
        begin
          t:=copy(s,6);
          try
            q:=StrToQWordEx(t);
            result:='$'+inttohex(q,8);
            exit;
          except
          end;
        end;

        if copy(s,1,8)='(DOUBLE)' then
        begin
          t:=copy(s,9);
          val(t, d,j);
          if j=0 then
          begin
            result:='$'+inttohex(PINT64(@d)^,8);

            if s[1]='-' then
              result:='-'+result;

            if s[1]='+' then
              result:='+'+result;

            exit;
          end;
        end;

        if copy(s,1,11)='(DOUBLE32L)' then
        begin
          t:=copy(s,12);
          val(t, d,j);
          if j=0 then
          begin
            q:=PINT64(@d)^ and $ffffffff;
            result:='$'+inttohex(q,8);
            exit;
          end;
        end;

        if copy(s,1,11)='(DOUBLE32H)' then
        begin
          t:=copy(s,12);
          val(t, d,j);
          if j=0 then
          begin
            q:=PINT64(@d)^ shr 32;
            result:='$'+inttohex(q,8);
            exit;
          end;
        end;

        if copy(s,1,7)='(FLOAT)' then
        begin
          t:=copy(s,8);
          val(t, f,j);
          if j=0 then
          begin
            result:='$'+inttohex(pdword(@f)^,8);

            if s[1]='-' then
              result:='-'+result;

            if s[1]='+' then
              result:='+'+result;

            exit;
          end;
        end;
      end;

      '0'..'9', 'a'..'f', 'A'..'F','-','+':
      begin
        //ok
      end
      else
        exit('');
    end;


  if s[1]='-' then
  begin
    result:='-'+ishex+copy(s,start+1)
  end
  else
  if s[1]='+' then
  begin
    result:='+'+ishex+copy(s,start+1);
  end
  else
  begin
    result:=ishex+copy(s,start);
  end;
end;

function IntToHexSigned(v: INT64; digits: integer): string;
begin
  if v>=0 then
    result:=inttohex(v, digits)
  else
    result:='-'+inttohex(-v, digits);
end;

function HexStrToInt(const S: string): Integer;
begin
  result:=StrToint(ConvertHexStrToRealStr(s));
end;

function HexStrToInt64(const S: string): Int64;
begin
  result:=StrToQWordEx(ConvertHexStrToRealStr(s));
end;

end.

