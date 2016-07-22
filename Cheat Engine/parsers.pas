unit Parsers;
{General parsers}


{$mode delphi}

interface

uses
  Classes, SysUtils, strutils, commonTypeDefs, math;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes; canHandleNibbleWildcards: boolean=false);
function BinToInt(s: string): int64;
function IntToBin(i: qword): string;
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

implementation

resourcestring
   rsInvalidInteger = 'Invalid integer';

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
    raise exception.create(rsInvalidInteger)
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
begin
  if s='' then exit('');

  start:=1;

  ishex:='$';
  for i:=start to length(s) do
    case s[i] of
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

              result:='$';
              for k:=length(bytes) downto 1 do
                result:=result+inttohex(byte(bytes[k]),2);

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
          t:=copy(s,6,length(s));
          try
            q:=StrToQWordEx(t);
            result:='$'+inttohex(q,8);
            exit;
          except
          end;
        end;

        if copy(s,1,8)='(DOUBLE)' then
        begin
          t:=copy(s,9,length(s));
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

        if copy(s,1,7)='(FLOAT)' then
        begin
          t:=copy(s,8,length(s));
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
    result:='-'+ishex+copy(s,start+1,length(s))
  end
  else
  if s[1]='+' then
  begin
    result:='+'+ishex+copy(s,start+1,length(s));
  end
  else
  begin
    result:=ishex+copy(s,start,length(s));
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

