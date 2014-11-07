unit Parsers;
{General parsers}


{$mode delphi}

interface

uses
  Classes, SysUtils, strutils, commonTypeDefs, math;

procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
function BinToInt(s: string): int64;
function IntToBin(i: qword): string;
function StrToQWordEx(s: string): qword;

implementation

resourcestring
   rsInvalidInteger = 'Invalid integer';

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
    if s[1]='-' then
      result:=StrToInt64(s)
    else
      result:=StrToQWord(s);
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



procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
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
          end;

          inc(j,2);
        end;
      end;

    end;
  end;
end;

end.

