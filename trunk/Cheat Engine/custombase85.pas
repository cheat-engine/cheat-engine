unit CustomBase85;


// uses 85 ascii characters which are better to use in XML ( no &'<"\> chars and no space char = more efficient )
//
// Base85ToBin doesn't support white spaces and line breaks (HexToBin doesn't support it too)

// implementation by mgr.inz.Player http://forum.cheatengine.org/viewtopic.php?t=560886 and http://pastebin.com/EUfGFpZ9

{$mode delphi}

interface

uses
  Classes;

procedure BinToBase85(BinValue, outputStringBase85: PChar; BinBufSize: integer);
//
//  example:
//    getmem(outputstring, (BinarySize div 4) * 5 + 5 );
//    BinToBase85(b,outputstring,BinarySize);
//
//  it adds a 0 terminator to outputstring

function Base85ToBin(inputStringBase85, BinValue: PChar): integer;
//
//  example:
//    size:=length(inputstring);
//    if (size mod 5) > 1 then
//      BinarySize:= (size div 5) * 4 + (size mod 5) - 1
//    else
//      BinarySize:= (size div 5) * 4;
//   getmem(b, BinarySize);
//   Base85ToBin(inputstring, b);
//
//  Base85ToBin doesn't support: white space between the characters, line breaks. (HexToBin doesn't support it too)
//  Base85ToBin doesn't check for data corruption.

implementation

const
  customBase85='0123456789'+
                'ABCDEFGHIJKLMNOPQRSTUVWXYZ'+
                'abcdefghijklmnopqrstuvwxyz'+
                '!#$%()*+,-./:;=?@[]^_{}';

procedure BinToBase85(BinValue, outputStringBase85: PChar; BinBufSize: integer);
var
  i : integer;
  a : dword;
begin

  i:=0;
  while i<binbufsize do
  begin

    //first byte ( from 4-tuple )
    a:=pbyte(BinValue+i)^ shl 24;
    inc(i);

    if i<binbufsize then
    begin
      //second byte
      a:=a or pbyte(BinValue+i)^ shl 16;
      inc(i);
    end;

    if i<binbufsize then
    begin
      //third byte
      a:=a or pbyte(BinValue+i)^ shl 8;
      inc(i);
    end;

    if i<binbufsize then
    begin
      //fourth byte
      a:=a or pbyte(BinValue+i)^;
      inc(i);
    end;

    outputStringBase85[4]:= customBase85[a mod 85 + 1]; a:= a div 85;
    outputStringBase85[3]:= customBase85[a mod 85 + 1]; a:= a div 85;
    outputStringBase85[2]:= customBase85[a mod 85 + 1]; a:= a div 85;
    outputStringBase85[1]:= customBase85[a mod 85 + 1]; a:= a div 85;
    outputStringBase85[0]:= customBase85[a mod 85 + 1];
    inc(outputStringBase85,5);

  end;

  //add zero terminator at right place
  a:= (4 - (BinBufSize mod 4)) mod 4;
  dec(outputStringBase85,a);
  outputStringBase85[0]:=#0;
end;

function Base85ToBin(inputStringBase85, BinValue: PChar): integer;
var i,j: integer;
    size : integer;
    a : dword;
begin

  size:=length(inputStringBase85);

  i:=0;
  j:=0;
  while i<size do
  begin
    a:=( pos((inputStringBase85+i)^, customBase85) - 1 )*85*85*85*85;
    inc(i);

    if i<size then
    begin
      a:= a + ( pos((inputStringBase85+i)^, customBase85) - 1 )*85*85*85;
      inc(i);
    end;

    if i<size then
    begin
      a:= a + ( pos((inputStringBase85+i)^, customBase85) - 1 )*85*85;
      inc(i);
    end;

    if i<size then
    begin
      a:= a + ( pos((inputStringBase85+i)^, customBase85) - 1 )*85;
      inc(i);
    end;

    if i<size then
    begin
      a:= a + ( pos((inputStringBase85+i)^, customBase85) - 1 );
      inc(i);

      // 5-tuple
      binvalue[j+0]:= char(  (a shr 24) and $ff  );
      binvalue[j+1]:= char(  (a shr 16) and $ff  );
      binvalue[j+2]:= char(  (a shr  8) and $ff  );
      binvalue[j+3]:= char(  a and $ff           );
      inc(j,4);
    end;
  end;


  case (size mod 5) of
    2: begin // must be padded with three digits (last radix85 digit used)
         a:= a + 84*85*85 + 84*85 + 84;
         binvalue[j+0]:= char(  (a shr 24) and $ff  ); // last three bytes of the output are ignored
         inc(j);
       end;

    3: begin // must be padded with two digits (last radix85 digit used)
         a:= a            + 84*85 + 84;
         binvalue[j+0]:= char(  (a shr 24) and $ff  );
         binvalue[j+1]:= char(  (a shr 16) and $ff  ); // last two bytes of the output are ignored
         inc(j,2);
       end;

    4: begin // must be padded with one digit (last radix85 digit used)
         a:= a                    + 84;
         binvalue[j+0]:= char(  (a shr 24) and $ff  );
         binvalue[j+1]:= char(  (a shr 16) and $ff  );
         binvalue[j+2]:= char(  (a shr  8) and $ff  ); // last byte of the output is ignored
         inc(j,3);
       end;
  end;

  result:=j;

end;

end.

