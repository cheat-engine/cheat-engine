{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclMime.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Ralf Junker.                                       }
{ Portions created by Ralf Junker are Copyright (C) Ralf Junker. All rights reserved.              }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Marcel van Brakel                                                                              }
{   Ralf Junker                                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   edbored                                                                                        }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Lightning fast Mime (Base64) Encoding and Decoding routines. Coded by Ralf Junker                }
{ (ralfjunker att gmx dott de).                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{ Migration Guide from JCL 1.90 and older:                                                         }
{                                                                                                  }
{ These new functions now support line breaks (CRLF) as required by RFC 2045.                      }
{ Inserting line breaks is the default behaviour in RFC 2045 therefor the encoding functions now   }
{ encode with line breaks.                                                                         }
{                                                                                                  }
{ This may require changes to your code:                                                           }
{ Encoding without inserting line breaks is possible using the corresponding NoCRLF procedures:    }
{                                                                                                  }
{ MimeEncode => MimeEncodeNoCRLF                                                                   }
{ MimeEncodeString => MimeEncodeStringNoCRLF                                                       }
{ MimeEncodeStream => MimeEncodeStreamNoCRLF                                                       }
{ MimeEncodedSize => MimeEncodedSizeNoCRLF                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclMime;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF CLR}
  System.Text,
  {$ENDIF CLR}
  SysUtils, Classes,
  JclBase;

function MimeEncodeString(const S: AnsiString): AnsiString;
function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
function MimeDecodeString(const S: AnsiString): AnsiString;
function MimeEncodedSize(const InputSize: Cardinal): Cardinal;
function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
function MimeDecodedSize(const InputSize: Cardinal): Cardinal;
procedure DecodeHttpBasicAuthentication(const BasicCredentials: string;
  out UserId, PassWord: string);
{$IFDEF CLR}
procedure MimeEncode(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal = 0); overload;
procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal = 0); overload;
procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal = 0); overload;
function MimeDecode(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal = 0): Cardinal; overload;
function MimeDecodePartial(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal; overload;
function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; OutputOffset: Cardinal;
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal; overload;

procedure MimeEncode(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray); overload;
procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray); overload;
procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray); overload;
function MimeDecode(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray): Cardinal; overload;
function MimeDecodePartial(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal; overload;
function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal; overload;

{$ELSE}
procedure MimeEncode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
function MimeDecode(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer): Cardinal;
function MimeDecodePartial(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
function MimeDecodePartialEnd(out OutputBuffer; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;
{$ENDIF CLR}
procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);

const
  MIME_ENCODED_LINE_BREAK = 76;
  MIME_DECODED_LINE_BREAK = MIME_ENCODED_LINE_BREAK div 4 * 3;
  MIME_BUFFER_SIZE = MIME_DECODED_LINE_BREAK * 3 * 4 * 4;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclMime.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

// Caution: For MimeEncodeStream and all other kinds of multi-buffered
// Mime encodings (i.e. Files etc.), BufferSize must be set to a multiple of 3.
// Even though the implementation of the Mime decoding routines below
// do not require a particular buffer size, they work fastest with sizes of
// multiples of four. The chosen size is a multiple of 3 and of 4 as well.
// The following numbers are, in addition, also divisible by 1024:
// $2400, $3000, $3C00, $4800, $5400, $6000, $6C00.

const
  BUFFER_SIZE = $3000;
  EqualSign = Byte('=');

const
  { The mime encoding table. Do not alter. }
  MIME_ENCODE_TABLE: array [0..63] of Byte = (
    065, 066, 067, 068, 069, 070, 071, 072, //  00 - 07
    073, 074, 075, 076, 077, 078, 079, 080, //  08 - 15
    081, 082, 083, 084, 085, 086, 087, 088, //  16 - 23
    089, 090, 097, 098, 099, 100, 101, 102, //  24 - 31
    103, 104, 105, 106, 107, 108, 109, 110, //  32 - 39
    111, 112, 113, 114, 115, 116, 117, 118, //  40 - 47
    119, 120, 121, 122, 048, 049, 050, 051, //  48 - 55
    052, 053, 054, 055, 056, 057, 043, 047); // 56 - 63

  MIME_PAD_CHAR = Byte('=');

  MIME_DECODE_TABLE: array [Byte] of Cardinal = (
    255, 255, 255, 255, 255, 255, 255, 255, //   0 -   7
    255, 255, 255, 255, 255, 255, 255, 255, //   8 -  15
    255, 255, 255, 255, 255, 255, 255, 255, //  16 -  23
    255, 255, 255, 255, 255, 255, 255, 255, //  24 -  31
    255, 255, 255, 255, 255, 255, 255, 255, //  32 -  39
    255, 255, 255, 062, 255, 255, 255, 063, //  40 -  47
    052, 053, 054, 055, 056, 057, 058, 059, //  48 -  55
    060, 061, 255, 255, 255, 255, 255, 255, //  56 -  63
    255, 000, 001, 002, 003, 004, 005, 006, //  64 -  71
    007, 008, 009, 010, 011, 012, 013, 014, //  72 -  79
    015, 016, 017, 018, 019, 020, 021, 022, //  80 -  87
    023, 024, 025, 255, 255, 255, 255, 255, //  88 -  95
    255, 026, 027, 028, 029, 030, 031, 032, //  96 - 103
    033, 034, 035, 036, 037, 038, 039, 040, // 104 - 111
    041, 042, 043, 044, 045, 046, 047, 048, // 112 - 119
    049, 050, 051, 255, 255, 255, 255, 255, // 120 - 127
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255);

{$IFDEF CLR}
procedure MimeEncode(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncodeNoCRLF(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray);
begin
  MimeEncodeFullLines(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

function MimeDecode(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray): Cardinal;
begin
  Result := MimeDecode(InputBuffer, 0, InputByteCount, OutputBuffer, 0);
end;

function MimeDecodePartial(const InputBuffer: TDynByteArray; const InputByteCount: Cardinal;
  out OutputBuffer: TDynByteArray; var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
begin
  Result := MimeDecodePartial(InputBuffer, 0, InputByteCount, OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;

function MimeDecodePartialEnd(out OutputBuffer: TDynByteArray; const ByteBuffer: Cardinal;
  const ByteBufferSpace: Cardinal): Cardinal;
begin
  Result := MimeDecodePartialEnd(OutputBuffer, 0, ByteBuffer, ByteBufferSpace);
end;
{$ELSE}
type
  PByte4 = ^TByte4;
  TByte4 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
    B4: Byte;
  end;

  PByte3 = ^TByte3;
  TByte3 = packed record
    B1: Byte;
    B2: Byte;
    B3: Byte;
  end;
{$ENDIF CLR}

// Wrapper functions & procedures
function MimeEncodeString(const S: AnsiString): AnsiString;
var
  L: Cardinal;
  {$IFDEF CLR}
  Bytes: TDynByteArray;
  {$ENDIF CLR}
begin
  if S <> '' then
  begin
    {$IFDEF CLR}
    L := Length(S);
    SetLength(Bytes, MimeEncodedSize(L));
    MimeEncode(BytesOf(S), 0, L, Bytes, 0);
    Result := Bytes;
    {$ELSE}
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, MimeEncodedSize(L));
    MimeEncode(Pointer(S)^, L, Pointer(Result)^);
    {$ENDIF CLR}
  end
  else
    Result := '';
end;

function MimeEncodeStringNoCRLF(const S: AnsiString): AnsiString;
var
  L: Cardinal;
  {$IFDEF CLR}
  Bytes: TDynByteArray;
  {$ENDIF CLR}
begin
  if S <> '' then
  begin
    {$IFDEF CLR}
    L := Length(S);
    SetLength(Bytes, MimeEncodedSizeNoCRLF(L));
    MimeEncodeNoCRLF(BytesOf(S), 0, L, Bytes, 0);
    Result := Bytes;
    {$ELSE}
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, MimeEncodedSizeNoCRLF(L));
    MimeEncodeNoCRLF(Pointer(S)^, L, Pointer(Result)^);
    {$ENDIF CLR}
  end
  else
    Result := '';
end;

function MimeDecodeString(const S: AnsiString): AnsiString;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  L: Cardinal;
  {$IFDEF CLR}
  Bytes: TDynByteArray;
  {$ENDIF CLR}
begin
  if S <> '' then
  begin
    {$IFDEF CLR}
    L := Length(S);
    SetLength(Bytes, MimeEncodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := MimeDecodePartial(BytesOf(S), 0, L, Bytes, 0, ByteBuffer, ByteBufferSpace);
    Inc(L, MimeDecodePartialEnd(Bytes, 0 + L, ByteBuffer, ByteBufferSpace));
    SetLength(Bytes, L);
    Result := Bytes;
    {$ELSE}
    L := PCardinal(Cardinal(S) - 4)^;
    SetLength(Result, MimeDecodedSize(L));
    ByteBuffer := 0;
    ByteBufferSpace := 4;
    L := MimeDecodePartial(Pointer(S)^, L, Pointer(Result)^, ByteBuffer, ByteBufferSpace);
    Inc(L, MimeDecodePartialEnd(Pointer(Cardinal(Result) + L)^, ByteBuffer, ByteBufferSpace));
    SetLength(Result, L);
    {$ENDIF CLR}
  end
  else
    Result := '';
end;

procedure DecodeHttpBasicAuthentication(const BasicCredentials: string; out UserId, PassWord: string);
const
  LBasic = 6; { Length ('Basic ') }
{$IFDEF CLR}
var
  Index: Cardinal;
  Decoded: TDynByteArray;
  I, L: Cardinal;
begin
  UserId := '';
  PassWord := '';
  L := Length(BasicCredentials);
  if L < LBasic then // includes "L = 0"
    Exit;
  Dec(L, LBasic);
  Index := LBasic;

  SetLength(Decoded, MimeDecodedSize(L));
  L := MimeDecode(BytesOf(BasicCredentials), Index, L, Decoded, 0);

  { Look for colon (':'). }
  I := 0;
  while (L > 0) and (Char(Decoded[I]) <> ':') do
  begin
    Inc(I);
    Dec(L);
  end;

  { Store UserId and Password. }
  UserId := Copy(Decoded, 0, I);
  if L > 1 then
    PassWord := Copy(Decoded, I + 1, L - 1)
  else
    PassWord := '';
end;
{$ELSE}
var
  DecodedPtr, P: PAnsiChar;
  I, L: Cardinal;
begin
  UserId := '';
  PassWord := '';

  P := Pointer(BasicCredentials);
  if P = nil then
    Exit;

  L := Cardinal(Pointer(P - 4)^);
  if L <= LBasic then
    Exit;

  Dec(L, LBasic);
  Inc(P, LBasic);

  GetMem(DecodedPtr, MimeDecodedSize(L));
  L := MimeDecode(P^, L, DecodedPtr^);

  { Look for colon (':'). }
  I := 0;
  P := DecodedPtr;
  while (L > 0) and (P[I] <> ':') do
  begin
    Inc(I);
    Dec(L);
  end;

  { Store UserId and Password. }
  SetString(UserId, DecodedPtr, I);
  if L > 1 then
    SetString(PassWord, DecodedPtr + I + 1, L - 1)
  else
    PassWord := '';

  FreeMem(DecodedPtr);
end;
{$ENDIF CLR}

// Helper functions
function MimeEncodedSize(const InputSize: Cardinal): Cardinal;
begin
  if InputSize > 0 then
    Result := (InputSize + 2) div 3 * 4 + (InputSize - 1) div MIME_DECODED_LINE_BREAK * 2
  else
    Result := InputSize;
end;

function MimeEncodedSizeNoCRLF(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 2) div 3 * 4;
end;

function MimeDecodedSize(const InputSize: Cardinal): Cardinal;
begin
  Result := (InputSize + 3) div 4 * 3;
end;


// Primary functions & procedures
procedure MimeEncode(const InputBuffer {$IFDEF CLR}: TDynByteArray; InputOffset: Cardinal {$ENDIF CLR};
  const InputByteCount: Cardinal;
  out OutputBuffer {$IFDEF CLR}: TDynByteArray; OutputOffset: Cardinal {$ENDIF CLR});
var
  IDelta, ODelta: Cardinal;
begin
  {$IFDEF CLR}
  MimeEncodeFullLines(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset);
  {$ELSE}
  MimeEncodeFullLines(InputBuffer, InputByteCount, OutputBuffer);
  {$ENDIF CLR}
  IDelta := InputByteCount div MIME_DECODED_LINE_BREAK; // Number of lines processed so far.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  {$IFDEF CLR}
  MimeEncodeNoCRLF(InputBuffer, InputOffset + IDelta, InputByteCount - IDelta, OutputBuffer, OutputOffset + ODelta);
  {$ELSE}
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, InputByteCount - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);
  {$ENDIF CLR}
end;

{$IFDEF CLR}
procedure MimeEncodeFullLines(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InIndex: Cardinal;
  OutIndex: Cardinal;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  InnerLimit := InIndex;
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := InIndex;
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat
    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InputBuffer[InIndex + 0];
      B := B shl 8;
      B := B or InputBuffer[InIndex + 1];
      B := B shl 8;
      B := B or InputBuffer[InIndex + 2];
      Inc(InIndex, 3);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutputBuffer[OutIndex + 3] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
      Inc(OutIndex, 3);
    until InIndex >= InnerLimit;

    { Write line break (CRLF). }
    OutputBuffer[OutIndex + 0] := 13;
    OutputBuffer[OutIndex + 1] := 10;
    Inc(OutIndex, 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;
{$ELSE}
procedure MimeEncodeFullLines(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  { Do we have enough input to encode a full line? }
  if InputByteCount < MIME_DECODED_LINE_BREAK then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, MIME_DECODED_LINE_BREAK);

  OuterLimit := Cardinal(InPtr);
  Inc(OuterLimit, InputByteCount);

  { Multiple line loop. }
  repeat
    { Single line loop. }
    repeat
      { Read 3 bytes from InputBuffer. }
      B := InPtr^.B1;
      B := B shl 8;
      B := B or InPtr^.B2;
      B := B shl 8;
      B := B or InPtr^.B3;
      Inc(InPtr);
      { Write 4 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
      B := B shr 6;
      OutPtr^.B1 := MIME_ENCODE_TABLE[B];
      Inc(OutPtr);
    until Cardinal(InPtr) >= InnerLimit;

    { Write line break (CRLF). }
    OutPtr^.B1 := 13;
    OutPtr^.B2 := 10;
    Inc(Cardinal(OutPtr), 2);

    Inc(InnerLimit, MIME_DECODED_LINE_BREAK);
  until InnerLimit > OuterLimit;
end;
{$ENDIF CLR}

{$IFDEF CLR}
procedure MimeEncodeNoCRLF(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InIndex: Cardinal;
  OutIndex: Cardinal;
begin
  if InputByteCount = 0 then
    Exit;

  InIndex := InputOffset;
  OutIndex := OutputOffset;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := InIndex;
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while InIndex < InnerLimit do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InputBuffer[InIndex + 0];
    B := B shl 8;
    B := B or InputBuffer[InIndex + 1];
    B := B shl 8;
    B := B or InputBuffer[InIndex + 2];
    Inc(InIndex, 3);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutputBuffer[OutIndex + 3] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
    Inc(OutIndex, 3);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 4;
        OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 2] := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutputBuffer[OutIndex + 3] := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InputBuffer[InIndex + 0];
        B := B shl 8;
        B := B or InputBuffer[InIndex + 1];
        B := B shl 2;
        OutputBuffer[OutIndex + 2] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 1] := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutputBuffer[OutIndex + 0] := MIME_ENCODE_TABLE[B];
        OutputBuffer[OutIndex + 3] := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;
{$ELSE}
procedure MimeEncodeNoCRLF(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer);
var
  B, InnerLimit, OuterLimit: Cardinal;
  InPtr: PByte3;
  OutPtr: PByte4;
begin
  if InputByteCount = 0 then
    Exit;

  InPtr := @InputBuffer;
  OutPtr := @OutputBuffer;

  OuterLimit := InputByteCount div 3 * 3;

  InnerLimit := Cardinal(InPtr);
  Inc(InnerLimit, OuterLimit);

  { Last line loop. }
  while Cardinal(InPtr) < InnerLimit do
  begin
    { Read 3 bytes from InputBuffer. }
    B := InPtr^.B1;
    B := B shl 8;
    B := B or InPtr^.B2;
    B := B shl 8;
    B := B or InPtr^.B3;
    Inc(InPtr);
    { Write 4 bytes to OutputBuffer (in reverse order). }
    OutPtr^.B4 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B3 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B2 := MIME_ENCODE_TABLE[B and $3F];
    B := B shr 6;
    OutPtr^.B1 := MIME_ENCODE_TABLE[B];
    Inc(OutPtr);
  end;

  { End of data & padding. }
  case InputByteCount - OuterLimit of
    1:
      begin
        B := InPtr^.B1;
        B := B shl 4;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B3 := MIME_PAD_CHAR; { Pad remaining 2 bytes. }
        OutPtr.B4 := MIME_PAD_CHAR;
      end;
    2:
      begin
        B := InPtr^.B1;
        B := B shl 8;
        B := B or InPtr^.B2;
        B := B shl 2;
        OutPtr.B3 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B2 := MIME_ENCODE_TABLE[B and $3F];
        B := B shr 6;
        OutPtr.B1 := MIME_ENCODE_TABLE[B];
        OutPtr.B4 := MIME_PAD_CHAR; { Pad remaining byte. }
      end;
  end;
end;
{$ENDIF CLR}

// Decoding Core
function MimeDecode(const InputBuffer {$IFDEF CLR}: TDynByteArray; InputOffset: Cardinal {$ENDIF CLR};
  const InputByteCount: Cardinal; out OutputBuffer {$IFDEF CLR}: TDynByteArray; OutputOffset: Cardinal {$ENDIF CLR}): Cardinal;
var
  ByteBuffer, ByteBufferSpace: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  {$IFDEF CLR}
  Result := MimeDecodePartial(InputBuffer, InputOffset, InputByteCount, OutputBuffer, OutputOffset, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(OutputBuffer, OutputOffset + Result, ByteBuffer, ByteBufferSpace));
  {$ELSE}
  Result := MimeDecodePartial(InputBuffer, InputByteCount, OutputBuffer, ByteBuffer, ByteBufferSpace);
  Inc(Result, MimeDecodePartialEnd(Pointer(Cardinal(@OutputBuffer) + Result)^, ByteBuffer, ByteBufferSpace));
  {$ENDIF CLR}
end;

{$IFDEF CLR}
function MimeDecodePartial(const InputBuffer: TDynByteArray; InputOffset: Cardinal;
  const InputByteCount: Cardinal; out OutputBuffer: TDynByteArray; OutputOffset: Cardinal;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InIndex, OuterLimit: Cardinal;
  OutIndex: Cardinal;
begin
  if InputByteCount > 0 then
    begin
      InIndex := InputOffset;
      OuterLimit := InIndex + InputByteCount;
      OutIndex := OutputOffset;
      LByteBuffer := ByteBuffer;
      LByteBufferSpace := ByteBufferSpace;
      while InIndex < OuterLimit do
      begin
        { Read from InputBuffer. }
        C := MIME_DECODE_TABLE[InputBuffer[InIndex]];
        Inc(InIndex);
        if C = $FF then
          Continue;
        LByteBuffer := LByteBuffer shl 6;
        LByteBuffer := LByteBuffer or C;
        Dec(LByteBufferSpace);
        { Have we read 4 bytes from InputBuffer? }
        if LByteBufferSpace <> 0 then
          Continue;

        { Write 3 bytes to OutputBuffer (in reverse order). }
        OutputBuffer[OutIndex + 2] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutIndex + 1] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutIndex + 0] := Byte(LByteBuffer);
        LByteBuffer := 0;
        Inc(OutIndex, 3);
        LByteBufferSpace := 4;
      end;
      ByteBuffer := LByteBuffer;
      ByteBufferSpace := LByteBufferSpace;
      Result := OutIndex - OutputOffset;
    end
  else
    Result := 0;
end;
{$ELSE}
function MimeDecodePartial(const InputBuffer; const InputByteCount: Cardinal; out OutputBuffer;
  var ByteBuffer: Cardinal; var ByteBufferSpace: Cardinal): Cardinal;
var
  LByteBuffer, LByteBufferSpace, C: Cardinal;
  InPtr, OuterLimit: ^Byte;
  OutPtr: PByte3;
begin
  if InputByteCount > 0 then
  begin
    InPtr := @InputBuffer;
    Cardinal(OuterLimit) := Cardinal(InPtr) + InputByteCount;
    OutPtr := @OutputBuffer;
    LByteBuffer := ByteBuffer;
    LByteBufferSpace := ByteBufferSpace;
    while InPtr <> OuterLimit do
    begin
      { Read from InputBuffer. }
      C := MIME_DECODE_TABLE[InPtr^];
      Inc(InPtr);
      if C = $FF then
        Continue;
      LByteBuffer := LByteBuffer shl 6;
      LByteBuffer := LByteBuffer or C;
      Dec(LByteBufferSpace);
      { Have we read 4 bytes from InputBuffer? }
      if LByteBufferSpace <> 0 then
        Continue;

      { Write 3 bytes to OutputBuffer (in reverse order). }
      OutPtr^.B3 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B2 := Byte(LByteBuffer);
      LByteBuffer := LByteBuffer shr 8;
      OutPtr^.B1 := Byte(LByteBuffer);
      LByteBuffer := 0;
      Inc(OutPtr);
      LByteBufferSpace := 4;
    end;
    ByteBuffer := LByteBuffer;
    ByteBufferSpace := LByteBufferSpace;
    Result := Cardinal(OutPtr) - Cardinal(@OutputBuffer);
  end
  else
    Result := 0;
end;
{$ENDIF CLR}

function MimeDecodePartialEnd(out OutputBuffer {$IFDEF CLR}: TDynByteArray; OutputOffset: Cardinal {$ENDIF CLR};
  const ByteBuffer: Cardinal; const ByteBufferSpace: Cardinal): Cardinal;
var
  LByteBuffer: Cardinal;
begin
  case ByteBufferSpace of
    1:
      begin
        LByteBuffer := ByteBuffer shr 2;
        {$IFDEF CLR}
        OutputBuffer[OutputOffset + 1] := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        OutputBuffer[OutputOffset + 0] := Byte(LByteBuffer);
        {$ELSE}
        PByte3(@OutputBuffer)^.B2 := Byte(LByteBuffer);
        LByteBuffer := LByteBuffer shr 8;
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        {$ENDIF CLR}
        Result := 2;
      end;
    2:
      begin
        LByteBuffer := ByteBuffer shr 4;
        {$IFDEF CLR}
        OutputBuffer[OutputOffset + 0] := Byte(LByteBuffer);
        {$ELSE}
        PByte3(@OutputBuffer)^.B1 := Byte(LByteBuffer);
        {$ENDIF CLR}
        Result := 1;
      end;
  else
    Result := 0;
  end;
end;

// File Encoding & Decoding
procedure MimeEncodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeEncodeFileNoCRLF(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeEncodeStreamNoCRLF(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure MimeDecodeFile(const InputFileName, OutputFileName: AnsiString);
var
  InputStream, OutputStream: TFileStream;
begin
  InputStream := TFileStream.Create(InputFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(OutputFileName, fmCreate);
    try
      MimeDecodeStream(InputStream, OutputStream);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

// Stream Encoding & Decoding
procedure MimeEncodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  {$IFDEF CLR}
  OutputBuffer: array of Byte;
  {$ELSE}
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2 - 1] of Byte;
  {$ENDIF CLR}
  BytesRead: Cardinal;
  IDelta, ODelta: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  {$IFDEF CLR}
  SetLength(OutputBuffer, (MIME_BUFFER_SIZE + 2) div 3 * 4 + MIME_BUFFER_SIZE div MIME_DECODED_LINE_BREAK * 2);
  {$ENDIF CLR}

  while BytesRead = Cardinal(Length(InputBuffer)) do
  begin
    MimeEncodeFullLines(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  MimeEncodeFullLines(InputBuffer, BytesRead, OutputBuffer);

  IDelta := BytesRead div MIME_DECODED_LINE_BREAK; // Number of lines processed.
  ODelta := IDelta * (MIME_ENCODED_LINE_BREAK + 2);
  IDelta := IDelta * MIME_DECODED_LINE_BREAK;
  {$IFDEF ClR}
  MimeEncodeNoCRLF(InputBuffer, IDelta, BytesRead - IDelta, OutputBuffer, ODelta);
  {$ELSE}
  MimeEncodeNoCRLF(Pointer(Cardinal(@InputBuffer) + IDelta)^, BytesRead - IDelta, Pointer(Cardinal(@OutputBuffer) + ODelta)^);
  {$ENDIF CLR}

  OutputStream.Write(OutputBuffer, MimeEncodedSize(BytesRead));
end;

procedure MimeEncodeStreamNoCRLF(const InputStream: TStream; const OutputStream: TStream);
var
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  {$IFDEF CLR}
  OutputBuffer: array of Byte;
  {$ELSE}
  OutputBuffer: array [0..((MIME_BUFFER_SIZE + 2) div 3) * 4 - 1] of Byte;
  {$ENDIF CLR}
  BytesRead: Cardinal;
begin
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  {$IFDEF CLR}
  SetLength(OutputBuffer, ((MIME_BUFFER_SIZE + 2) div 3) * 4);
  {$ENDIF CLR}

  while BytesRead = Cardinal(Length(InputBuffer)) do
  begin
    MimeEncodeNoCRLF(InputBuffer, Length(InputBuffer), OutputBuffer);
    OutputStream.Write(OutputBuffer, Length(OutputBuffer));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;

  MimeEncodeNoCRLF(InputBuffer, BytesRead, OutputBuffer);
  OutputStream.Write(OutputBuffer, MimeEncodedSizeNoCRLF(BytesRead));
end;

procedure MimeDecodeStream(const InputStream: TStream; const OutputStream: TStream);
var
  ByteBuffer, ByteBufferSpace: Cardinal;
  InputBuffer: array [0..MIME_BUFFER_SIZE - 1] of Byte;
  {$IFDEF CLR}
  OutputBuffer: array of Byte;
  {$ELSE}
  OutputBuffer: array [0..(MIME_BUFFER_SIZE + 3) div 4 * 3 - 1] of Byte;
  {$ENDIF CLR}
  BytesRead: Cardinal;
begin
  ByteBuffer := 0;
  ByteBufferSpace := 4;
  BytesRead := InputStream.Read(InputBuffer, SizeOf(InputBuffer));
  {$IFDEF CLR}
  SetLength(OutputBuffer, (MIME_BUFFER_SIZE + 3) div 4 * 3);
  {$ENDIF CLR}

  while BytesRead > 0 do
  begin
    OutputStream.Write(OutputBuffer, MimeDecodePartial(InputBuffer, BytesRead, OutputBuffer, ByteBuffer, ByteBufferSpace));
    BytesRead := InputStream.Read(InputBuffer, Length(InputBuffer));
  end;
  OutputStream.Write(OutputBuffer, MimeDecodePartialEnd(OutputBuffer, ByteBuffer, ByteBufferSpace));
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
