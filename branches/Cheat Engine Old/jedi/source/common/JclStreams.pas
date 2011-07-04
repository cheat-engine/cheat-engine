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
{ The Original Code is JclStreams.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Robert Marquardt. Portions created by              }
{ Robert Marquardt are Copyright (C) Robert Marquardt (robert_marquardt att gmx dott de)           }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Florent Ouchet (outchy)                                                                        }
{   Heinz Zastrau                                                                                  }
{   Andreas Schmidt                                                                                }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Stream-related functions and classes                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-06-08 00:31:45 +0200 (dim., 08 juin 2008)                         $ }
{ Revision:      $Rev:: 2378                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclStreams;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Libc,
  {$ENDIF LINUX}
  SysUtils, Classes,
  JclBase;

type
  {$IFDEF COMPILER5}
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
  {$ENDIF COMPILER5}

  EJclStreamError = class(EJclError);

  // abstraction layer to support Delphi 5 and C++Builder 5 streams
  // 64 bit version of overloaded functions are introduced
  TJclStream = class(TStream)
  protected
    {$IFNDEF CLR}
    procedure SetSize(NewSize: Longint); overload; override;
    {$ENDIF ~CLR}
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
      {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE} overload; override; {$ENDIF}
  public
    {$IFNDEF CLR}
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
      {$IFDEF COMPILER5} reintroduce; overload; virtual; {$ELSE} overload; override; {$ENDIF}
  end;

  //=== VCL stream replacements ===

  {$IFNDEF CLR}
  TJclHandleStream = class(TJclStream)
  private
    FHandle: THandle;
  protected
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AHandle: THandle);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property Handle: THandle read FHandle;
  end;

  TJclFileStream = class(TJclHandleStream)
  public
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal = 0);
    destructor Destroy; override;
  end;
  {$ENDIF ~CLR}

  {
  TJclCustomMemoryStream = class(TJclStream)
  end;

  TJclMemoryStream = class(TJclCustomMemoryStream)
  end;

  TJclStringStream = class(TJclStream)
  end;

  TJclResourceStream = class(TJclCustomMemoryStream)
  end;
  }

  //=== new stream ideas ===

  TJclEmptyStream = class(TJclStream)
  protected
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclNullStream = class(TJclStream)
  private
    FPosition: Int64;
    FSize: Int64;
  protected
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

  TJclRandomStream = class(TJclNullStream)
  {$IFDEF CLR}
  private
    FRandomGenerator: System.Random;
  {$ENDIF CLR}
  protected
    function GetRandSeed: Longint; virtual;
    procedure SetRandSeed(Seed: Longint); virtual;
  public
    {$IFDEF CLR}
    constructor Create;
    destructor Destroy; override;
    {$ENDIF CLR}
    function RandomData: Byte; virtual;
    procedure Randomize; dynamic;
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    property RandSeed: Longint read GetRandSeed write SetRandSeed;
  end;

  TJclMultiplexStream = class(TJclStream)
  private
    FStreams: TList;
    FReadStreamIndex: Integer;
    function GetStream(Index: Integer): TStream;
    function GetCount: Integer;
    procedure SetStream(Index: Integer; const Value: TStream);
    function GetReadStream: TStream;
    procedure SetReadStream(const Value: TStream);
    procedure SetReadStreamIndex(const Value: Integer);
  protected
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    constructor Create;
    destructor Destroy; override;
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    function Add(NewStream: TStream): Integer;
    procedure Clear;
    function Remove(AStream: TStream): Integer;
    procedure Delete(const Index: Integer);

    property Streams[Index: Integer]: TStream read GetStream write SetStream;
    property ReadStreamIndex: Integer read FReadStreamIndex write SetReadStreamIndex;
    property ReadStream: TStream read GetReadStream write SetReadStream;
    property Count: Integer read GetCount;
  end;

  TJclStreamDecorator = class(TJclStream)
  private
    FAfterStreamChange: TNotifyEvent;
    FBeforeStreamChange: TNotifyEvent;
    FOwnsStream: Boolean;
    FStream: TStream;
    procedure SetStream(Value: TStream);
  protected
    procedure DoAfterStreamChange; virtual;
    procedure DoBeforeStreamChange; virtual;
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property AfterStreamChange: TNotifyEvent read FAfterStreamChange write FAfterStreamChange;
    property BeforeStreamChange: TNotifyEvent read FBeforeStreamChange write FBeforeStreamChange;
    property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
    property Stream: TStream read FStream write SetStream;
  end;

  {$IFNDEF CLR}
  TJclBufferedStream = class(TJclStreamDecorator)
  protected
    FBuffer: array of Byte;
    FBufferCurrentSize: Longint;
    FBufferMaxModifiedPos: Longint;
    FBufferSize: Longint;
    FBufferStart: Int64; // position of the first byte of the buffer in stream
    FPosition: Int64; // current position in stream
    function BufferHit: Boolean;
    function GetCalcedSize: Int64; virtual;
    function LoadBuffer: Boolean; virtual;
    function ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
    function WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
  protected
    procedure DoAfterStreamChange; override;
    procedure DoBeforeStreamChange; override;
    procedure SetSize(const NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; AOwnsStream: Boolean = False);
    destructor Destroy; override;
    procedure Flush; virtual;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property BufferSize: Longint read FBufferSize write FBufferSize;
  end;
  {$ENDIF ~CLR}

  TStreamNotifyEvent = procedure(Sender: TObject; Position: Int64; Size: Int64) of object;

  TJclEventStream = class(TJclStreamDecorator)
  private
    FNotification: TStreamNotifyEvent;
    procedure DoNotification;
  protected
    procedure DoBeforeStreamChange; override;
    procedure DoAfterStreamChange; override;
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    constructor Create(AStream: TStream; ANotification: TStreamNotifyEvent = nil;
      AOwnsStream: Boolean = False);
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property OnNotification: TStreamNotifyEvent read FNotification write FNotification;
  end;

  TJclEasyStream = class(TJclStreamDecorator)
  public
    function IsEqual(Stream: TStream): Boolean;
    function ReadBoolean: Boolean;
    function ReadChar: Char;
    {$IFNDEF CLR}
    function ReadCurrency: Currency;
    function ReadDateTime: TDateTime;
    {$ENDIF ~CLR}
    function ReadDouble: Double;
    function ReadExtended: Extended;
    function ReadInt64: Int64;
    function ReadInteger: Integer;
    function ReadCString: string;
    function ReadShortString: string;
    function ReadSingle: Single;
    function ReadSizedString: string;
    procedure WriteBoolean(Value: Boolean);
    procedure WriteChar(Value: Char);
    {$IFNDEF CLR}
    procedure WriteCurrency(const Value: Currency);
    procedure WriteDateTime(const Value: TDateTime);
    {$ENDIF ~CLR}
    procedure WriteDouble(const Value: Double);
    procedure WriteExtended(const Value: Extended);
    procedure WriteInt64(Value: Int64); overload;
    procedure WriteInteger(Value: Integer); overload;
    procedure WriteCString(const Value: string);
    procedure WriteStringDelimitedByNull(const Value: string); {$IFDEF ACCEPT_DEPRECATED}deprecated;{$ENDIF ACCEPT_DEPRECATED}
    procedure WriteShortString(const Value: ShortString);
    procedure WriteSingle(const Value: Single);
    procedure WriteSizedString(const Value: string);
  end;

  TJclScopedStream = class(TJclStream)
  private
    FParentStream: TStream;
    FStartPos: Int64;
    FCurrentPos: Int64;
    FMaxSize: Int64;
  protected
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    // scopedstream starting at the current position of the ParentStream
    //   if MaxSize is positive or null, read and write operations cannot overrun this size or the ParentStream limitation
    //   if MaxSize is negative, read and write operations are unlimited (up to the ParentStream limitation)
    constructor Create(AParentStream: TStream; AMaxSize: Int64 = -1);
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;

    property ParentStream: TStream read FParentStream;
    property StartPos: Int64 read FStartPos;
    property MaxSize: Int64 read FMaxSize write FMaxSize;
  end;

  TJclStreamSeekEvent = function(Sender: TObject; const Offset: Int64;
    Origin: TSeekOrigin): Int64 of object;
  TJclStreamReadEvent = function(Sender: TObject; var Buffer; {$IFDEF CLR}Offset,{$ENDIF CLR} Count: Longint): Longint of object;
  TJclStreamWriteEvent = function(Sender: TObject; const Buffer; {$IFDEF CLR}Offset,{$ENDIF CLR}Count: Longint): Longint of object;
  TJclStreamSizeEvent = procedure(Sender: TObject; {$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64) of object;

  TJclDelegatedStream = class(TJclStream)
  private
    FOnSeek: TJclStreamSeekEvent;
    FOnRead: TJclStreamReadEvent;
    FOnWrite: TJclStreamWriteEvent;
    FOnSize: TJclStreamSizeEvent;
  protected
    procedure SetSize({$IFNDEF CLR}const{$ENDIF CLR} NewSize: Int64); override;
  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$IFDEF CLR}
    function Read(var Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: array of Byte; Offset, Count: Longint): Longint; override;
    {$ELSE ~CLR}
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    {$ENDIF ~CLR}
    property OnSeek: TJclStreamSeekEvent read FOnSeek write FOnSeek;
    property OnRead: TJclStreamReadEvent read FOnRead write FOnRead;
    property OnWrite: TJclStreamWriteEvent read FOnWrite write FOnWrite;
    property OnSize: TJclStreamSizeEvent read FOnSize write FOnSize;
  end;

  // ancestor classes for streams with checksums and encrypted streams
  // data are stored in sectors: each BufferSize-d buffer is followed by FBlockOverHeader bytes
  // containing the checksum. In case of an encrypted stream, there is no byte
  // but sector is encrypted

  // reusing some code from TJclBufferedStream
  {$IFNDEF CLR}
  TJclSectoredStream = class(TJclBufferedStream)
  protected
    FSectorOverHead: Integer;
    function FlatToSectored(const Position: Int64): Int64;
    function SectoredToFlat(const Position: Int64): Int64;
    function GetCalcedSize: Int64; override;
    function LoadBuffer: Boolean; override;
    procedure DoAfterStreamChange; override;
    procedure AfterBlockRead; virtual;   // override to check protection
    procedure BeforeBlockWrite; virtual; // override to compute protection
    procedure SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64); override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False;
      ASectorOverHead: Integer = 0);

    procedure Flush; override;
  end;

  TJclCRC16Stream = class(TJclSectoredStream)
  protected
    procedure AfterBlockRead; override;
    procedure BeforeBlockWrite; override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False);
  end;

  TJclCRC32Stream = class(TJclSectoredStream)
  protected
    procedure AfterBlockRead; override;
    procedure BeforeBlockWrite; override;
  public
    constructor Create(AStorageStream: TStream; AOwnsStream: Boolean = False);
  end;

  TJclSplitStream = class(TJclStream)
  private
    FVolume: TStream;
    FVolumeIndex: Integer;
    FVolumeMaxSize: Int64;
    FPosition: Int64;
    FVolumePosition: Int64;
  protected
    function GetVolume(Index: Integer): TStream; virtual; abstract;
    function GetVolumeMaxSize(Index: Integer): Int64; virtual; abstract;
    function GetSize: Int64; {$IFDEF COMPILER7_UP}override;{$ENDIF COMPILER7_UP}
    procedure SetSize(const NewSize: Int64); override;
    procedure InternalLoadVolume(Index: Integer);
  public
    constructor Create;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TJclVolumeEvent = function(Index: Integer): TStream of object;
  TJclVolumeMaxSizeEvent = function(Index: Integer): Int64 of object;

  TJclDynamicSplitStream = class(TJclSplitStream)
  private
    FOnVolume: TJclVolumeEvent;
    FOnVolumeMaxSize: TJclVolumeMaxSizeEvent;
  protected
    function GetVolume(Index: Integer): TStream; override;
    function GetVolumeMaxSize(Index: Integer): Int64; override;
  public
    property OnVolume: TJclVolumeEvent read FOnVolume write FOnVolume;
    property OnVolumeMaxSize: TJclVolumeMaxSizeEvent read FOnVolumeMaxSize
      write FOnVolumeMaxSize;
  end;

  TJclSplitVolume = record
    MaxSize: Int64;
    Stream: TStream;
    OwnStream: Boolean;
  end;
  PJclSplitVolume = ^TJclSplitVolume;

  TJclStaticSplitStream = class(TJclSplitStream)
  private
    FVolumes: TList;
    function GetVolumeCount: Integer;
  protected
    function GetVolume(Index: Integer): TStream; override;
    function GetVolumeMaxSize(Index: Integer): Int64; override;
  public
    constructor Create;
    destructor Destroy; override;

    function AddVolume(AStream: TStream; AMaxSize: Int64 = 0;
      AOwnStream: Boolean = False): Integer;

    property VolumeCount: Integer read GetVolumeCount;
    property Volumes[Index: Integer]: TStream read GetVolume;
    property VolumeMaxSizes[Index: Integer]: Int64 read GetVolumeMaxSize;
  end;
  {$ENDIF ~CLR}

// call TStream.Seek(Int64,TSeekOrigin) if present (TJclStream or COMPILER6_UP)
// otherwize call TStream.Seek(LongInt,Word) with range checking
function StreamSeek(Stream: TStream; const Offset: Int64;
  const Origin: TSeekOrigin): Int64;

// buffered copy of all available bytes from Source to Dest
// returns the number of bytes that were copied
function StreamCopy(Source: TStream; Dest: TStream; BufferSize: Integer = 4096): Int64;

// compares 2 streams for differencies
function CompareStreams(A, B : TStream; BufferSize: Integer = 4096): Boolean;
// compares 2 files for differencies (calling CompareStreams)
function CompareFiles(const FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclStreams.pas $';
    Revision: '$Revision: 2378 $';
    Date: '$Date: 2008-06-08 00:31:45 +0200 (dim., 08 juin 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF CLR}
  System.Text,
  {$ENDIF CLR}
  JclResources, JclMath;

{$IFDEF KYLIX}
function __open(PathName: PChar; Flags: Integer; Mode: Integer): Integer; cdecl;
  external 'libc.so.6' name 'open';
{$ENDIF KYLIX}

function StreamSeek(Stream: TStream; const Offset: Int64;
  const Origin: TSeekOrigin): Int64; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF SUPPORTS_INLINE}
begin
  if Assigned(Stream) then
  begin
    {$IFDEF COMPILER5}
    if Stream is TJclStream then
      Result := TJclStream(Stream).Seek(Offset, Origin)
    else
    if (Offset <= MaxLongint) or (Offset > -MaxLongint) then
      Result := Stream.Seek(Longint(Offset), Ord(Origin))
    else
      Result := -1;
    {$ELSE}
    Result := Stream.Seek(Offset, Origin);
    {$ENDIF COMPILER5}
  end
  else
    Result := -1;
end;

function StreamCopy(Source: TStream; Dest: TStream; BufferSize: Integer): Int64;
var
  Buffer: array of Byte;
  ByteCount: Integer;
begin
  Result := 0;
  SetLength(Buffer, BufferSize);
  try
    repeat
      {$IFDEF CLR}
      ByteCount := Source.Read(Buffer, 0, BufferSize);
      {$ELSE ~CLR}
      ByteCount := Source.Read(Buffer[0], BufferSize);
      {$ENDIF ~CLR}
      Result := Result + ByteCount;
      {$IFDEF CLR}
      Dest.WriteBuffer(Buffer, ByteCount);
      {$ELSE ~CLR}
      Dest.WriteBuffer(Buffer[0], ByteCount);
      {$ENDIF ~CLR}
    until ByteCount < BufferSize;
  finally
    SetLength(Buffer, 0);
  end;
end;

function CompareStreams(A, B : TStream; BufferSize: Integer = 4096): Boolean;
var
  BufferA, BufferB: array of Byte;
  ByteCountA, ByteCountB: Integer;
  {$IFDEF CLR}
  Index: Integer;
  {$ENDIF CLR}
begin
  SetLength(BufferA, BufferSize);
  try
    SetLength(BufferB, BufferSize);
    try
      repeat
        {$IFDEF CLR}
        ByteCountA := A.Read(BufferA, 0, BufferSize);
        ByteCountB := A.Read(BufferB, 0, BufferSize);
        {$ELSE ~CLR}
        ByteCountA := A.Read(BufferA[0], BufferSize);
        ByteCountB := B.Read(BufferB[0], BufferSize);
        {$ENDIF ~CLR}

        Result := (ByteCountA = ByteCountB);
        {$IFDEF CLR}
        if Result then
          for Index := 0 to ByteCountA - 1 do
            if BufferA[Index] <> BufferB[Index] then
        begin
          Result := False;
          Break;
        end;
        {$ELSE CLR}
        Result := Result and CompareMem(BufferA, BufferB, ByteCountA);
        {$ENDIF ~CLR}
      until (ByteCountA <> BufferSize) or (ByteCountB <> BufferSize) or not Result;
    finally
      SetLength(BufferB, 0);
    end;
  finally
    SetLength(BufferA, 0);
  end;
end;

function CompareFiles(const FileA, FileB: TFileName; BufferSize: Integer = 4096): Boolean;
var
  A, B: TStream;
begin
  A := TFileStream.Create(FileA, fmOpenRead or fmShareDenyWrite);
  try
    B := TFileStream.Create(FileB, fmOpenRead or fmShareDenyWrite);
    try
      Result := CompareStreams(A, B, BufferSize);
    finally
      B.Free;
    end;
  finally
    A.Free;
  end;
end;

//=== { TJclStream } =========================================================

{$IFNDEF CLR}
function TJclStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  Result64: Int64;
begin
  case Origin of
    soFromBeginning:
      Result64 := Seek(Int64(Offset), soBeginning);
    soFromCurrent:
      Result64 := Seek(Int64(Offset), soCurrent);
    soFromEnd:
      Result64 := Seek(Int64(Offset), soEnd);
  else
    Result64 := -1;
  end;
  if (Result64 < 0) or (Result64 > High(Longint)) then
    Result64 := -1;
  Result := Result64;
end;
{$ENDIF ~CLR}

function TJclStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // override to customize
  Result := -1;
end;

{$IFNDEF CLR}
procedure TJclStream.SetSize(NewSize: Longint);
begin
  SetSize(Int64(NewSize));
end;
{$ENDIF ~CLR}

procedure TJclStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  // override to customize
end;

{$IFNDEF CLR}
//=== { TJclHandleStream } ===================================================

constructor TJclHandleStream.Create(AHandle: THandle);
begin
  inherited Create;
  FHandle := AHandle;
end;

function TJclHandleStream.Read(var Buffer; Count: Longint): Longint;
begin
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not ReadFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __read(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

function TJclHandleStream.Write(const Buffer; Count: Longint): Longint;
begin
  {$IFDEF MSWINDOWS}
  if (Count <= 0) or not WriteFile(Handle, Buffer, DWORD(Count), DWORD(Result), nil) then
    Result := 0;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Result := __write(Handle, Buffer, Count);
  {$ENDIF LINUX}
end;

{$IFDEF MSWINDOWS}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  INVALID_SET_FILE_POINTER = -1;
type
  TLarge = record
    case Boolean of
    False:
     (OffsetLo: Longint;
      OffsetHi: Longint);
    True:
      (Offset64: Int64);
  end;
var
  Offs: TLarge;
begin
  Offs.Offset64 := Offset;
  Offs.OffsetLo := SetFilePointer(Handle, Offs.OffsetLo, @Offs.OffsetHi, Ord(Origin));
  if (Offs.OffsetLo = INVALID_SET_FILE_POINTER) and (GetLastError <> NO_ERROR) then
    Result := -1
  else
    Result := Offs.Offset64;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
function TJclHandleStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
const
  SeekOrigins: array [TSeekOrigin] of Cardinal = ( SEEK_SET {soBeginning}, SEEK_CUR {soCurrent}, SEEK_END {soEnd} );
begin
{$IFDEF KYLIX}
  Result := __lseek(Handle, Offset, SeekOrigins[Origin]);
{$ELSE}
  Result := lseek(Handle, Offset, SeekOrigins[Origin]);
{$ENDIF}
end;
{$ENDIF LINUX}

procedure TJclHandleStream.SetSize(const NewSize: Int64);
begin
  Seek(NewSize, soBeginning);
  {$IFDEF MSWINDOWS}
  if not SetEndOfFile(Handle) then
    RaiseLastOSError;
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  if ftruncate(Handle, Position) = -1 then
    raise EJclStreamError.CreateRes(@RsStreamsSetSizeError);
  {$ENDIF LINUX}
end;

//=== { TJclFileStream } =====================================================

constructor TJclFileStream.Create(const FileName: string; Mode: Word; Rights: Cardinal);
var
  H: THandle;
{$IFDEF LINUX}
const
  INVALID_HANDLE_VALUE = -1;
{$ENDIF LINUX}
begin
  if Mode = fmCreate then
  begin
    {$IFDEF LINUX}
    {$IFDEF KYLIX}
    H := __open(PChar(FileName), O_CREAT or O_RDWR, FileAccessRights);
    {$ELSE ~KYLIX}
    H := open(PChar(FileName), O_CREAT or O_RDWR, $666);
    {$ENDIF}
    {$ELSE ~LINUX}
    H := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
      0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
    {$ENDIF ~LINUX}
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      {$IFDEF CLR}
      raise EJclStreamError.CreateFmt(RsStreamsCreateError, [FileName]);
      {$ELSE}
      raise EJclStreamError.CreateResFmt(@RsStreamsCreateError, [FileName]);
      {$ENDIF CLR}
  end
  else
  begin
    H := THandle(FileOpen(FileName, Mode));
    inherited Create(H);
    if Handle = INVALID_HANDLE_VALUE then
      {$IFDEF CLR}
      raise EJclStreamError.CreateFmt(RsStreamsOpenError, [FileName]);
      {$ELSE}
      raise EJclStreamError.CreateResFmt(@RsStreamsOpenError, [FileName]);
      {$ENDIF CLR}
  end;
end;

destructor TJclFileStream.Destroy;
begin
  {$IFDEF MSWINDOWS}
  if Handle <> INVALID_HANDLE_VALUE then
    CloseHandle(Handle);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  __close(Handle);
  {$ENDIF LINUX}
  inherited Destroy;
end;

{$ENDIF ~CLR}

//=== { TJclEmptyStream } ====================================================

// a stream which stays empty no matter what you do
// so it is a Unix /dev/null equivalent

procedure TJclEmptyStream.SetSize({$IFNDEF CLR}const{$ENDIF CLR} NewSize: Int64);
begin
  // nothing
end;

{$IFDEF CLR}
function TJclEmptyStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclEmptyStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  // you cannot read anything
  Result := 0;
end;

{$IFDEF CLR}
function TJclEmptyStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclEmptyStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  // you cannot write anything
  Result := 0;
end;

function TJclEmptyStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Offset <> 0 then
    // seeking to anywhere except the position 0 is an error
    Result := -1
  else
    Result := 0;
end;

//=== { TJclNullStream } =====================================================

// a stream which only keeps position and size, but no data
// so it is a Unix /dev/zero equivalent (?)

procedure TJclNullStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  if NewSize > 0 then
    FSize := NewSize
  else
    FSize := 0;
  if FPosition > FSize then
    FPosition := FSize;
end;

{$IFDEF CLR}
function TJclNullStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
var
  Index: Integer;
{$ELSE ~CLR}
function TJclNullStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Count < 0 then
    Count := 0;
  // FPosition > FSize is possible!
  if FSize - FPosition < Count then
    Count := FSize - FPosition;
  // does not read if beyond EOF
  if Count > 0 then
  begin
    {$IFDEF CLR}
    for Index := Offset to Offset + Count - 1 do
      Buffer[Index] := 0;
    {$ELSE ~CLR}
    FillChar(Buffer, Count, 0);
    {$ENDIF ~CLR}
    FPosition := FPosition + Count;
  end;
  Result := Count;
end;

{$IFDEF CLR}
function TJclNullStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclNullStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Count < 0 then
    Count := 0;
  FPosition := FPosition + Count;
  // writing when FPosition > FSize is possible!
  if FPosition > FSize then
    FSize := FPosition;
  Result := Count;
end;

function TJclNullStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
var
  Rel: Int64;
begin
  case Origin of
    soBeginning:
      Rel := 0;
    soCurrent:
      Rel := FPosition;
    soEnd:
      Rel := FSize;
  else
    // force Rel + Offset = -1 (code is never reached)
    Rel := Offset - 1;
  end;
  if Rel + Offset >= 0 then
  begin
    // all non-negative destination positions including beyond EOF are valid
    FPosition := Rel + Offset;
    Result := FPosition;
  end
  else
    Result := -1;
end;

//=== { TJclRandomStream } ===================================================

// A TJclNullStream decendant which returns random data when read
// so it is a Unix /dev/random equivalent

{$IFDEF CLR}
constructor TJclRandomStream.Create;
begin
  inherited Create;
  FRandomGenerator := System.Random.Create;
end;

destructor TJclRandomStream.Destroy;
begin
  FRandomGenerator.Free;
  inherited Destroy;
end;
{$ENDIF CLR}

function TJclRandomStream.GetRandSeed: Longint;
begin
  {$IFDEF CLR}
  Result := 0;
  {$ELSE ~CLR}
  Result := System.RandSeed;
  {$ENDIF ~CLR}
end;

procedure TJclRandomStream.SetRandSeed(Seed: Longint);
begin
  {$IFDEF CLR}
  FRandomGenerator.Free;
  FRandomGenerator := System.Random.Create(Seed);
  {$ELSE ~CLR}
  System.RandSeed := Seed;
  {$ENDIF ~CLR}
end;

function TJclRandomStream.RandomData: Byte;
begin
  {$IFDEF CLR}
  Result := FRandomGenerator.Next(256);
  {$ELSE ~CLR}
  Result := System.Random(256);
  {$ENDIF ~CLR}
end;

procedure TJclRandomStream.Randomize;
begin
  {$IFNDEF CLR}
  System.Randomize;
  {$ENDIF ~CLR}
end;

{$IFDEF CLR}
function TJclRandomStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
var
  I: Longint;
begin
  // this handles all necessary checks
  Count := inherited Read(Buffer, Offset, Count);
  for I := Offset to Offset + Count - 1 do
    Buffer[I] := RandomData;
  Result := Count;
end;
{$ELSE ~CLR}
function TJclRandomStream.Read(var Buffer; Count: Longint): Longint;
var
  I: Longint;
  BufferPtr: PByte;
begin
  // this handles all necessary checks
  Count := inherited Read(Buffer, Count);
  BufferPtr := @Buffer;
  for I := 0 to Count - 1 do
  begin
    BufferPtr^ := RandomData;
    Inc(BufferPtr);
  end;
  Result := Count;
end;
{$ENDIF ~CLR}

//=== { TJclMultiplexStream } ================================================

constructor TJclMultiplexStream.Create;
begin
  inherited Create;
  FStreams := TList.Create;
  FReadStreamIndex := -1;
end;

destructor TJclMultiplexStream.Destroy;
begin
  FStreams.Free;
  inherited Destroy;
end;

function TJclMultiplexStream.Add(NewStream: TStream): Integer;
begin
  {$IFDEF CLR}
  Result := FStreams.Add(NewStream);
  {$ELSE ~CLR}
  Result := FStreams.Add(Pointer(NewStream));
  {$ENDIF ~CLR}
end;

procedure TJclMultiplexStream.Clear;
begin
  FStreams.Clear;
  FReadStreamIndex := -1;
end;

procedure TJclMultiplexStream.Delete(const Index: Integer);
begin
  FStreams.Delete(Index);
  if ReadStreamIndex = Index then
    FReadStreamIndex := -1
  else
  if ReadStreamIndex > Index then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.GetReadStream: TStream;
begin
  if FReadStreamIndex >= 0 then
    Result := TStream(FStreams.Items[FReadStreamIndex])
  else
    Result := nil;
end;

function TJclMultiplexStream.GetStream(Index: Integer): TStream;
begin
  Result := TStream(FStreams.Items[Index]);
end;

function TJclMultiplexStream.GetCount: Integer;
begin
  Result := FStreams.Count;
end;

{$IFDEF CLR}
function TJclMultiplexStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclMultiplexStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
var
  Stream: TStream;
begin
  Stream := ReadStream;
  if Assigned(Stream) then
    Result := Stream.Read(Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count)
  else
    Result := 0;
end;

function TJclMultiplexStream.Remove(AStream: TStream): Integer;
begin
  {$IFDEF CLR}
  Result := FStreams.Remove(AStream);
  {$ELSE ~CLR}
  Result := FStreams.Remove(Pointer(AStream));
  {$ENDIF ~CLR}
  if FReadStreamIndex = Result then
    FReadStreamIndex := -1
  else
  if FReadStreamIndex > Result then
    Dec(FReadStreamIndex);
end;

function TJclMultiplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  // what should this function do?
  Result := -1;
end;

procedure TJclMultiplexStream.SetReadStream(const Value: TStream);
begin
  {$IFDEF CLR}
  FReadStreamIndex := FStreams.IndexOf(Value);
  {$ELSE ~CLR}
  FReadStreamIndex := FStreams.IndexOf(Pointer(Value));
  {$ENDIF ~CLR}
end;

procedure TJclMultiplexStream.SetReadStreamIndex(const Value: Integer);
begin
  FReadStreamIndex := Value;
end;

procedure TJclMultiplexStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  // what should this function do?
end;

procedure TJclMultiplexStream.SetStream(Index: Integer; const Value: TStream);
begin
  FStreams.Items[Index] := {$IFDEF CLR}Value;{$ELSE ~CLR}Pointer(Value){$ENDIF ~CLR};
end;

{$IFDEF CLR}
function TJclMultiplexStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclMultiplexStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
var
  Index: Integer;
  ByteWritten, MinByteWritten: Longint;
begin
  MinByteWritten := Count;
  for Index := 0 to Self.Count - 1 do
  begin
    ByteWritten := TStream(FStreams.Items[Index]).Write(Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count);
    if ByteWritten < MinByteWritten then
      MinByteWritten := ByteWritten;
  end;
  Result := MinByteWritten;
end;

//=== { TJclStreamDecorator } ================================================

constructor TJclStreamDecorator.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
end;

destructor TJclStreamDecorator.Destroy;
begin
  if OwnsStream then
    FStream.Free;
  inherited Destroy;
end;

procedure TJclStreamDecorator.DoAfterStreamChange;
begin
  if Assigned(FAfterStreamChange) then
    FAfterStreamChange(Self);
end;

procedure TJclStreamDecorator.DoBeforeStreamChange;
begin
  if Assigned(FBeforeStreamChange) then
    FBeforeStreamChange(Self);
end;

{$IFDEF CLR}
function TJclStreamDecorator.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclStreamDecorator.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Assigned(FStream) then
    Result := Stream.Read(Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count)
  else
    Result := 0;
end;

function TJclStreamDecorator.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := StreamSeek(Stream, Offset, Origin);
end;

procedure TJclStreamDecorator.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  if Assigned(FStream) then
    Stream.Size := NewSize;
end;

procedure TJclStreamDecorator.SetStream(Value: TStream);
begin
  if Value <> FStream then
    try
      DoBeforeStreamChange;
    finally
      if OwnsStream then
        FStream.Free;
      FStream := Value;
      DoAfterStreamChange;
    end;
end;

{$IFDEF CLR}
function TJclStreamDecorator.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclStreamDecorator.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Assigned(FStream) then
    Result := Stream.Write(Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count)
  else
    Result := 0;
end;

{$IFNDEF CLR}

//=== { TJclBufferedStream } =================================================

constructor TJclBufferedStream.Create(AStream: TStream; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  if Stream <> nil then
    FPosition := Stream.Position;
  BufferSize := 4096;
end;

destructor TJclBufferedStream.Destroy;
begin
  Flush;
  inherited Destroy;
end;

function TJclBufferedStream.BufferHit: Boolean;
begin
  Result := (FBufferStart <= FPosition) and (FPosition < (FBufferStart + FBufferCurrentSize));
end;

procedure TJclBufferedStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  FBufferCurrentSize := 0; // invalidate buffer after stream is changed
  FBufferStart := 0;
  if Stream <> nil then
    FPosition := Stream.Position;
end;

procedure TJclBufferedStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  Flush;
end;

procedure TJclBufferedStream.Flush;
begin
  if (Stream <> nil) and (FBufferMaxModifiedPos > 0) then
  begin
    Stream.Position := FBufferStart;
    Stream.WriteBuffer(FBuffer[0], FBufferMaxModifiedPos);
    FBufferMaxModifiedPos := 0;
  end;
end;

function TJclBufferedStream.GetCalcedSize: Int64;
begin
  if Assigned(Stream) then
    Result := Stream.Size
  else
    Result := 0;
  if Result < FBufferMaxModifiedPos + FBufferStart then
    Result := FBufferMaxModifiedPos + FBufferStart;
end;

function TJclBufferedStream.LoadBuffer: Boolean;
begin
  Flush;
  if Length(FBuffer) <> FBufferSize then
    SetLength(FBuffer, FBufferSize);
  if Stream <> nil then
  begin
    Stream.Position := FPosition;
    FBufferCurrentSize := Stream.Read(FBuffer[0], FBufferSize);
  end
  else
    FBufferCurrentSize := 0;
  FBufferStart := FPosition;
  Result := (FBufferCurrentSize > 0);
end;

function TJclBufferedStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Count;
  while Count > 0 do
  begin
    if not BufferHit then
      if not LoadBuffer then
        Break;
    Dec(Count, ReadFromBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count;
end;

function TJclBufferedStream.ReadFromBuffer(var Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > FBufferCurrentSize - BufPos then
    Result := FBufferCurrentSize - BufPos;
  P := @Buffer;
  Move(FBuffer[BufPos], P[Start], Result);
  Inc(FPosition, Result);
end;

function TJclBufferedStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  NewPos: Int64;
begin
  NewPos := FPosition;
  case Origin of
    soBeginning:
      NewPos := Offset;
    soCurrent:
      Inc(NewPos, Offset);
    soEnd:
      NewPos := GetCalcedSize + Offset;
  else
    NewPos := -1;
  end;
  if NewPos < 0 then
    NewPos := -1
  else
    FPosition := NewPos;
  Result := NewPos;
end;

procedure TJclBufferedStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(NewSize);
  if NewSize < (FBufferStart + FBufferMaxModifiedPos) then
  begin
    FBufferMaxModifiedPos := NewSize - FBufferStart;
    if FBufferMaxModifiedPos < 0 then
      FBufferMaxModifiedPos := 0;
  end;
  if NewSize < (FBufferStart + FBufferCurrentSize) then
  begin
    FBufferCurrentSize := NewSize - FBufferStart;
    if FBufferCurrentSize < 0 then
      FBufferCurrentSize := 0;
  end;
  // fix from Marcelo Rocha
  if Stream <> nil then
    FPosition := Stream.Position;
end;

function TJclBufferedStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  while Count > 0 do
  begin
    if not BufferHit then
      LoadBuffer;
    Dec(Count, WriteToBuffer(Buffer, Count, Result - Count));
  end;
  Result := Result - Count;
end;

function TJclBufferedStream.WriteToBuffer(const Buffer; Count, Start: Longint): Longint;
var
  BufPos: Longint;
  P: PChar;
begin
  Result := Count;
  BufPos := FPosition - FBufferStart;
  if Result > Length(FBuffer) - BufPos then
    Result := Length(FBuffer) - BufPos;
  if FBufferCurrentSize < BufPos + Result then
    FBufferCurrentSize := BufPos + Result;
  P := @Buffer;
  Move(P[Start], FBuffer[BufPos], Result);
  FBufferMaxModifiedPos := BufPos + Result;
  Inc(FPosition, Result);
end;

{$ENDIF ~CLR}

//=== { TJclEventStream } ====================================================

constructor TJclEventStream.Create(AStream: TStream; ANotification:
  TStreamNotifyEvent = nil; AOwnsStream: Boolean = False);
begin
  inherited Create(AStream, AOwnsStream);
  FNotification := ANotification;
end;

procedure TJclEventStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoBeforeStreamChange;
begin
  inherited DoBeforeStreamChange;
  if Stream <> nil then
    DoNotification;
end;

procedure TJclEventStream.DoNotification;
begin
  if Assigned(FNotification) then
    FNotification(Self, Stream.Position, Stream.Size);
end;

{$IFDEF CLR}
function TJclEventStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclEventStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  Result := inherited Read(Buffer, Count);
  DoNotification;
end;

function TJclEventStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := inherited Seek(Offset, Origin);
  DoNotification;
end;

procedure TJclEventStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  inherited SetSize(NewSize);
  DoNotification;
end;

{$IFDEF CLR}
function TJclEventStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclEventStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  Result := inherited Write(Buffer, Count);
  DoNotification;
end;

//=== { TJclEasyStream } =====================================================

function TJclEasyStream.IsEqual(Stream: TStream): Boolean;
var
  SavePos, StreamSavePos: Integer;
begin
  SavePos := Position;
  StreamSavePos := Stream.Position;
  try
    Position := 0;
    Stream.Position := 0;
    Result := CompareStreams(Self, Stream);
  finally
    Position := SavePos;
    Stream.Position := StreamSavePos;
  end;
end;

function TJclEasyStream.ReadBoolean: Boolean;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadChar: Char;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

{$IFNDEF CLR}
function TJclEasyStream.ReadCurrency: Currency;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadDateTime: TDateTime;
begin
  ReadBuffer(Result, SizeOf(Result));
end;
{$ENDIF ~CLR}

function TJclEasyStream.ReadDouble: Double;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadExtended: Extended;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInt64: Int64;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadInteger: Integer;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadCString: string;
var
  {$IFDEF CLR}
  SB: System.Text.StringBuilder;
  Ch: Char;
  {$ELSE ~CLR}
  CurrPos: Integer;
  StrSize: Integer;
  {$ENDIF ~CLR}
begin
  {$IFDEF CLR}
  SB := System.Text.StringBuilder.Create;
  repeat
    Ch := ReadChar;
    if ReadChar <> #0 then
      SB.Append(Ch);
  until ReadChar = #0;
  Result := SB.ToString;
  {$ELSE ~CLR}
  CurrPos := Position;
  repeat
  until ReadChar = #0;
  StrSize := Position - CurrPos - 1;
  SetString(Result, PChar(nil), StrSize);
  Position := CurrPos;
  ReadBuffer(Pointer(Result)^, StrSize);
  Position := Position + 1;
  {$ENDIF ~CLR}
end;

function TJclEasyStream.ReadShortString: string;
var
  {$IFDEF CLR}
  SB: System.Text.StringBuilder;
  {$ENDIF CLR}
  StrSize: Integer;
begin
  StrSize := Ord(ReadChar);
  {$IFDEF CLR}
  SB := System.Text.StringBuilder.Create(StrSize);
  while StrSize > 0 do
  begin
    SB.Append(ReadChar);
    Dec(StrSize);
  end;
  Result := SB.ToString;
  {$ELSE ~CLR}
  SetString(Result, PChar(nil), StrSize);
  ReadBuffer(Pointer(Result)^, StrSize);
  {$ENDIF ~CLR}
end;

function TJclEasyStream.ReadSingle: Single;
begin
  ReadBuffer(Result, SizeOf(Result));
end;

function TJclEasyStream.ReadSizedString: string;
var
  {$IFDEF CLR}
  SB: System.Text.StringBuilder;
  {$ENDIF CLR}
  StrSize: Integer;
begin
  StrSize := ReadInteger;
  {$IFDEF CLR}
  SB := System.Text.StringBuilder.Create(StrSize);
  while StrSize > 0 do
  begin
    SB.Append(ReadChar);
    Dec(StrSize);
  end;
  Result := SB.ToString;
  {$ELSE ~CLR}
  SetString(Result, PChar(nil), StrSize);
  ReadBuffer(Pointer(Result)^, StrSize);
  {$ENDIF ~CLR}
end;

procedure TJclEasyStream.WriteBoolean(Value: Boolean);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteChar(Value: Char);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

{$IFNDEF CLR}
procedure TJclEasyStream.WriteCurrency(const Value: Currency);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteDateTime(const Value: TDateTime);
begin
  WriteBuffer(Value, SizeOf(Value));
end;
{$ENDIF ~CLR}

procedure TJclEasyStream.WriteDouble(const Value: Double);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteExtended(const Value: Extended);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInt64(Value: Int64);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteInteger(Value: Integer);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteCString(const Value: string);
{$IFDEF CLR}
var
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  for I := 1 to Length(Value) do
    WriteChar(Value[I]);
  WriteChar(#0);
  {$ELSE ~CLR}
  WriteBuffer(PChar(Value)^, Length(Value) + 1);
  {$ENDIF ~CLR}
end;

procedure TJclEasyStream.WriteStringDelimitedByNull(const Value: string);
begin
  WriteCString(Value);
end;

procedure TJclEasyStream.WriteShortString(const Value: ShortString);
{$IFDEF CLR}
var
  I: Integer;
{$ENDIF CLR}
begin
  {$IFDEF CLR}
  for I := 0 to Length(Value) do
    inherited WriteBuffer(Value[I]);
  {$ELSE ~CLR}
  WriteBuffer(Value[0], Length(Value) + 1);
  {$ENDIF ~CLR}
end;

procedure TJclEasyStream.WriteSingle(const Value: Single);
begin
  WriteBuffer(Value, SizeOf(Value));
end;

procedure TJclEasyStream.WriteSizedString(const Value: string);
var
  StrSize: Integer;
  {$IFDEF CLR}
  I: Integer;
  {$ENDIF CLR}
begin
  StrSize := Length(Value);
  WriteInteger(StrSize);
  {$IFDEF CLR}
  for I := 1 to Length(Value) do
    WriteChar(Value[I]);
  {$ELSE ~CLR}
  WriteBuffer(Pointer(Value)^, StrSize);
  {$ENDIF ~CLR}
end;

//=== { TJclScopedStream } ===================================================

constructor TJclScopedStream.Create(AParentStream: TStream; AMaxSize: Int64);
begin
  inherited Create;

  FParentStream := AParentStream;
  FStartPos := ParentStream.Position;
  FCurrentPos := 0;
  FMaxSize := AMaxSize;
end;

{$IFDEF CLR}
function TJclScopedStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclScopedStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if (MaxSize >= 0) and ((FCurrentPos + Count) > MaxSize) then
    Count := MaxSize - FCurrentPos;

  if (Count > 0) and Assigned(ParentStream) then
  begin
    Result := ParentStream.Read(Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count);
    Inc(FCurrentPos, Result);
  end
  else
    Result := 0;
end;

function TJclScopedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      begin
        if (Offset < 0) or ((MaxSize >= 0) and (Offset > MaxSize)) then
          Result := -1            // low and high bound check
        else
          Result := StreamSeek(ParentStream, StartPos + Offset, soBeginning) - StartPos;
      end;
    soCurrent:
      begin
        if Offset = 0 then
          Result := FCurrentPos   // speeding the Position property up
        else if ((FCurrentPos + Offset) < 0) or ((MaxSize >= 0)
          and ((FCurrentPos + Offset) > MaxSize)) then
          Result := -1            // low and high bound check
        else
          Result := StreamSeek(ParentStream, Offset, soCurrent) - StartPos;
      end;
    soEnd:
      begin
        if (MaxSize >= 0) then
        begin
          if (Offset > 0) or (MaxSize < -Offset) then // low and high bound check
            Result := -1
          else
            Result := StreamSeek(ParentStream, StartPos + MaxSize + Offset, soBeginning) - StartPos;
        end
        else
        begin
          Result := StreamSeek(ParentStream, Offset, soEnd);
          if (Result <> -1) and (Result < StartPos) then // low bound check
          begin
            Result := -1;
            StreamSeek(ParentStream, StartPos + FCurrentPos, soBeginning);
          end;
        end;
      end;
    else
      Result := -1;
  end;
  if Result <> -1 then
    FCurrentPos := Result;
end;

procedure TJclScopedStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
var
  ScopedNewSize: Int64;
begin
  if (FMaxSize >= 0) and (NewSize >= (FStartPos + FMaxSize)) then
    ScopedNewSize := FMaxSize + FStartPos
  else
    ScopedNewSize := NewSize;
  inherited SetSize(ScopedNewSize);
end;

{$IFDEF CLR}
function TJclScopedStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclScopedStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if (MaxSize >= 0) and ((FCurrentPos + Count) > MaxSize) then
    Count := MaxSize - FCurrentPos;

  if (Count > 0) and Assigned(ParentStream) then
  begin
    Result := ParentStream.Write(Buffer, Count);
    Inc(FCurrentPos, Result);
  end
  else
    Result := 0;
end;

//=== { TJclDelegateStream } =================================================

procedure TJclDelegatedStream.SetSize({$IFNDEF CLR}const{$ENDIF ~CLR} NewSize: Int64);
begin
  if Assigned(FOnSize) then
    FOnSize(Self, NewSize);
end;

function TJclDelegatedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if Assigned(FOnSeek) then
    Result := FOnSeek(Self, Offset, Origin)
  else
    Result := -1;
end;

{$IFDEF CLR}
function TJclDelegatedStream.Read(var Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclDelegatedStream.Read(var Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Assigned(FOnRead) then
    Result := FOnRead(Self, Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count)
  else
    Result := -1;
end;

{$IFDEF CLR}
function TJclDelegatedStream.Write(const Buffer: array of Byte; Offset, Count: Longint): Longint;
{$ELSE ~CLR}
function TJclDelegatedStream.Write(const Buffer; Count: Longint): Longint;
{$ENDIF ~CLR}
begin
  if Assigned(FOnWrite) then
    Result := FOnWrite(Self, Buffer, {$IFDEF CLR}Offset,{$ENDIF CLR} Count)
  else
    Result := -1;
end;

{$IFNDEF CLR}
//=== { TJclSectoredStream } =================================================

procedure TJclSectoredStream.AfterBlockRead;
begin
  // override to customize (checks of protection)
end;

procedure TJclSectoredStream.BeforeBlockWrite;
begin
  // override to customize (computation of protection)
end;

constructor TJclSectoredStream.Create(AStorageStream: TStream;
  AOwnsStream: Boolean; ASectorOverHead: Integer);
begin
  inherited Create(AStorageStream, AOwnsStream);
  FSectorOverHead := ASectorOverHead;
  if Stream <> nil then
    FPosition := SectoredToFlat(Stream.Position);
end;

procedure TJclSectoredStream.DoAfterStreamChange;
begin
  inherited DoAfterStreamChange;
  if Stream <> nil then
    FPosition := SectoredToFlat(Stream.Position);
end;

function TJclSectoredStream.FlatToSectored(const Position: Int64): Int64;
begin
  Result := (Position div BufferSize) * (BufferSize + FSectorOverHead) // add overheads of previous buffers
    + Position mod BufferSize; // offset in sector
end;

procedure TJclSectoredStream.Flush;
begin
  if (Stream <> nil) and (FBufferMaxModifiedPos > 0) then
  begin
    BeforeBlockWrite;

    Stream.Position := FlatToSectored(FBufferStart);
    Stream.WriteBuffer(FBuffer[0], FBufferCurrentSize + FSectorOverHead);
    FBufferMaxModifiedPos := 0;
  end;
end;

function TJclSectoredStream.GetCalcedSize: Int64;
var
  VirtualSize: Int64;
begin
  if Assigned(Stream) then
    Result := SectoredToFlat(Stream.Size)
  else
    Result := 0;
  VirtualSize := FBufferMaxModifiedPos + FBufferStart;
  if Result < VirtualSize then
    Result := VirtualSize;
end;

function TJclSectoredStream.LoadBuffer: Boolean;
var
  TotalSectorSize: Integer;
begin
  Flush;
  TotalSectorSize := FBufferSize + FSectorOverHead;
  if Length(FBuffer) <> TotalSectorSize then
    SetLength(FBuffer, TotalSectorSize);
  FBufferStart := (FPosition div BufferSize) * BufferSize;
  if Stream <> nil then
  begin
    Stream.Position := FlatToSectored(FBufferStart);
    FBufferCurrentSize := Stream.Read(FBuffer[0], TotalSectorSize);
    if FBufferCurrentSize > 0 then
    begin
      Dec(FBufferCurrentSize, FSectorOverHead);
      AfterBlockRead;
    end;
  end
  else
    FBufferCurrentSize := 0;
  Result := (FBufferCurrentSize > 0);
end;

function TJclSectoredStream.SectoredToFlat(const Position: Int64): Int64;
var
  TotalSectorSize: Int64;
begin
  TotalSectorSize := BufferSize + FSectorOverHead;
  Result := (Position div TotalSectorSize) * BufferSize // remove previous overheads
    + Position mod TotalSectorSize; // offset in sector
end;

procedure TJclSectoredStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(FlatToSectored(NewSize));
end;

//=== { TJclCRC16Stream } ====================================================

procedure TJclCRC16Stream.AfterBlockRead;
var
  CRC: Word;
begin
  CRC := FBuffer[FBufferCurrentSize] + (FBuffer[FBufferCurrentSize + 1] shl 8);
  if CheckCrc16(FBuffer, FBufferCurrentSize, CRC) < 0 then
    raise EJclStreamError.CreateRes(@RsStreamsCRCError);
end;

procedure TJclCRC16Stream.BeforeBlockWrite;
var
  CRC: Word;
begin
  CRC := Crc16(FBuffer, FBufferCurrentSize);
  FBuffer[FBufferCurrentSize] := CRC and $FF;
  FBuffer[FBufferCurrentSize + 1] := CRC shr 8;
end;

constructor TJclCRC16Stream.Create(AStorageStream: TStream; AOwnsStream: Boolean);
begin
  inherited Create(AStorageStream, AOwnsStream, 2);
end;

//=== { TJclCRC32Stream } ====================================================

procedure TJclCRC32Stream.AfterBlockRead;
var
  CRC: Cardinal;
begin
  CRC := FBuffer[FBufferCurrentSize] + (FBuffer[FBufferCurrentSize + 1] shl 8)
    + (FBuffer[FBufferCurrentSize + 2] shl 16) + (FBuffer[FBufferCurrentSize + 3] shl 24);
  if CheckCrc32(FBuffer, FBufferCurrentSize, CRC) < 0 then
    raise EJclStreamError.CreateRes(@RsStreamsCRCError);
end;

procedure TJclCRC32Stream.BeforeBlockWrite;
var
  CRC: Cardinal;
begin
  CRC := Crc32(FBuffer, FBufferCurrentSize);
  FBuffer[FBufferCurrentSize] := CRC and $FF;
  FBuffer[FBufferCurrentSize + 1] := (CRC shr 8) and $FF;
  FBuffer[FBufferCurrentSize + 2] := (CRC shr 16) and $FF;
  FBuffer[FBufferCurrentSize + 3] := (CRC shr 24) and $FF;
end;

constructor TJclCRC32Stream.Create(AStorageStream: TStream;
  AOwnsStream: Boolean);
begin
  inherited Create(AStorageStream, AOwnsStream, 4);
end;

//=== { TJclSplitStream } ====================================================

constructor TJclSplitStream.Create;
begin
  inherited Create;
  FVolume := nil;
  FVolumeIndex := -1;
  FVolumeMaxSize := 0;
  FPosition := 0;
  FVolumePosition := 0;
end;

function TJclSplitStream.GetSize: Int64;
var
  OldVolumeIndex: Integer;
  OldVolumePosition, OldPosition: Int64;
begin
  OldVolumeIndex := FVolumeIndex;
  OldVolumePosition := FVolumePosition;
  OldPosition := FPosition;

  Result := 0;
  try
    FVolumeIndex := -1;
    repeat
      InternalLoadVolume(FVolumeIndex + 1);
      if not Assigned(FVolume) then
        Break;
      Result := Result + FVolume.Size;
    until FVolume.Size = 0;
  finally
    InternalLoadVolume(OldVolumeIndex);
    FPosition := OldPosition;
    if Assigned(FVolume) then
      FVolumePosition := StreamSeek(FVolume, OldVolumePosition, soBeginning);
  end;
end;

procedure TJclSplitStream.InternalLoadVolume(Index: Integer);
begin
  if Index = -1 then
    Index := 0;
  if Index <> FVolumeIndex then
  begin
    FVolumeIndex := Index;
    FVolumePosition := 0;
    FVolume := GetVolume(Index);
    FVolumeMaxSize := GetVolumeMaxSize(Index);
    if Assigned(FVolume) then
      StreamSeek(FVolume, 0, soBeginning);
  end;
end;

function TJclSplitStream.Read(var Buffer; Count: Integer): Longint;
var
  Data: PByte;
  Total, LoopRead: Integer;
begin
  Result := 0;

  InternalLoadVolume(FVolumeIndex);
  if not Assigned(FVolume) then
    Exit;

  Data := PByte(@Buffer);
  Total := Count;

  repeat
    // try to read (Count) bytes from current stream
    LoopRead := FVolume.Read(Data^, Count);
    FVolumePosition := FVolumePosition + LoopRead;
    FPosition := FPosition + LoopRead;
    Inc(Result, LoopRead);
    if Result = Total then
      Break;

    // with next volume
    Dec(Count, Result);
    Inc(Data, Result);
    InternalLoadVolume(FVolumeIndex + 1);
    if not Assigned(FVolume) then
      Break;
  until False;
end;

function TJclSplitStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
var
  ExpectedPosition, RemainingOffset: Int64;
begin
  case TSeekOrigin(Origin) of
    soBeginning:
      ExpectedPosition := Offset;
    soCurrent:
      ExpectedPosition := FPosition + Offset;
    soEnd:
      ExpectedPosition := Size - Offset;
  else
    raise EJclStreamError.CreateRes(@RsStreamsSeekError);
  end;
  RemainingOffset := ExpectedPosition - FPosition;
  Result := FPosition;
  repeat
    InternalLoadVolume(FVolumeIndex);
    if not Assigned(FVolume) then
      Break;

    if RemainingOffset < 0 then
    begin
      // FPosition > ExpectedPosition, seek backward
      if FVolumePosition >= -RemainingOffset then
      begin
        // seek in current volume
        FVolumePosition := StreamSeek(FVolume, FVolumePosition + RemainingOffset, soBeginning);
        Result := Result + RemainingOffset;
        FPosition := Result;
        RemainingOffset := 0;
      end
      else
      begin
        // seek to previous volume
        if FVolumeIndex = 0 then
          Exit;
        // seek to the beginning of current volume
        RemainingOffset := RemainingOffset + FVolumePosition;
        Result := Result - FVolumePosition;
        FPosition := Result;
        FVolumePosition := FVolume.Seek(0, soFromBeginning);
        // load previous volume
        InternalLoadVolume(FVolumeIndex - 1);
        if not Assigned(FVolume) then
          Break;
        Result := Result - FVolume.Size;
        FPosition := Result;
        RemainingOffset := RemainingOffset + FVolume.Size;
      end;
    end
    else if RemainingOffset > 0 then
    begin
      // FPosition < ExpectedPosition, seek forward
      if (FVolumeMaxSize = 0) or ((FVolumePosition + RemainingOffset) < FVolumeMaxSize) then
      begin
        // can seek in current volume
        FVolumePosition := StreamSeek(FVolume, FVolumePosition + RemainingOffset, soBeginning);
        Result := Result + RemainingOffset;
        FPosition := Result;
        RemainingOffset := 0;
      end
      else
      begin
        // seek to next volume
        RemainingOffset := RemainingOffset - FVolumeMaxSize + FVolumePosition;
        Result := Result + FVolumeMaxSize - FVolumePosition;
        FPosition := Result;
        InternalLoadVolume(FVolumeIndex + 1);
        if not Assigned(FVolume) then
          Break;
      end;
    end;
  until RemainingOffset = 0;
end;

procedure TJclSplitStream.SetSize(const NewSize: Int64);
var
  OldVolumeIndex: Integer;
  OldVolumePosition, OldPosition, RemainingSize, VolumeSize: Int64;
begin
  OldVolumeIndex := FVolumeIndex;
  OldVolumePosition := FVolumePosition;
  OldPosition := FPosition;

  RemainingSize := NewSize;
  try
    FVolumeIndex := 0;
    repeat
      InternalLoadVolume(FVolumeIndex);
      if not Assigned(FVolume) then
        Break;
      if (FVolumeMaxSize > 0) and (RemainingSize > FVolumeMaxSize) then
        VolumeSize := FVolumeMaxSize
      else
        VolumeSize := RemainingSize;
      FVolume.Size := VolumeSize;
      RemainingSize := RemainingSize - VolumeSize;

      Inc(FVolumeIndex);
    until RemainingSize = 0;
  finally
    InternalLoadVolume(OldVolumeIndex);
    FPosition := OldPosition;
    if Assigned(FVolume) then
      FVolumePosition := StreamSeek(FVolume, OldVolumePosition, soBeginning);
  end;
end;

function TJclSplitStream.Write(const Buffer; Count: Integer): Longint;
var
  Data: PByte;
  Total, LoopWritten: Integer;
begin
  Result := 0;

  InternalLoadVolume(FVolumeIndex);
  if not Assigned(FVolume) then
    Exit;

  Data := PByte(@Buffer);
  Total := Count;

  repeat
    // do not write more than (VolumeMaxSize) bytes in current stream
    if (FVolumeMaxSize > 0) and ((Count + FVolumePosition) > FVolumeMaxSize) then
      LoopWritten := FVolumeMaxSize - FVolumePosition
    else
      LoopWritten := Count;
    // try to write (Count) bytes from current stream
    LoopWritten := FVolume.Write(Data^, LoopWritten);
    FVolumePosition := FVolumePosition + LoopWritten;
    FPosition := FPosition + LoopWritten;
    Inc(Result, LoopWritten);
    if Result = Total then
      Break;

    // with next volume
    Dec(Count, LoopWritten);
    Inc(Data, LoopWritten);
    InternalLoadVolume(FVolumeIndex + 1);
    if not Assigned(FVolume) then
      Break;
  until False;
end;

//=== { TJclDynamicSplitStream } =============================================

function TJclDynamicSplitStream.GetVolume(Index: Integer): TStream;
begin
  if Assigned(FOnVolume) then
    Result := FOnVolume(Index)
  else
    Result := nil;
end;

function TJclDynamicSplitStream.GetVolumeMaxSize(Index: Integer): Int64;
begin
  if Assigned(FOnVolumeMaxSize) then
    Result := FOnVolumeMaxSize(Index)
  else
    Result := 0;
end;

//=== { TJclStaticSplitStream } ===========================================

constructor TJclStaticSplitStream.Create;
begin
  inherited Create;
  FVolumes := TList.Create;
end;

destructor TJclStaticSplitStream.Destroy;
var
  Index: Integer;
  AVolumeRec: PJclSplitVolume;
begin
  if Assigned(FVolumes) then
  begin
    for Index := 0 to FVolumes.Count - 1 do
    begin
      AVolumeRec := FVolumes.Items[Index];
      if AVolumeRec^.OwnStream then
        AVolumeRec^.Stream.Free;
      Dispose(AVolumeRec);
    end;
    FVolumes.Destroy;
  end;
  inherited Destroy;
end;

function TJclStaticSplitStream.AddVolume(AStream: TStream; AMaxSize: Int64;
  AOwnStream: Boolean): Integer;
var
  AVolumeRec: PJclSplitVolume;
begin
  New(AVolumeRec);
  AVolumeRec^.MaxSize := AMaxSize;
  AVolumeRec^.Stream := AStream;
  AVolumeRec^.OwnStream := AOwnStream;
  Result := FVolumes.Add(AVolumeRec);
end;

function TJclStaticSplitStream.GetVolume(Index: Integer): TStream;
begin
  Result := PJclSplitVolume(FVolumes.Items[Index]).Stream;
end;

function TJclStaticSplitStream.GetVolumeCount: Integer;
begin
  Result := FVolumes.Count;
end;

function TJclStaticSplitStream.GetVolumeMaxSize(Index: Integer): Int64;
begin
  Result := PJclSplitVolume(FVolumes.Items[Index]).MaxSize;
end;

{$ENDIF ~CLR}

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
