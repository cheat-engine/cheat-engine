unit JvDesignClip;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  LCLProc, LCLType, LResources, LCLIntf, LMessages, Classes;

type
  TJvDesignComponentClipboard = class(TObject)
  protected
    Stream: TMemoryStream;
    FParentComponent: TComponent;
    procedure Close;
    procedure Open;
    procedure ReadError(Reader: TReader; const Msg: string; var Handled: Boolean);
  public
    constructor Create(ParentComponent: TComponent);

    function GetComponent: TComponent;
    procedure CloseRead;
    procedure CloseWrite;
    procedure OpenRead;
    procedure OpenWrite;
    procedure SetComponent(InComponent: TComponent);
  end;

function DesignLoadComponentFromBinaryStream(InStream: TStream;
  InComponent: TComponent; InOnError: TReaderError): TComponent;
procedure DesignSaveComponentToBinaryStream(InStream: TStream; InComponent: TComponent);
procedure DesignCopyStreamFromClipboard(InFmt: Cardinal; InS: TStream);
procedure DesignCopyStreamToClipboard(InFmt: Cardinal; InS: TStream);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jvcl.svn.sourceforge.net/svnroot/jvcl/tags/JVCL3_39/run/JvDesignClip.pas $';
    Revision: '$Revision: 12515 $';
    Date: '$Date: 2009-09-23 09:51:16 +0200 (mer., 23 sept. 2009) $';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, Clipbrd,
  JvDesignUtils;

var
  CF_COMPONENTSTREAM: UINT;

procedure DesignSaveComponentToBinaryStream(InStream: TStream; InComponent: TComponent);
var
  MS: TMemoryStream;
  SZ: Int64;
begin
  MS := TMemoryStream.Create;
  try
    MS.WriteComponent(InComponent);
    MS.Position := 0;
    SZ := MS.Size;
    InStream.Write(SZ, SizeOf(SZ));
    InStream.CopyFrom(MS, SZ);
  finally
    MS.Free;
  end;
end;

function DesignLoadComponentFromBinaryStream(InStream: TStream;
  InComponent: TComponent; InOnError: TReaderError): TComponent;
var
  MS: TMemoryStream;
  SZ: Int64;
begin
  InStream.Read(SZ, SizeOf(SZ));
  MS := TMemoryStream.Create;
  try
    MS.CopyFrom(InStream, SZ);
    MS.Position := 0;
    with TReader.Create(MS, 4096) do
    try
      Parent := InComponent;
      OnError := InOnError;
      Result := ReadRootComponent(nil);
    finally
      Free;
    end;
  finally
    MS.Free;
  end;
end;

procedure DesignCopyStreamToClipboard(InFmt: Cardinal; InS: TStream);
var
  HMem: THandle;
  PMem: Pointer;
begin
  Clipboard.Open;
  Clipboard.AddFormat( InFmt, InS);
  Clipboard.Close;
{  InS.Position := 0;
  HMem := GlobalAlloc(GHND or GMEM_DDESHARE, InS.Size);
  if HMem <> 0 then
  begin
    PMem := GlobalLock(HMem);
    if PMem <> nil then
    begin
      InS.Read(PMem^, InS.Size);
      InS.Position := 0;
      GlobalUnlock(HMem);
      Clipboard.Open;
      try
        Clipboard.SetAsHandle(InFmt, HMem);
      finally
        Clipboard.Close;
      end;
    end
    else
    begin
      GlobalFree(HMem);
      OutOfMemoryError;
    end;
  end else
    OutOfMemoryError; }
end;

procedure DesignCopyStreamFromClipboard(InFmt: Cardinal; InS: TStream);
var
  HMem: THandle;
  PMem: Pointer;
begin
  Clipboard.GetFormat(InFmt, InS);
 { HMem := Clipboard.GetAsHandle(InFmt);
  if HMem <> 0 then
  begin
    PMem := GlobalLock(HMem);
    if PMem <> nil then
    begin
      InS.Write(PMem^, GlobalSize(HMem));
      InS.Position := 0;
      GlobalUnlock(HMem);
    end;
  end; }
end;

//=== { TJvDesignComponentClipboard } ========================================

procedure TJvDesignComponentClipboard.Close;
begin
  Stream.Free;
  Clipboard.Close;
end;

procedure TJvDesignComponentClipboard.CloseRead;
begin
  Close;
end;

procedure TJvDesignComponentClipboard.CloseWrite;
begin
  DesignCopyStreamToClipboard(CF_COMPONENTSTREAM, Stream);
  Close;
end;

constructor TJvDesignComponentClipboard.Create(ParentComponent: TComponent);
begin
  inherited Create;

  FParentComponent := ParentComponent;
end;

function TJvDesignComponentClipboard.GetComponent: TComponent;
begin
  if Stream.Position < Stream.Size then
    Result := DesignLoadComponentFromBinaryStream(Stream, FParentComponent,
    TReaderError( @ReadError))
  else
    Result := nil;
end;

procedure TJvDesignComponentClipboard.Open;
begin
  Clipboard.Open;
  Stream := TMemoryStream.Create;
end;

procedure TJvDesignComponentClipboard.OpenRead;
begin
  Open;
  DesignCopyStreamFromClipboard(CF_COMPONENTSTREAM, Stream);
  stream.Position:=0;
end;

procedure TJvDesignComponentClipboard.OpenWrite;
begin
  Open;
end;

procedure TJvDesignComponentClipboard.ReadError(Reader: TReader;
  const Msg: string; var Handled: Boolean);
begin
  Handled := True;
end;

procedure TJvDesignComponentClipboard.SetComponent(InComponent: TComponent);
begin
  DesignSaveComponentToBinaryStream(Stream, InComponent);
end;

initialization
  { The following string should not be localized }
  CF_COMPONENTSTREAM := RegisterClipboardFormat('Delphi Components');
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}

finalization
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.
