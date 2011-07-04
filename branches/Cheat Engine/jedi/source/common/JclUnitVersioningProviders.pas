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
{ The Original Code is JclUnitVersioningProviders.pas.                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is Uwe Schuster.                                      }
{ Portions created by Uwe Schuster are Copyright (C) Uwe Schuster. All rights reserved.            }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Contains a TCustomUnitVersioningProvider implementation                                          }
{                                                                                                  }
{ Unit owner: Uwe Schuster                                                                         }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclUnitVersioningProviders;

{$I jcl.inc}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  JclPeImage,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  Types,
  {$ENDIF LINUX}
  SysUtils, Classes, Contnrs,
  JclUnitVersioning;

type
  { TODO : store compressed? }
  TJclUnitVersioningList = class(TObject)
  private
    FItems: TList;
    function GetCount: Integer;
    function GetItems(AIndex: Integer): PUnitVersionInfo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Info: TUnitVersionInfo);
    procedure Clear;
    function Load(AModule: HMODULE): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function LoadFromDefaultResource(AModule: HMODULE): Boolean;
    {$IFDEF MSWINDOWS}
    function LoadFromDefaultSection(AModule: HMODULE): Boolean;
    {$ENDIF MSWINDOWS}
    procedure SaveToFile(AFileName: string);
    procedure SaveToStream(AStream: TStream);
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: PUnitVersionInfo read GetItems; default;
  end;

  TJclUnitVersioningProviderModule = class(TObject)
  private
    FInfoList: TJclUnitVersioningList;
    FInstance: THandle;
  public
    constructor Create(Instance: THandle);
    destructor Destroy; override;
    property InfoList: TJclUnitVersioningList read FInfoList;
    property Instance: THandle read FInstance;
  end;

  TJclDefaultUnitVersioningProvider = class(TCustomUnitVersioningProvider)
  private
    FModules: TObjectList;
    function IndexOfInstance(Instance: THandle): Integer;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadModuleUnitVersioningInfo(Instance: THandle); override;
    procedure ReleaseModuleUnitVersioningInfo(Instance: THandle); override;
  end;

{$IFDEF MSWINDOWS}
function InsertUnitVersioningSection(const ExecutableFileName: TFileName;
  AUnitList: TJclUnitVersioningList): Boolean;
{$ENDIF MSWINDOWS}

implementation

const
  JclUnitVersioningDataResName = 'JCLUV';

type
  PJclUnitVersioningHeader = ^TJclUnitVersioningHeader;
  TJclUnitVersioningHeader = record
    UnitCount: Integer;
  end;

//=== { TJclUnitVersioningList } =============================================

constructor TJclUnitVersioningList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TJclUnitVersioningList.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TJclUnitVersioningList.Add(Info: TUnitVersionInfo);
var
  UnitVersionInfoPtr: PUnitVersionInfo;
begin
  New(UnitVersionInfoPtr);
  UnitVersionInfoPtr^ := Info;
  FItems.Add(UnitVersionInfoPtr);
end;

procedure TJclUnitVersioningList.Clear;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    Dispose(FItems[I]);
  FItems.Clear;
end;

function TJclUnitVersioningList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TJclUnitVersioningList.GetItems(AIndex: Integer): PUnitVersionInfo;
begin
  Result := FItems[AIndex];
end;

procedure WriteStringToStream(AStream: TStream; const AString: string);
var
  StringLength: Integer;
begin
  if Assigned(AStream) then
  begin
    StringLength := Length(AString);
    AStream.Write(StringLength, SizeOf(StringLength));
    if StringLength > 0 then
      AStream.Write(PChar(AString)^, StringLength);
  end;
end;

function ReadStringFromStream(AStream: TStream; var AString: string): Boolean;
var
  StringLength: Integer;
begin
  Result := False;
  AString := '';
  if Assigned(AStream) then
  begin
    if AStream.Size - AStream.Position >= SizeOf(StringLength) then
    begin
      AStream.Read(StringLength, SizeOf(StringLength));
      if StringLength <= AStream.Size - AStream.Position then
      begin
        if StringLength > 0 then
        begin
          SetLength(AString, StringLength);
          AStream.Read(PChar(AString)^, StringLength);
        end;
        Result := True;
      end;
    end;
  end;
end;

function ReadUnitVersionInfo(AStream: TStream; var AVersionInfo: TUnitVersionInfo): Boolean;
begin
  Result := True;
  with AVersionInfo do
  begin
    Result := Result and ReadStringFromStream(AStream, RCSfile);
    Result := Result and ReadStringFromStream(AStream, Revision);
    Result := Result and ReadStringFromStream(AStream, Date);
    Result := Result and ReadStringFromStream(AStream, LogPath);
    Result := Result and ReadStringFromStream(AStream, Extra);
    Data := nil;
  end;
end;

function TJclUnitVersioningList.Load(AModule: HMODULE): Boolean;
begin
  Result := LoadFromDefaultResource(AModule);
  {$IFDEF MSWINDOWS}
  if not Result then
    Result := LoadFromDefaultSection(AModule);
  {$ENDIF MSWINDOWS}    
end;

function TJclUnitVersioningList.LoadFromDefaultResource(AModule: HMODULE): Boolean;
var
  ResourceStream: TResourceStream;
begin
  Result := False;
  if FindResource(AModule, JclUnitVersioningDataResName, RT_RCDATA) <> 0 then
  begin
    ResourceStream := TResourceStream.Create(AModule, JclUnitVersioningDataResName, RT_RCDATA);
    try
      Result := LoadFromStream(ResourceStream);
    finally
      ResourceStream.Free;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function TJclUnitVersioningList.LoadFromDefaultSection(AModule: HMODULE): Boolean;
var
  PeSectionStream: TJclPeSectionStream;
begin
  Result := False;
  if PeMapImgFindSectionFromModule(Pointer(AModule), JclUnitVersioningDataResName) <> nil then
  begin
    PeSectionStream := TJclPeSectionStream.Create(AModule, JclUnitVersioningDataResName);
    try
      Result := LoadFromStream(PeSectionStream);
    finally
      PeSectionStream.Free;
    end;
  end;
end;
{$ENDIF MSWINDOWS}

function TJclUnitVersioningList.LoadFromStream(AStream: TStream): Boolean;
var
  Header: TJclUnitVersioningHeader;
  UnitsToRead: Integer;
  LastReadOkay: Boolean;
  UnitVersionInfoPtr: PUnitVersionInfo;
begin
  Result := False;
  if Assigned(AStream) then
  begin
    Clear;
    AStream.Read(Header, SizeOf(Header));
    UnitsToRead := Header.UnitCount;
    LastReadOkay := True;
    while (UnitsToRead > 0) and LastReadOkay do
    begin
      New(UnitVersionInfoPtr);
      LastReadOkay := ReadUnitVersionInfo(AStream, UnitVersionInfoPtr^);
      if not LastReadOkay then
        Dispose(UnitVersionInfoPtr)
      else
        FItems.Add(UnitVersionInfoPtr);
      Dec(UnitsToRead);
    end;
    Result := (UnitsToRead = 0) and LastReadOkay;
  end;
end;

procedure TJclUnitVersioningList.SaveToFile(AFileName: string);
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(FileStream);
  finally
    FileStream.Free;
  end;
end;

procedure TJclUnitVersioningList.SaveToStream(AStream: TStream);
var
  UnitVersioningHeader: TJclUnitVersioningHeader;
  I: Integer;
begin
  UnitVersioningHeader.UnitCount := Count;
  AStream.Write(UnitVersioningHeader, SizeOf(UnitVersioningHeader));
  for I := 0 to Pred(Count) do
    with Items[I]^ do
    begin
      WriteStringToStream(AStream, RCSfile);
      WriteStringToStream(AStream, Revision);
      WriteStringToStream(AStream, Date);
      WriteStringToStream(AStream, LogPath);
      WriteStringToStream(AStream, Extra);
    end;
end;

//=== { TJclUnitVersioningProviderModule } ===================================

{$IFDEF MSWINDOWS}
function InsertUnitVersioningSection(const ExecutableFileName: TFileName;
  AUnitList: TJclUnitVersioningList): Boolean;
var
  SectionStream: TMemoryStream;
begin
  SectionStream := TMemoryStream.Create;
  try
    Result := Assigned(AUnitList);
    if Result then
    begin
      AUnitList.SaveToStream(SectionStream);
      Result := PeInsertSection(ExecutableFileName, SectionStream,
        JclUnitVersioningDataResName);
    end;
  finally
    SectionStream.Free;
  end;
end;
{$ENDIF MSWINDOWS}

constructor TJclUnitVersioningProviderModule.Create(Instance: THandle);
var
  I: Integer;
begin
  inherited Create;
  FInstance := Instance;
  FInfoList := TJclUnitVersioningList.Create;
  if FInfoList.Load(Instance) then
    for I := 0 to FInfoList.Count -1 do
      RegisterUnitVersion(Instance, FInfoList[I]^);
end;

destructor TJclUnitVersioningProviderModule.Destroy;
begin
  FInfoList.Free;
  inherited Destroy;
end;

//=== { TJclDefaultUnitVersioningProvider } ==================================

constructor TJclDefaultUnitVersioningProvider.Create;
begin
  inherited Create;
  FModules := TObjectList.Create;
end;

destructor TJclDefaultUnitVersioningProvider.Destroy;
begin
  FModules.Free;
  inherited Destroy;
end;

function TJclDefaultUnitVersioningProvider.IndexOfInstance(Instance: THandle): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FModules.Count - 1 do
    if TJclUnitVersioningProviderModule(FModules[I]).Instance = Instance then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJclDefaultUnitVersioningProvider.LoadModuleUnitVersioningInfo(Instance: THandle);
begin
  if IndexOfInstance(Instance) < 0 then
    FModules.Add(TJclUnitVersioningProviderModule.Create(Instance));
end;

procedure TJclDefaultUnitVersioningProvider.ReleaseModuleUnitVersioningInfo(Instance: THandle);
var
  Idx: Integer;
begin
  Idx := IndexOfInstance(Instance);
  if Idx <> -1 then
    FModules.Delete(Idx);
end;

const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclUnitVersioningProviders.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\common';
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);

end.
