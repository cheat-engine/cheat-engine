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
{ The Original Code is JclIniFiles.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is John C Molyneux.                                   }
{ Portions created by John C Molyneux are Copyright (C) John C Molyneux.                           }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Eric S. Fisher                                                                                 }
{   John C Molyneux                                                                                }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclIniFiles;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes, IniFiles;

// Initialization (ini) Files
function IniReadBool(const FileName, Section, Line: string): Boolean;              // John C Molyneux
function IniReadInteger(const FileName, Section, Line: string): Integer;           // John C Molyneux
function IniReadString(const FileName, Section, Line: string): string;             // John C Molyneux
procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);     // John C Molyneux
procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);  // John C Molyneux
procedure IniWriteString(const FileName, Section, Line, Value: string);            // John C Molyneux

// Initialization (ini) Files helper routines
procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclIniFiles.pas $';
    Revision: '$Revision: 2175 $';
    Date: '$Date: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF CLR}
type
  TIniFile = TMemIniFile;
{$ENDIF CLR}

// Initialization Files
function IniReadBool(const FileName, Section, Line: string): Boolean;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadBool(Section, Line, False);
  finally
    Ini.Free;
  end;
end;

function IniReadInteger(const FileName, Section, Line: string): Integer;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadInteger(Section, Line, 0);
  finally
    Ini.Free;
  end;
end;

function IniReadString(const FileName, Section, Line: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Result := Ini.ReadString(Section, Line, '');
  finally
    Ini.Free;
  end;
end;

procedure IniWriteBool(const FileName, Section, Line: string; Value: Boolean);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteBool(Section, Line, Value);
    {$IFDEF CLR}
    Ini.UpdateFile;
    {$ENDIF CLR}
  finally
    Ini.Free;
  end;
end;

procedure IniWriteInteger(const FileName, Section, Line: string; Value: Integer);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteInteger(Section, Line, Value);
    {$IFDEF CLR}
    Ini.UpdateFile;
    {$ENDIF CLR}
  finally
    Ini.Free;
  end;
end;

procedure IniWriteString(const FileName, Section, Line, Value: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString(Section, Line, Value);
    {$IFDEF CLR}
    Ini.UpdateFile;
    {$ENDIF CLR}
  finally
    Ini.Free;
  end;
end;

// Initialization (ini) Files helper routines
const
  ItemCountName = 'Count';

procedure IniReadStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  Count, I: Integer;
begin
  with IniFile do
  begin
    Strings.BeginUpdate;
    try
      Strings.Clear;
      Count := ReadInteger(Section, ItemCountName, 0);
      for I := 0 to Count - 1 do
        Strings.Add(ReadString(Section, IntToStr(I), ''));
    finally
      Strings.EndUpdate;
    end;
  end;
end;

procedure IniWriteStrings(IniFile: TCustomIniFile; const Section: string; Strings: TStrings);
var
  I: Integer;
begin
  with IniFile do
  begin
    EraseSection(Section);
    WriteInteger(Section, ItemCountName, Strings.Count);
    for I := 0 to Strings.Count - 1 do
      WriteString(Section, IntToStr(I), Strings[I]);
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
