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
{ The Original Code is JclFont.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Jean-Fabien Connault.                              }
{ Portions created by these individuals are Copyright (C) of these individuals.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains function to initialize TFont objects from standard font styles.               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-11-13 20:39:23 +0100 (mar., 13 nov. 2007)                          $ }
{ Revision:      $Rev:: 2216                                                                     $ }
{ Author:        $Author:: ahuser                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclFont;

interface

type
  TFontType  = (ftAuto, ftCaption, ftContent);

procedure SetObjectFontToSystemFont(const AObject: TObject; const FontType: TFontType = ftAuto);

implementation

uses
  StdCtrls, ComCtrls, Graphics, TypInfo,
  JclSysUtils, JclSysInfo;

procedure SetCaptionFont(const AObjectFont: TFont);
begin
  if IsWinVista or IsWinServer2008 then
  begin
    AObjectFont.Name := 'Segoe UI';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Tahoma';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'MS Sans Serif';
    AObjectFont.Size := 8;
  end;
end;

procedure SetContentFont(const AObjectFont: TFont);
begin
  if IsWinVista or IsWinServer2008 then
  begin
    AObjectFont.Name := 'Calibri';
    AObjectFont.Size := 9;
  end
  else if IsWinXP or IsWin2k or IsWin2003 then
  begin
    // MS Shell Dlg 2
    AObjectFont.Name := 'Verdana';
    AObjectFont.Size := 8;
  end
  else
  begin
    // MS Shell Dlg
    AObjectFont.Name := 'MS Sans Serif';
    AObjectFont.Size := 8;
  end;
end;

procedure SetObjectFontToSystemFont(const AObject: TObject; const FontType: TFontType);
var
  AObjectFont: TFont;
  AFontType:   TFontType;
begin
  if (AObject.ClassType = TFont) then
    AObjectFont := TFont(AObject)
  else
    AObjectFont := TFont(GetObjectProp(AObject, 'Font', TFont));

  if (FontType = ftAuto) then
  begin
    if (AObject.ClassType = TMemo) or (AObject.ClassType = TRichEdit) then
      AFontType := ftContent
    else
      AFontType := ftCaption;
  end
  else
    AFontType := FontType;

  if (AFontType = ftCaption) then
  begin
    SetCaptionFont(AObjectFont);
  end
  else if (AFontType = ftContent) then
  begin
    SetContentFont(AObjectFont);
  end;
end;

end.
