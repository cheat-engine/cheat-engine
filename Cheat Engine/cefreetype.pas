unit cefreetype;

{$mode delphi}

interface

uses
  Classes, SysUtils;

{$ifdef USELAZFREETYPE}
function loadCEFreeTypeFonts: boolean;
{$endif}

implementation

{$ifdef USELAZFREETYPE}
uses forms, FPCanvas, EasyLazFreeType, LazFreeTypeFontCollection, LazFreeTypeIntfDrawer,
  LazFreeTypeFPImageDrawer, IntfGraphics, fpimage, graphtype;

var
  freetypefontsloaded: boolean;

function loadCEFreeTypeFonts: boolean;
begin
  if freetypefontsloaded=false then
  begin
    freetypefontsloaded:=false;
    try
      freetypefontsloaded:=fontCollection.AddFile(extractfiledir(extractfiledir(Application.ExeName))+'/Resources/couri.ttf')<>nil;
    except
    end;

    if freetypefontsloaded=false then
    begin
      try
        freetypefontsloaded:=FontCollection.AddFile('couri.ttf')<>nil;
      except
      end;
    end;
  end;

  result:=freetypefontsloaded;
end;

{$endif}

end.

