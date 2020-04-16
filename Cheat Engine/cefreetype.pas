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
      {$ifdef darwin}
      freetypefontsloaded:=fontCollection.AddFile(extractfiledir(extractfiledir(Application.ExeName))+'/Resources/cour.ttf')<>nil;
      fontCollection.AddFile(extractfiledir(extractfiledir(Application.ExeName))+'/Resources/courbd.ttf');
      {$else}
      freetypefontsloaded:=fontCollection.AddFile(extractfilepath(Application.ExeName)+'cour.ttf')<>nil;
      fontCollection.AddFile(extractfilepath(Application.ExeName)+'courbd.ttf');
      {$endif}
    except
    end;

    if freetypefontsloaded=false then
    begin
      try
        freetypefontsloaded:=FontCollection.AddFile('cour.ttf')<>nil;
        FontCollection.AddFile('courbd.ttf');
      except
      end;
    end;
  end;

  result:=freetypefontsloaded;
end;

{$endif}

end.

