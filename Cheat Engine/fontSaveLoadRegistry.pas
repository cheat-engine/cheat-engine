unit fontSaveLoadRegistry;

{Responsible for saving and loading a font from the registry}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, registry, Graphics;

procedure SaveFontToRegistry(f: TFont; reg: TRegistry);
procedure LoadFontFromRegistry(f: TFont; reg: TRegistry);

implementation



procedure SaveFontToRegistry(f: TFont; reg: TRegistry);
var fd: TFontData;
begin
  fd:=GetFontData(f.handle);
  reg.WriteInteger('Height', fd.Height);
  reg.WriteInteger('Pitch', integer(fd.Pitch));
  reg.WriteInteger('Style', integer(fd.Style));
  reg.WriteInteger('CharSet', integer(fd.CharSet));
  reg.WriteInteger('Quality', integer(fd.Quality));
  reg.WriteString('Name', fd.Name);
  reg.WriteInteger('Orientation', fd.Orientation);
  reg.WriteInteger('Color', f.Color);
end;

procedure LoadFontFromRegistry(f: TFont; reg: TRegistry);
var fd: TFontData;
begin
  try
    f.Height:=reg.ReadInteger('Height');
    f.Name:=reg.ReadString('Name');
    f.Color:=reg.ReadInteger('Color');
    integer(fd.Pitch):=reg.ReadInteger('Pitch');
    integer(fd.Style):=reg.ReadInteger('Style');
    integer(fd.Quality):=reg.ReadInteger('Quality');

    f.Pitch:=fd.Pitch;
    f.style:=fd.style;
    f.Quality:=fd.Quality;

    f.CharSet:=reg.ReadInteger('CharSet');
    f.Orientation:=reg.ReadInteger('Orientation');
  except
  end;
end;

end.

