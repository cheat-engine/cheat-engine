program ceregreset;

//{$APPTYPE CONSOLE}

uses
  SysUtils,
  windows,registry;


var reg: tregistry;
begin
  { TODO -oUser -cConsole Main : Insert code here }
  try
    reg:=tregistry.Create;
    reg.RootKey:=HKEY_CURRENT_USER;
    reg.DeleteKey('\Software\Cheat Engine');
    messagebox(0,'Successfully reset the settings of Cheat Engine','Registry Reset',0);
  except
    messagebox(0,'Couldn''t reset the settings','Registry Reset',0);  

  end;
end.
 