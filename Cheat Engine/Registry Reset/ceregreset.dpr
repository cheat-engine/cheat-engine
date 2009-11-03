program ceregreset;

//{$APPTYPE CONSOLE}

uses
  SysUtils,
  windows,registry;


var reg: tregistry;
    silent: boolean;
    i: integer;
begin
  silent:=false;
  { TODO -oUser -cConsole Main : Insert code here }


  for i:=0 to ParamCount-1 do
    if lowercase(ParamStr(i))='-silent' then silent:=true;
    
  try
    reg:=tregistry.Create;
    reg.RootKey:=HKEY_CURRENT_USER;
    reg.DeleteKey('\Software\Cheat Engine');
    if not silent then
      messagebox(0,'Successfully reset the settings of Cheat Engine','Registry Reset',0);

  except
    if not silent then
      messagebox(0,'Couldn''t reset the settings','Registry Reset',0);
  end;
end.
 