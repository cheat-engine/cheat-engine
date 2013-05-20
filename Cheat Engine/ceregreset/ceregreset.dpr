program ceregreset;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$ifdef cpu64}
//{$error only 32-bit may compile this}
{$endif}

//{$APPTYPE CONSOLE}

uses
  classes,
  SysUtils,
  windows,
  registry;



var reg: tregistry;
    silent: boolean;
    i,j: integer;
    names: tstringlist;
    customtypenames: tstringlist;

    deletecustomtypes: boolean;
    needsToSaveStuff: boolean;
    s: string;

    ki: TRegKeyInfo;
begin
  silent:=false;
  { TODO -oUser -cConsole Main : Insert code here }

  needsToSaveStuff:=false;
  deletecustomtypes:=true;


  for i:=1 to ParamCount do
  begin
    if lowercase(ParamStr(i))='-silent' then silent:=true;
    if lowercase(ParamStr(i))='-dontdeletecustomtypes' then deletecustomtypes:=false;
  end;

  try
    reg:=tregistry.Create;
    reg.RootKey:=HKEY_CURRENT_USER;

    //check that a custom type exists
    if reg.OpenKey('\Software\Cheat Engine', false) then
    begin
      //get rid of individual things
      names:=tstringlist.create;
      reg.GetKeyNames(names);
      for i:=0 to names.count-1 do
      begin
        s:=names[i];
        if (s='CustomTypes') then
        begin
          if reg.OpenKey(s,false) then
          begin
            if reg.GetKeyInfo(ki) then
              needsToSaveStuff:=(deletecustomtypes=false) and (ki.NumSubKeys>0);

            if deletecustomtypes then
            begin
              customtypenames:=tstringlist.create;
              reg.GetKeyNames(customtypenames);
              for j:=0 to customtypenames.count-1 do
                reg.DeleteKey(customtypenames[j]);


              customtypenames.free;
            end;

            reg.closekey;
            reg.OpenKey('\Software\Cheat Engine', false);
          end;

          if needsToSaveStuff=false then
            reg.DeleteKey(names[i]);
        end
        else
        reg.DeleteKey(names[i]);
      end;


      reg.GetValueNames(names);
      for i:=0 to names.count-1 do
        reg.DeleteValue(names[i]);

      names.free;
    end;

    if needsToSaveStuff=false then
      reg.DeleteKey('\Software\Cheat Engine'); //get rid of the whole key

    if not silent then
      messagebox(0,'Successfully reset the settings of Cheat Engine','Registry Reset',0);

    reg.free;
  except
    if not silent then
      messagebox(0,'Couldn''t reset the settings','Registry Reset',0);
  end;
end.
 
