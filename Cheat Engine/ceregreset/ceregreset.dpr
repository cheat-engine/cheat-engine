program ceregreset;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{$ifndef DEBUG}
  {$ifdef cpu64}
    {$error only 32-bit may compile this}
  {$endif}
{$endif}

//{$APPTYPE CONSOLE}

uses
  classes,
  SysUtils,
  windows,
  registry;

const regname: string={$ifdef altname}'Runtime Modifier'{$else}'Cheat Engine'{$endif};


procedure DeleteFullRegKey(reg: Tregistry; keyname: string);
var i: integer;
    r: Tregistry;
    s: tstringlist;
begin
  r:=TRegistry.Create;
  r.RootKey:=HKEY_CURRENT_USER;
  if r.OpenKey(reg.CurrentPath+'\'+keyname, false) then
  begin
    s:=tstringlist.create;
    try
      r.GetKeyNames(s);
    except
      exit;
    end;
    for i:=0 to s.count-1 do
      DeleteFullRegKey(r, s[i]);

    s.free;
    r.free;
  end;

  reg.DeleteKey(keyname);
end;

procedure resetSettings;
var reg: tregistry;
    silent: boolean;
    i,j: integer;
    names: tstringlist;
    customtypenames: tstringlist;

    deletecustomtypes: boolean;
    needsToSaveStuff: boolean;
    deleteversioncheck: boolean;
    s: string;

    ki: TRegKeyInfo;
begin
  silent:=false;

  needsToSaveStuff:=false;
  deletecustomtypes:=true;
  deleteversioncheck:=true;


  for i:=1 to ParamCount do
  begin
    if lowercase(ParamStr(i))='-silent' then silent:=true;
    if lowercase(ParamStr(i))='-dontdeletecustomtypes' then deletecustomtypes:=false;
    if lowercase(ParamStr(i))='-dontdeleteversioncheck' then deleteversioncheck:=false;
  end;

  try
    reg:=tregistry.Create;
    reg.RootKey:=HKEY_CURRENT_USER;

    //check that a custom type exists
    if reg.OpenKey('\Software\'+regname, false) then
    begin
      //get rid of individual things
      names:=tstringlist.create;
      reg.GetKeyNames(names);
      for i:=0 to names.count-1 do
      begin
        s:=names[i];

        if (s='VersionCheck') and (deleteversioncheck=false) then continue;


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
            reg.OpenKey('\Software\'+regname, false);
          end;

          if needsToSaveStuff=false then
            reg.DeleteKey(names[i]);
        end
        else
          deleteFullRegKey(reg, names[i]);
      end;


      reg.GetValueNames(names);
      for i:=0 to names.count-1 do
        reg.DeleteValue(names[i]);

      names.free;
    end;

    if needsToSaveStuff=false then
      reg.DeleteKey('\Software\'+regname); //get rid of the whole key

    if not silent then
      messagebox(0,pchar('Successfully reset the settings of '+regname),'Registry Reset',0);

    reg.free;
  except
    if not silent then
      messagebox(0,'Couldn''t reset the settings','Registry Reset',0);
  end;
end;




{$R *.res}

begin
  resetSettings;
end.
 
