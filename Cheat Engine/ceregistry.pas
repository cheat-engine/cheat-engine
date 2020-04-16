unit ceregistry;

{
Wrapper to replace the creating and destroying of default level registry objects with a uniform method
}

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macPort,
  {$endif}
  Classes, SysUtils, registry;

type
  TCEReg=class
  private
    reg: TRegistry;
    openedregistry: boolean;
    needsforce: boolean;
    triedforce: boolean;
    lastforce: qword;
    function getRegistry(force: boolean):boolean;
  public
    procedure writeBool(registryValueName: string; value: boolean);
    function readBool(registryValueName:string; def: boolean=false): boolean;
    procedure writeInteger(registryValueName: string; value: integer);
    function readInteger(registryValueName:string; def: integer=0): integer;
    procedure writeString(registryValueName: string; value: string);
    function readString(registryValueName:string; def: string=''): string;
  end;

var cereg: TCEReg;

implementation

function TCEReg.getRegistry(force: boolean):boolean;
begin
  {$ifdef darwin}
  //all registry objects access the same object. and that object has the current key set...
  if reg<>nil then
    reg.OpenKey('\Software\Cheat Engine\', false);


  {$endif}

  if not openedregistry then
  begin
    if triedforce and (gettickcount64<lastforce+2000) then exit(false);
    if needsforce and (force=false) then exit(false); //don't bother

    if reg=nil then
      reg:=tregistry.create;

    openedregistry:=reg.OpenKey('\Software\Cheat Engine\', force);

    if (not openedregistry) then
    begin
      if force then
      begin
        triedforce:=true;
        lastforce:=gettickcount64; //don't bother with trying for a few seconds (perhaps the user will fix it though ...)
      end
      else
        needsforce:=true;
    end;

    {$ifdef darwin}
    macPortFixRegPath;
    {$endif}
  end;


  result:=openedregistry;
end;

function TCEReg.readBool(registryValueName: string; def: boolean=false): boolean;
begin
  result:=def;
  if getregistry(false) and (reg.ValueExists(registryValueName)) then
  begin
    try
      result:=reg.ReadBool(registryValueName);
    except
    end;
  end;
end;

procedure TCEReg.writeBool(registryValueName: string; value: boolean);
begin
  if getregistry(true) then
  begin
    try
      reg.WriteBool(registryValueName, value);
    except
    end;
  end;
end;

function TCEReg.readInteger(registryValueName: string; def: integer=0): integer;
begin
  result:=def;
  if getregistry(false) and (reg.ValueExists(registryValueName)) then
    result:=reg.ReadInteger(registryValueName);
end;

procedure TCEReg.writeInteger(registryValueName: string; value: integer);
begin
  if getregistry(true) then
    reg.WriteInteger(registryValueName, value);
end;

function TCEReg.readString(registryValueName: string; def: string=''): string;
begin
  result:=def;
  if getregistry(false) and (reg.ValueExists(registryValueName)) then
    result:=reg.ReadString(registryValueName);
end;

procedure TCEReg.writeString(registryValueName: string; value: string);
begin
  if getregistry(true) then
    reg.WriteString(registryValueName, value);
end;

initialization
  cereg:=TCEReg.Create;

end.

