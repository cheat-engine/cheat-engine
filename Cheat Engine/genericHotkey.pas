unit genericHotkey;

{$mode delphi}

interface

uses
  Classes, SysUtils, cefuncproc, commonTypeDefs;

type TGenericHotkey=class
  public
    keys: TKeycombo;
    onNotify: TNotifyEvent;
    procedure setDelayBetweenActivate(delay: integer);
    function getDelayBetweenActivate: integer;
    constructor create(routine: TNotifyEvent; keys: TKeycombo);
    destructor destroy; override;
  published
    property delayBetweenActivate: integer read getDelayBetweenActivate write setDelayBetweenActivate;
end;

implementation

uses mainunit, hotkeyhandler, LuaCaller;

procedure TGenericHotkey.setDelayBetweenActivate(delay: integer);
var ghi: PHotkeyItem;
begin
  CSKeys.enter;
  try
    ghi:=getGenericHotkeyKeyItem(self);
    if ghi<>nil then
      ghi.delayBetweenActivate:=delay;
  finally
    CSKeys.leave;
  end;
end;

function TGenericHotkey.getDelayBetweenActivate: integer;
var ghi: PHotkeyItem;
begin
  CSKeys.enter;
  try
    ghi:=getGenericHotkeyKeyItem(self);
    if ghi<>nil then
      result:=ghi.delayBetweenActivate
    else
      result:=0;
  finally
    CSKeys.leave;
  end;
end;

constructor TGenericHotkey.create(routine: TNotifyEvent; keys: TKeycombo);
begin
  //register hotkey
  onNotify:=routine;
  self.keys:=keys;
  RegisterHotKey2(mainform.handle, -1, keys, nil,self);
end;

destructor TGenericHotkey.destroy;
begin
  //unregister hotkey
  UnregisterGenericHotkey(self);

  CleanupLuaCall(tmethod(onNotify));

  Inherited destroy;
end;

end.

