unit genericHotkey;

{$mode delphi}

interface

uses
  Classes, SysUtils, cefuncproc;

type TGenericHotkey=class
  public
    keys: TKeycombo;
    onNotify: TNotifyEvent;

    constructor create(routine: TNotifyEvent; keys: TKeycombo);
    destructor destroy; override;
end;

implementation

uses mainunit, hotkeyhandler, LuaCaller;

constructor TGenericHotkey.create(routine: TNotifyEvent; keys: TKeycombo);
begin
  //register hotkey
  onNotify:=routine;
  self.keys:=keys;
  RegisterHotKey2(mainform.handle, 0, keys, nil,self);
end;

destructor TGenericHotkey.destroy;
begin
  //unregister hotkey
  UnregisterGenericHotkey(self);

  CleanupLuaCall(tmethod(onNotify));

  Inherited destroy;
end;

end.

