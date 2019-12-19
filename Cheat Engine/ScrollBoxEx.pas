unit ScrollBoxEx;

{$mode delphi}

interface

uses
  Classes, SysUtils, lmessages, forms, messages;

type
  TScrollBox=class(forms.TScrollbox)
  private
    procedure WMVScroll(var Msg: TMessage); message WM_VSCROLL;
  public
    OnVScroll: TNotifyEvent;
end;

implementation

procedure TScrollBox.WMVScroll(var Msg: TMessage);
begin
  if assigned(OnVScroll) then OnVScroll(self);

  inherited;
end;

end.

