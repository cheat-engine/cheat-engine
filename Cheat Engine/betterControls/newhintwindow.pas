unit newhintwindow;

{$mode delphi}

interface

uses
  Classes, SysUtils,forms;


type
  TNewHintwindow=class(THintWindow)
  public
    constructor Create(AOwner: TComponent); override;
  end;



implementation

uses bettercontrols;

constructor TNewHintwindow.Create(AOwner: TComponent);
begin
  inherited create(AOwner);
  Color := clWindow;
  font.color:=clWindowtext;
end;

end.

