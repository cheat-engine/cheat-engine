unit Unit3;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls;

type
  TWaitform = class(TForm)
    Animate1: TAnimate;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    closeit: boolean;
  end;

var
  Waitform: TWaitform;

implementation

uses Unit2;

{$R *.DFM}

procedure TWaitform.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=closeit;
  if canclose then animate1.Active:=false;
end;

procedure TWaitform.FormShow(Sender: TObject);
begin
  animate1.Active:=true;
  mainform.enableD:=false;
end;

procedure TWaitform.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  mainform.Enabled:=true;
end;

end.
