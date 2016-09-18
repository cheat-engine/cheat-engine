unit frmBusyUnit;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmBusy }

  TfrmBusy = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    WaitForHandle: THandle;
  end;

var
  frmBusy: TfrmBusy;

implementation

{$R *.lfm}

{ TfrmBusy }

procedure TfrmBusy.FormShow(Sender: TObject);
begin
  timer1.enabled:=true;
end;

procedure TfrmBusy.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  closeaction:=cafree;
end;

procedure TfrmBusy.FormDestroy(Sender: TObject);
begin
  frmBusy:=nil;
end;

procedure TfrmBusy.Timer1Timer(Sender: TObject);
begin
  if (WaitForHandle<>0) and (WaitForSingleObject(WaitForHandle, 50)=WAIT_OBJECT_0) then
    modalresult:=mrok;
end;

end.

