unit frmMainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,settingsunit;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses PatcherUnit, MemoryTrainerUnit;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if openself then
  begin
    //it is a patcher
    frmPatcher:=TfrmPatcher.create(self);
    frmPatcher.show;
  end
  else
  begin
    //it is a memory trainer
    frmMemoryTrainer:=TFrmMemoryTrainer.create(self);
    if frmmemorytrainer.viewdefault then
      frmMemoryTrainer.show;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  settingsunit.trainerfile.Free;
end;

end.
