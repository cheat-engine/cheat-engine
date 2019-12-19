unit frmWatchListAddEntryUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TfrmWatchListAddEntry }

  TfrmWatchListAddEntry = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    edtExpression: TEdit;
    Panel1: TPanel;
    rgType: TRadioGroup;
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmWatchListAddEntry: TfrmWatchListAddEntry;

implementation

{$R *.lfm}

{ TfrmWatchListAddEntry }

procedure TfrmWatchListAddEntry.FormShow(Sender: TObject);
begin
  edtExpression.SetFocus;
end;

end.

