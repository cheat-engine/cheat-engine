unit frmHotkeyExUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, cefuncproc;

type

  { TfrmHotkeyEx }

  TfrmHotkeyEx = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    edtHotkey: TEdit;
    Label1: TLabel;
    procedure edtHotkeyKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
    newhotkey: tkeycombo;
  end; 



implementation

{$R *.lfm}

{ TfrmHotkeyEx }

procedure TfrmHotkeyEx.edtHotkeyKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var i: integer;
begin
  if newhotkey[4]=0 then
  begin
    for i:=0 to 4 do
      if newhotkey[i]=0 then
      begin
        newhotkey[i]:=key;
        break;
      end else
      if newhotkey[i]=key then break;
  end;

  edtHotkey.Text:=ConvertKeyComboToString(newhotkey);
end;

end.

