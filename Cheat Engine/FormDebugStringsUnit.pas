unit FormDebugStringsUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources;

type

  { TFormDebugStrings }

  TFormDebugStrings = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormDebugStrings: TFormDebugStrings;

implementation
    uses Windows;

procedure TFormDebugStrings.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormDebugStrings.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=VK_ESCAPE)then
    self.close;
end;

initialization
  {$i FormDebugStringsUnit.lrs}

end.
