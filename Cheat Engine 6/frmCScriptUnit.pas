unit frmCScriptUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Menus, LResources, underc;

type
  TfrmCScript = class(TForm)
    Panel1: TPanel;
    edtCommand: TEdit;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Options1: TMenuItem;
    Verbose1: TMenuItem;
    Savelog1: TMenuItem;
    Loadandparsescript1: TMenuItem;
    Saveinput1: TMenuItem;
    Memo1: TMemo;
    Clearallpreviouscommands1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure edtCommandKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCScript: TfrmCScript;

implementation


procedure TfrmCScript.FormCreate(Sender: TObject);
begin
  scriptengine.beginscript;
  panel1.ClientHeight:=edtCommand.Height;
end;

procedure TfrmCScript.Panel1Resize(Sender: TObject);
begin
  edtCommand.Width:=panel1.ClientWidth;
end;

procedure TfrmCScript.edtCommandKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=13 then
  begin
    memo1.Lines.Add(edtcommand.text);
    if scriptengine.execute_command(edtcommand.text) then
    begin
      edtCommand.Clear;
      memo1.Lines.Add(scriptengine.getResult);
    end
    else
    begin
      memo1.Lines.Add(scriptengine.getError);
    end;
  end;
end;

procedure TfrmCScript.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
  scriptengine.endScript;
end;

initialization
  {$i frmCScriptUnit.lrs}

end.
