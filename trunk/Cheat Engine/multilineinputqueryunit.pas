unit multilineinputqueryunit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;




function MultilineInputQuery(const ACaption, APrompt : String; Values : TStrings) : Boolean;

implementation

{$R *.lfm}

{ TfrmMultilineInputQuery }

type
  TfrmMultilineInputQuery = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    lblPrompt: TLabel;
    Memo1: TMemo;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

function MultilineInputQuery(const ACaption, APrompt : String; Values : TStrings) : Boolean;
var f: TfrmMultilineInputQuery;
  i: integer;
begin
  f:=TfrmMultilineInputQuery.Create(application);
  f.Caption:=ACaption;
  f.lblPrompt.caption:=APrompt;
  f.Memo1.Lines.Clear;
  f.memo1.Lines.AddStrings(values);


  i:=f.lblPrompt.Canvas.TextWidth(APrompt);
  if f.ClientWidth<i+6 then
    f.width:=i+6;

  if f.showmodal=mrok then
  begin
    values.Clear;
    values.AddStrings(f.memo1.lines);
  end;

  f.free;
end;

procedure TfrmMultilineInputQuery.Memo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_ESCAPE then
    modalresult:=mrcancel;
end;

end.

