unit multilineinputqueryunit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;




function MultilineInputQuery(const ACaption, APrompt : String; Values : TStrings) : Boolean; overload;
function MultilineInputQuery(const ACaption, APrompt : String; var Value : String) : Boolean; overload;

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

function MultilineInputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
var values: Tstringlist;
begin
  values:=tstringlist.create;
  try
    values.Text:=Value;
    result:=MultilineInputQuery(ACaption, APrompt, Values);
    if result then
      value:=values.Text;
  finally
    values.free;
  end;
end;

function MultilineInputQuery(const ACaption, APrompt : String; Values : TStrings) : Boolean;
var f: TfrmMultilineInputQuery;
  i: integer;
begin
  result:=false;
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
    result:=true;
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

