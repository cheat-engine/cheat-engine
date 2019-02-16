unit askToRunLuaScript;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, LuaSyntax;

type

  { TfrmLuaScriptQuestion }

  TfrmLuaScriptQuestion = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox5: TGroupBox;
    Label16: TLabel;
    Panel1: TPanel;
    rbAlways: TRadioButton;
    rbSignedOnly: TRadioButton;
    rbAlwaysAsk: TRadioButton;
    rbNever: TRadioButton;
    script: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    synhighlighter: TSynLuaSyn;
    function getLuaScriptAction: integer;
    procedure setLuaScriptAction(a: integer);
  public
    { public declarations }
    property LuaScriptAction: integer read getLuaScriptAction write setLuaScriptAction;
  end;

implementation

{$R *.lfm}

{ TfrmLuaScriptQuestion }

function TfrmLuaScriptQuestion.getLuaScriptAction: integer;
begin
  result:=0;

  if rbAlways.checked then result:=0 else
  if rbSignedOnly.checked then result:=1 else
  if rbAlwaysAsk.checked then result:=2 else
  if rbNever.checked then result:=3;
end;

procedure TfrmLuaScriptQuestion.setLuaScriptAction(a: integer);
begin
  case a of
    0: rbAlways.checked:=true;
    1: rbSignedOnly.checked:=true;
    2: rbAlwaysAsk.checked:=true;
    3: rbNever.checked:=true;
  end;
end;

procedure TfrmLuaScriptQuestion.FormCreate(Sender: TObject);
begin
  synhighlighter:=TSynLuaSyn.Create(self);
  script.Highlighter:=synhighlighter;
end;

procedure TfrmLuaScriptQuestion.FormDestroy(Sender: TObject);
begin
  script.Highlighter:=nil;
  if synhighlighter<>nil then
    synhighlighter.free;
end;

end.

