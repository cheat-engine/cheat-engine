unit multilineinputqueryunit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport, LCLIntf,
  {$endif}
  {$ifdef windows}
  win32proc, windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  LCLType, math;

{ TfrmMultilineInputQuery }


function MultilineInputQuery(const ACaption, APrompt : String; Values : TStrings) : Boolean; overload;
function MultilineInputQuery(const ACaption, APrompt : String; var Value : String) : Boolean; overload;

implementation

{$R *.lfm}

type
  TfrmMultilineInputQuery = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    lblPrompt: TLabel;
    Memo1: TMemo;
    procedure Memo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
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

procedure TfrmMultilineInputQuery.FormShow(Sender: TObject);
const CCHILDREN_TITLEBAR=5;
type
  TTitleBarInfoEx=record
    cbSize: DWORD;
    rcTitleBar: TRECT;
    rgstate: array [0..CCHILDREN_TITLEBAR] of DWORD;
    rgrect: array [0..CCHILDREN_TITLEBAR] of TRECT;
  end;

var
  tbi: TTITLEBARINFOEX;
  i: integer;
  widthneeded: integer;
begin
  widthneeded:=canvas.TextWidth(' '+caption+' ');

  Memo1.Constraints.MinHeight:=canvas.TextHeight('X')*3;
  constraints.MinWidth:=max(button1.Width+button2.width+panel2.width+32, widthneeded+GetSystemMetrics(SM_CXSIZE)*2+GetSystemMetrics(SM_CXMENUSIZE));

  {$ifdef windows}
  if WindowsVersion>=wvVista then
  begin
    tbi.cbSize:=sizeof(tbi);
    sendmessage(handle, WM_GETTITLEBARINFOEX, 0, ptruint(@tbi));


    autosize:=false;
    i:=tbi.rcTitleBar.Right-tbi.rcTitleBar.Left;
    dec(i,tbi.rgrect[5].Right-tbi.rgrect[5].left);
    dec(i,tbi.rgrect[3].Right-tbi.rgrect[3].left);
    dec(i,tbi.rgrect[2].Right-tbi.rgrect[2].left);
    dec(i, GetSystemMetrics(SM_CXSIZE));
    dec(i, GetSystemMetrics(SM_CXPADDEDBORDER));
    dec(i, GetSystemMetrics(SM_CXBORDER));

    Width:=width+(widthneeded-i);
  end;
  {$endif}
end;

end.

