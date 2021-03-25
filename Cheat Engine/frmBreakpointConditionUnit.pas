unit frmBreakpointConditionUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, betterControls;

type

  { TfrmBreakpointCondition }

  TfrmBreakpointCondition = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtEasy: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    mComplex: TMemo;
    PageControl1: TPageControl;
    rbEasy: TRadioButton;
    rbComplex: TRadioButton;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rbEasyChange(Sender: TObject);

  private
    { private declarations }
    loadedFormPosition: boolean;
  public
    { public declarations }
  end;


implementation

uses CEFuncProc, math;

{ TfrmBreakpointCondition }

procedure TfrmBreakpointCondition.FormCreate(Sender: TObject);
var i: integer;
begin
  for i:=0 to pagecontrol1.PageCount-1 do
    pagecontrol1.Pages[i].TabVisible:=false;

  loadedFormPosition:=LoadFormPosition(self);
end;

procedure TfrmBreakpointCondition.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmBreakpointCondition.FormShow(Sender: TObject);
var
  minh: integer;
  minw: integer;

begin
  if loadedFormPosition=false then
  begin
    autosize:=false;

    minh:=canvas.TextHeight(label1.Caption)*15;
    minw:=canvas.TextWidth(label1.Caption+'          ');

    if clientwidth<minw then
      clientwidth:=minw;

    if clientheight<minh then
      clientheight:=minh;
  end;
end;

procedure TfrmBreakpointCondition.rbEasyChange(Sender: TObject);
begin
  if rbEasy.checked then
    pagecontrol1.ActivePageIndex:=0
  else
    pagecontrol1.ActivePageIndex:=1;

end;



initialization
  {$I frmBreakpointConditionUnit.lrs}

end.

