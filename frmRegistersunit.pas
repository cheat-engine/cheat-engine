unit frmRegistersunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, frmFloatingPointPanelUnit, newkernelhandler,
  clipbrd;

type
  TRegisters = class(TForm)
    Panel1: TPanel;
    EAXLabel: TLabel;
    EBXlabel: TLabel;
    ECXlabel: TLabel;
    EDXlabel: TLabel;
    ESIlabel: TLabel;
    EDIlabel: TLabel;
    EBPlabel: TLabel;
    ESPlabel: TLabel;
    EIPlabel: TLabel;
    Label14: TLabel;
    Shape1: TShape;
    Panel2: TPanel;
    sbShowFloats: TSpeedButton;
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EAXLabelDblClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    context: PContext;
    fpp: TfrmFloatingPointPanel;
    function TagToValue(tag: integer): dword;
  public
    { Public declarations }
    procedure SetContextPointer(context: PContext);
  end;

implementation

{$R *.dfm}

uses memorybrowserformunit;


procedure TRegisters.SetContextPointer(context: PContext);
begin
  self.context:=context;
  EAXLabel.Caption:=format('EAX %0.8x',[context.Eax]);
  EBXLabel.Caption:=format('EBX %0.8x',[context.Ebx]);
  ECXLabel.Caption:=format('ECX %0.8x',[context.Ecx]);
  EDXLabel.Caption:=format('EDX %0.8x',[context.Edx]);
  ESILabel.Caption:=format('ESI %0.8x',[context.Esi]);
  EDILabel.Caption:=format('EDI %0.8x',[context.Edi]);
  EBPLabel.Caption:=format('EBP %0.8x',[context.Ebp]);
  ESPLabel.Caption:=format('ESP %0.8x',[context.Esp]);
  EIPLabel.Caption:=format('EIP %0.8x',[context.Eip]);
  
end;

procedure TRegisters.sbShowFloatsClick(Sender: TObject);
begin
  if fpp=nil then
    fpp:=TfrmFloatingPointPanel.create(self);

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(context);
  fpp.show;//pop to foreground
end;

procedure TRegisters.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fpp.free;
  action:=cafree;
end;

function TRegisters.TagToValue(tag: integer): dword;
begin
  case tag of
    0: result:=context.Eax;
    1: result:=context.Ebx;
    2: result:=context.Ecx;
    3: result:=context.Edx;
    4: result:=context.esi;
    5: result:=context.Edi;
    6: result:=context.Ebp;
    7: result:=context.Esp;
    8: result:=context.Eip;
    else result:=0;
  end;
end;

procedure TRegisters.EAXLabelDblClick(Sender: TObject);
begin
  memorybrowser.memoryaddress:=TagToValue(tlabel(sender).tag);
  memorybrowser.RefreshMB;
end;

procedure TRegisters.FormResize(Sender: TObject);
begin
  sbShowFloats.Top:=(clientheight div 2)-(sbShowFloats.height div 2);
end;

procedure TRegisters.RegisterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=tlabel(sender).Caption;
      i:=pos(' ',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));
        clipboard.AsText:=s;
      end;
    end;
  end;
end;  

end.
