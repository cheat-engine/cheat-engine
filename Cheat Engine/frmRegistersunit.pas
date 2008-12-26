unit frmRegistersunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, ExtCtrls, StdCtrls, frmFloatingPointPanelUnit;

type
  TRegisters = class(TForm)
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
    sbShowFloats: TSpeedButton;
    procedure sbShowFloatsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    context: PContext;
    fpp: TfrmFloatingPointPanel;
  public
    { Public declarations }
    procedure SetContextPointer(context: PContext);
  end;

implementation

{$R *.dfm}




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

end.
