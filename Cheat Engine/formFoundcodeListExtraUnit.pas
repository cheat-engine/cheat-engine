unit formFoundcodeListExtraUnit;

{$MODE Delphi}

interface

uses
  windows, LResources, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Menus,Clipbrd, ExtCtrls, Buttons,
  frmFloatingPointPanelUnit, NewKernelHandler,cefuncproc, frmStackViewUnit;

type

  { TFormFoundCodeListExtra }

  TFormFoundCodeListExtra = class(TForm)
    eiImageList: TImageList;
    lblGSBase: TLabel;
    lblCR3: TLabel;
    Label18: TLabel;
    lblPhysicalAddress: TLabel;
    lblVirtualAddress: TLabel;
    lblFSBase: TLabel;
    lblRAX: TLabel;
    lblRBP: TLabel;
    lblRBX: TLabel;
    lblRCX: TLabel;
    lblRDI: TLabel;
    lblRDX: TLabel;
    lblRIP: TLabel;
    lblRSI: TLabel;
    lblRSP: TLabel;
    pnlEPTWatch: TPanel;
    pnlRegisters: TPanel;
    Panel8: TPanel;
    pmCopy: TPopupMenu;
    Copyaddresstoclipboard1: TMenuItem;
    Panel1: TPanel;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label6: TLabel;
    Label17: TLabel;
    Panel5: TPanel;
    Button1: TButton;
    Panel6: TPanel;
    pmCopy2: TPopupMenu;
    Copyguesstoclipboard1: TMenuItem;
    pmEmpty: TPopupMenu;
    sbShowFloats: TSpeedButton;
    sbShowStack: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Copyaddresstoclipboard1Click(Sender: TObject);
    procedure Copyguesstoclipboard1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label1DblClick(Sender: TObject);
    procedure RegisterDblClick(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel6Resize(Sender: TObject);
    procedure sbShowStackClick(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
  private
    { Private declarations }
    fIs64bit: boolean;
    fprobably: ptrUint;
    fpp:       TfrmFloatingPointPanel;
    stackview: TfrmStackView;
    procedure setprobably(address:ptrUint);
  public
    { Public declarations }
    context: Context;
    stack: record
      savedsize: dword;
      stack: pbyte;
    end;

    lblR8: tlabel;
    lblR9: tlabel;
    lblR10: tlabel;
    lblR11: tlabel;
    lblR12: tlabel;
    lblR13: tlabel;
    lblR14: tlabel;
    lblR15: tlabel;
    lblR16: tlabel; //arm
    lblR17: tlabel;
    property probably: ptrUint read fprobably write setprobably;
  end;



implementation

uses MemoryBrowserFormUnit;

resourcestring
  rsTheValueOfThePointerNeededToFindThisAddressIsProba = 'The value of the '
    +'pointer needed to find this address is probably %s';
  rsProbableBasePointer = 'Probable base pointer =%s';

procedure TFormFoundCodeListExtra.setprobably(address: ptrUint);
begin
  fprobably:=address;
  Label17.Caption:=Format(rsTheValueOfThePointerNeededToFindThisAddressIsProba, [IntToHex(address, 8)]);
end;


procedure TFormFoundCodeListExtra.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Stackview<>nil then
    freeandnil(Stackview);

  if fpp<>nil then
    freeandnil(fpp);

  if stack.stack<>nil then
    freememandnil(stack.stack);
    
  action:=cafree;
end;

procedure TFormFoundCodeListExtra.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormFoundCodeListExtra.Copyaddresstoclipboard1Click(
  Sender: TObject);
var clip: tclipboard;
s: string;
begin
  s:=lblRAX.Caption+#13#10;
  s:=s+lblRBX.Caption+#13#10;
  s:=s+lblRCX.Caption+#13#10;
  s:=s+lblRDX.Caption+#13#10;
  s:=s+lblRSI.Caption+#13#10;
  s:=s+lblRDI.Caption+#13#10;
  s:=s+lblRBP.Caption+#13#10;
  s:=s+lblRSP.Caption+#13#10;
  s:=s+lblRIP.Caption+#13#10;
  s:=s+#13#10;
  s:=s+Format(rsProbableBasePointer, [inttohex(probably, 8)])+#13#10#13#10;

  s:=s+label1.Caption+#13#10;
  s:=s+label2.Caption+#13#10;
  s:=s+label3.Caption+#13#10;
  s:=s+label4.Caption+#13#10;
  s:=s+label5.Caption+#13#10;
  clipboard.SetTextBuf(pchar(s));
end;

procedure TFormFoundCodeListExtra.Copyguesstoclipboard1Click(
  Sender: TObject);
var
s: string;
begin
  s:=inttohex(probably,8);
  clipboard.SetTextBuf(pchar(s));
end;

procedure TFormFoundCodeListExtra.FormCreate(Sender: TObject);
var x: array of integer;
begin
  setlength(x,0);
  loadformposition(self,x);
end;

procedure TFormFoundCodeListExtra.FormDestroy(Sender: TObject);
begin
  if stackview<>nil then
    stackview.free;

  if fpp<>nil then
    fpp.Free;

  saveformposition(self);
end;

procedure TFormFoundCodeListExtra.FormShow(Sender: TObject);
begin
  panel1.Font.Height:=GetFontData(font.reference.Handle).Height-5;
  pnlRegisters.Font.Height:=panel1.Font.Height;

  label3.parentfont:=false;
  label10.parentfont:=false;
  label3.font.assign(pnlRegisters.font);
  label10.font.assign(pnlRegisters.font);

  label3.font.color:=clRed;
  label10.font.color:=clRed;

  Constraints.MaxHeight:=panel5.Top+panel5.height+10;
  Constraints.MinHeight:=Constraints.MaxHeight;

  autosize:=false;
end;

procedure TFormFoundCodeListExtra.Label1DblClick(Sender: TObject);
begin
  if (sender is tlabel) then
  begin
    memorybrowser.disassemblerview.SelectedAddress:=tlabel(sender).tag;
    memorybrowser.show;
  end;
end;

procedure TFormFoundCodeListExtra.RegisterDblClick(Sender: TObject);
var s: string;
i: integer;
begin
  if (sender is TLabel) then
  begin
    s:=tlabel(sender).Caption;
    i:=pos('=',s);
    if i>0 then //should always be true
    begin
      s:=copy(s,i+1,length(s));

      memorybrowser.hexview.address:=StrToInt64('$'+s);
      memorybrowser.show;
    end;
  end;

end;


procedure TFormFoundCodeListExtra.RegisterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var s: string;
i: integer;
begin
  if button = mbright then
  begin
    if (sender is TLabel) then
    begin
      s:=tlabel(sender).Caption;
      i:=pos('=',s);
      if i>0 then //should always be true
      begin
        s:=copy(s,i+1,length(s));

        clipboard.AsText:=s;
      end;
    end;
  end;
end;

procedure TFormFoundCodeListExtra.Panel6Resize(Sender: TObject);
var maxrightwidth: integer;
begin
  {maxrightwidth:=lblRBP.width;
  maxrightwidth:=max(maxrightwidth, lblRSP.Width);
  maxrightwidth:=max(maxrightwidth, lblRIP.Width);
  if lblR10<>nil then
  begin
    maxrightwidth:=max(maxrightwidth, lblR10.Width);
    maxrightwidth:=max(maxrightwidth, lblR13.Width);
  end;

  lblRBP.Left:=(panel6.ClientWidth-sbShowFloats.width)-maxrightwidth-lblRAX.Left;
  lblRSP.left:=lblRBP.left;
  lblRIP.Left:=lblRBP.Left;

  lblRDX.Left:=((panel6.ClientWidth-sbShowFloats.width) div 2)-(lblRDX.Width div 2);
  lblRSI.left:=lblRDX.left;
  lblRDI.Left:=lblRDX.left;

  if lblR8<>nil then
  begin
    lblR9.left:=lblRDX.left;
    lblR12.Left:=lblRDX.Left;
    lblR15.Left:=lblRDX.Left;

    lblR10.left:=lblRBP.left;
    lblR13.left:=lblRBP.left;
  end;


  sbShowFloats.top:=lblRSP.Top+(lblRSP.height div 2)-(sbShowFloats.height);
  sbShowFloats.Left:=panel6.ClientWidth-sbShowFloats.Width;

  sbShowstack.top:=lblRSP.Top+(lblRSP.height div 2);
  sbShowstack.left:=sbShowFloats.left;

  label18.top:=panel6.clientheight-label18.height;

         }
end;

procedure TFormFoundCodeListExtra.sbShowStackClick(Sender: TObject);
begin
  if stack.stack=nil then exit;

  if Stackview=nil then
    stackview:=TfrmStackView.create(self);

  stackview.SetContextPointer(@context, stack.stack, stack.savedsize);
  stackview.show;
end;

procedure TFormFoundCodeListExtra.sbShowFloatsClick(Sender: TObject);
begin
  if fpp=nil then
    fpp:=TfrmFloatingPointPanel.create(self);

  fpp.Left:=self.left+self.Width;
  fpp.Top:=self.top;
  fpp.SetContextPointer(@context);
  fpp.show;//pop to foreground
end;

initialization
  {$i formFoundcodeListExtraUnit.lrs}

end.

