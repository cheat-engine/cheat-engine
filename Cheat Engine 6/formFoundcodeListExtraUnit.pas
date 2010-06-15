unit formFoundcodeListExtraUnit;

{$MODE Delphi}

interface

uses
  windows, LResources, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,Clipbrd, ExtCtrls, Buttons, frmFloatingPointPanelUnit, NewKernelHandler,cefuncproc;

type

  { TFormFoundCodeListExtra }

  TFormFoundCodeListExtra = class(TForm)
    pmCopy: TPopupMenu;
    Copyaddresstoclipboard1: TMenuItem;
    Panel1: TPanel;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Label6: TLabel;
    Label17: TLabel;
    Panel5: TPanel;
    Button1: TButton;
    Panel6: TPanel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label18: TLabel;
    pmCopy2: TPopupMenu;
    Copyguesstoclipboard1: TMenuItem;
    pmEmpty: TPopupMenu;
    sbShowFloats: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Copyaddresstoclipboard1Click(Sender: TObject);
    procedure Copyguesstoclipboard1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RegisterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Panel6Resize(Sender: TObject);
    procedure sbShowFloatsClick(Sender: TObject);
  private
    { Private declarations }
    fprobably: ptrUint;
    fpp: TfrmFloatingPointPanel;
    procedure setprobably(address:ptrUint);
  public
    { Public declarations }
    context: Context;
    property probably: ptrUint read fprobably write setprobably;
  end;

var
  FormFoundCodeListExtra: TFormFoundCodeListExtra;

implementation


procedure TFormFoundCodeListExtra.setprobably(address: ptrUint);
begin
  fprobably:=address;
  Label17.Caption:='The value of the pointer needed to find this address is probably '+IntToHex(address,8);
end;


procedure TFormFoundCodeListExtra.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if fpp<>nil then
    freeandnil(fpp);
    
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
  s:=label7.Caption+#13#10;
  s:=s+label8.Caption+#13#10;
  s:=s+label9.Caption+#13#10;
  s:=s+label11.Caption+#13#10;
  s:=s+label16.Caption+#13#10;
  s:=s+label14.Caption+#13#10;
  s:=s+label12.Caption+#13#10;
  s:=s+label13.Caption+#13#10;
  s:=s+label15.Caption+#13#10;
  s:=s+#13#10;
  s:=s+'Probable base pointer ='+inttohex(probably,8)+#13#10#13#10;

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
  saveformposition(self,[]);
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
begin
  label12.Left:=(panel6.ClientWidth-sbShowFloats.width)-label12.width-label7.Left;
  label13.left:=label12.left;
  label15.Left:=label12.Left;

  label11.Left:=((panel6.ClientWidth-sbShowFloats.width) div 2)-(label11.Width div 2);
  label16.left:=label11.left;
  label14.Left:=label11.left;

  sbShowFloats.top:=label13.Top+(label13.height div 2)-(sbShowFloats.height div 2);
  sbShowFloats.Left:=panel6.ClientWidth-sbShowFloats.Width;

  label18.top:=panel6.clientheight-label18.height;
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

