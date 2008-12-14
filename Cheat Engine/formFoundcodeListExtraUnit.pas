unit formFoundcodeListExtraUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,Clipbrd, ExtCtrls;

type
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Copyaddresstoclipboard1Click(Sender: TObject);
  private
    { Private declarations }
    fprobably: dword;
    procedure setprobably(address:dword);
  public
    { Public declarations }
    property probably: dword read fprobably write setprobably;
  end;

var
  FormFoundCodeListExtra: TFormFoundCodeListExtra;

implementation

{$R *.dfm}

procedure TFormFoundCodeListExtra.setprobably(address: dword);
begin
  fprobably:=address;
  Label17.Caption:='The value of the pointer needed to find this address is probably '+IntToHex(address,8);
end;


procedure TFormFoundCodeListExtra.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
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

end.

