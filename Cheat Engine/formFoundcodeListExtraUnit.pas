unit formFoundcodeListExtraUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus,Clipbrd;

type
  TFormFoundCodeListExtra = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label10: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    pmCopy: TPopupMenu;
    Copyaddresstoclipboard1: TMenuItem;
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
begin
  clip:=TClipboard.Create;
  clip.AsText:=inttohex(probably,8);
  clip.free;
end;

end.
