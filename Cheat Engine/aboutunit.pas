unit aboutunit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,shellapi;

type
  TAbout = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Image1: TImage;
    Label6: TLabel;
    Button1: TButton;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Button2: TButton;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    lblDBVM: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Label8Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  About: TAbout;

implementation

{$R *.dfm}
uses tlgunit,mainunit2,
{$ifdef net}
unit2;
{$else}
mainunit;
{$endif}

procedure TAbout.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure TAbout.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=caFree;
end;

procedure TAbout.Button2Click(Sender: TObject);
begin
  shellexecute(0,'open','https://www.paypal.com/xclick/business=dark_byte%40hotmail.com&no_note=1&tax=0','','',sw_maximize);
end;

procedure TAbout.FormShow(Sender: TObject);
var supportsdbvm: boolean;
    a,b,c,d: dword;
begin
  {$ifdef net}
    groupbox1.Caption:=unit2.CEnorm;
  {$else}
    groupbox1.Caption:=mainunit2.CEnorm;
  {$endif}

  supportsdbvm:=false;

  begin
    asm
      pushad
      mov eax,0
      cpuid
      mov a,eax
      mov b,ebx
      mov c,ecx
      mov d,edx
      popad
    end;

    if (b=$756e6547) and (d=$49656e69) and (c=$6c65746e) then
    begin
      asm
        pushad
        mov eax,1
        cpuid
        mov a,eax
        mov b,ebx
        mov c,ecx
        mov d,edx
        popad
      end;

      if ((c shr 5) and 1)=1 then
        supportsdbvm:=true;
    end;
  end;

  if supportsdbvm then
  begin
    lblDBVM.Font.Color:=clGreen;
    lbldbvm.caption:='Your system supports DBVM';
  end
  else
  begin
    lblDBVM.Font.Color:=clRed;
    lbldbvm.caption:='Your system DOES NOT support DBVM';
  end;
end;

procedure TAbout.Label8Click(Sender: TObject);
begin
  ShellExecute(0, pchar('open'),pchar('http://syndiv.com/ce/'), pchar(''),pchar(''), SW_MAXIMIZE	);
end;

procedure TAbout.Label9Click(Sender: TObject);
begin
  ShellExecute(0, pchar('open'),pchar('http://forum.cheatengine.org/'), pchar(''),pchar(''), SW_MAXIMIZE	);
end;

procedure TAbout.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (ssCtrl in Shift) and (ssAlt in Shift) and (ssShift in Shift) then
  begin
    ShowMessage('Did you really think you''d find an easter egg by doing this? Well, you know what? You where right!');
    with TTlg.create(self) do show;
  end;
end;

end.
