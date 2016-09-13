unit Unit1;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, LResources{,tlhelp32, XPMan};

type

  { TForm1 }

  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Timer1: TTimer;
    edtPassword: TEdit;
    Label1: TLabel;
    btnOK: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure edtPasswordKeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9, Unit10, cetranslator;

resourcestring
  rsFirstStepTooHardBetterGiveUpNow =
    'First step too hard? Better give up now!';

  rsTutorial1='Welcome to the Cheat Engine Tutorial. (v3.3)'+#13#10+
              ''+#13#10+
              'This tutorial will try to explain the basics of cheating on games, and getting you more familiar with Cheat Engine.'+#13#10+
              ''+#13#10+
              'First open Cheat Engine if it hasn''t been opened yet.'+#13#10+
              'Then click on the ''open process'' icon. (top left icon, with the computer on it)'+#13#10+
              ''+#13#10+
              'When the process window is open find this tutorial. The process name is probably ''tutorial.exe'' unless you'+
              ' renamed it.'+#13#10+
              'Select it, and click "Open". Just ignore all the other buttons right now, but experiment with them later if you feel like it.'+#13#10+
              ''+#13#10+
              'When everything went right, the process window should be gone now and at the top of CE the process name is'+
              ' shown.'+#13#10+
              ''+#13#10+
              'Now, click NEXT to continue to the next step. (Or fill in the password to proceed to that particular step you want)';


procedure TForm1.Button1Click(Sender: TObject);
begin
  self.hide;

  form2:=Tform2.create(self);
  form2.width:=width;
  form2.height:=height;
  form2.left:=left;
  form2.top:=top;
  form2.show;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
(*Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    FullProcessName,ProcessName: String;
    I: Integer;   *)
begin
//GOD!!! THOSE STUPID RETARDED SHITHEADS NEVER MAKE IT PAST THIS STEP!!!!!!
//Thats why it's out....

{  //search for cheat engine
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check=true do
    begin
      ProcessName:='';
      FullProcessName:='';
      FullProcessName:=processentry.szExeFile;
      i:=Length(FullProcessName);
      while (i>0) and (FullProcessname[i-1]<>'\') do dec(i);
      processname:=copy(FullProcessName,i,length(FullProcessname)-i+1);

      if (uppercase(processname)='CHEAT ENGINE SERVER.EXE') or (uppercase(processname)='CESERVER.EXE') or (uppercase(processname)='CHEATENGINE.EXE') or (uppercase(processname)='CHEAT ENGINE.EXE') then
      begin
        button1.Enabled:=true;
        timer1.enableD:=false;
        exit;
      end;

      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
  end;}



end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  forms.Application.ShowButtonGlyphs:=sbgNever;
  randomize;

  memo1.lines.text:=rsTutorial1;

  font.size:=12;

end;

procedure TForm1.btnOKClick(Sender: TObject);
var x: string;
begin

  if length(edtPassword.text)=6 then
  begin
    x:=uppercase(edtPassword.text);

    if (edtPassword.text[1]='0') and (edtPassword.text[2]='9') and (edtPassword.Text[3]='0') and (edtPassword.Text[4]='4') and (edtPassword.text[5]='5') and (edtPassword.Text[6]='3') then
    begin
      //2:090453
     hide;
     form2:=tform2.create(self);
     form2.width:=width;
     form2.height:=height;
     form2.left:=left;
     form2.top:=top;
     form2.show;
    end;

    if (edtPassword.text[1]='4') and (edtPassword.text[2]='1') and (edtPassword.Text[3]='9') and (edtPassword.Text[4]='4') and (edtPassword.text[5]='8') and (edtPassword.Text[6]='2') then
    begin
      //3:419482
     hide;
     form3:=tform3.create(self);
     form3.width:=width;
     form3.height:=height;
     form3.left:=left;
     form3.top:=top;
     form3.show;
    end;

    if (edtPassword.text[1]='8') and (edtPassword.text[2]='9') and (edtPassword.Text[3]='0') and (edtPassword.Text[4]='1') and (edtPassword.text[5]='2') and (edtPassword.Text[6]='4') then
    begin
      //4:890124
     hide;
     form5:=tform5.create(self);
     form5.width:=width;
     form5.height:=height;
     form5.left:=left;
     form5.top:=top;
     form5.show;
    end;

    if (edtPassword.text[1]='8') and (edtPassword.text[2]='8') and (edtPassword.Text[3]='8') and (edtPassword.Text[4]='8') and (edtPassword.text[5]='9') and (edtPassword.Text[6]='9') then
    begin
      //5:888899
      hide;
      form6:=tform6.create(self);
      form6.width:=width;
      form6.height:=height;
      form6.left:=left;
      form6.top:=top;
      form6.show;
    end;

    if (edtPassword.text[1]='0') and (edtPassword.text[2]='9') and (edtPassword.Text[3]='8') and (edtPassword.Text[4]='7') and (edtPassword.text[5]='1') and (edtPassword.Text[6]='2') then
    begin
      //6:098712
      hide;
      form7:=tform7.create(self);
      form7.width:=width;
      form7.height:=height;
      form7.left:=left;
      form7.top:=top;
      form7.show;
    end;

    if (edtPassword.text[1]='0') and (edtPassword.text[2]='1') and (edtPassword.Text[3]='3') and (edtPassword.Text[4]='3') and (edtPassword.text[5]='7') and (edtPassword.Text[6]='0') then
    begin
      //7:013370
      hide;
      form8:=tform8.create(self);
      form8.width:=width;
      form8.height:=height;
      form8.left:=left;
      form8.top:=top;
      form8.show;
    end;

    if (edtPassword.text[1]='5') and (edtPassword.text[2]='2') and (edtPassword.Text[3]='5') and (edtPassword.Text[4]='9') and (edtPassword.text[5]='2') and (edtPassword.Text[6]='7') then
    begin
      //8:525927
      hide;
      form9:=tform9.create(self);
      form9.width:=width;
      form9.height:=height;
      form9.left:=left;
      form9.top:=top;
      form9.show;
    end;

  end;

  if length(edtPassword.text)=8 then
    if (edtPassword.text[1]='3') and (edtPassword.text[2]='1') and (edtPassword.Text[3]='3') and (edtPassword.Text[4]='3') and (edtPassword.text[5]='7') and (edtPassword.Text[6]='1') and (edtPassword.text[7]='5') and (edtPassword.Text[8]='7') then
    begin
      //9:31337157
      hide;
      form10:=tform10.create(self);
      form10.width:=width;
      form10.height:=height;
      form10.left:=left;
      form10.top:=top;
      form10.show;
    end;

end;

procedure TForm1.edtPasswordKeyPress(Sender: TObject; var Key: Char);
begin
  if key =chr(13) then btnOK.click;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsFirstStepTooHardBetterGiveUpNow, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

initialization
  {$i Unit1.lrs}
  {$i Unit1.lrs}

end.
