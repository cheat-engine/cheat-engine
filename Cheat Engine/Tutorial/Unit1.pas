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
    procedure FormShow(Sender: TObject);
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

uses Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9, Unit10, cetranslator, frmHelpUnit;

resourcestring
  rsFirstStepTooHardBetterGiveUpNow =
    'First step too hard? Go to forum.cheatengine.org, then click on "Tutorials" for helpful guides!';

  rsTutorial1='Welcome to the Cheat Engine Tutorial (v3.4)'+#13#10+
              ''+#13#10+
              'This tutorial will teach you the basics of cheating in video games. It will also show you foundational aspects of using Cheat Engine (or CE for short). Follow the steps below to get started.'+#13#10+
              ''+#13#10+
              '1: Open Cheat Engine if it currently isn''t running.'+#13#10+
              '2: Click on the "Open Process" icon (it''s the top-left icon with the computer on it, below "File".).'+#13#10+
              '3: With the Process List window now open, look for this tutorial''s process in the list. It will look something like "00001F98-Tutorial-x86_64.exe" or "0000047C-Tutorial-i386.exe". (The first 8 numbers/letters will probably be different.)'+#13#10+
              '4: Once you''ve found the process, click on it to select it, then click the "Open" button. (Don''t worry about all the other buttons right now. You can learn about them later if you''re interested.)'+#13#10+
              ''+#13#10+
              'Congratulations! If you did everything correctly, the process window should be gone with Cheat Engine now attached to the tutorial (you will see the process name towards the top-center of CE).'+#13#10+
              ''+#13#10+
              'Click the "Next" button below to continue, or fill in the password and click the "OK" button to proceed to that step.)'+#13#10+
              ''+#13#10+
              'If you''re having problems, simply head over to forum.cheatengine.org, then click on "Tutorials" to view beginner-friendly guides!';


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

procedure TForm1.FormShow(Sender: TObject);
var
  tw: integer;
  th: integer;
begin
  tw:=canvas.TextWidth('XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX');
  th:=canvas.TextHeight('ajFgWX');

  if width<tw then
    width:=tw;

  if memo1.height<th*10 then
    height:=height+(th*10-memo1.height);


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

  //frmHelp:=TfrmHelp.Create(application);
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
