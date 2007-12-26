unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,tlhelp32, XPMan;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Timer1: TTimer;
    XPManifest1: TXPManifest;
    Edit1: TEdit;
    Label1: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9, Unit10;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  self.hide;
  form2:=Tform2.create(self);
  form2.show;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    FullProcessName,ProcessName: String;
    I: Integer;
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
  randomize;
end;

procedure TForm1.Button2Click(Sender: TObject);
var x: string;
begin

  if length(edit1.text)=6 then
  begin
    x:=uppercase(edit1.text);

    if (edit1.text[1]='0') and (edit1.text[2]='9') and (edit1.Text[3]='0') and (edit1.Text[4]='4') and (edit1.text[5]='5') and (edit1.Text[6]='3') then
    begin
      //2:090453
     hide;
     form2:=tform2.create(self);
     form2.show;
    end;

    if (edit1.text[1]='4') and (edit1.text[2]='1') and (edit1.Text[3]='9') and (edit1.Text[4]='4') and (edit1.text[5]='8') and (edit1.Text[6]='2') then
    begin
      //3:419482
     hide;
     form3:=tform3.create(self);
     form3.show;
    end;

    if (edit1.text[1]='8') and (edit1.text[2]='9') and (edit1.Text[3]='0') and (edit1.Text[4]='1') and (edit1.text[5]='2') and (edit1.Text[6]='4') then
    begin
      //4:890124
     hide;
     form5:=tform5.create(self);
     form5.show;
    end;

    if (edit1.text[1]='8') and (edit1.text[2]='8') and (edit1.Text[3]='8') and (edit1.Text[4]='8') and (edit1.text[5]='9') and (edit1.Text[6]='9') then
    begin
      //5:888899
      hide;
      form6:=tform6.create(self);
      form6.show;
    end;

    if (edit1.text[1]='0') and (edit1.text[2]='9') and (edit1.Text[3]='8') and (edit1.Text[4]='7') and (edit1.text[5]='1') and (edit1.Text[6]='2') then
    begin
      //6:098712
      hide;
      form7:=tform7.create(self);
      form7.show;
    end;

    if (edit1.text[1]='0') and (edit1.text[2]='1') and (edit1.Text[3]='3') and (edit1.Text[4]='3') and (edit1.text[5]='7') and (edit1.Text[6]='0') then
    begin
      //7:013370
      hide;
      form8:=tform8.create(self);
      form8.show;
    end;

    if (edit1.text[1]='5') and (edit1.text[2]='2') and (edit1.Text[3]='5') and (edit1.Text[4]='9') and (edit1.text[5]='2') and (edit1.Text[6]='7') then
    begin
      //8:525927
      hide;
      form9:=tform9.create(self);
      form9.show;
    end;

  end;

  if length(edit1.text)=8 then
    if (edit1.text[1]='3') and (edit1.text[2]='1') and (edit1.Text[3]='3') and (edit1.Text[4]='3') and (edit1.text[5]='7') and (edit1.Text[6]='1') and (edit1.text[7]='5') and (edit1.Text[8]='7') then
    begin
      //9:31337157
      hide;
      form10:=tform10.create(self);
      form10.show;
    end;

end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
  if key =chr(13) then button2.click;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('First step too hard eh? Better give up now!',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

end.
