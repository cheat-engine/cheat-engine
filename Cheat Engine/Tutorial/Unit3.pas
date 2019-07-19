unit Unit3;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, ComCtrls, Buttons, LResources;

type
  TForm3 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    Timer2: TTimer;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

uses Unit4, Unit5, frmHelpUnit;

resourcestring
  rsStep3UnknownInitialValuePW = 'Step 3: Unknown initial value (PW=';
  rsDead = 'Seems you''ve done it again! Let me get a replacement! (And restart your scan!)';
  rsTryAgain3 = 'Step 3 isn''t really that hard. Just do a new scan, unkown initial value and then decreased value till you find it. Almost everyone gets past'
    +' this one. Sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep3=
    'Ok, seeing that you''ve figured out how to find a value using exact value let''s move on to the next step.'+#13#10+
          ''+#13#10+
          'First things first though. Since you are doing a new scan, you have to click on New Scan first, to start a new scan. (You may think this is straighforward, but you''d be surprised how many people get stuck on that step) I won''t be explaining this step again, so keep this in mind'+#13#10+
          'Now that you''ve started a new scan, let''s continue'+#13#10+
          ''+#13#10+
          'In the previous test we knew the initial value so we could do a exact value, but now we have a status bar where '+
          'we don''t know the starting value.'+#13#10+
          'We only know that the value is between 0 and 500. And each time you click ''hit me'' you lose some health. The '+
          'amount you lose each time is shown above the status bar.'+#13#10+
          ''+#13#10+
          'Again there are several different ways to find the value. (like doing a decreased value by... scan), but I''ll only '+
          'explain the easiest. "Unknown initial value", and decreased value.'+#13#10+
          'Because you don''t know the value it is right now, a exact value wont do any good, so choose as scantype '+
          '''Unknown initial value'', again, the value type is 4-bytes. (most windows apps use 4-bytes)'+
          'click first scan and wait till it''s done.'+#13#10+
          ''+#13#10+
          'When it is done click ''hit me''. You''ll lose some of your health. (the amount you lost shows for a few seconds and '+
          'then disappears, but you don''t need that)'+#13#10+
          'Now go to Cheat Engine, and choose ''Decreased Value'' and click ''Next Scan'''+#13#10+
          'When that scan is done, click hit me again, and repeat the above till you only find a few. '+#13#10+
          ''+#13#10+
          'We know the value is between 0 and 500, so pick the one that is most likely the address we need, and add it to '+
          'the list.'+#13#10+
          'Now change the health to 5000, to proceed to the next step.';



procedure TForm3.FormCreate(Sender: TObject);
begin
  memo1.lines.text:=rsTutorialStep3;

  health:=random(500);
  progressbar1.Min:=-2000;
  progressbar1.Max:=-2000+health;
  progressbar1.Position:=-2000+health;

  memo1.Lines.Insert(0, rsStep3UnknownInitialValuePW+inttostr(419482)+')');
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'3');
end;

procedure TForm3.Button2Click(Sender: TObject);
var loss: integer;
begin
  loss:=1+random(10);
  dec(health,loss);
  progressbar1.Position:=-2000+health;

  if health<0 then
  begin
    showmessage(rsDead);
    health:=random(500);
    progressbar1.Min:=-2000;
    progressbar1.Max:=-2000+health;
    progressbar1.Position:=-2000+health;
  end else
  begin
    timer2.Enabled:=false;
    timer2.Interval:=1000;
    timer2.enabled:=true;
    label1.Visible:=true;
    label1.Caption:='-'+IntToStr(loss);
  end;
end;

procedure TForm3.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  application.Terminate;
end;

procedure TForm3.Timer1Timer(Sender: TObject);
begin
  if health=5000 then
  begin
    timer1.enabled:=false;
    button1.Enabled:=true;
  end;
end;

procedure TForm3.Timer2Timer(Sender: TObject);
begin
  timer2.enabled:=false;
  label1.caption:='';
end;

procedure TForm3.Button1Click(Sender: TObject);
begin
  hide;
  form5:=tform5.create(self);
  form5.Width:=width;
  form5.Height:=height;
  form5.show;
  form5.left:=left;
  form5.top:=top;
end;

procedure TForm3.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsTryAgain3, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm3.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button1.Click;
end;

initialization
  {$i Unit3.lrs}
  {$i Unit3.lrs}

end.
