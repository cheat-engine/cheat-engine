unit Unit6;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, LResources;

type
  TForm6 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    SpeedButton1: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    i: ^Integer;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses Unit4, Unit7, frmHelpUnit;

resourcestring
  rsWellDoneYouScrewedUpTheTutorial = 'Well done, you screwed up the tutorial!!!!';
  rsStep5CodeFinderPW = 'Step 5: Code finder (PW=%s)';
  rsTryAgain6 = 'This may look difficult. but it''s basicly. Find health, rigthclick health, find what writes, change health, click replace, change health, '
    +'done.  But don''t feel down if you don''t get it. at least you know the basicas of memory scanning...  Are you sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep5=
      'Sometimes the location something is stored at changes when you restart the game, or even while you''re playing.. In '+
      'that case you can use 2 things to still make a table that works.'+#13#10+
      'In this step I''ll try to describe how to use the Code Finder function.'+#13#10+
      ''+#13#10+
      'The value down here will be at a different location each time you start the tutorial, so a normal entry in the address '+
      'list wouldn''t work.'+#13#10+
      'First try to find the address. (you''ve got to this point so I assume you know how to)'+#13#10+
      'When you''ve found the address, right-click the address in Cheat Engine and choose "Find out what writes to this '+
      'address". A window will pop up with an empty list.'+#13#10+
      'Then click on the Change value button in this tutorial, and go back to Cheat Engine. If everything went right there '+
      'should be an address with assembler code there now.'+#13#10+
      'Click it and choose the replace option to replace it with code that does nothing. That will also add the code address '+
      'to the code list in the advanced options window. (Which gets saved if you save your table)'+#13#10+
      ''+#13#10+
      'Click on stop, so the game will start running normal again, and close to close the window.'+#13#10+
      'Now, click on Change value, and if everything went right the Next button should become enabled.'+#13#10+
      ''+#13#10+
      'Note: When you''re freezing the address with a high enough speed it may happen that next becomes visible anyhow';


procedure TForm6.Button2Click(Sender: TObject);
begin
  hide;
  form7:=tform7.create(self);
  form7.left:=left;
  form7.top:=top;
  form7.width:=width;
  form7.height:=height;
  form7.show;
end;

procedure TForm6.Button1Click(Sender: TObject);
var j,k,l: integer;
begin
  //set new value
  j:=i^;
  k:=random(1000);
  l:=0;
  while k=j do
  begin
    k:=random(1000);
    inc(l);
    if l=100 then
      raise exception.Create(rsWellDoneYouScrewedUpTheTutorial);
  end;

  i^:=k;
  if i^=j then button2.Enabled:=true;

  label1.Caption:=IntToStr(i^);

  {$O+}
  j:=0;
  k:=0;
  {$O-}
end;

procedure TForm6.FormCreate(Sender: TObject);
var k: integer;
begin
  k:=1+random(10);
  while k>0 do
  begin
    getmem(i,4);
    dec(k);
  end;
  i^:=100;

  memo1.lines.text:=rsTutorialStep5;
  memo1.Lines.Insert(0, Format(rsStep5CodeFinderPW, [inttostr(888899)]));
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'5');
end;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm6.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsTryAgain6, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm6.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button2.Click;
end;

initialization
  {$i Unit6.lrs}
  {$i Unit6.lrs}

end.
