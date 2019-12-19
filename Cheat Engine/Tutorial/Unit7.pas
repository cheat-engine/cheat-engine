unit Unit7;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, LResources;

type
  TForm7 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    SpeedButton1: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form7: TForm7;
  i: ^integer;

implementation

uses Unit4, Unit8, frmHelpUnit;

resourcestring
  rsWellDoneYouScrewedUpTheTutorial = 'Well done, you screwed up the tutorial!!!!';
  rsYouVeGotSecondsLeftToChangeTheValueTo5000 = 'You have %s second%s left to change the value to 5000';
  rsStep6PointersPW = 'Step 6: Pointers: (PW=%s)';
  rsTryAgain7 = 'So, pointers are too difficult eh? Don''t worry, try again later. For most beginners this is difficult to grasp. But I have to tell you it''s'
    +' a powerful feature if you learn to use it. Are you sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep6=
    'In the previous step I explained how to use the Code finder to handle changing locations. But that method alone '+
    'makes it difficult to find the address to set the values you want.'+#13#10+
    'That''s why there are pointers:'+#13#10+
    ''+#13#10+
    'At the bottom you''ll find 2 buttons. One will change the value, and the other changes the value AND the location of '+
    'the value.'+#13#10+
    'For this step you don''t really need to know assembler, but it helps a lot if you do.'+#13#10+
    ''+#13#10+
    'First find the address of the value. When you''ve found it use the function to find out what accesses this address.'+#13#10+
    'Change the value again, and a item will show in the list. Double click that item. (or select and click on more info) and '+
    'a new window will open with detailed information on what happened when the instruction ran.'+#13#10+
    'If the assembler instruction doesn''t have anything between a ''['' and '']'' then use another item in the list.'+#13#10+
    'If it does it will say what it think will be the value of the pointer you need.'+#13#10+
    'Go back to the main cheat engine window (you can keep this extra info window open if you want, but if you close it, '+
    'remember what is between the [ and ] ) and do a 4 byte scan in hexadecimal for the value the extra info told you.'+#13#10+
    'When done scanning it may return 1 or a few hundred addresses. Most of the time the address you need will be the '+
    'smallest one. Now click on manually add and select the pointer checkbox.'+#13#10+
    ''+#13#10+
    'The window will change and allow you to type in the address of a pointer and a offset.'+#13#10+
    'Fill in as address the address you just found.'+#13#10+
    'If the assembler instruction has a calculation (e.g: [esi+12]) at the end then type the value in that''s at the end. else '+
    'leave it 0. If it was a more complicated instruction look at the calculation.'+#13#10+
    ''+#13#10+
    'example of a more complicated instruction:'+#13#10+
    '[EAX*2+EDX+00000310] eax=4C and edx=00801234.'+#13#10+
    'In this case EDX would be the value the pointer has, and EAX*2+00000310 the offset, so the offset you''d fill in '+
    'would be 2*4C+00000310=3A8.  (this is all in hex, use calc.exe from windows in scientific mode to calculate)'+#13#10+
    ''+#13#10+
    'Back to the tutorial, click OK and the address will be added, If all went right the address will show P->xxxxxxx, with '+
    'xxxxxxx being the address of the value you found. If thats not right, you''ve done something wrong.'+#13#10+
    'Now, change the value using the pointer you added in 5000 and freeze it. Then click Change pointer, and if all went '+#13#10+
    'right the next button will become visible.'+#13#10+
    ''+#13#10+
    ''+#13#10+
    'extra:'+#13#10+
    'And you could also use the pointer scanner to find the pointer to this address';
  rsDontFuckingFreezeThePointer = 'I''m sorry, but freezing the pointer is not'
    +' really a functional solution';


procedure TForm7.Button2Click(Sender: TObject);
begin
  hide;
  form8:=tform8.create(self);
  form8.left:=left;
  form8.top:=top;
  form8.width:=width;
  form8.height:=height;
  form8.show;
end;

procedure TForm7.Button1Click(Sender: TObject);
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

procedure TForm7.Button3Click(Sender: TObject);
var j: integer;
    k: integer;

    s: string;
    oldp: pointer;
begin
  button1.Anchors:=[akLeft];
  label2.anchors:=[akLeft];

  button3.visible:=false;
  button3.repaint;

  oldp:=i;

  getmem(i,4); //don't free else it's too easy
  i^:=random(1000);
  label1.caption:=IntToStr(i^);
  label1.Repaint;


  k:=3;
  label2.Caption:=IntToStR(k);
  label2.Visible:=true;

  while k>0 do
  begin

    if k>1 then
      s:='s'
    else
      s:='';

    label2.Caption:=Format(rsYouVeGotSecondsLeftToChangeTheValueTo5000, [IntToStR(k), s]);
    label2.Repaint;
    sleep(1000);
    deC(k);
  end;

  label2.visible:=false;

  //check if it is 5000
  if i^=5000 then
  begin
    if i=oldp then
    begin
      oldp:=nil;
      MessageDlg(rsDontFuckingFreezeThePointer, mtError, [mbok], 0);
    end
    else
      button2.Enabled:=true;
  end;

  {$O+}
  j:=0;
  {$O-}
  //if j=1 then beep;

  button3.visible:=true;

  button1.Anchors:=[akLeft, akBottom];
  label2.anchors:=[akLeft, akBottom];

end;

procedure TForm7.FormCreate(Sender: TObject);
var k: integer;
begin
  k:=1+random(10);
  while k>0 do
  begin
    getmem(i,4); //got to love memory leaks. (but at least it's a good test)
    dec(k);
  end;
  i^:=100;

  memo1.lines.text:=rstutorialStep6;
  memo1.Lines.Insert(0, Format(rsStep6PointersPW, [inttostr(0)+inttostr(98712)]));
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'6');
end;


procedure TForm7.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm7.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsTryAgain7, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm7.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button2.Click;
end;

initialization
  {$i Unit7.lrs}
  {$i Unit7.lrs}

end.
