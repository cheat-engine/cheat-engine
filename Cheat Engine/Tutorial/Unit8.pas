unit Unit8;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, LResources;

type
  TForm8 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Label1: TLabel;
    Button1: TButton;
    SpeedButton1: TSpeedButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses Unit4, Unit9, frmHelpUnit;

resourcestring
  rsHealth = 'Health';
  rsAwYouReDeathLetMeReviveYou = 'Aw, you''re dead! Let me revive you';
  rsStep7CodeInjectionPW = 'Step 7: Code Injection: (PW=%s)';
  rsTryAgain8 = 'Code injections too tough? No problem, memory scanning and basic pointers should be enough to get you experienced enough and you can always '
    +'try the tutorial later. Are you sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep7=
    'Code injection is a technique where you inject a piece of code into the target process, and then reroute the '+
    'execution of code to go through your own written code.'+#13#10+
    ''+#13#10+
    'In this tutorial you''ll have a health value and a button that will decrease your health by 1 each time you click it.'+#13#10+
    'Your task is to use code injection to make the button increase your health by 2 each time it is clicked'+#13#10+
    ''+#13#10+
    'Start with finding the address and then find what writes to it.'+#13#10+
    'then when you''ve found the code that decreases it browse to that address in the disassembler, and open the auto '+
    'assembler window (ctrl+a)'+#13#10+
    'There click on template and then code injection, and give it the address that decreases health (If it isn''t already filled '+
    'in correctly)'+#13#10+
    'That will generate a basic auto assembler injection framework you can use for your code.'+#13#10+
    ''+#13#10+
    'Notice the alloc, that will allocate a block of memory for your code cave, in the past, in the pre windows 2000 '+
    'systems, people had to find code caves in the memory(regions of memory unused by the game), but that''s luckily a '+
    'thing of the past since windows 2000, and will these days cause errors when trying to be used, due to SP2 of XP '+
    'and the NX bit of new CPU''s'+#13#10+
    ''+#13#10+
    'Also notice the line newmem: and originalcode: and the text "Place your code here"'+#13#10+
    'As you guessed it, write your code here that will increase the  health with 2.'+#13#10+
    'An usefull assembler instruction in this case is the "ADD instruction"'+#13#10+
    'here are a few examples:'+#13#10+
    '"ADD [00901234],9" to increase the address at 00901234 with 9'+#13#10+
    '"ADD [ESP+4],9" to increase the address pointed to by ESP+4 with 9'+#13#10+
    'In this case, you''ll have to use the same thing between the brackets as the original code has that decreases your '+
    'health'+#13#10+
    ''+#13#10+
    'Notice:'+#13#10+
    'It is recommended to delete the line that decreases your health from the original code section, else you''ll have to '+
    'increase your health with 3 (you increase with 3, the original code decreases with 1, so the end result is increase '+
    'with 2), which might become confusing. But it''s all up to you and your programming.'+#13#10+
    ''+#13#10+
    'Notice 2:'+#13#10+
    'In some games the original code can exist out of multiple instructions, and sometimes, not always, it might happen '+
    'that a code at another place jumps into your jump instruction end will then cause unknown behavior. If that '+
    'happens, you should usually look near that instruction and see the jumps and fix it, or perhaps even choose to use a '+
    'different address to do the code injection from. As long as you''re able to figure out the address to change from inside '+
    'your injected code.';


procedure TForm8.Button1Click(Sender: TObject);
var oldhealth: dword;
begin
  oldhealth:=health;
  dec(health);
  label1.Caption:=inttostr(health);

  if health=oldhealth+2 then button2.Enabled:=true;

  if health<0 then
  begin
    showmessage(rsAwYouReDeathLetMeReviveYou);
    health:=100;
    label1.Caption:=rsHealth+': '+inttostr(health);
  end;
end;

procedure TForm8.FormCreate(Sender: TObject);
begin
  health:=100;


  memo1.lines.text:=rsTutorialStep7;
  memo1.Lines.Insert(0, Format(rsStep7CodeInjectionPW, [inttostr(0)+inttostr(13370)]));
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'7');
end;

procedure TForm8.Button2Click(Sender: TObject);
begin
  hide;
  form9:=tform9.create(self);
  form9.left:=left;
  form9.top:=top;
  form9.width:=width;
  form9.height:=height;
  form9.show;
end;

procedure TForm8.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm8.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsTryAgain8, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TForm8.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button2.Click;
end;

initialization
  {$i Unit8.lrs}
  {$i Unit8.lrs}

end.
