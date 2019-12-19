unit Unit9;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, LResources;

type
  TForm9 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    procedure relocate;
  public
    { Public declarations }
  end;

type TLevel4 = record
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  f: integer;
  health: integer;
end;
type PLevel4=^TLevel4;

type TLevel3 = record
  p: PLevel4;
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  f: integer;
end;
type PLevel3=^TLevel3;

type TLevel2 = record
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  p: PLevel3;
  f: integer;
end;
type PLevel2=^TLevel2;

type TLevel1 = record
  a: integer;
  b: integer;
  c: integer;
  p: PLevel2;
  d: integer;
  e: integer;
  f: integer;
end;
type PLevel1=^TLevel1;





var
  Form9: TForm9;

  basepointer: plevel1;

implementation

uses Unit4, Unit10, frmHelpUnit;

resourcestring
  rsStep8MultilevelPointersPW = 'Step 8: Multilevel pointers: (PW=%s)';
  rsUnrandomizerDetected = 'Unrandomizer detected';
  rsYouVeGotSecondsLeftToChangeTheValueTo5000 = 'You''ve got %s seconds left to change the value to 5000';
  rsTryAgain9 = 'Aw, you''ve almost reached the end. But don''t worry, multilevel pointers can be a real pain when dealing with. If you get more experienced '
    +'someday you can try it again. Are you sure you want to quit?';
  rsLOSER = 'BOO';

  rsTutorialStep8=
          'This step will explain how to use multi-level pointers.'+#13#10+
          'In step 6 you had a simple level-1 pointer, with the first address found already being the real base address.'+#13#10+
          'This step however is a level-4 pointer. It has a pointer to a pointer to a pointer to a pointer to a pointer to the health.'+#13#10+
          ''+#13#10+
          'You basicly do the same as in step 6. Find out what accesses the value, look at the instruction and what probably is '+
          'the base pointer value, and what is the offset, and already fill that in or write it down. But in this case the address '+
          'you''ll find will also be a pointer. You just have to find out the pointer to that pointer exactly the same way as you did '+
          'with the value. Find out what accesses that address you found, look at the assembler instruction, note the probable '+
          'instruction and offset, and use that.'+#13#10+
          'and continue till you can''t get any further (usually when the base address is a static address, shown up as green)'+#13#10+
          ''+#13#10+
          'Click Change Value to let the tutorial access the health.'+#13#10+
          'If you think you''ve found the pointer path click Change Register. The pointers and value will then change and you''ll '+
          'have 3 seconds to freeze the address to 5000'+#13#10+
          ''+#13#10+
          'Extra: This problem can also be solved using a auto assembler script, or using the pointer scanner'+#13#10+
          'Extra2: In some situations it is recommended to change ce''s codefinder settings to Access violations when '+#13#10+
          'Encountering instructions like mov eax,[eax] since debugregisters show it AFTER it was changed, making it hard to '+
          'find out the the value of the pointer'+#13#10+
          ''+#13#10+
          ''+#13#10+
          ''+#13#10+
          ''+#13#10+
          ''+#13#10+
          'Extra3: If you''re still reading. You might notice that when looking at the assembler instructions that the pointer is '+
          'being read and filled out in the same codeblock (same routine, if you know assembler, look up till the start of the '+
          'routine). This doesn''t always happen, but can be really useful in finding a '+
          'pointer when debugging is troublesome';



procedure TForm9.relocate;
begin
  //just adding some more chaos:
  if (basepointer<>nil) then
  begin
    zeromemory(basepointer.p.p.p,sizeof(TLevel4));
    zeromemory(basepointer.p.p,sizeof(TLevel3));
    freemem(basepointer.p.p);
    zeromemory(basepointer.p,sizeof(TLevel2));
    freemem(basepointer.p);
    zeromemory(basepointer,sizeof(TLevel1));
    //delete a chunk inbetween if emptying wasn't enough already

  end;



  getmem(basepointer,sizeof(TLevel1)+random(128));
  basepointer.a:=random(99999);
  basepointer.b:=random(99999);
  basepointer.c:=random(99999);
  basepointer.d:=random(99999);
  basepointer.e:=random(99999);
  basepointer.f:=random(99999);

  getmem(basepointer.p,sizeof(TLevel2)+random(128));
  basepointer.p.a:=random(99999);
  basepointer.p.b:=random(99999);
  basepointer.p.c:=random(99999);
  basepointer.p.d:=random(99999);
  basepointer.p.e:=random(99999);
  basepointer.p.f:=random(99999);

  getmem(basepointer.p.p,sizeof(TLevel3)+random(128));
  basepointer.p.p.a:=random(99999);
  basepointer.p.p.b:=random(99999);
  basepointer.p.p.c:=random(99999);
  basepointer.p.p.d:=random(99999);
  basepointer.p.p.e:=random(99999);
  basepointer.p.p.f:=random(99999);

  getmem(basepointer.p.p.p,sizeof(TLevel4)+random(128));
  basepointer.p.p.p.a:=random(99999);
  basepointer.p.p.p.b:=random(99999);
  basepointer.p.p.p.c:=random(99999);
  basepointer.p.p.p.d:=random(99999);
  basepointer.p.p.p.e:=random(99999);
  basepointer.p.p.p.f:=random(99999);

end;

procedure TForm9.FormCreate(Sender: TObject);
begin
  relocate;
  basepointer.p.p.p.health:=random(4000);
  label1.caption:=inttostr(basepointer.p.p.p.health);

  memo1.lines.text:=rsTutorialStep8;
  memo1.Lines.Insert(0, Format(rsStep8MultilevelPointersPW, [inttostr(525)+inttostr(927)]));
  memo1.SelStart:=0;
  font.size:=12;
  frmHelp.attach(self,'8');
end;

procedure TForm9.Button1Click(Sender: TObject);
var h: ^integer;
    i: integer;
    a: plevel1;
    b: plevel2;
    c: plevel3;
    d: plevel4;

begin
  a:=basepointer;
  if (a.a=a.b) and (a.b=a.c) and (a.c=a.d) and (a.d=a.e) and (a.e=a.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if a.p=nil then exit;
  b:=a.p;
  if (b.a=b.b) and (b.b=b.c) and (b.c=b.d) and (b.d=b.e) and (b.e=b.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if b.p=nil then exit;
  c:=b.p;
  if (c.a=c.b) and (c.b=c.c) and (c.c=c.d) and (c.d=c.e) and (c.e=c.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if c.p=nil then exit;
  d:=c.p;
  if (d.a=d.b) and (d.b=d.c) and (d.c=d.d) and (d.d=d.e) and (d.e=d.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  d.health:=random(4000);
  label1.caption:=inttostr(d.health);
end;

procedure TForm9.Button3Click(Sender: TObject);
var j: integer;
    k: integer;
    i: integer;
    a: plevel1;
    b: plevel2;
    c: plevel3;
    d: plevel4;

begin
  relocate;

  a:=basepointer;
  if (a.a=a.b) and (a.b=a.c) and (a.c=a.d) and (a.d=a.e) and (a.e=a.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if a.p=nil then exit;
  b:=a.p;
  if (b.a=b.b) and (b.b=b.c) and (b.c=b.d) and (b.d=b.e) and (b.e=b.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if b.p=nil then exit;
  c:=b.p;
  if (c.a=c.b) and (c.b=c.c) and (c.c=c.d) and (c.d=c.e) and (c.e=c.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  if c.p=nil then exit;
  d:=c.p;
  if (d.a=d.b) and (d.b=d.c) and (d.c=d.d) and (d.d=d.e) and (d.e=d.f) then
  begin
    showmessage(rsUnrandomizerDetected);
    exit;
  end;

  d.health:=random(4000);
  label1.caption:=inttostr(d.health);
  label1.Repaint;

  k:=3;
  label2.Caption:=IntToStR(k);
  label2.Visible:=true;

  button1.Anchors:=[akLeft];
  label2.anchors:=[akLeft];

  button3.visible:=false;

  while k>0 do
  begin
    label2.Caption:=Format(rsYouVeGotSecondsLeftToChangeTheValueTo5000, [IntToStR(k)]);
    label2.Repaint;
    sleep(1000);
    deC(k);
  end;

  label2.visible:=false;

  button3.visible:=true;
  button1.Anchors:=[akLeft, akBottom];
  label2.anchors:=[akLeft, akBottom];


  //check if it is 5000
  if d.health=5000 then button2.Enabled:=true;

  {$O+}
  j:=0;
  {$O-}
  //if j=1 then beep;
end;

procedure TForm9.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm9.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg(rsTryAgain9, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;



procedure TForm9.Button2Click(Sender: TObject);
begin

  hide;

  form10:=tform10.create(self);
  form10.left:=left;
  form10.top:=top;
  form10.width:=width;
  form10.height:=height;
  form10.show;

 { hide;
  form4:=tform4.create(self);
  form4.show; }
end;

procedure TForm9.SpeedButton1Click(Sender: TObject);
begin
  showmessage(rsLOSER);
  button2.Click;
end;

initialization
  {$i Unit9.lrs}
  {$i Unit9.lrs}

end.
