unit Unit9;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm9 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button2Click(Sender: TObject);
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

type TLevel3 = record
  p: ^TLevel4;
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  f: integer;
end;

type TLevel2 = record
  a: integer;
  b: integer;
  c: integer;
  d: integer;
  e: integer;
  p: ^TLevel3;
  f: integer;
end;

type TLevel1 = record
  a: integer;
  b: integer;
  c: integer;
  p: ^TLevel2;
  d: integer;
  e: integer;
  f: integer;
end;

var
  Form9: TForm9;

  basepointer: ^tlevel1;

implementation

uses Unit4, Unit10;

{$R *.dfm}

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

  memo1.Lines.Insert(0,'Step 8: Multilevel pointers: (PW='+inttostr(525)+inttostr(927)+')');
  memo1.SelStart:=0;
end;

procedure TForm9.Button1Click(Sender: TObject);
var h: ^integer;
    i: integer;
begin
  basepointer.p.p.p.health:=random(4000);
  label1.caption:=inttostr(basepointer.p.p.p.health);
end;

procedure TForm9.Button3Click(Sender: TObject);
var j: integer;
    k: integer;
begin
  relocate;
  basepointer.p.p.p.health:=random(4000);
  label1.caption:=inttostr(basepointer.p.p.p.health);
  label1.Repaint;

  k:=3;
  label2.Caption:=IntToStR(k);
  label2.Visible:=true;

  while k>0 do
  begin
    label2.Caption:='You''ve got '+IntToStR(k)+' seconds left to change the value to 5000';
    label2.Repaint;
    sleep(1000);
    deC(k);
  end;

  label2.visible:=false;

  //check if it is 5000
  if basepointer.p.p.p.health=5000 then button2.Enabled:=true;

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
  canclose:=MessageDlg('Aw, you''ve almost reached the end. But don''t worry, multilevel pointers can be a real pain when dealing with. If you get more experienced someday you can try it again. Are you sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

procedure TForm9.Button2Click(Sender: TObject);
begin
  hide;
  form10:=tform10.create(self);
  form10.show;
end;

end.
