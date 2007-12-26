unit Unit7;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm7 = class(TForm)
    Memo1: TMemo;
    Button2: TButton;
    Button1: TButton;
    Label1: TLabel;
    Button3: TButton;
    Label2: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form7: TForm7;
  i: ^integer;

implementation

uses Unit4, Unit8;

{$R *.dfm}

procedure TForm7.Button2Click(Sender: TObject);
begin
  hide;
  form8:=tform8.create(self);
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
      raise exception.Create('Well done, you screwed up the tutorial!!!!');
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
begin
  getmem(i,4); //don't free else it's too easy
  i^:=random(1000);
  label1.caption:=IntToStr(i^);
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
  if i^=5000 then button2.Enabled:=true;

  {$O+}
  j:=0;
  {$O-}
  //if j=1 then beep;

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

  memo1.Lines.Insert(0,'Step 6: Pointers: (PW='+inttostr(0)+inttostr(98712)+')');
  memo1.SelStart:=0;
end;


procedure TForm7.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm7.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('So, pointers are too difficult eh? Don''t worry, try again later. For most beginners this is difficult to grasp. But I have to tell you it''s a powerfull feature if you learn to use it. Are you sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

end.
