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

uses Unit4, Unit7;


procedure TForm6.Button2Click(Sender: TObject);
begin
  hide;
  form7:=tform7.create(self);
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

procedure TForm6.FormCreate(Sender: TObject);
var k: integer;
begin
  k:=random(10);
  while k>0 do
  begin
    getmem(i,4);
    dec(k);
  end;
  i^:=100;

  memo1.Lines.Insert(0,'Step 5: Code finder (PW='+inttostr(888899)+')');
  memo1.SelStart:=0;
end;

procedure TForm6.FormClose(Sender: TObject; var Action: TCloseAction);
begin
application.Terminate;
end;

procedure TForm6.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  canclose:=MessageDlg('This may look difficult. but it''s basicly. Find health, rigthclick health, find what writes, change health, click replace, change health, done. '+' But don''t feel down if you don''t get it. at least you know the basicas of memory scanning...  Are you sure you want to quit?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

procedure TForm6.SpeedButton1Click(Sender: TObject);
begin
  showmessage('LOSER');
  button2.Click;
end;

initialization
  {$i Unit6.lrs}
  {$i Unit6.lrs}

end.
