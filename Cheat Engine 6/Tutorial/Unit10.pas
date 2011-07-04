unit Unit10;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Unit8, StdCtrls, Buttons, LResources;

type

  { TForm10 }

  TForm10 = class(TForm8)
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    health: integer;
  public
    { Public declarations }
  end;

var
  Form10: TForm10;

implementation

uses Unit4;


procedure TForm10.Button2Click(Sender: TObject);
begin
  hide;
  form4:=tform4.create(self);
  form4.show;
end;

procedure TForm10.Button1Click(Sender: TObject);
var realhealth: integer;
    h,m,s,ms: word;
begin
  realhealth:=health;
  realhealth:=realhealth xor $ce103478;
  realhealth:=realhealth-1-random(3);
  label1.caption:=inttostr(realhealth);

  if realhealth<0 then
  begin
    showmessage('You died! Loser! But don''t worry, i''ll bring you back to life.');
    realhealth:=100;
    label1.Caption:=inttostr(realhealth);
  end;


  realhealth:=realhealth xor $ce103478;
  health:=realhealth;



end;

procedure TForm10.FormCreate(Sender: TObject);
begin
  memo1.Lines.Insert(0,'Step 9: Custom scan: (PW='+inttostr(313)+inttostr(37157)+')');
  memo1.SelStart:=0;
  health:=100 xor $ce103478;
end;

procedure TForm10.SpeedButton1Click(Sender: TObject);
begin
  showmessage('This was the last tutorial and you skipped it. You lose');
  Application.Terminate;
end;

initialization
  {$i Unit10.lrs}

end.
