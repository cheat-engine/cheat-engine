unit frmMicrotransactionsUnit;

//Don't piss your pants, this is just an april fools thing

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LuaInternet, lua, luahandler, lualib, lauxlib, betterControls;

type

  { TfrmMicroTransactions }

  TfrmMicroTransactions = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Image1: TImage;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Timer1: TTimer;
    tShaker: TTimer;
    procedure BitBtn1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tShakerTimer(Sender: TObject);
  private
    shakedistance: integer;
    {$IFDEF windows}
    internet: TWinInternet;
    {$ENDIF}
    PreviousOnChangeBounds: TNotifyEvent;
    procedure MainFormChangeBounds(Sender: TObject);
  public

  end;

var
  frmMicroTransactions: TfrmMicroTransactions;

implementation

uses MainUnit, cheatecoins{$IFDEF windows} , windows {$ENDIF};

{ TfrmMicroTransactions }

procedure TfrmMicroTransactions.MainFormChangeBounds(Sender: TObject);
var bw: integer;
begin
  {$IFDEF windows}
  if assigned(PreviousOnChangeBounds) then
    PreviousOnChangeBounds(sender);

  bw:=GetSystemMetrics(SM_CXEDGE);
  bw:=bw+GetSystemMetrics(SM_CXBORDER);

  left:=mainform.left+mainform.Width+bw*2+8;
  top:=mainform.top;
  {$ENDIF}
end;

procedure TfrmMicroTransactions.BitBtn1Click(Sender: TObject);
var ss: TStringstream;
begin
  {$IFDEF windows}
  if internet=nil then
    internet:=TWinInternet.Create('Cheat Engine microtransaction system');

  ss:=tstringstream.create({$if FPC_FULLVERSION<030200}''{$endif});
  try
    try
      internet.getURL('https://cheatengine.org/microtransaction.php?action=buy&amount='+inttostr(tbitbtn(sender).Tag), ss);
      if luaL_loadstring(LuaVM, pchar(ss.DataString))=0 then
        lua_pcall(LuaVM,0,0,0)
      else
      begin
        lua_settop(LuaVM,0);
        MessageDlg('There is an issue with the webserver. Please try again later',mtInformation, [mbok],0);
      end;
    except
      MessageDlg('Sorry, but the Cheat Engine server is currently overloaded or you have no internet. Please try again later',mtInformation, [mbok],0);
    end;
  finally
    ss.free;
  end;
  {$ENDIF}

end;

procedure TfrmMicroTransactions.Button1Click(Sender: TObject);
begin

end;

procedure TfrmMicroTransactions.FormCreate(Sender: TObject);
begin
  PreviousOnChangeBounds:=MainForm.OnChangeBounds;

  MainForm.OnChangeBounds:=MainFormChangeBounds;
  label2.font.size:=30;
  bitbtn1.font.size:=10;
  bitbtn2.font.size:=11;
  bitbtn3.font.size:=12;
end;

procedure TfrmMicroTransactions.FormShow(Sender: TObject);
begin
  MainFormChangeBounds(mainform);
  timer1timer(timer1);
end;

procedure TfrmMicroTransactions.Timer1Timer(Sender: TObject);
var
  c: integer;
  fs: integer;
  fc: tcolor;

  red: single;
  green: single;
begin
  {$IFDEF windows}
  c:=checkCoinStatus;

  if c<0 then
  begin
    timer1.enabled:=false;
    tshaker.enabled:=false;
    hide;
    exit;
  end;

  label2.caption:=inttostr(c);

  fs:=30;
  if c<=10 then fs:=35;
  if c<=5 then fs:=40+(5-c)*5;


  fc:=$008000;
  green:=c/20;
  red:=1-c/20;

  fc:=(trunc(red*255) shl 0)+(trunc(green*200) shl 8);
  label2.Font.color:=fc;
  if label2.font.size<>fs then
  begin
    label2.font.size:=fs;


    DoAutoSize;

    BitBtn1.Repaint;
    BitBtn2.Repaint;
    BitBtn3.Repaint;

  end;

  if image1.Width<>image1.height then
    image1.Width:=image1.height;

  tshaker.enabled:=c<=3;
  shakedistance:=3-c;
  {$ENDIF}
end;

procedure TfrmMicroTransactions.tShakerTimer(Sender: TObject);
begin
  label2.BorderSpacing.Left:=32-shakedistance+random(shakedistance*2);
end;

initialization
  {$I frmMicrotransactionsUnit.lrs}


end.

