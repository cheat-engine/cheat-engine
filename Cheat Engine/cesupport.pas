unit cesupport;

{$mode delphi}

interface

uses
  {$ifdef windows}
  windows, activex, comobj,
  {$endif}
  lclintf, Classes, SysUtils,forms, controls, LMessages,
  ExtCtrls, Graphics, FileUtil, Dialogs, math;

type TADWindow=class(TCustomForm)
  private
    browserisvalid,browserisvalid2: boolean;
    browser: Olevariant;


    attachedForm: TCustomForm;
    attachedwindowproc: TWndMethod;
    attachside: TAnchorKind;

    secondsSinceLastShowAd: integer;
    showAdTimer: TTimer;
    counter: integer;
    userurl: string;
    userpercentage: integer;

    procedure checkAdTimer(sender: TObject);
    function getoptionalstring: string;
    function getBase: string;
    procedure hook(var TheMessage: TLMessage);
  public
    optional: string;
    procedure handleMove;
    procedure AttachToForm(form: TCustomForm);
    procedure setCanClose(state: boolean);
    procedure setPosition(side: TAnchorKind);
    procedure setUserUrl(url: string);
    procedure setUserPercentage(percentage: integer);
    procedure LoadAd;
    procedure LoadAdNow;
    constructor Create2(AOwner: TComponent;canclose: boolean);
    destructor destroy; override;
end;

var adwindow: TADWindow;

implementation



procedure TADWindow.setUserUrl(url: string);
begin
  userurl:=url;
end;

procedure TADWindow.setUserPercentage(percentage: integer);
begin
  userpercentage:=min(85, percentage);
end;

procedure TADWindow.checkAdTimer(sender: TObject);
begin
  inc(secondsSinceLastShowAd);

  LoadAd;


end;

procedure TADWindow.handleMove;
var m: TLMMove;
  wr: trect;
  ar: trect;
begin
  LCLIntf.GetWindowRect(attachedform.handle, wr);
  LCLIntf.GetWindowRect(handle, ar);

  case attachside of
    akBottom:
    begin
      top:=wr.Bottom+2;
      left:=wr.left+((wr.right-wr.left) div 2) - ((ar.right-ar.left) div 2);
    end;

    akTop:
    begin
      top:=wr.top-(ar.bottom-ar.Top)-2;
      left:=wr.left+((wr.right-wr.left) div 2) - ((ar.right-ar.left) div 2);
    end;

    akLeft:
    begin
      left:=attachedform.left-(ar.Right-ar.left)-2;
      top:=wr.Top+((wr.Bottom-wr.top) div 2) - ((ar.bottom-ar.top) div 2);
    end;

    akRight:
    begin
      left:=wr.right+2;
      top:=wr.Top+((wr.Bottom-wr.top) div 2) - ((ar.bottom-ar.top) div 2);
    end;

  end;
end;

procedure TADWindow.hook(var TheMessage: TLMessage);
begin
  case TheMessage.msg of
    LM_MOVE: handleMove;
    LM_SIZE: handleMove;
    LM_CLOSEQUERY: hide;
    LM_DESTROY:
    begin
      attachedform.WindowProc:=attachedwindowproc;
      attachedform:=nil;
    end;
  end;

  attachedwindowproc(TheMessage);
end;

procedure TADWindow.setPosition(side: TAnchorKind);
begin
  attachside:=side;
  handleMove;
end;

procedure TADWindow.AttachToForm(form: TCustomForm);
var updatemessage: TLMMove;
begin
  //first undo in case a new form is chosen
  if assigned(attachedwindowproc) then
    attachedform.WindowProc:=attachedwindowproc;

  attachedform:=form;

  if form<>nil then
  begin
    attachedwindowproc:=form.WindowProc;
    form.WindowProc:=hook;
  end
  else
    attachedwindowproc:=nil;
end;

function TADWindow.getoptionalstring: string;
begin
  if optional<>'' then
    result:='&'+optional
  else
    result:='';
end;

function TADWindow.getBase: string;
begin
  result:='http://www.cheatengine.org/ceads.php';
  if userurl<>'' then //let's see if it's time to show the url of the user
  begin
    if (Random(100)+1)<=userpercentage then  //(1-100) <= userpercentage
      result:=userurl; //do the users url instead

  end;



end;

procedure TADWindow.LoadAdNow;
begin
  secondsSinceLastShowAd:=121;
  LoadAd;
end;

procedure TADWindow.LoadAd;
var url: widestring;
  pid: dword;
begin
  {$ifdef windows}
  if (counter=0) or (secondsSinceLastShowAd>120) then
  begin
    GetWindowThreadProcessId(GetForegroundWindow,pid);
    if (counter=0) or (GetCurrentProcessId=pid) then //only show the ad when the foreground window is ce or if it's the first ad
    begin
      if visible and browserisvalid then
      begin
       // BringToFront;
        inc(counter);



        url:=getbase+'?cewidth='+inttostr(clientwidth)+'&ceheight='+inttostr(clientheight)+'&fn='+extractfilename(ExtractFileNameWithoutExt(application.ExeName))+'&counter='+inttostr(counter)+getoptionalstring;
        browser.Navigate(url);

      end;

      secondsSinceLastShowAd:=0;

      //showmessage(inttostr(browser.width));
    end;
  end;
  {$endif}

end;

procedure TADWindow.setCanClose(state: boolean);
begin
  if state then
  begin
    BorderStyle:=bsToolWindow;
    bordericons:=[biSystemMenu];
  end
  else
  begin
    BorderStyle:=bsNone;
  end;

  browserisvalid:=false;
end;

constructor TADWindow.create2(AOwner: TComponent; canclose: boolean);
begin
  inherited createnew(AOwner);

  setCanClose(canclose);

  color:=clGreen;

  {$ifdef windows}
  try
    browser := CreateOleObject('InternetExplorer.Application');

    windows.setparent(browser.hwnd, handle); // you can use panel1.handle, etc..
    browser.toolbar:=false;
    browser.fullscreen:=true;
    browser.Resizable:=false;
    browser.visible:=true;
    browserisvalid:=true; //we got to this point without a horrible crash, so I guess it's ok
    browserisvalid2:=true;

  except

  end;
  {$endif}

  //loadAd;

  showAdTimer:=TTimer.create(self);
  showAdTimer.interval:=1000; //every second
  showAdTimer.OnTimer:=checkAdTimer;
  showAdTimer.enabled:=true;

end;

destructor TADWindow.destroy;
begin
  {$ifdef windows}
  browser.Quit();
  browser:=NULL; //Unassigned;


  if attachedform<>nil then
    attachedform.WindowProc:=attachedwindowproc;

  CoFreeUnusedLibraries;
  {$endif}

  inherited destroy;
end;

end.

