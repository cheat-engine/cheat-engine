unit hypermode;

interface

uses classes,windows,sysutils,NewKernelHandler,messages;

type TReplacedCode = record
  address: dword;
  originalcode: array of byte;
end;

type THypermode=class(tthread)
  private
    CEHOOKDLL: Thandle;
    cescanhook: THandle;
    failed: boolean;
    procedure FailedLoading;
    procedure Success;
  public
    speedhackenabled: boolean;
    HyperscanWindow: thandle;

    procedure DisableSpeedhack;
    procedure execute; override;
    constructor create;
    destructor destroy; override;
end;


implementation

uses mainunit,cefuncproc;

resourcestring
  strHyperscanfailed='Hypermode didn''t respond';
  strhookfailed='I can''t set the hook required to get into the process. Check if there isn''t any anti-cheat protection running.';


destructor THypermode.destroy;
begin
  if hyperscanwindow<>0 then postmessage(HyperscanWindow,wm_destroy,0,0);

  hyperscanwindow:=0;
  mainform.cbfasterscan.Checked:=false;
  mainform.cbspeedhack.Checked:=false;

  inherited destroy;
end;

constructor Thypermode.create;
var
  CEScanProcAddress:pointer;

  winhandle,possiblewinhandle: thandle;
  winprocess: dword;
  winthreadid: dword;

  x: ^byte;
  i,j: integer;
  s: string;
begin
  with mainform do
  begin
    cbpausewhilescanning.Checked:=false;
    cbpausewhilescanning.Enabled:=false;
    cbspeedhack.Enabled:=false;
    label52.Enabled:=false;
    label51.Enabled:=false;
    edit2.Enabled:=false;
    edit1.Enabled:=false;
    btnsetspeedhack.Enabled:=false;
    speedhackenabled:=false;
    cbfasterscan.Enabled:=false;
    newscan.Enabled:=false;
  end;

  hyperscanview.startaddress:=processid; //use the startaddress to identify the target process. (so dont do CE)
  hyperscanview.mainformhandle:=handle;
  hyperscanview.scanning:=false;
  hyperscanview.UseHyperscan:=true;


  if iswin2kplus then
  begin
    try
      InjectDLL(CheatEngineDir+'CEHook.dll','IHWCI');
    except
      failed:=true;
    end;
    //now wait for the other program to set scanning to true
  end
  else
  begin
    //find a window that belongs to the program (preferable the main window, the one with most objects)
    possiblewinhandle:=0;
    hyperscanwindow:=0;

    winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);
    while winhandle<>0 do
    begin
      winthreadid:=GetWindowThreadProcessId(winhandle,@winprocess);
      if winprocess=processid then
      begin
        possiblewinhandle:=winhandle;
        if GetWindow(possiblewinhandle,GW_CHILD)<>0 then break;  //if we find one that has at least one component then stop searching
      end;
      winhandle:=getwindow(winhandle,GW_HWNDNEXT);
    end;

    if possiblewinhandle=0 then exit;

    CEHOOKDLL:=LoadLibrary('CEHook.dll');
    if CEHOOKDLL=0 then exit;

    //still here so the dll is loaded
    CEScanProcAddress:=GetProcAddress(CEHOOKDLL,'MyHook');
    if (CEScanProcAddress=nil) then//something went wrong (dont know why though)
    begin
      FreeLibrary(CEHOOKDLL);
      exit;
    end;

    CEScanHook:=setwindowshookex(WH_CALLWNDPROCRET	,CEScanProcAddress,CEHOOKDLL,0); //just to get the dll inside the process
    hyperscanview.StopAddress:=CEScanhook;

    if cescanhook=0 then exit;

    hyperscanview.formscanningHandle:=CEScanHook;
    SendMessage(possiblewinhandle,wm_user+666,$33333333,0);
  end;



  inherited create(false);
end;

procedure THypermode.disablespeedhack;
var i: integer;
    a,original,written:dword;
begin
  speedhackenabled:=false;
  postmessage(hyperscanwindow,wm_user+5,1,0);
end;

procedure THypermode.FailedLoading;
begin
  cefuncproc.hypermode:=nil;
  
  with mainform do
  begin
    cbspeedhack.Enabled:=iswin2kplus;
    cbfasterscan.enabled:=true;

    cbfasterscan.Checked:=false;
    cbspeedhack.Checked:=false;
    hyperscanview.UseHyperscan:=false;

    cbPauseWhileScanning.enabled:=true;
    newscan.Enabled:=true;
  end;


end;

procedure THypermode.Success;
begin
  hyperscanwindow:=hyperscanview.hyperscanwindow;

  if mainform.cbSpeedhack.checked then
  begin
    postmessage(hyperscanwindow,wm_user+4,0,0);
    mainform.btnSetSpeedhack.Click; //set the speed
  end;

  with mainform do
  begin
    if iswin2kplus then cbspeedhack.enabled:=true;
    cbfasterscan.enabled:=true;


    label52.Enabled:=true;
    label51.Enabled:=true;
    edit2.Enabled:=true;
    edit1.Enabled:=true;
    btnsetspeedhack.Enabled:=true;
    speedhackenabled:=cbspeedhack.checked;

    cbpausewhilescanning.Enabled:=true;
    newscan.enabled:=true;
  end;

end;

procedure THypermode.execute;
var i: integer;
begin
  if failed then
  begin
    synchronize(FailedLoading);
    exit;
  end;

  self.Priority:=tpLower;
  i:=0;
  while (not hyperscanview.scanning) and (i<500) do
  begin
    sleep(10); //shouldn't take long
    inc(i);
  end;

  if not iswin2kplus then
  begin
    unhookwindowshookex(cescanhook);
    FreeLibrary(CEHOOKDLL);
  end;

  hyperscanview.scanning:=false;

  if i>=500 then
  begin
    synchronize(FailedLoading);
    exit;
  end;


  synchronize(success);
end;

end.
