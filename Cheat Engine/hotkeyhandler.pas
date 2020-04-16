unit HotkeyHandler;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, machotkeys,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}

  LCLType,LCLIntf,classes,sysutils, SyncObjs,CEFuncProc,messages,genericHotkey, math,
  commonTypeDefs;

type thotkeyitem=record
  {$ifdef darwin}
  machk: pointer; //needed for unregister
  {$endif}
  keys: TKeyCombo;
  windowtonotify: thandle;
  id: integer;
  memrechotkey: pointer; //if set this gets passed down to the hotkey handler in the message
  genericHotkey: TGenericHotkey;

  //extra
  fuModifiers: word;
  uVirtKey:word;
  handler2:boolean;
  lastactivate: dword; //determines when the last time this hotkey was activated
  delayBetweenActivate: integer; //If not 0 this is a userdefined delay for a hotkey, so you have have fast responding and slow responding hotkeys at the same time
end;

type
  PHotkeyItem=^THotkeyItem;

  THotkeyThreadState=(htsActive=0, htsMemrecOnly=1, htsNoMemrec=2, htsDisabled=3);


  Thotkeythread=class(tthread)
  private
    memrechk: pointer;
    generichk: TGenericHotkey;
    fstate: THotkeyThreadState;

    mainformhotkey2command: integer;

    procedure memrechotkey;
    procedure handleGenericHotkey;
    procedure mainformhotkey2;
  public
    suspended: boolean;
    hotkeylist: array of thotkeyitem;
    procedure execute; override;
    constructor create(suspended: boolean);
  published
    property state: THotkeyThreadState read fState write fState;
end;


function RegisterHotKey(hWnd: HWND; id: Integer; fsModifiers, vk: UINT): BOOL; stdcall;
function RegisterHotKey2(hWnd: HWND; id: Integer; keys: TKeyCombo; memrechotkey: pointer=nil; genericHotkey: TGenericHotkey=nil): boolean;
function UnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
function UnregisterAddressHotkey(memrechotkey: pointer): boolean;
function UnregisterGenericHotkey(generichotkey: TGenericHotkey): boolean;
function GetGenericHotkeyKeyItem(generichotkey: TGenericHotkey): PHotkeyItem;

//function OldUnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
function GetKeyComboLength(keycombo: TKeyCombo): integer;
function CheckKeyCombo(keycombo: tkeycombo; nocache: boolean=false):boolean;
procedure ConvertOldHotkeyToKeyCombo(fsModifiers, vk: uint; var k: tkeycombo);
procedure ClearHotkeylist;
procedure SuspendHotkeyHandler;
procedure ResumeHotkeyHandler;
procedure hotkeyTargetWindowHandleChanged(oldhandle, newhandle: thandle);

function IsKeyPressed(key: integer; nocache: boolean=false):boolean;


procedure InitializeHotkeyHandler;

var hotkeythread: THotkeythread;
    CSKeys: TCriticalSection;


    hotkeyPollInterval: integer=100;
    hotkeyIdletime: integer=100;

const WM_HOTKEY2=$8000;


implementation

uses MemoryRecordUnit, {$ifdef windows}xinput,winsapi,{$endif} MainUnit;

type tkeystate=(ks_undefined=0, ks_pressed=1, ks_notpressed=2);

var
    keystate: array [0..255] of tkeystate;  //0=undefined, 1=pressed, 2-not pressed
    ksCS: TCriticalSection;
    {$ifdef windows}
    ControllerState: XINPUT_STATE;
    {$endif}



function IsKeyPressed(key: integer; nocache: boolean=false):boolean;
var
    ks: dword;
    sks: short;
begin
  result:=false;
  if key>255 then //not inside the list... (doubt it's valid)
  begin
    //anyhow, check if it is currently pressed

    {$ifdef windows}
    if (key>=VK_PAD_A) and (key<=VK_PAD_RTHUMB_DOWNLEFT) then
    begin
      //controller key
      if InitXinput then
      begin
        if ControllerState.dwPacketNumber=0 then   //needs an update
          if XInputGetState(0, ControllerState)<>0 then exit;

        case key of
          VK_PAD_A: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_A)>0);
          VK_PAD_B: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_B)>0);
          VK_PAD_X: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_X)>0);
          VK_PAD_Y: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_Y)>0);

          VK_PAD_RSHOULDER: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_SHOULDER)>0);
          VK_PAD_LSHOULDER: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_SHOULDER)>0);

          VK_PAD_LTRIGGER: exit(ControllerState.Gamepad.bLeftTrigger>40);
          VK_PAD_RTRIGGER: exit(ControllerState.Gamepad.bRightTrigger>40);

          VK_PAD_DPAD_UP: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_UP)>0);
          VK_PAD_DPAD_DOWN: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_DOWN)>0);
          VK_PAD_DPAD_LEFT: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_LEFT)>0);
          VK_PAD_DPAD_RIGHT: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_DPAD_RIGHT)>0);

          VK_PAD_START: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_START)>0);
          VK_PAD_BACK: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_BACK)>0);

          VK_PAD_LTHUMB_PRESS: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_LEFT_THUMB)>0);
          VK_PAD_RTHUMB_PRESS: exit((ControllerState.Gamepad.wButtons and XINPUT_GAMEPAD_RIGHT_THUMB)>0);

          VK_PAD_LTHUMB_UP: exit(ControllerState.Gamepad.sThumbLY>20000);
          VK_PAD_LTHUMB_DOWN: exit(ControllerState.Gamepad.sThumbLY<-20000);
          VK_PAD_LTHUMB_RIGHT: exit(ControllerState.Gamepad.sThumbLX>20000);
          VK_PAD_LTHUMB_LEFT: exit(ControllerState.Gamepad.sThumbLX<-20000);

          VK_PAD_LTHUMB_UPLEFT: exit((ControllerState.Gamepad.sThumbLY>20000) and (ControllerState.Gamepad.sThumbLX<-20000));
          VK_PAD_LTHUMB_UPRIGHT: exit((ControllerState.Gamepad.sThumbLY>20000) and (ControllerState.Gamepad.sThumbLX>20000));
          VK_PAD_LTHUMB_DOWNRIGHT: exit((ControllerState.Gamepad.sThumbLY<-20000) and (ControllerState.Gamepad.sThumbLX>20000));
          VK_PAD_LTHUMB_DOWNLEFT: exit((ControllerState.Gamepad.sThumbLY<-20000) and (ControllerState.Gamepad.sThumbLX<-20000));

          VK_PAD_RTHUMB_UP: exit(ControllerState.Gamepad.sThumbRY>20000);
          VK_PAD_RTHUMB_DOWN: exit(ControllerState.Gamepad.sThumbRY<-20000);
          VK_PAD_RTHUMB_RIGHT: exit(ControllerState.Gamepad.sThumbRX>20000);
          VK_PAD_RTHUMB_LEFT: exit(ControllerState.Gamepad.sThumbRX<-20000);

          VK_PAD_RTHUMB_UPLEFT: exit((ControllerState.Gamepad.sThumbRY>20000) and (ControllerState.Gamepad.sThumbRX<-20000));
          VK_PAD_RTHUMB_UPRIGHT: exit((ControllerState.Gamepad.sThumbRY>20000) and (ControllerState.Gamepad.sThumbRX>20000));
          VK_PAD_RTHUMB_DOWNRIGHT: exit((ControllerState.Gamepad.sThumbRY<-20000) and (ControllerState.Gamepad.sThumbRX>20000));
          VK_PAD_RTHUMB_DOWNLEFT: exit((ControllerState.Gamepad.sThumbRY<-20000) and (ControllerState.Gamepad.sThumbRX<-20000));

        end;
      end;
    end
    else
    {$endif}
      result:=((word(getasynckeystate(key)) shr 15) and 1) = 1;
    exit;
  end;

  //look up in the list
  sks:=0;
  ksCS.enter;
  if keystate[key]=ks_undefined then
  begin

    sks:=getasynckeystate(longint(key));



    if ((sks and 1)=1) then
    begin


      keystate[key]:=ks_pressed
    end
    else
    begin
      if ((sks shr 15) and 1)=1 then
      begin


        keystate[key]:=ks_pressed
      end
      else
      begin


        keystate[key]:=ks_notpressed; //not pressed at all
      end;
    end;
  end;

  result:=keystate[key]=ks_pressed;

  if nocache and (not result) then
  begin

    if sks<>0 then
      result:=(sks and $8001)<>0;

    if result then exit;

    sks:=GetAsyncKeyState(longint(key));
    result:=(sks and $8001)<>0;

    if result=false then
    begin
      sks:=GetKeyState(key);
      result:=(sks and $8001)<>0;
      if not result then
      begin
        beep;
      end;
    end;
  end;

  ksCS.Leave;
end;

procedure clearkeystate;
var i: integer;
begin
  ksCS.enter;
  zeromemory(@keystate[0],256*sizeof(tkeystate));
  for i:=0 to 255 do
    getasynckeystate(i); //clears the last call flag

  {$ifdef windows}
  ControllerState.dwPacketNumber:=0;
  {$endif}
  ksCS.leave;
end;

procedure hotkeyTargetWindowHandleChanged(oldhandle, newhandle: thandle);
{
Called when the handle of a window is changed. This will update all associated hotkeys
}
var i: integer;
begin
  cskeys.Enter;
  try
    for i:=0 to length(hotkeythread.hotkeylist)-1 do
      if hotkeythread.hotkeylist[i].windowtonotify=oldhandle then
        hotkeythread.hotkeylist[i].windowtonotify:=newhandle;
  finally
    cskeys.Leave;
  end;
end;

var presuspendstate: THotkeyThreadState;
procedure SuspendHotkeyHandler;
begin
  if hotkeythread<>nil then
  begin
    presuspendstate:=hotkeythread.state;
    hotkeythread.state:=htsDisabled;
  end;
end;

procedure ResumeHotkeyHandler;
begin
  if hotkeythread<>nil then
    hotkeythread.state:=presuspendstate;
end;

procedure ClearHotkeylist;
begin
  CSKeys.Enter;
  //it's now safe to clear the list
  setlength(hotkeythread.hotkeylist,0);

  CSKeys.Leave;
end;

//get all keystates and store them in a buffer

function GetKeyComboLength(keycombo: TKeyCombo): integer;
var i: integer;
begin
  result:=0;
  for i:=0 to length(keycombo)-1 do
    if keycombo[i]<>0 then
      inc(result);
end;

function CheckKeyCombo(keycombo: tkeycombo; nocache: boolean=false):boolean;
var i: integer;
begin
  result:=false;

  if keycombo[0]<>0 then
  begin
    result:=true;

    for i:=0 to length(keycombo)-1 do
    begin
      if (keycombo[i]=0) then exit;

      if not IsKeyPressed(keycombo[i], nocache) then
      begin
        //not pressed
        result:=false;
        exit;
      end;

    end;

  end;


end;

{$ifdef darwin}
procedure MacHotkeyCallBackOriginal(sender: TObject; param: pointer);
begin
  if (hotkeythread<>nil) and (not (hotkeythread.state in [htsActive,htsNoMemrec])) then exit; //ignore

  mainform.hotkey2(integer(param));
end;
{$endif}

{$ifdef darwin}
procedure MacHotkeyCallBackMemRecHotkey(sender: TObject; param: pointer);
begin
  if (hotkeythread<>nil) and (not (hotkeythread.state in [htsActive,htsMemrecOnly])) then exit; //ignore

  TMemoryRecordHotkey(param).DoHotkey;
end;
{$endif}

{$ifdef darwin}
procedure MacHotkeyCallBackGenericHotkey(sender: TObject; param: pointer);
begin
  if (hotkeythread<>nil) and (not (hotkeythread.state in [htsActive,htsNoMemrec])) then exit; //ignore

  if assigned(TGenericHotkey(param).onNotify) then
    TGenericHotkey(param).onNotify(tobject(param));
end;
{$endif}

function RegisterHotKey(hWnd: HWND; id: Integer; fsModifiers, vk: UINT): BOOL; stdcall;
{$ifdef darwin}
var
  keys: TKeyCombo;
  hk: pointer;
{$endif}
begin
  {$ifdef darwin}
  outputdebugstring('RegisterHotKey Old. Modifiers='+inttohex(fsModifiers,2)+' vk='+inttohex(vk,2));
  ConvertOldHotkeyToKeyCombo(fsModifiers, vk, keys);
  hk:=RegisterHotkeyMac(keys,MacHotkeyCallBackOriginal,pointer(id));
  if hk=nil then exit(false);
  {$endif}

  CSKeys.Enter;
  try
    setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)+1);
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].windowtonotify:=hWnd;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].id:=id;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].fuModifiers:=fsmodifiers;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].uVirtKey:=vk;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].handler2:=false;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].delayBetweenActivate:=0;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].memrechotkey:=nil;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].generichotkey:=nil;

    ConvertOldHotkeyToKeyCombo(fsModifiers, vk, hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].keys);

    {$ifdef darwin}
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].machk:=hk;
    {$endif}

    result:=true;
  finally
    CSKeys.Leave;
  end;
end;




function RegisterHotKey2(hWnd: HWND; id: Integer; keys: TKeyCombo; memrechotkey: pointer=nil; genericHotkey: TGenericHotkey=nil): boolean;
{$ifdef darwin}
var
  hk: pointer;
{$endif}
begin


  {$ifdef darwin}
  if keys[0]=0 then
    outputdebugstring('RegisterHotKey2 called with empty hotkey');

  outputdebugstring('RegisterHotKey2');
  if (memrechotkey=nil) and (generichotkey=nil) then
  begin
    outputdebugstring('old hotkey');
    hk:=RegisterHotkeyMac(keys,MacHotkeyCallBackOriginal,pointer(id))
  end
  else
  begin
    outputdebugstring('new hotkey');
    if memrechotkey<>nil then
    begin
      outputdebugstring('memrechotkey');
      hk:=RegisterHotkeyMac(keys,MacHotkeyCallBackMemRecHotkey,memrechotkey)
    end
    else
    begin
      outputdebugstring('generichotkey');
      hk:=RegisterHotkeyMac(keys,MacHotkeyCallBackGenericHotkey,generichotkey);
    end;
  end;
  {$endif}


  CSKeys.Enter;
  try
    checkkeycombo(keys);

    setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)+1);
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].windowtonotify:=hWnd;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].keys:=keys;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].id:=id;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].fuModifiers:=0;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].uVirtKey:=0;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].handler2:=true;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].delayBetweenActivate:=0;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].memrechotkey:=memrechotkey;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].generichotkey:=genericHotkey;
    {$ifdef darwin}
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].machk:=hk;
    {$endif}

    result:=true;
  finally
    CSKeys.Leave;
  end;
end;

procedure ConvertOldHotkeyToKeyCombo(fsModifiers, vk: uint; var k: tkeycombo);
{
Function will take the fsmodifier and vk of the normal registerhotkey call and
fill it in into a TKeyCombo type array used by ce's hotkey handler
}
var i: integer;
begin
  k[0]:=vk;
  i:=1;

  if (fsModifiers and MOD_CONTROL)>0 then
  begin
    k[i]:=vk_control;
    inc(i);
  end;

  if (fsModifiers and MOD_ALT)>0 then
  begin
    k[i]:=vk_menu;
    inc(i);
  end;

  if (fsModifiers and MOD_SHIFT)>0 then
  begin
    k[i]:=vk_shift;
    inc(i);
  end;

  k[i]:=0;
end;

function getGenericHotkeyKeyItem(generichotkey: TGenericHotkey): PHotkeyItem;
//pre: ONLY call this when the CSKeys critical section has been acquired
var i: integer;
begin
  result:=nil;
  for i:=0 to length(hotkeythread.hotkeylist)-1 do
  begin
    if hotkeythread.hotkeylist[i].genericHotkey=genericHotkey then
    begin
      //found, so delete it
      result:=@hotkeythread.hotkeylist[i];
      exit;
    end;
  end;
end;

function UnregisterGenericHotkey(generichotkey: TGenericHotkey): boolean;
var i,j: integer;
begin
  result:=false;

  CSKeys.enter;
  try
    for i:=0 to length(hotkeythread.hotkeylist)-1 do
    begin
      if hotkeythread.hotkeylist[i].genericHotkey=genericHotkey then
      begin
        //found, so delete it
        {$ifdef darwin}
        UnregisterHotkeyMac(hotkeythread.hotkeylist[i].machk);
        {$endif}

        for j:=i to length(hotkeythread.hotkeylist)-2 do
          hotkeythread.hotkeylist[j]:=hotkeythread.hotkeylist[j+1];


        setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)-1);
        result:=true;
        exit;
      end;
    end;
  finally
    CSKeys.Leave;
  end;
end;

function UnregisterAddressHotkey(memrechotkey: pointer): boolean;
var i,j: integer;
begin
  result:=false;

  CSKeys.enter;
  try
    for i:=0 to length(hotkeythread.hotkeylist)-1 do
    begin
      if hotkeythread.hotkeylist[i].memrechotkey=memrechotkey then
      begin
        //found, so delete it
        {$ifdef darwin}
        UnregisterHotkeyMac(hotkeythread.hotkeylist[i].machk);
        {$endif}

        for j:=i to length(hotkeythread.hotkeylist)-2 do
          hotkeythread.hotkeylist[j]:=hotkeythread.hotkeylist[j+1];


        setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)-1);
        result:=true;
        exit;
      end;
    end;
  finally
    CSKeys.Leave;
  end;
end;

function UnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
var i,j: integer;
begin
  result:=false;

  CSKeys.Enter;
  try
    for i:=0 to length(hotkeythread.hotkeylist)-1 do
    begin
      if (hotkeythread.hotkeylist[i].windowtonotify=hwnd) and
         (hotkeythread.hotkeylist[i].id=id) then
      begin
        {$ifdef darwin}
        UnregisterHotkeyMac(hotkeythread.hotkeylist[i].machk);
        {$endif}

        for j:=i to length(hotkeythread.hotkeylist)-2 do
          hotkeythread.hotkeylist[j]:=hotkeythread.hotkeylist[j+1];

        setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)-1);
        result:=true;
        exit;
      end;
    end;

    //still here so not found, try windows
    {$ifdef windows}
    result:=windows.UnregisterHotKey(hWnd,id)
    {$endif}



  finally
    CSKeys.Leave;
  end;
end;

constructor THotkeyThread.create(suspended: boolean);
begin
  //init
  inherited create(suspended);
end;

procedure Thotkeythread.memrechotkey;
begin
//  sendmessage(mainform.handle,integer(cefuncproc.WM_HOTKEY2),0,ptrUint(memrechk));

  //not 100% sure why sendmessage works here but not from within the thread...
  //but since we're here anyhow:
  TMemoryRecordHotkey(memrechk).DoHotkey;
end;

procedure Thotkeythread.handleGenericHotkey;
begin
  if assigned(generichk.onNotify) then
    generichk.onNotify(generichk);
end;

procedure THotkeythread.mainformhotkey2;
begin
  mainform.hotkey2(mainformhotkey2command);
end;

procedure THotkeyThread.execute;
type
  TActiveHotkeyData=record   //structure to hold hotkey and keycount
    hotkeylistItem: PHotkeyItem;
    keycount: integer;
  end;
  PActiveHotkeyData=^TActiveHotkeyData;
var i: integer;
    a,b,c: dword;

    activeHotkeyList: Tlist;
    maxActiveKeyCount: integer;

    tempHotkey: PActiveHotkeyData;

    handeit: boolean;
begin
  activeHotkeyList:=Tlist.create;
  while not terminated do
  begin
    {$ifdef darwin}
    //using mac hotkeys instead
    if loadMacHotkeyFunctions then
    begin
      sleep(1000);
      continue;
    end;

    //else use the getasynckeystate method which can only get function keys and modifiers
    {$endif}
    try
      CSKeys.Enter;
      try
        //get a list of all the activated hotkeys

        maxActiveKeyCount:=0;
        for i:=0 to length(hotkeylist)-1 do
        begin
          if
            ((state in [htsActive,htsMemrecOnly]) and (hotkeylist[i].memrechotkey<>nil) and (checkkeycombo(TMemoryrecordHotkey(hotkeylist[i].memrechotkey).keys))) or
            ((state in [htsActive,htsNoMemrec]) and (hotkeylist[i].genericHotkey<>nil) and (checkkeycombo(TGenericHotkey(hotkeylist[i].generichotkey).keys))) or
            ((state in [htsActive,htsNoMemrec]) and ((hotkeylist[i].memrechotkey=nil) and (hotkeylist[i].generichotkey=nil)) and checkkeycombo(hotkeylist[i].keys))

          then
          begin
            //OutputDebugString('Hotkey triggered');

            //the hotkey got pressed
            //6.3: Add it to a list of hotkeys
            tempHotkey:=getmem(sizeof(tempHotkey));

            tempHotkey.hotkeylistItem:=@hotkeylist[i];
            if (hotkeylist[i].memrechotkey<>nil) then
              tempHotkey.keycount:=GetKeyComboLength(TMemoryrecordHotkey(hotkeylist[i].memrechotkey).keys)
            else
            if (hotkeylist[i].genericHotkey<>nil) then
              tempHotkey.keycount:=GetKeyComboLength(TGenericHotkey(hotkeylist[i].generichotkey).keys)
            else
              tempHotkey.keycount:=GetKeyComboLength(hotkeylist[i].keys);

            maxActiveKeyCount:=max(maxActiveKeyCount, tempHotkey.keycount);


            activeHotkeyList.Add(temphotkey);

          end;

        end;

        //now go through the activeHotkeyList looking for a keycount of maxActiveKeyCount
        i:=0;
        while i<activeHotkeyList.count do
        begin
          OutputDebugString('Handling hotkey');
          temphotkey:=PActiveHotkeyData(activeHotkeyList[i]);
          if temphotkey.keycount=maxActiveKeyCount then //it belongs to the max complex hotkey count
          begin
            OutputDebugString('1');
            if ((tempHotkey.hotkeylistItem.lastactivate+ifthen(tempHotkey.hotkeylistItem.delaybetweenActivate>0, tempHotkey.hotkeylistItem.delaybetweenActivate, hotkeyIdletime))<GetTickCount) then //check if it can be activated
            begin
              a:=tempHotkey.hotkeylistItem.windowtonotify;
              b:=tempHotkey.hotkeylistItem.id;
              c:=(tempHotkey.hotkeylistItem.uVirtKey shl 16)+tempHotkey.hotkeylistItem.fuModifiers;
              OutputDebugString('2');

              tempHotkey.hotkeylistItem.lastactivate:=gettickcount;
              if tempHotkey.hotkeylistItem.handler2 then
              begin
                OutputDebugString('3');
                if tempHotkey.hotkeylistItem.memrechotkey<>nil then
                begin
                  OutputDebugString('4');
                  memrechk:=tempHotkey.hotkeylistItem.memrechotkey;

                  CSKeys.leave;
                  OutputDebugString('5');
                  Synchronize(memrechotkey);
                  OutputDebugString('6');
                  cskeys.enter;
                  OutputDebugString('7');
                end
                else
                begin
                  OutputDebugString('8');
                  if tempHotkey.hotkeylistItem.generichotkey<>nil then
                  begin
                    OutputDebugString('9');
                    generichk:=tempHotkey.hotkeylistItem.genericHotkey;
                    synchronize(handlegenerichotkey);

                    //sendmessage(a,integer(WM_HOTKEY2),1,ptrUint(tempHotkey.hotkeylistItem.genericHotkey))
                  end
                  else
                  begin

                    OutputDebugString('10');

                    mainformhotkey2command:=b;
                    synchronize(mainformhotkey2);

                    //sendmessage(a,integer(WM_HOTKEY2),b,0)
                  end
                end;
                OutputDebugString('11');

              end
              {$ifdef windows}
              else
                sendmessage(a,WM_HOTKEY,b,c)
              {$endif};

              OutputDebugString('12');
            end;
          end;

          //cleanup the memory as well while we're at it
          freememandnil(temphotkey);
          inc(i);
        end;
        activeHotkeyList.clear;

      finally
        CSKeys.Leave;
      end;
      
    except
    end;

    clearkeystate; //keys have been handled

    sleep(hotkeyPollInterval);
  end;
end;

procedure InitializeHotkeyHandler;
begin
  if hotkeythread=nil then
    hotkeythread:=Thotkeythread.Create(false);
end;

initialization
  ksCS:=TCriticalSection.Create;
  clearkeystate;

  CSKeys:=TCriticalSection.Create;

  InitializeHotkeyHandler;


finalization
  if hotkeythread<>nil then
    hotkeythread.Terminate;
    
  CSKeys.Free;
end.

