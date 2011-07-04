unit HotkeyHandler;

interface

uses windows,classes,SyncObjs,cefuncproc,messages;

type thotkeyitem=record
  keys: TKeyCombo;
  windowtonotify: thandle;
  id: integer;
  //extra
  fuModifiers: word;
  uVirtKey:word;
  handler2:boolean;
  lastactivate: dword; //determines when the last time this hotkey was activated
end;

type Thotkeythread=class(tthread)
  private
  public
    suspended: boolean;
    hotkeylist: array of thotkeyitem;
    procedure execute; override;
    constructor create(suspended: boolean);
end;

function RegisterHotKey(hWnd: HWND; id: Integer; fsModifiers, vk: UINT): BOOL; stdcall;
function RegisterHotKey2(hWnd: HWND; id: Integer; keys: TKeyCombo): boolean;
function UnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
//function OldUnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
function CheckKeyCombo(keycombo: tkeycombo):boolean;
procedure ConvertOldHotkeyToKeyCombo(fsModifiers, vk: uint; var k: tkeycombo);
procedure ClearHotkeylist;
procedure SuspendHotkeyHandler;
procedure ResumeHotkeyHandler;
procedure hotkeyTargetWindowHandleChanged(oldhandle, newhandle: thandle);


var hotkeythread: THotkeythread;
    CSKeys: TCriticalSection;

    hotkeyPollInterval: integer=100;
    hotkeyIdletime: integer=100;

implementation

type tkeystate=(ks_undefined=0, ks_pressed=1, ks_notpressed=2);

var keystate: array [0..255] of tkeystate;  //0=undefined, 1=pressed, 2-not pressed

function IsKeyPressed(key: integer):boolean;
var ks: dword;
begin
  if key>255 then //not inside the list... (doubt it's valid)
  begin
    //anyhow, check if it is currently pressed
    result:=((word(getasynckeystate(key)) shr 15) and 1) = 1;
    exit;
  end;

  //look up in the list
  if keystate[key]=ks_undefined then
  begin
    ks:=getasynckeystate(key);
    if ((ks and 1)=1) then
      keystate[key]:=ks_pressed
    else
    if ((ks shr 15) and 1)=1 then
      keystate[key]:=ks_pressed
    else
      keystate[key]:=ks_notpressed; //not pressed at all
  end;

  result:=keystate[key]=ks_pressed;
end;

procedure clearkeystate;
var i: integer;
begin
  zeromemory(@keystate[0],256*sizeof(tkeystate));
  for i:=0 to 255 do
    getasynckeystate(i); //clears the last call flag
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

procedure SuspendHotkeyHandler;
begin
  if not hotkeythread.suspended then cskeys.Enter;
  hotkeythread.suspended:=true;
end;

procedure ResumeHotkeyHandler;
begin
  if hotkeythread.suspended then cskeys.Leave;
  hotkeythread.suspended:=false;
end;

procedure ClearHotkeylist;
var i:integer;
begin
  CSKeys.Enter;
  //it's now safe to clear the list
  setlength(hotkeythread.hotkeylist,0);

  CSKeys.Leave;
end;

//get all keystates and store them in a buffer

function CheckKeyCombo(keycombo: tkeycombo):boolean;
var i: integer;
    x: word;
begin
  result:=false;

  if keycombo[0]<>0 then
  begin
    result:=true;

    for i:=0 to length(keycombo)-1 do
    begin
      if (keycombo[i]=0) then exit;

      if not IsKeyPressed(keycombo[i]) then
      begin
        //not pressed
        result:=false;
        exit;
      end;

    end;

  end;


end;

function RegisterHotKey(hWnd: HWND; id: Integer; fsModifiers, vk: UINT): BOOL; stdcall;
var i: integer;
begin
  CSKeys.Enter;
  try
    setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)+1);
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].windowtonotify:=hWnd;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].id:=id;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].fuModifiers:=fsmodifiers;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].uVirtKey:=vk;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].handler2:=false;

    ConvertOldHotkeyToKeyCombo(fsModifiers, vk, hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].keys);

    result:=true;
  finally
    CSKeys.Leave;
  end;
end;


function RegisterHotKey2(hWnd: HWND; id: Integer; keys: TKeyCombo): boolean;
begin
  CSKeys.Enter;
  try
    checkkeycombo(keys);

    setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)+1);
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].windowtonotify:=hWnd;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].keys:=keys;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].id:=id;
    hotkeythread.hotkeylist[length(hotkeythread.hotkeylist)-1].handler2:=true;

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

function UnregisterHotKey(hWnd: HWND; id: Integer): BOOL; stdcall;
var i,j: integer;
    found: boolean;
begin
  result:=false;
  found:=false;
  
  CSKeys.Enter;
  try
    for i:=0 to length(hotkeythread.hotkeylist)-1 do
    begin
      if (hotkeythread.hotkeylist[i].windowtonotify=hwnd) and
         (hotkeythread.hotkeylist[i].id=id) then
      begin
        found:=true;
        for j:=i to length(hotkeythread.hotkeylist)-2 do
          hotkeythread.hotkeylist[j]:=hotkeythread.hotkeylist[j+1];

        break;
      end;
    end;

    if not found then
      result:=windows.UnregisterHotKey(hWnd,id)
    else
    begin
      setlength(hotkeythread.hotkeylist,length(hotkeythread.hotkeylist)-1);
      result:=true;
    end;
  finally
    CSKeys.Leave;
  end;
end;

constructor THotkeyThread.create(suspended: boolean);
begin
  //init
  inherited create(suspended);
end;

procedure THotkeyThread.execute;
var i: integer;
    a,b,c: dword;
    handledhotkey: boolean;
begin
  while not terminated do
  begin
    try
      CSKeys.Enter;
      try
        for i:=0 to length(hotkeylist)-1 do
        begin
          if ((hotkeylist[i].lastactivate+hotkeyIdletime)<GetTickCount) and checkkeycombo(hotkeylist[i].keys) then
          begin
            handledhotkey:=true;
            //the hotkey got pressed
            a:=hotkeylist[i].windowtonotify;
            b:=hotkeylist[i].id;
            c:=(hotkeylist[i].uVirtKey shl 16)+hotkeylist[i].fuModifiers;

            hotkeylist[i].lastactivate:=gettickcount;
            if hotkeylist[i].handler2 then
              sendmessage(a,integer(cefuncproc.WM_HOTKEY2),b,0)
            else
              sendmessage(a,WM_HOTKEY,b,c);
          end;
        end;
      finally
        CSKeys.Leave;
      end;
      
    except
    end;

    clearkeystate; //keys have been handled

    sleep(hotkeyPollInterval);
  end;
end;

initialization
  clearkeystate;
  CSKeys:=TCriticalSection.Create;
  hotkeythread:=Thotkeythread.Create(true);
  hotkeythread.Resume;

finalization
  if hotkeythread<>nil then
    hotkeythread.Terminate;
    
  CSKeys.Free;
end.

