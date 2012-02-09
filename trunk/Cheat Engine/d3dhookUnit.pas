unit d3dhookUnit;
{
This unit will inject the d3d hook dll into the target process and hook the
apropriate functions.
A shared object will be used for communicating states and data
}

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, sharedMemory, forms, graphics, cefuncproc,
  newkernelhandler, controls, Clipbrd;

type
  TCEMessage=packed record
    uMsg: DWORD;
    wParam: UINT64;
    lParam: UINT64;
    character: DWORD;
  end;

  TResourceInfo=packed record
    valid: integer;
    updatedpos: integer;
    updatedresource: integer;
    width: integer;
    height: integer;
    x: integer;
    y: integer;
    alphablend: Single;
    resourcesize: integer;
    resourceoffset: integer; //offset into the shared memory region containing the bitmat info
  end;

  type TResourceInfoArray=packed array [0..0] of TResourceInfo;

  TD3DHookShared=packed record
    cheatenginedir: array [0..255] of char;
    dxgi_present: UINT64;
    d3d9_present: UINT64;
    d3d9_reset: UINT64;

    d3d9_drawprimitive: UINT64;
    d3d9_drawindexedprimitive: UINT64;
    d3d9_drawprimitiveup: UINT64;
    d3d9_drawindexedprimitiveup: UINT64;
    d3d9_drawrectpatch: UINT64;
    d3d9_drawtripatch: UINT64;

    d3d10_drawindexed: UINT64;
    d3d10_draw: UINT64;
    d3d10_drawindexedinstanced: UINT64;
    d3d10_drawinstanced: UINT64;
    d3d10_drawauto: UINT64;

    d3d11_drawindexed: UINT64;
    d3d11_draw: UINT64;
    d3d11_drawindexedinstanced: UINT64;
    d3d11_drawinstanced: UINT64;
    d3d11_drawauto: UINT64;


    dxgi_newpresent: UINT64;
    d3d9_newpresent: UINT64;
    d3d9_newreset: UINT64;

    d3d9_newdrawprimitive: UINT64;
    d3d9_newdrawindexedprimitive: UINT64;
    d3d9_newdrawprimitiveup: UINT64;
    d3d9_newdrawindexedprimitiveup: UINT64;
    d3d9_newdrawrectpatch: UINT64;
    d3d9_newdrawtripatch: UINT64;

    d3d10_newdrawindexed: UINT64;
    d3d10_newdraw: UINT64;
    d3d10_newdrawindexedinstanced: UINT64;
    d3d10_newdrawinstanced: UINT64;
    d3d10_newdrawauto: UINT64;

    d3d11_newdrawindexed: UINT64;
    d3d11_newdraw: UINT64;
    d3d11_newdrawindexedinstanced: UINT64;
    d3d11_newdrawinstanced: UINT64;
    d3d11_newdrawauto: UINT64;


    dxgi_originalpresent: UINT64;
    d3d9_originalpresent: UINT64;
    d3d9_originalreset: UINT64;

    d3d9_originaldrawprimitive: UINT64;
    d3d9_originaldrawindexedprimitive: UINT64;
    d3d9_originaldrawprimitiveup: UINT64;
    d3d9_originaldrawindexedprimitiveup: UINT64;
    d3d9_originaldrawrectpatch: UINT64;
    d3d9_originaldrawtripatch: UINT64;

    d3d10_originaldrawindexed: UINT64;
    d3d10_originaldraw: UINT64;
    d3d10_originaldrawindexedinstanced: UINT64;
    d3d10_originaldrawinstanced: UINT64;
    d3d10_originaldrawauto: UINT64;

    d3d11_originaldrawindexed: UINT64;
    d3d11_originaldraw: UINT64;
    d3d11_originaldrawindexedinstanced: UINT64;
    d3d11_originaldrawinstanced: UINT64;
    d3d11_originaldrawauto: UINT64;


    wireframe: integer;
    disabledzbuffer: integer;

    hookwnd: integer;
    clipmouseinwindow: integer;

    clickedoverlay: integer;
    clickedx: integer;
    clickedy: integer;

    console: packed record
      hasconsole: integer;
      consolevisible: integer;
      consolekey: dword;
      overlayid: integer;
      lastmessage: TCEMessage;

    end;

    lastHwnd: DWORD;



    MouseOverlayId: integer;
    OverLayHasUpdate: integer; //When set to not 0 the renderer will check what needs to be updated
    overlaycount: integer;
    resources: TResourceInfoArray;
    //followed by the resource data


  end;
  PD3DHookShared=^TD3DHookShared;


type
  TD3DClickEvent=procedure(overlayid: integer; x,y: integer) of object;

  TD3DHook=class;

  TD3DMessageHandler=class(tthread)
  private
    owner: TD3DHook;
    overlayid, x,y: integer; //clicked overlay

    lastmessage: TCEMessage;

    consolelog: Tstringlist;
    consolecursorpos: integer;
    consolecommand: string;


    procedure doclick;
    procedure dokeyboard;
  public
    procedure execute; override;
  end;


  TD3DHook=class
  private
    sharename: string;
    shared: PD3DHookShared; //local address of the D3DHookShared structure

    fmhandle: THandle;

    images: array of TPicture;

    isupdating: integer;

    maxsize: integer;
    fprocessid: dword;

    hasclickevent: THandle;
    hashandledclickevent: THandle;

    haskeyboardevent: THandle;        //todo: combine into one "HasMessage" event, but for now, to make sure I don't break anything, this method...
    hashandledkeyboardevent: THandle;

    messagehandler: TD3DMessageHandler;

    consoleOverlayid: integer;
    consoleImage: TPicture;


    procedure waitforready;
    procedure UpdateResourceData;
    procedure UpdateConsoleOverlay(command: string; log: Tstrings);
  public
    onclick: TD3DClickEvent;
    procedure beginupdate;
    procedure endupdate;

    function createOverlayFromPicture(p: TPicture; x,y: integer): integer;
    procedure SetOverlayAlphaBlend(overlayid: integer; blend: single);
    procedure SetOverlayVisibility(overlayid: integer; state: boolean);
    procedure updateOverlayImage(overlayid: integer; skipsync: boolean=false);
    procedure updateOverlayPosition(overlayid,x,y: integer);
    procedure setOverlayAsMouse(overlayid: integer);
    procedure setDisabledZBuffer(state: boolean);
    procedure setWireframeMode(state: boolean);
    procedure setMouseClip(state: boolean);

    function getWidth: integer;
    function getHeight: integer;

    procedure createConsole(virtualkey: DWORD);
    procedure reinitializeConsoleIfNeeded;

    constructor create(size: integer; hookhwnd: boolean=true);
    destructor destroy; override;
    property processid: dword read fprocessid;
  end;

var D3DHook: TD3DHook;

function safed3dhook(size: integer=16*1024*1024; hookwindow: boolean=true): TD3DHook;

implementation

uses frmautoinjectunit, autoassembler;

procedure TD3DMessageHandler.dokeyboard;
var virtualkey: dword;
    scancode: dword;


    s: pchar;
begin
  //check the size of the window. If it's changed, reinitialize the consoleoverlay
  owner.ReinitializeConsoleIfNeeded;

  //handle the key
  virtualkey:=owner.shared.console.lastmessage.wParam;
  scancode:=(owner.shared.console.lastmessage.lparam shr 16) and $FF;
{
  GetAsyncKeyState(VK_CAPITAL);


  getmem(s,128);

  ToUnicode(virtualkey, MapVirtualKey(virtualkey, 0), @owner.shared.console.keyboardstate[0], s, 64,0);
 }
  if owner.shared.console.lastmessage.character<>0 then
  begin
    s:=@owner.shared.console.lastmessage.character;
  end
  else
    s:='No valid key';


  owner.updateConsoleOverlay(s,consolelog);

  // owner.updateConsoleOverlay('cAn i dO qUPERcASE?:',consolelog);

end;

procedure TD3DMessageHandler.doclick;
begin
  if assigned(owner.onclick) then
    owner.onclick(overlayid+1, x,y);
end;

procedure TD3DMessageHandler.execute;
var eventlist: array of THandle;
    r: dword;
begin
  consolelog:=tstringlist.create;
  consolecursorpos:=1;

  setlength(eventlist,2);
  eventlist[0]:=owner.hasclickevent;
  eventlist[1]:=owner.haskeyboardevent;

  while (not terminated) do
  begin
    r:=WaitForMultipleObjects(2, @eventlist[0], false, 5000);
    case r of
      WAIT_OBJECT_0:
      begin
        //click event: quickly save the variables and tell the game to continue
        x:=owner.shared.clickedx;
        y:=owner.shared.clickedy;
        overlayid:=owner.shared.clickedoverlay;
        SetEvent(owner.hashandledclickevent);

        Synchronize(doclick);
      end;

      WAIT_OBJECT_0+1:
      begin
        //keyboard event
        lastmessage:=owner.shared.console.lastmessage;
        Synchronize(dokeyboard);
        SetEvent(owner.hashandledkeyboardevent);
      end;
    end;

  end;

end;

procedure TD3DHook.UpdateConsoleOverlay(command: string; log: Tstrings);
var c: TCanvas;
    lineheight: integer;
    i: integer;
    linepos: integer;
    minelinepos: integer;
begin
  //Build the console from bottom to top
  //Bottom is the command line, black
  //above that is the history and log
  //Make sure that the log does not come above "consoleimage.Height"

  //first fill it to dark grey
  c:=consoleImage.Bitmap.Canvas;

  c.Font.Color:=$fefefe;
  c.Font.Style:=[fsBold];
  lineheight:=c.GetTextHeight('FUUUU');

  c.Brush.Color:=$111111;
  c.FillRect(0,0,c.Width, c.Height-(lineheight+2));

  c.Brush.color:=$000000;
  c.FillRect(0,c.height-(lineheight+1),c.width, c.height);

  c.pen.color:=clred;
  c.Line(0,c.Height-(lineheight+2), c.width, c.Height-(lineheight+2));

  //now render the text
  //command
  c.TextOut(4,c.Height-(lineheight+1), command);

  //and the log (from bottom to top, till the max is reached)
  c.Brush.Color:=$111111;

  if log<>nil then
  begin
    linepos:=c.Height-(lineheight+2)-lineheight; //the last line

    for i:=log.Count-1 downto 0 do
    begin
      c.textout(4, linepos, log[i]);
      linepos:=linepos-lineheight;
      if linepos<0 then break; //max reached
    end;
  end;


  updateOverlayImage(consoleOverlayid, true);
end;

procedure TD3DHook.reinitializeConsoleIfNeeded;
begin
  if getHeight=0 then exit;
  if getWidth=0 then exit;

  if consoleOverlayid=-1 then
  begin
    //first time created
    consoleimage:=TPicture.Create;
    consoleimage.Bitmap.width:=getWidth;
    consoleimage.Bitmap.Height:=getheight div 3;

    consoleOverlayid:=createOverlayFromPicture(consoleimage,0, getheight-(getheight div 3));
    SetOverlayAlphaBlend(consoleOverlayid, 88);
    SetOverlayVisibility(consoleOverlayid, false);
  end;

  if (consoleImage.Width<>getWidth) or (consoleImage.Height<>getHeight div 3) then
  begin
    consoleImage.bitmap.Width:=getWidth;
    consoleimage.bitmap.Height:=getheight div 3;
    shared.resources[consoleOverlayid-1].y:=getheight-(getheight div 3);
  end;


end;

procedure TD3DHook.createConsole(virtualkey: DWORD);
var s: tstringlist;
begin
  if shared.hookwnd=0 then raise exception.create('D3DHook was initialized without hooking the window. Restart the target app');
  //create an overlay

  reinitializeConsoleIfNeeded;

  //debug code
  s:=tstringlist.create;
  s.add('This');
  s.add('Was');
  s.add('A');
  s.add('Test');
  UpdateConsoleOverlay('Bla', s);


  shared.console.consolekey:=192; //tilde
  shared.console.overlayid:=consoleOverlayid-1;
  shared.console.hasconsole:=1;


end;

procedure TD3DHook.setDisabledZBuffer(state: boolean);
begin
  if state then
    shared.disabledzbuffer:=1
  else
    shared.disabledzbuffer:=0;
end;

procedure TD3DHook.setWireframeMode(state: boolean);
begin
  if state then
    shared.wireframe:=1
  else
    shared.wireframe:=0;
end;

procedure TD3DHook.setMouseClip(state: boolean);
begin
  if state then
    shared.clipmouseinwindow:=1
  else
    shared.clipmouseinwindow:=0;
end;

function TD3DHook.getWidth: integer;
var x: trect;
begin
  if GetClientRect(shared.lastHwnd, x) then
    result:=x.Right-x.left
  else
    result:=0;

end;

function TD3DHook.getHeight: integer;
var x: trect;
begin
  if GetClientRect(shared.lastHwnd, x) then
    result:=x.bottom-x.top
  else
    result:=0;
end;

procedure TD3DHook.beginupdate;
var i: integer;
begin
  waitforready;
  inc(isupdating);
end;

procedure TD3DHook.endupdate;
begin
  if isupdating>0 then
  begin
    dec(isupdating);
    if isupdating=0 then
    begin
      updateResourceData;
      shared.OverLayHasUpdate:=1;
    end;
  end;
end;

procedure TD3DHook.updateOverlayImage(overlayid: integer; skipsync: boolean=false);
begin
  if skipsync then
    inc(isupdating);  //prevents the locking

  beginupdate;
  shared.resources[overlayid-1].updatedresource:=1;
  endupdate;

  if skipsync then
    endupdate;
end;

procedure TD3DHook.setOverlayAsMouse(overlayid: integer);
begin
  shared.MouseOverlayId:=overlayid-1; //no update necesary
end;

procedure TD3DHook.updateOverlayPosition(overlayid,x,y: integer);
begin
  beginupdate;
  shared.resources[overlayid-1].x:=x;
  shared.resources[overlayid-1].y:=y;
  shared.resources[overlayid-1].updatedpos:=1;
  endupdate;
end;

procedure TD3DHook.SetOverlayAlphaBlend(overlayid: integer; blend: single);
begin
  shared.resources[overlayid-1].alphaBlend:=blend / 100.0;
end;

procedure TD3DHook.SetOverlayVisibility(overlayid: integer; state: boolean);
begin

  if state then
    shared.resources[overlayid-1].valid:=1
  else
    shared.resources[overlayid-1].valid:=0;

  if isupdating=0 then
    shared.OverLayHasUpdate:=1;
end;

procedure TD3DHook.UpdateResourceData;
//Fill in the resources and the offsets pointing to them
var
  i,j: integer;
  s: TMemoryStream;
  start: PByteArray;
begin
  //now update all the entries
  s:=tmemorystream.Create;
  start:=@shared.resources[shared.overlaycount];

  try
    for i:=0 to shared.overlaycount-1 do
    begin
//      if (shared.resources[i].valid<>0) then
      begin
        s.Clear;
        images[i].SaveToStream(s);


        shared.resources[i].height:=images[i].Height;
        shared.resources[i].width:=images[i].width;

        shared.resources[i].resourceoffset:=PtrUInt(start)-ptruint(shared);

        if shared.resources[i].resourceoffset+s.Size>maxsize then
        begin
          //out of memory, set this and all following overlays to unusable (stays valid so if a previous overlays is destroyed one might become available again)
          for j:=i to shared.overlaycount-1 do
            shared.resources[j].resourceoffset:=0;  //mark as invalid

          exit;
        end;

        //copy the resource to the target process
        CopyMemory(start, s.Memory, s.Size);


        shared.resources[i].resourcesize:=s.Size;
        inc(start, s.size);
      end;
    end;

  finally
    s.free;
  end;
end;

procedure TD3DHook.waitforready;
var i: integer;
//wait till the overlayhasupdate variable is set to 0. Timeout of 2 second  (0.5 fps games suck)
begin

  if isupdating=0 then
  begin
    i:=0;
    while (i<200) and (shared.OverLayHasUpdate<>0) do    //max 2 second
    begin
      sleep(10); //wait till the last update has been handled
      inc(i);
    end;
    shared.OverLayHasUpdate:=0; //screw it
  end;
end;

function TD3DHook.createOverlayFromPicture(p: TPicture; x,y: integer): integer;
var
  i: integer;
begin
  setlength(images, length(images)+1);
  result:=length(images);

  images[result-1]:=p;

  beginupdate;


  shared.overlaycount:=result;


  shared.resources[result-1].height:=p.Height;
  shared.resources[result-1].width:=p.width;
  shared.resources[result-1].x:=x;
  shared.resources[result-1].y:=y;
  shared.resources[result-1].alphablend:=1.0;

  shared.resources[result-1].updatedpos:=1;
  shared.resources[result-1].updatedresource:=1;
  shared.resources[result-1].resourceoffset:=0; //this will be updated when endupdate is called which calls updateresourcedata when ready
  shared.resources[result-1].resourcesize:=0;
  shared.resources[result-1].valid:=1;



  endupdate;

end;

destructor TD3DHook.Destroy;
begin
  UnmapViewOfFile(shared);
  closehandle(fmhandle);
  inherited destroy;
end;

constructor TD3DHook.create(size: integer; hookhwnd: boolean=true);
var h: thandle;
    s: TStringList;
begin
  consoleOverlayid:=-1;

  sharename:='CED3D_'+inttostr(processhandler.ProcessID);

  fprocessid:=processhandler.processid;
  maxsize:=size;

  createSharedMemory(sharename, sizeof(TD3DHookShared)+maxsize);

  fmhandle:=OpenFileMapping(FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE, false, pchar(sharename));
  if fmhandle=0 then
    raise exception.create('D3DHook: Failure to open the shared memory object');

  shared:=MapViewOfFile(fmhandle,FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE, 0,0,0 );

  if shared=nil then
    raise exception.create('D3DHook: Failure to map the shared memory object');

  ZeroMemory(shared, sizeof(TD3DHookShared));


  shared.cheatenginedir:=CheatEngineDir;
  shared.MouseOverlayId:=-1;

  if hookhwnd then
    shared.hookwnd:=1;

  h:=CreateEventA(nil, true, false, pchar(sharename+'_READY') );

  if (h<>0) then
  begin

    if hookhwnd then
    begin
      hasclickevent:=CreateEventA(nil, false, false, pchar(sharename+'_HASCLICK') );
      hashandledclickevent:=CreateEventA(nil, false, true, pchar(sharename+'_HANDLEDCLICK') );

      haskeyboardevent:=CreateEventA(nil, false, false, pchar(sharename+'_HASKEYBOARD') );
      hashandledkeyboardevent:=CreateEventA(nil, false, true, pchar(sharename+'_HANDLEDKEYBOARD') );


      messagehandler:=TD3DMessageHandler.Create(true);
      messagehandler.owner:=self;
      messagehandler.start;
    end;


    //now inject the dll
    injectdll(cheatenginedir+'d3dhook.dll');

    //wait till the injection is done
    WaitForSingleObject(h, INFINITE);
    closehandle(h);

    //and hook the functions
    //(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');
    s:=tstringlist.create;
    try
      if shared.d3d9_present<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_present,8), inttohex(shared.d3d9_newpresent,8),  inttohex(shared.d3d9_originalpresent,8), '0');

      if shared.d3d9_reset<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_reset,8), inttohex(shared.d3d9_newreset,8),  inttohex(shared.d3d9_originalreset,8), '1');

      if shared.dxgi_present<>0 then
        generateAPIHookScript(s, inttohex(shared.dxgi_present,8), inttohex(shared.dxgi_newpresent,8),  inttohex(shared.dxgi_originalpresent,8), '2');


      if shared.d3d9_drawprimitive<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawprimitive,8), inttohex(shared.d3d9_newdrawprimitive,8),  inttohex(shared.d3d9_originaldrawprimitive,8), '3');

      if shared.d3d9_drawindexedprimitive<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawindexedprimitive,8), inttohex(shared.d3d9_newdrawindexedprimitive,8),  inttohex(shared.d3d9_originaldrawindexedprimitive,8), '4');

      if shared.d3d9_drawprimitiveup<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawprimitiveup,8), inttohex(shared.d3d9_newdrawprimitiveup,8),  inttohex(shared.d3d9_originaldrawprimitiveup,8), '5');

      if shared.d3d9_drawindexedprimitiveup<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawindexedprimitiveup,8), inttohex(shared.d3d9_newdrawindexedprimitiveup,8),  inttohex(shared.d3d9_originaldrawindexedprimitiveup,8), '6');

      if shared.d3d9_drawrectpatch<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawrectpatch,8), inttohex(shared.d3d9_newdrawrectpatch,8),  inttohex(shared.d3d9_originaldrawrectpatch,8), '7');

      if shared.d3d9_drawtripatch<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_drawtripatch,8), inttohex(shared.d3d9_newdrawtripatch,8),  inttohex(shared.d3d9_originaldrawtripatch,8), '8');


      if shared.d3d10_drawindexed<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d10_drawindexed,8), inttohex(shared.d3d10_newdrawindexed,8),  inttohex(shared.d3d10_originaldrawindexed,8), '9');

      if shared.d3d10_draw<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d10_draw,8), inttohex(shared.d3d10_newdraw,8),  inttohex(shared.d3d10_originaldraw,8), '10');

      if shared.d3d10_drawindexedinstanced<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d10_drawindexedinstanced,8), inttohex(shared.d3d10_newdrawindexedinstanced,8),  inttohex(shared.d3d10_originaldrawindexedinstanced,8), '11');

      if shared.d3d10_drawinstanced<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d10_drawinstanced,8), inttohex(shared.d3d10_newdrawinstanced,8),  inttohex(shared.d3d10_originaldrawinstanced,8), '12');

      if shared.d3d10_drawauto<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d10_drawauto,8), inttohex(shared.d3d10_newdrawauto,8),  inttohex(shared.d3d10_originaldrawauto,8), '13');


      if shared.d3d11_drawindexed<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d11_drawindexed,8), inttohex(shared.d3d11_newdrawindexed,8),  inttohex(shared.d3d11_originaldrawindexed,8), '14');

      if shared.d3d11_draw<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d11_draw,8), inttohex(shared.d3d11_newdraw,8),  inttohex(shared.d3d11_originaldraw,8), '15');

      if shared.d3d11_drawindexedinstanced<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d11_drawindexedinstanced,8), inttohex(shared.d3d11_newdrawindexedinstanced,8),  inttohex(shared.d3d11_originaldrawindexedinstanced,8), '16');

      if shared.d3d11_drawinstanced<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d11_drawinstanced,8), inttohex(shared.d3d11_newdrawinstanced,8),  inttohex(shared.d3d11_originaldrawinstanced,8), '17');

      if shared.d3d11_drawauto<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d11_drawauto,8), inttohex(shared.d3d11_newdrawauto,8),  inttohex(shared.d3d11_originaldrawauto,8), '18');

      clipboard.AsText:=s.text;

      //if there is a script execute it.
      if (s.count>0) and (autoassemble(s,false)=false) then
      begin
        //on error write the script to the clipboard
        clipboard.AsText:=s.text; //debug
      end;
    finally
      s.free;
    end;


  end;


end;

function safed3dhook(size: integer=16*1024*1024; hookwindow: boolean=true): TD3DHook;
//Calls the d3dhook constructor but captures exceptions
var wr: DWORD;
begin
  if d3dhook=nil then
  begin
    try
      d3dhook:=TD3DHook.Create(size, hookwindow);
    except
      d3dhook:=nil;
    end;
  end
  else
  begin
    if d3dhook.processid<>processid then //different process
    begin
      d3dhook.Free;
      try
        d3dhook:=TD3DHook.Create(size, hookwindow);
      except
        d3dhook:=nil;
      end;
    end
    else
    begin
      //same process
      wr:=WaitForSingleObject(processhandle, 0);
      if wr<>WAIT_TIMEOUT then //not alive anymore
        freeandnil(d3dhook);

    end;

  end;

  result:=d3dhook;
end;

end.

