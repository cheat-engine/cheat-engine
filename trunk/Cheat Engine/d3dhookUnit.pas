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
  newkernelhandler, controls, Clipbrd, strutils, LuaHandler;

type
  TCEMessage=packed record
    uMsg: DWORD;
    wParam: UINT64;
    lParam: UINT64;
    character: DWORD;
  end;

  TTextureEntry=packed record
    AddressOfTexture: UINT64;
    size: integer;
    colorKey: dword;
    hasBeenUpdated: integer;
    reserved: integer;
  end;

  TTextureEntryArray=Array [0..1000] of TTextureEntry;
  PTextureEntryArray=^TTextureEntryArray;

  type TRenderCommandEnum=(rcEndOfCommandlist=0, //Stop going through the list
  			   rcIgnored=1,  //Ignore (Being updated)
  			   rcDrawSprite=2,		//Render the sprite at the given position
  			   rcDrawFont=3	);		//Render some text at the given coordinates. The string is located at  "addressoftext"

  TSpriteCommand=packed record
    x: single;
    y: single;
    alphablend: single;
    width: integer;
    height: integer;
    isMouse: integer;
    textureid: integer;
  end;

  TFontCommand=packed record
    x: single;
    y: single;
    alphablend: single;
    color: DWORD;
    addressoftext: UINT64;
  end;

  TRenderCommand=packed record
    command: integer;
    case TRenderCommandEnum of
      rcDrawSprite: (Sprite: TSpriteCommand);
      rcDrawFont: (Font: TFontCommand);
  end;
  PRenderCommand=^TRenderCommand;

  TRenderCommandarray=array [0..100] of TRenderCommand;
  PRenderCommandArray=^TRenderCommandArray;


  TD3DHookShared=packed record
    cheatenginedir: array [0..255] of char;
    dxgi_present: UINT64;
    dxgi_resizebuffers: UINT64;
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
    dxgi_newresizebuffers: UINT64;
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
    dxgi_originalresizebuffers: UINT64;
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
      cursorid: integer;
      lastmessage: TCEMessage;

    end;

    lastHwnd: DWORD;
    texturelistHasUpdate: integer; //If 1 this means that CE might be waiting for the HasHandledTextureUpdate event (if it is a high priority update)
    textureCount: integer;
    texturelist: UINT64 ; //offset into texturelist based on the start of the shared object (once setup, this does not change)
    TextureLock: UINT64 ; //target process handle
    commandListLock: UINT64;
    useCommandListLock: integer;


    //followed by the rendercommands


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


  TD3DHook_Texture=class(TObject)
  private
    id: integer;
    owner: TD3DHook;
    resource: ptruint; //address where the current resource is located
    fheight: integer;
    fwidth: integer;
  public
    function getID: integer;
   {

    procedure LoadTextureFromFile(filename: string);

    constructor Create(filename: string); overload;  }
    procedure LoadTextureByPicture(picture: TPicture);
    constructor Create(owner: TD3DHook; picture: TPicture);
    constructor Create(owner: TD3DHook);
    destructor destroy; override;

    property height: integer read fheight;
    property width: integer read fwidth;
  end;

  TD3DHook_RenderObject=class(TObject)
  protected
    owner: TD3DHook;
    updatecount: integer;
    procedure setZOrder(newpos: integer);
  public
    function getIndex: integer;
    procedure beginUpdate;
    procedure endUpdate;
    procedure UpdateRenderCommand; virtual; abstract;

    destructor destroy; override;
    constructor create(owner: TD3DHook);

  published
    property zOrder: integer read getIndex write setZOrder;
  end;

  TD3DHook_Sprite=class(TD3DHook_RenderObject)
  private
    ftexture: TD3DHook_Texture;
    fx: single;
    fy: single;
    falphablend: single;
    fwidth: integer;
    fheight: integer;
    fIsMouse: boolean;

    fvisible: boolean;
    procedure setVisible(state: boolean);
    procedure setIsMouse(state: boolean);
    procedure setTexture(s: TD3DHook_Texture);
    procedure setX(v: single);
    procedure setY(v: single);
    procedure setWidth(v: integer);
    procedure setHeight(v: integer);
    procedure setAlphaBlend(v: single);
  public

    procedure UpdateRenderCommand; override;

    constructor create(owner: TD3DHook; texture: TD3DHook_Texture);
  published
    property alphaBlend: single read falphablend write setAlphablend;
    property x: single read fx write setX;
    property y: single read fy write setY;
    property width: integer read fWidth write setWidth;
    property height: integer read fHeight write setHeight;
    property visible: boolean read fVisible write setVisible;
    property isMouse: boolean read fIsMouse write setIsMouse;
    property texture: TD3DHook_Texture read ftexture write setTexture;
  end;


  TD3DHook=class(TObject)
  private
    sharename: string;
    shared: PD3DHookShared; //local address of the D3DHookShared structure

    tea: PTextureEntryArray;


    fmhandle: THandle;

    textures: TList;
    commandlist: TList; //collection of pointer to objects used for lookup and id management.  1-on-1 relation to the renderCommands list
    renderCommandList: PRenderCommandArray;

    isupdating: integer; //update counter for the texture list
    isupdatingCL: integer; //update counter for the command list

    maxsize: integer;
    fprocessid: dword;

    hasclickevent: THandle;
    hashandledclickevent: THandle;

    haskeyboardevent: THandle;        //todo: combine into one "HasMessage" event, but for now, to make sure I don't break anything, this method...
    hashandledkeyboardevent: THandle;

    texturelock: THandle;
    CommandListLock: THandle;

    messagehandler: TD3DMessageHandler;

    consoleCursorId: integer;
    consoleOverlayid: integer;
    consoleImage: TPicture;
    consoleCursorImage: TPicture;


    procedure UpdateResourceData;
    procedure UpdateConsoleOverlay(command: string; log: Tstrings);
    procedure UpdateConsolecursorPos(command: string; pos: integer);
  public
    onclick: TD3DClickEvent;
    procedure beginTextureUpdate;
    procedure endTextureUpdate;

    procedure beginCommandListUpdate;
    procedure endCommandListUpdate;

    function createTexture(p: TPicture): TD3DHook_Texture;
    function createSprite(texture: TD3DHook_Texture): TD3DHook_Sprite;


    procedure SetOverlayAlphaBlend(overlayid: integer; blend: single);
    procedure SetOverlayVisibility(overlayid: integer; state: boolean);
    procedure updateOverlayImage(overlayid: integer; skipsync: boolean=false);
    procedure updateOverlayPosition(overlayid,x,y: integer);
    procedure setDisabledZBuffer(state: boolean);
    procedure setWireframeMode(state: boolean);
    procedure setMouseClip(state: boolean);

    function getWidth: integer;
    function getHeight: integer;

    procedure setCommandListLockFeature(state: boolean);


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

    c,c2: string;
    s: pchar;
    old: tstrings;
begin
  //check the size of the window. If it's changed, reinitialize the consoleoverlay
  owner.ReinitializeConsoleIfNeeded;

  //handle the key
  virtualkey:=owner.shared.console.lastmessage.wParam;
  scancode:=(owner.shared.console.lastmessage.lparam shr 16) and $FF;


  //handle keys like delete, backspace, etc...
  case virtualkey of
    VK_RETURN:
    begin
      //execute the command
      consolelog.add(consolecommand);

      LuaCS.enter;
      old:=lua_oldprintoutput;
      lua_setPrintOutput(consolelog);
      lua_dostring(LuaVM, pchar(consolecommand));
      lua_setPrintOutput(old);
      luacs.Leave;

      consolecommand:='';
      consolecursorpos:=0;
    end;
    VK_LEFT:
    begin
      if consolecursorpos>0 then
        dec(consolecursorpos);
    end;

    VK_RIGHT:
    begin
      if consolecursorpos<length(consolecommand) then
        inc(consolecursorpos);
    end;
    VK_BACK:
    begin
      //delete the character before the cursorpos
      if consolecursorpos>0 then
      begin
        c:=copy(consolecommand, 1, consolecursorpos-1);
        c2:=copy(consolecommand, consolecursorpos+1, length(consolecommand));
        consolecommand:=c+c2;
        dec(consolecursorpos);
      end;
    end;

    VK_DELETE:
    begin
      //delete the character after the current cursor
      c:=copy(consolecommand, 1, consolecursorpos);
      c2:=copy(consolecommand, consolecursorpos+2, length(consolecommand));
      consolecommand:=c+c2;
    end
    else
    begin
      //not a control key, check if it's a character
      if owner.shared.console.lastmessage.character<>0 then
      begin
        //it's a character
        s:=@owner.shared.console.lastmessage.character;

        c:=copy(consolecommand, 1, consolecursorpos);
        c2:=copy(consolecommand, consolecursorpos+1, length(consolecommand));


        consolecommand:=c+s+c2;

        inc(consolecursorpos);
      end;
    end;
  end;


  owner.updateConsoleOverlay(consolecommand,consolelog);
  owner.updateConsoleCursorPos(consolecommand, consolecursorpos);


end;

procedure TD3DMessageHandler.doclick;
begin
  if assigned(owner.onclick) then
    owner.onclick(overlayid+1, x,y);
end;

procedure TD3DMessageHandler.execute;
var eventlist: array of THandle;
    r: dword;

    cursor: boolean;
    cursorstart: dword;
begin
  cursorstart:=gettickcount;
  consolelog:=tstringlist.create;
  consolecursorpos:=0;

  setlength(eventlist,2);
  eventlist[0]:=owner.hasclickevent;
  eventlist[1]:=owner.haskeyboardevent;

  while (not terminated) do
  begin
    r:=WaitForMultipleObjects(2, @eventlist[0], false, 100);
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
        SetEvent(owner.hashandledkeyboardevent);

        Synchronize(dokeyboard);
        cursorstart:=GetTickCount;
      end;
    end;

    if owner.consoleCursorId<>-1 then //toggle the cursor visible or invisible based on the current time and if a key was pressed (keep the cursor visible rigth after pressing a key, so reset the timerstart)
    begin
      cursor:=true; //(owner.shared.console.consolevisible=1) and (((GetTickCount-cursorstart) mod 1000)<500);
      owner.SetOverlayVisibility(owner.consoleCursorId, cursor);
    end;

  end;

end;

//----------------------------D3DHook_RenderObject------------------------------

procedure TD3DHook_RenderObject.setZOrder(newpos: integer);
var
  mypos: integer;
  replaced: TD3DHook_RenderObject;
begin
  //change the order of the commandlist
  mypos:=zorder;
  if (newpos<>mypos) and (newpos<owner.commandlist.count) then
  begin
    replaced:=TD3DHook_RenderObject(owner.commandlist[newpos]);

    owner.beginCommandListUpdate;

    owner.renderCommandList^[newpos].command:=integer(rcIgnored); //in case the locking option is not used. Will cause some flickering in the worst case
    owner.renderCommandList^[mypos].command:=integer(rcIgnored);

    owner.commandlist[newpos]:=self;
    owner.commandlist[mypos]:=replaced;

    //now update mine and the replaced one's rendercommand entry
    UpdateRenderCommand;
    if replaced<>nil then
      replaced.UpdateRenderCommand;

    owner.endCommandListUpdate;
  end;
end;

function TD3DHook_RenderObject.getIndex: integer;
begin
  result:=owner.commandlist.IndexOf(self);
end;

procedure TD3DHook_RenderObject.beginUpdate;
begin
  inc(updatecount);
end;

procedure TD3DHook_RenderObject.endUpdate;
begin
  if updatecount>0 then
    dec(updatecount);

  if updatecount=0 then
    UpdateRenderCommand;
end;

destructor TD3DHook_RenderObject.destroy;
var index: integer;
begin
  index:=getindex;
  owner.renderCommandList^[Index].command:=integer(rcIgnored);
  owner.commandlist[index]:=nil;
end;

constructor TD3DHook_RenderObject.create(owner: TD3DHook);
begin
  self.owner:=owner;
  owner.commandlist.Add(self);
end;

//-------------------------------D3DHook_Sprite---------------------------------
procedure TD3DHook_Sprite.setWidth(v: integer);
begin
  beginUpdate;
  fwidth:=v;
  endUpdate;
end;

procedure TD3DHook_Sprite.setHeight(v: integer);
begin
  beginUpdate;
  fheight:=v;
  endUpdate;
end;

procedure TD3DHook_Sprite.setAlphaBlend(v: single);
begin
  BeginUpdate;
  falphablend:=v;
  endUpdate;
end;

procedure TD3DHook_Sprite.setX(v: single);
begin
  beginUpdate;
  fx:=v;
  endUpdate;
end;

procedure TD3DHook_Sprite.setY(v: single);
begin
  beginUpdate;
  fy:=v;
  endUpdate;
end;

procedure TD3DHook_Sprite.setIsMouse(state: boolean);
begin
  beginUpdate;
  fIsMouse:=state;
  endUpdate;
end;

procedure TD3DHook_Sprite.setVisible(state: boolean);
begin
  beginUpdate;
  fvisible:=state;
  endUpdate;
end;


procedure TD3DHook_Sprite.setTexture(s: TD3DHook_Texture);
begin
  BeginUpdate;
  ftexture:=s;
  width:=s.width;
  height:=s.height;
  EndUpdate;
end;

procedure TD3DHook_Sprite.UpdateRenderCommand;
var index: integer;
begin
  index:=getindex;

  owner.beginCommandListUpdate;

  if (index<>-1) and (texture<>nil) then
  begin
    if owner.renderCommandList^[index].command<>integer(rcDrawSprite) then //something completly new
      owner.renderCommandList^[index].command:=integer(rcIgnored); //in case the lock is not used

    owner.renderCommandList^[index].Sprite.x:=x;
    owner.renderCommandList^[index].Sprite.y:=y;
    owner.renderCommandList^[index].Sprite.alphablend:=alphablend;
    owner.renderCommandList^[index].Sprite.width:=width;
    owner.renderCommandList^[index].Sprite.height:=height;
    owner.renderCommandList^[index].Sprite.ismouse:=integer(ismouse);
    owner.renderCommandList^[index].Sprite.textureid:=texture.getID;
    owner.renderCommandList^[index].command:=integer(rcDrawSprite);
  end
  else
    owner.renderCommandList^[index].command:=integer(rcIgnored);



  owner.endCommandListUpdate;
end;

constructor TD3DHook_Sprite.create(owner: TD3DHook; texture: TD3DHook_Texture);
begin
  inherited create(owner);
  beginupdate;
  alphablend:=1;

  setTexture(texture);
  endUpdate;
end;

//-------------------------------d3dhook_texture--------------------------------
procedure TD3DHook_Texture.LoadTextureByPicture(picture: TPicture);
var m: tmemorystream;
    newblock: pointer;
    x: dword;
    msp: pointer;
    s: integer;
begin
  m:=TMemoryStream.create;
  picture.PNG.SaveToStream(m);

  owner.beginTextureUpdate;

  newblock:=VirtualAllocEx(processhandle, nil, m.Size, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
  if newblock<>nil then
  begin
    msp:=m.Memory;
    s:=m.size;
    if WriteProcessMemory(processhandle, newblock, m.memory, m.size, x) then
    begin
      owner.tea[id].AddressOfTexture:=qword(newblock);
      owner.tea[id].Size:=m.size;
      owner.tea[id].hasBeenUpdated:=1;
      owner.tea[id].colorKey:=$ffffffff; //white and alpha

      //free old block if possible
      if (resource<>0) then
        VirtualFreeEx(processhandle, pointer(resource),0,MEM_RELEASE);

      fheight:=picture.Height;
      fwidth:=picture.Width;
    end;

    resource:=qword(newblock);
  end;

  m.free;

  owner.endTextureUpdate;
end;

function TD3DHook_Texture.getID: integer;
begin
  result:=id;
end;

destructor TD3DHook_Texture.destroy;
begin
  owner.beginTextureUpdate;

  owner.tea[id].AddressOfTexture:=0;
  owner.tea[id].hasBeenUpdated:=1; //marks for deletion

  VirtualFreeEx(processhandle, pointer(resource),0,MEM_RELEASE);

  owner.textures[id]:=nil;
  owner.endTextureUpdate;
  inherited destroy;
end;

constructor TD3DHook_Texture.Create(owner: TD3DHook);
var i: integer;
begin
  self.owner:=owner;
  id:=-1;

  //find a empty slot, and if not found, add it instead
  for i:=0 to owner.textures.count-1 do
    if owner.textures[i]=nil then
    begin
      id:=i;
      owner.textures[id]:=self;
      break;
    end;

  if id=-1 then
    id:=owner.textures.add(self);
end;

constructor TD3DHook_Texture.Create(owner: TD3DHook; picture: TPicture);
var m: TMemoryStream;
begin
  create(owner);

  LoadTextureByPicture(picture);
end;



//----------------------------------D3dhook-------------------------------------
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
  c.FillRect(0,0,(c.Width), (c.Height)-(lineheight+2));

  c.Brush.color:=$000000;
  c.FillRect(0,(c.height)-(lineheight+1),(c.width), (c.height));

  c.pen.color:=clred;
  c.Line(0,(c.Height)-(lineheight+2), (c.width-1), (c.Height)-(lineheight+2));

  //todo: In the future implement font rendering inside the dxhook and render on top of the overlay

  //now render the text
  //command
  c.TextOut(4,(c.Height)-(lineheight+1), command);

  //and the log (from bottom to top, till the max is reached)
  c.Brush.Color:=$111111;

  if log<>nil then
  begin
    linepos:=(c.Height)-(lineheight+2)-lineheight; //the last line

    for i:=log.Count-1 downto 0 do
    begin
      c.textout(4, linepos, log[i]);
      linepos:=linepos-lineheight;
      if linepos<0 then break; //max reached
    end;
  end;


  UpdateResourceData;
{  shared.resources[consoleOverlayid-1].updatedresource:=1;
  shared.OverLayHasUpdate:=1;         }

end;

procedure TD3DHook.UpdateConsolecursorPos(command: string; pos: integer);
begin
 { shared.resources[consoleCursorId-1].x:=4+consoleImage.bitmap.Canvas.TextWidth(copy(command, 1,pos));
  shared.resources[consoleCursorId-1].updatedpos:=1;
  shared.OverLayHasUpdate:=1;     }
end;

procedure TD3DHook.reinitializeConsoleIfNeeded;
var c: TCanvas;
begin
  if getHeight=0 then exit;
  if getWidth=0 then exit;

  {
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

  if consoleCursorId=-1 then
  begin
    consoleCursorImage:=TPicture.create;
    consoleCursorImage.Bitmap.Height:=consoleimage.Bitmap.canvas.GetTextHeight('F');
    consoleCursorImage.Bitmap.Width:=3;
    c:=consoleCursorImage.Bitmap.canvas;
    c.Brush.Color:=$fefefe;
    c.FillRect(0,0,consoleCursorImage.Bitmap.width,consoleCursorImage.Bitmap.Height);
    consoleCursorId:=createOverlayFromPicture(consoleCursorImage,4, getheight-consoleCursorImage.height);
    SetOverlayVisibility(consoleCursorId, false);
  end;

  if (consoleImage.Width<>getWidth) or (consoleImage.Height<>getHeight div 3) then
  begin
    consoleImage.bitmap.Width:=getWidth;
    consoleimage.bitmap.Height:=getheight div 3;
    shared.resources[consoleOverlayid-1].y:=getheight-(getheight div 3);
    shared.resources[consoleCursorId-1].y:=getheight-consoleCursorImage.height;
  end;

   }
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
  shared.console.cursorid:=consolecursorid-1;
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
  if (shared<>nil) and (GetClientRect(shared.lastHwnd, x)) then
    result:=x.Right-x.left
  else
    result:=0;
end;

function TD3DHook.getHeight: integer;
var x: trect;
begin
  if (shared<>nil) and (GetClientRect(shared.lastHwnd, x)) then
    result:=x.bottom-x.top
  else
    result:=0;

end;

procedure TD3DHook.setCommandListLockFeature(state: boolean);
begin
  shared.useCommandListLock:=integer(state);
end;

procedure TD3DHook.beginCommandListUpdate;
begin
  if isupdatingCL=0 then //start of an edit
    WaitForSingleObject(CommandListLock, INFINITE);  //obtain lock

  inc(isupdatingCL);
end;

procedure TD3DHook.endCommandListUpdate;
begin
  renderCommandList^[commandlist.count].command:=integer(rcEndOfCommandlist);

  if isupdatingCL>0 then
  begin
    dec(isupdatingCL);
    if isupdatingCL=0 then
      SetEvent(CommandListLock); //release the lock
  end;


end;

procedure TD3DHook.beginTextureUpdate;
begin
  if isupdating=0 then //start of an edit
    WaitForSingleObject(TextureLock, INFINITE);  //obtain lock

  inc(isupdating);
end;

procedure TD3DHook.endTextureUpdate;
begin
  if isupdating>0 then
  begin
    dec(isupdating);
    if isupdating=0 then
    begin
      shared.textureCount:=textures.count;
      shared.texturelistHasUpdate:=1;

      SetEvent(TextureLock); //release the lock
    end;
  end;
end;

procedure TD3DHook.updateOverlayImage(overlayid: integer; skipsync: boolean=false);
begin
  if skipsync then
    inc(isupdating);  //prevents the locking

  beginTextureUpdate;
 { shared.resources[overlayid-1].updatedresource:=1;  }
  endTextureUpdate;

  if skipsync then
    endTextureUpdate;
end;

procedure TD3DHook.updateOverlayPosition(overlayid,x,y: integer);
begin
  beginTextureUpdate;
 { shared.resources[overlayid-1].x:=x;
  shared.resources[overlayid-1].y:=y;
  shared.resources[overlayid-1].updatedpos:=1; }
  endTextureUpdate;
end;

procedure TD3DHook.SetOverlayAlphaBlend(overlayid: integer; blend: single);
begin
 { shared.resources[overlayid-1].alphaBlend:=blend / 100.0;    }
end;

procedure TD3DHook.SetOverlayVisibility(overlayid: integer; state: boolean);
begin
{
  if state then
    shared.resources[overlayid-1].valid:=1
  else
    shared.resources[overlayid-1].valid:=0;

  if isupdating=0 then
    shared.OverLayHasUpdate:=1;}
end;

procedure TD3DHook.UpdateResourceData;
//Fill in the resources and the offsets pointing to them
var
  i,j: integer;
  s: TMemoryStream;
  start: PByteArray;
begin
  //now update all the entries
 { s:=tmemorystream.Create;
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
        start:=pointer(PtrUint(start)+s.size);
      end;
    end;

  finally
    s.free;
  end;     }
end;


//++++++++++++++++++++++++++++++++++++++++++++++++++++++version 2
function TD3DHook.createTexture(p: TPicture): TD3DHook_Texture;
begin
  beginTextureUpdate;
  result:=TD3DHook_Texture.Create(self, p);
  endTextureUpdate;
end;

function TD3DHook.createSprite(texture: TD3DHook_Texture): TD3DHook_Sprite;
begin
  beginCommandListUpdate;
  result:=TD3DHook_Sprite.create(self, texture);
  endCommandListUpdate;
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
  textures:=TList.create;
  commandlist:=TList.create;

  consoleOverlayid:=-1;
  consoleCursorId:=-1;

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

  shared.texturelist:=sizeof(TD3DHookShared)+(maxsize div 2);
  shared.cheatenginedir:=CheatEngineDir;

  tea:=PTextureEntryArray(ptruint(shared)+shared.texturelist);
  renderCommandList:=PRenderCommandArray(ptruint(shared)+sizeof(TD3dHookShared));;


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


    TextureLock:=CreateEventA(nil, false, true, nil);
    DuplicateHandle(GetCurrentProcess, TextureLock, processhandle, @shared.TextureLock,DUPLICATE_SAME_ACCESS, false,0);

    CommandListLock:=CreateEventA(nil, false, true, nil);
    DuplicateHandle(GetCurrentProcess, CommandListLock, processhandle, @shared.CommandListLock,DUPLICATE_SAME_ACCESS, false,0);


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

      if shared.dxgi_resizebuffers<>0 then
        generateAPIHookScript(s, inttohex(shared.dxgi_resizebuffers,8), inttohex(shared.dxgi_newresizebuffers,8),  inttohex(shared.dxgi_originalresizebuffers,8), '19');


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

