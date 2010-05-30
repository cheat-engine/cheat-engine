unit KeyListener;

interface

uses classes,windows,D3DX81mo,graphics,sysutils,syncobjs;

type TAPIInfo = record
  location: Pointer;
  Original: Array [0..4] of byte;
  Jump:     Array [0..4] of byte;
end;

procedure InitializeKeyListener;


type TKeyCombo=array [0..4] of word;
type TKeys=record
  configured: boolean;
  CEDir: string[255];
  cewindow: thandle; 

  callibrationmode: boolean;  //false=no textureselect hud
  callibrationkey: TKeycombo;

  setcallibration: boolean;
  mousecallibrationhorizontal1point: single;
  mousecallibrationvertical1point: single;

  mousecallibrationhorizontal2point: single;
  mousecallibrationvertical2point: single;

  mousecallibrationhorizontal5point: single;
  mousecallibrationvertical5point: single;

  mousecallibrationhorizontal10point: single;
  mousecallibrationvertical10point: single;

  mousecallibrationhorizontal20point: single;
  mousecallibrationvertical20point: single;

  mousecallibrationhorizontal40point: single;
  mousecallibrationvertical40point: single;

  loadaimsettingsfile: tkeycombo;
  saveaimsettingsfile: tkeycombo;
  aimsettings1: string[255];
  Aimsettings2: string[255];
  Aimsettings3: string[255];

  setaimsetting1: tkeycombo;
  setaimsetting2: tkeycombo;
  setaimsetting3: tkeycombo;

  nexttexture: tkeycombo;
  previoustexture: tkeycombo;
  locktexture: tkeycombo;

  IncreaseX: tkeycombo;
  DecreaseX: TKeyCombo;
  Increasey: tkeycombo;
  Decreasey: TKeyCombo;
  Increasez: tkeycombo;
  Decreasez: TKeyCombo;

  HoldAutoaimtoggle: boolean;
  autoshoot: boolean;
  autoaimtoggle: tKeycombo;
  increaselag: tkeycombo;
  decreaselag: tkeycombo;

  zoomin,zoomout: TKeyCombo;
  nozoom: tKeyCombo;
  zoom1: tKeyCombo;
  zoomlevel1: single;
  zoom2: tkeycombo;
  zoomlevel2: single;
  zoom3: tkeycombo;
  zoomlevel3: single;
  zoom4: tkeycombo;
  zoomlevel4: single;
  zoom5: tkeycombo;
  zoomlevel5: single;

  zoomdelta: single;
  lagdelta: integer;

  setlag: boolean;
  lagtoset: dword;
  usefpslag: boolean;

  rotateleft: tKeycombo;
  rotateright: tkeycombo;
  rotateup: tkeycombo;
  rotatedown: tkeycombo;
  moveleft: tkeycombo;
  moveright: tkeycombo;
  moveup: tkeycombo;
  movedown: tkeycombo;
  moveforward: tkeycombo;
  movebackwards: tkeycombo;

  movespeed: single;
  rotatespeed: single;

  setcameraback: tkeycombo;

  zbuffer: tkeycombo;
  fog: tkeycombo;
  lighting: tkeycombo;
  wireframe: tkeycombo;

  ShowKeylist: tkeycombo;

  SaveAlltextures: TKeycombo;

  selectedlagrecord: string[50];
  lagmemorytype: byte;
  getlagfrommemory: boolean;
  nrofoffsets: dword;
  lagaddress: dword;
  offset1: dword;
  offset2: dword;
  offset3: dword;
  offset4: dword;
  offset5: dword;
  offset6: dword;
  offset7: dword;
  offset8: dword;
  offset9: dword;
  offset10: dword;
  offset11: dword;
  offset12: dword;
  offset13: dword;
  offset14: dword;
  offset15: dword;


  pollinginterval: integer;
end;
type PKeys= ^TKeys;


type Tdirectxversion= (Directx8,Directx9);
type tcepointer=record
  address:dword;
  offset:dword;
end;

type TKeyListener=class(TThread)
  public
    procedure execute; override;
end;
var KeyListenerThread: TKeyListener;
    keys: PKeys;
    KeysFileMapping:THandle;

//some public vars
var tickspersecond: int64;
    TicksPerMS:double;
    onetick: double; //holds the time in miliseconds that a tick takes
    lasttick: int64;

    lag: integer; //should be in keys
    lagfrommemory: dword;
    fpslag: double;
    lagtimer: dword;

    usefpslag: boolean;
    lagoffsets: array of tcepointer;


    bbb: dword;
    xdelta,ydelta: single;
    xdelta2,ydelta2: single;

    autoaim: boolean;
    autoaimtimer: dword;

    aimsettings: string;
    aimsettingsset: boolean;
    aimsettingstimer: dword;
    selectedaimconfig: integer;  //1,2 or 3

    showloading: boolean;
    loadedtimer: dword;
    showsaving: boolean;
    savedtimer: dword;

    callibrationmode: boolean;

    //mouse callibration part
    mousecallibrationactive: boolean;
    mousecallibrationmode: integer;

    mousecallibrationpreviouspos: td3dxvector2;
    mousecallibrationhorizontal1point: single;
    mousecallibrationvertical1point: single;

    mousecallibrationhorizontal2point: single;
    mousecallibrationvertical2point: single;

    mousecallibrationhorizontal5point: single;
    mousecallibrationvertical5point: single;

    mousecallibrationhorizontal10point: single;
    mousecallibrationvertical10point: single;

    mousecallibrationhorizontal20point: single;
    mousecallibrationvertical20point: single;

    mousecallibrationhorizontal40point: single;
    mousecallibrationvertical40point: single;


    mousespeedx: array [0..40] of single;
    mousespeedy: array [0..40] of single;

    cefonthandle: tFont;
    zoom: single;
    fog,zbuffer,lighting,wireframe: dword;

    LoadPhase: integer; //1=loading the locked textures 2=comparing
    currentposition,maxposition: integer;

    texturepointer: integer;
    locking: boolean;
    imreleasing: boolean;

    autoshoot: boolean;
    shot: boolean;
    mousedowntime: dword;
    clicktime: dword;
    intervalbetweenshots: dword;
    lastshot: dword;

    directxversion: TDirectxversion;
    imdrawing:boolean;

    showkeylist: boolean;
    keylist: tstringlist;
    requiredkeylistwidth,requiredkeylistheight:integer;

    texturelistCS: TCriticalsection;
    LockedtexturelistCS: TCriticalsection;

function ConvertKeyComboToString(x: tkeycombo):string;
function CheckKeyCombo(keycombo: tkeycombo):boolean;
procedure getlag;

implementation

uses directxhook,directx9hook;

function ConvertKeyComboToString(x: tkeycombo):string;
var i: integer;
    newstr: string;
begin
  result:='';
  for i:=0 to 4 do
    if x[i]=0 then
      break
    else
    begin
      newstr:='';
      case x[i] of
        VK_BACK	: newstr:='Backspace';
        VK_SHIFT: newstr:='Shift';
        VK_CONTROL: newstr:='Ctrl';
        VK_MENU: newstr:='Alt';
        VK_TAB	: newstr:='Tab';
        VK_CLEAR	: newstr:='Clear';
        VK_RETURN	: newstr:='Enter';
        VK_PAUSE	: newstr:='Pause';
        VK_CAPITAL	: newstr:='Caps Lock';
        VK_ESCAPE	: newstr:='Esc';
        VK_SPACE	: newstr:='Space bar';
        VK_PRIOR	: newstr:='Page Up';
        VK_NEXT	: newstr:='Page Down';
        VK_END	: newstr:='End';
        VK_HOME	: newstr:='Home';
        VK_LEFT	: newstr:='Left Arrow';
        VK_UP	: newstr:='Up Arrow';
        VK_RIGHT	: newstr:='Right Arrow';
        VK_DOWN	: newstr:='Down Arrow';
        VK_SELECT	: newstr:='Select';
        VK_PRINT	: newstr:='Print';
        VK_EXECUTE	: newstr:='Execute';
        VK_SNAPSHOT	: newstr:='Print Screen';
        VK_INSERT	: newstr:='Insert';
        VK_DELETE	: newstr:='Delete';
        VK_HELP	: newstr:='Help';
        VK_LWIN	: newstr:='Left Windows key';
        VK_RWIN	: newstr:='Right Windows key';
        VK_APPS	: newstr:='Applications key';
        VK_NUMPAD0	: newstr:='numeric 0';
        VK_NUMPAD1	: newstr:='numeric 1';
        VK_NUMPAD2	: newstr:='numeric 2';
        VK_NUMPAD3	: newstr:='numeric 3';
        VK_NUMPAD4	: newstr:='numeric 4';
        VK_NUMPAD5	: newstr:='numeric 5';
        VK_NUMPAD6	: newstr:='numeric 6';
        VK_NUMPAD7	: newstr:='numeric 7';
        VK_NUMPAD8	: newstr:='numeric 8';
        VK_NUMPAD9	: newstr:='numeric 9';
        VK_MULTIPLY	: newstr:='numeric *';
        VK_ADD	: newstr:='numeric +';
        VK_SEPARATOR : newstr:='numeric Separator';
        VK_SUBTRACT	: newstr:='numeric -';
        VK_DECIMAL	: newstr:='numeric .';
        VK_DIVIDE	: newstr:='numeric /';
        VK_F1	: newstr:='F1';
        VK_F2	: newstr:='F2';
        VK_F3	: newstr:='F3';
        VK_F4	: newstr:='F4';
        VK_F5	: newstr:='F5';
        VK_F6	: newstr:='F6';
        VK_F7	: newstr:='F7';
        VK_F8	: newstr:='F8';
        VK_F9	: newstr:='F9';
        VK_F10	: newstr:='F10';
        VK_F11	: newstr:='F11';
        VK_F12	: newstr:='F12';
        VK_F13	: newstr:='F13';
        VK_F14	: newstr:='F14';
        VK_F15	: newstr:='F15';
        VK_F16	: newstr:='F16';
        VK_F17	: newstr:='F17';
        VK_F18	: newstr:='F18';
        VK_F19	: newstr:='F19';
        VK_F20	: newstr:='F20';
        VK_F21	: newstr:='F21';
        VK_F22	: newstr:='F22';
        VK_F23	: newstr:='F23';
        VK_F24	: newstr:='F24';
        VK_NUMLOCK	: newstr:='Num Lock';
        VK_SCROLL	: newstr:='Scroll Lock';
        48..57      : newstr:=chr(x[i]);
        65..90      : newstr:=chr(x[i]);
        else  newstr:='#'+inttostr(x[i]);
      end;

      result:=result+newstr+'+';
    end;

  result:=copy(result,1,length(result)-1);
end;

function CheckKeyCombo(keycombo: tkeycombo):boolean;
var i: integer;
begin
  result:=false;
  if keycombo[0]=0 then exit;

  if keycombo[0]<>0 then
    begin
      result:=true;
      for i:=0 to 4 do
        if (keycombo[i]=0) then break
        else
        if (getasynckeystate(keycombo[i])=0) then  result:=false;
    end;
end;

procedure TKeyListener.execute;
var i,j: integer;
    ok,found: boolean;
    tempsingle: single;
begin

  KeysFileMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tkeys),'CEKEYS');
  keys:=MapViewOfFile(OpenFileMapping(FILE_MAP_ALL_ACCESS,false,'CEKEYS'),FILE_MAP_ALL_ACCESS,0,0,0);

  priority:=tpHigher; //higher, but the sleep will make it so the game doesn''t suffer too much
  outputdebugstring('keylistener started');

  while not terminated do
  begin
    try
      try
        if not keys.configured then sleep(10);
      except
//        messagebox(0,'keys is invalid','keys is invalid',mb_ok);

      end;

      if not keys.configured then
      begin
        sleep(500);
        continue;
      end;

    if not aimsettingsset then
    begin
      aimsettings:=keys.aimsettings1;
      aimsettingsset:=true;
    end;


    if checkkeycombo(keys.ShowKeylist) then
    begin
      if not showkeylist then
      begin
        keylist.Clear;
        if keys.callibrationkey[0]<>0 then keylist.Add('Callibrate mouse:'+Convertkeycombotostring(keys.callibrationkey));
        if keys.loadaimsettingsfile[0]<>0 then keylist.Add('Load aimsettings:'+Convertkeycombotostring(keys.loadaimsettingsfile));
        if keys.saveaimsettingsfile[0]<>0 then keylist.Add('Save aimsettings:'+Convertkeycombotostring(keys.saveaimsettingsfile));
        if keys.setaimsetting1[0]<>0 then keylist.Add('Set aimsettings 1:'+Convertkeycombotostring(keys.setaimsetting1));
        if keys.setaimsetting2[0]<>0 then keylist.Add('Set aimsettings 2:'+Convertkeycombotostring(keys.setaimsetting2));
        if keys.setaimsetting3[0]<>0 then keylist.Add('Set aimsettings 3:'+Convertkeycombotostring(keys.setaimsetting3));
        if keys.nexttexture[0]<>0 then keylist.Add('Next texture:'+Convertkeycombotostring(keys.nexttexture));
        if keys.previoustexture[0]<>0 then keylist.Add('Previous texture:'+Convertkeycombotostring(keys.previoustexture));
        if keys.locktexture[0]<>0 then keylist.Add('Lock texture:'+Convertkeycombotostring(keys.locktexture));
        if keys.IncreaseX[0]<>0 then keylist.Add('Increase X:'+Convertkeycombotostring(keys.IncreaseX));
        if keys.DecreaseX[0]<>0 then keylist.Add('Decrease X:'+Convertkeycombotostring(keys.DecreaseX));
        if keys.Increasey[0]<>0 then keylist.Add('Increase Y:'+Convertkeycombotostring(keys.Increasey));
        if keys.decreasey[0]<>0 then keylist.Add('Decrease Y:'+Convertkeycombotostring(keys.decreasey));
        if keys.increasez[0]<>0 then keylist.Add('Increase Z:'+Convertkeycombotostring(keys.increasez));
        if keys.decreasez[0]<>0 then keylist.Add('Decrease Z:'+Convertkeycombotostring(keys.decreasez));
        if keys.autoaimtoggle[0]<>0 then keylist.Add('Autoaim:'+Convertkeycombotostring(keys.autoaimtoggle));
        if keys.increaselag[0]<>0 then keylist.Add('Increase Lag:'+Convertkeycombotostring(keys.increaselag));
        if keys.decreaselag[0]<>0 then keylist.Add('Decrease Lag:'+Convertkeycombotostring(keys.decreaselag));
        if keys.zoomin[0]<>0 then keylist.Add('Zoom in:'+Convertkeycombotostring(keys.zoomin));
        if keys.zoomout[0]<>0 then keylist.Add('Zoom out:'+Convertkeycombotostring(keys.zoomout));
        if keys.nozoom[0]<>0 then keylist.Add('No zoom:'+Convertkeycombotostring(keys.nozoom));
        if keys.zoom1[0]<>0 then keylist.Add('Zoom 1:'+Convertkeycombotostring(keys.zoom1));
        if keys.zoom2[0]<>0 then keylist.Add('Zoom 2:'+Convertkeycombotostring(keys.zoom2));
        if keys.zoom3[0]<>0 then keylist.Add('Zoom 3:'+Convertkeycombotostring(keys.zoom3));
        if keys.zoom4[0]<>0 then keylist.Add('Zoom 4:'+Convertkeycombotostring(keys.zoom4));
        if keys.zoom5[0]<>0 then keylist.Add('Zoom 5:'+Convertkeycombotostring(keys.zoom5));
        if keys.zbuffer[0]<>0 then keylist.Add('Z-buffer:'+Convertkeycombotostring(keys.zbuffer));
        if keys.fog[0]<>0 then keylist.Add('Fog:'+Convertkeycombotostring(keys.fog));
        if keys.lighting[0]<>0 then keylist.Add('Lighting:'+Convertkeycombotostring(keys.lighting));
        if keys.wireframe[0]<>0 then keylist.Add('Wireframe:'+Convertkeycombotostring(keys.wireframe));
        if keys.ShowKeylist[0]<>0 then keylist.Add('Show hotkey list:'+Convertkeycombotostring(keys.ShowKeylist));

        requiredkeylistheight:=keylist.count*16;
        requiredkeylistwidth:=190;

      end;

      showkeylist:=not showkeylist;
    end;

    //check if a key combo is pressed
    if CheckKeyCombo(keys.zoom1) then zoom:=keys.zoomlevel1;
    if CheckKeyCombo(keys.zoom2) then zoom:=keys.zoomlevel2;
    if CheckKeyCombo(keys.zoom3) then zoom:=keys.zoomlevel3;
    if CheckKeyCombo(keys.zoom4) then zoom:=keys.zoomlevel4;
    if CheckKeyCombo(keys.zoom5) then zoom:=keys.zoomlevel5;

    if checkkeycombo(keys.zoomin) then zoom:=zoom+keys.zoomdelta;
    if checkkeycombo(keys.zoomout) then zoom:=zoom-keys.zoomdelta;
    if checkkeycombo(keys.nozoom) then zoom:=1;

    if checkkeycombo(keys.fog) then fog:=(fog + 1) mod 2;
    if checkkeycombo(keys.zbuffer) then zbuffer:=(zbuffer + 1) mod 2;
    if checkkeycombo(keys.lighting) then lighting:=(lighting + 1) mod 2;
    if checkkeycombo(keys.wireframe) then wireframe:=(wireframe +1) mod 2;

    if checkkeycombo(keys.autoaimtoggle) then
    begin
      if keys.holdautoaimtoggle then autoaim:=true else autoaim:=not autoaim;
      autoaimtimer:=gettickcount;
    end else
    begin
      if keys.HoldAutoaimtoggle then autoaim:=false;
      autoaimtimer:=0;
      if shot then
      begin
        shot:=false;
        lastshot:=0;
        mouse_event(MOUSEEVENTF_LEFTUP,0,0,0,0); //stop fire (key got released)
      end;

    end;

    if checkkeycombo(keys.increaselag) then
    begin
      inc(lag,keys.lagdelta);
      lagtimer:=gettickcount;
    end;

    if checkkeycombo(keys.decreaselag) then
    begin
      dec(lag,keys.lagdelta);
      lagtimer:=gettickcount;
    end;

    if keys.setlag then
    begin
      usefpslag:=keys.usefpslag;
      lag:=keys.lagtoset;
      keys.setlag:=false;

      //set the offsets
      if keys.nrofoffsets>0 then
      begin
        setlength(lagoffsets,keys.nrofoffsets);
        if keys.nrofoffsets>=1 then lagoffsets[0].offset:=keys.offset1;
        if keys.nrofoffsets>=2 then lagoffsets[1].offset:=keys.offset2;
        if keys.nrofoffsets>=3 then lagoffsets[2].offset:=keys.offset3;
        if keys.nrofoffsets>=4 then lagoffsets[3].offset:=keys.offset4;
        if keys.nrofoffsets>=5 then lagoffsets[4].offset:=keys.offset5;
        if keys.nrofoffsets>=6 then lagoffsets[5].offset:=keys.offset6;
        if keys.nrofoffsets>=7 then lagoffsets[6].offset:=keys.offset7;
        if keys.nrofoffsets>=8 then lagoffsets[7].offset:=keys.offset8;
        if keys.nrofoffsets>=9 then lagoffsets[8].offset:=keys.offset9;
        if keys.nrofoffsets>=10 then lagoffsets[9].offset:=keys.offset10;
        if keys.nrofoffsets>=11 then lagoffsets[10].offset:=keys.offset11;
        if keys.nrofoffsets>=12 then lagoffsets[11].offset:=keys.offset12;
        if keys.nrofoffsets>=13 then lagoffsets[12].offset:=keys.offset13;
        if keys.nrofoffsets>=14 then lagoffsets[13].offset:=keys.offset14;
        if keys.nrofoffsets=15 then lagoffsets[14].offset:=keys.offset15;
      end;
    end;

    try
      if directxversion=directx8 then
        directxhook.HandleKeypresses
      else
        directx9hook.handlekeypresses;
    except
      locking:=false;
    end;


    if checkkeycombo(keys.setaimsetting1) then
    begin
      aimsettings:=keys.aimsettings1;
      aimsettingstimer:=gettickcount;
      aimsettingsset:=true;
    end;

    if checkkeycombo(keys.setaimsetting2) then
    begin
      aimsettings:=keys.aimsettings2;
      aimsettingstimer:=gettickcount;
      aimsettingsset:=true;
    end;

    if checkkeycombo(keys.setaimsetting3) then
    begin
      aimsettings:=keys.aimsettings3;
      aimsettingstimer:=gettickcount;
      aimsettingsset:=true;
    end;


    if checkkeycombo(keys.SaveAlltextures) then
    begin
      if directxversion=directx8 then
        SaveAllTextures8
      else
        savealltextures9;

    end;


    if checkkeycombo(keys.Loadaimsettingsfile) then
    begin
      showloading:=true;
      if directxversion=directx8 then
        LoadLockedTextureInfo8(aimsettings)
      else
        LoadLockedTextureInfo9(aimsettings);

      showloading:=false;
      loadedtimer:=gettickcount;
    end;


    if (keys.callibrationmode) and checkkeycombo(keys.saveaimsettingsfile) then
    begin
      showsaving:=true;
      if directxversion=directx8 then
        saveLockedTextureInfo8(aimsettings)
      else
        savelockedtextureinfo9(aimsettings);

      showsaving:=false;
      savedtimer:=gettickcount;
    end;

    callibrationmode:=keys.callibrationmode;

    if checkkeycombo(keys.callibrationkey) then
    begin
      if not mousecallibrationactive then
      begin
        mousecallibrationmode:=1;
        mousecallibrationactive:=true;
      end;
    end;

    if keys.setcallibration then
    begin
      keys.setcallibration:=false;


      mousespeedx[1]:=keys.mousecallibrationhorizontal1point;
      mousespeedx[2]:=keys.mousecallibrationhorizontal2point;
      mousespeedx[5]:=keys.mousecallibrationhorizontal5point;
      mousespeedx[10]:=keys.mousecallibrationhorizontal10point;
      mousespeedx[20]:=keys.mousecallibrationhorizontal20point;
      mousespeedx[40]:=keys.mousecallibrationhorizontal40point;
      mousespeedx[3]:=mousespeedx[1]+mousespeedx[2];
      mousespeedx[4]:=mousespeedx[3]+((mousespeedx[5]-mousespeedx[2]) / 3);

      tempsingle:=(mousespeedx[10]-mousespeedx[5])/5;
      for i:=6 to 9 do
        mousespeedx[i]:=mousespeedx[5]+(i-5)*tempsingle;

      tempsingle:=(mousespeedx[20]-mousespeedx[10])/10;
      for i:=11 to 19 do
        mousespeedx[i]:=mousespeedx[10]+(i-10)*tempsingle;

      tempsingle:=(mousespeedx[40]-mousespeedx[20])/20;
      for i:=21 to 39 do
        mousespeedx[i]:=mousespeedx[20]+(i-20)*tempsingle;

      //y
      mousespeedy[1]:=keys.mousecallibrationvertical1point;
      mousespeedy[2]:=keys.mousecallibrationvertical2point;
      mousespeedy[5]:=keys.mousecallibrationvertical5point;
      mousespeedy[10]:=keys.mousecallibrationvertical10point;
      mousespeedy[20]:=keys.mousecallibrationvertical20point;
      mousespeedy[40]:=keys.mousecallibrationvertical40point;
      mousespeedy[3]:=mousespeedy[1]+mousespeedy[2];
      mousespeedy[4]:=mousespeedy[3]+((mousespeedy[5]-mousespeedy[2]) / 3);

      tempsingle:=(mousespeedy[10]-mousespeedy[5])/5;
      for i:=6 to 9 do
        mousespeedy[i]:=mousespeedy[5]+(i-5)*tempsingle;

      tempsingle:=(mousespeedy[20]-mousespeedy[10])/10;
      for i:=11 to 19 do
        mousespeedy[i]:=mousespeedy[10]+(i-10)*tempsingle;

      tempsingle:=(mousespeedy[40]-mousespeedy[20])/20;
      for i:=21 to 39 do
        mousespeedy[i]:=mousespeedy[20]+(i-20)*tempsingle;


      autoshoot:=keys.autoshoot;
    end;

    if keys.getlagfrommemory then getlag;

    sleep(keys.pollinginterval);

    except

//      outputdebugstring('the keylistener had an error');

    end;


  end;

//  messagebox(0,'error','error',mb_ok);
end;

procedure getlag;
var realaddress,realaddress2: dword;
    j: integer;
    count: dword;
    s: string;
begin
//  outputdebugstring('getlag called');
  try
    if keys.nrofoffsets>0 then
    begin
      //it's a pointer
      realaddress2:=keys.lagaddress;

      s:='start address='+IntToHex(realaddress2,8);
     // outputdebugstring(pchar(s));

      for j:=keys.nrofoffsets-1 downto 0 do
      begin
        count:=0;
        realaddress:=pdword(realaddress2)^;
        realaddress2:=realaddress+lagoffsets[j].offset;

        s:='RealAddress='+IntToHex(realaddress,8);
      //  outputdebugstring(pchar(s));

        s:='Offset='+IntToHex(lagoffsets[j].offset,8);
      //  outputdebugstring(pchar(s));

        s:='RealAddress2='+IntToHex(realaddress2,8);
       // outputdebugstring(pchar(s));
      end;

      realaddress:=realaddress2;
      s:='final RealAddress='+IntToHex(realaddress,8);
     // outputdebugstring(pchar(s));

    end else realaddress:=keys.lagaddress;

    case keys.lagmemorytype of
      0:  lagfrommemory:=pbyte(realaddress)^;
      1:  lagfrommemory:=pword(realaddress)^;
      2:  lagfrommemory:=pdword(realaddress)^;
      3:  lagfrommemory:=trunc(psingle(realaddress)^);
      4:  lagfrommemory:=trunc(pdouble(realaddress)^);
      6:  lagfrommemory:=pint64(realaddress)^;
    end;

  except
   // s:='Error';
   //outputdebugstring(pchar(s));

    lagfrommemory:=0;
  end;


end;


procedure InitializeKeyListener;
begin
//open the filemapping object that holds the keys to watch
  aimsettings:='';
  aimsettingsset:=false;
  Keylistenerthread:=TKeylistener.Create(false);
  TextureListCS:=TCriticalSection.create;
  LockedTexturelistCS:=TCriticalSection.create;
end;


end.

