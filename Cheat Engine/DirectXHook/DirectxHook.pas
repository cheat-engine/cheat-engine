unit DirectxHook;

interface
//directx8 hook

uses controls,classes,windows,messages,forms,dialogs,sysutils,directxGraphics,math,graphics,keylistener,d3dx81mo;

type CEVertex=record
  position: TD3DVector;
  RHW: single;
//  color: Dword;
  tu,tv: single;
end;
const D3DFVF_CEVertex=(D3DFVF_XYZRHW or {D3DFVF_DIFFUSE or} D3DFVF_TEX1);

type TTextureInfo = record
  //creation parameters
  Width, Height, Levels : Cardinal;
  Usage : LongWord;
  Format : TD3DFormat;
  Pool : TD3DPool;

  locked: boolean;
//  checked: boolean;
  texturehandle: IDirect3DTexture8;
  xdelta,ydelta,zdelta: single;
end;

type PIDirect3D8=^IDirect3D8;
type TDirect3DCreate8=function(SDKVersion: DWORD): PIDirect3D8; stdcall;

type TIDirect3D_CreateDevice_Original=function(const self:IDirect3D8;const Adapter : Cardinal; const DeviceType : TD3DDevType; hFocusWindow : HWND; BehaviorFlags : LongWord; var pPresentationParameters : TD3DPresent_Parameters; out ppReturnedDeviceInterface : IDirect3DDevice8) : HResult; stdcall;


type TIDirect3DDevice8_Reset_Original=function(const self:IDirect3DDevice8; var pPresentationParameters : TD3DPresent_Parameters) : HResult; stdcall;
type TIDirect3DDevice8_CreateTexture_Original=function(const self:IDirect3DDevice8;const Width, Height, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppTexture : IDirect3DTexture8) : HResult; stdcall;
type TIDirect3DDevice8_CreateVertexBuffer_Original=function(const self:IDirect3DDevice8;const Length : Cardinal; const Usage, FVF : LongWord; const Pool : TD3DPool; out ppVertexBuffer : IDirect3DVertexBuffer8) : HResult; stdcall;
type TIDirect3DDevice8_UpdateTexture_Original=function(const self: IDirect3DDevice8; const pSourceTexture, pDestinationTexture : IDirect3DBaseTexture8) : HResult; stdcall;
type TIDirect3DDevice8_BeginScene_Original=function(const self:IDirect3DDevice8): HResult; stdcall;
type TIDirect3DDevice8_EndScene_Original=function(const self:IDirect3DDevice8): HResult; stdcall;
type TIDirect3DDevice8_SetTransform_Original=function(const self: IDirect3DDevice8; const State : TD3DTransformStateType; const pMatrix : TD3DMatrix) : HResult; stdcall;
type TIDirect3DDevice8_GetTransform_Original=function(const self:IDirect3DDevice8;const State : TD3DTransformStateType; out pMatrix : TD3DMatrix) : HResult; stdcall;
type TIDirect3DDevice8_SetRenderState_Original=function(const self:IDirect3DDevice8;const State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
type TIDirect3DDevice8_SetTexture_Original=function(const self:IDirect3DDevice8;const Stage : LongWord; const pTexture : IDirect3DBaseTexture8) : HResult; stdcall;
type TIDirect3DDevice8_DrawPrimitive_Original=function(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
type TIDirect3DDevice8_DrawIndexedPrimitive_Original=function(const self: IDirect3DDevice8;const _Type : TD3DPrimitiveType; const minIndex, NumVertices, startIndex, primCount : Cardinal) : HResult; stdcall;
type TIDirect3DDevice8_DrawPrimitiveUP_Original=function(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
type TIDirect3DDevice8_DrawIndexedPrimitiveUP_Original=function(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertexIndices, PrimitiveCount : Cardinal; pIndexData : Pointer; IndexDataFormat : TD3DFormat; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
type TIDirect3DDevice8_SetVertexShader_Original=function(const self:IDirect3DDevice8;const Handle : LongWord) : HResult; stdcall;

type TIDirect3DTexture8_AddRef_Original=function(const self:IDirect3DTexture8) : integer; stdcall;
type TIDirect3DTexture8_Release_Original=function(const self:IDirect3DTexture8) : integer; stdcall;
type TIDirect3DTexture8_GetSurfaceLevel_Original=function(const self:IDirect3DTexture8; const Level : Cardinal; out ppSurfaceLevel : IDirect3DSurface8) : HResult; stdcall;
type TIDirect3DTexture8_UnlockRect_Original=function(const self:IDirect3DTexture8; const Level : Cardinal) : HResult; stdcall;

type TIDirect3DVertexBuffer8_Lock_Original=function(const self:IDirect3DVertexBuffer8; const OffsetToLock, SizeToLock : Cardinal; var ppbData : PByte; const Flags : LongWord) : HResult; stdcall;
type TIDirect3DVertexBuffer8_Unlock_Original=function(const self:IDirect3DVertexBuffer8) : HResult; stdcall;




var Direct3DCreate8:TDirect3DCreate8;
    Direct3DCreate8Info: TAPIInfo;

var IDirect3D_CreateDevice: TAPIInfo;

var IDirect3DDevice8_Reset: TAPIInfo;
    IDirect3dDevice8_CreateTexture: TApiInfo;
    IDirect3DDevice8_CreateVertexBuffer: TAPIInfo;
    IDirect3DDevice8_UpdateTexture:TAPIInfo;
    IDirect3DDevice8_BeginScene: TAPIInfo;
    IDirect3DDevice8_EndScene: TAPIInfo;
    IDirect3DDevice8_SetTransform: TAPIInfo;
    IDirect3DDevice8_GetTransform: TAPIInfo;
    IDirect3DDevice8_SetRenderState: TAPIInfo;
    IDirect3DDevice8_SetTexture: TAPIInfo;
    IDirect3DDevice8_DrawPrimitive: TAPIInfo;
    IDirect3DDevice8_DrawIndexedPrimitive: TAPIInfo;
    IDirect3DDevice8_DrawPrimitiveUP: TAPIInfo;
    IDirect3DDevice8_DrawIndexedPrimitiveUP: TAPIInfo;
    IDirect3DDevice8_SetVertexShader: TAPIInfo;

var IDirect3DVertexBuffer8_Lock: TAPIInfo;
    IDirect3DVertexBuffer8_UnLock: TAPIInfo;

var IDirect3DTexture8_AddRef: TAPIInfo;
    IDirect3DTexture8_Release: TAPIInfo;
    IDirect3DTexture8_GetSurfaceLevel: TAPIInfo;
    IDirect3DTexture8_UnlockRect: TAPIInfo;


procedure SaveAllTextures8;
procedure SaveLockedTextureInfo8(aimconfigfile: string);
procedure LoadLockedTextureInfo8(aimconfigfile:string);
procedure handlekeypresses;


function MultiplyMatrix(const A: TD3dMatrix; const B: TD3dMatrix): TD3dMatrix;

function Direct3DCreate8Hook(SDKVersion: DWORD): PIDirect3D8; stdcall;
function IDirect3D_CreateDevice_Hook(const self:IDirect3D8;const Adapter : Cardinal; const DeviceType : TD3DDevType; hFocusWindow : HWND; BehaviorFlags : LongWord; var pPresentationParameters : TD3DPresent_Parameters; out ppReturnedDeviceInterface : IDirect3DDevice8) : HResult; stdcall;

function IDirect3DDevice8_Reset_Hook(const self:IDirect3DDevice8; var pPresentationParameters : TD3DPresent_Parameters) : HResult; stdcall;
function IDirect3DDevice8_CreateTexture_Hook(const self:IDirect3DDevice8;const Width, Height, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppTexture : IDirect3DTexture8) : HResult; stdcall;
function IDirect3DDevice8_CreateVertexBuffer_Hook(const self:IDirect3DDevice8;const Length : Cardinal; const Usage, FVF : LongWord; const Pool : TD3DPool; out ppVertexBuffer : IDirect3DVertexBuffer8) : HResult; stdcall;
function IDirect3DDevice8_UpdateTexture_hook(const self: IDirect3DDevice8; const pSourceTexture, pDestinationTexture : IDirect3DBaseTexture8) : HResult; stdcall;
function IDirect3DDevice8_BeginScene_Hook(const self:IDirect3DDevice8): HResult; stdcall;
function IDirect3DDevice8_EndScene_Hook(const self:IDirect3DDevice8): HResult; stdcall;
function IDirect3DDevice8_SetTransform_Hook(const self: IDirect3DDevice8; const State : TD3DTransformStateType; const pMatrix : TD3DMatrix) : HResult; stdcall;
function IDirect3DDevice8_GetTransform_Hook(const self:IDirect3DDevice8;const State : TD3DTransformStateType; out pMatrix : TD3DMatrix) : HResult; stdcall;
function IDirect3DDevice8_SetRenderState_Hook(const self:IDirect3DDevice8;const State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
function IDirect3DDevice8_SetTexture_Hook(const self:IDirect3DDevice8;const Stage : LongWord; const pTexture : IDirect3DBaseTexture8) : HResult; stdcall;
function IDirect3DDevice8_DrawPrimitive_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
function IDirect3DDevice8_DrawIndexedPrimitive_Hook(const self: IDirect3DDevice8;const _Type : TD3DPrimitiveType; const minIndex, NumVertices, startIndex, primCount : Cardinal) : HResult; stdcall;
function IDirect3DDevice8_DrawPrimitiveUP_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
function IDirect3DDevice8_DrawIndexedPrimitiveUP_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertexIndices, PrimitiveCount : Cardinal; pIndexData : Pointer; IndexDataFormat : TD3DFormat; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
function IDirect3DDevice8_SetVertexShader_Hook(const self:IDirect3DDevice8;const Handle : LongWord) : HResult; stdcall;

function IDirect3DTexture8_AddRef_Hook(const self:IDirect3DTexture8) : integer; stdcall;
function IDirect3DTexture8_Release_Hook(const self:IDirect3DTexture8) : integer; stdcall;
function IDirect3DTexture8_GetSurfaceLevel_Hook(const self:IDirect3DTexture8; const Level : Cardinal; out ppSurfaceLevel : IDirect3DSurface8) : HResult; stdcall;
function IDirect3DTexture8_UnlockRect_Hook(const self:IDirect3DTexture8; const Level : Cardinal) : HResult; stdcall;

function IDirect3DVertexBuffer8_Lock_Hook(const self:IDirect3DVertexBuffer8; const OffsetToLock, SizeToLock : Cardinal; var ppbData : PByte; const Flags : LongWord) : HResult; stdcall;
function IDirect3DVertexBuffer8_Unlock_Hook (const self:IDirect3DVertexBuffer8) : HResult; stdcall;


var d3d8dll: THandle;
    HookedDirect3D: Boolean;

    toggleon: boolean;

    tempf1: single;
    tempf2: single;
    tempf3: single;
    tempf4,tempf5: single;



    XRotation: single;
    YRotation: single;
    ZRotation: single;



    //ce userinterface
    ceimage: TBitmap;
    cetexture: IDirect3DBaseTexture8; //;IDirect3DTexture8;
    texturestringtexture: IDirect3DTexture8;
    Lockedstringtexture: IDirect3DTexture8;
    UnLockedstringtexture: IDirect3DTexture8;

    cevertices: array of CEVertex;

    vertexshaderset: boolean;
    dontsetlastvertexshader: boolean;
    lastvertexshader: thandle;

    lastworldmatrix: TD3dmatrix;
    dontsetlastworldmatrix: boolean;
    worldmatrixset: boolean;

    lastprojectionmatrix,lastprojectionmatrix2: TD3dmatrix;
    dontsetlastprojectionmatrix: boolean;
    projectionmatrixset: boolean;

    lastviewmatrix: TD3dmatrix;
    dontsetlastviewmatrix: boolean;
    viewmatrixset: boolean;

    lockedtexturelist: array of TTextureInfo;
    texturelist: array of TTextureInfo;


    cefont: ID3dXFont;
    mysprite: ID3DXSprite;
    myspritetexture: IDirect3DTexture8;

    targettexture: IDirect3DTexture8;
    locktexture: IDirect3DTexture8;
    movementtexture: IDirect3DTexture8;
    blackTexture: IDirect3DTexture8;

    watchfornextdraw: boolean;
    nextdrawstage: dword;
    currenttexture: integer;
    xylist: array of td3dxvector3;
    xylist2: array of td3dxvector3;
    oldpos: td3dxvector3;

    screencenter: td3dxvector2;

    resetdevice: boolean;


implementation

procedure SaveAllTextures8;
var i: integer;
begin
  texturelistCS.Enter;
  try
    for i:=0 to length(texturelist)-1 do
      D3DXSaveTextureToFile(pchar('CETEX'+IntToStr(i)+'.bmp'),D3DXIFF_BMP,texturelist[i].texturehandle,nil);
  finally
    texturelistcs.Leave;
  end;
end;


procedure Removetexture(j: integer);
var i: integer;
begin
  if texturepointer>j then dec(texturepointer);

  texturelistcs.Enter;
  try
    try
      for i:=j to length(texturelist)-2 do
      begin
        texturelist[i].Width:=texturelist[i+1].Width;
        texturelist[i].Height:=texturelist[i+1].Height;
        texturelist[i].Levels:=texturelist[i+1].Levels;
        texturelist[i].Usage:=texturelist[i+1].Usage;
        texturelist[i].Format:=texturelist[i+1].Format;
        texturelist[i].Pool:=texturelist[i+1].Pool;
        texturelist[i].locked:=texturelist[i+1].locked;
        texturelist[i].xdelta:=texturelist[i+1].xdelta;
        texturelist[i].ydelta:=texturelist[i+1].ydelta;
        texturelist[i].zdelta:=texturelist[i+1].zdelta;
        copymemory(@texturelist[i].texturehandle,@texturelist[i+1].texturehandle,4);
      end;
    except
      outputdebugstring('error in removing texture (1)');
    end;

    try
      fillmemory(@texturelist[length(texturelist)-1].texturehandle,4,0);
    except
      outputdebugstring('error in removing texture (2)');
    end;

    try
      setlength(texturelist,length(texturelist)-1);
    except
      outputdebugstring('error in removing texture (2)');
    end;

    if texturepointer>=length(texturelist) then texturepointeR:=length(texturelist)-1;

  finally
    texturelistcs.Leave;
  end;

end;

procedure handlekeypresses;
var found: boolean;
    i,j: integer;
begin
  if keys.callibrationmode then
  begin
    j:=-1;

    if checkkeycombo(keys.previoustexture) then if (texturepointer-1)<0 then texturepointer:=length(texturelist)-1 else dec(texturepointer);
    if checkkeycombo(keys.nexttexture) then if (texturepointer+1)>=length(texturelist) then texturepointer:=0 else inc(texturepointer);
    if checkkeycombo(keys.locktexture) then  //(un)lock
    begin
      locking:=true;
      texturelistcs.Enter;

      try
        found:=false;
        for i:=0 to length(lockedtexturelist)-1 do
        begin
          if dword(lockedtexturelist[i].texturehandle)=dword(texturelist[texturepointer].texturehandle) then
          begin
            j:=i;
            found:=true;
            break;
          end;
        end;

        if found then
        begin
          imreleasing:=true;
          lockedtexturelist[j].texturehandle._Release;
          imreleasing:=false;

          fillmemory(@lockedtexturelist[j].texturehandle,4,0);

          for i:=j to length(lockedtexturelist)-2 do
          begin
            lockedtexturelist[i].Width:=lockedtexturelist[i+1].Width;
            lockedtexturelist[i].Height:=lockedtexturelist[i+1].Height;
            lockedtexturelist[i].Levels:=lockedtexturelist[i+1].Levels;
            lockedtexturelist[i].Usage:=lockedtexturelist[i+1].Usage;
            lockedtexturelist[i].Format:=lockedtexturelist[i+1].Format;
            lockedtexturelist[i].Pool:=lockedtexturelist[i+1].Pool;
            lockedtexturelist[i].locked:=lockedtexturelist[i+1].locked;
            lockedtexturelist[i].xdelta:=lockedtexturelist[i+1].xdelta;
            lockedtexturelist[i].ydelta:=lockedtexturelist[i+1].ydelta;
            lockedtexturelist[i].zdelta:=lockedtexturelist[i+1].zdelta;
            copymemory(@lockedtexturelist[i].texturehandle,@lockedtexturelist[i+1].texturehandle,4);
          end;
          setlength(lockedtexturelist,length(lockedtexturelist)-1);


          texturelist[texturepointer].locked:=false;

        end
        else
        begin
          i:=length(lockedtexturelist);
          setlength(lockedtexturelist,i+1);

          lockedtexturelist[i].Width:=texturelist[texturepointer].Width;
          lockedtexturelist[i].Height:=texturelist[texturepointer].Height;
          lockedtexturelist[i].Levels:=texturelist[texturepointer].Levels;
          lockedtexturelist[i].Usage:=texturelist[texturepointer].Usage;
          lockedtexturelist[i].Format:=texturelist[texturepointer].Format;
          lockedtexturelist[i].Pool:=texturelist[texturepointer].Pool;
          lockedtexturelist[i].locked:=texturelist[texturepointer].locked;
          lockedtexturelist[i].xdelta:=texturelist[texturepointer].xdelta;
          lockedtexturelist[i].ydelta:=texturelist[texturepointer].ydelta;
          lockedtexturelist[i].zdelta:=texturelist[texturepointer].zdelta;
          copymemory(@lockedtexturelist[i].texturehandle,@texturelist[texturepointer].texturehandle,4);

          lockedtexturelist[i].texturehandle._AddRef;

          lockedtexturelist[i].locked:=true;
          texturelist[texturepointer].locked:=true;

        end;
      finally
        locking:=false;
        texturelistcs.Leave;
      end;
    end;



    try
      texturelistcs.Enter;
      try
        if (texturepointer<>-1) and (texturelist[texturepointer].locked) then
        begin
          if checkkeycombo(keys.IncreaseX) then
          for i:=0 to length(lockedtexturelist)-1 do
            if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
            begin
              lockedtexturelist[i].xdelta:=lockedtexturelist[i].xdelta+1;
              texturelist[texturepointer].xdelta:=texturelist[texturepointer].xdelta+1;
              break;
            end;


          if checkkeycombo(keys.Increasey) then
            for i:=0 to length(lockedtexturelist)-1 do
              if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
              begin
                lockedtexturelist[i].ydelta:=lockedtexturelist[i].ydelta+1;
                texturelist[texturepointer].ydelta:=texturelist[texturepointer].ydelta+1;
                break;
              end;

          if checkkeycombo(keys.Increasez) then
            for i:=0 to length(lockedtexturelist)-1 do
              if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
              begin
                lockedtexturelist[i].zdelta:=lockedtexturelist[i].zdelta+1;
                texturelist[texturepointer].zdelta:=texturelist[texturepointer].zdelta+1;
                break;
              end;

          if checkkeycombo(keys.DecreaseX) then
            for i:=0 to length(lockedtexturelist)-1 do
              if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
              begin
                lockedtexturelist[i].xdelta:=lockedtexturelist[i].xdelta-1;
                texturelist[texturepointer].xdelta:=texturelist[texturepointer].xdelta-1;
                break;
              end;

          if checkkeycombo(keys.Decreasey) then
            for i:=0 to length(lockedtexturelist)-1 do
              if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
              begin
                lockedtexturelist[i].ydelta:=lockedtexturelist[i].ydelta-1;
                texturelist[texturepointer].ydelta:=texturelist[texturepointer].ydelta-1;
                break;
              end;

          if checkkeycombo(keys.Decreasez) then
            for i:=0 to length(lockedtexturelist)-1 do
              if lockedtexturelist[i].texturehandle=texturelist[texturepointer].texturehandle then
              begin
                lockedtexturelist[i].zdelta:=lockedtexturelist[i].zdelta-1;
                texturelist[texturepointer].zdelta:=texturelist[texturepointer].zdelta-1;
                break;
              end;

        end;
      finally
        texturelistcs.Leave;
      end;
    except
      outputdebugstring('error in handlekeypresses(dx8)');
    end;
  end;
end;

procedure SaveLockedTextureInfo8(aimconfigfile: string);
var i: integer;
    tr: TD3DLocked_Rect;
    aimsettings: tfilestream;

    s: string;

    total: dword;
    pdesc: td3dsurface_desc;
begin
  try
    aimsettings:=tfilestream.Create((keys.cedir+aimconfigfile+'.cta'),fmCreate);
    lockedtexturelistcs.Enter;
    try
      total:=length(lockedtexturelist);
      aimsettings.WriteBuffer(total,4);
      for i:=0 to length(lockedtexturelist)-1 do
      begin
        //save the levels
        aimsettings.WriteBuffer(lockedtexturelist[i],sizeof(TTextureInfo));

        try
          if lockedtexturelist[i].texturehandle.LockRect(0,tr,nil,{0}D3DLOCK_NOSYSLOCK)=0 then
          begin
            try
              lockedtexturelist[i].texturehandle.GetLevelDesc(0,pdesc);

              s:='Width='+IntToStr(lockedtexturelist[i].Width)+' Height='+IntToStr(lockedtexturelist[i].Height)+' pitch='+IntToStr(tr.Pitch)+' format='+IntToStr(lockedtexturelist[i].Format)+' size='+inttostr(pdesc.Size)+' levels='+inttostr(lockedtexturelist[i].Levels);

              total:=pdesc.Size;
              aimsettings.WriteBuffer(total,4);

              try
                aimsettings.WriteBuffer(tr.pBits^,total);
              except

              end;
            finally
              lockedtexturelist[i].texturehandle.UnlockRect(0);
            end;
          end;
        except
          //something went wrong, but try to not screw up
          total:=0;
          aimsettings.WriteBuffer(total,4);
        end;
      end;
    finally
      aimsettings.free;
      lockedtexturelistcs.Leave;
    end;


  except
    outputdebugstring('error in savelockedtextureinfo8');
  end;

end;

procedure LoadLockedTextureInfo8(aimconfigfile:string);
var aimsettings: tfilestream;
    images: array of array of byte;
    i,j: integer;
    tempimage: array of byte;

    imageloaded:boolean;
    total: dword;
    tr: TD3DLocked_Rect;
    pdesc: td3dsurface_desc;
begin
  try
    LockedtexturelistCS.Enter;
    try
      for i:=0 to length(lockedtexturelist)-1 do
      begin
        try
          lockedtexturelist[i].texturehandle._Release;
        except

        end;
        zeromemory(@lockedtexturelist[i],sizeof(TTextureInfo));
      end;
      setlength(lockedtexturelist,0);
    finally
      LockedtexturelistCS.Leave;
    end;

    texturelistCS.Enter;
    try
      for i:=0 to length(texturelist)-1 do
      begin
        try
          texturelist[i].locked:=false;
        except

        end;
      end;
    finally
      texturelistCS.Leave;
    end;
  except

  end;

  LoadPhase:=1;

  try
    try
      aimsettings:=tfilestream.Create(keys.cedir+aimconfigfile+'.cta',fmOpenRead);
      try
        aimsettings.ReadBuffer(total,4);

        lockedtexturelistcs.Enter;
        try
          setlength(lockedtexturelist,total);
          setlength(images,total);

          maxposition:=length(lockedtexturelist)-1;
          for i:=0 to length(lockedtexturelist)-1 do
          begin
            currentposition:=i;

            aimsettings.ReadBuffer(lockedtexturelist[i],sizeof(TTextureInfo));
            lockedtexturelist[i].locked:=false;
            try
              aimsettings.ReadBuffer(total,4);
              setlength(images[i],total);
              aimsettings.ReadBuffer(images[i][0],total);
            except
              exit; //failed to load.....
            end;
            sleep(1);
          end;
        finally
          lockedtexturelistcs.Leave;
        end;
      finally
        aimsettings.free;
      end;

      //check the texturelist for these textures

      loadphase:=2;
      lockedtexturelistcs.Enter;
      texturelistcs.enter;
      try
        maxposition:=length(texturelist);
        for i:=0 to length(texturelist)-1 do
        begin
          currentposition:=i;
          imageloaded:=false;

          //check the lockedtextures to see if this is a texture that has to be locked
          for j:=0 to length(lockedtexturelist)-1 do
          begin
            if texturelist[i].Width<>lockedtexturelist[j].width then continue;
            if texturelist[i].Height<>lockedtexturelist[j].Height then continue;
            if texturelist[i].Levels<>lockedtexturelist[j].Levels then continue;
            if texturelist[i].Usage<>lockedtexturelist[j].Usage then continue;
            if texturelist[i].Format<>lockedtexturelist[j].Format then continue;
            if texturelist[i].Pool<>lockedtexturelist[j].pool then continue;


            if not imageloaded then
            begin
              //copy the memory of the texture to a local var
              try
                if texturelist[i].texturehandle.LockRect(0,tr,nil,{0}D3DLOCK_NOSYSLOCK  )=0 then
                begin
                  try
                    texturelist[i].texturehandle.GetLevelDesc(0,pdesc);
                    total:=pdesc.Size;

                    if total<>length(images[j]) then continue;
                    setlength(tempimage,total);
                    copymemory(@tempimage[0],tr.pBits,total);
                    imageloaded:=true;
                  finally
                    texturelist[i].texturehandle.UnlockRect(0);
                    sleep(5);
                  end;
                end;
              except
                //doesn't exist
              end;

            end;

            if comparemem(@tempimage[0],@images[j][0],length(tempimage)) then
            begin
             // lockedtexturelist[j].zdelta:=lockedtexturelist[j].zdelta+5;

              texturelist[i].locked:=true;
              texturelist[i].xdelta:=lockedtexturelist[j].xdelta;
              texturelist[i].ydelta:=lockedtexturelist[j].ydelta;
              texturelist[i].zdelta:=lockedtexturelist[j].zdelta;

              copymemory(@lockedtexturelist[j].texturehandle,@texturelist[i].texturehandle,4);
              try
                lockedtexturelist[j].texturehandle._AddRef;
              except

              end;
              lockedtexturelist[j].locked:=true;
            end;
            sleep(1);
          end;
          sleep(1);
        end;
      finally
        lockedtexturelistcs.Leave;
        texturelistcs.Leave;
      end;
    except

    end;
  finally
    for i:=0 to length(images)-1 do
      setlength(images[i],0);
      
    setlength(images,0);
    setlength(tempimage,0);
  end;

end;


function multiplymatrix(const A: td3dmatrix;const b:td3dmatrix):td3dmatrix;
begin
  result._11:=a._11*b._11+a._12*b._21+a._13*b._31+a._14*b._41;
  result._12:=a._11*b._12+a._12*b._22+a._13*b._32+a._14*b._42;
  result._13:=a._11*b._13+a._12*b._23+a._13*b._33+a._14*b._43;
  result._14:=a._11*b._14+a._12*b._24+a._13*b._34+a._14*b._44;

  result._21:=a._21*b._11+a._22*b._21+a._23*b._31+a._24*b._41;
  result._22:=a._21*b._12+a._22*b._22+a._23*b._32+a._24*b._42;
  result._23:=a._21*b._13+a._22*b._23+a._23*b._33+a._24*b._43;
  result._24:=a._21*b._14+a._22*b._24+a._23*b._34+a._24*b._44;

  result._31:=a._31*b._11+a._32*b._21+a._33*b._31+a._34*b._41;
  result._32:=a._31*b._12+a._32*b._22+a._33*b._32+a._34*b._42;
  result._33:=a._31*b._13+a._32*b._23+a._33*b._33+a._34*b._43;
  result._34:=a._31*b._14+a._32*b._24+a._33*b._34+a._34*b._44;

  result._41:=a._41*b._11+a._42*b._21+a._43*b._31+a._44*b._41;
  result._42:=a._41*b._12+a._42*b._22+a._43*b._32+a._44*b._42;
  result._43:=a._41*b._13+a._42*b._23+a._43*b._33+a._44*b._43;
  result._44:=a._41*b._14+a._42*b._24+a._43*b._34+a._44*b._44;
end;


{
0=addref
4=release
8=queryInterface
c=RegisterSoftwareDevice(pInitializeFunction : Pointer) : HResult; stdcall;
10=GetAdapterCount : Cardinal; stdcall;
14=GetAdapterIdentifier(const Adapter : Cardinal; const Flags : LongWord; out pIdentifier : TD3DAdapter_Identifier8) : HResult; stdcall;
18=GetAdapterModeCount(Adapter : Cardinal) : Cardinal; stdcall;
1c=EnumAdapterModes(const Adapter, Mode : Cardinal; var pMode : TD3DDisplayMode) : HResult; stdcall;
20=GetAdapterDisplayMode(const Adapter : Cardinal; var pMode : TD3DDisplayMode) : HResult; stdcall;
24=CheckDeviceType(const Adapter : Cardinal; const CheckType : TD3DDevType; const DisplayFormat, BackBufferFormat : TD3DFormat; const Windowed : BOOL) : HResult; stdcall;
28=CheckDeviceFormat(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat : TD3DFormat; const Usage : LongWord; const RType : TD3DResourceType; const CheckFormat : TD3DFormat) : HResult; stdcall;
2c=CheckDeviceMultiSampleType(const Adapter : Cardinal; const DeviceType : TD3DDevType; const SurfaceFormat : TD3DFormat; const Windowed : BOOL; const MultiSampleType : TD3DMultiSample_Type) : HResult; stdcall;
30=CheckDepthStencilMatch(const Adapter : Cardinal; const DeviceType : TD3DDevType; const AdapterFormat, RenderTargetFormat, DepthStencilFormat : TD3DFormat) : HResult; stdcall;
34=GetDeviceCaps(const Adapter : Cardinal; const DeviceType : TD3DDevType; out pCaps : TD3DCaps8) : HResult; stdcall;
38=GetAdapterMonitor(const Adapter : Cardinal) : HMONITOR; stdcall;
3c=CreateDevice(const Adapter : Cardinal; const DeviceType : TD3DDevType; hFocusWindow : HWND; BehaviorFlags : LongWord; var pPresentationParameters : TD3DPresent_Parameters; out ppReturnedDeviceInterface : IDirect3DDevice8) : HResult; stdcall;
  end;
}

//function D3DXMatrixPerspectiveFovLH , hrm, perhaps later,

function Direct3DCreate8Hook(SDKVersion: DWORD): PIDirect3D8; stdcall;
var x,y: dword;
    xp: ^dword;
    xpp: ^Pdword;
    AIDirect3DVertexBuffer8: IDirect3DVertexBuffer8;
begin
  //restore with original code

  asm
    push esi
    push edi
    lea esi,Direct3DCreate8Info.original[0]
    mov edi,Direct3DCreate8Info.location
    movsd
    movsb

    pop edi
    pop esi
  end;

  outputdebugstring('Creating a direct3d8 object');

  //execute api
  result:=directxhook.Direct3DCreate8(SDKVersion);
  x:=dword(result);

  //hook createdevice
  outputdebugstring('Hooking createdevice');


  if IDirect3D_CreateDevice.location=nil then
    IDirect3D_CreateDevice.location:=pointer(pdword(pdword(x)^+$3c)^);

  virtualprotect(pointer(pdword(x)^+$3c),4,PAGE_EXECUTE_READWRITE,y);
  pdword(pdword(x)^+$3c)^:=dword(@IDirect3D_CreateDevice_Hook);
  virtualprotect(pointer(pdword(x)^+$3c),4,y,y);

  outputdebugstring('Hooking createdevice-success');
end;

function Fixhook(const self:IDirect3DDevice8):boolean; stdcall;
var x:dword;
begin
    virtualprotect(pdword(pdword(dword(Self))^),4*77,PAGE_EXECUTE_READWRITE,x);

    //hook Reset
    if IDirect3DDevice8_Reset.location=nil then
      IDirect3DDevice8_Reset.location:=pointer(pdword(pdword(dword(Self))^+4*14)^);
    pdword(pdword(dword(Self))^+4*14)^:=dword(@IDirect3DDevice8_Reset_Hook);

    //hook createtexture
    if IDirect3DDevice8_CreateTexture.location=nil then
      IDirect3DDevice8_CreateTexture.location:=pointer(pdword(pdword(dword(Self))^+4*20)^);
    pdword(pdword(dword(Self))^+4*20)^:=dword(@IDirect3DDevice8_CreateTexture_Hook);

    //hook CreateVertexBuffer
    if IDirect3DDevice8_CreateVertexBuffer.location=nil then
      IDirect3DDevice8_CreateVertexBuffer.location:=pointer(pdword(pdword(dword(Self))^+4*23)^);
    pdword(pdword(dword(Self))^+4*23)^:=dword(@IDirect3DDevice8_CreateVertexBuffer_Hook);

    //hook beginscene
    if IDirect3DDevice8_BeginScene.location=nil then
      IDirect3DDevice8_BeginScene.location:=pointer(pdword(pdword(dword(Self))^+4*34)^);
    pdword(pdword(dword(Self))^+4*34)^:=dword(@IDirect3DDevice8_BeginScene_Hook);

    //EndScene
    if IDirect3DDevice8_EndScene.location=nil then
      IDirect3DDevice8_EndScene.location:=pointer(pdword(pdword(dword(Self))^+4*35)^);
    pdword(pdword(dword(Self))^+4*35)^:=dword(@IDirect3DDevice8_EndScene_Hook);

    //SetTransform
    if IDirect3DDevice8_SetTransform.location=nil then
      IDirect3DDevice8_SetTransform.location:=pointer(pdword(pdword(dword(Self))^+4*37)^);
    pdword(pdword(dword(Self))^+4*37)^:=dword(@IDirect3DDevice8_SetTransform_Hook);
    x:=dword(pdword(dword(Self))^+4*37);

    //GetTransform
    if IDirect3DDevice8_GetTransform.location=nil then
      IDirect3DDevice8_GetTransform.location:=pointer(pdword(pdword(dword(Self))^+4*38)^);
    pdword(pdword(dword(Self))^+4*38)^:=dword(@IDirect3DDevice8_GetTransform_Hook);

    //hook SetRenderState
    if IDirect3DDevice8_SetRenderState.location=nil then
      IDirect3DDevice8_SetRenderState.location:=pointer(pdword(pdword(dword(Self))^+4*50)^);
    pdword(pdword(dword(Self))^+4*50)^:=dword(@IDirect3DDevice8_SetRenderState_Hook);

    //hook SetTexture
    if IDirect3DDevice8_SetTexture.location=nil then
      IDirect3DDevice8_SetTexture.location:=pointer(pdword(pdword(dword(Self))^+4*61)^);
    pdword(pdword(dword(Self))^+4*61)^:=dword(@IDirect3DDevice8_SetTexture_Hook);

//----------

    //hook DrawPrimitive
    if IDirect3DDevice8_DrawPrimitive.location=nil then
      IDirect3DDevice8_DrawPrimitive.location:=pointer(pdword(pdword(dword(Self))^+4*70)^);
    pdword(pdword(dword(Self))^+4*70)^:=dword(@IDirect3DDevice8_DrawPrimitive_Hook);

    //hook DrawIndexedPrimitive
    if IDirect3DDevice8_DrawIndexedPrimitive.location=nil then
      IDirect3DDevice8_DrawIndexedPrimitive.location:=pointer(pdword(pdword(dword(Self))^+4*71)^);
    pdword(pdword(dword(Self))^+4*71)^:=dword(@IDirect3DDevice8_DrawIndexedPrimitive_Hook);

    //hook DrawPrimitiveUP
    if IDirect3DDevice8_DrawPrimitiveUP.location=nil then
      IDirect3DDevice8_DrawPrimitiveUP.location:=pointer(pdword(pdword(dword(Self))^+4*72)^);
    pdword(pdword(dword(Self))^+4*72)^:=dword(@IDirect3DDevice8_DrawPrimitiveUP_Hook);

    //hook DrawIndexedPrimitiveUP
    if IDirect3DDevice8_DrawIndexedPrimitiveUP.location=nil then
      IDirect3DDevice8_DrawIndexedPrimitiveUP.location:=pointer(pdword(pdword(dword(Self))^+4*73)^);
    pdword(pdword(dword(Self))^+4*73)^:=dword(@IDirect3DDevice8_DrawIndexedPrimitiveUP_Hook);

//----------
    //hook SetVertexShader
    if IDirect3DDevice8_SetVertexShader.location=nil then
      IDirect3DDevice8_SetVertexShader.location:=pointer(pdword(pdword(dword(Self))^+4*76)^);
    pdword(pdword(dword(Self))^+4*76)^:=dword(@IDirect3DDevice8_SetVertexShader_Hook);

    virtualprotect(pointer(pdword(dword(Self))^),4*77,x,x);

end;

function IDirect3D_CreateDevice_Hook(const self: IDirect3D8; const Adapter : Cardinal; const DeviceType : TD3DDevType; hFocusWindow : HWND; BehaviorFlags : LongWord; var pPresentationParameters : TD3DPresent_Parameters; out ppReturnedDeviceInterface : IDirect3DDevice8) : HResult; stdcall;
var pMode: TD3DDisplayMode;
    x,y: dword;
    tl: TD3DLocked_Rect;
    r: Trect;
    pr: prect;
    ca,cr,cg,cb: word;
begin
  directxversion:=directx8;
  setlength(xylist,0);
  setlength(xylist2,0);

  outputdebugstring('going to create a direct3d8 device');

  self.GetAdapterDisplayMode(D3dadapter_default,pmode);

{  x:=pPresentationParameters.hDeviceWindow;
  zeromemory(@ppresentationParameters,sizeof(td3dpresent_parameters));

  pPresentationParameters.Windowed:=true;
  pPresentationParameters.SwapEffect:=d3dswapeffect_discard;
  pPresentationParameters.BackBufferFormat:=pmode.format;
  pPresentationParameters.hDeviceWindow:=x;

  if pPresentationParameters.hDeviceWindow<>0 then
  begin
    setWindowPos(pPresentationParameters.hDeviceWindow,HWND_NOTOPMOST	, 10,10,600,450, SWP_NOZORDER);
  end;

  if hFocusWindow<>0 then
  begin
    setWindowPos(pPresentationParameters.hDeviceWindow,HWND_NOTOPMOST	, 10,10,600,450,SWP_NOMOVE or SWP_NOZORDER);
  end;}


  Behaviorflags:=behaviorflags and not (D3DCREATE_PUREDEVICE);
  result:=TIDirect3D_CreateDevice_Original(IDirect3D_CreateDevice.location)(self,Adapter,Devicetype,hFocusWindow,BehaviorFlags,pPresentationParameters,ppReturnedDeviceInterface);

  try
    texturepointer:=-1;

    screencenter.x:=screen.Width / 2;
    screencenter.y:=screen.Height / 2;

   
    if result=0 then  //also hook some methods of IDirect3DDevice8
    begin

    if resetdevice then
    begin
      if mysprite<>nil then mysprite:=nil;
      D3DXCreateSprite(ppReturnedDeviceInterface,mysprite);

      cefont.OnResetDevice;

      resetdevice:=false;
      exit;
    end;

    D3DXCreateTextureFromFile(ppReturnedDeviceInterface,pchar(keys.CEDir+'lockedstring.bmp'),lockedStringTexture);
    D3DXCreateTextureFromFile(ppReturnedDeviceInterface,pchar(keys.CEDir+'unlockedstring.bmp'),unlockedStringTexture);
    D3DXCreateTextureFromFile(ppReturnedDeviceInterface,pchar(keys.CEDir+'texturestring.bmp'),textureStringTexture);
    D3DXCreateTextureFromFile(ppReturnedDeviceInterface,pchar(keys.CEDir+'Black.bmp'),blackTexture);
//    D3DXCreateTextureFromFileEx(ppReturnedDeviceInterface,pchar(keys.CEDir+'Logo3.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,myspritetexture);
    D3DXCreateTextureFromFileEx(ppReturnedDeviceInterface,pchar(keys.CEDir+'TargetTexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,targettexture);
    D3DXCreateTextureFromFileEx(ppReturnedDeviceInterface,pchar(keys.CEDir+'Locktexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,locktexture);
    D3DXCreateTextureFromFileEx(ppReturnedDeviceInterface,pchar(keys.CEDir+'Movementtexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,movementtexture);



    cefonthandle.free;
    cefonthandle:=tfont.Create;
    cefonthandle.Color:=clred;
    cefonthandle.Name:='Courier';
    D3DXCreateFont(ppReturnedDeviceInterface,cefonthandle.handle,CEFont);
    D3DXCreateSprite(ppReturnedDeviceInterface,mysprite);

    fixhook(ppReturnedDeviceInterface);

  end;

  except
    outputdebugstring('something went wrong');
  end;

end;

function IDirect3DDevice8_Reset_Hook(const self:IDirect3DDevice8; var pPresentationParameters : TD3DPresent_Parameters) : HResult; stdcall;
var i: integer;
    err: dword;
begin
  try
    mysprite.OnLostDevice;
    cefont.OnLostDevice;

    texturelistcs.Enter;
    lockedtexturelistcs.Enter;
    try
      fillmemory(@texturelist[0],length(texturelist)*sizeof(TTextureInfo),0);
      fillmemory(@lockedtexturelist[0],length(texturelist)*sizeof(TTextureInfo),0);

      setlength(texturelist,0);
      setlength(lockedtexturelist,0);
    finally
      lockedtexturelistcs.Leave;
      texturelistcs.Leave;
    end;

  except
    outputdebugstring('error in reset(8)');
  end;

  texturepointer:=-1;
  err:=TIDirect3DDevice8_Reset_Original(IDirect3DDevice8_Reset.location)(self,pPresentationParameters);

  try
    if err=0 then
    begin
      mysprite.OnResetDevice;
      cefont.OnResetDevice;
    end else
    begin
      resetdevice:=true;
    end;
  except
    //outputdebugstring('Error in my reset routine');
  end;

  result:=err;
end;

function IDirect3dDevice8_CreateTexture_Hook(const self:IDirect3DDevice8;const Width, Height, Levels : Cardinal; const Usage : LongWord; const Format : TD3DFormat; const Pool : TD3DPool; out ppTexture : IDirect3DTexture8) : HResult; stdcall;
type a=^IDirect3DTexture8;
var i: integer;
    x: dword;
begin

  result:=TIDirect3DDevice8_CreateTexture_Original(IDirect3DDevice8_createtexture.location)(self,width,height,levels,usage,format,pool,pptexture);

  texturelistcs.Enter;
  try
    if imdrawing then exit;

    if not ((usage=0) and (pool=D3DPOOL_DEFAULT)) then
    begin
      i:=length(texturelist);
      setlength(texturelist,i+1);

      copymemory(@texturelist[i].texturehandle,@pptexture,4);
      texturelist[i].Width:=width;
      texturelist[i].Height:=height;
      texturelist[i].Levels:=levels;
      texturelist[i].Usage:=usage;
      texturelist[i].Format:=format;
      texturelist[i].Pool:=pool;
      texturelist[i].locked:=false;
      //texturelist[i].checked:=false;

      if texturepointer=-1 then texturepointer:=0;
    end;

  finally
    texturelistcs.Leave;
  end;



  if result=0 then
  begin
    //hook release
    virtualprotect(pdword(pdword(dword(ppTexture))^+4*2),4,PAGE_EXECUTE_READWRITE,x);

    if IDirect3DTexture8_release.location=nil then
      IDirect3DTexture8_release.location:=pointer(pdword(pdword(dword(ppTexture))^+4*2)^);
    pdword(pdword(dword(ppTexture))^+4*2)^:=dword(@IDirect3DTexture8_release_Hook);

    virtualprotect(pdword(pdword(dword(ppTexture))^+4*2),4,x,x);
  end;
end;

function IDirect3DDevice8_CreateVertexBuffer_Hook(const self:IDirect3DDevice8;const Length : Cardinal; const Usage, FVF : LongWord; const Pool : TD3DPool; out ppVertexBuffer : IDirect3DVertexBuffer8) : HResult; stdcall;
begin
  result:=TIDirect3DDevice8_CreateVertexBuffer_Original(IDirect3DDevice8_CreateVertexBuffer.location)(self,length,usage,fvf,pool,ppVertexBuffer);
end;

function IDirect3DDevice8_UpdateTexture_hook(const self: IDirect3DDevice8; const pSourceTexture, pDestinationTexture : IDirect3DBaseTexture8) : HResult; stdcall;
begin
  result:=TIDirect3DDevice8_UpdateTexture_Original(IDirect3DDevice8_UpdateTexture.location)(self,pSourceTexture,pDestinationTexture);
end;

function IDirect3DDevice8_BeginScene_Hook(const self:IDirect3DDevice8): HResult; stdcall;
var cp: single;
    c: TD3DClipStatus8;
begin
  setlength(xylist,0);
  setlength(xylist2,0);

  result:=TIDirect3DDevice8_BeginScene_Original(IDirect3DDevice8_BeginScene.location)(self);
end;

procedure DrawHotkeylist8(const self: IDirect3DDevice8;vp:td3dviewport8);
var position,scale: td3dxvector2;
    pdesc: TD3DSurface_Desc;
    i: integer;
    r: trect;

begin
  mysprite._Begin;

  if blacktexture.GetLevelDesc(0,pdesc)=0 then
  begin
    scale.x:=requiredkeylistwidth / pdesc.Width;
    scale.y:=requiredkeylistheight / pdesc.Height;
  end
  else
  begin
    scale.x:=requiredkeylistwidth;
    scale.y:=requiredkeylistheight;
  end;

  position.x:=vp.Width-requiredkeylistwidth;
  position.y:=0;
  mysprite.Draw(blacktexture,nil,@scale,nil,0,@position,D3DCOLOR_ARGB(140,255,255,255));
  mysprite._End;

  //draw the strings in the box
  cefont._Begin;
  r.Left:=vp.Width-requiredkeylistwidth+1;
  r.Top:=0;
  r.Bottom:=r.left+16;
  r.Right:=vp.Width;

  for i:=0 to keylist.Count-1 do
  begin
    r.Top:=i*16;
    r.Bottom:=i*16+(r.left+16);
    cefont.DrawTextA(pchar(keylist[i]),length(keylist[i]),r,0,D3DCOLOR_ARGB(240,255,255,255));
  end;

  cefont._End;
end;

function IDirect3DDevice8_EndScene_Hook(const self:IDirect3DDevice8): HResult; stdcall;
var tm:TD3DMatrix;
    vp:Td3dViewport8;
    x: idirect3dvertexbuffer8;
    s: dword;
    t: idirect3dbasetexture8;

    bla,bla5: pd3dxvector2;
    bla2,bla6,position,scale: td3dxvector2;
    bla3: prect;
    bla4: trect;
    pdesc: TD3DSurface_Desc;
    i:integer;

    closest: integer;
    closestsqr1: single;
    closestsqr2: single;

    xd,yd: single;
    xd2,yd2: integer;
    newtick: int64;
    tickssincelastdraw: dword;
    mssincelastdraw: double;

    r: trect;

    ts: string;
    tempsingle: single;

   // xdelta,ydelta: single;
   start,stop: integer;
   rot,rot2,rot3: td3dxmatrix;
   v1,v2,v3: TD3dvector;
begin
  s:=gettickcount;

  try
    imdrawing:=true;
    try
      dontsetlastvertexshader:=true;
      dontsetlastworldmatrix:=true;
      dontsetlastprojectionmatrix:=true;
      dontsetlastviewmatrix:=true;

      self.GetViewport(vp);

      mysprite._Begin;

      if callibrationmode then
      begin
        //draw texture string
        position.x:=0;
        position.y:=0;
        mysprite.Draw(texturestringtexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(127,255,255,255));


        if texturepointer>=0 then
        begin
          texturelistcs.Enter;
          lockedtexturelistcs.Enter;
          try
            position.y:=16;

            if texturelist[texturepointer].texturehandle.GetLevelDesc(0,pdesc)=0 then
            begin
              scale.x:=100 / pdesc.Width;
              scale.y:=100 / pdesc.Height;
            end
            else
            begin
              scale.x:=1;
              scale.y:=1;
            end;

            mysprite.Draw(texturelist[texturepointer].texturehandle,nil,@scale,nil,0,@position,D3DCOLOR_ARGB(127,255,255,255));


            position.y:=116;
            if texturelist[texturepointer].locked then
            begin
              mysprite.Draw(lockedstringtexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(127,255,255,255));
              cefont._Begin;
              ts:=format('x=%.1f y=%.1f z=%.1f',[texturelist[texturepointer].xdelta, texturelist[texturepointer].ydelta, texturelist[texturepointer].zdelta]);
              r.left:=trunc(position.x)+10;
              r.Top:=trunc(position.y)+16;
              r.Right:=trunc(position.x)+10+800;
              r.Bottom:=trunc(position.y)+16+16;
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));
              cefont._End;
            end
            else
            begin
              mysprite.Draw(unlockedstringtexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(127,255,255,255));
            end;

          finally
            texturelistcs.Leave;
            lockedtexturelistcs.leave;
          end;

        end;
      end;

      closest:=0;
      closestsqr2:=-1;

      if length(xylist)>0 then
        closestsqr2:=(abs((vp.Width/2)-(xylist[0].x)))*(abs((vp.height/2)-(xylist[0].y)));



      for i:=0 to length(xylist2)-1 do
      begin
        position.x:=xylist2[i].x-8; //I thought it would be -4....
        position.y:=xylist2[i].y-8;
        mysprite.Draw(targettexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(255,255,255,255));

{        ts:=format('(%.3f,%.3f,%.3f)',[xylist2[i].x,xylist2[i].y,xylist2[i].z]);
                  ,
        //'Position:('+FloatToStr(xylist2[i].x)+','+FloatToStr(xylist2[i].y)+','+FloatToStr(xylist2[i].z)+')';
        cefont._Begin;
        r.left:=trunc(position.x)+10;
        r.Top:=trunc(position.y);
        r.Right:=trunc(position.x)+10+800;
        r.Bottom:=trunc(position.y)+16;
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));
        cefont._End;
 }


        closestsqr1:=(abs((vp.Width/2)-(xylist2[i].x)))*(abs((vp.height/2)-(xylist2[i].y)));

        if closestsqr2>closestsqr1 then
        begin
          closestsqr2:=closestsqr1;
          closest:=i;
        end;
      end;


      if length(xylist)>0 then
      begin
        //put in xd and yd the distance between the closest object and the screencenter
        xd:=xylist[closest].x-(vp.Width/2);
        yd:=xylist[closest].y-(vp.height/2);

        //-----------------------------------------------------
        //extrapolate the speed of the player when there is lag between pressing fire and actually shooting
        //Doesn't handle acceleration or decceleration, just if it is using a constant speed (standing still is also a constant speed...)
        inc(bbb);

        if (bbb mod 4)=0 then //frame 0:get position
        begin
          oldpos:=xylist[closest]; //store current position
          QueryPerformanceCounter(lasttick); //store current time
          bbb:=4; //I hate overflows
        end;

        if (bbb mod 4)=2 then //calculate the new speed(assuming the player is moving with a constant speed)
        begin
          //frame 2: get new position and new time and use that to calulate the speed

          //calculate new speed
          QueryPerformanceCounter(newtick);
          tickssincelastdraw:=newtick-lasttick;

          mssincelastdraw:=tickssincelastdraw*onetick;

          xdelta:=xylist[closest].x-oldpos.x;
          ydelta:=xylist[closest].y-oldpos.y;

          //delta now contains the location differences since 3 frames ago

          //delta devided by the time since the oldpos and the newpos is the position change per milisecond
          //positionchange per milisecond multiplied by the ammount of milliseconds you want to be in fron


          if usefpslag then
          begin
            fpslag:=mssincelastdraw/2; //there where 2 draws since the start of the measurement
            xdelta:=(xdelta/mssincelastdraw)*(lag+lagfrommemory+fpslag);
            ydelta:=(ydelta/mssincelastdraw)*(lag+lagfrommemory+fpslag);
          end
          else
          begin
            xdelta:=(xdelta/mssincelastdraw)*(lag+lagfrommemory);
            ydelta:=(ydelta/mssincelastdraw)*(lag+lagfrommemory);
          end;
        end;

        if zoom=1 then
        begin
          position.x:=xylist[closest].x-8+xdelta2;
          position.y:=xylist[closest].y-8+ydelta2;
          mysprite.draw(movementtexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(255,255,255,255));
        end;

        if (bbb mod 4)>=2 then  //move the mouse, after recalculating the speed or when it's doing nothing
        begin
          //modify xd and yd with the predicted location (so move the mouse to the predicted location instead of the current location)
          xd:=xd+xdelta;
          yd:=yd+ydelta;
          //--------------------------------------------------------------
          if autoaim then
          begin

            xd2:=0;
            yd2:=0;

            if abs(xd)>mousespeedx[40] then
            begin
              //calculate the size of the movement
              //(xd-mousespeedx[40]) = pixels needed to be moved (after a 40)
              //(mousespeedx[40]-mousespeedx[35] / 5)=pixels moved for every step

              xd2:=40+trunc((abs(xd)-mousespeedx[40])/((mousespeedx[40]-mousespeedx[35]) / 5));
              if xd<0 then xd2:=-xd2;
            end else
            begin
              //find the closest in the list
              i:=20;

              start:=1;
              stop:=40;

              while (i<stop) and not ((mousespeedx[i]<abs(xd)) and (mousespeedx[i+1]>xd)) do
              begin
                if mousespeedx[i]<abs(xd) then start:=i else stop:=i;
                i:=start+(stop-start) div 2;
              end;

              if (i=1) and (mousespeedx[i]<xd) then i:=0;

              if xd<0 then xd2:=-i else xd2:=i;
            end;

            //y
            if abs(yd)>mousespeedy[40] then
            begin
              //calculate the size of the movement
              //(yd-mousespeedy[40]) = pixels needed to be moved (after a 40)
              //(mousespeedy[40]-mousespeedy[35] / 5)=pixels moved for every step

              yd2:=40+trunc((abs(yd)-mousespeedy[40])/((mousespeedy[40]-mousespeedy[35]) / 5));
              if yd<0 then yd2:=-yd2;
            end else
            begin
              //find the closest in the list
              i:=20;

              start:=1;
              stop:=40;

              while (i<stop) and not ((mousespeedy[i]<abs(yd)) and (mousespeedy[i+1]>yd)) do
              begin
                if mousespeedy[i]<abs(yd) then start:=i else stop:=i;
                i:=start+(stop-start) div 2;
              end;

              if (i=1) and (mousespeedy[i]<yd) then i:=0;

              if yd<0 then yd2:=-i else yd2:=i;
            end;

            cefont._Begin;
            r.left:=trunc(vp.Width/2)-20;
            r.Top:=trunc(vp.Height/2)+100;
            r.Right:=r.left+800;
            r.Bottom:=r.top+16;
            ts:=format('xd=%.2f xd2=%d',[xd,xd2]);
            cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,255,255));
            cefont._End;


            mouse_event(MOUSEEVENTF_MOVE,xd2,yd2,0,0);

            if ((bbb mod 4)=2) and autoshoot then
            begin
              if not shot then
              begin
                //if s>=(lastshot+intervalbetweenshots) then
                begin
                  mouse_event(MOUSEEVENTF_LEFTDOWN,0,0,0,0); //fire
                  shot:=true;
                  lastshot:=s;
                end;
              end;

              {if shot and (s>=(lastshot+clicktime)) then
              begin

                shot:=false;
                mouse_event(MOUSEEVENTF_LEFTUP,0,0,0,0); //stop fire
              end;}

            end;
          end;
        end;


        position.x:=xylist2[closest].x-8;
        position.y:=xylist2[closest].y-8;

        mysprite.Draw(locktexture,nil,nil,nil,0,@position,D3DCOLOR_ARGB(255,255,255,255));

        if mousecallibrationactive then
        begin
          case mousecallibrationmode of
            //-----------------------------------------------
            //1
            //-----------------------------------------------
            1:
            begin
              // move the mouse 1 point on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,1,0,0,0);
              inc(mousecallibrationmode);


            end;

            2:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal1point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-1),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);



            end;

            3:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 1 notch in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,1,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);


            end;

            4:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical1point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-1),0,0); //move back and wait for next frame
              inc(mousecallibrationmode);


            end;

            //-----------------------------------------------
            //2
            //-----------------------------------------------
            5:
            begin
              // move the mouse 2 points on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,2,0,0,0);
              inc(mousecallibrationmode);

            end;

            6:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal2point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-2),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            7:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 2 points in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,2,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            8:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical2point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-2),0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            //------------------------------------------------------
            //5
            //------------------------------------------------------

            9:
            begin
              // move the mouse 5 points on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,5,0,0,0);
              inc(mousecallibrationmode);
            end;

            10:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal5point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-5),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            11:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 5 points in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,5,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            12:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical5point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-5),0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            //------------------------------------------------------
            //10
            //------------------------------------------------------

            13:
            begin
              // move the mouse 10 points on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,10,0,0,0);
              inc(mousecallibrationmode);
            end;

            14:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal10point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-10),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            15:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 10 points in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,10,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            16:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical10point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-10),0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            //------------------------------------------------------
            //20
            //------------------------------------------------------

            17:
            begin
              // move the mouse 20 points on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,20,0,0,0);
              inc(mousecallibrationmode);
            end;

            18:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal20point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-20),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            19:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 20 points in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,20,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            20:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical20point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-20),0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            //----------------------------------------
            //40
            //----------------------------------------
            21:
            begin
              // move the mouse 40 points on the x axis
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,40,0,0,0);
              inc(mousecallibrationmode);
            end;

            22:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationhorizontal40point:=position.x-mousecallibrationpreviouspos.x;
              mouse_event(MOUSEEVENTF_MOVE,dword(-40),0,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            23:
            begin
              //mouse should be back at starting pos so:
              //move the mouse up 40 points in the vertical direction
              mousecallibrationpreviouspos:=position;
              mouse_event(MOUSEEVENTF_MOVE	,0,40,0,0); //move back and wait for next frame
              inc(mousecallibrationmode);
            end;

            24:
            begin
              //the mouse has been moved and this is a updated scene
              //newpos-oldpos = change
              mousecallibrationvertical40point:=position.y-mousecallibrationpreviouspos.y;
              mouse_event(MOUSEEVENTF_MOVE	,0,dword(-40),0,0); //move back and wait for next frame

              mousespeedx[1]:=abs(mousecallibrationhorizontal1point);
              mousespeedx[2]:=abs(mousecallibrationhorizontal2point);
              mousespeedx[5]:=abs(mousecallibrationhorizontal5point);
              mousespeedx[10]:=abs(mousecallibrationhorizontal10point);
              mousespeedx[20]:=abs(mousecallibrationhorizontal20point);
              mousespeedx[40]:=abs(mousecallibrationhorizontal40point);
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
              mousespeedy[1]:=abs(mousecallibrationvertical1point);
              mousespeedy[2]:=abs(mousecallibrationvertical2point);
              mousespeedy[5]:=abs(mousecallibrationvertical5point);
              mousespeedy[10]:=abs(mousecallibrationvertical10point);
              mousespeedy[20]:=abs(mousecallibrationvertical20point);
              mousespeedy[40]:=abs(mousecallibrationvertical40point);
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

                mousecallibrationactive:=false;
                //tell ce the callibration results
                keys.mousecallibrationhorizontal1point:=mousespeedx[1];
                keys.mousecallibrationhorizontal2point:=mousespeedx[2];
                keys.mousecallibrationhorizontal5point:=mousespeedx[5];
                keys.mousecallibrationhorizontal10point:=mousespeedx[10];
                keys.mousecallibrationhorizontal20point:=mousespeedx[20];
                keys.mousecallibrationhorizontal40point:=mousespeedx[40];

                keys.mousecallibrationvertical1point:=mousespeedy[1];
                keys.mousecallibrationvertical2point:=mousespeedy[2];
                keys.mousecallibrationvertical5point:=mousespeedy[5];
                keys.mousecallibrationvertical10point:=mousespeedy[10];
                keys.mousecallibrationvertical20point:=mousespeedy[20];
                keys.mousecallibrationvertical40point:=mousespeedy[40];

                if keys.cewindow<>0 then
                  postmessage(keys.cewindow,wm_user+1,0,0); //the callibration values have been changed
            end;


            25:
            begin


              cefont._Begin;
              r.left:=trunc(vp.Width/2)+10;
              r.Top:=trunc(vp.Height/2)-16;
              r.Right:=r.left+800;
              r.Bottom:=r.top+16;
              ts:=format('mousespeedx[1]=%.2f',[mousespeedx[1]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              r.Top:=r.top+16;
              r.Bottom:=r.Bottom+16;
              ts:=format('mousespeedx[2]=%.2f',[mousespeedx[2]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              r.Top:=r.top+16;
              r.Bottom:=r.Bottom+16;
              ts:=format('mousespeedx[5]=%.2f',[mousespeedx[5]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              r.Top:=r.top+16;
              r.Bottom:=r.Bottom+16;
              ts:=format('mousespeedx[10]=%.2f',[mousespeedx[10]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              r.Top:=r.top+16;
              r.Bottom:=r.Bottom+16;
              ts:=format('mousespeedx[20]=%.2f',[mousespeedx[20]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              r.Top:=r.top+16;
              r.Bottom:=r.Bottom+16;
              ts:=format('mousespeedx[40]=%.2f',[mousespeedx[40]]);
              cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,0,255,0));

              cefont._End;


              mousecallibrationactive:=false;
              inc(mousecallibrationmode);
            end;



            else mousecallibrationactive:=false;


          end;
        end;
      end;

      mysprite._End;

      //messages
      if s<(autoaimtimer+2000) then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=20;
        r.Right:=400;
        r.Bottom:=70;

        if autoaim then
          cefont.DrawTextA('Autoaim enabled',15,r,0,D3DCOLOR_ARGB(255,255,255,255))
        else
          cefont.DrawTextA('Autoaim disabled',16,r,0,D3DCOLOR_ARGB(255,255,255,255));

        cefont._End;
      end;

      if s<(lagtimer+2000) then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=20;
        r.Right:=500;
        r.Bottom:=70;

        if usefpslag then
          ts:='UserLag='+IntToStr(lag)+' Computedlag='+IntToStr(lagfrommemory)+' FPSlag='+format('%.2f',[fpslag])+' Totallag='+format('%.2f',[lag+lagfrommemory+fpslag])
        else
          ts:='UserLag='+IntToStr(lag)+' Computedlag='+IntToStr(lagfrommemory)+'Totallag='+IntToStr(lag+lagfrommemory);

        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,0));

        cefont._End;
      end;


      if showloading then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=36;
        r.Right:=500;
        r.Bottom:=86;
        ts:='Loading aimsettings file '+aimsettings+' (stage:'+IntToStr(LoadPhase)+'/2 pos:'+IntToStr(currentposition)+'/'+inttostr(maxposition);
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,255));

        cefont._End;
      end
      else if s<(loadedtimer+2000) then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=36;
        r.Right:=500;
        r.Bottom:=86;
        ts:='Loaded aimsettings '+aimsettings;
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,255));

        cefont._End;
      end;

      if showsaving then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=36;
        r.Right:=500;
        r.Bottom:=86;
        ts:='Saving aimsettings file '+aimsettings;
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,255));

        cefont._End;
      end
      else if s<(savedtimer+2000) then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=36;
        r.Right:=500;
        r.Bottom:=86;
        ts:='Saved aimsettings '+aimsettings;
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,255));

        cefont._End;
      end;

      if s<(aimsettingstimer+2000) then
      begin
        cefont._Begin;
        r.left:=0;
        r.Top:=200;
        r.Right:=400;
        r.Bottom:=216;
        ts:='Current aimsettingfile='+aimsettings;
        cefont.DrawTextA(pchar(ts),length(ts),r,0,D3DCOLOR_ARGB(255,255,0,0));

        cefont._End;
      end;

      if showkeylist then DrawHotkeylist8(self,vp);


      fixhook(self);

      if vertexshaderset then self.SetVertexShader(lastvertexshader);
      if worldmatrixset then {tidirect3ddevice_SetVertexShader_original}  self.SetTransform(D3DTS_WORLD,lastworldmatrix);
      if projectionmatrixset then self.SetTransform(D3DTS_Projection,lastprojectionmatrix);
      if viewmatrixset then self.SetTransform(D3DTS_VIEW,lastviewmatrix);


    finally
      dontsetlastvertexshader:=false;
      dontsetlastworldmatrix:=false;
      dontsetlastprojectionmatrix:=false;
      dontsetlastviewmatrix:=false;
    end;

  except

  end;

  imdrawing:=false;
  result:=TIDirect3DDevice8_EndScene_Original(IDirect3DDevice8_EndScene.location)(self);
end;


function IDirect3DDevice8_SetTransform_Hook(const self: IDirect3DDevice8; const State : TD3DTransformStateType; const pMatrix : TD3DMatrix) : HResult; stdcall;
var pMatrix2: TD3DMatrix;
    pmatrix3,pmatrix4: td3dmatrix;
    i: integer;
begin
{
   D3DXMatrixPerspectiveFovLH(matrix,fovy,aspect,zn,zf);
   D3DXMatrixPerspectiveFovLH(&matProj, //Result Matrix
                              D3DX_PI/4,//Field of View, in radians. (PI/4) is typical
                              ((float)g_width / (float)g_height),     //Aspect ratio
                              1.0f,     //Near view plane
                              100.0f ); // Far view plane

w       0       0               0
0       h       0               0
0       0       zf/(zf-zn)      1
0       0       -zn*zf/(zf-zn)  0
where:
h is the view space height. It is calculated from
h = cot(fovY/2)

zf/(zf-zn)=x
}

  pmatrix2:=pmatrix;

  if (zoom<>1) and (state=D3DTS_PROJECTION) then  //zoom stuff
  begin
    if{(pmatrix2._12=0) and
       (pmatrix2._13=0) and
       (pmatrix2._14=0) and
       (pmatrix2._21=0) and
       (pmatrix2._23=0) and
       (pmatrix2._24=0) and
       (pmatrix2._31=0) and
       (pmatrix2._32=0) and  }
       (pmatrix2._34=1){ and
       (pmatrix2._41=0) and
       (pmatrix2._42=0) and
       (pmatrix2._44=0)} then
    begin


      pmatrix3:=pmatrix;


      tempf1:=arccot(pmatrix2._22)*2; //=fov
      tempf2:=pmatrix2._22/pmatrix2._11; //=aspect

      //zn (near)
      tempf3:=pmatrix2._43;
      tempf3:=tempf3 / pmatrix2._33;
      tempf3:=-tempf3; //tempf3= zn

      tempf3:=tempf3;//*0.1;  //increase tempf3 to look through nearby objects

      tempf4:=pmatrix2._33;
      tempf4:=tempf4*tempf3;
      tempf4:=-tempf4;
      tempf4:=tempf4/tempf3+1;

//      pmatrix2._33:=0.1;//500000/(500000-tempf3);
//      pmatrix2._43:=-0.1;//-tempf3*500000/(500000-tempf3);

      pmatrix2._22:=cot(tempf1/2/zoom);  //devide this with a higher value to zoom in
      pmatrix2._11:=pmatrix2._22/tempf2;
    end;
  end;

  result:=TIDirect3DDevice8_SetTransform_Original(IDirect3DDevice8_SetTransform.location)(self,state,pMatrix2);

  if state=D3DTS_VIEW then
  begin
    if not dontsetlastviewmatrix then
    begin
      lastviewmatrix:=pMatrix;
      viewmatrixset:=true;
    end;
  end;

  if state=D3DTS_WORLD then
  begin
    if not dontsetlastworldmatrix then
    begin
      lastworldmatrix:=pMatrix;
      worldmatrixset:=true;
    end;
  end;

  if state=D3dts_projection then
  begin
    lastprojectionmatrix2:=pMatrix2;
    if not dontsetlastprojectionmatrix then
    begin
      lastprojectionmatrix:=pMatrix;
      projectionmatrixset:=true;
    end;
  end;
end;

function IDirect3DDevice8_GetTransform_Hook(const self:IDirect3DDevice8;const State : TD3DTransformStateType; out pMatrix : TD3DMatrix) : HResult; stdcall;
var getit: boolean;
begin
  result:=D3D_OK;

  if state=D3DTS_VIEW then
  begin
    if not dontsetlastviewmatrix then
    begin
      pMatrix:=lastviewmatrix;
      exit;
    end;
  end;

  if state=D3DTS_WORLD then
  begin
    if not dontsetlastworldmatrix then
    begin
      pMatrix:=lastworldmatrix;
      exit;
    end;
  end;

  if state=D3dts_projection then
  begin
    if not dontsetlastprojectionmatrix then
    begin
      pMatrix:=lastprojectionmatrix;
      exit;
    end;
  end;

  //still here so it wasn't set (So this should fail, but try anyhow...)
  result:=TIDirect3DDevice8_GetTransform_Original(IDirect3DDevice8_GetTransform.location)(self,state,pMatrix);
end;


function IDirect3DDevice8_SetRenderState_Hook(const self:IDirect3DDevice8;const State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
var x: single;
    y: dword absolute x;
begin
  result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,State,Value);

  if fog<>2 then result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,D3DRS_FOGENABLE,fog);
  if lighting<>2 then result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,D3DRS_lighting,lighting);
  if zbuffer<>2 then result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,D3DRS_ZENABLE,zbuffer);
  if wireframe=1 then result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,D3DRS_fillmode,D3DFILL_WIREFRAME);
  if wireframe=0 then result:=TIDirect3DDevice8_SetRenderState_Original(IDirect3DDevice8_SetRenderState.location)(self,D3DRS_Fillmode,D3DFILL_SOLID);
end;

function IDirect3DDevice8_SetTexture_Hook(const self:IDirect3DDevice8;const Stage : LongWord; const pTexture : IDirect3DBaseTexture8) : HResult; stdcall;
var i,j:integer;
    stop: boolean;
    px: td3dxvector3;
    pv: td3dviewport8;
    v: tD3DXVector3;

    p: PD3DXVector3;
    pp: TD3DXVector3;

begin
  try
  if (not imdrawing) and (not watchfornextdraw) then
  for i:=0 to length(lockedtexturelist)-1 do
  begin
    if (dword(pTexture)=dword(lockedtexturelist[i].texturehandle)) then
    begin
      if (lockedtexturelist[i].locked) then  //always.....
      begin
        currenttexture:=i;
        watchfornextdraw:=true;
        nextdrawstage:=stage;


  {  try
    j:=length(xylist);
    setlength(xylist,j+1);
    setlength(xylist2,j+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[j], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    D3DXVec3Project(xylist2[j], V  ,pv, lastProjectionmatrix2, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except

    end;}

      end;
      break;
    end;
  end;
  except

  end;

  result:=TIDirect3DDevice8_SetTexture_Original(IDirect3DDevice8_SetTexture.location)(self,stage,pTexture);
end;

function IDirect3DDevice8_DrawPrimitive_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
var i: integer;
    px: td3dxvector3;
    pv: td3dviewport8;
    v: tD3DXVector3;

    p: PD3DXVector3;
    pp: TD3DXVector3;
    t:string;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);
    setlength(xylist2,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    D3DXVec3Project(xylist2[i], V  ,pv, lastProjectionmatrix2, lastViewmatrix, lastWorldmatrix);

   // outputdebugstring(pchar(t));

    self.SetTexture(nextdrawstage,nil);
    result:=TIDirect3DDevice8_DrawPrimitive_original(IDirect3DDevice8_DrawPrimitive.location)(self,primitivetype,startvertex,primitivecount);

    except

    end;
  end;

  result:=TIDirect3DDevice8_DrawPrimitive_Original(IDirect3DDevice8_DrawPrimitive.location)(self,PrimitiveType,StartVertex,PrimitiveCount);
  watchfornextdraw:=false;
end;

function IDirect3DDevice8_DrawIndexedPrimitive_Hook(const self: IDirect3DDevice8;const _Type : TD3DPrimitiveType; const minIndex, NumVertices, startIndex, primCount : Cardinal) : HResult; stdcall;
var i: integer;
    px: td3dxvector3;
    pv: td3dviewport8;
    v: tD3DXVector3;

    p: PD3DXVector3;
    pp: TD3DXVector3;
    j: single;

    aaa: TD3DXMatrix;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);
    setlength(xylist2,i+1);

    p:=@pp;

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    D3DXVec3Project(xylist2[i], V  ,pv, lastprojectionmatrix2, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except

    end;

  end;

  result:=TIDirect3DDevice8_DrawIndexedPrimitive_original(IDirect3DDevice8_DrawIndexedPrimitive.location)(self,_Type,minIndex,NumVertices,startIndex,primCount);
  watchfornextdraw:=false;
end;

function IDirect3DDevice8_DrawPrimitiveUP_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
var i: integer;
    px: td3dxvector3;
    pv: td3dviewport8;
    v: tD3DXVector3;

    p: PD3DXVector3;
    pp: TD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);
    setlength(xylist2,i+1);

    p:=@pp;

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    D3DXVec3Project(xylist2[i], V  ,pv, lastProjectionmatrix2, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except

    end;

  end;

  result:=TIDirect3DDevice8_DrawPrimitiveUP_Original(IDirect3DDevice8_DrawPrimitiveUP.location)(self,PrimitiveType,primitivecount,pVertexStreamZeroData,VertexStreamZeroStride);
  watchfornextdraw:=false;
end;

function IDirect3DDevice8_DrawIndexedPrimitiveUP_Hook(const self:IDirect3DDevice8;const PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertexIndices, PrimitiveCount : Cardinal; pIndexData : Pointer; IndexDataFormat : TD3DFormat; pVertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
var i: integer;
    px: td3dxvector3;
    pv: td3dviewport8;
    v: tD3DXVector3;

    pp: TD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);
    setlength(xylist2,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    D3DXVec3Project(xylist2[i], V  ,pv, lastProjectionmatrix2, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except

    end;
  end;

  result:=TIDirect3DDevice8_DrawIndexedPrimitiveUP_original(IDirect3DDevice8_DrawIndexedPrimitiveUP.location)(self,PrimitiveType,minVertexIndex,numvertexindices,primitivecount,pIndexData,indexDataFormat,pVertexStreamZeroData,VertexStreamZeroStride);
  watchfornextdraw:=false;
end;


function IDirect3DDevice8_SetVertexShader_Hook(const self:IDirect3DDevice8;const Handle : LongWord) : HResult; stdcall;
begin
  result:=TIDirect3DDevice8_SetVertexShader_Original(IDirect3DDevice8_SetVertexShader.location)(self,handle);
  if not dontsetlastvertexshader then lastvertexshadeR:=handle;
end;

//-----------------------------------------------------------------------
//IDirect3DTexture Hooks
//-----------------------------------------------------------------------
function IDirect3DTexture8_AddRef_Hook(const self:IDirect3DTexture8) : integer; stdcall;
begin
  result:=TIDirect3DTexture8_AddRef_Original(IDirect3DTexture8_AddRef.location)(self);
end;


function IDirect3DTexture8_Release_Hook(const self:IDirect3DTexture8) : integer; stdcall;
var old: dword;
    i,j: integer;
    found: boolean;
begin
  old:=dword(self);
  if not imreleasing then while locking do ;

  result:=TIDirect3DTexture8_release_original(IDirect3DTexture8_release.location)(self);
  if result=0 then
  begin
    found:=false;
    for i:=0 to length(texturelist)-1 do
      if old=dword(texturelist[i].texturehandle) then
      begin
        found:=true;
        j:=i;
        break;
      end;

    if found then
    begin
      try
        removetexture(j);
      except

      end;
    end;
  end;
end;

function IDirect3DTexture8_GetSurfaceLevel_Hook(const self:IDirect3DTexture8; const Level : Cardinal; out ppSurfaceLevel : IDirect3DSurface8) : HResult; stdcall;
begin
  result:=TIDirect3DTexture8_GetSurfaceLevel_original(IDirect3DTexture8_GetSurfaceLevel.location)(self,level,ppSurfaceLevel);

  if result=0 then
  begin
    //hook Unlock

  end;

end;

function IDirect3DTexture8_UnlockRect_Hook(const self:IDirect3DTexture8; const Level : Cardinal) : HResult; stdcall;
begin
  result:=TIDirect3DTexture8_UnlockRect_original(IDirect3DTexture8_UnlockRect.location)(self,level);
end;


//-----------------------------------------------------------------------
//IDirect3DVertexBuffer8 Hooks
//-----------------------------------------------------------------------


function IDirect3DVertexBuffer8_Lock_Hook(const self:IDirect3DVertexBuffer8; const OffsetToLock, SizeToLock : Cardinal; var ppbData : PByte; const Flags : LongWord) : HResult; stdcall;
begin
  result:=TIDirect3DVertexBuffer8_Lock_original(IDirect3DVertexBuffer8_Lock.location)(self,OffsetToLock,SizeToLock,ppbData,Flags);
end;

function IDirect3DVertexBuffer8_Unlock_Hook (const self:IDirect3DVertexBuffer8) : HResult; stdcall;
begin
  result:=TIDirect3DVertexBuffer8_UnLock_original(IDirect3DVertexBuffer8_UnLock.location)(self);
end;


end.
