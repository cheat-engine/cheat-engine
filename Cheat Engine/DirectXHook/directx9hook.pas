unit directx9hook;

interface
uses classes,windows,messages,forms,dialogs,sysutils,direct3d9,d3dx9,math,graphics,keylistener;

type TTextureInfo = record
  //creation parameters
  Width, Height, Levels : Cardinal;
  Usage : LongWord;
  Format : TD3DFormat;
  Pool : TD3DPool;

  locked: boolean;
  texturehandle: IDirect3DTexture9;
  xdelta,ydelta,zdelta: single;
end;

type PIDirect3D9=^IDirect3D9;
type TDirect3DCreate9=function(SDKVersion: DWORD): PIDirect3D9; stdcall;

type TIDirect3D9_CreateDevice_Original=function(const self: IDirect3D9; const Adapter : Cardinal; const DeviceType : TD3DDevType; FocusWindow : HWND; BehaviorFlags : LongWord; var PresentationParameters : TD3DPresentParameters; out ReturnedDeviceInterface : IDirect3DDevice9) : HResult; stdcall;
type TIDirect3DDevice9_Reset_Original=function(const self: IDirect3DDevice9 ;var PresentationParameters : TD3DPresentParameters) : HResult; stdcall;
type TIDirect3DDevice9_CreateTexture_Original=function(const self: IDirect3DDevice9 ;const Width, Height, Levels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out Texture : IDirect3DTexture9; SharedHandle : PHandle) : HResult; stdcall;
type TIDirect3DDevice9_BeginScene_Original=function(const self: IDirect3DDevice9): HResult; stdcall;
type TIDirect3DDevice9_EndScene_Original=function(const self: IDirect3DDevice9): HResult; stdcall;
type TIDirect3DDevice9_SetTransform_Original=function(const self: IDirect3DDevice9; State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
type TIDirect3DDevice9_GetTransform_Original=function(const self: IDirect3DDevice9; State : TD3DTransformStateType; out Matrix : TD3DMatrix) : HResult; stdcall;
type TIDirect3DDevice9_SetRenderState_Original=function(const self: IDirect3DDevice9; State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
type TIDirect3DDevice9_SetTexture_Original=function(const self: IDirect3DDevice9; const Stage : LongWord; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
type TIDirect3DDevice9_DrawPrimitive_Original=function(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
type TIDirect3DDevice9_DrawIndexedPrimitive_Original=function(const self: IDirect3DDevice9;_Type : TD3DPrimitiveType; const BaseVertexIndex : Integer; const MinVertexIndex, NumVertices, StartIndex, PrimCount : Cardinal) : HResult; stdcall;
type TIDirect3DDevice9_DrawPrimitiveUP_Original=function(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
type TIDirect3DDevice9_DrawIndexedPrimitiveUP_Original=function(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertices, PrimitiveCount : Cardinal; IndexData : Pointer; IndexDataFormat : TD3DFormat; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;

type TIDirect3DTexture9_Release_Original=function(const self: IDirect3DTexture9): integer; stdcall;



var Direct3DCreate9:TDirect3DCreate9;
    Direct3DCreate9Info: TAPIInfo;

    IDirect3D9_CreateDevice:TAPIInfo;

    //IDirect3DDevice8 method hooks
    IDirect3DDevice9_Reset: TApiInfo;
    IDirect3DDevice9_CreateTexture: TApiInfo;
    IDirect3DDevice9_BeginScene: TApiInfo;
    IDirect3DDevice9_EndScene: TApiInfo;
    IDirect3DDevice9_SetTransform: TApiInfo;
    IDirect3DDevice9_GetTransform: TApiInfo;
    IDirect3DDevice9_SetRenderState: TApiInfo;
    IDirect3DDevice9_SetTexture: TAPIInfo;
    IDirect3DDevice9_DrawPrimitive: TAPIInfo;
    IDirect3DDevice9_DrawIndexedPrimitive: TAPIInfo;
    IDirect3DDevice9_DrawPrimitiveUP:TAPIInfo;
    IDirect3DDevice9_DrawIndexedPrimitiveUP: TApiinfo;

    IDirect3DTexture9_Release: TAPIInfo;


    d3d9dll: THandle;

procedure handlekeypresses;
procedure LoadLockedTextureInfo9(aimconfigfile:string);
procedure SaveLockedTextureInfo9(aimconfigfile: string);
procedure SaveAllTextures9;


function Direct3DCreate9Hook(SDKVersion: DWORD): PIDirect3D9; stdcall;
function IDirect3D9_CreateDevice_Hook(const self: IDirect3D9; const Adapter : Cardinal; const DeviceType : TD3DDevType; FocusWindow : HWND; BehaviorFlags : LongWord; var PresentationParameters : TD3DPresentParameters; out ReturnedDeviceInterface : IDirect3DDevice9) : HResult; stdcall;

function IDirect3DDevice9_Reset_Hook(const self: IDirect3DDevice9 ;var PresentationParameters : TD3DPresentParameters) : HResult; stdcall;
function IDirect3DDevice9_CreateTexture_Hook(const self: IDirect3DDevice9 ;const Width, Height, Levels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out Texture : IDirect3DTexture9; SharedHandle : PHandle) : HResult; stdcall;
function IDirect3DDevice9_BeginScene_Hook(const self: IDirect3DDevice9): HResult; stdcall;
function IDirect3DDevice9_EndScene_Hook(const self: IDirect3DDevice9): HResult; stdcall;
function IDirect3DDevice9_SetTransform_Hook(const self: IDirect3DDevice9; State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
function IDirect3DDevice9_GetTransform_Hook(const self: IDirect3DDevice9; State : TD3DTransformStateType; out Matrix : TD3DMatrix) : HResult; stdcall;
function IDirect3DDevice9_SetRenderState_Hook(const self: IDirect3DDevice9; State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
function IDirect3DDevice9_SetTexture_Hook(const self: IDirect3DDevice9; const Stage : LongWord; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
function IDirect3DDevice9_DrawPrimitive_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
function IDirect3DDevice9_DrawIndexedPrimitive_Hook(const self: IDirect3DDevice9;_Type : TD3DPrimitiveType; const BaseVertexIndex : Integer; const MinVertexIndex, NumVertices, StartIndex, PrimCount : Cardinal) : HResult; stdcall;
function IDirect3DDevice9_DrawPrimitiveUP_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
function IDirect3DDevice9_DrawIndexedPrimitiveUP_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertices, PrimitiveCount : Cardinal; IndexData : Pointer; IndexDataFormat : TD3DFormat; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;

function IDirect3DTexture9_Release_Hook(const self: IDirect3DTexture9): integer; stdcall;

var //variables for the directx9 hook
    cefont: ID3dXFont;
    mysprite: ID3DXSprite;
    myspritetexture: IDirect3DTexture9;
    targettexture: IDirect3DTexture9;
    locktexture: IDirect3DTexture9;
    movementtexture: IDirect3DTexture9;

    lockedstringtexture: IDirect3dTexture9;
    unlockedStringTexture: IDirect3dTexture9;
    textureStringTexture: IDirect3DTexture9;
    blacktexture: IDirect3DTexture9;


    dontsetlastviewmatrix:  boolean;
    dontsetlastworldmatrix: boolean;
    dontsetlastprojectionmatrix: boolean;
    lastviewmatrix:  TD3DMatrix;
    lastworldmatrix: TD3DMatrix;
    lastprojectionmatrix:  TD3DMatrix;
    lastprojectionmatrix2: TD3DMatrix;
    viewmatrixset:  boolean;
    worldmatrixset: boolean;
    projectionmatrixset: boolean;

    lockedtexturelist: array of TTextureInfo;
    texturelist: array of TTextureInfo;
    xylist: array of td3dxvector3;

    oldpos: td3dxvector3;
    newtick: int64;
    tickssincelastdraw: dword;
    mssincelastdraw: double;

    watchfornextdraw: boolean;
    CurrentTexture: integer;
    nextdrawstage: dword;

    resetdevice: boolean;

implementation

procedure SaveAllTextures9;
var i: integer;
begin
  for i:=0 to length(texturelist)-1 do
    D3DXSaveTextureToFile(pchar('CETEX'+IntToStr(i)+'.bmp'),D3DXIFF_BMP,texturelist[i].texturehandle,nil);
end;


procedure handlekeypresses;
var found: boolean;
    i,j: integer;
begin
  if keys.callibrationmode then
  begin
    try
      if checkkeycombo(keys.previoustexture) then if (texturepointer-1)<0 then texturepointer:=length(texturelist)-1 else dec(texturepointer);
      if checkkeycombo(keys.nexttexture) then if (texturepointer+1)>=length(texturelist) then texturepointer:=0 else inc(texturepointer);
      if checkkeycombo(keys.locktexture) then  //(un)lock
      begin
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
          lockedtexturelist[i].texturehandle._Release;
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
          fillmemory(@lockedtexturelist[length(lockedtexturelist)-1].texturehandle,4,0);


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
      end;


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
    except

    end;
  end;
end;


procedure SaveLockedTextureInfo9(aimconfigfile: string);
var i,j,k: integer;
    tr: TD3DLocked_Rect;
    aimsettings: tfilestream;
    lr:TD3DLocked_Rect;
    bts: array of byte;
    s: string;

    total: dword;
    pdesc: td3dsurface_desc;
    bug: boolean;
    temp: array of byte;
begin
  try
    aimsettings:=tfilestream.Create((keys.cedir+aimconfigfile+'.cta'),fmCreate);
    try
      total:=length(lockedtexturelist);
      aimsettings.WriteBuffer(total,4);
      for i:=0 to length(lockedtexturelist)-1 do
      begin
        //save the levels
        aimsettings.WriteBuffer(lockedtexturelist[i],sizeof(TTextureInfo));

        try
          if lockedtexturelist[i].texturehandle.LockRect(0,tr,nil,D3DLOCK_NOSYSLOCK)=0 then
          begin

            try
              lockedtexturelist[i].texturehandle.GetLevelDesc(0,pdesc);

//              s:='Width='+IntToStr(lockedtexturelist[i].Width)+' Height='+IntToStr(lockedtexturelist[i].Height)+' pitch='+IntToStr(tr.Pitch)+' format='+IntToStr(lockedtexturelist[i].Format)+' levels='+inttostr(lockedtexturelist[i].Levels);
              total:=pdesc.width*pdesc.Height; //sigh...
              total:=total div 4;

              setlength(temp,total);
              bug:=true;
              while bug do
              begin
                try
                  copymemory(@temp[0],tr.Bits,total);
                  bug:=false;
                except
                  on e: exception do
                  begin
                    total:=total div 2;
                    setlength(temp,total);
                    if total=0 then bug:=false; //bah
                  end;
                end;
              end;

              aimsettings.WriteBuffer(total,4);
              aimsettings.WriteBuffer(temp[0],total);
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
    end;
  except end;
end;

procedure LoadLockedTextureInfo9(aimconfigfile:string);
var aimsettings,aimsettings2: tfilestream;
    images: array of array of byte;
    i,j: integer;
    tempimage: array of byte;

    imageloaded:boolean;
    total: dword;
    tr: TD3DLocked_Rect;
    pdesc: td3dsurface_desc;
    bug:boolean;
begin
  try

    for i:=0 to length(lockedtexturelist)-1 do
    begin
      try
        lockedtexturelist[i].texturehandle._Release;
      except
        //outputdebugstring('failed to release');
      end;
      zeromemory(@lockedtexturelist[i],sizeof(TTextureInfo));
    end;

    for i:=0 to length(texturelist)-1 do
    begin
      try
        texturelist[i].locked:=false;
      except

      end;
    end;

    setlength(lockedtexturelist,0);
  except

  end;

  LoadPhase:=1;

  try
    try
      aimsettings:=tfilestream.Create(keys.cedir+aimconfigfile+'.cta',fmOpenRead);
      try

        aimsettings.ReadBuffer(total,4);
        setlength(lockedtexturelist,total);
        setlength(images,total);

        maxposition:=length(lockedtexturelist)-1;
        for i:=0 to length(lockedtexturelist)-1 do
        begin
          currentposition:=i;

          aimsettings.ReadBuffer(lockedtexturelist[i].width,sizeof(TTextureInfo));
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
        aimsettings.free;
      end;

      //check the texturelist for these textures
      //outputdebugstring('Entering phase 2');


      loadphase:=2;
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
                  total:=(pdesc.Width*pdesc.Height);
                  total:=total div 4;
                  if total<length(images[j]) then continue;

                  setlength(tempimage,total);

                  bug:=true;
                  while bug do
                  begin
                    try
                      copymemory(@tempimage[0],tr.Bits,total);
                      imageloaded:=true;
                      bug:=false;
                    except
                      total:=total div 2;
                      setlength(tempimage,total);
                      if (total<length(images[j])) or (total=0) then bug:=false;
                    end;
                  end;
                finally
                  texturelist[i].texturehandle.UnlockRect(0);
                  sleep(5);
                end;
              end;
              except
                //doesn't exist
              end;

            end;

            if comparemem(@tempimage[0],@images[j][0],500) then
            begin
              //outputdebugstring('It''s the same');
              texturelist[i].locked:=true;
              texturelist[i].xdelta:=lockedtexturelist[j].xdelta;
              texturelist[i].ydelta:=lockedtexturelist[j].ydelta;
              texturelist[i].zdelta:=lockedtexturelist[j].zdelta;

              copymemory(@lockedtexturelist[j].texturehandle,@texturelist[i].texturehandle,4);
              lockedtexturelist[j].locked:=true;
              try
                lockedtexturelist[j].texturehandle._AddRef;
              except
                //outputdebugstring('failed to addref');
              end;
            end;
            sleep(1);
          end;
          sleep(1);
        end;
      except

      end;
    finally
      for i:=0 to length(images)-1 do setlength(images[i],0);
      setlength(images,0);
      setlength(tempimage,0);
    end;

end;


{
-------------------------------------------------------------------------
DirectX 9
-------------------------------------------------------------------------
}
procedure Removetexture(j: integer);
var i: integer;
begin
  if texturepointer>j then dec(texturepointer);

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

  fillmemory(@texturelist[length(texturelist)-1].texturehandle,4,0);

  try
    setlength(texturelist,length(texturelist)-1);
  except

  end;

  if length(texturelist)=0 then texturepointer:=-1;
  if (texturepointer)>(length(texturelist)-1) then texturepointeR:=length(texturelist)-1;
end;

function Direct3DCreate9Hook(SDKVersion: DWORD): PIDirect3D9; stdcall;
var x,y:dword;
begin
  asm
    push esi
    push edi
    lea esi,Direct3DCreate9Info.original[0]
    mov edi,Direct3DCreate9Info.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  outputdebugstring('CEHOOK:Creating a direct3d9 object');
  result:=Direct3DCreate9(SDKVersion);

  x:=dword(result);

 //hook createdevice

  outputdebugstring('Hooking createdevice');


  if IDirect3D9_CreateDevice.location=nil then
    IDirect3D9_CreateDevice.location:=pointer(pdword(pdword(x)^+4*16)^);

  virtualprotect(pointer(pdword(x)^+4*16),4,PAGE_EXECUTE_READWRITE,y);
  pdword(pdword(x)^+4*16)^:=dword(@IDirect3D9_CreateDevice_Hook);
  virtualprotect(pointer(pdword(x)^+4*16),4,y,y);

  outputdebugstring('Hooking createdevice-success');
end;




//Hooked IDirect3D9 methods:

function Fixhook(const self:IDirect3DDevice9):boolean; stdcall;
var x:dword;
begin
  virtualprotect(pdword(pdword(dword(Self))^),4*77,PAGE_EXECUTE_READWRITE,x);

  //hook Reset
  if IDirect3DDevice9_Reset.location=nil then
    IDirect3DDevice9_Reset.location:=pointer(pdword(pdword(dword(Self))^+4*16)^);
  pdword(pdword(dword(Self))^+4*16)^:=dword(@IDirect3DDevice9_Reset_Hook);

  //hook createtexture
  if IDirect3DDevice9_CreateTexture.location=nil then
    IDirect3DDevice9_CreateTexture.location:=pointer(pdword(pdword(dword(Self))^+4*23)^);
  pdword(pdword(dword(Self))^+4*23)^:=dword(@IDirect3DDevice9_CreateTexture_Hook);

  //hook BeginScene
  if IDirect3DDevice9_BeginScene.location=nil then
    IDirect3DDevice9_BeginScene.location:=pointer(pdword(pdword(dword(Self))^+4*41)^);
  pdword(pdword(dword(Self))^+4*41)^:=dword(@IDirect3DDevice9_BeginScene_Hook);


  //hook EndScene
  if IDirect3DDevice9_EndScene.location=nil then
    IDirect3DDevice9_EndScene.location:=pointer(pdword(pdword(dword(Self))^+4*42)^);
  pdword(pdword(dword(Self))^+4*42)^:=dword(@IDirect3DDevice9_EndScene_Hook);

  //hook SetTransform
  if IDirect3DDevice9_SetTransform.location=nil then
    IDirect3DDevice9_SetTransform.location:=pointer(pdword(pdword(dword(Self))^+4*44)^);
  pdword(pdword(dword(Self))^+4*44)^:=dword(@IDirect3DDevice9_SetTransform_Hook);

  //hook GetTransform
  if IDirect3DDevice9_GetTransform.location=nil then
    IDirect3DDevice9_GetTransform.location:=pointer(pdword(pdword(dword(Self))^+4*45)^);
  pdword(pdword(dword(Self))^+4*45)^:=dword(@IDirect3DDevice9_GetTransform_Hook);

  //hook SetRenderState
  if IDirect3DDevice9_SetRenderState.location=nil then
    IDirect3DDevice9_SetRenderState.location:=pointer(pdword(pdword(dword(Self))^+4*57)^);
  pdword(pdword(dword(Self))^+4*57)^:=dword(@IDirect3DDevice9_SetRenderState_Hook);

  //hook SetTexture
  if IDirect3DDevice9_SetTexture.location=nil then
    IDirect3DDevice9_SetTexture.location:=pointer(pdword(pdword(dword(Self))^+4*65)^);
  pdword(pdword(dword(Self))^+4*65)^:=dword(@IDirect3DDevice9_SetTexture_Hook);

  //hook DrawPrimitive
  if IDirect3DDevice9_DrawPrimitive.location=nil then
    IDirect3DDevice9_DrawPrimitive.location:=pointer(pdword(pdword(dword(Self))^+4*81)^);
  pdword(pdword(dword(Self))^+4*81)^:=dword(@IDirect3DDevice9_DrawPrimitive_Hook);

  //hook DrawIndexedPrimitive
  if IDirect3DDevice9_DrawIndexedPrimitive.location=nil then
    IDirect3DDevice9_DrawIndexedPrimitive.location:=pointer(pdword(pdword(dword(Self))^+4*82)^);
  pdword(pdword(dword(Self))^+4*82)^:=dword(@IDirect3DDevice9_DrawIndexedPrimitive_Hook);

  //hook DrawPrimitiveUP
  if IDirect3DDevice9_DrawPrimitiveUP.location=nil then
    IDirect3DDevice9_DrawPrimitiveUP.location:=pointer(pdword(pdword(dword(Self))^+4*83)^);
  pdword(pdword(dword(Self))^+4*83)^:=dword(@IDirect3DDevice9_DrawPrimitiveUP_Hook);

  //hook DrawIndexedPrimitiveUP
  if IDirect3DDevice9_DrawIndexedPrimitiveUP.location=nil then
    IDirect3DDevice9_DrawIndexedPrimitiveUP.location:=pointer(pdword(pdword(dword(Self))^+4*84)^);
  pdword(pdword(dword(Self))^+4*84)^:=dword(@IDirect3DDevice9_DrawIndexedPrimitiveUP_Hook);
end;


function IDirect3D9_CreateDevice_Hook(const self: IDirect3D9; const Adapter : Cardinal; const DeviceType : TD3DDevType; FocusWindow : HWND; BehaviorFlags : LongWord; var PresentationParameters : TD3DPresentParameters; out ReturnedDeviceInterface : IDirect3DDevice9) : HResult; stdcall;
var x: dword;
    logfont: tlogfont;
    desc:TD3DXFontDescA;
begin
  directxversion:=directx9;
  setlength(xylist,0);


  Behaviorflags:=behaviorflags and not (D3DCREATE_PUREDEVICE);
  result:=TIDirect3D9_CreateDevice_Original(IDirect3D9_CreateDevice.location)(self,Adapter,DeviceType,FocusWindow,BehaviorFlags,PresentationParameters, ReturnedDeviceInterface);

  if result=0 then
  begin
    D3DXCreateTextureFromFile(ReturnedDeviceInterface,pchar(keys.CEDir+'lockedstring.bmp'),lockedStringTexture);
    D3DXCreateTextureFromFile(ReturnedDeviceInterface,pchar(keys.CEDir+'unlockedstring.bmp'),unlockedStringTexture);
    D3DXCreateTextureFromFile(ReturnedDeviceInterface,pchar(keys.CEDir+'texturestring.bmp'),textureStringTexture);
    D3DXCreateTextureFromFile(ReturnedDeviceInterface,pchar(keys.CEDir+'Black.bmp'),blackTexture);

    //D3DXCreateTextureFromFileEx(ReturnedDeviceInterface,pchar(keys.CEDir+'Logo3.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,myspritetexture);
    D3DXCreateTextureFromFileEx(ReturnedDeviceInterface,pchar(keys.CEDir+'TargetTexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,targettexture);
    D3DXCreateTextureFromFileEx(ReturnedDeviceInterface,pchar(keys.CEDir+'Locktexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,locktexture);
    D3DXCreateTextureFromFileEx(ReturnedDeviceInterface,pchar(keys.CEDir+'Movementtexture.bmp'),D3DX_DEFAULT,D3DX_DEFAULT,1,0,D3DFMT_A8R8G8B8,D3DPOOL_MANAGED,D3DX_DEFAULT,D3DX_DEFAULT,$FFFFFFFF,nil,nil,movementtexture);

    D3DXCreateSprite(ReturnedDeviceInterface,mysprite);

    cefonthandle.free;
    cefonthandle:=tfont.Create;
    cefonthandle.Color:=clred;
    cefonthandle.Name:='Courier';

    if GetObject(cefonthandle.handle, SizeOf(TLogFont), @logfont)<>0 then
    begin
      desc.Height:=logfont.lfHeight;
      desc.Width:=logfont.lfWidth;
      desc.Weight:=logfont.lfWeight;
      desc.MipLevels:=0;
      desc.Italic:=false;
      desc.CharSet:=logfont.lfCharSet;
      desc.OutputPrecision:=logfont.lfOutPrecision;
      desc.Quality:=logfont.lfQuality;
      desc.PitchAndFamily:=logfont.lfPitchAndFamily;

      copymemory(@desc.FaceName[0],@logfont.lfFaceName[0],32);

      desc.Height:=0;
      desc.Width:=0;
      desc.Weight:=0;
      desc.MipLevels:=1;
      desc.Italic:=false;
      desc.CharSet:=DEFAULT_CHARSET;
      desc.OutputPrecision:=0;
      desc.Quality:=DEFAULT_QUALITY	;
      desc.PitchAndFamily:=DEFAULT_PITCH;
      desc.FaceName:='Courier';

      //Why doesn't the font creation in dx9 work on a intel 82815 graphics controller, but the dx8 version does work ? 
      d3dx9.d3dxcreatefontindirectA(ReturnedDeviceInterface,@desc,CEFont);
    end;

  end;


  if result=0 then
    fixhook(ReturnedDeviceInterface);
end;

function IDirect3DDevice9_Reset_Hook(const self: IDirect3DDevice9 ;var PresentationParameters : TD3DPresentParameters) : HResult; stdcall;
var i: integer;
    err: dword;
begin
  try
    mysprite.OnLostDevice;
    cefont.OnLostDevice;

    fillmemory(@texturelist[0],length(texturelist)*sizeof(TTextureInfo),0);
    fillmemory(@lockedtexturelist[0],length(texturelist)*sizeof(TTextureInfo),0);

    setlength(texturelist,0);
    setlength(lockedtexturelist,0);
  except

  end;
  texturepointer:=-1;
  err:=TIDirect3DDevice9_Reset_Original(IDirect3DDevice9_Reset.location)(self,PresentationParameters);

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


function IDirect3DDevice9_CreateTexture_Hook(const self: IDirect3DDevice9 ;const Width, Height, Levels : Cardinal; const Usage : LongWord; Format : TD3DFormat; Pool : TD3DPool; out Texture : IDirect3DTexture9; SharedHandle : PHandle) : HResult; stdcall;
var i: integer;
    x: dword;
begin
  result:=TIDirect3DDevice9_CreateTexture_Original(IDirect3DDevice9_CreateTexture.location)(self,width,height,levels,usage,format,pool,texture,sharedhandle);

  if imdrawing then exit;

  if not ((usage=0) and (pool=D3DPOOL_DEFAULT)) then
  begin
    i:=length(texturelist);
    setlength(texturelist,i+1);

    copymemory(@texturelist[i].texturehandle,@texture,4);
    texturelist[i].Width:=width;
    texturelist[i].Height:=height;
    texturelist[i].Levels:=levels;
    texturelist[i].Usage:=usage;
    texturelist[i].Format:=format;
    texturelist[i].Pool:=pool;
    texturelist[i].locked:=false;

    if texturepointer=-1 then texturepointer:=0;
  end;

  if result=0 then
  begin
    //hook release
    virtualprotect(pdword(pdword(dword(Texture))^+4*2),4,PAGE_EXECUTE_READWRITE,x);

    if IDirect3DTexture9_release.location=nil then
      IDirect3DTexture9_release.location:=pointer(pdword(pdword(dword(Texture))^+4*2)^);
    pdword(pdword(dword(Texture))^+4*2)^:=dword(@IDirect3DTexture9_release_Hook);

    virtualprotect(pdword(pdword(dword(Texture))^+4*2),4,x,x);
  end;
end;


function IDirect3DDevice9_BeginScene_Hook(const self: IDirect3DDevice9): HResult; stdcall;
begin
  setlength(xylist,0);
  result:=TIDirect3DDevice9_BeginScene_Original(IDirect3DDevice9_BeginScene.location)(self);
end;

procedure DrawHotkeylist9(const self: IDirect3DDevice9;vp:td3dviewport9);
var position,scale: td3dxvector3;
    transform: td3dmatrix;
    pdesc: TD3DSurface_Desc;
    i: integer;
    r: trect;
begin
  mysprite._Begin(D3DXSPRITE_ALPHABLEND);

  if blacktexture.GetLevelDesc(0,pdesc)=0 then
  begin
    scale.x:=requiredkeylistwidth / pdesc.Width;
    scale.y:=requiredkeylistheight / pdesc.Height;
    scale.z:=1;
  end
  else
  begin
    scale.x:=requiredkeylistwidth;
    scale.y:=requiredkeylistheight;
    scale.z:=1;
  end;

  position.x:=vp.Width-requiredkeylistwidth;
  position.y:=0;
  position.z:=0;

  D3DXMatrixTransformation(transform,nil,nil,scale,nil,nil,position);
  mysprite.SetTransform(transform);
  mysprite.Draw(blacktexture,nil,nil,nil,D3DCOLOR_ARGB(127,255,255,255));


  D3DXMatrixScaling(transform,1,1,1);
  mysprite.SetTransform(transform);
  mysprite._End;

  //draw the strings in the box
  r.Left:=vp.Width-requiredkeylistwidth+1;
  r.Top:=0;
  r.Bottom:=r.left+16;
  r.Right:=vp.Width;

  for i:=0 to keylist.Count-1 do
  begin
    r.Top:=i*16;
    r.Bottom:=i*16+(r.left+16);
    cefont.DrawTextA(nil,pchar(keylist[i]),length(keylist[i]),r,0,D3DCOLOR_ARGB(240,255,255,255));
  end;  

end;


function IDirect3DDevice9_EndScene_Hook(const self: IDirect3DDevice9): HResult; stdcall;
var i: integer;
    r: trect;
    scale:td3dxvector3;
    position: td3dxvector3;
    desc: TD3Dsurface_desc;
    transform,transform2: td3dmatrix;
    closest: integer;
    closestsqr1: single;
    closestsqr2: single;

    vp: Td3dViewport9;
    tempstring: string;
    tempsingle: single;

    xd,yd: single;
    xd2,yd2: integer;
    start,stop: integer;
    curtime:dword;

begin
  position.z:=0;

  try
    imdrawing:=true;
    try
      dontsetlastworldmatrix:=true;
      dontsetlastprojectionmatrix:=true;
      dontsetlastviewmatrix:=true;
      self.GetViewport(vp);

      mysprite._Begin(D3DXSPRITE_ALPHABLEND);
      try
        if callibrationmode then
        begin
          //draw texture string

          position.x:=0;
          position.y:=0;
          position.z:=0;
          mysprite.Draw(texturestringtexture,nil,nil,@position,D3DCOLOR_ARGB(127,255,255,255));

          if texturepointer>=0 then
          begin
            position.y:=16;

            scale.z:=1;
            try
              tempstring:=format('texturepointer=%d',[texturepointer]);
              r.left:=trunc(position.x)+100;
              r.Top:=trunc(position.y)+16;
              r.Right:=trunc(position.x)+100+800;
              r.Bottom:=trunc(position.y)+16+16;
              cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,255,255));



              if texturelist[texturepointer].texturehandle.GetLevelDesc(0,desc)=0 then
              begin
                scale.x:=100 / desc.Width;
                scale.y:=100 / desc.Height;
              end
              else
              begin
                scale.x:=1;
                scale.y:=1;
              end;

              D3DXMatrixTransformation(transform,nil,nil,scale,nil,nil,position);

              mysprite.SetTransform(transform);
              mysprite.Draw(texturelist[texturepointer].texturehandle,nil,nil,nil,D3DCOLOR_ARGB(127,255,255,255));
              D3DXMatrixScaling(transform,1,1,1);
              mysprite.SetTransform(transform);

            except
              removetexture(texturepointer);
            end;

            position.y:=116;
            if texturelist[texturepointer].locked then
            begin
              mysprite.Draw(lockedstringtexture,nil,nil,@position,D3DCOLOR_ARGB(127,255,255,255));

              tempstring:=format('x=%.1f y=%.1f z=%.1f',[texturelist[texturepointer].xdelta, texturelist[texturepointer].ydelta, texturelist[texturepointer].zdelta]);
              r.left:=trunc(position.x)+10;
              r.Top:=trunc(position.y)+16;
              r.Right:=trunc(position.x)+10+800;
              r.Bottom:=trunc(position.y)+16+16;
              cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));


            end
            else
              mysprite.Draw(unlockedstringtexture,nil,nil,@position,D3DCOLOR_ARGB(127,255,255,255));
          end;
        end;

        closest:=0;
        closestsqr2:=-1;


        if length(xylist)>0 then
          closestsqr2:=(abs((vp.Width/2)-(xylist[0].x)))*(abs((vp.height/2)-(xylist[0].y)));


        for i:=0 to length(xylist)-1 do
        begin
          position.x:=xylist[i].x-8; //I thought it would be -4....
          position.y:=xylist[i].y-8;
          mysprite.Draw(targettexture,nil,nil,@position,D3DCOLOR_ARGB(255,255,255,255));

          tempstring:=format('(%.3f,%.3f,%.3f)',[xylist[i].x,xylist[i].y,xylist[i].z]);

          r.left:=trunc(position.x)+10;
          r.Top:=trunc(position.y);
          r.Right:=trunc(position.x)+10+800;
          r.Bottom:=trunc(position.y)+16;
          cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

          closestsqr1:=(abs((vp.Width/2)-(xylist[i].x)))*(abs((vp.height/2)-(xylist[i].y)));

          if closestsqr2>closestsqr1 then
          begin
            closestsqr2:=closestsqr1;
            closest:=i;
          end;
        end;

        //lock on to something if there is something

        if length(xylist)>0 then
        begin
          //put in xd and yd the distance between the closest object and the screencenter
          xd:=xylist[closest].x-(vp.Width/2);
          yd:=xylist[closest].y-(vp.height/2);

          //-----------------------------------------------------
          //extrapolate the speed of the player when there is lag between pressing fire and actually shooting
          //Doesn't handle acceleration or decceleration, just if it is using a constant speed (standing still is also a constant speed...)
          inc(bbb);

          if (bbb mod 4)=0 then //get oldpos if b mod 4=0
          begin
            oldpos:=xylist[closest]; //store current position
            QueryPerformanceCounter(lasttick); //store current time
            bbb:=4; //I hate overflows
          end;

          if (bbb mod 4)=2 then //calculate the new speed(assuming the player is moving with a constant speed)
          begin
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
            position.x:=xylist[closest].x-8+xdelta;
            position.y:=xylist[closest].y-8+ydelta;
            mysprite.draw(movementtexture,nil,nil,@position,D3DCOLOR_ARGB(255,255,255,255));
          end;

          if (bbb mod 4>=2) then  //move the mouse, after recalculating the speed or when it's doing nothing
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

              r.left:=trunc(vp.Width/2)-20;
              r.Top:=trunc(vp.Height/2)+100;
              r.Right:=r.left+800;
              r.Bottom:=r.top+16;
              tempstring:=format('xd=%.2f xd2=%d',[xd,xd2]);
              cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,255,255));


              mouse_event(MOUSEEVENTF_MOVE,xd2,yd2,0,0);

              if ((bbb mod 4)=2) and autoshoot then
              begin
                if not shot then
                begin
                  //if s>=(lastshot+intervalbetweenshots) then
                  begin
                    mouse_event(MOUSEEVENTF_LEFTDOWN,0,0,0,0); //fire
                    shot:=true;
                    
                  end;
                end;

              end;
            end;
          end;

          position.x:=xylist[closest].x-8;
          position.y:=xylist[closest].y-8;

          mysprite.Draw(locktexture,nil,nil,@position,D3DCOLOR_ARGB(255,255,255,255));

          if mousecallibrationactive then
          begin
            case mousecallibrationmode of
              //-----------------------------------------------
              //1
              //-----------------------------------------------
              1:
              begin
                // move the mouse 1 point on the x axis
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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
                mousecallibrationpreviouspos.x:=position.x;
                mousecallibrationpreviouspos.y:=position.y;
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

                r.left:=trunc(vp.Width/2)+10;
                r.Top:=trunc(vp.Height/2)-16;
                r.Right:=r.left+800;
                r.Bottom:=r.top+16;
                tempstring:=format('mousespeedx[1]=%.2f',[mousespeedx[1]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

                r.Top:=r.top+16;
                r.Bottom:=r.Bottom+16;
                tempstring:=format('mousespeedx[2]=%.2f',[mousespeedx[2]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

                r.Top:=r.top+16;
                r.Bottom:=r.Bottom+16;
                tempstring:=format('mousespeedx[5]=%.2f',[mousespeedx[5]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

                r.Top:=r.top+16;
                r.Bottom:=r.Bottom+16;
                tempstring:=format('mousespeedx[10]=%.2f',[mousespeedx[10]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

                r.Top:=r.top+16;
                r.Bottom:=r.Bottom+16;
                tempstring:=format('mousespeedx[20]=%.2f',[mousespeedx[20]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));

                r.Top:=r.top+16;
                r.Bottom:=r.Bottom+16;
                tempstring:=format('mousespeedx[40]=%.2f',[mousespeedx[40]]);
                cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,0,255,0));


                mousecallibrationactive:=false;
                inc(mousecallibrationmode);
              end;

              else mousecallibrationactive:=false;


            end;
          end;


        end;
        
      finally
        mysprite._End;
      end;

      curtime:=gettickcount;
      if curtime<(autoaimtimer+2000) then
      begin
        r.left:=0;
        r.Top:=20;
        r.Right:=400;
        r.Bottom:=70;

        if autoaim then
          cefont.DrawTextA(nil,'Autoaim enabled',15,r,0,D3DCOLOR_ARGB(255,255,255,255))
        else
          cefont.DrawTextA(nil,'Autoaim disabled',16,r,0,D3DCOLOR_ARGB(255,255,255,255));

      end;

      if curtime<(lagtimer+2000) then
      begin
        r.left:=0;
        r.Top:=20;
        r.Right:=500;
        r.Bottom:=70;
        if usefpslag then
          tempstring:='UserLag='+IntToStr(lag)+' Computedlag='+IntToStr(lagfrommemory)+' FPSlag='+format('%.2f',[fpslag])+' Totallag='+format('%.2f',[lag+lagfrommemory+fpslag])
        else
          tempstring:='UserLag='+IntToStr(lag)+' Computedlag='+IntToStr(lagfrommemory)+'Totallag='+IntToStr(lag+lagfrommemory);

        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,0));
      end;


      if showloading then
      begin
        r.left:=0;
        r.Top:=36;
        r.Right:=500;
        r.Bottom:=86;
        tempstring:='Loading aimsettings file '+aimsettings+' (stage:'+IntToStr(LoadPhase)+'/2 pos:'+IntToStr(currentposition)+'/'+inttostr(maxposition);
        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,255));
      end
      else if curtime<(loadedtimer+2000) then
      begin
        r.left:=0;
        r.Top:=36;
        r.Right:=400;
        r.Bottom:=86;
        tempstring:='Loaded aimsettings '+aimsettings;
        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,255));
      end;

      if showsaving then
      begin
        r.left:=0;
        r.Top:=36;
        r.Right:=400;
        r.Bottom:=86;
        tempstring:='Saving aimsettings file '+aimsettings;
        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,255));
      end
      else if curtime<(savedtimer+2000) then
      begin
        r.left:=0;
        r.Top:=36;
        r.Right:=400;
        r.Bottom:=86;
        tempstring:='Saved aimsettings '+aimsettings;
        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,255));
      end;

      if curtime<(aimsettingstimer+2000) then
      begin
        r.left:=0;
        r.Top:=200;
        r.Right:=400;
        r.Bottom:=216;
        tempstring:='Current aimsettingfile='+aimsettings;
        cefont.DrawTextA(nil,pchar(tempstring),length(tempstring),r,0,D3DCOLOR_ARGB(255,255,0,0));
      end;

      if showkeylist then DrawHotkeylist9(self,vp);


    finally
      dontsetlastworldmatrix:=false;
      dontsetlastprojectionmatrix:=false;
      dontsetlastviewmatrix:=false;
    end;
    fixhook(self);

    if worldmatrixset then self.SetTransform(D3DTS_WORLD,lastworldmatrix);
    if projectionmatrixset then self.SetTransform(D3DTS_Projection,lastprojectionmatrix);
    if viewmatrixset then self.SetTransform(D3DTS_VIEW,lastviewmatrix);
  except

  end;

  imdrawing:=false;
  result:=TIDirect3DDevice9_EndScene_Original(IDirect3DDevice9_EndScene.location)(self);
end;

function IDirect3DDevice9_SetTransform_Hook(const self: IDirect3DDevice9; State : TD3DTransformStateType; const Matrix : TD3DMatrix) : HResult; stdcall;
var Matrix2,Matrix3,Matrix4: TD3DMatrix;
    i: integer;
    tempf1,tempf2,tempf3,tempf4: single;
begin
  //same math as in directx8 (look it up there...)
  matrix2:=Matrix;

  if (zoom<>1) and (state=D3DTS_PROJECTION) then  //zoom stuff
  begin
    if (Matrix2._23=1) then
    begin
      Matrix3:=Matrix;

      tempf1:=arccot(Matrix2._11)*2; //=fov
      tempf2:=Matrix2._11/Matrix2._00; //=aspect

      //zn (near)
      tempf3:=Matrix2._32;
      tempf3:=tempf3 / Matrix2._22;
      tempf3:=-tempf3; //tempf3= zn

      tempf3:=tempf3;//*0.1;  //increase tempf3 to look through nearby objects

      tempf4:=Matrix2._22;
      tempf4:=tempf4*tempf3;
      tempf4:=-tempf4;
      tempf4:=tempf4/tempf3+1;

      //Matrix2._22:=0.1;//500000/(500000-tempf3);
      //Matrix2._32:=-0.1;//-tempf3*500000/(500000-tempf3);

      Matrix2._11:=cot(tempf1/2/zoom);  //devide this with a higher value to zoom in
      Matrix2._00:=Matrix2._11/tempf2;
    end;
  end;

  result:=TIDirect3DDevice9_SetTransform_original(IDirect3DDevice9_SetTransform.location)(self,State,Matrix2);

  if state=D3DTS_VIEW then
  begin
    if not dontsetlastviewmatrix then
    begin
      lastviewmatrix:=Matrix;
      viewmatrixset:=true;
    end;
  end;

  if state=D3DTS_WORLD then
  begin
    if not dontsetlastworldmatrix then
    begin
      lastworldmatrix:=Matrix;
      worldmatrixset:=true;
    end;
  end;

  if state=D3dts_projection then
  begin
    lastprojectionmatrix2:=Matrix2;
    if not dontsetlastprojectionmatrix then
    begin
      lastprojectionmatrix:=Matrix;
      projectionmatrixset:=true;
    end;
  end;
end;

function IDirect3DDevice9_GetTransform_Hook(const self: IDirect3DDevice9; State : TD3DTransformStateType; out Matrix : TD3DMatrix) : HResult; stdcall;
begin
  result:=D3D_OK;

  if state=D3DTS_VIEW then
  begin
    if not dontsetlastviewmatrix then
    begin
      Matrix:=lastviewmatrix;
      exit;
    end;
  end;

  if state=D3DTS_WORLD then
  begin
    if not dontsetlastworldmatrix then
    begin
      Matrix:=lastworldmatrix;
      exit;
    end;
  end;

  if state=D3dts_projection then
  begin
    if not dontsetlastprojectionmatrix then
    begin
      Matrix:=lastprojectionmatrix;
      exit;
    end;
  end;

  result:=TIDirect3DDevice9_GetTransform_original(IDirect3DDevice9_GetTransform.location)(self,State,Matrix);
end;

function IDirect3DDevice9_SetRenderState_Hook(const self: IDirect3DDevice9; State : TD3DRenderStateType; const Value : LongWord) : HResult; stdcall;
begin

  result:=tIDirect3DDevice9_SetRenderState_original(IDirect3DDevice9_SetRenderState.location)(self,State,Value);
  if fog<>2 then self.SetRenderState(D3DRS_FOGENABLE,fog);
  if lighting<>2 then self.SetRenderState(D3DRS_lighting,lighting);
  if zbuffer<>2 then self.SetRenderState(D3DRS_ZENABLE,zbuffer);
  if wireframe=1 then self.SetRenderState(D3DRS_fillmode,D3DFILL_WIREFRAME);
  if wireframe=0 then self.SetRenderState(D3DRS_Fillmode,D3DFILL_SOLID);
end;

function IDirect3DDevice9_SetTexture_Hook(const self: IDirect3DDevice9; const Stage : LongWord; Texture : IDirect3DBaseTexture9) : HResult; stdcall;
var i: integer;
    stop: boolean;
begin

  try
  if (not imdrawing) and (not watchfornextdraw) then
  for i:=0 to length(lockedtexturelist)-1 do
  begin
    if (dword(Texture)=dword(lockedtexturelist[i].texturehandle)) then
    begin
      if (lockedtexturelist[i].locked) then  //always.....
      begin
        currenttexture:=i;
        watchfornextdraw:=true;
        nextdrawstage:=stage;
      end;
      break;
    end;
  end;
  except
    //not the end of the world if this fails, so I wont bother with critical sections and stuff.
  end;

  result:=tIDirect3DDevice9_SetTexture_original(IDirect3DDevice9_SetTexture.location)(self,stage,texture);
end;

function IDirect3DDevice9_DrawPrimitive_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const StartVertex, PrimitiveCount : Cardinal) : HResult; stdcall;
var i: integer;
    pv: td3dviewport9;
    v: tD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try

    i:=length(xylist);
    setlength(xylist,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);
    self.SetTexture(nextdrawstage,nil);

    except
      //outputdebugstring('crash in drawprimitive');
    end;

  end;
  result:=TIDirect3DDevice9_DrawPrimitive_original(IDirect3DDevice9_DrawPrimitive.location)(self,Primitivetype,StartVertex,Primitivecount);
  watchfornextdraw:=false;
end;

function IDirect3DDevice9_DrawIndexedPrimitive_Hook(const self: IDirect3DDevice9;_Type : TD3DPrimitiveType; const BaseVertexIndex : Integer; const MinVertexIndex, NumVertices, StartIndex, PrimCount : Cardinal) : HResult; stdcall;
var i: integer;
    pv: td3dviewport9;
    v: tD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except
      //outputdebugstring('crash in drawindexedprimitive');
    end;


  end;
  result:=tIDirect3DDevice9_DrawIndexedPrimitive_original(IDirect3DDevice9_DrawIndexedPrimitive.location)(self,_Type,BaseVertexIndex,MinVertexIndex,NumVertices,StartIndex,PrimCount);
  watchfornextdraw:=false;
end;

function IDirect3DDevice9_DrawPrimitiveUP_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const PrimitiveCount : Cardinal; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
var i: integer;
    pv: td3dviewport9;
    v: tD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try
    i:=length(xylist);
    setlength(xylist,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);

    self.SetTexture(nextdrawstage,nil);
    except
      //outputdebugstring('crash in drawprimitiveup');
    end;

  end;
  result:=tIDirect3DDevice9_DrawPrimitiveUP_original(IDirect3DDevice9_DrawPrimitiveUP.location)(self,PrimitiveType,PrimitiveCount,VertexStreamZeroData,VertexStreamZeroStride);
  watchfornextdraw:=false;
end;

function IDirect3DDevice9_DrawIndexedPrimitiveUP_Hook(const self: IDirect3DDevice9;PrimitiveType : TD3DPrimitiveType; const MinVertexIndex, NumVertices, PrimitiveCount : Cardinal; IndexData : Pointer; IndexDataFormat : TD3DFormat; VertexStreamZeroData : Pointer; const VertexStreamZeroStride : Cardinal) : HResult; stdcall;
var i: integer;
    pv: td3dviewport9;
    v: tD3DXVector3;
begin
  if watchfornextdraw then
  begin
    try

    i:=length(xylist);
    setlength(xylist,i+1);

    self.GetViewport(pv);

    v.x:=lockedtexturelist[currenttexture].xdelta;
    v.y:=lockedtexturelist[currenttexture].ydelta;
    v.z:=lockedtexturelist[currenttexture].zdelta;
    D3DXVec3Project(xylist[i], V  ,pv, lastProjectionmatrix, lastViewmatrix, lastWorldmatrix);

    except
      //outputdebugstring('crash in drawindexedprimitiveup');
    end;

    self.SetTexture(nextdrawstage,nil);
  end;
  result:=tIDirect3DDevice9_DrawIndexedPrimitiveUP_original(IDirect3DDevice9_DrawIndexedPrimitiveUP.location)(self,Primitivetype,minvertexindex,numvertices,primitivecount,indexdata,indexdataformat,vertexstreamzerodata,vertexstreamzerostride);
  watchfornextdraw:=false;
end;

//Texture hooks

function IDirect3DTexture9_Release_Hook(const self: IDirect3DTexture9): integer; stdcall;
var old: dword;
    i,j: integer;
    found: boolean;
begin
  old:=dword(self);
  if not imreleasing then while locking do ;

  result:=TIDirect3DTexture9_Release_original(IDirect3DTexture9_Release.location)(self);

  try
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
        //outputdebugstring('crash in removetexture(j)');
      end;
    end;
  end;
  except

  end;
end;

end.
