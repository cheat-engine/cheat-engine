library dxhook;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  windows,
  KeyListener in 'KeyListener.pas',
  DirectxHook in 'DirectxHook.pas',
  directx9hook in 'directx9hook.pas',
  directx7hook in 'directx7hook.pas',
  directxmessconfig in '..\directxmessconfig.pas';

{$R *.res}

exports Direct3DCreate8Hook;
exports Direct3DCreate8_original;

exports Direct3DCreate9Hook;
exports Direct3DCreate9_original;
exports IDirect3D9_CreateDevice_Hook;
exports IDirect3D9_CreateDevice_original;
exports IDirect3DDevice9_Release_Hook;
exports IDirect3DDevice9_Release_original;
exports IDirect3DDevice9_Reset_Hook;
exports IDirect3DDevice9_Reset_original;
exports IDirect3DDevice9_CreateTexture_Hook;
exports IDirect3DDevice9_CreateTexture_original;
exports IDirect3DDevice9_BeginScene_Hook;
exports IDirect3DDevice9_BeginScene_original;
exports IDirect3DDevice9_EndScene_Hook;
exports IDirect3DDevice9_EndScene_original;
exports IDirect3DDevice9_SetTransform_Hook;
exports IDirect3DDevice9_SetTransform_original;
exports IDirect3DDevice9_GetTransform_Hook;
exports IDirect3DDevice9_GetTransform_original;
exports IDirect3DDevice9_SetRenderState_Hook;
exports IDirect3DDevice9_SetRenderState_original;
exports IDirect3DDevice9_SetTexture_Hook;
exports IDirect3DDevice9_SetTexture_original;
exports IDirect3DDevice9_DrawPrimitive_Hook;
exports IDirect3DDevice9_DrawPrimitive_original;
exports IDirect3DDevice9_DrawIndexedPrimitive_Hook;
exports IDirect3DDevice9_DrawIndexedPrimitive_original;
exports IDirect3DDevice9_DrawPrimitiveUP_Hook;
exports IDirect3DDevice9_DrawPrimitiveUP_original;
exports IDirect3DDevice9_DrawIndexedPrimitiveUP_Hook;
exports IDirect3DDevice9_DrawIndexedPrimitiveUP_original;

exports IDirect3DTexture9_Release_Hook;
exports IDirect3DTexture9_Release_original;

exports configurationname;
exports InitializeKeyListener;

var i: integer;
begin
  outputdebugstring('dxhook basic initialization');

  d3d8dll:=Loadlibrary('d3d8.dll');
  d3d9dll:=Loadlibrary('d3d9.dll');




  keylist:=tstringlist.Create;
  showkeylist:=false;
  texturepointer:=-1;
  zoom:=1;
  fog:=2;
  zbuffer:=2;
  lighting:=2;
  wireframe:=2;

  usefpslag:=true;
  clicktime:=80;
  intervalbetweenshots:=5000;
  lastshot:=0;


  mousecallibrationmode:=0;
  mousecallibrationactive:=false;

  directxhook.projectionmatrixset:=false;
  directxhook.dontsetlastprojectionmatrix:=false;
  directxhook.worldmatrixset:=false;
  directxhook.dontsetlastworldmatrix:=false;
  directxhook.viewmatrixset:=false;
  directxhook.dontsetlastviewmatrix:=false;


  directx9hook.projectionmatrixset:=false;
  directx9hook.dontsetlastprojectionmatrix:=false;
  directx9hook.worldmatrixset:=false;
  directx9hook.dontsetlastworldmatrix:=false;
  directx9hook.viewmatrixset:=false;
  directx9hook.dontsetlastviewmatrix:=false;


  mousespeedx[0]:=0;
  mousespeedx[1]:=1;
  for i:=2 to 40 do mousespeedx[i]:=mousespeedx[i-1]+1;

  mousespeedy[0]:=0;
  mousespeedy[1]:=1;
  for i:=2 to 40 do mousespeedy[i]:=mousespeedy[i-1]+1;

  resetdevice:=false;
  imdrawing:=false;


  QueryPerformanceFrequency(tickspersecond);
  TicksPerMS:=tickspersecond / 1000;
  onetick:=1/ticksperms;
end.
