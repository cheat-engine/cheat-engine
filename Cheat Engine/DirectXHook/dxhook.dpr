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
  directx7hook in 'directx7hook.pas';

{$R *.res}

procedure InitializeDirectX_Hook;
var i: integer;
    op:dword;
begin
  outputdebugstring('InitializeDirectX_Hook is called');


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


  HookedDirect3D:=false;
  d3d8dll:=Loadlibrary('d3d8.dll');

  if d3d8dll<>0 then
  begin
    Direct3DCreate8Info.location:=GetProcAddress(d3d8dll,'Direct3DCreate8');
    @Direct3DCreate8:=Direct3DCreate8Info.location;

    if VirtualProtect(Direct3DCreate8Info.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Direct3DCreate8Info.jump[0]:=$e9;
      pdword(@Direct3DCreate8Info.jump[1])^:=dword(@Direct3DCreate8Hook)-dword(Direct3DCreate8Info.location)-5;

      try
        asm
          push edi
          push esi
          lea edi,Direct3DCreate8Info.original[0]
          mov esi,Direct3DCreate8Info.location
          movsd
          movsb

          lea esi,Direct3DCreate8Info.jump[0]
          mov edi,Direct3DCreate8Info.location
          movsd
          movsb

          pop esi
          pop edi
        end;
        HookedDirect3D:=true;
      except

      end;

    end;
  end;

  //hook Direct3dCreate9
  d3d9dll:=Loadlibrary('d3d9.dll');
  if d3d9dll<>0 then
  begin
    Direct3DCreate9Info.location:=GetProcAddress(d3d9dll,'Direct3DCreate9');
    @Direct3DCreate9:=Direct3DCreate9Info.location;


    if VirtualProtect(Direct3DCreate9Info.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Direct3DCreate9Info.jump[0]:=$e9;
      pdword(@Direct3DCreate9Info.jump[1])^:=dword(@Direct3DCreate9Hook)-dword(Direct3DCreate9Info.location)-5;


      try
        asm
          push edi
          push esi
          lea edi,Direct3DCreate9Info.original[0]
          mov esi,Direct3DCreate9Info.location
          movsd
          movsb

          lea esi,Direct3DCreate9Info.jump[0]
          mov edi,Direct3DCreate9Info.location
          movsd
          movsb

          pop esi
          pop edi
        end;
        HookedDirect3D:=true;
      except

      end;
    end;
  end;


  InitializeKeyListener;
end;

exports InitializeDirectX_Hook;

begin
end.
