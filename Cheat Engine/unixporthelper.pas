unit unixporthelper;

//

{$mode delphi}

interface

uses
  Classes, SysUtils{$ifdef android},log{$endif};

//just fill in some basic info. Most of it won't be used for jni. It's mostly for some structures and function declaration/pointers
type
  TLargeInteger=QWORD;
  ULONG_PTR=ptruint;
  LONG=integer;
  ULONG=DWORD;
  PVOID=pointer;
  LPBYTE=^BYTE;
  LPVOID=pointer;
  SIZE_T=integer;
  LPLPVOID=^pointer;
  WINBOOL=boolean;
  BOOL=boolean;
  PBOOL=^BOOL;
  LPSTR=pchar;
  LPTSTR=pwidechar;
  HANDLE=integer;
  HDC=HANDLE;
  HWND=HANDLE;
  HICON=HANDLE;
  UINT=UInt32;
  PSYSTEM_LOGICAL_PROCESSOR_INFORMATION=pointer;
  TFNThreadStartRoutine=pointer;
  TThreadEntry32=record end;
  THeapList32=record end;
  TDebugEvent=record end;
  TFloatingSaveArea=record end;
  TMemoryBasicInformation=record
    BaseAddress: PVOID;
    AllocationBase: PVOID;
    AllocationProtect: ULONG;
    RegionSize: ULONG;
    State: ULONG;
    Protect: ULONG;
    _Type: ULONG;
  end;
  _MEMORYSTATUS=record end;

const
 MAX_PATH=256;
 MAX_MODULE_NAME32=256;
 CONTEXT_i386=$10000;
 PAGE_READONLY = 2;
 PAGE_READWRITE = 4;
 PAGE_WRITECOPY = 8;
 PAGE_EXECUTE = 16;
 PAGE_EXECUTE_READ = 32;
 PAGE_EXECUTE_READWRITE = 64;
 PAGE_EXECUTE_WRITECOPY = 128;
 PAGE_GUARD = 256;
 PAGE_NOACCESS = 1;
 PAGE_NOCACHE = 512;
 MEM_COMMIT = 4096;
 MEM_FREE = 65536;
 MEM_RESERVE = 8192;
 MEM_IMAGE = 16777216;
 MEM_MAPPED = 262144;
 MEM_PRIVATE = 131072;
 MEM_DECOMMIT = 16384;
 MEM_RELEASE = 32768;
 MEM_TOP_DOWN = 1048576;
 MEM_RESET        = $80000;
 MEM_WRITE_WATCH  = $200000;
 MEM_PHYSICAL     = $400000;
 MEM_LARGE_PAGES  = $20000000;
 MEM_4MB_PAGES    = dword($80000000);


 INVALID_SOCKET = THandle(not(0));
 INVALID_HANDLE_VALUE = -1;

  TH32CS_SNAPHEAPLIST = $00000001;
  TH32CS_SNAPPROCESS  = $00000002;
  TH32CS_SNAPTHREAD   = $00000004;
  TH32CS_SNAPMODULE   = $00000008;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
                        TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
  TH32CS_GETALLMODS   = $80000000;

  PROCESS_ALL_ACCESS = $1f0fff; //not really used


procedure ZeroMemory(destination: pointer; size: integer);
procedure CopyMemory(destination: pointer; Origin: pointer; size: integer);
procedure MoveMemory(destination: pointer; Origin: pointer; size: integer);
function QueryPerformanceFrequency(var tps: TLargeInteger): boolean;
function QueryPerformanceCounter(var currenttick: TLargeInteger): boolean;

procedure log(l: string);


{$ifndef cefuncproc}
//for now...
function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;
function InRangeQ(const AValue, AMin, AMax: QWord): Boolean;inline;


{$endif}

implementation

function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;

function InRangeQ(const AValue, AMin, AMax: QWord): Boolean;inline;
begin
  Result:=(AValue>=AMin) and (AValue<=AMax);
end;

function QueryPerformanceFrequency(var tps: TLargeInteger): boolean;
begin
  tps:=1000;
  result:=true;
end;

function QueryPerformanceCounter(var currenttick: TLargeInteger): boolean;
begin
  currenttick:=GetTickCount64;
  result:=true;
end;

procedure ZeroMemory(destination: pointer; size: integer);
begin
  //log('ZeroMemory');
  FillByte(destination^, size, 0); //perhaps filldword might be faster, but for now keep it compatible
  //log('ZeroMemory returned');
end;

procedure MoveMemory(destination: pointer; Origin: pointer; size: integer);
begin
  Move(Origin^, Destination^, size);
end;

procedure CopyMemory(destination: pointer; Origin: pointer; size: integer);
begin
  MoveMemory(destination, origin, size);
end;

procedure log(l: string);
begin
{$ifdef android}
  __android_log_write(ANDROID_LOG_ERROR, 'CECORE',pchar(l));
{$endif}

{$ifdef windows}
  Outputdebugstring(pchar(l));
{$endif}

end;

end.

