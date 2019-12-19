unit unixporthelper;

//

{$mode delphi}

interface

uses
  Classes, SysUtils{$ifdef android},log{$endif}, BaseUnix;

//just fill in some basic info. Most of it won't be used for jni. It's mostly for some structures and function declaration/pointers
type
  TColor = DWord;

  PByteArray = ^TByteArray;
  TByteArray = Array[0..{$ifdef CPU16}32766{$else}32767{$endif}] of Byte;

  PWordarray = ^TWordArray;
  TWordArray = array[0..{$ifdef CPU16}16382{$else}16383{$endif}] of Word;

  TLargeInteger=QWORD;
  PUINT64=^UInt64;
  ULONG_PTR=ptruint;
  ULONG64=qword;
  ULONG32=dword;
  LONG=integer;
  ULONG=DWORD;

  pint=^integer;

  dword64=UInt64;
  PDWORD64=^dword64;
  LONG64=UInt64;

  PVOID=pointer;
  LPBYTE=^BYTE;
  LPVOID=pointer;
  SIZE_T=integer;
  LPLPVOID=^pointer;
  WINBOOL=boolean;
  BOOL=boolean;
  PBOOL=^BOOL;
  PSTR=pchar;
  LPSTR=pchar;
  LPTSTR=pwidechar;
  TCHAR=WCHAR;
  PWCHAR=^WCHAR;
  HANDLE=integer;
  HDC=HANDLE;
  HWND=HANDLE;
  HICON=HANDLE;
  HMODULE=HANDLE;
  PHMODULE=^HMODULE;

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
  PMemoryBasicInformation=^TMemoryBasicInformation;
  _MEMORYSTATUS=record end;


  //ce classes not implemented yet, but globally accessed
  TCustomProgressBar=class(Tobject)
  private
  public
    position: integer;
  end;



  TListViewItem=class(TObject)
  private
  public
    index: integer;
    procedure MakeVisible(s: boolean);
  end;

  TListViewItems=class(TStrings)
  public
    count: integer;
    function getItem(index: integer): TListViewItem;
  published
    property item[index:integer]: TListViewItem read getItem; default;
  end;

  TListView=class(TObject)
  private
  public
    itemindex: integer;
    items: TListViewItems;
    topitem: TListViewItem;
    VisibleRowCount: integer;
    ownerdata: boolean;
    procedure Refresh;
    procedure clear;
    constructor create(aowner: TObject);
    destructor destroy; override;
  published
  end;




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
  TH32CS_SNAPMODULE32 = 0;
  TH32CS_SNAPALL      = TH32CS_SNAPHEAPLIST or TH32CS_SNAPPROCESS or
                        TH32CS_SNAPTHREAD or TH32CS_SNAPMODULE;
  TH32CS_GETALLMODS   = $80000000;

  PROCESS_ALL_ACCESS = $1f0fff; //not really used

  clWindowText=0;

{$ifdef ANDROID}
function GetTempDir: string;
var tempdirOverride: string;
{$endif}

function VirtualAlloc(lpAddress:LPVOID; dwSize:PTRUINT; flAllocationType:DWORD; flProtect:DWORD):LPVOID;
function VirtualFree(lpAddress:LPVOID; dwSize:PTRUINT; dwFreeType:DWORD):WINBOOL;

function CopyFile(source: string; destination: string; failifdestinationExists: boolean=false): boolean;
Function DirectoryExistsUTF8 (Const Directory : RawByteString) : Boolean;
Function CreateDirUTF8(Const NewDir : UnicodeString) : Boolean;

function GetCurrentProcessID: SizeUInt;
procedure ZeroMemory(destination: pointer; size: integer);
procedure CopyMemory(destination: pointer; Origin: pointer; size: integer);
procedure MoveMemory(destination: pointer; Origin: pointer; size: integer);
function QueryPerformanceFrequency(var tps: TLargeInteger): boolean;
function QueryPerformanceCounter(var currenttick: TLargeInteger): boolean;

procedure log(l: string);


function WinCPToUTF8(s: string): string;
function UTF8ToUTF16(s: string): widestring;
function UTF16ToUTF8(s: widestring): string;


{$ifndef cefuncproc}
//for now...
function InRangeX(const AValue, AMin, AMax: ptrUint): Boolean;inline;
function InRangeQ(const AValue, AMin, AMax: QWord): Boolean;inline;


{$endif}

implementation

function UTF16ToUTF8(s: widestring): string;
begin
  result:=s;
end;

function UTF8ToUTF16(s: string): widestring;
begin
  result:=s;
end;

function WinCPToUTF8(s: string): string;
begin
  result:=AnsiToUtf8(s);
end;



function TListViewItems.getItem(index: integer): TListViewItem;
begin
  result:=nil;
end;

procedure TListViewItem.MakeVisible(s: boolean);
begin
  //
end;

procedure TListView.Refresh;
begin
  //
end;

procedure TListView.clear;
begin
  //
end;

constructor TListView.create(aowner: TObject);
begin
  items:=TListViewItems.Create;
  inherited create;
end;

destructor TListView.destroy;
begin
  if items<>nil then
    freeandnil(items);

  inherited destroy;
end;

{$ifdef ANDROID}
function GetTempDir: string;
begin
  result:=tempdirOverride;
end;

{$endif}

Function CreateDirUTF8(Const NewDir : UnicodeString) : Boolean;
begin
  result:=false;
end;

Function DirectoryExistsUTF8 (Const Directory : RawByteString) : Boolean;
begin
  result:=false;
end;

function CopyFile(source: string; destination: string; failifdestinationExists: boolean=false): boolean;
var src,dst: TFileStream;
begin
  Log('CopyFile. source='+source+' destination='+destination+'Fail if exists'+ BoolToStr(failifdestinationExists, 'yes','no'));

  result:=false;
  try
    src:=nil;
    dst:=nil;

    if not fileexists('"'+source+'"')=false then
    begin
      log('source does not exist');
      exit;
    end;

    if failifdestinationExists and fileexists(destination) then
    begin
      log('failifdestinationExists is true and the destination exists');
      exit;
    end;


    try
      src:=tfilestream.create(source, fmOpenRead);
      dst:=tfilestream.create(destination, fmCreate);
      dst.CopyFrom(src,0); //copy the whole file

      result:=true; //still here and no exception
      log('Copy successful')
    finally
      if src<>nil then
        freeandnil(src);

      if dst<>nil then
        freeandnil(dst);
    end;


    ;
  except
    on e: exception do
      log('Error during copy:'+e.message);
  end;
end;

function VirtualFree(lpAddress:LPVOID; dwSize:PTRUINT; dwFreeType:DWORD):WINBOOL;
begin
  Fpmunmap(lpAddress, dwSize);
end;

function VirtualAlloc(lpAddress:LPVOID; dwSize:PTRUINT; flAllocationType:DWORD; flProtect:DWORD):LPVOID;
var r: pointer;
begin
  log('Calling VirtualAlloc');
  result:=Fpmmap(lpAddress,dwsize, PROT_READ or PROT_WRITE or PROT_EXEC, MAP_PRIVATE or MAP_ANONYMOUS, 0,0 );

  log('After calling VirtualAlloc. Result='+inttohex(ptruint(result),8));

end;

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

function GetCurrentProcessID: SizeUInt;
begin
  result:=GetProcessID;
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

