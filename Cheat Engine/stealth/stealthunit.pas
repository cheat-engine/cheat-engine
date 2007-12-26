unit stealthunit;

interface

uses windows,globals,sysutils,tlhelp32,psapi;

//type TIsDebuggerPresent=function:boolean; stdcall;

var EnumProcessesInfo:TAPIInfo;
    EnumThreadWindowsInfo: TAPIInfo;
    EnumWindowsInfo:TAPIInfo;
    FindwindowAInfo:TAPIInfo;
    FindWindowWInfo:TAPIInfo;
    GetWindowInfo: TAPIInfo;
    GetWindowTextAInfo: TAPIInfo;
    GetWindowTextWInfo: TAPIInfo;
    GetNextWindowInfo: TAPIInfo;
    IsDebuggerPresentInfo: TAPIInfo;
    Process32FirstInfo:TAPIInfo;
    Process32FirstWInfo:TAPIInfo;
    Process32NextInfo:TAPIInfo;
    Process32NextWInfo:TAPIInfo;


    alreadystealth: boolean;

//    IsDebuggerPresent: TIsDebuggerPresent;
    executablebuffer: pointer;


procedure InitializeStealth;

function EnumProcesses_Hook(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL stdcall;
function EnumThreadWindows_Hook(dwThreadId:DWORD; lpfn: pointer; lParam:LPARAM):BOOL; stdcall;
function EnumWindows_Hook(lpEnumFunc: pointer; lParam: LPARAM): BOOL; stdcall;
function FindWindowA_hook(lpClassName, lpWindowName: PAnsiChar): HWND; stdcall;
function FindWindowW_hook(lpClassName, lpWindowName: PWideChar): HWND; stdcall;
function GetNextWindow_Hook(hwnd:HWND;wcmd:UINT):HWND; stdcall;
function GetWindowTextA_hook(hwnd:HWND; lpString:PAnsiChar; nMaxCount:integer):integer; stdcall;
function GetWindowTextW_hook(hwnd:HWND; lpString:PWideChar; nMaxCount:integer):integer; stdcall;
function GetWindow_hook(h:HWND; cmd:UINT):HWND; stdcall;
function IsDebuggerPresent_Hook:boolean; stdcall;
function Process32First_hook(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
function Process32FirstW_hook(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL; stdcall;
function Process32Next_hook(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
function Process32NextW_hook(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL; stdcall;

implementation

{
prototype:

functioncall
jmp myfunction
nop's if needed
..
..
..



oldfunction:
[originalcode]
jmp functioncall+5

myfunction (sameparams as hooked api)
begin
  //do your stuff and checking here
  //if you want to call the unhooked function call oldfunction(params)

end;

}


function EnumProcesses_Hook(lpidProcess: LPDWORD; cb: DWORD; var cbNeeded: DWORD): BOOL stdcall;
var p,p2: LPDWORD;
    i,j: integer;
begin
  asm
    push esi
    push edi
    lea esi,EnumProcessesInfo.original[0]
    mov edi,EnumProcessesInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=EnumProcesses(lpidProcess,cb,cbNeeded);

  p:=lpidProcess;
  for i:=0 to (cbneeded div 4)-1 do
  begin
    if p^=scansettings.CEProcessID then
    begin
      //found and remove it from the list
      p2:=p;
      inc(p2);
      for j:=i to (cbneeded div 4)-2 do
      begin
        p^:=p2^;
        inc(p);
        inc(p2);
      end;

      p^:=0;

      dec(cbNeeded,4);
      break;
    end;

    inc(p);
  end;

  asm
    push esi
    push edi
    lea esi,EnumProcessesInfo.jump[0]
    mov edi,EnumProcessesInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;


type tenumw=function (hwnd:HWND; lParam: LPARAM):BOOL; stdcall;

type Tenumwindowsstruct=record
  lparam:LPARAM;
  lpEnumFunc: tenumw;
end;

function EnumWindows2_Hook(hwnd:HWND; lParam: LPARAM): BOOL; stdcall;
var x:^Tenumwindowsstruct;
    winprocesS:dword;
begin
  result:=true;
  x:=pointer(lParam);


  GetWindowThreadProcessId(hwnd,@winprocess);
  //ignore it if it is one of the protected windows
  if not ((hwnd=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID))) then
    result:=x^.lpEnumFunc(hwnd,x.lparam);
end;

function EnumWindows_Hook(lpEnumFunc: pointer; lParam: LPARAM): BOOL; stdcall;
var x: Tenumwindowsstruct;
begin
  asm
    push esi
    push edi
    lea esi,EnumWindowsInfo.original[0]
    mov edi,EnumWindowsInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  x.lparam:=lparam;
  x.lpEnumFunc:=lpenumfunc;
  result:=EnumWindows(@EnumWindows2_Hook,dword(@x));
  asm
    push esi
    push edi
    lea esi,EnumWindowsInfo.jump[0]
    mov edi,EnumWindowsInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function EnumThreadWindows_Hook(dwThreadId:DWORD; lpfn:pointer; lParam:LPARAM):BOOL; stdcall;
begin
  asm
    push esi
    push edi
    lea esi,EnumThreadWindowsInfo.original[0]
    mov edi,EnumThreadWindowsInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  if (dwThreadID=hyperscanthreadid) or (dwthreadid=scansettings.CEMainThreadID) then
    result:=false
  else
    result:=EnumThreadWindows(dwThreadID,lpfn,lParam);

  asm
    push esi
    push edi
    lea esi,EnumThreadWindowsInfo.jump[0]
    mov edi,EnumThreadWindowsInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function FindWindowA_hook(lpClassName, lpWindowName: PAnsiChar): HWND; stdcall;
var winprocesS:dword;
begin
  asm
    push esi
    push edi
    lea esi,FindWindowAInfo.original[0]
    mov edi,FindWindowAInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=FindWindowA(lpClassName,lpWindowName);
  GetWindowThreadProcessID(result,@winprocess);

  if (result=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID)) then
    result:=0;

  asm
    push esi
    push edi
    lea esi,FindWindowAInfo.jump[0]
    mov edi,FindWindowAInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function FindWindowW_hook(lpClassName, lpWindowName: PWideChar): HWND; stdcall;
var winprocesS:dword;
begin
  asm
    push esi
    push edi
    lea esi,FindWindowWInfo.original[0]
    mov edi,FindWindowWInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=FindWindowW(lpClassName,lpWindowName);
  GetWindowThreadProcessID(result,@winprocess);

  if (result=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID)) then
    result:=0;

  asm
    push esi
    push edi
    lea esi,FindWindowWInfo.jump[0]
    mov edi,FindWindowWInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;


function GetNextWindow_Hook(hwnd:HWND;wcmd:UINT):HWND; stdcall;
var winprocess:dword;
begin
  asm
    push esi
    push edi
    lea esi,GetNextWindowInfo.original[0]
    mov edi,GetNextWindowInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=GetNextWindow(hwnd,wcmd);

  GetWindowThreadProcessId(result,@winprocess);
  while (result<>0) and (result=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID)) do
  begin
    result:=GetNextWindow(result,wcmd);
    GetWindowThreadProcessId(result,@winprocess);
  end;

  asm
    push esi
    push edi
    lea esi,GetNextWindowInfo.jump[0]
    mov edi,GetNextWindowInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function GetWindowTextA_hook(hwnd:HWND; lpString:PAnsiChar; nMaxCount:integer):integer; stdcall;
var winprocess: dword;
begin
  asm
    push esi
    push edi
    lea esi,GetWindowTextAInfo.original[0]
    mov edi,GetWindowTextAInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
 // GetWindowThreadProcessId(hwnd,@winprocess);
//  if (winprocess=0) or ((hwnd=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID))) then
//    result:=0
//  else
    result:=GetWindowTextA(hwnd,lpString,nMaxCount);

  asm
    push esi
    push edi
    lea esi,GetWindowTextAInfo.jump[0]
    mov edi,GetWindowTextAInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function GetWindowTextW_hook(hwnd:HWND; lpString:PWideChar; nMaxCount:integer):integer; stdcall;
var winprocess: dword;
    i: integer;
begin
  asm
    push esi
    push edi
    lea esi,GetWindowTextWInfo.original[0]
    mov edi,GetWindowTextWInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  GetWindowThreadProcessId(hwnd,@winprocess);
  if (winprocess=0) or ((hwnd=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID))) then
  begin
    for i:=0 to nmaxcount-1 do
      lpString[i]:=#0;
    result:=0;
  end else result:=GetWindowTextW(hwnd,lpString,nMaxCount);

  asm
    push esi
    push edi
    lea esi,GetWindowTextWInfo.jump[0]
    mov edi,GetWindowTextWInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;


function GetWindow_hook(h:HWND; cmd:UINT):HWND; stdcall;
var winprocess: dword;
    bug: dword;
begin
  asm
    push esi
    push edi
    lea esi,GetWindowInfo.original[0]
    mov edi,GetWindowInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;

  result:=GetWindow(h,cmd);
  winprocess:=0;
  GetWindowThreadProcessId(result,@winprocess);

 // messagebox(0,pchar('winprocess='+IntToHex(winprocess,8)),pchar('winprocess='+IntToHex(scansettings.CEProcessID,8)),mb_ok);

  bug:=0;
  while (bug<1000) and (result<>0) and ((winprocess=0) or ((result=scansettings.hyperscanwindow) or ((scansettings.CEProcessID<>0) and (winprocess=scansettings.CEProcessID)))) do
  begin
    inc(bug);

    case cmd of
      GW_HWNDFIRST,GW_HWNDNEXT:
      begin
        result:=getwindow(result,GW_HWNDNEXT);
        winprocess:=0;
        GetWindowThreadProcessId(result,@winprocess);
      end;

      else result:=0;
    end;
  end;


  asm
    push esi
    push edi
    lea esi,GetWindowInfo.jump[0]
    mov edi,GetWindowInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function IsDebuggerPresent_Hook:boolean; stdcall;
begin
  result:=false;
end;


function Process32First_hook(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
begin
  asm
    push esi
    push edi
    lea esi,Process32FirstInfo.original[0]
    mov edi,Process32FirstInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=Process32First(hSnapshot,lppe);
  if lppe.th32ProcessID=scansettings.CEProcessID then
    result:=process32next(hsnapshot,lppe);

  asm
    push esi
    push edi
    lea esi,Process32FirstInfo.jump[0]
    mov edi,Process32FirstInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function Process32FirstW_hook(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL; stdcall;
begin
  asm
    push esi
    push edi
    lea esi,Process32FirstWInfo.original[0]
    mov edi,Process32FirstWInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=Process32FirstW(hSnapshot,lppe);
  if lppe.th32ProcessID=scansettings.CEProcessID then
    result:=process32nextw(hsnapshot,lppe);

  asm
    push esi
    push edi
    lea esi,Process32FirstWInfo.jump[0]
    mov edi,Process32FirstWInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function Process32Next_hook(hSnapshot: THandle; var lppe: TProcessEntry32): BOOL; stdcall;
begin
  asm
    push esi
    push edi
    lea esi,Process32NextInfo.original[0]
    mov edi,Process32NextInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=Process32Next(hSnapshot,lppe);
  if lppe.th32ProcessID=scansettings.CEProcessID then
    result:=process32next(hsnapshot,lppe);

  asm
    push esi
    push edi
    lea esi,Process32NextInfo.jump[0]
    mov edi,Process32NextInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;

function Process32NextW_hook(hSnapshot: THandle; var lppe: TProcessEntry32W): BOOL; stdcall;
begin
  asm
    push esi
    push edi
    lea esi,Process32NextWInfo.original[0]
    mov edi,Process32NextWInfo.location
    movsd
    movsb

    pop edi
    pop esi
  end;
  result:=Process32NextW(hSnapshot,lppe);
  if lppe.th32ProcessID=scansettings.CEProcessID then
    result:=process32nextW(hsnapshot,lppe);

  asm
    push esi
    push edi
    lea esi,Process32NextWInfo.jump[0]
    mov edi,Process32NextWInfo.location
    movsd
    movsb
    pop edi
    pop esi
  end;
end;


//------------------------------------------------------------------------
procedure InitializeStealth;
var user32dll,kernel32dll,psapidll: THandle;
    op:dword;
begin
  outputdebugstring('InitializeStealth got called');

  //new method test
  user32dll:=loadlibrary('user32.dll');
  if user32dll<>0 then
  begin
    GetWindowTextAInfo.location:=GetProcAddress(user32dll,'GetWindowTextA');
    if VirtualProtect(GetWindowTextAInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetWindowTextAInfo.jump[0]:=$e9;
      pdword(@GetWindowTextAInfo.jump[1])^:=dword(@GetWindowTextA_Hook)-dword(GetWindowTextAInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetWindowTextAInfo.original[0]
          mov esi,GetWindowTextAInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetWindowTextAInfo.jump[0]
          mov edi,GetWindowTextAInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;
  end;



  if scansettings.CEProcessID=getcurrentprocessid then exit;
  if alreadystealth then exit;
  alreadystealth:=true;

  psapidll:=loadlibrary('psapi.dll');
  if psapidll<>0 then
  begin
    EnumProcessesInfo.location:=GetProcAddress(psapidll,'EnumProcesses');
    if VirtualProtect(EnumProcessesInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      EnumProcessesInfo.jump[0]:=$e9;
      pdword(@EnumProcessesInfo.jump[1])^:=dword(@EnumProcesses_Hook)-dword(EnumProcessesInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,EnumProcessesInfo.original[0]
          mov esi,EnumProcessesInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,EnumProcessesInfo.jump[0]
          mov edi,EnumProcessesInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;
  end;

  kernel32dll:=loadlibrary('kernel32.dll');
  if kernel32dll<>0 then
  begin
//    @IsDebuggerPresent:=GetProcAddress(kernel32dll,'IsDebuggerPresent');
    IsDebuggerPresentInfo.location:=GetProcAddress(kernel32dll,'IsDebuggerPresent');
    if VirtualProtect(IsDebuggerPresentInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      IsDebuggerPresentInfo.jump[0]:=$e9;
      pdword(@IsDebuggerPresentInfo.jump[1])^:=dword(@IsDebuggerPresent_Hook)-dword(IsDebuggerPresentInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,IsDebuggerPresentInfo.original[0]
          mov esi,IsDebuggerPresentInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,IsDebuggerPresentInfo.jump[0]
          mov edi,IsDebuggerPresentInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except
        outputdebugstring(' failed to hook isdebuggerpresent');
      end;
    end;



    Process32FirstInfo.location:=GetProcAddress(kernel32dll,'Process32First');
    if VirtualProtect(Process32FirstInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Process32FirstInfo.jump[0]:=$e9;
      pdword(@Process32FirstInfo.jump[1])^:=dword(@Process32First_Hook)-dword(Process32FirstInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,Process32FirstInfo.original[0]
          mov esi,Process32FirstInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,Process32FirstInfo.jump[0]
          mov edi,Process32FirstInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    Process32FirstWInfo.location:=GetProcAddress(kernel32dll,'Process32FirstW');
    if VirtualProtect(Process32FirstWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Process32FirstWInfo.jump[0]:=$e9;
      pdword(@Process32FirstWInfo.jump[1])^:=dword(@Process32FirstW_Hook)-dword(Process32FirstWInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,Process32FirstWInfo.original[0]
          mov esi,Process32FirstWInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,Process32FirstWInfo.jump[0]
          mov edi,Process32FirstWInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    Process32NextInfo.location:=GetProcAddress(kernel32dll,'Process32Next');
    if VirtualProtect(Process32NextInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Process32NextInfo.jump[0]:=$e9;
      pdword(@Process32NextInfo.jump[1])^:=dword(@Process32Next_Hook)-dword(Process32NextInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,Process32NextInfo.original[0]
          mov esi,Process32NextInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,Process32NextInfo.jump[0]
          mov edi,Process32NextInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    Process32NextWInfo.location:=GetProcAddress(kernel32dll,'Process32NextW');
    if VirtualProtect(Process32NextWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      Process32NextWInfo.jump[0]:=$e9;
      pdword(@Process32NextWInfo.jump[1])^:=dword(@Process32NextW_Hook)-dword(Process32NextWInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,Process32NextWInfo.original[0]
          mov esi,Process32NextWInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,Process32NextWInfo.jump[0]
          mov edi,Process32NextWInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;


  end;


  user32dll:=Loadlibrary('user32.dll');
  if user32dll<>0 then
  begin
    EnumWindowsInfo.location:=GetProcAddress(user32dll,'EnumWindows');
    if VirtualProtect(EnumWindowsInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      EnumWindowsInfo.jump[0]:=$e9;
      pdword(@EnumWindowsInfo.jump[1])^:=dword(@EnumWindows_Hook)-dword(EnumWindowsInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,EnumWindowsInfo.original[0]
          mov esi,EnumWindowsInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,EnumWindowsInfo.jump[0]
          mov edi,EnumWindowsInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;


    EnumThreadWindowsInfo.location:=GetProcAddress(user32dll,'EnumThreadWindows');
    if VirtualProtect(EnumThreadWindowsInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      EnumThreadWindowsInfo.jump[0]:=$e9;
      pdword(@EnumThreadWindowsInfo.jump[1])^:=dword(@EnumThreadWindows_Hook)-dword(EnumThreadWindowsInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,EnumThreadWindowsInfo.original[0]
          mov esi,EnumThreadWindowsInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,EnumThreadWindowsInfo.jump[0]
          mov edi,EnumThreadWindowsInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;


    FindWindowAInfo.location:=GetProcAddress(user32dll,'FindWindowA');
    if VirtualProtect(FindWindowAInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      FindWindowAInfo.jump[0]:=$e9;
      pdword(@FindWindowAInfo.jump[1])^:=dword(@FindWindowA_Hook)-dword(FindWindowAInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,FindWindowAInfo.original[0]
          mov esi,FindWindowAInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,FindWindowAInfo.jump[0]
          mov edi,FindWindowAInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    FindWindowWInfo.location:=GetProcAddress(user32dll,'FindWindowW');
    if VirtualProtect(FindWindowWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      FindWindowWInfo.jump[0]:=$e9;
      pdword(@FindWindowWInfo.jump[1])^:=dword(@FindWindowW_Hook)-dword(FindWindowWInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,FindWindowWInfo.original[0]
          mov esi,FindWindowWInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,FindWindowWInfo.jump[0]
          mov edi,FindWindowWInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    
    GetNextWindowInfo.location:=GetProcAddress(user32dll,'GetNextWindow');
    if VirtualProtect(GetNextWindowInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetNextWindowInfo.jump[0]:=$e9;
      pdword(@GetNextWindowInfo.jump[1])^:=dword(@GetNextWindow_Hook)-dword(GetNextWindowInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetNextWindowInfo.original[0]
          mov esi,GetNextWindowInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetNextWindowInfo.jump[0]
          mov edi,GetNextWindowInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    
    GetWindowInfo.location:=GetProcAddress(user32dll,'GetWindow');
    if VirtualProtect(GetWindowInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetWindowInfo.jump[0]:=$e9;
      pdword(@GetWindowInfo.jump[1])^:=dword(@GetWindow_Hook)-dword(GetWindowInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetWindowInfo.original[0]
          mov esi,GetWindowInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetWindowInfo.jump[0]
          mov edi,GetWindowInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;

    {GetWindowTextAInfo.location:=GetProcAddress(user32dll,'GetWindowTextA');
    if VirtualProtect(GetWindowTextAInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetWindowTextAInfo.jump[0]:=$e9;
      pdword(@GetWindowTextAInfo.jump[1])^:=dword(@GetWindowTextA_Hook)-dword(GetWindowTextAInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetWindowTextAInfo.original[0]
          mov esi,GetWindowTextAInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetWindowTextAInfo.jump[0]
          mov edi,GetWindowTextAInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;
    {

    GetWindowTextWInfo.location:=GetProcAddress(user32dll,'GetWindowTextW');
    if VirtualProtect(GetWindowTextWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      GetWindowTextWInfo.jump[0]:=$e9;
      pdword(@GetWindowTextWInfo.jump[1])^:=dword(@GetWindowTextW_Hook)-dword(GetWindowTextWInfo.location)-5;

      try
        asm
          //store original
          push edi
          push esi
          lea edi,GetWindowTextWInfo.original[0]
          mov esi,GetWindowTextWInfo.location
          movsd
          movsb

          //replace with jump
          lea esi,GetWindowTextWInfo.jump[0]
          mov edi,GetWindowTextWInfo.location
          movsd
          movsb

          pop esi
          pop edi
        end;
      except

      end;
    end;  }



  end;

end;

procedure uninitializestealth;
var op: dword;
begin

 //put it all back to where it was
  if EnumProcessesInfo.location<>nil then
  begin
    if VirtualProtect(EnumProcessesInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,EnumProcessesInfo.original
          mov edi,EnumProcessesInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if EnumThreadWindowsInfo.location<>nil then
  begin
    if VirtualProtect(EnumThreadWindowsInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,EnumThreadWindowsInfo.original
          mov edi,EnumThreadWindowsInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if EnumWindowsInfo.location<>nil then
  begin
    if VirtualProtect(EnumWindowsInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,EnumWindowsInfo.original
          mov edi,EnumWindowsInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if FindwindowAInfo.location<>nil then
  begin
    if VirtualProtect(FindwindowAInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,FindwindowAInfo.original
          mov edi,FindwindowAInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if FindWindowWInfo.location<>nil then
  begin
    if VirtualProtect(FindWindowWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,FindWindowWInfo.original
          mov edi,FindWindowWInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;


  if GetWindowInfo.location<>nil then
  begin
    if VirtualProtect(GetWindowInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,GetWindowInfo.original
          mov edi,GetWindowInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if GetWindowTextAInfo.location<>nil then
  begin
    if VirtualProtect(GetWindowTextAInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,GetWindowTextAInfo.original
          mov edi,GetWindowTextAInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if GetWindowTextWInfo.location<>nil then
  begin
    if VirtualProtect(GetWindowTextWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,GetWindowTextWInfo.original
          mov edi,GetWindowTextWInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if GetNextWindowInfo.location<>nil then
  begin
    if VirtualProtect(GetNextWindowInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,GetNextWindowInfo.original
          mov edi,GetNextWindowInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if IsDebuggerPresentInfo.location<>nil then
  begin
    if VirtualProtect(IsDebuggerPresentInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,IsDebuggerPresentInfo.original
          mov edi,IsDebuggerPresentInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;


  if Process32FirstInfo.location<>nil then
  begin
    if VirtualProtect(Process32FirstInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,Process32FirstInfo.original
          mov edi,Process32FirstInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if Process32FirstWInfo.location<>nil then
  begin
    if VirtualProtect(Process32FirstWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,Process32FirstWInfo.original
          mov edi,Process32FirstWInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if Process32NextInfo.location<>nil then
  begin
    if VirtualProtect(Process32NextInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,Process32NextInfo.original
          mov edi,Process32NextInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;

  if Process32NextWInfo.location<>nil then
  begin
    if VirtualProtect(Process32NextWInfo.location,5,PAGE_EXECUTE_READWRITE,op) then
    begin
      try
        //replace with original
        asm
          lea esi,Process32NextWInfo.original
          mov edi,Process32NextWInfo.location
          movsd
          movsb
        end;
      except

      end;
    end;
  end;
end;

initialization
  InitializeStealth;

finalization
  UninitializeStealth;

end.
