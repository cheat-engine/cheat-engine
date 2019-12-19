unit com;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, syncobjs, math;


type
  TServer=class(TThread) //server pipe for dealing with initialization
  private
    pipe: THandle;
  protected
    procedure execute; override;
  public
  end;

  TCEConnection=class
  private
    cs: TCriticalSection;
    pipe: THandle;
    connected: boolean;
  public
    function DoCommand(s: string): qword;
    procedure DoCommandMR(async: boolean; s: string; returncount: integer; results: PQword);
    constructor create;
    destructor destroy; override;
  end;

var
  CEConnection: TCEConnection;
  server: TServer;

procedure execute2(p: pointer); stdcall;

implementation

uses proc;

var terminated: boolean;
procedure execute2(p: pointer); stdcall;
var
  command: byte;
  x: DWORD;
  hwnd: qword;
  pa: qword;
  pipe: THandle;
begin
  while not terminated  do
  begin
    pipe:=CreateNamedPipe(pchar('\\.\pipe\CEWINHOOKC'+inttostr(GetProcessID)), PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 255, 16, 8192, 0, nil);

    if ConnectNamedPipe(pipe, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then
    begin
      //connected
      //send this pipe of to the handler and create a new pipe
      while not terminated do
      begin
        command:=0;
        readfile(pipe, command, sizeof(command), x, nil);
        case command of
          1: //get WNDPROC of hWND
          begin
            readfile(pipe, hwnd, sizeof(hwnd), x,nil);
            pa:=GetWindowLongPtr(hwnd, GWL_WNDPROC);
            writeFile(pipe, pa, sizeof(pa), x, nil);
          end;

          2: //set WNDPROC of hWND
          begin
            readfile(pipe, hwnd, sizeof(hwnd), x,nil);
            SetWindowLongPtrA(hwnd, GWL_WNDPROC, qword(@wp));
            pa:=1;
            WriteFile(pipe, pa,1,x,nil);
          end;

          3: //set specifically (usually restore)
          begin
            readfile(pipe, hwnd, sizeof(hwnd), x,nil);
            readfile(pipe, pa, sizeof(pa), x,nil);
            SetWindowLongPtrA(hwnd, GWL_WNDPROC, pa);
            pa:=1;
            WriteFile(pipe, pa,1,x,nil);
          end;

          4: //set async state
          begin
            async:=false;
            readfile(pipe, async, 1,x,nil);
            WriteFile(pipe, async,1,x,nil);
          end

          else
          begin
            closeHandle(pipe);
            break; //invalid command
          end;
        end;
      end;

    end
    else
    begin
      CloseHandle(pipe); //failure, try again
    end;
  end;
end;

procedure TServer.execute;
begin
  execute2(nil);
end;

//connection

constructor TCEConnection.create;
var
  pipename: string;
  starttime: qword;
begin
  cs:=TCriticalSection.Create;
  pipename:='CEWINHOOK'+inttostr(GetProcessID);
  starttime:=GetTickCount64;
  while GetTickCount64<starttime+5000 do
  begin
    pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if (pipe<>INVALID_HANDLE_VALUE) then
    begin
      Connected:=true;
      break; //open
    end
    else sleep(100);
  end;
end;

destructor TCEConnection.destroy;
begin
  closehandle(pipe);
  inherited destroy;
end;

procedure TCEConnection.DoCommandMR(async: boolean; s: string; returncount: integer; results: PQword);
var
  m: tmemorystream;
  r: qword=0;
  x: dword=0;
  i: integer;
begin
  if not connected then exit;

  m:=TMemoryStream.Create;
  m.writebyte(2); //execute lua function, with a variable paramcount and returncount
  m.writebyte(ifthen(async,1,0));
  m.WriteDWord(length(s));
  m.WriteBuffer(s[1],length(s));
  m.writeQword(0);
  m.writeByte(returncount);

  cs.Enter;
  WriteFile(pipe, m.Memory^, m.Size, x, nil);
  for i:=0 to returncount-1 do
    ReadFile(pipe, results[i], sizeof(QWORD), x, nil);

  cs.Leave;

  m.free;

end;

function TCEConnection.DoCommand(s: string): qword;
var
  m: tmemorystream;
  r: qword=0;
  x: dword=0;
begin
  if not connected then exit(0);

  m:=TMemoryStream.Create;
  m.writebyte(1); //execute lua script
  m.WriteDWord(length(s));
  m.WriteBuffer(s[1],length(s));
  m.writeQword(0);

  cs.Enter;
  WriteFile(pipe, m.Memory^, m.Size, x, nil);
  ReadFile(pipe, r, sizeof(r), x, nil);
  cs.Leave;
  result:=r;

  m.free;
end;

finalization
//  MessageBoxA(0,'BYE','BYE',0);
  terminated:=true;

end.

