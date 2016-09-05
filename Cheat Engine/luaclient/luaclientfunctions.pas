unit luaclientfunctions;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, syncobjs;

function CELUA_Initialize(pipename: pchar): BOOL; stdcall;
function CELUA_ExecuteFunction(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;
function CELUA_ExecuteFunctionAsync(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;

implementation

var
  pipe: THandle=INVALID_HANDLE_VALUE;
  cs: TCriticalSection;

procedure CELUA_Error;
begin
  closehandle(pipe);
  pipe:=INVALID_HANDLE_VALUE;
end;

function CELUA_ExecuteFunction_Internal(script: pchar; parameters: UINT_PTR; async: boolean=false): UINT_PTR;
var
  command: byte;
  bw: dword;
  l: integer;
  p: qword; //biggest common denominator
  r: qword;
begin
  bw:=0;
  r:=0;
  cs.enter;
  try

    if pipe<>INVALID_HANDLE_VALUE then
    begin
      if async then
        command:=4 //async
      else
        command:=1; //sync

      if writefile(pipe, command,sizeof(command), bw, nil) then
      begin
        l:=strlen(script);
        if writefile(pipe, l, sizeof(l), bw, nil) then
        begin
          if writefile(pipe, script^, l, bw, nil) then
          begin
            p:=parameters;
            if writefile(pipe, p, sizeof(p), bw, nil) then
            begin
              //get the result (also 8 bytes, even for a 32-bit app)
              if ReadFile(pipe, r, sizeof(r), bw, nil) then
                result:=r
              else
                CELUA_Error;

            end;
          end
          else
            CELUA_Error;
        end
        else
          CELUA_Error;

      end
      else
        CELUA_Error;
    end;

  finally
    cs.leave;
  end;
end;

function CELUA_ExecuteFunction(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;
begin
  result:=CELUA_ExecuteFunction_Internal(script, parameters, false);
end;

function CELUA_ExecuteFunctionAsync(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;
begin
  result:=CELUA_ExecuteFunction_Internal(script, parameters, true);
end;


function CELUA_Initialize(pipename: pchar): BOOL; stdcall;
begin
  if cs=nil then
    cs:=TCriticalSection.create;

  cs.enter;
  try
    if pipe<>INVALID_HANDLE_VALUE then
      closehandle(pipe);

    pipe:=CreateFile(pchar('\\.\pipe\'+pipename), GENERIC_READ or GENERIC_WRITE, 0, nil,  OPEN_EXISTING, 0, 0);
    result:=pipe<>INVALID_HANDLE_VALUE;
  finally
    cs.leave;
  end;
end;

end.

