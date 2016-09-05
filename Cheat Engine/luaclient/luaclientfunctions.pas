unit luaclientfunctions;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, syncobjs;

function CELUA_Initialize(pipename: pchar): BOOL; stdcall;
function CELUA_ExecuteFunction(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;
function CELUA_ExecuteFunctionAsync(script: pchar; parameters: UINT_PTR): UINT_PTR; stdcall;

function CELUA_GetFunctionReferenceFromName(functionname: pchar): integer; stdcall;
function CELUA_ExecuteFunctionByReference(ref: integer; paramcount: integer; AddressOfParameters: PPointer; async: BOOLEAN): UINT_PTR; stdcall;

implementation

var
  pipe: THandle=INVALID_HANDLE_VALUE;
  cs: TCriticalSection;

procedure CELUA_Error;
begin
  closehandle(pipe);
  pipe:=INVALID_HANDLE_VALUE;
end;

function CELUA_ExecuteFunctionByReference(ref: integer; paramcount: integer; AddressOfParameters: PPointer; async: BOOLEAN): UINT_PTR; stdcall;
type TParamType=(ptNil=0, ptBoolean=1, ptInt64=2, ptInt32=3, ptNumber=4, ptString=5, ptTable=6, ptUnknown=255);
var
  command: byte;
  bw: dword;
  a: byte;
  i: integer;
  valtype: TParamType;
  vtb: byte;
  returncount: byte;

  v: qword;
  d: double;
  stringlength: word;
  s: pchar;
begin
  result:=0;

  {$ifdef cpu32}
  valtype:=ptInt32;
  {$else}
  valtype:=ptInt64;
  {$endif}

  vtb:=byte(valtype);

  cs.enter;
  try
    if pipe<>INVALID_HANDLE_VALUE then
    begin
      command:=3;
      if writefile(pipe, command, sizeof(command), bw, nil) then

      if async then a:=1 else a:=0;
      if writefile(pipe, a, sizeof(a), bw, nil) then
      begin
        if writefile(pipe, ref, sizeof(ref), bw, nil) then
        begin
          if writefile(pipe, paramcount, 1, bw, nil) then
          begin
            for i:=0 to paramcount-1 do
            begin
              WriteFile(pipe, vtb, sizeof(vtb),bw,nil);
              WriteFile(pipe, AddressOfParameters[i],sizeof(pointer),bw,nil);
            end;

            returncount:=1;
            if writefile(pipe, returncount, sizeof(returncount),bw,nil) then
            begin
              //the lua function is being called now
              if readfile(pipe, returncount, sizeof(returncount), bw, nil) then
              begin
                if returncount>0 then
                begin
                  for i:=0 to returncount-1 do
                  begin
                    if readfile(pipe, vtb, sizeof(vtb), bw, nil) then
                    begin
                      case TParamType(vtb) of
                        ptNil, ptUnknown: exit(0);
                        ptBoolean:
                        begin
                          readfile(pipe, a, sizeof(a), bw, nil);
                          exit(a);
                        end;

                        ptInt64:
                        begin
                          readfile(pipe, v, sizeof(v), bw, nil);
                          exit(v);
                        end;

                        ptNumber:
                        begin
                          readfile(pipe, d, sizeof(d), bw, nil);
                          exit(trunc(d));
                        end;

                        ptString:
                        begin
                          readfile(pipe, stringlength, sizeof(stringlength), bw, nil);
                          getmem(s, stringlength+1);
                          readfile(pipe, s[0], stringlength, bw, nil);
                          s[stringlength]:=#0;
                          try
                            result:=StrToInt(s);
                          except
                          end;
                        end;

                      end;
                    end else
                    begin
                      CELUA_Error;
                      exit;
                    end;

                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      CELUA_Error;
    end;
  finally
    cs.leave;
  end;
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

function CELUA_GetFunctionReferenceFromName(functionname: pchar): integer; stdcall;
begin
  result:=CELUA_ExecuteFunction(pchar('return createRef('+functionname+')'), 0);
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

