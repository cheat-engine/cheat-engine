unit main;

{$mode objfpc}{$H+}

interface

{$ifndef cpu32}
//{$error The xmplayer can ONLY be used in 32-bit}
{$endif}

uses
  windows, Classes, SysUtils,dialogs, xmplayer_defines;

var pipe: THandle;

procedure StartListening;

implementation

procedure HandleLoadFileCommand;
var size: integer;
  f: pointer;
  x: dword;
begin
  if readfile(pipe, size, 4, x,nil) then
  begin
    getmem(f, size);
    if readfile(pipe, f^, size, x,nil) then
    begin
      writeln('Read file! size='+inttostr(size)+' x='+inttostr(x));

    end;
  end;
end;

procedure HandlePause;
begin
  writeln('pausing xm');
end;

procedure HandleResume;
begin
  writeln('resuming xm');

end;

procedure HandleStop;
begin
  writeln('stop');

end;

procedure StartListening;
var command: byte;
  actualread: dword;
  i: integer;

  pipename: string;

  loadedevent: Thandle;
begin
  if Paramcount=1 then
  begin
    pipename:=ParamStr(1) ;

    pipe:=CreateFile(pchar('\\.\pipe\'+pipename) , GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if pipe=INVALID_HANDLE_VALUE then
      WriteLn('fuck')
    else
    begin
      loadedevent:=OpenEvent(EVENT_MODIFY_STATE , false, pchar(pipename));
      if loadedevent<>0 then
        SetEvent(loadedevent);


      writeln('weee');
      actualread:=0;
      while ReadFile(pipe, command, 1, actualread, nil) do
      begin
        writeln('command='+inttostr(command));

        case command of
          XMPLAYER_PLAYXM: HandleLoadFileCommand;
          XMPLAYER_PAUSE: HandlePause;
          XMPLAYER_RESUME: HandleResume;
          XMPLAYER_STOP: HandleStop;
        end;

      end;
      closehandle(pipe);
    end;



  end
  else
    writeln('RAAAAGE!');

  writeln('the end');

  while true do sleep(1000);
end;

end.

