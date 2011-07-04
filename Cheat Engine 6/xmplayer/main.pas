unit main;

{$mode objfpc}{$H+}

interface

{$ifndef cpu32}
{$error The xmplayer can ONLY be used in 32-bit}
{$endif}

uses
  windows, Classes, SysUtils,dialogs, xmplayer_defines, uFMOD;

var pipe: THandle;

  currentsong: pointer;

procedure StartListening;

implementation

procedure HandleLoadFileCommand;
var size: integer;
  x: dword;
begin
  if readfile(pipe, size, 4, x,nil) then
  begin
    uFMOD_StopSong;

    if currentsong<>nil then
      freemem(currentsong);

    getmem(currentsong, size);
    if readfile(pipe, currentsong^, size, x,nil) then
      uFMOD_PlaySong(currentsong, size, XM_MEMORY);
  end;
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
    if pipe<>INVALID_HANDLE_VALUE then
    begin
      loadedevent:=OpenEvent(EVENT_MODIFY_STATE , false, pchar(pipename));
      if loadedevent<>0 then
        SetEvent(loadedevent);


      //writeln('weee');
      actualread:=0;
      while ReadFile(pipe, command, 1, actualread, nil) do
      begin
        //writeln('command='+inttostr(command));

        case command of
          XMPLAYER_PLAYXM: HandleLoadFileCommand;
          XMPLAYER_PAUSE: uFMOD_Pause;
          XMPLAYER_RESUME: uFMOD_Resume;
          XMPLAYER_STOP: uFMOD_StopSong;
        end;

      end;
      closehandle(pipe);
    end;



  end else messagebox(0,'Please give a unique audio pipe id', 'Param error',MB_OK or MB_ICONERROR);
end;

end.

