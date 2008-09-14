unit htmlHelp;
{
This unit will hook the hlp pop-up and spawn the .CHM equivalent
}

interface

uses windows,forms, sysutils, dialogs, classes, D6OnHelpFix;


type THtmlHelp=class
  private
  public
    function helphandler(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
    constructor create;
  end;

var
  HtmlHelpA: function(hwndCaller: HWND; pszFile: PAnsiChar; uCommand: UInt; dwData: DWORD): HWND; stdcall;
  HtmlHelpW: function(hwndCaller: HWND; pszFile: PWideChar; uCommand: UInt; dwData: DWORD): HWND; stdcall;

implementation

var hhctrl: DWORD;

const
  HH_DISPLAY_TOPIC        = $0000;
  HH_HELP_CONTEXT         = $000F;
  HH_CLOSE_ALL            = $0012;

function THtmlHelp.helphandler(Command: Word; Data: Longint; var CallHelp: Boolean): Boolean;
var x: THandle;
begin
  callhelp:=false;
  result:=true;


  if command=HELP_CONTEXT then
    x:=HtmlHelpA(application.handle,pchar(ExtractFilePath(application.ExeName)+application.HelpFile),HH_HELP_CONTEXT,data);

end;

constructor THtmlHelp.create;
begin
  application.onhelp:=helphandler;
end;


initialization
  hhctrl:=LoadLibrary('hhctrl.ocx');
  if hhctrl<>0 then
  begin
    HtmlHelpA:=GetProcAddress(hhctrl,'HtmlHelpA');
    HtmlHelpW:=GetProcAddress(hhctrl,'HtmlHelpW');
  end;

  THtmlHelp.create;

end.
