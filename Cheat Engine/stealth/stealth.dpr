library stealth;

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
  stealthunit in 'stealthunit.pas',
  globals in '..\CEHook\globals.pas';

{$R *.res}

function hook(code: integer; wParam: word; lParam: longword): longword; stdcall;
begin
  CallNextHookEx(h,code,wParam,lparam);  //call the next hook proc if there is one
  hook:=0;
end;
exports hook;


begin

end.
