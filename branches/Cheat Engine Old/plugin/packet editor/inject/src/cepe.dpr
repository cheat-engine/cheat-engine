library cepe;

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
  mainunit in 'mainunit.pas' {mainform},
  packetfilter in 'packetfilter.pas',
  hexedit in 'hexedit.pas',
  filterform in 'filterform.pas' {frmFilter};

{$R *.res}

exports ws2send;
exports ws2sendorig;
exports ws2sendto;
exports ws2sendtoorig;
exports ws2recv;
exports ws2recvorig;
exports ws2recvfrom;
exports ws2recvfromorig;
exports ws2WSAsend;
exports ws2WSAsendorig;
exports ws2WSAsendto;
exports ws2WSAsendtoorig;
exports ws2WSArecv;
exports ws2WSArecvorig;
exports ws2WSArecvfrom;
exports ws2WSArecvfromorig;


begin
  loadlibrary('ws2_32.dll');
end.
