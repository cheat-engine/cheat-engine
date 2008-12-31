unit packedfilter;

interface

uses windows, winsock;

type Tsend=function(s: integer; buf: pchar; len: integer; flags: integer): integer; stdcall;
type Tsendto=function(s: integer; buf: pchar; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;


function ws2send(s: integer; buf: pchar; len: integer; flags: integer): integer; stdcall;
function ws2sendto(s: integer; buf: pchar; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;


var
  ws2sendorig: Tsend;
  ws2sendtoorig: Tsendto;

implementation

uses mainunit;

function ws2send(s: integer; buf: pchar; len: integer; flags: integer): integer; stdcall;
begin
  result:=ws2sendorig(s,buf,len,flags);
  if result>0 then
  begin

  end;
end;

function ws2sendto(s: integer; buf: pchar; len: integer; flags: Integer; var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
begin
  result:=ws2sendtoorig(s,buf,len,flags,addrto, tolen);
end;

end.
