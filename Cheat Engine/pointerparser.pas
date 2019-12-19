unit pointerparser;

{$mode delphi}

interface

uses
  Classes, SysUtils;

function getPointerAddress(address: ptruint; const offsets: array of integer; var hasError: boolean): ptruint;

implementation

{$ifdef windows}
uses newkernelhandler, ProcessHandlerUnit;
{$endif}

{$ifdef darwin}
uses macport,newkernelhandler, ProcessHandlerUnit;
{$endif}

{$ifdef jni}
uses unixporthelper, newkernelhandler, ProcessHandlerUnit;
{$endif}

function getPointerAddress(address: ptruint; const offsets: array of integer; var hasError: boolean): ptruint;
var realaddress, realaddress2: PtrUInt;
    count: PtrUInt;
    check: boolean;
    i: integer;
begin
  realaddress2:=address;
  for i:=length(offsets)-1 downto 0 do
  begin
    realaddress:=0;
    check:=readprocessmemory(processhandle,pointer(realaddress2),@realaddress,processhandler.pointersize,count);
    if check and (count=processhandler.pointersize) then
      realaddress2:=realaddress+offsets[i]
    else
    begin
      result:=0;

      exit;
    end;
  end;

  result:=realAddress2;
  hasError:=false;
end;

end.

