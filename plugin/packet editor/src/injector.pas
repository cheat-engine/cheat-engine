unit injector;

interface

uses windows, sysutils, cepluginsdk;

procedure InjectPacketEditor; stdcall;

implementation

uses unit1;

procedure InjectPacketEditor; stdcall;
var x: pchar;
    s: string;
begin
  if ce_exported.OpenedProcessHandle^=0 then
  begin
    ce_exported.showmessage('Please select a process first');
    exit;
  end;

  getmem(x,1024);
  try
    if GetModuleFileName(hinstance,x,1024)=0 then
    begin
      ce_exported.showmessage('Failed finding out where I am');
      exit;
    end;

    s:=extractfilepath(x);
    s:=s+'inject\cepe.dll';
    if not fileexists(s) then
    begin
      ce_exported.showmessage(pchar('Failed finding '+s));
      exit;
    end;




    //valid processhandle, so:
    if ce_exported.InjectDLL(pchar(s),nil) then
    begin
      //note for gg: this is a specific string, go fetch!
      ce_exported.ce_generateAPIHookScript('ws2_32!send','cepe!ws2send','cepe!ws2sendorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!send hook error','cePE error',MB_OK);
        exit;
      end;

 

      ce_exported.ce_generateAPIHookScript('ws2_32!sendto','cepe!ws2sendto','cepe!ws2sendtoorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!sendto hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!recv','cepe!ws2recv','cepe!ws2recvorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!recv hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!recvfrom','cepe!ws2recvfrom','cepe!ws2recvfromorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!recvfrom hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!WSAsend','cepe!ws2WSAsend','cepe!ws2WSAsendorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!WSAsend hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!WSAsendto','cepe!ws2WSAsendto','cepe!ws2WSAsendtoorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!WSAsendto hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!WSArecv','cepe!ws2WSArecv','cepe!ws2WSArecvorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!WSArecv hook error','cePE error',MB_OK);
        exit;
      end;

      ce_exported.ce_generateAPIHookScript('ws2_32!WSArecvfrom','cepe!ws2WSArecvfrom','cepe!ws2WSArecvfromorig',x,1024);
      if not ce_exported.AutoAssemble(x) then
      begin
        messagebox(0,'ws2_32!WSArecvfrom hook error','cePE error',MB_OK);
        exit;
      end;

    end else MessageBox(0,'cepe.dll not injected','Erroc',MB_OK);

  finally
    freemem(x);
  end;

end;

end.
