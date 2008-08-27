unit controlwindow;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmControlWindow = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

type TControlwindowThread=class(tthread)
  private
  public
    controlwindow: TfrmControlWindow;
    procedure execute; override;

end;

implementation

uses Unit1;

procedure TControlwindowThread.execute;
begin
  controlwindow:=TfrmControlWindow.Create(nil);
  controlwindow.showmodal;
  freeandnil(controlwindow);
end;

{$R *.dfm}

procedure TfrmControlWindow.Button1Click(Sender: TObject);
var x: pchar;
    s: tstringlist;
begin
  //inject the PE dll into the target process
  //execute script that'll hook the api's to the correct

  if ce_exported.InjectDLL('c:\svnx\cheat engine\plugin\packet editor\cepe\cepe.dll',nil) then
  begin
    //note for gg: this is a specific string, go fetch!
    getmem(x,1024);
    try
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
    finally
      freemem(x);
    end;

  end else MessageBox(0,'cepe.dll not injected','Erroc',MB_OK);
end;

end.
