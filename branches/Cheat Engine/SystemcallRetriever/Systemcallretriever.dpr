program Systemcallretriever;

uses
  Forms,
  mainunit in 'mainunit.pas' {Form1},
  debugger in 'debugger.pas',
  NewKernelHandler in '..\NewKernelHandler.pas',
  Filehandler in '..\Filehandler.pas',
  resultwindowunit in 'resultwindowunit.pas' {resultwindow};

{$R ..\manifest.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(Tresultwindow, resultwindow);
  Application.Run;
end.
