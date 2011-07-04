program CEServer;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CEFuncProc in '..\..\CEFuncProc.pas',
  ScanThread in 'ScanThread.pas',
  reinit in '..\..\reinit.pas',
  NewKernelHandler in '..\..\NewKernelHandler.pas',
  Debugger in '..\..\Debugger.pas',
  disassembler in '..\..\disassembler.pas',
  Assemblerunit in '..\..\Assemblerunit.pas',
  Filehandler in '..\..\Filehandler.pas',
  symbolhandler in '..\..\symbolhandler.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Cheat Engine 5.2 Server';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
