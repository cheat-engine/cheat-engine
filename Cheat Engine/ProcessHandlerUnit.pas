unit ProcessHandlerUnit;
{
Will handle all process specific stuff like openening and closing a process
The ProcessHandler variable will be in cefuncproc, but a tabswitch to another
process will set it to the different tab's process
}

interface

uses windows;

type TProcessHandler=class
  private
  public
    processid: dword;
    processhandle: THandle;
    procedure Open;
end;

implementation

procedure TProcessHandler.Open;
begin

end;

end.

