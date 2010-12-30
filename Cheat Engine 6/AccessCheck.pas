unit AccessCheck;
{obsolete, all files are now in the user or temp folder}

{$MODE Delphi}

{
This unit will contain routines to be used for testing and verifying that ce has
the required access needed.

FileAccessTest is the main routine

}

interface

uses LCLIntf, SysUtils, classes, forms, CEFuncProc, NewKernelHandler;

procedure FileAccessTest;

implementation



procedure FileAccessTest;
var f: tfilestream;
begin
  try
    f:=TFilestream.Create(CheatEngineDir+'accesscheck.tmp', fmCreate);
    try
      f.WriteBuffer('No delete rights'#13#10,18);
    finally
      f.Free;
    end;
  except
    raise exception.Create('No file creation rights or no file overwrite rights');
  end;

  try
    f:=TFilestream.Create(CheatEngineDir+'accesscheck.tmp', fmOpenReadWrite);
    try
      f.Seek(0,soFromEnd);
      f.WriteBuffer('But you do have modify rights'#13#10,31);
    finally
      f.free;
    end;
  except
    raise exception.Create('No file modification rights');
  end;

  if not deletefile(CheatEngineDir+'accesscheck.tmp') then
    raise exception.Create('No file deletion rights');
  
end;

end.
