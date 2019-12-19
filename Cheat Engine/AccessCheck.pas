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

uses Globals;

resourcestring
  rsNoFileCreationRightsOrNoFileOverwriteRights = 'No file creation rights or no file overwrite rights';
  rsNoFileModificationRights = 'No file modification rights';
  rsNoFileDeletionRights = 'No file deletion rights';
  rsNoDeleteRights = 'No delete rights';
  rsButYouDoHaveModifyRights = 'But you do have modify rights';

procedure FileAccessTest;
var f: tfilestream;
begin
  try
    f:=TFilestream.Create(CheatEngineDir+'accesscheck.tmp', fmCreate);
    try
      f.WriteBuffer(rsNoDeleteRights+#13#10,18);
    finally
      f.Free;
    end;
  except
    raise exception.Create(rsNoFileCreationRightsOrNoFileOverwriteRights);
  end;

  try
    f:=TFilestream.Create(CheatEngineDir+'accesscheck.tmp', fmOpenReadWrite);
    try
      f.Seek(0,soFromEnd);
      f.WriteBuffer(rsButYouDoHaveModifyRights+#13#10,31);
    finally
      f.free;
    end;
  except
    raise exception.Create(rsNoFileModificationRights);
  end;

  if not deletefile(CheatEngineDir+'accesscheck.tmp') then
    raise exception.Create(rsNoFileDeletionRights);
  
end;

end.
