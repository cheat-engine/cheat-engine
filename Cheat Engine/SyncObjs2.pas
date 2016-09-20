unit SyncObjs2;

{$MODE Delphi}

interface

uses windows, LCLIntf;

type TSemaphore=class
  private
    handle: THandle;
  public
    procedure Acquire;
    procedure Release;
    constructor create(maxcount: integer);
    destructor destroy;  override;
end;

implementation


constructor TSemaphore.create(maxcount:integer);
begin
  handle:=CreateSemaphore(nil,maxcount,maxcount,nil);
end;

destructor TSemaphore.destroy;
begin
  closehandle(handle);
  inherited destroy;
end;

procedure TSemaphore.Acquire;
begin
  waitforsingleobject(handle,infinite);
end;

procedure TSemaphore.Release;
var previouscount: dword;
begin
  releasesemaphore(handle,1,@previouscount);
end;

end.








