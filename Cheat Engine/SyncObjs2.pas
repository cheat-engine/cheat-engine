unit SyncObjs2;

interface

uses windows;

type TSemaphore=class
  private
    handle: THandle;
  public
    procedure Aquire;
    procedure Release;
    constructor create(maxcount: integer);
    destructor destroy;  override;
end;

implementation


constructor TSemaphore.create(maxcount:integer);
begin
  handle:=CreateSemaphore(nil,maxcount,maxcount,'');
end;

destructor TSemaphore.destroy;
begin
  closehandle(handle);
  inherited destroy;
end;

procedure TSemaphore.Aquire;
begin
  waitforsingleobject(handle,infinite);
end;

procedure TSemaphore.Release;
var previouscount: dword;
begin
  releasesemaphore(handle,1,@previouscount);
end;

end.








