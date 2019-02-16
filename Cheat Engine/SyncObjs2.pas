unit SyncObjs2;

{$MODE Delphi}

interface

uses windows, classes, sysutils, LCLIntf;

type TSemaphore=class
  private
    h: THandle;
    max: integer;
  public
    function TryAcquire: boolean;
    procedure Acquire;
    function Release(count:integer=1):integer;
    constructor create(maxcount: integer; init0:boolean=false);
    destructor destroy;  override;
end;

implementation


constructor TSemaphore.create(maxcount:integer; init0: boolean=false);
var init: integer;
begin
  max:=maxcount;
  if init0 then
    init:=0
  else
    init:=maxcount;

  h:=CreateSemaphore(nil,init,maxcount,nil);
end;

destructor TSemaphore.destroy;
begin
  closehandle(h);
  inherited destroy;
end;

procedure TSemaphore.Acquire;
begin
  waitforsingleobject(h,infinite);
end;

function TSemaphore.TryAcquire:boolean;
begin
  result:=waitforsingleobject(h,0)=WAIT_OBJECT_0;
end;

function TSemaphore.Release(count: integer=1): integer;
var
  previouscount: LONG;
  e: integer;
begin
  if releasesemaphore(h,count,@previouscount) then
    result:=previouscount
  else
    result:=-1;
end;

end.








