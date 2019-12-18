unit circularBuffer;

{$MODE Delphi}

{
Circular buffer for only 1 writer and 1 reader thread
Do not use with multiple writers or multiple readers (or at least protect the read and write yourself then)
}

interface

uses
  {$ifdef darwin}macport,{$endif}
  {$ifdef windows}windows,{$endif}
  LCLIntf, syncobjs,classes, sysutils;

type TCircularObjectBuffer=class
  private
    buffer: array of TObject;
    ReadIndex: integer;
    WriteIndex: integer;
    maxTimeout: integer;
    hasData: Tevent;
    hasSpace: TEvent;
    function getCount: integer;
  public
    function Write(o: TObject): boolean;
    function Read: TObject;
    constructor create(InitialSize, maxTimeout: integer);
    destructor destroy; override;
    property count: integer read getCount;
end;

implementation

constructor TCircularObjectBuffer.create(InitialSize, maxTimeout: integer);
begin
  setlength(buffer,initialsize);
  ReadIndex:=0;
  WriteIndex:=0;
  HasData:=Tevent.create(nil,false,false,'');
  HasSpace:=Tevent.create(nil,false,true,'');
  self.maxTimeout:=MaxTimeout;
end;

destructor TCircularObjectBuffer.destroy;
begin
  setlength(buffer,0);
  if HasData<>nil then
    HasData.Free;

  if HasSpace<>nil then
    HasSpace.Free;
end;

function TCircularObjectBuffer.getCount: integer;
begin
  if ReadIndex=WriteIndex then
    result:=0
  else
  if ReadIndex<WriteIndex then
    result:=WriteIndex-ReadIndex
  else
    result:=(length(buffer)-ReadIndex)+WriteIndex;
end;

function TCircularObjectBuffer.Write(o: TObject): boolean;
var nextwriteindex: integer;
begin
  result:=false;
  nextwriteindex:=(WriteIndex+1) mod length(buffer);
  if (nextwriteindex=ReadIndex) then
  begin
    //buffer full
    sleep(5000);
    if (hasspace.WaitFor(maxTimeout)<>wrsignaled) then exit;
  end;

  buffer[WriteIndex]:=o;
  writeindex:=nextwriteindex;
  HasData.SetEvent;

  result:=true;
end;

function TCircularObjectBuffer.Read: TObject;
begin
  result:=nil;
  if (readindex=writeindex) and (hasdata.WaitFor(MaxTimeout)<>wrSignaled) then exit;

  result:=Buffer[ReadIndex];
  ReadIndex:=(readindex+1) mod length(buffer);

  HasSpace.SetEvent;
end;

end.
