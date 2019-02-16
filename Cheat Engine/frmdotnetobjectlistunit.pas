unit frmDotNetObjectListUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TfrmDotNetObjectList }

  TfrmDotNetObjectList = class(TForm)
    ListView1: TListView;
  private
    { private declarations }
  public
    { public declarations }
    procedure loadlist;
  end;

var
  frmDotNetObjectList: TfrmDotNetObjectList;

implementation

uses symbolhandler, DotNetPipe, maps;

{$R *.lfm}

type
  TLoadListThread=class(TThread)
  private
    flushcount: integer;
    e: array [0..9] of TDotNetObject;

    procedure flush;
  public
    procedure Execute; override;
  end;

var loadListThread: TLoadListThread;

procedure TLoadListThread.flush;    //synchronize routine
var
  i: integer;
  li: tlistitem;
begin
  for i:=0 to flushcount-1 do
  begin
    li:=frmDotNetObjectList.ListView1.Items.Add;
    li.caption:=inttohex(e[i].startaddress,8);
    li.subitems.add(e[i].classname);
  end;

  flushcount:=0;
end;

procedure TLoadListThread.execute;
var
  l: TDOTNETObjectList;
  li: TMapIterator;
  i: integer;
begin
  FreeOnTerminate:=true;
  try
    //get the list
    l:=symhandler.getdotnetobjectlist;

    if l<>nil then
    begin
      li:=TMapIterator.create(l);
      li.first;
      flushcount:=0;
      while not li.eom do
      begin
        //add to frmDotNetObjectList
        li.GetData(e[flushcount]);

        inc(flushcount);
        if flushcount>=10 then
          synchronize(flush);

        li.next;
      end;

      if flushcount>0 then
        synchronize(flush);

      symhandler.freedotnetobjectlist(l);
    end;
  finally
    loadListThread:=nil;
  end;
end;


procedure TfrmDotNetObjectList.loadlist;
begin
  if loadListThread=nil then
  begin
    listview1.Clear;
    TLoadListThread.create(false);
  end;
end;

end.

