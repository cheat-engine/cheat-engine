unit frmHeapsUnit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,{tlhelp32,}CEFuncProc, frmMemoryAllocHandlerUnit, LResources;

type TFillHeapList=class(tthread)
  private
    c: integer;
    list: array [0..63] of record
      address: dword;
      size: integer;
    end;
    expanded: boolean;
    procedure enumerateHeapList(const memreclist: PMemRecTableArray; level: integer);
    procedure updatelist;
  public
    node: ttreenode;
    procedure execute; override;
end;

type
  TfrmHeaps = class(TForm)
    Panel1: TPanel;
    Button1: TButton;
    ListView1: TListView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
  private
    { Private declarations }
    fillthread: TFillHeapList;
  public
    { Public declarations }
  end;

var
  frmHeaps: TfrmHeaps;

implementation


uses MemoryBrowserFormUnit;

procedure TFillHeapList.updatelist;
var i:integer;
  li: tlistitem;
begin
  if GetCurrentThreadID <> MainThreadID then
    exit;

  if frmheaps<>nil then
  begin
    with frmheaps do
    begin
      listview1.Items.BeginUpdate;
      for i:=0 to c-1 do
      begin
        li:=listview1.Items.Add;
        li.Caption:=inttohex(list[i].address,8);
        li.SubItems.Add(inttostr(list[i].size));
      end;

      listview1.Items.EndUpdate;
    end;
  end else terminate;

  c:=0;
end;

procedure TFillHeapList.enumerateHeapList(const memreclist: PMemRecTableArray; level: integer);
var i: integer;
begin
  if terminated then exit;
  
  if level=7 then
  begin
    //add all <>nil entries
    for i:=0 to 15 do
    begin
      if memreclist[i].memallocevent<>nil then
      begin
        list[c].address:=memreclist[i].memallocevent.BaseAddress;
        list[c].size:=memreclist[i].memallocevent.HookEvent.HeapAllocEvent.Size;
        inc(c);
        if c=64 then
        begin
          synchronize(updatelist);
          sleep(100);
        end;
      end;
    end;

  end
  else
  begin
    for i:=0 to 15 do
    begin
      if memreclist[i].MemrecArray<>nil then
        enumerateHeapList(memreclist[i].MemrecArray, level+1);
    end;
  end;


end;

procedure TFillHeapList.execute;
var check: boolean;
    id: dword;
    i: integer;
begin
  c:=0;
  frmMemoryAllocHandler.memrecCS.Enter;
  try
    enumerateHeapList(@frmMemoryAllocHandler.HeapBaselevel, 0);
  finally
    frmMemoryAllocHandler.memrecCS.Leave;
  end;

  if c>0 then
    synchronize(updatelist);
end;

procedure TfrmHeaps.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if fillthread<>nil then
  begin
    fillthread.Terminate;
    fillthread.WaitFor;
    fillthread.Free;
  end;

  action:=caFree;
  frmheaps:=nil;
end;

procedure TfrmHeaps.FormCreate(Sender: TObject);
begin
  if frmMemoryAllocHandler=nil then
    frmMemoryAllocHandler:=TfrmMemoryAllocHandler.Create(memorybrowser); //just not show

  if frmMemoryAllocHandler.WaitForInitializationToFinish then
  begin
    //start the thread that enumerates the heaplist
    button1.Left:=(clientwidth div 2) - (button1.Width div 2);

    fillthread:=TFillHeapList.Create(false);
  end;
end;

procedure TfrmHeaps.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmHeaps.ListView1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex<>-1 then
    memorybrowser.memoryaddress:=strtoint('$'+listview1.Items[listview1.ItemIndex].Caption);
end;

initialization
  {$i frmHeapsUnit.lrs}

end.
