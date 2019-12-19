unit frmBranchMapperUnit;

{$mode delphi}
{$warn 4056 off}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CheckLst, maps, ExtCtrls, syncobjs;

type

  { TfrmBranchMapper }

  TfrmBranchMapper = class(TForm)
    Button1: TButton;
    Button2: TButton;
    clbThreads: TCheckListBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    map: tmap;
    mapmrew: TMultiReadExclusiveWriteSynchronizer;
  end;

var
  frmBranchMapper: TfrmBranchMapper;

implementation

{$R *.lfm}

uses CEFuncProc, CEDebugger, debugeventhandler, DebugHelper, symbolhandler, symbolhandlerstructs;

{ TfrmBranchMapper }

resourcestring
  rsStartMapping = 'Start Mapping';
  rsStop = 'Stop';
  rsFoundNr = 'Found %d';
  rsSaveResultsToFile = 'Save results to file';

procedure TfrmBranchMapper.Button1Click(Sender: TObject);
var
  tidlist: Tlist;
  i: integer;
  tid: dword;
begin
  if button1.tag=0 then
  begin
    tidlist:=tlist.create;
    i:=0;
    for i:=0 to clbthreads.count-1 do
    begin
      if clbThreads.checked[i] then
      begin
        tid:=dword(clbThreads.Items.Objects[i]);
        tidlist.add(pointer(tid));
      end;
    end;

    if tidlist.count=0 then
      debuggerthread.startBranchMapper
    else
      debuggerthread.startBranchMapper(tidlist);

    tidlist.free;

    button1.caption:=rsStop;
    button1.tag:=1;
  end
  else
  begin
    debuggerthread.stopBranchMapper;
    button1.caption:=rsStartMapping;
    button1.tag:=0;
  end;
end;

procedure TfrmBranchMapper.Button2Click(Sender: TObject);
var
  f: tfilestream;
  mi: TMapIterator=nil;
  address: ptruint;
  ml: tstringlist=nil;
  modinfo: TModuleInfo;
  i: integer;
  ind: word;
begin
  if savedialog1.execute then
  begin

    f:=tfilestream.Create(savedialog1.filename, fmCreate);

    try
      button2.Caption:='Saving. Please wait';
      button2.Repaint;

      ml:=tstringlist.create;

      symhandler.getModuleList(ml);
      f.WriteWord(ml.Count);
      for i:=0 to ml.count-1 do
        f.WriteAnsiString(ml[i]);

      mapmrew.beginread;
      try
        mi:=TMapIterator.create(map);
        while not mi.eom do
        begin
          mi.GetID(address);

          if symhandler.getmodulebyaddress(address, modinfo) then
          begin
            ind:=ml.IndexOf(modinfo.modulename);
            address:=address-modinfo.baseaddress;
          end
          else
            ind:=$ffff;

          f.WriteWord(ind);
          f.WriteQWord(address);

          mi.next;
        end;

      finally
        mapmrew.endread;

        if mi<>nil then
          freeandnil(mi);

        if ml<>nil then
          freeandnil(ml);
      end;

    finally
      f.free;

      button2.Caption:=rsSaveResultsToFile;
      button2.Repaint;
    end;
  end;
end;

procedure TfrmBranchMapper.FormCreate(Sender: TObject);
begin
  map:=tmap.Create(ituPtrSize, 1);
  mapmrew:=TMultiReadExclusiveWriteSynchronizer.Create;

  LoadFormPosition(self);
end;

procedure TfrmBranchMapper.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmBranchMapper.FormShow(Sender: TObject);
var
  list: Tlist;
  i: integer;
  c: integer;
  ct: TDebugThreadHandler;
begin
  if startdebuggerifneeded then
  begin
    clbThreads.Clear;

    list:=debuggerthread.lockThreadlist;
    c:=list.count;
    for i:=0 to c-1 do
    begin
      ct:=TDebugThreadHandler(list[i]);
      clbThreads.AddItem(inttohex(ct.ThreadID,1), tobject(ct.ThreadID));
      clbThreads.Checked[i]:=true;
    end;

    debuggerthread.unlockThreadlist;

  end
  else
    close;


end;

procedure TfrmBranchMapper.Timer1Timer(Sender: TObject);
var c: integer;
begin
  mapmrew.beginread;
  c:=map.count;
  mapmrew.endread;

  label2.caption:=format(rsFoundNr, [c]);
  if c>0 then
    button2.enabled:=true;
end;

end.

