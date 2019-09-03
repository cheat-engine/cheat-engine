unit frmBreakpointlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CEDebugger, KernelDebugger, ExtCtrls, LResources, ComCtrls, Menus,
  debuggertypedefinitions, BreakpointTypeDef;

const
  WM_BPUPDATE=WM_USER+1;

type
  { TfrmBreakpointlist }

  TfrmBreakpointlist = class(TForm)
    bplImageList: TImageList;
    ListView1: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miPageWide: TMenuItem;
    miShowShadow: TMenuItem;
    miDelBreakpoint: TMenuItem;
    miSetCondition: TMenuItem;
    pmBreakpoint: TPopupMenu;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miPageWideClick(Sender: TObject);
    procedure miShowShadowClick(Sender: TObject);
    procedure miDelBreakpointClick(Sender: TObject);
    procedure miSetConditionClick(Sender: TObject);
    procedure pmBreakpointPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure update(var m: tmessage); message WM_BPUPDATE; overload;

  public
    { Public declarations }
    procedure updatebplist;
  end;

var
  frmBreakpointlist: TfrmBreakpointlist;

implementation


uses formsettingsunit, MemoryBrowserFormUnit, DebugHelper, frmBreakpointConditionunit,
  vmxfunctions;

type
  tdbvmbpinfo=record
    VirtualAddress: ptruint;
    PhysicalAddress: ptruint;
  end;

resourcestring
  rsBPAreYouSureYouWishToChangeThisToAPegewideBP = 'Are you sure you wish to change this to a pagewide breakpoint?';


function getDBVMInfo(s: string): tdbvmbpinfo;
//s is formatted as 'DBVM VirtualAddress (PhysicalAddress)'
var
  sva: string;
  spa: string;
begin
  sva:=trim(copy(s,6,pos('(',s)-6));
  spa:=copy(s,pos('(',s)+1,length(s));
  spa:=trim(copy(spa,1,pos(')',spa)-1));

  try
    result.VirtualAddress:=strtoint64('$'+sva);
  except
    result.VirtualAddress:=0;
  end;

  try
    result.PhysicalAddress:=strtoint64('$'+spa);
  except
    result.PhysicalAddress:=0
  end;
end;

procedure TFrmBreakpointlist.update(var m: tmessage);
begin
  updatebplist;
end;

procedure TFrmBreakpointlist.updatebplist;
var
  i: integer;
  s: pointer;
  s2: string;
  l: TStringList;
  bp: PDBVMBreakpoint;

  li: TListItem;
  str: string;
begin
  //store the selected item
  if listview1.Selected<>nil then
  begin
    s:=listview1.selected.data;
    s2:=listview1.Caption;
  end
  else
    s:=nil;

  if debuggerthread<>nil then
    debuggerthread.updatebplist(ListView1, miShowShadow.checked);

  i:=0;
  while i<listview1.items.count do
  begin
    if listview1.items[i].data=pointer(-1) then
      listview1.Items[i].Delete
    else
      inc(i);
  end;

  l:=TStringList.create;
  try
    dbvm_getBreakpointList(l);
    for i:=0 to l.count-1 do
    begin
      li:=Listview1.Items.Add;
      li.caption:='DBVM '+l[i];
      li.Data:=pointer(-1);
    end;
  finally
    l.free;
  end;

  if s<>nil then //if something was selected then try to find it back
  begin
    for i:=0 to listview1.Items.count-1 do
    begin
      if ((s=pointer(-1)) and (s2=listview1.items[i].caption)) or (listview1.items[i].data=s) then
      begin
        listview1.items[i].Selected:=true;
        exit;
      end;
    end;
  end;

end;

procedure TfrmBreakpointlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmbreakpointlist:=nil;
end;

procedure TfrmBreakpointlist.FormCreate(Sender: TObject);
begin
  updatebplist;
  miShowShadowClick(miShowShadow);
end;

procedure TfrmBreakpointlist.ListBox1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex>=0 then
    memorybrowser.disassemblerview.SelectedAddress:=StrToInt('$'+listview1.Items[listview1.itemindex].Caption);
end;

procedure TfrmBreakpointlist.ListView1DblClick(Sender: TObject);
var
  bp: pbreakpoint;
  address: ptruint;
  s: string;

  dbvmbpinfo: tdbvmbpinfo;
begin
  if (listview1.selected=nil) then exit;
  
  bp:=listview1.selected.data;
  if bp=pointer(-1) then
  begin
    //dbvm bp
    dbvmbpinfo:=getDBVMInfo(listview1.selected.caption);

    if dbvmbpinfo.virtualAddress<>0 then
    begin
      memorybrowser.AddToDisassemblerBackList(pointer(memorybrowser.disassemblerview.SelectedAddress));
      try
        memorybrowser.disassemblerview.SelectedAddress:=dbvmbpinfo.virtualAddress
      except
      end;
    end;
    exit;
  end;

  if debuggerthread<>nil then
  begin
    debuggerthread.lockbplist;
    try
      updatebplist;

      if listview1.Selected<>nil then
      begin
        bp:=listview1.selected.data;
        address:=bp.address;
        if bp.breakpointTrigger=bptExecute then
        begin
          //show in disassembler
          memorybrowser.AddToDisassemblerBackList(pointer(memorybrowser.disassemblerview.SelectedAddress));
          memorybrowser.disassemblerview.SelectedAddress:=address;
        end
        else
        begin
          //show in hexview
          memorybrowser.hexview.history.Push(pointer(memorybrowser.hexview.address));
          memorybrowser.hexview.address:=address;
        end;

      end;
    finally
      debuggerthread.unlockbplist;
    end;
  end;
end;

procedure TfrmBreakpointlist.MenuItem2Click(Sender: TObject);
var bp: Pbreakpoint;
 i: integer;
begin
  if listview1.Selected<>nil then
  begin
    if debuggerthread<>nil then
    begin
      debuggerthread.lockbplist; //prevent the list from updating/deleting bp's
      try
        if listview1.Selected<>nil then
        begin
          bp:=listview1.Selected.Data;
          debuggerthread.UnsetBreakpoint(bp);
        end;
      finally
        debuggerthread.unlockbplist;
      end;

    end;

  end;
end;

procedure TfrmBreakpointlist.miPageWideClick(Sender: TObject);
var bp: PBreakpoint;
begin

  if (listview1.selected<>nil) and (MessageDlg(rsBPAreYouSureYouWishToChangeThisToAPegewideBp, mtConfirmation, [mbyes, mbno], 0)=mryes) then
  begin
    debuggerthread.lockbplist;
    try
      bp:=listview1.selected.Data;
      if bp.breakpointMethod=bpmException then
      begin
        bp.address:=bp.address or (not $fff);
        bp.size:=4096;
      end;

    finally
      debuggerthread.unlockbplist;
    end;


    updatebplist;
  end;
end;

procedure TfrmBreakpointlist.miShowShadowClick(Sender: TObject);
var i: integer;
begin
  for i:=ListView1.Columns.count-1 downto 5 do
    listview1.Column[i].Visible:=miShowShadow.checked;

  updatebplist;
end;

procedure TfrmBreakpointlist.miDelBreakpointClick(Sender: TObject);
var
  bp: Pbreakpoint;
  dbvmbpinfo: tdbvmbpinfo;
begin
  if listview1.Selected<>nil then
  begin
    if listview1.selected.data=pointer(-1) then
    begin
      dbvmbpinfo:=getDBVMInfo(listview1.selected.Caption);
      dbvm_cloak_removechangeregonbp(dbvmbpinfo.PhysicalAddress);
    end
    else
    begin
      if debuggerthread<>nil then
      begin
        debuggerthread.lockbplist; //prevent the list from updating/deleting bp's
        try
          if listview1.Selected<>nil then
          begin
            bp:=listview1.Selected.Data;

            //bp.active:=false;

            if not bp.markedfordeletion then
              debuggerthread.RemoveBreakpoint(bp);
          end;
        finally
          debuggerthread.unlockbplist;
        end;
      end;
    end;
  end;
end;

procedure TfrmBreakpointlist.miSetConditionClick(Sender: TObject);
var bp: PBreakpoint;
  script: string;
  easy: boolean;
begin
  if listview1.Selected<>nil then
  begin
    debuggerthread.lockbplist;
    try
      if listview1.selected<>nil then
        bp:=listview1.Selected.Data;

      if bp.markedfordeletion then exit; //useless breakpoint

      //still here so use this breakpoint
      inc(bp.referencecount);

    finally
      debuggerthread.unlockbplist;
    end;

    //still here so bp isn't about to be deleted.
    try
      with TfrmBreakpointCondition.create(self) do
      begin
        script:=debuggerthread.getbreakpointcondition(bp, easy);
        if easy then
          edtEasy.text:=script
        else
          mComplex.text:=script;

        rbEasy.Checked:=easy;
        rbComplex.checked:=not easy;

        if showmodal=mrok then
        begin
          easy:=rbEasy.checked;
          if easy then
            script:=edtEasy.text
          else
            script:=mComplex.text;

          debuggerthread.setbreakpointcondition(bp, easy, script);
        end;
        free;
      end;
    finally
      dec(bp.referencecount);
    end;

  end;

end;

procedure TfrmBreakpointlist.pmBreakpointPopup(Sender: TObject);
var bp: Pbreakpoint;
begin
  if listview1.selected<>nil then
  begin
    miDelBreakpoint.enabled:=true;

    bp:=listview1.selected.Data;
    if bp=pointer(-1) then
    begin
      miSetCondition.enabled:=false;
      miPageWide.visible:=false;
    end
    else
    begin
      miSetCondition.enabled:=true;

      if debuggerthread<>nil then
      begin
        debuggerthread.lockbplist;
        try
          if bp.breakpointMethod=bpmException then
            miPageWide.visible:=true;

        finally
          debuggerthread.unlockbplist;
        end;
      end;
    end;
  end
  else
  begin
    miDelBreakpoint.enabled:=false;
    miSetCondition.enabled:=false;
    miPageWide.visible:=false;
  end;
end;

procedure TfrmBreakpointlist.Timer1Timer(Sender: TObject);
begin
  updatebplist; //every 5 seconds
end;

initialization
  {$i frmBreakpointlistunit.lrs}

end.
