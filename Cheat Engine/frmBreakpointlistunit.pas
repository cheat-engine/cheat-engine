unit frmBreakpointlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CEDebugger, KernelDebugger, ExtCtrls, LResources, ComCtrls, Menus,
  debuggertypedefinitions;

const
  WM_BPUPDATE=WM_USER+1;

type
  { TfrmBreakpointlist }

  TfrmBreakpointlist = class(TForm)
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
    procedure updatebplist;
  public
    { Public declarations }

  end;

var
  frmBreakpointlist: TfrmBreakpointlist;

implementation


uses formsettingsunit, MemoryBrowserFormUnit, DebugHelper, frmBreakpointConditionunit;

resourcestring
  rsBPAreYouSureYouWishToChangeThisToAPegewideBP = 'Are you sure you wish to change this to a pagewide breakpoint?';

procedure TFrmBreakpointlist.update(var m: tmessage);
begin
  updatebplist;
end;

procedure TFrmBreakpointlist.updatebplist;
var i: integer;
 s: pointer;
begin
  //store the selected item
  if listview1.Selected<>nil then
    s:=listview1.selected.data
  else
    s:=nil;

  if debuggerthread<>nil then
    debuggerthread.updatebplist(ListView1, miShowShadow.checked);

  if s<>nil then //if something was selected then try to find it back
  begin
    for i:=0 to listview1.Items.count-1 do
      if listview1.items[i].data=s then
      begin
        listview1.items[i].Selected:=true;
        exit;
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
var bp: pbreakpoint;
 address: ptruint;
begin
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
var bp: Pbreakpoint;
begin

  if listview1.Selected<>nil then
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
    miSetCondition.enabled:=true;

    bp:=listview1.selected.Data;
    debuggerthread.lockbplist;
    try
      if bp.breakpointMethod=bpmException then
        miPageWide.visible:=true;

    finally
      debuggerthread.unlockbplist;
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
