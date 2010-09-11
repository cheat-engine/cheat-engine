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
    miDelBreakpoint: TMenuItem;
    miSetCondition: TMenuItem;
    pmBreakpoint: TPopupMenu;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure miDelBreakpointClick(Sender: TObject);
    procedure miSetConditionClick(Sender: TObject);
    procedure pmBreakpointPopup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    procedure update(var m: tmessage); message WM_BPUPDATE;
    procedure updatebplist;
  public
    { Public declarations }

  end;

var
  frmBreakpointlist: TfrmBreakpointlist;

implementation


uses formsettingsunit, MemoryBrowserFormUnit, DebugHelper, frmBreakpointConditionunit;

procedure TFrmBreakpointlist.update(var m: tmessage);
begin
  updatebplist;
end;

procedure TFrmBreakpointlist.updatebplist;
var i: integer;
begin

  listview1.Clear;
  if debuggerthread<>nil then
    debuggerthread.updatebplist(ListView1);
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
end;

procedure TfrmBreakpointlist.ListBox1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex>=0 then
    memorybrowser.disassemblerview.SelectedAddress:=StrToInt('$'+listview1.Items[listview1.itemindex].Caption);
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
begin
  miDelBreakpoint.enabled:=listview1.Selected<>nil;
  miSetCondition.enabled:=listview1.Selected<>nil;
end;

procedure TfrmBreakpointlist.Timer1Timer(Sender: TObject);
begin
  updatebplist; //every 5 seconds
end;

initialization
  {$i frmBreakpointlistunit.lrs}

end.
