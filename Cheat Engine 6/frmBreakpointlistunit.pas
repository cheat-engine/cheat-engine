unit frmBreakpointlistunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEDebugger,KernelDebugger, ExtCtrls, LResources, ComCtrls;

const
  WM_BPUPDATE=WM_USER+1;

type
  { TfrmBreakpointlist }

  TfrmBreakpointlist = class(TForm)
    ListView1: TListView;
    Timer1: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
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


uses formsettingsunit, MemoryBrowserFormUnit, DebugHelper;

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

  if kdebugger.isactive then
  begin
   (* for i:=0 to 3 do
      if debuggerthread2.breakpoints[i]<>0 then
        listbox1.items.add(inttohex(debuggerthread2.breakpoints[i],8)); *)
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
end;

procedure TfrmBreakpointlist.ListBox1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex>=0 then
    memorybrowser.disassemblerview.SelectedAddress:=StrToInt('$'+listview1.Items[listview1.itemindex].Caption);
end;

procedure TfrmBreakpointlist.Timer1Timer(Sender: TObject);
begin
  updatebplist; //every 5 seconds
end;

initialization
  {$i frmBreakpointlistunit.lrs}

end.
