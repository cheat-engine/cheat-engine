unit frmBreakpointlistunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,debugger,debugger2;

type
  TfrmBreakpointlist = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure updatebplist;
  end;

var
  frmBreakpointlist: TfrmBreakpointlist;

implementation

{$R *.dfm}

uses formsettingsunit, MemoryBrowserFormUnit;

procedure TFrmBreakpointlist.updatebplist;
var i: integer;
begin
  listbox1.Clear;
  if debuggerthread<>nil then
  begin
    if formsettings.rbDebugAsBreakpoint.checked then
    begin
      //show the userbreakpoints
      for i:=0 to length(debuggerthread.userbreakpoints)-1 do
        listbox1.Items.Add(IntToHex(debuggerthread.userbreakpoints[i],8));
    end
    else
    begin
      for i:=0 to length(debuggerthread.int3userbreakpoints)-1 do
        listbox1.Items.Add(inttohex(debuggerthread.int3userbreakpoints[i].address,8));
    end;

  end;

  if debuggerthread2<>nil then
    for i:=0 to 3 do
      if debuggerthread2.breakpoints[i]<>0 then
        listbox1.items.add(inttohex(debuggerthread2.breakpoints[i],8));
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

procedure TfrmBreakpointlist.Button1Click(Sender: TObject);
var address: dword;
    i: integer;
begin
  if listbox1.ItemIndex>=0 then
  begin

    if debuggerthread2<>nil then
    begin
      address:=strtoint('$'+listbox1.items[listbox1.itemindex]);
      for i:=0 to 3 do
        if debuggerthread2.breakpoints[i]=address then
        begin
          debuggerthread2.breakpoints[i]:=0;
          break;
        end;

      debuggerthread2.setbreakpoints;
      updatebplist;
    end;


    if debuggerthread<>nil then
      debugger.ToggleBreakpoint(strtoint('$'+listbox1.items[listbox1.itemindex]));

  end;


end;

procedure TfrmBreakpointlist.ListBox1DblClick(Sender: TObject);
begin
  if listbox1.ItemIndex>=0 then
    memorybrowser.Disassembleraddress:=StrToInt('$'+listbox1.items[listbox1.itemindex]);
  memorybrowser.updatedisassemblerview;
end;

end.
