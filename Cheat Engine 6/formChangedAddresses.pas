unit formChangedAddresses;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ExtCtrls, ComCtrls, Menus, NewKernelHandler, LResources,
  disassembler, symbolhandler, byteinterpreter, CustomTypeHandler;

type
  TAddressEntry=class
  public
    address: ptruint; //for whatever reason it could be used in the future
    context: TContext;
    stack: record
      stack: pbyte;
      savedsize: dword;
    end;

    procedure savestack;
    destructor destroy; override;
  end;


  { TfrmChangedAddresses }

  TfrmChangedAddresses = class(TForm)
    lblInfo: TLabel;
    Panel1: TPanel;
    OKButton: TButton;
    Changedlist: TListView;
    cbDisplayType: TComboBox;
    Timer1: TTimer;
    PopupMenu1: TPopupMenu;
    Showregisterstates1: TMenuItem;
    Browsethismemoryregion1: TMenuItem;
    procedure OKButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ChangedlistDblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Showregisterstates1Click(Sender: TObject);
    procedure Browsethismemoryregion1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure refetchValues;
  public
    { Public declarations }
    procedure AddRecord;
  end;


implementation


uses CEDebugger, MainUnit, frmRegistersunit, MemoryBrowserFormUnit, debughelper, debugeventhandler;

destructor TAddressEntry.destroy;
begin
  if stack.stack<>nil then
    freemem(stack.stack);

  inherited destroy;
end;

procedure TAddressEntry.savestack;
begin
  getmem(stack.stack, 4096);
  ReadProcessMemory(processhandle, pointer(context.{$ifdef cpu64}Rsp{$else}esp{$endif}), stack.stack, 4096, stack.savedsize);
end;


procedure TfrmChangedAddresses.AddRecord;
var s: string;
haserror: boolean;
address: ptrUint;
 i: integer;
 li: tlistitem;
 currentthread: TDebugThreadHandler;

 x: TaddressEntry;
begin
  //the debuggerthread is idle at this point
  currentThread:=debuggerthread.CurrentThread;
  if currentthread<>nil then
  begin
    //get the instruction address
    address:=currentThread.context.{$ifdef cpu64}Rip{$else}eip{$endif};
    s:=disassemble(address);
    i:=pos('[',s)+1;
    s:=copy(s,i,pos(']',s)-i);

    address:=symhandler.getAddressFromName(s, false, haserror, currentthread.context);

    if not hasError then
    begin
      //check if this address is already in the list
      s:=inttohex(address,8);
      for i:=0 to changedlist.Items.Count-1 do
        if changedlist.items[i].caption=s then exit;

      //and if not, add it
      li:=changedlist.Items.add;
      li.caption:=s;
      li.SubItems.Add('');


      x:=TAddressEntry.create;
      x.context:=currentthread.context^;
      x.address:=address;
      x.savestack;

      li.Data:=x;

    end;
  end;
end;

procedure TfrmChangedAddresses.OKButtonClick(Sender: TObject);
var temp: dword;
    i: integer;
begin

  if OKButton.caption='Stop' then
  begin
    debuggerthread.FindWhatCodeAccessesStop(self);
    okButton.Caption:='Close';
  end
  else
    close;

end;

procedure TfrmChangedAddresses.FormClose(Sender: TObject;
  var Action: TCloseAction);
var temp:dword;
    i: integer;
    ae: TAddressEntry;
begin
  action:=caFree;


  for i:=0 to changedlist.Items.Count-1 do
  begin
    ae:=TAddressEntry(changedlist.Items[i].Data);
    ae.free;
  end;

  if OKButton.caption='Stop' then
    OKButton.Click;
end;

procedure TfrmChangedAddresses.FormShow(Sender: TObject);
begin
  OKButton.Caption:='Stop';
end;

procedure TfrmChangedAddresses.ChangedlistDblClick(Sender: TObject);
var i: integer;
    ad: dword;
begin
  if changedlist.Selected<>nil then
    mainform.addresslist.addaddress('No Description', changedlist.selected.caption, [],0, OldVarTypeToNewVarType(cbDisplayType.itemindex));
end;

procedure TfrmChangedAddresses.PopupMenu1Popup(Sender: TObject);
begin
  Showregisterstates1.enabled:=changedlist.selected<>nil;
  Browsethismemoryregion1.enabled:=changedlist.selected<>nil;
end;

procedure TfrmChangedAddresses.refetchValues;
var i: integer;
    s: string;
begin
  if changedlist.Items.Count>0 then
  begin
    for i:=0 to changedlist.Items.Count-1 do
    begin
      case cbDisplayType.ItemIndex of
        0: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtByte);
        1: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtWord);
        2: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtDWord);
        3: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtSingle);
        4: s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtDouble);
        else
        begin
          //custom type
          s:=ReadAndParseAddress(strtoint64('$'+changedlist.items[i].caption), vtCustom, TCustomType(cbDisplayType.Items.Objects[cbDisplayType.ItemIndex]));
        end;
      end;

      if Changedlist.Items[i].SubItems.Count=0 then
        Changedlist.Items[i].SubItems.Add('');

      Changedlist.Items[i].SubItems[0]:=s;
    end;
  end;
end;

procedure TfrmChangedAddresses.Timer1Timer(Sender: TObject);
begin
  refetchValues;
end;

procedure TfrmChangedAddresses.Showregisterstates1Click(Sender: TObject);
var ae: TAddressEntry;
begin
  if changedlist.Selected<>nil then
  begin
    with TRegisters.create(self) do
    begin
      borderstyle:=bsSingle;

      ae:=TAddressEntry(changedlist.Selected.Data);

      SetContextPointer(@ae.context, ae.stack.stack, ae.stack.savedsize);

      show;
    end;
  end;
end;

procedure TfrmChangedAddresses.Browsethismemoryregion1Click(
  Sender: TObject);
begin
  if changedlist.Selected<>nil then
  begin
    memorybrowser.memoryaddress:=strtoint64('$'+changedlist.Selected.Caption);
    if not memorybrowser.visible then
      memorybrowser.show;
  end;
end;

procedure TfrmChangedAddresses.FormDestroy(Sender: TObject);
begin
  saveformposition(self,[]);
end;

procedure TfrmChangedAddresses.FormCreate(Sender: TObject);
var x: array of integer;
    i: integer;
begin
  setlength(x, 0);
  loadformposition(self,x);

  //fill in the custom types
  for i:=0 to customTypes.count-1 do
    cbDisplayType.Items.AddObject(TCustomType(customTypes[i]).name, customTypes[i]);
end;

initialization
  {$i formChangedAddresses.lrs}

end.

