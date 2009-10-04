unit frmMemoryAllocHandlerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, symbolhandler, cefuncproc,newkernelhandler, autoassembler,
  ExtCtrls;

type
  TfrmMemoryAllocHandler = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblHandle: TLabel;
    lblBaseAddress: TLabel;
    lblAllocationType: TLabel;
    lblProtect: TLabel;
    lblSize: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    procedure hook_alloc(var m: TMessage); message wm_user+1;
    procedure hook_free(var m: TMessage); message wm_user+2;
  public
    { Public declarations }
    hookedprocessid: dword;
  end;

var
  frmMemoryAllocHandler: TfrmMemoryAllocHandler;

implementation

{$R *.dfm}

uses frmautoinjectunit, memorybrowserformunit;

type TmemoryAllocevent=class
  private
  public
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;

    _free: boolean;
    FreeType: dword;
    FreeSize: dword;
end;

type TCreateEvent=record
    handle: dword;
    baseaddress: dword;
    allocationType: dword;
    protect: dword;
    size: dword;
    esp: dword;
  end;


type TFreeEvent=record
  handle: dword;
  baseaddress: dword;
  size: dword;
  FreeType: dword;
end;

procedure TfrmMemoryAllocHandler.hook_alloc(var m: TMessage);
var x: TCreateEvent;
    br: dword;
    o: TMemoryAllocEvent;
begin
  if readprocessmemory(processhandle, pointer(m.WParam),@x,sizeof(x), br) then
  begin
    o:=TmemoryAllocevent.create;
    o.handle:=x.handle;
    o.baseaddress:=x.baseaddress;
    o.allocationType:=x.allocationType;
    o.protect:=x.protect;
    o.size:=x.size;
    o.esp:=x.esp;
    listbox1.Items.AddObject('Alloc:'+inttohex(x.baseaddress,8)+' ('+inttostr(x.size)+')',O);
  end;
end;

procedure TfrmMemoryAllocHandler.hook_free(var m: TMessage);
var x: TFreeEvent;
    br: dword;
    o: TMemoryAllocEvent;
begin
  if readprocessmemory(processhandle, pointer(m.WParam),@x,sizeof(x), br) then
  begin
    o:=TmemoryAllocevent.create;
    o._free:=true;
    o.handle:=x.handle;
    o.baseaddress:=x.baseaddress;
    o.freetype:=x.FreeType;
    o.FreeSize:=x.size;
    listbox1.Items.AddObject('Free:'+inttohex(x.baseaddress,8)+' ('+inttostr(x.size)+')',O);
  end;
end;


procedure TfrmMemoryAllocHandler.FormCreate(Sender: TObject);
var injectionscript: TStringlist;
begin
  injectionscript:=tstringlist.Create;
  try
    //inject allochook.dll
    injectdll(CheatEngineDir+'allochook.dll');
    symhandler.reinitialize;

    //set windowhandle
    injectionscript.Add('CeAllocHandlerWindow:');
    injectionscript.Add('DD '+inttohex(self.handle,8));


    //hook apis
    generateAPIHookScript(injectionscript,'NtAllocateVirtualMemory','CeAllocateVirtualMemory', 'NtAllocateVirtualMemoryOrig','0');
    generateAPIHookScript(injectionscript,'NtFreeVirtualMemory','CeFreeVirtualMemory', 'NtFreeVirtualMemoryOrig','1');

    if not autoassemble(injectionscript,false) then raise exception.Create('Failure hooking apis');
  finally
    injectionscript.Free;
  end;
end;

procedure TfrmMemoryAllocHandler.ListBox1Click(Sender: TObject);
var o: TMemoryAllocEvent;
begin
  if listbox1.ItemIndex<>-1 then
  begin
    if not panel1.Visible then panel1.visible:=true;
    o:=TMemoryAllocEvent(listbox1.Items.Objects[listbox1.ItemIndex]);
    lblhandle.caption:=inttohex(o.handle,1);
    lblBaseAddress.Caption:=inttohex(o.baseaddress,8);

    if o._free then
    begin
      label3.Caption:='Free Type:';
      lblAllocationType.caption:=freetypetostring(o.FreeType);

      label4.Caption:='Size:';
      lblProtect.Caption:=inttostr(o.size);;
      label6.Visible:=false;
    end
    else
    begin
      label3.Caption:='Allocation Type:';
      lblAllocationType.Caption:=allocationtypetostring(o.allocationType);

      label4.Caption:='Protect:';
      lblProtect.Caption:=allocationprotecttostring(o.protect);
      lblSize.Caption:=inttostr(o.size);
      label6.Visible:=true;
    end;
  end else
  begin
    panel1.Visible:=false;
  end;
end;

procedure TfrmMemoryAllocHandler.ListBox1DblClick(Sender: TObject);
var o: TMemoryAllocEvent;
begin
  if listbox1.ItemIndex<>-1 then
  begin
    o:=TMemoryAllocEvent(listbox1.Items.Objects[listbox1.ItemIndex]);
    memorybrowser.memoryaddress:=o.baseaddress;
    memorybrowser.RefreshMB;
  end;
end;

end.
