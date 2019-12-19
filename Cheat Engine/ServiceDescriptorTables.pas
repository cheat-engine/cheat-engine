unit ServiceDescriptorTables;

{$MODE Delphi}

interface

uses
  {$ifdef windows}
  windows, imagehlp,
  {$endif}LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,CEFuncProc,NewKernelHandler, Menus, ComCtrls,symbolhandler,disassembler,
  StdCtrls, LResources, commonTypeDefs;

{$ifdef windows}
type tenummodules= class(tthread)
  private
    symbolname: string;
    callnumber: dword;
    procedure done;
    procedure foundone;
  public
    procedure execute; override;
end;
{$endif}

type
  TfrmServiceDescriptorTables = class(TForm)
    TreeView1: TTreeView;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    Scancallnumbersandnames1: TMenuItem;
    CancelScan1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    FindDialog1: TFindDialog;
    PopupMenu1: TPopupMenu;
    Find1: TMenuItem;
    GotoSTDaddress1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Scancallnumbersandnames1Click(Sender: TObject);
    procedure CancelScan1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure TreeView1DblClick(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure GotoSTDaddress1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmServiceDescriptorTables: TfrmServiceDescriptorTables;

implementation


uses MemoryBrowserFormUnit, dbk32functions, ProcessHandlerUnit;

resourcestring
  rsServiceDescriptorTable = 'Service Descriptor Table';
  rsServiceDescriptorTableShadow = 'Service Descriptor Table Shadow';
  rsParameter = 'parameter';
  rsParameters = 'parameters';
  rsUnknownname = 'unknownname';
  rsIncompatibleSDT = 'Incompatible Service Descriptor Table. This table '
    +'contains more or less than in the file';
  rsIncompatibleSSDT = 'Incompatible Service Descriptor Table Shadow. This '
    +'table contains more or less than in the file';
  rsNothingFound = 'Nothing found';

var symbolname: string;
    canceled: boolean;

procedure TfrmServiceDescriptorTables.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmServiceDescriptorTables:=nil;
end;

procedure TfrmServiceDescriptorTables.FormCreate(Sender: TObject);
type TSdtStruct=record
  functionpointers: pointer;
  reserved: dword;
  nrofpointers: dword;
  parametercounts: pointer;
end;
var sdtstruct: tsdtstruct;
    sdt: ptrUint;
    x: ttreenode;
    second: boolean;
    p: thandle;
    nrofbytes: PtrUInt;

    functionpointers:PDwordArray;
    parametercounts: PByteArray;
    i: integer;
    s: string;
begin
//open own process in case kernelmode openprocess wasn't used
  {$ifdef windows}
  p:=dbk32functions.OP(PROCESS_ALL_ACCESS,true,getcurrentprocessid);

  second:=true;

  repeat
    second:=not second;

    if not second then
    begin
      sdt:=newkernelhandler.GetSDT;
      x:=treeview1.Items.Add(nil, rsServiceDescriptorTable);
    end
    else
    begin
      sdt:=newkernelhandler.GetSDTShadow;
      x:=treeview1.Items.Add(nil, rsServiceDescriptorTableShadow);
    end;

    if RPM(p,pointer(sdt),@sdtstruct,sizeof(sdtstruct),nrofbytes) then
    begin
      getmem(functionpointers,sdtstruct.nrofpointers*4);
      getmem(parametercounts,sdtstruct.nrofpointers);
      try
        if RPM(p,sdtstruct.parametercounts,parametercounts,sdtstruct.nrofpointers,nrofbytes) then
          if RPM(p,sdtstruct.functionpointers,functionpointers,sdtstruct.nrofpointers*4,nrofbytes) then
          begin
            for i:=0 to sdtstruct.nrofpointers-1 do
            begin
              s:=inttohex(functionpointers[i],8)+' ('+inttostr(parametercounts[i] div 4);
              if parametercounts[i]=4 then
                s:=s+' '+rsParameter+')'
              else
                s:=s+' '+rsParameters+')';

                s:=s+' - '+rsUnknownname;

              treeview1.Items.AddChild(x,s);
            end;
          end;
      finally
        freemem(parametercounts);
        freemem(functionpointers);
      end;
    end;

  until second;

  if IsValidHandle(p) then
    closehandle(p);
  {$endif}
end;


{$ifdef windows}
function ES(SymName: LPSTR; SymbolAddress, SymbolSize: ULONG; UserContext: Pointer): Bool; stdcall;
var buf: array[0..14] of byte;
    ar: ptrUint;
begin
  if readprocessmemory(processhandle,pointer(ptrUint(SymbolAddress)),@buf[0],15,ar) then
  begin
    if (buf[0]=$b8) and (buf[3]=$00) and (buf[4]=$00) and (buf[5]=$ba) and (buf[10]=$ff) and (buf[11]=$12) and (buf[12]=$c2) and (buf[14]=$00) then
    begin
      //b8 xx xx 00 00 ba xx xx xx xx ff 12 c2 xx 00
      tenummodules(UserContext).symbolname:=SymName;
      tenummodules(UserContext).callnumber:=pdword(@buf[1])^;
      tenummodules(UserContext).synchronize(tenummodules(UserContext).foundone);
    end;
  end;

  result:=not canceled;
end;

function EM(ModuleName: LPSTR; BaseOfDll: ULONG; UserContext: Pointer): Bool; stdcall;
begin
  result:=not canceled;
  SymEnumerateSymbols(processhandle,BaseOfDLL,@ES,usercontext);
end;



procedure tenummodules.foundone;
var tablenr: integer;
    entrynr: integer;
    x: ttreenode;
    s: string;
begin
  if frmServiceDescriptorTables<>nil then
  begin
    tablenr:=callnumber shr 12;
    if tablenr>1 then exit;

    entrynr:=callnumber and $fff;
    with frmServiceDescriptorTables do
    begin
      x:=treeview1.Items.GetFirstNode;
      if tablenr=1 then
      x:=x.getNextSibling;


      s:=x.Items[entrynr].Text;
      s:=copy(s,1,pos(' - ',s)-1);
      s:=s+' - '+symbolname;
      x.Items[entrynr].Text:=s;
    end;

  end;
end;

procedure tenummodules.done;
begin
  if frmServiceDescriptorTables<>nil then
  begin
    with frmServiceDescriptorTables do
    begin
      CancelScan1.visible:=false;
      Scancallnumbersandnames1.enabled:=true;
    end;
  end;
end;

procedure tenummodules.execute;
begin
  freeonterminate:=true;
  Priority:=tpLower;

  symhandler.waitforsymbolsloaded;

  if not canceled then
    SymEnumerateModules(processhandle,@EM,self);

  synchronize(done);
end;

{$endif}


procedure TfrmServiceDescriptorTables.Scancallnumbersandnames1Click(
  Sender: TObject);
begin
  {$ifdef windows}
  Scancallnumbersandnames1.Enabled:=false;
  cancelscan1.Visible:=true;
  canceled:=false;
  tenummodules.Create(false);
  {$endif}
end;

procedure TfrmServiceDescriptorTables.CancelScan1Click(Sender: TObject);
begin
  Canceled:=true;
end;

procedure TfrmServiceDescriptorTables.Save1Click(Sender: TObject);
var f: textfile;
    i: integer;
    t: ttreenode;
    s: string;
begin
  if savedialog1.Execute then
  begin
    assignfile(f,savedialog1.FileName);
    rewrite(f);

    t:=treeview1.Items.GetFirstNode;
    writeln(f,t.Count);
    for i:=0 to t.Count-1 do
    begin
      s:=t[i].Text;
      s:=copy(s,pos(' - ',s)+3,length(s));
      writeln(f,s);
    end;

    t:=t.getNextSibling;
    writeln(f,t.Count);
    for i:=0 to t.Count-1 do
    begin
      s:=t[i].Text;
      s:=copy(s,pos(' - ',s)+3,length(s));
      writeln(f,s);
    end;
    
    closefile(f);
  end;
end;

procedure TfrmServiceDescriptorTables.TreeView1DblClick(Sender: TObject);
var x: dword;
    i: integer;
begin
  if (treeview1.Selected<>nil) and (treeview1.Selected.Level=1) then
  begin
    val('$'+treeview1.Selected.Text,x,i);
    memorybrowser.disassemblerview.SelectedAddress:=x;
  end;
end;

procedure TfrmServiceDescriptorTables.Open1Click(Sender: TObject);
var f: textfile;
    i: integer;
    t: ttreenode;
    s,s2: string;
    x: integer;
begin
  if opendialog1.Execute then
  begin
    assignfile(f,opendialog1.FileName);
    reset(f);
    try
      readln(f,x);
      t:=treeview1.Items.GetFirstNode;
      if x<>t.Count then
        raise exception.Create(rsIncompatibleSDT);

      for i:=0 to x-1 do
      begin
        s:=t.Items[i].Text;
        s:=copy(s,1,pos(' - ',s)-1);
        s:=s+' - ';
        readln(f,s2);
        s:=s+s2;
        t.Items[i].Text:=s;
      end;

      readln(f,x);
      t:=t.getNextSibling;
      if x<>t.Count then
        raise exception.Create(rsIncompatibleSSDT);

      for i:=0 to x-1 do
      begin
        s:=t.Items[i].Text;
        s:=copy(s,1,pos(' - ',s)-1);
        s:=s+' - ';
        readln(f,s2);
        s:=s+s2;
        t.Items[i].Text:=s;
      end;

    finally
      closefile(f);
    end;
  end;

end;

procedure TfrmServiceDescriptorTables.FindDialog1Find(Sender: TObject);
var t: ttreenode;
begin
  t:=treeview1.Selected;
  if t=nil then
    t:=treeview1.Items.GetFirstNode
  else
    t:=t.GetNext;

  while t<>nil do
  begin
    if pos(uppercase(finddialog1.FindText),uppercase(t.Text))>0 then
    begin
      treeview1.Selected:=t;
      exit;
    end;

    t:=t.GetNext;
  end;

  showmessage(rsNothingFound);


  //finddialog1.FindText
end;

procedure TfrmServiceDescriptorTables.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmServiceDescriptorTables.GotoSTDaddress1Click(
  Sender: TObject);
var table: integer;
    a: ptrUint;
    x,y: ptrUint;

begin
  {$ifdef windows}
  if (treeview1.Selected=nil) or (treeview1.Selected.Level<>1) then exit;
  table:=treeview1.Selected.Parent.Index;

  if table=0 then
    a:=GetSDT
  else
    a:=GetSDTShadow;

  y:=0;

  readprocessmemory(processhandle,pointer(a),@y,4,x);
  inc(y,treeview1.Selected.Index*4);
  memorybrowser.memoryaddress:=y;
  {$endif}
end;

initialization
  {$i ServiceDescriptorTables.lrs}

end.
