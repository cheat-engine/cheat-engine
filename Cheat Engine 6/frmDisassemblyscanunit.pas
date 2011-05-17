unit frmDisassemblyscanunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,disassembler,{$ifndef net}NewKernelHandler,{$endif}CEFuncProc, ExtCtrls, StdCtrls,
  ComCtrls, LResources;

type
  TfrmDisassemblyscan = class;

  TDisassemblerthread=class(tthread)
  private
    foundline: string;
    disassembler: TDisassembler; //this thread specific disassembler
  public
    currentaddress:ptrUint;
    startaddress: ptrUint;
    strings: array of string;
    ownerform: TfrmDisassemblyscan;
    procedure execute; override;
    procedure foundone;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  { TfrmDisassemblyscan }

  TfrmDisassemblyscan = class(TForm)
    btnCancel: TButton;
    ListBox1: TListBox;
    Label1: TLabel;
    Panel1: TPanel;
    Timer1: TTimer;
    procedure Panel1Click(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnCancelClick(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    Disassemblerthread: TDisassemblerthread;
  public
    { Public declarations }
    startaddress: ptrUint;
    stringtofind: string;
  end;


implementation


uses MemoryBrowserFormUnit;

procedure TDisassemblerthread.foundone;
begin
  ownerform.ListBox1.Items.Add(foundline)
end;

constructor TDisassemblerthread.create(suspended: boolean);
begin
  inherited create(suspended);

  disassembler:=TDisassembler.Create;
end;

destructor TDisassemblerthread.destroy;
begin
  if disassembler<>nil then
    freeandnil(disassembler);

  inherited destroy;
end;

procedure TDisassemblerthread.execute;
var x: ptrUint;
    i,j: ptrUint;
    br: dword;
    d,y: string;
    found: boolean;

    maxaddress: ptruint;

begin
  x:=startaddress;
  currentaddress:=x;
  maxaddress:=currentaddress;

  while not terminated and (maxaddress<=x) do
  begin
    maxaddress:=currentaddress;

    currentaddress:=x;
    if (x mod 4096) = 0 then
    begin
      i:=0;
      if not readprocessmemory(processhandle,pointer(x),@i,processhandler.pointersize,br) then
      begin
        inc(x,4096);
        continue;
      end;
    end;

    d:=uppercase(disassembler.disassemble(x,y));
    y:=d;

    found:=true;
    for i:=0 to length(strings)-1 do
    begin
      j:=pos(strings[i],d);
      if j>0 then
        d:=copy(d,j+length(strings[i]),length(d))
      else
      begin
        found:=false;
        break;
      end;
    end;

    if found then
    begin
      foundline:=y;
      synchronize(foundone);
    end;

  end;
end;

procedure TfrmDisassemblyscan.Timer1Timer(Sender: TObject);
begin
  if disassemblerthread<>nil then
    label1.caption:=inttohex(disassemblerthread.currentaddress,8);

end;

procedure TfrmDisassemblyscan.Panel1Click(Sender: TObject);
begin

end;

procedure TfrmDisassemblyscan.Panel1Resize(Sender: TObject);
begin
  btnCancel.top:=panel1.height-btnCancel.clientheight-2;
end;

procedure TfrmDisassemblyscan.FormShow(Sender: TObject);
var i,j: integer;
    c: integer;
begin
  stringtofind:=uppercase(stringtofind);

  //split up into different strings when wildcards are used
  disassemblerthread:=Tdisassemblerthread.Create(true);

  setlength(disassemblerthread.strings,1);
  for i:=1 to length(stringtofind) do
  begin
    if stringtofind[i]<>'*' then
      disassemblerthread.strings[length(disassemblerthread.strings)-1]:=disassemblerthread.strings[length(disassemblerthread.strings)-1]+stringtofind[i]
    else
      setlength(disassemblerthread.strings,length(disassemblerthread.strings)+1);
  end;

  c:=0;
  for i:=0 to length(disassemblerthread.strings)-1 do
  begin
    if disassemblerthread.strings[i]='' then
    begin
      for j:=i to length(disassemblerthread.strings)-2 do
        disassemblerthread.strings[j]:=disassemblerthread.strings[j+1];
    end else inc(c);
  end;


  if c=0 then
  begin
    disassemblerthread.Free;
    disassemblerthread:=nil;
    close;
  end
  else
  begin
    setlength(disassemblerthread.strings,c);
    disassemblerthread.startaddress:=memorybrowser.disassemblerview.TopAddress;
    disassemblerthread.ownerform:=self;
    disassemblerthread.start;
  end;
end;

procedure TfrmDisassemblyscan.FormDestroy(Sender: TObject);
begin
  if disassemblerthread<>nil then
  begin
    disassemblerthread.Terminate;
    disassemblerthread.WaitFor;
    disassemblerthread.Free;
    disassemblerthread:=nil;
  end;
end;

procedure TfrmDisassemblyscan.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmDisassemblyscan.btnCancelClick(Sender: TObject);
begin
  if disassemblerthread<>nil then
  begin
    disassemblerthread.Terminate;
    disassemblerthread.WaitFor;
    disassemblerthread.Free;
    disassemblerthread:=nil;

    btnCancel.Caption:='Close';
  end else close;

end;

procedure TfrmDisassemblyscan.ListBox1DblClick(Sender: TObject);
var x: ptrUint;
   err: integer;
   s:string;
begin
  if listbox1.itemindex<>-1 then
  begin
    s:=listbox1.Items[listbox1.itemindex];
    s:=copy(s,1,pos('-',s)-2);
    val('$'+s,x,err);
    memorybrowser.disassemblerview.SelectedAddress:=x;
  end;
end;

initialization
  {$i frmDisassemblyscanunit.lrs}

end.
