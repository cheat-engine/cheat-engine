unit frmDisassemblyscanunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,disassembler,{$ifndef net}newkernelhandler,{$endif}cefuncproc, ExtCtrls, StdCtrls,
  ComCtrls;

type
  TfrmDisassemblyscan = class;

  TDisassemblerthread=class(tthread)
  private
    foundline: string;
  public
    currentaddress:dword;
    startaddress: dword;
    strings: array of string;
    ownerform: TfrmDisassemblyscan;
    procedure execute; override;
    procedure foundone;
  end;

  TfrmDisassemblyscan = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    Label1: TLabel;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
    Disassemblerthread: TDisassemblerthread;
  public
    { Public declarations }
    startaddress: dword;
    stringtofind: string;
  end;


implementation

{$R *.dfm}

uses MemoryBrowserFormUnit;

procedure TDisassemblerthread.foundone;
begin
  ownerform.ListBox1.Items.Add(foundline)
end;

procedure TDisassemblerthread.execute;
var x,i,j: dword;
    d,y: string;
    found: boolean;
begin
  x:=startaddress;
  while not terminated and (x<$fffffff0) do
  begin
    currentaddress:=x;
    if (x mod 4096) = 0 then
    begin
      if not readprocessmemory(processhandle,pointer(x),@i,4,j) then
      begin
        inc(x,4096);
        continue;
      end;
    end;

    d:=uppercase(disassemble(x,y));
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
    disassemblerthread.startaddress:=memorybrowser.Disassembleraddress;
    disassemblerthread.ownerform:=self;
    disassemblerthread.Resume;
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

procedure TfrmDisassemblyscan.Button1Click(Sender: TObject);
begin
  if disassemblerthread<>nil then
  begin
    disassemblerthread.Terminate;
    disassemblerthread.WaitFor;
    disassemblerthread.Free;
    disassemblerthread:=nil;

    button1.Caption:='Close';
  end else close;

end;

procedure TfrmDisassemblyscan.ListBox1DblClick(Sender: TObject);
var x: dword;
   err: integer;
begin
  if listbox1.itemindex<>-1 then
  begin
    val('$'+listbox1.Items[listbox1.itemindex],x,err);
    MemoryBrowser.dselected:=x;
    MemoryBrowser.Disassembleraddress:=x;
    MemoryBrowser.updatedisassemblerview;
  end;
end;

end.
