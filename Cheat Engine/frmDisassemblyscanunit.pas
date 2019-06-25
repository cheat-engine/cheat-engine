unit frmDisassemblyscanunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,disassembler,{$ifndef net}NewKernelHandler,{$endif}CEFuncProc, ExtCtrls, StdCtrls,
  ComCtrls, LResources, LCLProc, Menus, strutils, OldRegExpr, RegExpr, Clipbrd;

type
  TAssemblyScanResultFoundEvent=procedure(address: ptruint; s:string) of object;
  TfrmDisassemblyscan = class;

  TDisassemblerthread=class(tthread)
  private
    foundaddress: ptruint;
    foundline: string;
    disassembler: TDisassembler; //this thread specific disassembler
    function checkAddress(x: ptruint): ptruint;
  public
    currentaddress:ptrUint;
    startaddress: ptrUint;
    stopaddress: ptruint;
    regexpressions: array of TRegExpr;
    ownerform: TfrmDisassemblyscan;
    procedure execute; override;
    procedure foundone;
    constructor create(suspended: boolean);
    destructor destroy; override;
  end;

  { TfrmDisassemblyscan }

  TfrmDisassemblyscan = class(TForm)
    btnCancel: TButton;
    asImageList: TImageList;
    ListBox1: TListBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
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
    fStringsToFind: Tstrings; //actually a tstringlist
    fOnResultFound: TAssemblyScanResultFoundEvent;
    fOnScanDone: TNotifyEvent;
    procedure setStringsToFind(s: tstrings);
    procedure ResultFound(address: ptruint; s: string);
  public
    { Public declarations }
    startaddress: ptrUint;
    stopaddress: ptruint;
    property stringstofind: tstrings read fStringsToFind write setStringsToFind;
    property OnResultFound: TAssemblyScanResultFoundEvent read fOnResultFound write fOnResultFound;
    property OnScanDone: TNotifyEvent read fOnScanDone write fOnScanDone;
  end;


implementation


uses MemoryBrowserFormUnit, ProcessHandlerUnit;

resourcestring
  rsDone = 'Done';
  rsDSScanError = 'scan error';
  rsDSClose = 'Close';

procedure TDisassemblerthread.foundone;
begin
  ownerform.ResultFound(foundaddress, foundline);
end;

constructor TDisassemblerthread.create(suspended: boolean);
begin
  inherited create(suspended);

  disassembler:=TDisassembler.Create;
end;

destructor TDisassemblerthread.destroy;
var i: integer;
begin
  if disassembler<>nil then
    freeandnil(disassembler);

  for i:=0 to length(regexpressions)-1 do
    if regexpressions[i]<>nil then
      regexpressions[i].Free;

  inherited destroy;
end;


function TDisassemblerthread.checkAddress(x: ptruint): PtrUInt;
//check this address if it's the correct address.
//if so, add to the list
//Return the address of the next instruction
var ok: boolean;
   d: string;
   y: string;

   i,j: integer;
   matchpos,offset: integer;
   address: ptruint;
begin
  result:=0;
  address:=x;
  for i:=0 to length(regexpressions)-1 do
  begin
    //check if it confirms to the search querry

    //disassemble
    d:=uppercase(disassembler.disassemble(x,y));
    if i=0 then
    begin
      foundline:=d;
      result:=x; //if it's the first line return this address
    end;


    d:=d+'  ';   //why???? WHY???????
    matchpos:=0;
    offset:=1;

    ok:=regexpressions[i].Exec(d);

    if (not ok) or (regexpressions[i].MatchPos[0]=0) then exit;

    //if RegExprPos(regexpressions[i],pchar(d),index,len)=false then exit; //if not a match then exit
  end;

  //still here so a match
  foundaddress:=address;
  synchronize(foundone);
end;

procedure TDisassemblerthread.execute;
var x: ptrUint;
    i,j: ptrUint;
    br: ptrUint;
    d,y: string;
    found: boolean;

    maxaddress: ptruint;

begin
  try
    x:=startaddress;
    currentaddress:=x;
    maxaddress:=currentaddress;

    while (not terminated) and (maxaddress<=x) and (currentaddress<stopaddress) do
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

      x:=checkAddress(x);

    end;
  except
    on e:exception do
      messagebox(0,pchar(e.message),pchar(rsDSScanError), 0);
  end;
end;

procedure TfrmDisassemblyscan.Timer1Timer(Sender: TObject);
begin
  if disassemblerthread<>nil then
  begin
    if disassemblerthread.Finished=false then
    begin
      label1.caption:=inttohex(disassemblerthread.currentaddress,8);

      if Disassemblerthread.stopaddress>disassemblerthread.startaddress then
        progressbar1.Position:=trunc((disassemblerthread.currentaddress-disassemblerthread.startaddress)/(Disassemblerthread.stopaddress-disassemblerthread.startaddress)*100);
    end
    else
    begin
      label1.caption:=rsDone;
      progressbar1.Position:=100;
      if assigned(fOnScanDone) then
        fOnScanDone(self);
    end;
  end;
end;

procedure TfrmDisassemblyscan.MenuItem1Click(Sender: TObject);
begin
  listbox1.OnDblClick(listbox1);
end;

procedure TfrmDisassemblyscan.MenuItem2Click(Sender: TObject);
var
   i: integer;
   sl: tstringlist;
begin
  sl:=tstringlist.create;

  for i:=0 to listbox1.Items.count-1 do
    if listbox1.Selected[i] then
      sl.add(listbox1.items[i]);

  clipboard.AsText:=sl.text;
  sl.free;
end;

procedure TfrmDisassemblyscan.Panel1Resize(Sender: TObject);
begin
  btnCancel.top:=panel1.height-btnCancel.clientheight-2;
end;

procedure TfrmDisassemblyscan.ResultFound(address: ptruint; s: string);
begin
  ListBox1.Items.Add(s);
  if assigned(fOnResultFound) then
    fOnResultFound(address, s);
end;

procedure TfrmDisassemblyscan.FormShow(Sender: TObject);
var i,j: integer;
    c: integer;
    s: string;
begin

  //split up into different strings when wildcards are used
  disassemblerthread:=Tdisassemblerthread.Create(true);

  i:=0;
  while i<stringstofind.count do //cleanup the userinput
  begin
    if trim(stringstofind[i])='' then
      stringstofind.Delete(i)
    else
    begin
      stringstofind[i]:=StringReplace(EscapeStringForRegEx(stringstofind[i]), '\*','.*',[rfReplaceAll]);
      inc(i);
    end;
  end;

  setlength(disassemblerthread.regexpressions,stringstofind.count);
  for i:=0 to length(Disassemblerthread.regexpressions)-1 do
    Disassemblerthread.regexpressions[i]:=nil;//init to nil

  try
    for i:=0 to length(disassemblerthread.regexpressions)-1 do //create a regular expression for each entry
    begin
      s:=stringstofind[i];
      disassemblerthread.regexpressions[i]:=TRegExpr.Create;
      disassemblerthread.regexpressions[i].Expression:=stringstofind[i];
      disassemblerthread.regexpressions[i].ModifierI:=true;
    end;
  except
    on e:exception do
    begin
      disassemblerthread.free;
      disassemblerthread:=nil;
      raise exception.create(e.message);
    end;
  end;

  disassemblerthread.startaddress:=startaddress;
  disassemblerthread.stopaddress:=stopaddress;

  disassemblerthread.ownerform:=self;
  disassemblerthread.start;
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

  if fStringsToFind<>nil then
    freeandnil(fStringsToFind);
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

    btnCancel.Caption:=rsDSClose;
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

procedure TfrmDisassemblyscan.setStringsToFind(s: tstrings);
begin
  if fStringsToFind=nil then
    fStringsToFind:=tstringlist.create;

  fStringsToFind.Assign(s);
end;

initialization
  {$i frmDisassemblyscanunit.lrs}

end.
