unit frmDisassemblyscanunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,disassembler,{$ifndef net}NewKernelHandler,{$endif}CEFuncProc, ExtCtrls, StdCtrls,
  ComCtrls, LResources, LCLProc, Menus, strutils, OldRegExpr, RegExpr;

type
  TfrmDisassemblyscan = class;

  TDisassemblerthread=class(tthread)
  private
    foundline: string;
    disassembler: TDisassembler; //this thread specific disassembler
    function checkAddress(x: ptruint): ptruint;
  public
    currentaddress:ptrUint;
    startaddress: ptrUint;
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
    ListBox1: TListBox;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    Timer1: TTimer;
    procedure ListBox1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure ListBox1KeyPress(Sender: TObject; var Key: char);
    procedure MenuItem1Click(Sender: TObject);
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
    stringstofind: tstrings;  //externally allocated stringlist object (should be set as a property in the future)
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
begin
  for i:=0 to length(regexpressions)-1 do
  begin
    //check if it confirms to the search querry

    //disassemble
    d:=uppercase(disassembler.disassemble(x,y));
    if i=0 then
    begin
      foundline:=d;
      result:=x; //if it's the firt line return this address
    end;


    d:=d+'  ';   //why???? WHY???????
    matchpos:=0;
    offset:=1;

    ok:=regexpressions[i].Exec(d);

    if (not ok) or (regexpressions[i].MatchPos[0]=0) then exit;

    //if RegExprPos(regexpressions[i],pchar(d),index,len)=false then exit; //if not a match then exit
  end;

  //still here so a match
  synchronize(foundone);
end;

procedure TDisassemblerthread.execute;
var x: ptrUint;
    i,j: ptrUint;
    br: dword;
    d,y: string;
    found: boolean;

    maxaddress: ptruint;

begin
  try
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

    x:=checkAddress(x);



  end;
  except
    on e:exception do
      messagebox(0,pchar(e.message),pchar('scan error'), 0);
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

procedure TfrmDisassemblyscan.ListBox1KeyPress(Sender: TObject; var Key: char);
begin

end;

procedure TfrmDisassemblyscan.MenuItem1Click(Sender: TObject);
begin
  listbox1.OnDblClick(listbox1);
end;

procedure TfrmDisassemblyscan.ListBox1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin


end;

procedure TfrmDisassemblyscan.Panel1Resize(Sender: TObject);
begin
  btnCancel.top:=panel1.height-btnCancel.clientheight-2;
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
    if trim(stringstofind[i])='' then
      stringstofind.Delete(i)
    else
    begin
      stringstofind[i]:=StringReplace(EscapeStringForRegEx(stringstofind[i]), '\*','.*',[rfReplaceAll]);


      inc(i);
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


  {
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
  else  }
  begin
   // disassemblerthread.strings:=stringstofind;
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
