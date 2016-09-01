unit frmCodecaveScannerUnit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls{$ifdef netclient},netapis{$else},NewKernelHandler{$endif},CEFuncProc,
  ExtCtrls, Menus, clipbrd, LResources, commonTypeDefs;

type TCodeCaveScanner=class(tthread)
  private
    found: dword;
    progress:ptrUint;
    procedure updateprogressbar;
    procedure done;
    procedure foundone;
  public
    startaddress:ptrUint;
    stopaddress:ptrUint;
    size:dword;
    alsonx:boolean;
    procedure execute; override;
end;

type

  { TfrmCodecaveScanner }

  TfrmCodecaveScanner = class(TForm)
    lbCodecaveList: TListBox;
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    btnStart: TButton;
    editStart: TEdit;
    editStop: TEdit;
    editSize: TEdit;
    Panel2: TPanel;
    cbNoExecute: TCheckBox;
    ProgressBar1: TProgressBar;
    PopupMenu1: TPopupMenu;
    Copytoclipboard1: TMenuItem;
    procedure btnStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure lbCodecaveListDblClick(Sender: TObject);
    procedure Copytoclipboard1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    codecavescanner: TCodecavescanner;
  end;

var
  frmCodecaveScanner: TfrmCodecaveScanner;

implementation


uses MainUnit2, MemoryBrowserFormUnit, ProcessHandlerUnit, Globals, Parsers;

resourcestring
  rsPleaseProvideAValidStartAddress = 'Please provide a valid start address';
  rsPleaseProvideAValidStopAddress = 'Please provide a valid stop address';
  rsPleaseTellMeYouDonTNeedACodeCaveThisSmall = 'Please tell me you don''t need a code cave this small!!!';
  rsPleaseProvideAValidSizeForTheWantedCodeCave = 'Please provide a valid size for the wanted code cave';
  rsClosingThisWindowWillAlsoStopTheScannerAreYouSure = 'Closing this window will also stop the scanner. Are you sure?';

procedure TCodecavescanner.updateprogressbar;
begin
  if frmcodecavescanner<>nil then
    frmcodecavescanner.ProgressBar1.Position:=progress;
end;

procedure TCodecavescanner.foundone;
begin
  if frmCodecaveScanner<>nil then
  begin
    frmCodecaveScanner.lbCodecaveList.Items.Add(IntToHex(found,8));
    if frmCodecaveScanner.lbCodecaveList.Items.Count>30000 then terminate;  //too many found
  end;
end;

procedure TCodecavescanner.done;
begin
  if frmCodecaveScanner<>nil then
  begin
    frmCodecaveScanner.btnStart.caption:=strStart;
    frmCodecaveScanner.codecavescanner:=nil;
    frmCodecaveScanner.progressbar1.Position:=0;
  end;
end;

procedure TCodecavescanner.execute;
var mbi: _MEMORY_BASIC_INFORMATION;
    currentpos: ptrUint;
    a: boolean;
    i,j: integer;
    x:ptrUint;
    memoryregion:array of tmemoryregion;
    currentbyte: byte;
    bytecount:dword;
    buf: array of byte;
begin
 //not going to add much optimization to the scanroutines here
  freeonterminate:=true;

  try

  currentpos:=startaddress;
  setlength(buf,buffersize);


  while (not terminated) and (currentpos<stopaddress) do
  begin
    progress:=currentpos-startaddress;
    synchronize(updateprogressbar);

    //find the memoryranges to scan
    virtualqueryEx(processhandle,pointer(currentpos),mbi,sizeof(mbi));

    if alsonx then
      a:=(mbi.AllocationProtect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_READONLY or PAGE_EXECUTE_WRITECOPY))>0
    else
      a:=(mbi.AllocationProtect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY))>0;

    if not a then
    begin
      inc(currentpos,mbi.RegionSize);
      continue;
    end;

    //now scan the memory in chunks of "buffersize"
    x:=mbi.RegionSize;
    setlength(memoryregion,1+(x div buffersize));
    i:=0;
    while x>0 do
    begin
      memoryregion[i].BaseAddress:=ptrUint(mbi.BaseAddress)+i*buffersize;
      if x<buffersize then
        memoryregion[i].MemorySize:=x
      else
        memoryregion[i].MemorySize:=buffersize;

      if i>0 then memoryregion[i].IsChild:=true;

      dec(x,memoryregion[i].MemorySize);
      inc(i);
    end;

    //stop the


    //now scan the memory
    currentbyte:=0;
    bytecount:=0;
    i:=0;

    while (not terminated) and (i<length(memoryregion)) do
    begin
      progress:=memoryregion[i].BaseAddress-startaddress;
      synchronize(updateprogressbar);

      //read the mem
      if ReadProcessmemory(processhandle,pointer(memoryregion[i].BaseAddress),@buf[0],memoryregion[i].MemorySize,x) then
      begin
        //everything ok
        for j:=0 to memoryregion[i].MemorySize-1 do
        begin
          if buf[j]=currentbyte then
            inc(bytecount)
          else
          begin
            currentbyte:=buf[j];
            bytecount:=1;
          end;

          if bytecount=size then //found one
          begin
            found:=memoryregion[i].BaseAddress+j-bytecount+1;
            synchronize(foundone);
          end;
        end;
      end;

      inc(i);
    end;



    inc(currentpos,mbi.RegionSize);
  end;

  finally
    synchronize(done);
  end;
end;

procedure TfrmCodecaveScanner.btnStartClick(Sender: TObject);
var startaddress,stopaddress:ptrUint;
    bytelength: dword;
begin
{
start the thread that scans the memory for a array of the same bytes in read-
only memory
}
  if codecavescanner=nil then
  begin
    try
      startaddress:=StrToQWordEx('$'+editstart.text);
    except
      raise exception.Create(rsPleaseProvideAValidStartAddress);
    end;

    try
      stopaddress:=StrToQWordEx('$'+editStop.text);
    except
      raise exception.Create(rsPleaseProvideAValidStopAddress);
    end;

    try
      bytelength:=StrToQWordEx('$'+editsize.Text);
      if bytelength<3 then raise exception.Create(rsPleaseTellMeYouDonTNeedACodeCaveThisSmall);
    except
      raise exception.Create(rsPleaseProvideAValidSizeForTheWantedCodeCave);
    end;
    codecavescanner:=TCodecavescanner.create(true);
    codecavescanner.startaddress:=startaddress;
    codecavescanner.stopaddress:=stopaddress;
    codecavescanner.size:=bytelength;
    codecavescanner.AlsoNX:=cbnoexecute.checked;

    progressbar1.Position:=0;
    progressbar1.Max:=stopaddress-startaddress;
    btnStart.caption:=strStop;
    lbCodecaveList.Clear;
    codecavescanner.start;
  end else
  begin
    codecavescanner.terminate;
    codecavescanner:=nil;
    progressbar1.Position:=0;
    btnstart.Caption:=strStart;
  end;
end;

procedure TfrmCodecaveScanner.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if codecavescanner<>nil then
    codecavescanner.Terminate;

  codecavescanner:=nil;
end;

procedure TfrmCodecaveScanner.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if codecavescanner<>nil then
    canclose:=messagedlg(rsClosingThisWindowWillAlsoStopTheScannerAreYouSure, mtconfirmation, [mbyes, mbno], 0)=mryes;
end;

procedure TfrmCodecaveScanner.FormShow(Sender: TObject);
var fh: integer;
begin
  fh:=GetFontData(font.reference.Handle).Height;
  editstart.font.height:=fh;
  editstop.font.height:=fh;
  editsize.font.height:=fh;
end;

procedure TfrmCodecaveScanner.lbCodecaveListDblClick(Sender: TObject);
begin
  if lbCodecaveList.ItemIndex<>-1 then
    memorybrowser.memoryaddress:=StrToInt('$'+lbCodecaveList.Items[lbCodecaveList.itemindex]);

end;

procedure TfrmCodecaveScanner.Copytoclipboard1Click(Sender: TObject);
var i: integer;
    s: string;
begin
  S:='';
  for i:=0 to lbCodecaveList.count-1 do
    if lbCodecaveList.Selected[i] then
      s:=s+lbCodecaveList.Items[i]+#13#10;

  if s<>'' then
  begin
    s:=copy(s,1,length(s)-2);
    Clipboard.SetTextBuf(pchar(s));
  end;

end;

initialization
  {$i frmCodecaveScannerUnit.lrs}

end.
