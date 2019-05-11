unit frmCodecaveScannerUnit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,{$ifndef net}NewKernelHandler,{$endif}CEFuncProc,
  ExtCtrls, Menus, Clipbrd, LResources, commonTypeDefs, symbolhandler;

type TCodeCaveScanner=class(tthread)
  private
    found:qword;
    progress:integer;
    curraddr:qword;
    procedure updatelabel;
    procedure updateprogressbar;
    procedure done;
    procedure foundone;
  public
    startaddress:ptrUint;
    stopaddress:ptrUint;
    size:qword;
    alsonx:boolean;
    procedure execute; override;
end;

type

  { TfrmCodecaveScanner }

  TfrmCodecaveScanner = class(TForm)
    btnStart: TButton;
    editSize: TEdit;
    editStart: TEdit;
    editStop: TEdit;
    sfcImageList: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbCodecaveList: TListBox;
    Panel1: TPanel;
    Panel2: TPanel;
    cbNoExecute: TCheckBox;
    Panel3: TPanel;
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


uses MainUnit2, MemoryBrowserFormUnit, ProcessHandlerUnit, Globals, Parsers,
  DPIHelper, Math;

resourcestring
  rsPleaseProvideAValidStartAddress = 'Please provide a valid start address';
  rsPleaseProvideAValidStopAddress = 'Please provide a valid stop address';
  rsPleaseTellMeYouDonTNeedACodeCaveThisSmall = 'Please tell me you don''t need a code cave this small!!!';
  rsPleaseProvideAValidSizeForTheWantedCodeCave = 'Please provide a valid size for the wanted code cave';
  rsClosingThisWindowWillAlsoStopTheScannerAreYouSure = 'Closing this window will also stop the scanner. Are you sure?';

procedure TCodecavescanner.updatelabel;
begin
  if frmcodecavescanner<>nil then
    frmcodecavescanner.Label4.Caption:=inttohex(curraddr,8);
end;

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
    frmCodecaveScanner.Label4.Caption:='';
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
    progress:=trunc(currentpos/stopaddress*1000);
    curraddr:=currentpos;
    synchronize(updateprogressbar);
    synchronize(updatelabel);

    //find the memoryranges to scan
    virtualqueryEx(processhandle,pointer(currentpos),mbi,sizeof(mbi));

    if alsonx then
      a:=(mbi.Protect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY or PAGE_READONLY or PAGE_READWRITE))>0
    else
      a:=(mbi.Protect and (PAGE_EXECUTE or PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE or PAGE_EXECUTE_WRITECOPY))>0;

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
      progress:=trunc(memoryregion[i].BaseAddress/stopaddress*1000);
      curraddr:=memoryregion[i].BaseAddress;
      synchronize(updateprogressbar);
      synchronize(updatelabel);

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
    bytelength: Qword;
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
      startaddress:=symhandler.getAddressFromName(editstart.text);
    end;

    try
      stopaddress:=StrToQWordEx('$'+editstop.text);
    except
      stopaddress:=symhandler.getAddressFromName(editstop.text);
    end;

    try
      bytelength:=StrToQWordEx('$'+editsize.Text);
      if bytelength<3 then raise exception.Create(rsPleaseTellMeYouDonTNeedACodeCaveThisSmall);
    except
      raise exception.Create(rsPleaseProvideAValidSizeForTheWantedCodeCave);
    end;

    if startaddress>stopaddress then
    begin  //xor swap
      startaddress:=startaddress xor stopaddress;
      stopaddress:=stopaddress xor startaddress;
      startaddress:=startaddress xor stopaddress;
    end;

    codecavescanner:=TCodecavescanner.create(true);
    codecavescanner.startaddress:=startaddress;
    codecavescanner.stopaddress:=stopaddress;
    codecavescanner.size:=bytelength;

    //execute protection on by default on 64bit targets
    if not processhandler.is64bit then
       codecavescanner.AlsoNX:=cbnoexecute.checked
    else
       codecavescanner.AlsoNX:=false;

    progressbar1.Min:=0;
     progressbar1.Max:=1000;
    progressbar1.Position:=0;
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
var
  fh: integer;
  b: TBitmap;
  preferedwidth: integer;
begin
  fh:=GetFontData(font.reference.Handle).Height;
  editstart.font.height:=fh;
  editstop.font.height:=fh;
  editsize.font.height:=fh;

  //execute protection on by default on 64bit targets
  if processhandler.is64bit then
  begin
    //init just once if needed
    if (editstop.Text = '') or (editstart.Text = '') then   // if not initialized
     begin
        editstop.text:='7FFFFFFFFFFFFFFF';
        editstart.Text:='0000000000000000';
     end;
    cbNoExecute.checked:=false;
    cbNoExecute.Enabled:=false;
    cbNoExecute.visible:=false;
  end
  else
  begin
    //init just once if needed
    if (editstop.Text = '') or (editstart.Text = '') then   // if not initialized
    begin
       editstop.text:='7FFFFFFF';
       editstart.Text:='00000000';
    end;
  end;
  b:=tbitmap.Create;
  b.canvas.Font:=editstart.font;

  editStart.Constraints.MinWidth:=dpihelper.GetEditBoxMargins(editstart)+canvas.GetTextWidth(' XXXXXXXXXXXXXXXX ');
  editStop.Constraints.MinWidth:= editStart.Constraints.MinWidth;

  editStart.Width:=editStart.Constraints.MinWidth;
  editStop.Width:=editStart.Constraints.MinWidth;

  editSize.Constraints.MinWidth:=dpihelper.GetEditBoxMargins(editstart)+canvas.GetTextWidth(' XXXXX ');
  editSize.Width:=editSize.Constraints.MinWidth;

  progressbar1.height:=ceil(progressbar1.height*dpihelper.getDPIScaleFactor);

  DoAutoSize;
  autosize:=false;
  preferedwidth:=canvas.GetTextWidth('XXXXXXXXXXXXXX - XXXXXX')+panel1.width;
  if clientwidth<preferedwidth then
    clientwidth:=preferedwidth;
end;

procedure TfrmCodecaveScanner.lbCodecaveListDblClick(Sender: TObject);
begin
  if lbCodecaveList.ItemIndex<>-1 then
    memorybrowser.memoryaddress:=StrToQwordEx('$'+lbCodecaveList.Items[lbCodecaveList.itemindex]);

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
