unit frmCodecaveScannerUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls{$ifdef netclient},netapis{$else},newkernelhandler{$endif},cefuncproc;

type TCodeCaveScanner=class(tthread)
  private
    found: dword;
    progress:dword;
    procedure updateprogressbar;
    procedure done;
    procedure foundone;
  public
    start:dword;
    stop:dword;
    size:dword;
    alsonx:boolean;
    procedure execute; override;
end;

type
  TfrmCodecaveScanner = class(TForm)
    ListBox1: TListBox;
    btnStart: TButton;
    editStart: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    editStop: TEdit;
    cbNoExecute: TCheckBox;
    ProgressBar1: TProgressBar;
    editSize: TEdit;
    Label3: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ListBox1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    codecavescanner: TCodecavescanner;
  end;

var
  frmCodecaveScanner: TfrmCodecaveScanner;

implementation

{$R *.dfm}

uses mainunit2, MemoryBrowserFormUnit;

procedure TCodecavescanner.updateprogressbar;
begin
  if frmcodecavescanner<>nil then
    frmcodecavescanner.ProgressBar1.Position:=progress;
end;

procedure TCodecavescanner.foundone;
begin
  if frmCodecaveScanner<>nil then
  begin
    frmCodecaveScanner.ListBox1.Items.Add(IntToHex(found,8));
    if frmCodecaveScanner.ListBox1.Items.Count>30000 then terminate;  //too many found
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
    currentpos: dword;
    a: boolean;
    i,j: integer;
    x:dword;
    memoryregion:array of tmemoryregion;
    currentbyte: byte;
    bytecount:dword;
    buf: array of byte;
begin
 //not going to add much optimization to the scanroutines here
  freeonterminate:=true;

  try

  currentpos:=start;
  setlength(buf,buffersize);


  while (not terminated) and (currentpos<stop) do
  begin
    progress:=currentpos-start;
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
      memoryregion[i].BaseAddress:=dworD(mbi.BaseAddress)+i*buffersize;
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
      progress:=memoryregion[i].BaseAddress-start;
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
var start,stop:dword;
    bytelength: dword;
begin
{
start the thread that scans the memory for a array of the same bytes in read-
only memory
}
  if codecavescanner=nil then
  begin
    try
      start:=StrToInt('$'+editstart.text);
    except
      raise exception.Create('Please provide a valid start address');
    end;

    try
      stop:=StrToint('$'+editStop.text);
    except
      raise exception.Create('Please provide a valid stop address');
    end;

    try
      bytelength:=StrToInt('$'+editsize.Text);
      if bytelength<3 then raise exception.Create('Please tell me you don''t need a code cave this small!!!');
    except
      raise exception.Create('Please provide a valid size for the wanted code cave');
    end;
    codecavescanner:=TCodecavescanner.create(true);
    codecavescanner.start:=start;
    codecavescanner.stop:=stop;
    codecavescanner.size:=bytelength;
    codecavescanner.AlsoNX:=cbnoexecute.checked;

    progressbar1.Position:=0;
    progressbar1.Max:=stop-start;
    btnStart.caption:=strStop;
    listbox1.Clear;
    codecavescanner.Resume;
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
    canclose:=messagedlg('Closing this window will also stop the scanner. Are you sure?',mtconfirmation,[mbyes,mbno],0)=mryes;
end;

procedure TfrmCodecaveScanner.ListBox1DblClick(Sender: TObject);
begin
  if listbox1.ItemIndex<>-1 then
  begin
    memorybrowser.memoryaddress:=StrToInt('$'+listbox1.Items[listbox1.itemindex]);
    memorybrowser.RefreshMB;
  end;
end;

end.
