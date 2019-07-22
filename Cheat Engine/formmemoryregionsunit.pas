unit formmemoryregionsunit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ComCtrls, Menus,opensave,NewKernelHandler, LResources;

type tmoreinfo = record
  address: ptrUint;
  size: dword;
  isreadable: boolean;
end;

type

  { TFormMemoryRegions }

  TFormMemoryRegions = class(TForm)
    Button1: TButton;
    mrImageList: TImageList;
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Saveselectedregions1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SelectAllReadableMemory1: TMenuItem;
    Setselectedregionstobewritable1: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1Resize(Sender: TObject);
    procedure Saveselectedregions1Click(Sender: TObject);
    procedure SelectAllReadableMemory1Click(Sender: TObject);
    procedure Setselectedregionstobewritable1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { Private declarations }
    moreinfo: array of tmoreinfo;
  public
    { Public declarations }
  end;

var
  FormMemoryRegions: TFormMemoryRegions;

type TGetMappedFileName=function (hProcess: HANDLE; lpv: LPVOID; lpFilename: LPTSTR; nSize: DWORD): DWORD; stdcall;
var GetMappedFileName: TGetMappedFileName;



implementation

uses formsettingsunit, MemoryBrowserFormUnit, processhandlerunit;


resourcestring
  rsRead = 'Read';
  rsReadWrite = 'Read+Write';
  rsWriteCopy = 'Write Copy';
  rsExecute = 'Execute';
  rsExecuteRead = 'Execute+Read';
  rsExecuteReadWrite = 'Execute+Read+Write';
  rsExecuteWriteCopy = 'Execute+Write Copy';
  rsNoAccess = 'No Access';
  rsGuard = 'Guard';
  rsNoCache = 'No Cache';
  rsWriteCombine = 'Write Combine';
  rsCommit = 'Commit';
  rsFree = 'Free';
  rsReserve = 'Reserve';
  rsImage = 'Image';
  rsMapped = 'Mapped';
  rsPrivate = 'Private';
  rsThereAreAlreadyMemoryfilesWithThisNameDoYouWantToD = 'There are already memoryfiles with this name. Do you want to delete them? (choosing no will add the file(s) to empty '
    +'slots)';
  rsFailedToDelete = 'Failed to delete %s';
  rsMemoryRegionsSaving = 'Memory regions - Saving(%s/%s)';
  rsThereIsNoFreeSlot = 'There is no free slot';
  rsDone = 'done';
  rsMemoryRegions = 'Memory regions';
  rsDoYouWantToUseTheCOPYONWRITEBit = 'Do you want to use the COPY-ON-WRITE bit?';

procedure TFormMemoryRegions.FormShow(Sender: TObject);
var address: PtrUInt;
    mbi : _MEMORY_BASIC_INFORMATION;
    temp:string;
    mappedfilename: string;
    i: integer;

    kernelmode: boolean=false;
begin
  if DBKLoaded then
  begin
    kernelmode:=ssCtrl in GetKeyShiftState;
    if not kernelmode then
      statusbar1.Visible:=true;
  end;


   listview1.Clear;

    //query the process for the memory regions
    address:=0;
    mbi.RegionSize:=$1000;

    while (GetRegionInfo(processhandle,pointer(address),mbi,sizeof(mbi), mappedfilename)<>0) and ((address+mbi.RegionSize)>address) do
    begin
      setlength(moreinfo,length(moreinfo)+1);
      moreinfo[length(moreinfo)-1].address:=ptrUint(mbi.BaseAddress);
      moreinfo[length(moreinfo)-1].size:=mbi.RegionSize;
      moreinfo[length(moreinfo)-1].isreadable:=(mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0);


      ListView1.Items.Add.Caption:=IntToHex(PtrUInt(mbi.BaseAddress),8);

      temp:='';
      if (PAGE_READONLY and mbi.AllocationProtect)=PAGE_READONLY then temp:=rsRead;
      if (PAGE_READWRITE and mbi.AllocationProtect)=PAGE_READWRITE then temp:=rsReadWrite;
      if (PAGE_WRITECOPY and mbi.AllocationProtect)=PAGE_WRITECOPY then temp:=rsWriteCopy;
      if (PAGE_EXECUTE and mbi.AllocationProtect)=PAGE_EXECUTE then temp:=rsExecute;
      if (PAGE_EXECUTE_READ and mbi.AllocationProtect)=PAGE_EXECUTE_READ then temp:=rsExecuteRead;
      if (PAGE_EXECUTE_READWRITE and mbi.AllocationProtect)=PAGE_EXECUTE_READWRITE then temp:=rsExecuteReadWrite;
      if (PAGE_EXECUTE_WRITECOPY and mbi.AllocationProtect)=PAGE_EXECUTE_WRITECOPY then temp:=rsExecuteWriteCopy;
      if (PAGE_NOACCESS and mbi.AllocationProtect)=PAGE_NOACCESS then temp:=rsNoAccess;
      if (PAGE_GUARD and mbi.AllocationProtect)=PAGE_GUARD then temp:=temp+'+'+rsGuard;
      if (PAGE_NOCACHE	and mbi.AllocationProtect)=PAGE_NOCACHE then temp:=temp+'+'+rsNoCache;
      if (PAGE_WRITECOMBINE and mbi.AllocationProtect)=PAGE_WRITECOMBINE then temp:=temp+'+'+rsWriteCombine;
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      case mbi.State of
        MEM_COMMIT : temp:=rsCommit;
        MEM_FREE : temp:=rsFree;
        MEM_RESERVE	: temp:=rsReserve;
      end;
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      temp:='';
      if (PAGE_READONLY and mbi.Protect)=PAGE_READONLY then temp:=rsRead;
      if (PAGE_READWRITE and mbi.Protect)=PAGE_READWRITE then temp:=rsReadWrite;
      if (PAGE_WRITECOPY and mbi.Protect)=PAGE_WRITECOPY then temp:=rsWriteCopy;
      if (PAGE_EXECUTE and mbi.Protect)=PAGE_EXECUTE then temp:=rsExecute;
      if (PAGE_EXECUTE_READ and mbi.Protect)=PAGE_EXECUTE_READ then temp:=rsExecuteRead;
      if (PAGE_EXECUTE_READWRITE and mbi.Protect)=PAGE_EXECUTE_READWRITE then temp:=rsExecuteReadWrite;
      if (PAGE_EXECUTE_WRITECOPY and mbi.Protect)=PAGE_EXECUTE_WRITECOPY then temp:=rsExecuteWriteCopy;
      if (PAGE_NOACCESS and mbi.Protect)=PAGE_NOACCESS then temp:=rsNoAccess;
      if (PAGE_GUARD and mbi.Protect)=PAGE_GUARD then temp:=temp+'+'+rsGuard;
      if (PAGE_NOCACHE	and mbi.Protect)=PAGE_NOCACHE then temp:=temp+'+'+rsNoCache;
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      if mbi._Type=MEM_IMAGE	then listview1.Items[listview1.Items.Count-1].SubItems.add(rsImage) else
      if mbi._Type=MEM_MAPPED then listview1.Items[listview1.Items.Count-1].SubItems.add(rsMapped) else
      if mbi._Type=MEM_PRIVATE	then listview1.Items[listview1.Items.Count-1].SubItems.add(rsPrivate) else
      listview1.Items[listview1.Items.Count-1].SubItems.add('-');

      // regionlist.Items.Add(str);

      listview1.Items[listview1.Items.Count-1].SubItems.add(inttohex(mbi.regionsize,1));

     // if mbi._Type=MEM_MAPPED then
      listview1.Items[listview1.Items.Count-1].SubItems.Add(mappedfilename);



      inc(address,mbi.RegionSize);

      if not kernelmode then
      begin
        if processhandler.is64Bit then
        begin
          if (address>=QWORD($8000000000000000)) then exit;

        end
        else
          if (address>=$80000000) then exit;
      end;
    end;

end;

procedure TFormMemoryRegions.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TFormMemoryRegions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TFormMemoryRegions.ListView1Resize(Sender: TObject);
var i,j: integer;
begin
  j:=0;
  for i:=0 to listview1.Columns.Count-2 do
    j:=j+listview1.Column[i].Width;

  listview1.Column[listview1.Columns.Count-1].Width:=listview1.ClientWidth-j;
end;

procedure TFormMemoryRegions.Saveselectedregions1Click(Sender: TObject);
var f:TSearchRec;
    r: word;
    i,nr: integer;
begin

  if savedialog1.execute then
  begin
    try

    zeromemory(@f,sizeof(f));
    if findfirst(savedialog1.filename+'.m*',faAnyFile,f)=0 then
    begin
      r:=messagedlg(rsThereAreAlreadyMemoryfilesWithThisNameDoYouWantToD, mtConfirmation, [mbyes, mbno, mbcancel], 0);
      if r=mrcancel then exit;

      if r=mryes then
      begin
        deletefile(extractfilepath(savedialog1.filename)+f.Name);
        while findnext(f)=0 do if not deletefile(extractfilepath(savedialog1.filename)+f.Name) then raise exception.Create(Format(rsFailedToDelete, [f.Name]));
      end;
    end;


    nr:=0;
    for i:=0 to listview1.Items.Count-1 do
    begin
      caption:=Format(rsMemoryRegionsSaving, [IntToStr(i), IntToStr(listview1.Items.Count-1)]);
      repaint;

      if listview1.Items[i].Selected then
      begin
        //find a free slot
        while (nr<10000) and (fileexists(savedialog1.FileName+'.m'+format('%.4d',[nr]))) do inc(nr);

        if nr=10000 then raise exception.Create(rsThereIsNoFreeSlot);
        savecem(savedialog1.FileName+'.m'+format('%.4d',[nr]),moreinfo[i].address,moreinfo[i].size);
      end;
    end
    finally
      showmessage(rsDone);
      caption:=rsMemoryRegions;
    end;
  end;

end;

procedure TFormMemoryRegions.SelectAllReadableMemory1Click(
  Sender: TObject);
var i: integer;
begin
  for i:=0 to listview1.Items.Count-1 do
    listview1.Items[i].Selected:=false;

  for i:=0 to listview1.Items.Count-1 do
    if moreinfo[i].isreadable then listview1.Items[i].Selected:=true;
end;

procedure TFormMemoryRegions.Setselectedregionstobewritable1Click(
  Sender: TObject);
var res: word;
    copyonwrite: boolean;
    i: integer;
begin
  res:=MessageDlg(rsDoYouWantToUseTheCOPYONWRITEBit, mtConfirmation, [mbyes, mbno, mbcancel], 0);
  if res=mrcancel then exit;
  if res=mrNo then copyonwrite:=false else copyonwrite:=true;

  for i:=0 to listview1.Items.Count-1 do
    if listview1.Items[i].Selected then
    begin
      //change this memory's protection
      MakeWritable(moreinfo[i].address,moreinfo[i].size,copyonwrite);
    end;

end;

procedure TFormMemoryRegions.ListView1DblClick(Sender: TObject);
begin

  if (listview1.SelCount<>0) then
    MemoryBrowser.memoryaddress:=moreinfo[listview1.itemindex].address;

end;

procedure TFormMemoryRegions.PopupMenu1Popup(Sender: TObject);
begin
  setselectedregionstobewritable1.enabled:=DBKLoaded;

end;

procedure LoadPsApi;
var psapi: THandle;
begin
  psapi:=LoadLibrary('psapi.dll');

  GetMappedFileName:=GetProcAddress(psapi,'GetMappedFileNameA');
end;

initialization
  {$i formmemoryregionsunit.lrs}
  LoadPsApi;

end.
