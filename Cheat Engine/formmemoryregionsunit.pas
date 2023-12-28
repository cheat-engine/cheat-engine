unit formmemoryregionsunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,CEFuncProc, ComCtrls, Menus,OpenSave,NewKernelHandler, LResources, betterControls;

type
  tmoreinfo = record
    address: ptrUint;
    size: qword;
    isreadable: boolean;
  end;

  PMoreInfo=^TMoreInfo;

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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListView1ColumnClick(Sender: TObject; Column: TListColumn);
    procedure ListView1Compare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure ListView1Resize(Sender: TObject);
    procedure Saveselectedregions1Click(Sender: TObject);
    procedure SelectAllReadableMemory1Click(Sender: TObject);
    procedure Setselectedregionstobewritable1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMemoryRegions: TFormMemoryRegions;

type
  TGetMappedFileName=function (hProcess: HANDLE; lpv: LPVOID; lpFilename: LPTSTR; nSize: DWORD): DWORD; stdcall;

 { TWIN32_MEMORY_REGION_INFORMATION=record
    AllocationBase: pointer;
    AllocatioinProtect: DWORD;
    Flags: DWORD;
    RegionSize: SIZE_T;
    CommitSize: SIZE_T;
  end;
  PWIN32_MEMORY_REGION_INFORMATION=^TWIN32_MEMORY_REGION_INFORMATION;

  const
    MF_Private=1 shl 1;
    MF_MappedDataFile=1 shl 2;
    MF_MappedImage=1 shl 3;
    MF_MappedPageFile=1 shl 4;
    MF_MappedPhysical=1 shl 5;
    MF_DirectMapped=1 shl 6;      }


var GetMappedFileName: TGetMappedFileName;
    //QueryVirtualMemoryInformation: function(hProcess: HANDLE; address: LPVOID; class0: dword; meminfo: PWIN32_MEMORY_REGION_INFORMATION; meminfosize: size_t; out returnsize: size_t): boolean;



implementation

uses formsettingsunit, MemoryBrowserFormUnit, ProcessHandlerUnit;


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

  {  meminfo: TWIN32_MEMORY_REGION_INFORMATION;
    meminfosize: size_t; }
    li: TListitem;
    mi: PMoreInfo;
begin
  {$ifdef windows}
  if DBKLoaded then
  begin
    kernelmode:=ssCtrl in GetKeyShiftState;
    if not kernelmode then
      statusbar1.Visible:=true;
  end;
  {$endif}
  listview1.BeginUpdate;
  try
    for i:=0 to listview1.items.Count-1 do
      freemem(listview1.items[i].Data);

    listview1.Clear;

    //query the process for the memory regions
    address:=0;
    mbi.RegionSize:=$1000;

    while (GetRegionInfo(processhandle,pointer(address),mbi,sizeof(mbi), mappedfilename)<>0) and ((address+mbi.RegionSize)>address) do
    begin
      getmem(mi, sizeof(tmoreinfo));
      mi^.address:=ptrUint(mbi.BaseAddress);
      mi^.size:=mbi.RegionSize;
      mi^.isreadable:=(mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0);

      li:=ListView1.Items.Add;
      li.Data:=mi;

      li.Caption:=IntToHex(PtrUInt(mbi.BaseAddress),8);

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
      li.SubItems.add(temp);

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

      {$ifdef darwin}
      temp:=temp+' ('+inttostr(mbi.macprotect)+')';
      {$endif}
      li.SubItems.add(temp);

      {$ifdef darwin}
      temp:=inttostr(mbi.MaxProtect);
      li.SubItems.add(temp);
      {$endif}

      if mbi._Type=MEM_IMAGE	then li.SubItems.add(rsImage) else
      if mbi._Type=MEM_MAPPED then li.SubItems.add(rsMapped) else
      if mbi._Type=MEM_PRIVATE	then li.SubItems.add(rsPrivate) else
      li.SubItems.add('-');

      // regionlist.Items.Add(str);

      li.SubItems.add(inttohex(mbi.regionsize,1));

     // if mbi._Type=MEM_MAPPED then
     { if assigned(QueryVirtualMemoryInformation) then
      begin
        meminfosize:=sizeof(meminfo);
        if QueryVirtualMemoryInformation(processhandle, mbi.BaseAddress,0, @meminfo, sizeof(meminfo),meminfosize) then
        begin
          mappedfilename:=inttohex(meminfo.Flags,1)+mappedfilename;
        end;
      end; }

      li.SubItems.Add(mappedfilename);



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
  finally
    listview1.EndUpdate;
  end;
end;

procedure TFormMemoryRegions.FormCreate(Sender: TObject);
var ci: TListColumn;
begin
  {$ifdef darwin}
  ci:=listview1.Columns.Add;
  ci.Index:=4;
  ci.Caption:='Max Protect';
  ci.Tag:=7;
  {$endif}
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

procedure TFormMemoryRegions.ListView1ColumnClick(Sender: TObject; Column: TListColumn);
begin

end;

procedure TFormMemoryRegions.ListView1Compare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
var
  c: integer;
  s1,s2: string;
  q: Int64;
begin
  c:=listview1.SortColumn;

  if c=0 then
  begin
    if PMoreInfo(item1.Data)^.address=PMoreInfo(item2.Data)^.address then
    begin
      compare:=0;
      exit;
    end;

    if PMoreInfo(item1.Data)^.address>PMoreInfo(item2.Data)^.address then
      compare:=1
    else
      compare:=-1;
  end
  else
  if c={$ifdef darwin}6{$else}5{$endif} then
  begin
    if PMoreInfo(item1.Data)^.size=PMoreInfo(item2.Data)^.size then
    begin
      compare:=0;
      exit;
    end;

    if PMoreInfo(item1.Data)^.size>PMoreInfo(item2.Data)^.size then
      compare:=1
    else
      compare:=-1;
  end
  else
  begin
    s1:=item1.SubItems[c-1];
    s2:=item2.SubItems[c-1];
    compare:=CompareStr(s1,s2);
  end;

  if listview1.SortDirection=sdDescending then
    compare:=-compare;


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
    mi: PMoreInfo;
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

        mi:=PMoreInfo(listview1.Items[i].Data);
        savecem(savedialog1.FileName+'.m'+format('%.4d',[nr]), mi^.address,mi^.size);
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
  begin
    if PMoreInfo(listview1.Items[i].Data)^.isreadable then
      listview1.Items[i].Selected:=true;
  end;
end;

procedure TFormMemoryRegions.Setselectedregionstobewritable1Click(
  Sender: TObject);
var res: word;
    copyonwrite: boolean;
    i: integer;
begin
  {$ifdef windows}
  res:=MessageDlg(rsDoYouWantToUseTheCOPYONWRITEBit, mtConfirmation, [mbyes, mbno, mbcancel], 0);
  if res=mrcancel then exit;
  if res=mrNo then copyonwrite:=false else copyonwrite:=true;

  for i:=0 to listview1.Items.Count-1 do
    if listview1.Items[i].Selected then
    begin
      //change this memory's protection
      MakeWritable(PMoreInfo(listview1.Items[i].Data)^.address,PMoreInfo(listview1.Items[i].Data)^.size,copyonwrite);
    end;

  {$endif}
end;

procedure TFormMemoryRegions.ListView1DblClick(Sender: TObject);
var li: TListItem;
    mi: PMoreInfo;
begin
  if (listview1.SelCount<>0) and (listview1.ItemIndex<>-1) then
  begin
    li:=listview1.Items[listview1.ItemIndex];
    if li<>nil then
    begin
      mi:=pmoreinfo(li.Data);
      if mi<>nil then
        MemoryBrowser.memoryaddress:=mi^.address;
    end;
  end;

end;

procedure TFormMemoryRegions.PopupMenu1Popup(Sender: TObject);
begin


  setselectedregionstobewritable1.enabled:={$ifdef windows}DBKLoaded{$else}false{$endif};

end;

procedure LoadPsApi;
var psapi: THandle;
    m: THandle;
    s: integer;
begin
  {$ifdef windows}
  psapi:=LoadLibrary('psapi.dll');

  GetMappedFileName:=GetProcAddress(psapi,'GetMappedFileNameA');
     {
  m:=LoadLibrary('Api-ms-win-core-memory-l1-1-4.dll');
  if m<>0 then
    QueryVirtualMemoryInformation:=GetProcAddress(m,'QueryVirtualMemoryInformation');  }
  {$endif}
end;

initialization
  {$i formmemoryregionsunit.lrs}
  {$ifdef windows}
  LoadPsApi;
  {$endif}

end.
