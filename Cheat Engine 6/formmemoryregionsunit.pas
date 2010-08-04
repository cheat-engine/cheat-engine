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
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    Saveselectedregions1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SelectAllReadableMemory1: TMenuItem;
    Setselectedregionstobewritable1: TMenuItem;
    N1: TMenuItem;
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

uses formsettingsunit, MemoryBrowserFormUnit;


procedure TFormMemoryRegions.FormShow(Sender: TObject);
var address: PtrUInt;
    mbi : _MEMORY_BASIC_INFORMATION;
    temp:string;
    mappedfilename: pchar;
    i: integer;
begin
  getmem(mappedfilename,256);
  try

    listview1.Clear;

    //query the process for the memory regions
    address:=0;
    mbi.RegionSize:=$1000;

    while (Virtualqueryex(processhandle,pointer(address),mbi,sizeof(mbi))<>0) and ((address+mbi.RegionSize)>address) do
    begin
      setlength(moreinfo,length(moreinfo)+1);
      moreinfo[length(moreinfo)-1].address:=ptrUint(mbi.BaseAddress);
      moreinfo[length(moreinfo)-1].size:=mbi.RegionSize;
      moreinfo[length(moreinfo)-1].isreadable:=(mbi.State=mem_commit) and ((mbi.Protect and page_guard)=0) and ((mbi.protect and page_noaccess)=0);


      ListView1.Items.Add.Caption:=IntToHex(PtrUInt(mbi.BaseAddress),8);

      temp:='';
      if (PAGE_READONLY and mbi.AllocationProtect)=PAGE_READONLY then temp:='Read';
      if (PAGE_READWRITE and mbi.AllocationProtect)=PAGE_READWRITE then temp:='Read+Write';
      if (PAGE_WRITECOPY and mbi.AllocationProtect)=PAGE_WRITECOPY then temp:='Write Copy';
      if (PAGE_EXECUTE and mbi.AllocationProtect)=PAGE_EXECUTE then temp:='Execute';
      if (PAGE_EXECUTE_READ and mbi.AllocationProtect)=PAGE_EXECUTE_READ then temp:='Execute+Read';
      if (PAGE_EXECUTE_READWRITE and mbi.AllocationProtect)=PAGE_EXECUTE_READWRITE then temp:='Execute+Read+Write';
      if (PAGE_EXECUTE_WRITECOPY and mbi.AllocationProtect)=PAGE_EXECUTE_WRITECOPY then temp:='Execute+Write Copy';
      if (PAGE_NOACCESS and mbi.AllocationProtect)=PAGE_NOACCESS then temp:='No Access';
      if (PAGE_GUARD and mbi.AllocationProtect)=PAGE_GUARD then temp:=temp+'+Guard';
      if (PAGE_NOCACHE	and mbi.AllocationProtect)=PAGE_NOCACHE then temp:=temp+'+No Cache';
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      case mbi.State of
        MEM_COMMIT :temp:='Commit';
        MEM_FREE : temp:='Free';
        MEM_RESERVE	:temp:='Reserve';
      end;
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      temp:='';
      if (PAGE_READONLY and mbi.Protect)=PAGE_READONLY then temp:='Read';
      if (PAGE_READWRITE and mbi.Protect)=PAGE_READWRITE then temp:='Read+Write';
      if (PAGE_WRITECOPY and mbi.Protect)=PAGE_WRITECOPY then temp:='Write Copy';
      if (PAGE_EXECUTE and mbi.Protect)=PAGE_EXECUTE then temp:='Execute';
      if (PAGE_EXECUTE_READ and mbi.Protect)=PAGE_EXECUTE_READ then temp:='Execute+Read';
      if (PAGE_EXECUTE_READWRITE and mbi.Protect)=PAGE_EXECUTE_READWRITE then temp:='Execute+Read+Write';
      if (PAGE_EXECUTE_WRITECOPY and mbi.Protect)=PAGE_EXECUTE_WRITECOPY then temp:='Execute+Write Copy';
      if (PAGE_NOACCESS and mbi.Protect)=PAGE_NOACCESS then temp:='No Access';
      if (PAGE_GUARD and mbi.Protect)=PAGE_GUARD then temp:=temp+'+Guard';
      if (PAGE_NOCACHE	and mbi.Protect)=PAGE_NOCACHE then temp:=temp+'+No Cache';
      listview1.Items[listview1.Items.Count-1].SubItems.add(temp);

      if mbi._Type=MEM_IMAGE	then listview1.Items[listview1.Items.Count-1].SubItems.add('Image') else
      if mbi._Type=MEM_MAPPED then listview1.Items[listview1.Items.Count-1].SubItems.add('Mapped') else
      if mbi._Type=MEM_PRIVATE	then listview1.Items[listview1.Items.Count-1].SubItems.add('Private') else
      listview1.Items[listview1.Items.Count-1].SubItems.add('-');

      // regionlist.Items.Add(str);

      listview1.Items[listview1.Items.Count-1].SubItems.add(inttohex(mbi.regionsize,1));

     // if mbi._Type=MEM_MAPPED then
      begin
        i:=GetMappedFileName(processhandle,mbi.BaseAddress, mappedfilename, 255);
        mappedfilename[i]:=#0;
        listview1.Items[listview1.Items.Count-1].SubItems.Add(mappedfilename);
      end;



      inc(address,mbi.RegionSize);
    end;
  finally
    freemem(mappedfilename);
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
      r:=messagedlg('There are already memoryfiles with this name. Do you want to delete them? (choosing no will add the file(s) to empty slots)',mtConfirmation,[mbyes,mbno,mbcancel],0);
      if r=mrcancel then exit;

      if r=mryes then
      begin
        deletefile(extractfilepath(savedialog1.filename)+f.Name);
        while findnext(f)=0 do if not deletefile(extractfilepath(savedialog1.filename)+f.Name) then raise exception.Create('Failed to delete '+f.Name);
      end;
    end;


    nr:=0;
    for i:=0 to listview1.Items.Count-1 do
    begin
      caption:='Memory regions - Saving('+IntToStr(i)+'/'+IntToStr(listview1.Items.Count-1) + ')';
      repaint;

      if listview1.Items[i].Selected then
      begin
        //find a free slot
        while (nr<10000) and (fileexists(savedialog1.FileName+'.m'+format('%.4d',[nr]))) do inc(nr);

        if nr=10000 then raise exception.Create('There is no free slot');
        savecem(savedialog1.FileName+'.m'+format('%.4d',[nr]),moreinfo[i].address,moreinfo[i].size);
      end;
    end
    finally
      showmessage('done');
      caption:='Memory regions';
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
    i,j: integer;
    currentaddress,x:ptrUint;
    buf: dword;
begin
  res:=MessageDlg('Do you want to use the COPY-ON-WRITE bit?',mtConfirmation,[mbyes,mbno,mbcancel],0);
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
