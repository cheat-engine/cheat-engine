unit PEInfounit;

{
Changed title from PE info to Portable Executable (PE) info. I have this feeling
that 'some people' (idiots) would nout understand that it isn't a packet editor
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cefuncproc, newkernelhandler, Buttons, StdCtrls, ExtCtrls,
  ComCtrls, symbolhandler, peinfofunctions;

type
  TfrmPEInfo = class(TForm)
    GroupBox2: TGroupBox;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    edtAddress: TEdit;
    modulelist: TListBox;
    LoadButton: TSpeedButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    PEItv: TTreeView;
    lbImports: TListBox;
    lbExports: TListBox;
    lbBaseReloc: TListBox;
    Button1: TButton;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    procedure LoadButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure modulelistClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    memorycopy: pbytearray;
    memorycopysize: dword;
    modulebase: dword;

    loadedmodule: pbytearray;

    procedure ParseFile(loaded: boolean);
  public
    { Public declarations }
  end;

function peinfo_getcodesize(header: pointer): dword;
function peinfo_getentryPoint(header: pointer): dword;
function peinfo_getcodebase(header: pointer): dword;
function peinfo_getdatabase(header: pointer): dword;
function peinfo_getheadersize(header: pointer): dword;


implementation

{$R *.dfm}


function peinfo_getcodesize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;
begin
  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.SizeOfCode;
end;

function peinfo_getdatabase(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;
begin
  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.BaseOfData;
end;

function peinfo_getcodebase(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;
begin
  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.BaseOfCode;
end;

function peinfo_getEntryPoint(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;
begin
  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.AddressOfEntryPoint;
end;

function peinfo_getheadersize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  if PImageDosHeader(header)^.e_magic<>IMAGE_DOS_SIGNATURE then
    result:=0;

  if ImageNTHeader.OptionalHeader.Magic<>IMAGE_NT_SIGNATURE then
    result:=0;

  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.SizeOfHeaders;
end;

function peinfo_getimagesize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  ImageNTHeader:=PImageNtHeaders(dword(header)+PImageDosHeader(header)^._lfanew);
  result:=ImageNTHeader.OptionalHeader.SizeOfImage;
end;

procedure TfrmPEInfo.ParseFile(loaded: boolean);
{
This will parse the memorycopy and fill in the all data
params:
  Loaded: Determines if the memory copy is from when it has been loaded or on file (IAT filled in, relocations done, etc...)
}
var MZheader: ttreenode;
    PEheader: ttreenode;
    datadir: ttreenode;
    section: Ttreenode;
    va: ttreenode;

    wa: Pwordarray;
    ba: PByteArray;

    tempnode,tempnode2,tempnode3: ttreenode;


    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;

    sFileType,sCharacteristics, sType: string;
    i, j, k: integer;
    maxaddress: dword;

    importaddress: dword;
    importfunctionname: string;
    importmodulename: string;
    //ignore: dword;
    //correctprotection: dword;

    basedifference: dword;
    basedifference64: INT64;

    modhandle: thandle;
    funcaddress: dword;

    numberofrva: integer;
    is64bit: boolean;

    tempaddress,tempaddress2: dword;
    temps: string;


begin
  PEItv.Items.BeginUpdate;
  lbImports.Items.BeginUpdate;
  lbExports.Items.BeginUpdate;
  lbBaseReloc.Items.beginUpdate;
  try
    is64bit:=false;
    PEItv.Items.Clear;
    lbImports.Clear;
    lbExports.clear;
    lbBaseReloc.clear;

    if PImageDosHeader(memorycopy)^.e_magic<>IMAGE_DOS_SIGNATURE then
      raise exception.Create('This is not a valid image');



    MZheader:=PEItv.Items.Add(nil,'MZ header');
    PEItv.Items.AddChild(MZHeader, 'lfanew='+inttohex(PImageDosHeader(memorycopy)^._lfanew,2));
    PEItv.Items.AddChild(MZHeader, format('dos entrypoint = %.4x:%.4x',[PImageDosHeader(memorycopy)^.e_cs, PImageDosHeader(memorycopy)^.e_ip]));
    PEItv.Items.AddChild(MZHeader, format('dos stack = %.4x:%.4x',[PImageDosHeader(memorycopy)^.e_ss, PImageDosHeader(memorycopy)^.e_sp]));

    ImageNtHeader:=peinfo_getImageNtHeaders(memorycopy, memorycopysize);
    if ImageNtHeader=nil then exit;

    PEheader:=PEItv.Items.Add(nil,'PE header');

    if ImageNTHeader^.FileHeader.Machine=$8664 then
    begin
      PEItv.Items.addchild(PEHeader,format('Machine=%.2x (64 bit)' ,[ImageNTHeader^.FileHeader.Machine]));
      is64bit:=true;
    end
    else
      PEItv.Items.addchild(PEHeader,format('Machine=%.2x' ,[ImageNTHeader^.FileHeader.Machine]));
    PEItv.Items.addchild(PEHeader,format('Number of sections=%d' ,[ImageNTHeader^.FileHeader.NumberOfSections]));
    PEItv.Items.addchild(PEHeader,format('Time/Date =%d' ,[ImageNTHeader^.FileHeader.TimeDateStamp]));
    PEItv.Items.addchild(PEHeader,format('SymbolTable at %x' ,[ImageNTHeader^.FileHeader.PointerToSymbolTable]));
    PEItv.Items.addchild(PEHeader,format('Symbolcount = %x' ,[ImageNTHeader^.FileHeader.NumberOfSymbols]));
    PEItv.Items.addchild(PEHeader,format('OptionalHeader size = %x' ,[ImageNTHeader^.FileHeader.SizeOfOptionalHeader]));


    sFileType:='';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE = IMAGE_FILE_EXECUTABLE_IMAGE then sFileType:=sFiletype+'Executable, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_RELOCS_STRIPPED = IMAGE_FILE_RELOCS_STRIPPED then sFileType:=sFiletype+'No relocations, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_LINE_NUMS_STRIPPED = IMAGE_FILE_LINE_NUMS_STRIPPED then sFileType:=sFiletype+'No line numbers, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_LOCAL_SYMS_STRIPPED = IMAGE_FILE_LOCAL_SYMS_STRIPPED then sFileType:=sFiletype+'No local symbols, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_AGGRESIVE_WS_TRIM = IMAGE_FILE_AGGRESIVE_WS_TRIM then sFileType:=sFiletype+'Agressive trim, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_BYTES_REVERSED_LO = IMAGE_FILE_BYTES_REVERSED_LO then sFileType:=sFiletype+'Reversed bytes LO, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_32BIT_MACHINE = IMAGE_FILE_32BIT_MACHINE then sFileType:=sFiletype+'32-bit, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_DEBUG_STRIPPED = IMAGE_FILE_DEBUG_STRIPPED then sFileType:=sFiletype+'No DBG info, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP then sFileType:=sFiletype+'Removable: Run from swap, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_NET_RUN_FROM_SWAP = IMAGE_FILE_NET_RUN_FROM_SWAP then sFileType:=sFiletype+'Net: Run from swap, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_SYSTEM = IMAGE_FILE_SYSTEM then sFileType:=sFiletype+'System file, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_DLL = IMAGE_FILE_DLL then sFileType:=sFiletype+'DLL, ';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_UP_SYSTEM_ONLY = IMAGE_FILE_UP_SYSTEM_ONLY then sFileType:=sFiletype+'UP system only, ';

    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_BYTES_REVERSED_HI = IMAGE_FILE_BYTES_REVERSED_HI then sFileType:=sFiletype+'Reversed bytes HI, ';


    sFileType:=copy(sfiletype,1,length(sfiletype)-2);
    if sFileType='' then
      sFileType:='Unknown';


    PEItv.Items.addchild(PEHeader,format('Characteristics = %x (%s)' ,[ImageNTHeader^.FileHeader.Characteristics, sFileType]));
    PEItv.Items.addchild(PEHeader,'-----optional-----');
    PEItv.Items.addchild(PEHeader,format('Optional magic number = %x ' ,[ImageNTHeader^.OptionalHeader.Magic]));
    PEItv.Items.addchild(PEHeader,format('Major linker version = %d ' ,[ImageNTHeader^.OptionalHeader.MajorLinkerVersion]));
    PEItv.Items.addchild(PEHeader,format('Minor linker version = %d ' ,[ImageNTHeader^.OptionalHeader.MinorLinkerVersion]));
    PEItv.Items.addchild(PEHeader,format('Size of code = %x (%d) ' ,[ImageNTHeader^.OptionalHeader.SizeOfCode, ImageNTHeader^.OptionalHeader.SizeOfCode]));

    PEItv.Items.addchild(PEHeader,format('Size of initialized data = %x (%d)' ,[ImageNTHeader^.OptionalHeader.SizeOfInitializedData, ImageNTHeader^.OptionalHeader.SizeOfInitializedData]));
    PEItv.Items.addchild(PEHeader,format('Size of uninitialized data = %x (%d) ' ,[ImageNTHeader^.OptionalHeader.SizeOfUninitializedData, ImageNTHeader^.OptionalHeader.SizeOfUninitializedData]));

    PEItv.Items.addchild(PEHeader,format('Entry point = %.8x ' ,[ImageNTHeader^.OptionalHeader.AddressOfEntryPoint]));
    PEItv.Items.addchild(PEHeader,format('Base of code = %.8x ' ,[ImageNTHeader^.OptionalHeader.BaseOfCode]));
    if (not is64bit) then
    begin
      PEItv.Items.addchild(PEHeader,format('Base of data = %.8x ' ,[ImageNTHeader^.OptionalHeader.BaseOfData]));
      PEItv.Items.addchild(PEHeader,format('Prefered imagebase = %.8x ' ,[ImageNTHeader^.OptionalHeader.ImageBase]));
    end
    else
      PEItv.Items.addchild(PEHeader,format('Prefered imagebase = %.16x ' ,[PUINT64(@ImageNTHeader^.OptionalHeader.BaseOfData)^]));


    PEItv.Items.addchild(PEHeader,format('Section allignment = %x ' ,[ImageNTHeader^.OptionalHeader.SectionAlignment]));
    PEItv.Items.addchild(PEHeader,format('File Alignment = %x ' ,[ImageNTHeader^.OptionalHeader.FileAlignment]));
    PEItv.Items.addchild(PEHeader,format('Major Operating System Version = %d ' ,[ImageNTHeader^.OptionalHeader.MajorOperatingSystemVersion]));
    PEItv.Items.addchild(PEHeader,format('Major Image Version = %d ' ,[ImageNTHeader^.OptionalHeader.MajorImageVersion]));
    PEItv.Items.addchild(PEHeader,format('Minor Image Version = %d ' ,[ImageNTHeader^.OptionalHeader.MinorImageVersion]));
    PEItv.Items.addchild(PEHeader,format('Major Subsystem Version = %d ' ,[ImageNTHeader^.OptionalHeader.MajorSubsystemVersion]));
    PEItv.Items.addchild(PEHeader,format('Minor Subsystem Version = %d ' ,[ImageNTHeader^.OptionalHeader.MinorSubsystemVersion]));
    PEItv.Items.addchild(PEHeader,format('Win32 Version Value = %x ' ,[ImageNTHeader^.OptionalHeader.Win32VersionValue]));
    PEItv.Items.addchild(PEHeader,format('Size Of Image = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfImage]));
    PEItv.Items.addchild(PEHeader,format('Size Of Headers = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfHeaders]));
    PEItv.Items.addchild(PEHeader,format('CheckSum = %x ' ,[ImageNTHeader^.OptionalHeader.CheckSum]));
    PEItv.Items.addchild(PEHeader,format('Subsystem = %x ' ,[ImageNTHeader^.OptionalHeader.Subsystem]));
    PEItv.Items.addchild(PEHeader,format('Dll Characteristics = %x ' ,[ImageNTHeader^.OptionalHeader.DllCharacteristics]));
    if is64bit then
    begin
      PEItv.Items.addchild(PEHeader,format('Size Of Stack Reserve = %x ' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfStackReserve]));
      PEItv.Items.addchild(PEHeader,format('Size Of Stack Commit = %x ' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfStackCommit]));
      PEItv.Items.addchild(PEHeader,format('Size Of Heap Reserve = %x ' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeapReserve]));
      PEItv.Items.addchild(PEHeader,format('Size Of Heap Commit = %x ' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeapCommit]));
      PEItv.Items.addchild(PEHeader,format('Loader Flags = %x ' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.LoaderFlags]));
      numberofrva:=PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.NumberOfRvaAndSizes;
    end
    else
    begin
      PEItv.Items.addchild(PEHeader,format('Size Of Stack Reserve = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfStackReserve]));
      PEItv.Items.addchild(PEHeader,format('Size Of Stack Commit = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfStackCommit]));
      PEItv.Items.addchild(PEHeader,format('Size Of Heap Reserve = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfHeapReserve]));
      PEItv.Items.addchild(PEHeader,format('Size Of Heap Commit = %x ' ,[ImageNTHeader^.OptionalHeader.SizeOfHeapCommit]));
      PEItv.Items.addchild(PEHeader,format('Loader Flags = %x ' ,[ImageNTHeader^.OptionalHeader.LoaderFlags]));
      numberofrva:=ImageNTHeader^.OptionalHeader.NumberOfRvaAndSizes;
    end;

    datadir:=PEItv.Items.addchild(PEHeader,format('Number Of Rva And Sizes = %d ' ,[numberofrva]));

    ImageSectionHeader:=PImageSectionHeader(dword(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);
    PEItv.Items.addchild(PEHeader,'-----sections-----');

    maxaddress:=0;


    for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
    begin
      section:=PEItv.Items.addchild(PEHeader, copy(pchar(@ImageSectionHeader.Name[0]), 1, 8));

      PEItv.Items.addchild(section,format('Virtual Size=%x',[ImageSectionHeader.Misc.VirtualSize]));
      PEItv.Items.addchild(section,format('Virtual Address=%x',[ImageSectionHeader.VirtualAddress]));
      PEItv.Items.addchild(section,format('size of raw data=%x',[ImageSectionHeader.SizeOfRawData]));
      PEItv.Items.addchild(section,format('Pointer to raw data=%x',[ImageSectionHeader.PointerToRawData]));
      PEItv.Items.addchild(section,format('Pointer to relocations=%x',[ImageSectionHeader.PointerToRelocations]));
      PEItv.Items.addchild(section,format('Pointer to line numbers=%x',[ImageSectionHeader.PointerToLinenumbers]));
      PEItv.Items.addchild(section,format('number of relocations=%x',[ImageSectionHeader.NumberOfRelocations]));
      PEItv.Items.addchild(section,format('number of line numbers=%x',[ImageSectionHeader.NumberOfLinenumbers]));

      sCharacteristics:='';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_CODE = IMAGE_SCN_CNT_CODE then sCharacteristics:='Executable code, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA = IMAGE_SCN_CNT_INITIALIZED_DATA then sCharacteristics:=sCharacteristics+'Initialized data, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA = IMAGE_SCN_CNT_UNINITIALIZED_DATA then sCharacteristics:=sCharacteristics+'Uninitialized data, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_LNK_REMOVE = IMAGE_SCN_LNK_REMOVE then sCharacteristics:=sCharacteristics+'removed, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = IMAGE_SCN_MEM_DISCARDABLE then sCharacteristics:=sCharacteristics+'discardable, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_NOT_CACHED = IMAGE_SCN_MEM_NOT_CACHED then sCharacteristics:=sCharacteristics+'not cached, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_NOT_PAGED = IMAGE_SCN_MEM_NOT_PAGED then sCharacteristics:=sCharacteristics+'not paged, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_SHARED = IMAGE_SCN_MEM_SHARED then sCharacteristics:=sCharacteristics+'shared memory, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE then sCharacteristics:=sCharacteristics+'executable memory, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_READ = IMAGE_SCN_MEM_READ then sCharacteristics:=sCharacteristics+'readable memory, ';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_WRITE = IMAGE_SCN_MEM_WRITE then sCharacteristics:=sCharacteristics+'writable memory, ';

      sCharacteristics:=copy(sCharacteristics,1,length(sCharacteristics)-2);

      if sCharacteristics='' then sCharacteristics:='Unknown';
      PEItv.Items.addchild(section,format('characterisitics=%x (%s)',[ImageSectionHeader.Characteristics, sCharacteristics]));

      if maxaddress<(ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData) then
        maxaddress:=ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData;

      inc(ImageSectionHeader);
    end;

    ImageSectionHeader:=PImageSectionHeader(dword(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);

    if loaded then
    begin
      //in memory
      loadedmodule:=memorycopy;

      if is64bit then
      begin
        //no use in 5.4- , but let's do it anyhow
        basedifference:=dword(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
        basedifference64:=UINT64(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
      end
      else
      begin
        basedifference:=dword(loadedmodule)-ImageNTHeader^.OptionalHeader.ImageBase;
      end;

    end
    else
    begin
      //from a file
      loadedmodule:=virtualalloc(nil,maxaddress, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
      if loadedmodule=nil then raise exception.create('Failure at allocating memory');
      ZeroMemory(loadedmodule,maxaddress);

      label2.caption:=inttohex(dword(loadedmodule),8);
      for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
      begin
        CopyMemory(@loadedmodule[ImageSectionHeader.VirtualAddress], @memorycopy[ImageSectionHeader.PointerToRawData], ImageSectionHeader.SizeOfRawData);

     //determine the correct protection
     //   correctprotection:=0;

     //   VirtualProtect(@ImageSectionHeader[ImageSectionHeader.VirtualAddress], ImageSectionHeader.SizeOfRawData, correctprotection, ignore);


        //adjust rva's

        inc(ImageSectionHeader);
      end;

      if is64bit then
      begin
        CopyMemory(@loadedmodule[0], @memorycopy[0], PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeaders);

        basedifference:=dword(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
        basedifference64:=UINT64(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
      end
      else
      begin
        CopyMemory(@loadedmodule[0], @memorycopy[0], ImageNTHeader^.OptionalHeader.SizeOfHeaders);
        basedifference:=dword(loadedmodule)-ImageNTHeader^.OptionalHeader.ImageBase;

      end;

    end;



    //now it has been mapped the vla and other stuff can be handled
    for i:=0 to numberofrva-1 do
    begin
      case i of
        0: sType:='Export table';
        1: sType:='Import table';
        2: sType:='Resource table';
        3: sType:='Exception table';
        4: sType:='Certificate table';
        5: sType:='Base-Relocation table';
        6: sType:='Debugging info table';
        7: sType:='Architecture-Specific table';
        8: sType:='Global pointer table';
        9: sType:='TLS table';
       10: sType:='Load config table';
       11: sType:='Bound import table';
       12: sType:='import address table';
       13: sType:='Delay import descriptor table';
       else sType:='reserved';
      end;

      if is64bit then
        tempnode:=PEItv.Items.addchild(datadir,format('%.8x - %x (%s)' ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress, PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].Size, sType]))
      else
        tempnode:=PEItv.Items.addchild(datadir,format('%.8x - %x (%s)' ,[ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress, ImageNTHeader^.OptionalHeader.DataDirectory[i].Size, sType]));


      if (is64bit and (PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress=0)) or
         ((not is64bit) and (ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress=0)) then
        continue; //don't look into it


      if i=0 then
      begin
        //exports
        if is64bit then
          ImageExportDirectory:=PImageExportDirectory(dword(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
        else
          ImageExportDirectory:=PImageExportDirectory(dword(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);

        lbExports.Items.Add(pchar(dword(loadedmodule)+ImageExportDirectory.Name)+':');


        PEItv.Items.addchild(tempnode, format('Characteristics=%x (should be 0)', [ImageExportDirectory.Characteristics]));
        PEItv.Items.addchild(tempnode, format('Time datastamp=%x', [ImageExportDirectory.TimeDateStamp]));
        PEItv.Items.addchild(tempnode, format('Major version=%d', [ImageExportDirectory.MajorVersion]));
        PEItv.Items.addchild(tempnode, format('Minor version=%d', [ImageExportDirectory.MinorVersion]));
        PEItv.Items.addchild(tempnode, format('Name = %x (%s)', [ImageExportDirectory.Name, pchar(dword(loadedmodule)+ImageExportDirectory.Name)]));
        PEItv.Items.addchild(tempnode, format('Base = %x', [ImageExportDirectory.Base]));
        PEItv.Items.addchild(tempnode, format('NumberOfFunctions = %d', [ImageExportDirectory.NumberOfFunctions]));
        PEItv.Items.addchild(tempnode, format('NumberOfNames = %d', [ImageExportDirectory.NumberOfNames]));
        tempnode2:=PEItv.Items.addchild(tempnode, format('AddressOfFunctions = %x', [dword(ImageExportDirectory.AddressOfFunctions)]));

        if ImageExportDirectory.NumberOfFunctions<>ImageExportDirectory.NumberOfNames then
          tempnode2.Text:=tempnode2.Text+' (inconsistent)'
        else
        begin
          for j:=0 to ImageExportDirectory.NumberOfFunctions-1 do
            lbExports.Items.Add(format('%x - %s',[pdwordarray(dword(loadedmodule)+dword(ImageExportDirectory.AddressOfFunctions))[j], pchar(dword(loadedmodule)+pdwordarray(dword(loadedmodule)+dword(ImageExportDirectory.AddressOfNames))[j])]));


        end;



        for j:=0 to ImageExportDirectory.NumberOfFunctions-1 do
          PEItv.Items.addchild(tempnode2, format('%x',[pdwordarray(dword(loadedmodule)+dword(ImageExportDirectory.AddressOfFunctions))[j]]));

        tempnode2:=PEItv.Items.addchild(tempnode, format('AddressOfNames = %x', [dword(ImageExportDirectory.AddressOfNames)]));
        for j:=0 to ImageExportDirectory.NumberOfNames-1 do
          PEItv.Items.addchild(tempnode2, format('%s',[pchar(dword(loadedmodule)+pdwordarray(dword(loadedmodule)+dword(ImageExportDirectory.AddressOfNames))[j])]));


        PEItv.Items.addchild(tempnode, format('AddressOfNameOrdinals = %x', [dword(ImageExportDirectory.AddressOfNameOrdinals)]));
      end
      else
      if i=1 then
      begin  //import

        j:=0;
        if is64bit then
          ImageImportDirectory:=PImageImportDirectory(dword(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
        else
          ImageImportDirectory:=PImageImportDirectory(dword(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);



        while (j<45) do
        begin
          if ImageImportDirectory.name=0 then break;

          if j>0 then
            lbImports.Items.Add('');
            
          lbImports.Items.Add(format('%s', [pchar(dword(loadedmodule)+ImageImportDirectory.name)]));
          

          tempnode2:=PEItv.Items.addchild(tempnode,format('Import %d : %s',[j, pchar(dword(loadedmodule)+ImageImportDirectory.name)]));
          PEItv.Items.addchild(tempnode2, format('Characteristics/OriginalFirstThunk=%x', [ImageImportDirectory.characteristicsOrFirstThunk]));
          PEItv.Items.addchild(tempnode2, format('TimeDateStamp=%x (0=not bound -1=bound, and timestamp)', [ImageImportDirectory.TimeDateStamp]));
          PEItv.Items.addchild(tempnode2, format('Forwarder Chain=%x (-1 if no forwarders)', [ImageImportDirectory.ForwarderChain]));
          PEItv.Items.addchild(tempnode2, format('Name=%x : %s', [ImageImportDirectory.name, pchar(dword(loadedmodule)+ImageImportDirectory.name)]));
          PEItv.Items.addchild(tempnode2, format('FirstThunk=%x', [ImageImportDirectory.FirstThunk]));

          tempnode3:=PEItv.Items.addchild(tempnode2, format('imports:',[]));

        
          if ImageImportDirectory.ForwarderChain<>$ffffffff then
          begin
            importmodulename:=pchar(dword(loadedmodule)+ImageImportDirectory.name);

            if not loaded then
              modhandle:=loadlibrary(pchar(importmodulename));

            k:=0;
            if is64bit then
              while PUINT64(dword(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^<>0 do
              begin
                importaddress:=dword(loadedmodule)+ImageImportDirectory.FirstThunk+8*k;
                importfunctionname:=pchar(dword(loadedmodule)+pdword(importaddress)^+2);

                PEItv.Items.addchild(tempnode3, format('%x (%x) - %s',[PUINT64(dword(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^, importaddress, importfunctionname]));
                lbImports.Items.Add( format('%x (%x) - %s',[PUINT64(dword(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^, importaddress, importfunctionname]));

                if not loaded then
                begin
                  funcaddress:=dword(getprocaddress(modhandle, pchar(importfunctionname)));
                  pdword(importaddress)^:=funcaddress;
                end;

                inc(k);
              end
            else
              while PDWORD(dword(loadedmodule)+ImageImportDirectory.FirstThunk+4*k)^<>0 do
              begin
                importaddress:=dword(@pdwordarray(dword(loadedmodule)+ImageImportDirectory.FirstThunk)[k]);

                tempaddress:=dword(loadedmodule)+pdwordarray(dword(loadedmodule)+ImageImportDirectory.FirstThunk)[k]+2;
                if loaded then
                begin
                  tempaddress2:=pdwordarray(dword(loadedmodule)+ImageImportDirectory.FirstThunk)[k];
                  importfunctionname:=symhandler.getNameFromAddress(tempaddress2);

                  if uppercase(inttohex(tempaddress2,8))=uppercase(importfunctionname) then
                  begin
                    //failure to convert the address to an import
                    inc(k);
                    continue;
                  end;
                end
                else importfunctionname:=pchar(tempaddress);

                PEItv.Items.addchild(tempnode3, format('%x (%x) - %s',[pdwordarray(dword(loadedmodule)+ImageImportDirectory.FirstThunk)[k], importaddress, importfunctionname]));
                lbImports.Items.Add( format('%x (%x) - %s',[pdwordarray(dword(loadedmodule)+ImageImportDirectory.FirstThunk)[k], importaddress-dword(loadedmodule), importfunctionname]));

                if not loaded then
                begin
                  funcaddress:=dword(getprocaddress(modhandle, pchar(importfunctionname)));
                  pdword(importaddress)^:=funcaddress;
                end;

                inc(k);
              end;



          end
          else
          begin
            PEItv.Items.addchild(tempnode3,'Static addresses');
          end;

          inc(j);
          ImageImportDirectory:=PImageImportDirectory(dword(ImageImportDirectory)+sizeof(TImageImportDirectory));
        end;

      end
      else
      if i=5 then
      begin
        // IMAGE_BASE_RELOCATION stuff
          if is64bit then
          begin
            ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(dword(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress);
            maxaddress:=dword(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].Size;
          end
          else
          begin
            ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(dword(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);
            maxaddress:=dword(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress+ImageNTHeader^.OptionalHeader.DataDirectory[i].Size;
          end;

          while dword(ImageBaseRelocation)<maxaddress do
          begin
            if ImageBaseRelocation.SizeOfBlock=0 then break;

            VA:=PEItv.Items.addchild(tempnode,format('Virtual address base: %.8x (size=%x (%d))', [ImageBaseRelocation.virtualaddress, ImageBaseRelocation.SizeOfBlock, ImageBaseRelocation.SizeOfBlock]));

            for j:=0 to ((ImageBaseRelocation.SizeOfBlock-8) div 2)-1 do
            begin
              PEItv.Items.addchild(va, format('%.3x : %d',[(ImageBaseRelocation.rel[j] and $fff), (ImageBaseRelocation.rel[j] shr 12) ]));
              lbBaseReloc.Items.Add(format('%.3x : %d',[ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff), (ImageBaseRelocation.rel[j] shr 12) ]));

              if not loaded then
              begin
                if (ImageBaseRelocation.rel[j] shr 12)=3 then            //replace the address at this address with a relocated one (dword)
                  pdword(dword(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=pdword(dword(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference;

                if (ImageBaseRelocation.rel[j] shr 12)=10 then            //replace the address at this address with a relocated one (dword)
                  PUINT64(dword(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=PUINT64(dword(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference64;
              end;
            end;


            ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(dword(ImageBaseRelocation)+ImageBaseRelocation.SizeOfBlock);
          end;
      end;

    end;

  finally
    PEItv.Items.EndUpdate;
    lbImports.Items.EndUpdate;
    lbExports.Items.EndUpdate;
    lbBaseReloc.Items.EndUpdate;
  end;


  if loaded then
  begin
    loadedmodule:=nil;
  end
  else
  begin
    if (loadedmodule<>nil) then
    begin
      virtualfree(loadedmodule,0,MEM_RELEASE);
      loadedmodule:=nil;
    end;
  end;
end;

procedure TfrmPEInfo.LoadButtonClick(Sender: TObject);
var f: tfilestream;
begin
  radiobutton1.Checked:=true;
  if opendialog1.Execute then
  begin
    label1.Caption:=extractfilename(opendialog1.filename);

    //open the file and read mem
    f:=tfilestream.Create(opendialog1.filename, fmOpenRead or fmShareDenyNone	);
    try
      if memorycopy<>nil then
        freemem(memorycopy);

      if loadedmodule<>nil then
      begin
        virtualfree(loadedmodule,0,MEM_RELEASE	);
        loadedmodule:=nil;
      end;

      getmem(memorycopy, f.Size);
      f.ReadBuffer(memorycopy^,f.Size);
      memorycopysize:=f.size;

    finally
      f.free;
    end;

    //and parse it
    ParseFile(false);
  end;
end;

procedure TfrmPEInfo.FormDestroy(Sender: TObject);
begin
  if memorycopy<>nil then
    freemem(memorycopy);

  if loadedmodule<>nil then
    VirtualFree(loadedmodule,0,MEM_RELEASE);
end;

procedure TfrmPEInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmPEInfo.modulelistClick(Sender: TObject);
begin
  if modulelist.ItemIndex<>-1 then
    edtAddress.Text:=inttohex(dword(modulelist.Items.Objects[modulelist.itemindex]),8);
end;

procedure TfrmPEInfo.FormShow(Sender: TObject);
begin

  modulelist.Clear;
  symhandler.getModuleList(modulelist.Items);
  if modulelist.Count>0 then
  begin
    modulelist.ItemIndex:=0;
    modulelist.OnClick(modulelist);
  end;
end;

procedure TfrmPEInfo.Button1Click(Sender: TObject);
var address: dword;
    actualread: dword;
    headersize: dword;
    imagesize: dword;
    imagesizes: string;
    check: boolean;
begin
  try
    address:=strtoint('$'+edtAddress.text);
  except
    beep; //beeeeeeeeeeeeping idiot
    exit;
  end;

  if loadedmodule<>nil then
  begin
    virtualfree(loadedmodule,0,MEM_RELEASE	);
    loadedmodule:=nil;
  end;

  if memorycopy<>nil then
    freemem(memorycopy);

  getmem(memorycopy,4096);
  try
    if (not readprocessmemory(processhandle,pointer(address),memorycopy,4096,actualread)) or (actualread<>4096) then
      raise exception.Create('The header of the module could not be read');

    headersize:=peinfo_getheadersize(memorycopy);
    if headersize=0 then
      raise exception.Create('This is not a valid PE file');


    imagesize:=peinfo_getimagesize(memorycopy);

  finally
    freemem(memorycopy);

  end;


  if imagesize>256*1024*1024 then
  begin
    imagesizes:=inttostr(imagesize);
    if inputquery('PEInfo: Image size','The imagesize is more than 256 MB, is this the correct ammount? If not, edit here', imagesizes) then
    begin
      try
        imagesize:=strtoint(imagesizes);
      except
        exit;
      end;
    end
    else exit;
  end;
  getmem(memorycopy,imagesize);

  actualread:=0;
  check:=readprocessmemory(processhandle,pointer(address),memorycopy,imagesize,actualread);
  if actualread>0 then //work with this
  begin
    if not check then
      messagedlg('Not all memory could be read, working with a partial copy here',mtwarning,[mbok],0);

    memorycopysize:=actualread;
  end else raise exception.Create('Failure reading memory');


  modulebase:=address;
  parsefile(true);
end;

procedure TfrmPEInfo.FormCreate(Sender: TObject);
begin
  pagecontrol1.TabIndex:=0;
end;

end.






