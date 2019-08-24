unit PEInfounit;

{$MODE Delphi}

{
Changed title from PE info to Portable Executable (PE) info. I have this feeling
that 'some people' (idiots) would not understand that it isn't a packet editor
}

interface

uses
  windows, LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, CEFuncProc, NewKernelHandler, Buttons, StdCtrls, ExtCtrls,
  ComCtrls, LResources, Menus, symbolhandler, PEInfoFunctions, commonTypeDefs,
  Clipbrd;

type

  { TfrmPEInfo }

  TfrmPEInfo = class(TForm)
    GroupBox2: TGroupBox;
    peiImageList: TImageList;
    miCopyTab: TMenuItem;
    miCopyEverything: TMenuItem;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    edtAddress: TEdit;
    modulelist: TListBox;
    LoadButton: TSpeedButton;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    Label2: TLabel;
    PageControl1: TPageControl;
    pmInfo: TPopupMenu;
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
    procedure miCopyEverythingClick(Sender: TObject);
    procedure miCopyTabClick(Sender: TObject);
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

function peinfo_getcodesize(header: pointer; headersize: integer=0): dword;
function peinfo_getentryPoint(header: pointer; headersize: integer=0): ptrUint;
function peinfo_getcodebase(header: pointer; headersize: integer=0): ptrUint;
function peinfo_getdatabase(header: pointer; headersize: integer=0): ptrUint;
function peinfo_getheadersize(header: pointer): dword;


implementation

uses processhandlerunit, parsers, DPIHelper;

resourcestring
  rsThisIsNotAValidImage = 'This is not a valid image';
  rsPENotAllMemoryCouldBeReadWorkingWithaPartialCopyHere = 'Not all memory could be read, working with a partial copy here';
  rsPEPeHeader = 'PE header';
  rsPEMzHeader = 'MZ header';
  rsPEThisIsNotaVakidPeFile = 'This is not a valid PE file';
  rsPETheHeaderOfTheModeleCouldNobBeRead = 'The header of the module could not be read';
  rsPEFailureReadingMemory = 'Failure reading memory';
  rsPEPeinfoImageSize = 'PEInfo: Image size';
  rsPETheImagesizeIsMoreThan256 = 'The imagesize is more than 256 MB, is this the correct ammount? If not, edit here';
  rsPELfanew = 'lfanew=';
  rsPEDosEntrypoint = 'dos entrypoint = %.4x:%.4x';
  rsPEDosStack = 'dos stack = %.4x:%.4x';
  rsPEMachine64 = 'Machine=%.2x (64 bit)';
  rsPEMachine = 'Machine=%.2x';
  rsPENumberofSections = 'Number of sections=%d';
  rsPETimeDate = 'Time/Date =%d';
  rsPESymbolTableAt = 'SymbolTable at %x';
  rsPESymbolCount = 'Symbolcount = %x';
  rsPEOptionalHeader = 'OptionalHeader size = %x';
  rsPEExecutable = 'Executable, ';
  rsPENoRelocations = 'No relocations, ';
  rsPENoLineNumbers = 'No line numbers, ';
  rsPENoLocalSymbols = 'No local symbols, ';
  rsPEAgressiveTrim = 'Agressive trim, ';
  rsPEReversedBytesLO = 'Reversed bytes LO, ';
  rsPE32Bit = '32-bit, ';
  rsPENoDbgInfo = 'No DBG info, ';
  rsPERemovableRunFromSwap = 'Removable: Run from swap, ';
  rsPENetRunFromSwap = 'Net: Run from swap, ';
  rsPESystemFile = 'System file, ';
  rsPEDll = 'DLL, ';
  rsPEUpSystemOnly = 'UP system only, ';
  rsPEReversedBytesHI = 'Reversed bytes HI, ';
  rsPEUnknown = 'Unknown';
  rsPECharacteristics = 'Characteristics = %x (%s)';
  rsPEOptional = '-----optional-----';
  rsPEOptionalMagicNumber = 'Optional magic number = %x ';
  rsPEMajorLinkerVersion = 'Major linker version = %d ';
  rsPEMinorLinlerVersion = 'Minor linker version = %d ';
  rsPESizeOfCode = 'Size of code = %x (%d) ';
  rsPESizeOfInitializedData = 'Size of initialized data = %x (%d)';
  rsPESizeOfUninitializedData = 'Size of uninitialized data = %x (%d) ';
  rsPEEntryPoint = 'Entry point = %.8x ';
  rsPEBaseOfCode = 'Base of code = %.8x ';
  rsPEBaseOfData = 'Base of data = %.8x ';
  rsPEPreferedImagebase = 'Prefered imagebase = %.8x ';
  rsPEPreferedImagebase2 = 'Prefered imagebase = %.16x ';
  rsPESectionAllignment = 'Section allignment = %x ';
  rsPEFileAlignment = 'File Alignment = %x ';
  rsPEMajorOperatingSystemVersion = 'Major Operating System Version = %d ';
  rsPEMajorImageVersion = 'Major Image Version = %d ';
  rsPEMinorImageVersion = 'Minor Image Version = %d ';
  rsPEMajorSubsystemVersion = 'Major Subsystem Version = %d ';
  rsPEMinorSubsystemVersion = 'Minor Subsystem Version = %d ';
  rsPEWin32VersionValue = 'Win32 Version Value = %x ';
  rsPESizeOfImage = 'Size Of Image = %x ';
  rsPESizeOfHeader = 'Size Of Headers = %x ';
  rsPECheckSum = 'CheckSum = %x ';
  rsPESubsystem = 'Subsystem = %x ';
  rsPEDllCharacteristics = 'Dll Characteristics = %x ';
  rsPESizeOfStackReserve = 'Size Of Stack Reserve = %x ';
  rsPESizeOfStackCommit = 'Size Of Stack Commit = %x ';
  rsPESizeOfHeapReserve = 'Size Of Heap Reserve = %x ';
  rsPESizeOfHeapCommit = 'Size Of Heap Commit = %x ';
  rsPELoaderFlags = 'Loader Flags = %x ';
  rsPENumberOfRvaAndSize = 'Number Of Rva And Sizes = %d ';
  rsPESections = '-----sections-----';
  rsPEVirtualSize = 'Virtual Size=%x';
  rsPEVirtualAddress = 'Virtual Address=%x';
  rsPESizeOfRawData = 'size of raw data=%x';
  rsPEPointerToRawData = 'Pointer to raw data=%x';
  rsPEPointerToRelocations = 'Pointer to relocations=%x';
  rsPEPointerToLineNumbers = 'Pointer to line numbers=%x';
  rsPENumberOfRelocations = 'number of relocations=%x';
  rsPENumberOfLineNumbers = 'number of line numbers=%x';
  rsPEExecutableCode = 'Executable code, ';
  rsPEInitializedData = 'Initialized data, ';
  rsPEUninitializedData = 'Uninitialized data, ';
  rsPERemoved = 'removed, ';
  rsPEDiscardable = 'discardable, ';
  rsPENotCached = 'not cached, ';
  rsPENotPaged = 'not paged, ';
  rsPESharedMemory = 'shared memory, ';
  rsPEExecutableMemory = 'executable memory, ';
  rsPEReadableMemory = 'readable memory, ';
  rsPEWritableMemory = 'writable memory, ';
  rsPECharacterisitics = 'characterisitics=%x (%s)';
  rsPEFailureAtAllocationMemory = 'Failure at allocating memory';
  rsPEExportTable = 'Export table';
  rsPEImportTable = 'Import table';
  rsPEResourceTable = 'Resource table';
  rsPEExceptionTable = 'Exception table';
  rsPECertificateTabel = 'Certificate table';
  rsPEBaseRelocationTable = 'Base-Relocation table';
  rsPEDebuggingInfoTable = 'Debugging info table';
  rsPEArchitectureSpecificTable = 'Architecture-Specific table';
  rsPEGlobalPointerTable = 'Global pointer table';
  rsPETLSTable = 'TLS table';
  rsPELoadConfigTable = 'Load config table';
  rsPEBoundImportTable = 'Bound import table';
  rsPEImportAddressTable = 'import address table';
  rsPEDelayImportDescriptionTable = 'Delay import descriptor table';
  rsPEReserved = 'reserved';
  rsPECharacteristics2 = 'Characteristics=%x (should be 0)';
  rsPETimeDatastamp = 'Time datastamp=%x';
  rsPEMajorVersion = 'Major version=%d';
  rsPEMinorVersion = 'Minor version=%d';
  rsPEName = 'Name = %x (%s)';
  rsPEBase = 'Base = %x';
  rsPENumberOfFunctions = 'NumberOfFunctions = %d';
  rsPENumberOfNames = 'NumberOfNames = %d';
  rsPEAddressOfFunctions = 'AddressOfFunctions = %x';
  rsPEinconsistent = ' (inconsistent)';
  rsPEAddressOfNames = 'AddressOfNames = %x';
  rsPEAddressOfNameOrdinals = 'AddressOfNameOrdinals = %x';
  rsPEImport = 'Import %d : %s';
  rsPECharacteristicsOriginalFirstThunk = 'Characteristics/OriginalFirstThunk=%x';
  rsPETimeDateStamp = 'TimeDateStamp=%x (0=not bound -1=bound, and timestamp)';
  rsPEForwarderChain = 'Forwarder Chain=%x (-1 if no forwarders)';
  rsPEName2 = 'Name=%x : %s';
  rsPEFirstThunk = 'FirstThunk=%x';
  rsPEImports = 'imports:';
  rsPEStaticAddresses = 'Static addresses';
  rsPEVirtualAddressBase = 'Virtual address base: %.8x (size=%x (%d))';
  rsPEType = 'Type = %d';
  rsPEAddressOfRawData = 'Address of raw data = %x';
  rsPECodeview = 'Codeview';
  rsPESignature = 'Signature=%x (%s)';
  rsPEDebugfile = 'Debugfile =%s';

function peinfo_getcodesize(header: pointer; headersize: integer=0): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;

  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader.OptionalHeader.SizeOfCode;
  end;
end;

function peinfo_getdatabase(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader.OptionalHeader.BaseOfData;
  end;
end;

function peinfo_getcodebase(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader.OptionalHeader.BaseOfCode;
  end;
end;

function peinfo_getEntryPoint(header: pointer; headersize: integer=0): ptrUint;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if (headersize=0) or (PImageDosHeader(header)^._lfanew<=headersize-sizeof(TImageNtHeaders)) then
  begin
    ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
    result:=ImageNTHeader.OptionalHeader.AddressOfEntryPoint;
  end;

end;

function peinfo_getheadersize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  result:=0;
  if PImageDosHeader(header)^.e_magic<>IMAGE_DOS_SIGNATURE then exit;

  ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
  if ptrUint(ImageNTHeader)-ptrUint(header)>$1000 then exit;
  if ImageNTHeader.Signature<>IMAGE_NT_SIGNATURE then exit;

  result:=ImageNTHeader.OptionalHeader.SizeOfHeaders;
end;

function peinfo_getimagesize(header: pointer): dword;
var
    ImageNTHeader: PImageNtHeaders;
begin
  ImageNTHeader:=PImageNtHeaders(ptrUint(header)+PImageDosHeader(header)^._lfanew);
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

    tempnode,tempnode2,tempnode3: ttreenode;


    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
    ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
    ImageExportDirectory: PImageExportDirectory;
    ImageImportDirectory: PImageImportDirectory;

    ImageDebugDirectory: PImageDebugDirectory;


    sFileType,sCharacteristics, sType: string;
    i, j, k: integer;
    maxaddress: ptrUint;

    importaddress: PtrUInt;
    importfunctionname: string;
    importmodulename: string;
    //ignore: dword;
    //correctprotection: dword;

    basedifference: ptrUint;
    basedifference64: INT64;

    modhandle: thandle;
    funcaddress: ptrUint;

    numberofrva: integer;
    is64bit: boolean;

    tempaddress,tempaddress2: ptrUint;

    tempstring: pchar;

    s: string;
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
      raise exception.Create(rsThisIsNotAValidImage);



    MZheader:=PEItv.Items.Add(nil,rsPEMzHeader);
    PEItv.Items.AddChild(MZHeader, rsPELfanew+inttohex(PImageDosHeader(memorycopy)^._lfanew,2));
    PEItv.Items.AddChild(MZHeader, format(rsPEDosEntrypoint,[PImageDosHeader(memorycopy)^.e_cs, PImageDosHeader(memorycopy)^.e_ip]));
    PEItv.Items.AddChild(MZHeader, format(rsPEDosStack,[PImageDosHeader(memorycopy)^.e_ss, PImageDosHeader(memorycopy)^.e_sp]));

    ImageNtHeader:=peinfo_getImageNtHeaders(memorycopy, memorycopysize);
    if ImageNtHeader=nil then exit;

    PEheader:=PEItv.Items.Add(nil,rsPEPeHeader);

    if ImageNTHeader^.FileHeader.Machine=$8664 then
    begin
      PEItv.Items.addchild(PEHeader,format(rsPEMachine64 ,[ImageNTHeader^.FileHeader.Machine]));
      is64bit:=true;
    end
    else
      PEItv.Items.addchild(PEHeader,format(rsPEMachine ,[ImageNTHeader^.FileHeader.Machine]));
    PEItv.Items.addchild(PEHeader,format(rsPENumberofSections ,[ImageNTHeader^.FileHeader.NumberOfSections]));
    PEItv.Items.addchild(PEHeader,format(rsPETimeDate ,[ImageNTHeader^.FileHeader.TimeDateStamp]));
    PEItv.Items.addchild(PEHeader,format(rsPESymbolTableAt ,[ImageNTHeader^.FileHeader.PointerToSymbolTable]));
    PEItv.Items.addchild(PEHeader,format(rsPESymbolCount ,[ImageNTHeader^.FileHeader.NumberOfSymbols]));
    PEItv.Items.addchild(PEHeader,format(rsPEOptionalHeader ,[ImageNTHeader^.FileHeader.SizeOfOptionalHeader]));


    sFileType:='';
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_EXECUTABLE_IMAGE = IMAGE_FILE_EXECUTABLE_IMAGE then sFileType:=sFiletype+rsPEExecutable;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_RELOCS_STRIPPED = IMAGE_FILE_RELOCS_STRIPPED then sFileType:=sFiletype+rsPENoRelocations;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_LINE_NUMS_STRIPPED = IMAGE_FILE_LINE_NUMS_STRIPPED then sFileType:=sFiletype+rsPENoLineNumbers;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_LOCAL_SYMS_STRIPPED = IMAGE_FILE_LOCAL_SYMS_STRIPPED then sFileType:=sFiletype+rsPENoLocalSymbols;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_AGGRESIVE_WS_TRIM = IMAGE_FILE_AGGRESIVE_WS_TRIM then sFileType:=sFiletype+rsPEAgressiveTrim;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_BYTES_REVERSED_LO = IMAGE_FILE_BYTES_REVERSED_LO then sFileType:=sFiletype+rsPEReversedBytesLO;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_32BIT_MACHINE = IMAGE_FILE_32BIT_MACHINE then sFileType:=sFiletype+rsPE32Bit;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_DEBUG_STRIPPED = IMAGE_FILE_DEBUG_STRIPPED then sFileType:=sFiletype+rsPENoDbgInfo;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP = IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP then sFileType:=sFiletype+rsPERemovableRunFromSwap;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_NET_RUN_FROM_SWAP = IMAGE_FILE_NET_RUN_FROM_SWAP then sFileType:=sFiletype+rsPENetRunFromSwap;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_SYSTEM = IMAGE_FILE_SYSTEM then sFileType:=sFiletype+rsPESystemFile;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_DLL = IMAGE_FILE_DLL then sFileType:=sFiletype+rsPEDll;
    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_UP_SYSTEM_ONLY = IMAGE_FILE_UP_SYSTEM_ONLY then sFileType:=sFiletype+rsPEUpSystemOnly;

    if ImageNTHeader^.FileHeader.Characteristics and IMAGE_FILE_BYTES_REVERSED_HI = IMAGE_FILE_BYTES_REVERSED_HI then sFileType:=sFiletype+rsPEReversedBytesHI;


    sFileType:=copy(sfiletype,1,length(sfiletype)-2);
    if sFileType='' then
      sFileType:=rsPEUnknown;


    PEItv.Items.addchild(PEHeader,format(rsPECharacteristics ,[ImageNTHeader^.FileHeader.Characteristics, sFileType]));
    PEItv.Items.addchild(PEHeader,rsPEOptional);
    PEItv.Items.addchild(PEHeader,format(rsPEOptionalMagicNumber ,[ImageNTHeader^.OptionalHeader.Magic]));
    PEItv.Items.addchild(PEHeader,format(rsPEMajorLinkerVersion ,[ImageNTHeader^.OptionalHeader.MajorLinkerVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEMinorLinlerVersion ,[ImageNTHeader^.OptionalHeader.MinorLinkerVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPESizeOfCode ,[ImageNTHeader^.OptionalHeader.SizeOfCode, ImageNTHeader^.OptionalHeader.SizeOfCode]));

    PEItv.Items.addchild(PEHeader,format(rsPESizeOfInitializedData ,[ImageNTHeader^.OptionalHeader.SizeOfInitializedData, ImageNTHeader^.OptionalHeader.SizeOfInitializedData]));
    PEItv.Items.addchild(PEHeader,format(rsPESizeOfUninitializedData ,[ImageNTHeader^.OptionalHeader.SizeOfUninitializedData, ImageNTHeader^.OptionalHeader.SizeOfUninitializedData]));

    PEItv.Items.addchild(PEHeader,format(rsPEEntryPoint ,[ImageNTHeader^.OptionalHeader.AddressOfEntryPoint]));
    PEItv.Items.addchild(PEHeader,format(rsPEBaseOfCode ,[ImageNTHeader^.OptionalHeader.BaseOfCode]));
    if (not is64bit) then
    begin
      PEItv.Items.addchild(PEHeader,format(rsPEBaseOfData ,[ImageNTHeader^.OptionalHeader.BaseOfData]));
      PEItv.Items.addchild(PEHeader,format(rsPEPreferedImagebase ,[ImageNTHeader^.OptionalHeader.ImageBase]));
    end
    else
      PEItv.Items.addchild(PEHeader,format(rsPEPreferedImagebase2 ,[PUINT64(@ImageNTHeader^.OptionalHeader.BaseOfData)^]));


    PEItv.Items.addchild(PEHeader,format(rsPESectionAllignment ,[ImageNTHeader^.OptionalHeader.SectionAlignment]));
    PEItv.Items.addchild(PEHeader,format(rsPEFileAlignment ,[ImageNTHeader^.OptionalHeader.FileAlignment]));
    PEItv.Items.addchild(PEHeader,format(rsPEMajorOperatingSystemVersion ,[ImageNTHeader^.OptionalHeader.MajorOperatingSystemVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEMajorImageVersion ,[ImageNTHeader^.OptionalHeader.MajorImageVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEMinorImageVersion ,[ImageNTHeader^.OptionalHeader.MinorImageVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEMajorSubsystemVersion ,[ImageNTHeader^.OptionalHeader.MajorSubsystemVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEMinorSubsystemVersion ,[ImageNTHeader^.OptionalHeader.MinorSubsystemVersion]));
    PEItv.Items.addchild(PEHeader,format(rsPEWin32VersionValue ,[ImageNTHeader^.OptionalHeader.Win32VersionValue]));
    PEItv.Items.addchild(PEHeader,format(rsPESizeOfImage ,[ImageNTHeader^.OptionalHeader.SizeOfImage]));
    PEItv.Items.addchild(PEHeader,format(rsPESizeOfHeader ,[ImageNTHeader^.OptionalHeader.SizeOfHeaders]));
    PEItv.Items.addchild(PEHeader,format(rsPECheckSum ,[ImageNTHeader^.OptionalHeader.CheckSum]));
    PEItv.Items.addchild(PEHeader,format(rsPESubsystem ,[ImageNTHeader^.OptionalHeader.Subsystem]));
    PEItv.Items.addchild(PEHeader,format(rsPEDllCharacteristics ,[ImageNTHeader^.OptionalHeader.DllCharacteristics]));
    if is64bit then
    begin
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfStackReserve ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfStackReserve]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfStackCommit ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfStackCommit]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfHeapReserve ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeapReserve]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfHeapCommit ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeapCommit]));
      PEItv.Items.addchild(PEHeader,format(rsPELoaderFlags ,[PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.LoaderFlags]));
      numberofrva:=PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.NumberOfRvaAndSizes;
    end
    else
    begin
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfStackReserve ,[ImageNTHeader^.OptionalHeader.SizeOfStackReserve]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfStackCommit,[ImageNTHeader^.OptionalHeader.SizeOfStackCommit]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfHeapReserve ,[ImageNTHeader^.OptionalHeader.SizeOfHeapReserve]));
      PEItv.Items.addchild(PEHeader,format(rsPESizeOfHeapCommit ,[ImageNTHeader^.OptionalHeader.SizeOfHeapCommit]));
      PEItv.Items.addchild(PEHeader,format(rsPELoaderFlags ,[ImageNTHeader^.OptionalHeader.LoaderFlags]));
      numberofrva:=ImageNTHeader^.OptionalHeader.NumberOfRvaAndSizes;
    end;

    datadir:=PEItv.Items.addchild(PEHeader,format(rsPENumberOfRvaAndSize ,[numberofrva]));

    ImageSectionHeader:=PImageSectionHeader(ptrUint(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);
    PEItv.Items.addchild(PEHeader,rsPESections);

    maxaddress:=0;


    for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
    begin
      section:=PEItv.Items.addchild(PEHeader, copy(pchar(@ImageSectionHeader.Name[0]), 1, 8));

      PEItv.Items.addchild(section,format(rsPEVirtualSize,[ImageSectionHeader.Misc.VirtualSize]));
      PEItv.Items.addchild(section,format(rsPEVirtualAddress,[ImageSectionHeader.VirtualAddress]));
      PEItv.Items.addchild(section,format(rsPESizeOfRawData,[ImageSectionHeader.SizeOfRawData]));
      PEItv.Items.addchild(section,format(rsPEPointerToRawData,[ImageSectionHeader.PointerToRawData]));
      PEItv.Items.addchild(section,format(rsPEPointerToRelocations,[ImageSectionHeader.PointerToRelocations]));
      PEItv.Items.addchild(section,format(rsPEPointerToLineNumbers,[ImageSectionHeader.PointerToLinenumbers]));
      PEItv.Items.addchild(section,format(rsPENumberOfRelocations,[ImageSectionHeader.NumberOfRelocations]));
      PEItv.Items.addchild(section,format(rsPENumberOfLineNumbers,[ImageSectionHeader.NumberOfLinenumbers]));

      sCharacteristics:='';
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_CODE = IMAGE_SCN_CNT_CODE then sCharacteristics:=rsPEExecutableCode;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA = IMAGE_SCN_CNT_INITIALIZED_DATA then sCharacteristics:=sCharacteristics+rsPEInitializedData;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA = IMAGE_SCN_CNT_UNINITIALIZED_DATA then sCharacteristics:=sCharacteristics+rsPEUninitializedData;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_LNK_REMOVE = IMAGE_SCN_LNK_REMOVE then sCharacteristics:=sCharacteristics+rsPERemoved;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_DISCARDABLE = IMAGE_SCN_MEM_DISCARDABLE then sCharacteristics:=sCharacteristics+rsPEDiscardable;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_NOT_CACHED = IMAGE_SCN_MEM_NOT_CACHED then sCharacteristics:=sCharacteristics+rsPENotCached;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_NOT_PAGED = IMAGE_SCN_MEM_NOT_PAGED then sCharacteristics:=sCharacteristics+rsPENotPaged;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_SHARED = IMAGE_SCN_MEM_SHARED then sCharacteristics:=sCharacteristics+rsPESharedMemory;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_EXECUTE = IMAGE_SCN_MEM_EXECUTE then sCharacteristics:=sCharacteristics+rsPEExecutableMemory;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_READ = IMAGE_SCN_MEM_READ then sCharacteristics:=sCharacteristics+rsPEReadableMemory;
      if ImageSectionHeader.Characteristics and IMAGE_SCN_MEM_WRITE = IMAGE_SCN_MEM_WRITE then sCharacteristics:=sCharacteristics+rsPEWritableMemory;

      sCharacteristics:=copy(sCharacteristics,1,length(sCharacteristics)-2);

      if sCharacteristics='' then sCharacteristics:=rsPEUnknown;
      PEItv.Items.addchild(section,format(rsPECharacterisitics,[ImageSectionHeader.Characteristics, sCharacteristics]));

      if maxaddress<(ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData) then
        maxaddress:=ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData;

      inc(ImageSectionHeader);
    end;

    ImageSectionHeader:=PImageSectionHeader(ptrUint(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);

    if loaded then
    begin
      //in memory
      loadedmodule:=memorycopy;

      if is64bit then
      begin
        //no use in 5.4- , but let's do it anyhow
        basedifference:=ptrUint(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
        basedifference64:=UINT64(ptrUint(loadedmodule))-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
      end
      else
      begin
        basedifference:=ptrUint(loadedmodule)-ImageNTHeader^.OptionalHeader.ImageBase;
      end;

    end
    else
    begin
      //from a file
      loadedmodule:=virtualalloc(nil,maxaddress, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
      if loadedmodule=nil then raise exception.create(rsPEFailureAtAllocationMemory);
      ZeroMemory(loadedmodule,maxaddress);

      label2.caption:=inttohex(ptrUint(loadedmodule),8);
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

        basedifference:=ptrUint(loadedmodule)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
        basedifference64:=UINT64(ptrUint(loadedmodule))-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
      end
      else
      begin
        CopyMemory(@loadedmodule[0], @memorycopy[0], ImageNTHeader^.OptionalHeader.SizeOfHeaders);
        basedifference:=ptrUint(loadedmodule)-ImageNTHeader^.OptionalHeader.ImageBase;

      end;

    end;



    //now it has been mapped the vla and other stuff can be handled
    for i:=0 to numberofrva-1 do
    begin
      case i of
        0: sType:=rsPEExportTable;
        1: sType:=rsPEImportTable;
        2: sType:=rsPEResourceTable;
        3: sType:=rsPEExceptionTable;
        4: sType:=rsPECertificateTabel;
        5: sType:=rsPEBaseRelocationTable;
        6: sType:=rsPEDebuggingInfoTable;
        7: sType:=rsPEArchitectureSpecificTable;
        8: sType:=rsPEGlobalPointerTable;
        9: sType:=rsPETLSTable;
       10: sType:=rsPELoadConfigTable;
       11: sType:=rsPEBoundImportTable;
       12: sType:=rsPEImportAddressTable;
       13: sType:=rsPEDelayImportDescriptionTable;
       else sType:=rsPEReserved;
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
          ImageExportDirectory:=PImageExportDirectory(ptrUint(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
        else
          ImageExportDirectory:=PImageExportDirectory(ptrUint(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);

        lbExports.Items.Add(pchar(ptrUint(loadedmodule)+ImageExportDirectory.Name)+':');


        PEItv.Items.addchild(tempnode, format(rsPECharacteristics2, [ImageExportDirectory.Characteristics]));
        PEItv.Items.addchild(tempnode, format(rsPETimeDatastamp, [ImageExportDirectory.TimeDateStamp]));
        PEItv.Items.addchild(tempnode, format(rsPEMajorVersion, [ImageExportDirectory.MajorVersion]));
        PEItv.Items.addchild(tempnode, format(rsPEMinorVersion, [ImageExportDirectory.MinorVersion]));
        PEItv.Items.addchild(tempnode, format(rsPEName, [ImageExportDirectory.Name, pchar(ptrUint(loadedmodule)+ImageExportDirectory.Name)]));
        PEItv.Items.addchild(tempnode, format(rsPEBase, [ImageExportDirectory.Base]));
        PEItv.Items.addchild(tempnode, format(rsPENumberOfFunctions, [ImageExportDirectory.NumberOfFunctions]));
        PEItv.Items.addchild(tempnode, format(rsPENumberOfNames, [ImageExportDirectory.NumberOfNames]));
        tempnode2:=PEItv.Items.addchild(tempnode, format(rsPEAddressOfFunctions, [ptrUint(ImageExportDirectory.AddressOfFunctions)]));

        if ImageExportDirectory.NumberOfFunctions<>ImageExportDirectory.NumberOfNames then
          tempnode2.Text:=tempnode2.Text+rsPEinconsistent
        else
        begin
          for j:=0 to ImageExportDirectory.NumberOfFunctions-1 do
          begin
            //get name ordinal
            k:=pwordarray(ptrUint(loadedmodule)+ptrUint(ImageExportDirectory.AddressOfNameOrdinals))[j];

            lbExports.Items.Add(format('%x - %s',[pdwordarray(ptrUint(loadedmodule)+ptrUint(ImageExportDirectory.AddressOfFunctions))[k], pchar(ptrUint(loadedmodule)+pdwordarray(ptrUint(loadedmodule)+ptrUint(ImageExportDirectory.AddressOfNames))[j])]));
          end;


        end;



        for j:=0 to ImageExportDirectory.NumberOfFunctions-1 do
          PEItv.Items.addchild(tempnode2, format('%x',[pdwordarray(ptrUint(loadedmodule)+ptrUint(ImageExportDirectory.AddressOfFunctions))[j]]));

        tempnode2:=PEItv.Items.addchild(tempnode, format(rsPEAddressOfNames, [ptrUint(ImageExportDirectory.AddressOfNames)]));
        for j:=0 to ImageExportDirectory.NumberOfNames-1 do
          PEItv.Items.addchild(tempnode2, format('%s',[pchar(ptrUint(loadedmodule)+pdwordarray(ptrUint(loadedmodule)+ptrUint(ImageExportDirectory.AddressOfNames))[j])]));


        PEItv.Items.addchild(tempnode, format(rsPEAddressOfNameOrdinals, [ptrUint(ImageExportDirectory.AddressOfNameOrdinals)]));
      end
      else
      if i=1 then
      begin  //import

        j:=0;
        if is64bit then
          ImageImportDirectory:=PImageImportDirectory(ptrUint(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
        else
          ImageImportDirectory:=PImageImportDirectory(ptrUint(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);


        while (j<45) do
        begin
          if ImageImportDirectory.name=0 then break;

          if j>0 then
            lbImports.Items.Add('');

          s:=pchar(ptrUint(loadedmodule)+ImageImportDirectory.name);

          lbImports.Items.Add(format('%s', [s]));


          tempnode2:=PEItv.Items.addchild(tempnode,format(rsPEImport,[j, pchar(ptrUint(loadedmodule)+ImageImportDirectory.name)]));
          PEItv.Items.addchild(tempnode2, format(rsPECharacteristicsOriginalFirstThunk, [ImageImportDirectory.characteristicsOrFirstThunk]));
          PEItv.Items.addchild(tempnode2, format(rsPETimeDateStamp, [ImageImportDirectory.TimeDateStamp]));
          PEItv.Items.addchild(tempnode2, format(rsPEForwarderChain, [ImageImportDirectory.ForwarderChain]));
          PEItv.Items.addchild(tempnode2, format(rsPEName2, [ImageImportDirectory.name, pchar(ptrUint(loadedmodule)+ImageImportDirectory.name)]));
          PEItv.Items.addchild(tempnode2, format(rsPEFirstThunk, [ImageImportDirectory.FirstThunk]));

          tempnode3:=PEItv.Items.addchild(tempnode2, format(rsPEImports,[]));


          if ImageImportDirectory.ForwarderChain<>$ffffffff then
          begin
            importmodulename:=pchar(ptrUint(loadedmodule)+ImageImportDirectory.name);

            if not loaded then
              modhandle:=loadlibrary(pchar(importmodulename));

            k:=0;
            if is64bit then
            begin
              while PUINT64(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^<>0 do
              begin
                importaddress:=ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk+8*k;

                if InRangeX(importaddress, ptrUint(loadedmodule), ptrUint(loadedmodule)+maxaddress) then
                begin

                  if loaded then
                  begin
                    //lookup
                    tempaddress2:=pqwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k];
                    importfunctionname:=symhandler.getNameFromAddress(tempaddress2);

                    if uppercase(inttohex(tempaddress2,8))=uppercase(importfunctionname) then
                    begin
                      //failure to convert the address to an import
                      inc(k);
                      continue;
                    end;
                  end
                  else
                  begin
                    //get the name from the file
//                  tempaddress:=ptrUint(loadedmodule)+pqwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k]+2;
                    tempaddress:=ptrUint(loadedmodule)+pqwordarray(ptrUint(loadedmodule)+ImageImportDirectory.characteristicsOrFirstThunk)[k]+2;

                    if InRangeX(tempaddress, ptruint(loadedmodule), ptruint(loadedmodule)+maxaddress-100) then
                    begin
                      setlength(importfunctionname, 100);
                      CopyMemory(@importfunctionname[1], pointer(tempaddress), 99);
                      importfunctionname[99]:=#0;
                      s:=pchar(@importfunctionname[1]);
                      importfunctionname:=s;
                    end
                    else
                      importfunctionname:='err';
                  end;

                  PEItv.Items.addchild(tempnode3, format('%x (%x) - %s',[PUINT64(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^, importaddress, importfunctionname]));
                  lbImports.Items.Add( format('%x (%x) - %s',[PUINT64(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk+8*k)^, importaddress, importfunctionname]));

                  if not loaded then
                  begin
                    funcaddress:=ptrUint(getprocaddress(modhandle, pchar(importfunctionname)));
                    PUINT64(importaddress)^:=funcaddress;
                  end;

                end;

                inc(k);
              end;
            end
            else
            begin
              while PDWORD(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk+4*k)^<>0 do
              begin
                importaddress:=ptrUint(@pdwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k]);

                tempaddress:=ptrUint(loadedmodule)+pdwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k]+2;
                if loaded then
                begin
                  //lookup
                  tempaddress2:=pdwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k];
                  importfunctionname:=symhandler.getNameFromAddress(tempaddress2);

                  if uppercase(inttohex(tempaddress2,8))=uppercase(importfunctionname) then
                  begin
                    //failure to convert the address to an import
                    inc(k);
                    continue;
                  end;
                end
                else
                begin
                  //get the name from the file
                  if InRangeX(tempaddress, ptruint(loadedmodule), ptruint(loadedmodule)+memorycopysize-100) then
                  begin
                    setlength(importfunctionname, 100);
                    CopyMemory(@importfunctionname[1], pointer(tempaddress), 99);
                    importfunctionname[99]:=#0;
                  end
                  else
                    importfunctionname:='err';
                end;

                PEItv.Items.addchild(tempnode3, format('%x (%x) - %s',[pdwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k], importaddress, importfunctionname]));
                lbImports.Items.Add( format('%x (%x) - %s',[pdwordarray(ptrUint(loadedmodule)+ImageImportDirectory.FirstThunk)[k], importaddress-ptrUint(loadedmodule), importfunctionname]));

                if not loaded then
                begin
                  funcaddress:=ptrUint(getprocaddress(modhandle, pchar(importfunctionname)));
                  pdword(importaddress)^:=funcaddress;
                end;

                inc(k);
              end;
            end;
          end
          else
          begin
            PEItv.Items.addchild(tempnode3,rsPEStaticAddresses);
          end;

          inc(j);
          ImageImportDirectory:=PImageImportDirectory(ptrUint(ImageImportDirectory)+sizeof(TImageImportDirectory));
        end;

      end
      else
      if i=5 then
      begin
        // IMAGE_BASE_RELOCATION stuff
        if is64bit then
        begin
          ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress);
          maxaddress:=ptrUint(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].Size;
        end
        else
        begin
          ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);
          maxaddress:=ptrUint(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress+ImageNTHeader^.OptionalHeader.DataDirectory[i].Size;
        end;

        while ptrUint(ImageBaseRelocation)<maxaddress do
        begin
          if ImageBaseRelocation.SizeOfBlock=0 then break;

          VA:=PEItv.Items.addchild(tempnode,format(rsPEVirtualAddressBase, [ImageBaseRelocation.virtualaddress, ImageBaseRelocation.SizeOfBlock, ImageBaseRelocation.SizeOfBlock]));

          for j:=0 to ((ImageBaseRelocation.SizeOfBlock-8) div 2)-1 do
          begin
            PEItv.Items.addchild(va, format('%.3x : %d',[(ImageBaseRelocation.rel[j] and $fff), (ImageBaseRelocation.rel[j] shr 12) ]));
            lbBaseReloc.Items.Add(format('%.3x : %d',[ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff), (ImageBaseRelocation.rel[j] shr 12) ]));

            if not loaded then
            begin
              if (ImageBaseRelocation.rel[j] shr 12)=3 then            //replace the address at this address with a relocated one (dword)
                pdword(ptrUint(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=pdword(ptrUint(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference;

              if (ImageBaseRelocation.rel[j] shr 12)=10 then            //replace the address at this address with a relocated one (dword)
                PUINT64(ptrUint(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=PUINT64(ptrUint(loadedmodule)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference64;
            end;
          end;


          ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(ImageBaseRelocation)+ImageBaseRelocation.SizeOfBlock);
        end;
      end
      else
      if i=6 then
      begin
        //DEBUG INFO
        if is64bit then
          ImageDebugDirectory:=PImageDebugDirectory(ptrUint(loadedmodule)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
        else
          ImageDebugDirectory:=PImageDebugDirectory(ptrUint(loadedmodule)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);


        if InRangeX(ptruint(ImageDebugDirectory), ptrUint(loadedmodule),  ptruint(loadedmodule)+memorycopysize-sizeof(TImageDebugDirectory)) then
        begin
          PEItv.Items.addchild(tempnode, format(rsPEType, [ImageDebugDirectory.Type_]));

          if loaded then
          begin
            PEItv.Items.addchild(tempnode, format(rsPEAddressOfRawData, [ptrUint(loadedmodule)+ImageDebugDirectory.AddressOfRawData]));

          end
          else
            PEItv.Items.addchild(tempnode, format(rsPEAddressOfRawData, [ImageDebugDirectory.AddressOfRawData]));

           // PEItv.Items.addchild(tempnode, format('Pointer to raw data = %x', [ImageDebugDirectory.PointerToRawData]));

          if ImageDebugDirectory.Type_=IMAGE_DEBUG_TYPE_CODEVIEW then
          begin
            tempnode2:=PEItv.Items.addchild(tempnode, rsPECodeview);

            getmem(tempstring, 5);
            CopyMemory(tempstring, pointer(ptrUint(loadedmodule)+ImageDebugDirectory.AddressOfRawData),4);
            tempstring[4]:=#0;
            PEItv.Items.addchild(tempnode2, format(rsPESignature,[PDWORD(ptrUint(loadedmodule)+ImageDebugDirectory.AddressOfRawData)^, tempstring]));

            if PDWORD(ptrUint(loadedmodule)+ImageDebugDirectory.AddressOfRawData)^=$53445352 then  //RSDS
            begin
              PEItv.Items.addchild(tempnode2, format(rsPEDebugfile,[pchar(ptrUint(loadedmodule)+ImageDebugDirectory.AddressOfRawData+$18)]));

            end;

            freememandnil(tempstring);
          end;
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
        freememandnil(memorycopy);

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
    freememandnil(memorycopy);

  if loadedmodule<>nil then
  begin
    VirtualFree(loadedmodule,0,MEM_RELEASE);
    loadedmodule:=nil
  end;
end;

procedure TfrmPEInfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmPEInfo.miCopyEverythingClick(Sender: TObject);
var
  ss: TStringStream;
  i: integer;
begin
  ss:=TStringStream.Create;
  try
    PEItv.SaveToStream(ss);

    ss.WriteString(#13#10#13#10);
    ss.WriteString('---Imports---'+#13#10);
    ss.WriteString(lbImports.Items.Text);
    ss.WriteString(#13#10#13#10);
    ss.WriteString('---Exports---'+#13#10);
    ss.WriteString(lbExports.Items.Text);
    ss.WriteString(#13#10#13#10);
    ss.WriteString('---Relocs---'+#13#10);
    ss.WriteString(lbBaseReloc.Items.Text);


    ss.Position:=0;
    while ss.Position<ss.Size do
    begin
      if ss.ReadByte=0 then
      begin
        ss.position:=ss.position-1;
        ss.WriteByte(ord('.'));
      end;
    end;


    clipboard.AsText:=ss.DataString;


  finally
    ss.free;
  end;
end;

procedure TfrmPEInfo.miCopyTabClick(Sender: TObject);
var ss: TStringStream;
begin
  case PageControl1.TabIndex of
    0:
      begin
        ss:=TStringStream.Create('',TEncoding.Default, false);
        PEItv.SaveToStream(ss);
        Clipboard.AsText:=ss.DataString;

        ss.Position:=0;
        while ss.Position<ss.Size do
        begin
          if ss.ReadByte=0 then
          begin
            ss.position:=ss.position-1;
            ss.WriteByte(ord('.'));
          end;
        end;

        ss.free;
      end;

    1: Clipboard.AsText:=lbImports.Items.Text;
    2: Clipboard.AsText:=lbExports.Items.Text;
    3: Clipboard.AsText:=lbBaseReloc.Items.Text;
  end;

end;

procedure TfrmPEInfo.modulelistClick(Sender: TObject);
begin
  if modulelist.ItemIndex<>-1 then
    edtAddress.Text:=inttohex(ptrUint(modulelist.Items.Objects[modulelist.itemindex]),8);
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

  DPIHelper.AdjustSpeedButtonSize(LoadButton);
end;

procedure TfrmPEInfo.Button1Click(Sender: TObject);
var address: ptrUint;
    actualread: ptruint;
    headersize: dword;
    imagesize: dword;
    imagesizes: string;
    check: boolean;
begin
  try
    address:=StrToQWordEx('$'+edtAddress.text);
  except
    beep; //beeeeeeeeeeeeping idiot
    exit;
  end;

  if loadedmodule<>nil then
  begin
    virtualfree(loadedmodule,0,MEM_RELEASE);
    loadedmodule:=nil;
  end;

  if memorycopy<>nil then
    freememandnil(memorycopy);

  getmem(memorycopy,4096);
  try
    if (not readprocessmemory(processhandle,pointer(address),memorycopy,4096,actualread)) or (actualread<>4096) then
      raise exception.Create(rsPETheHeaderOfTheModeleCouldNobBeRead);

    headersize:=peinfo_getheadersize(memorycopy);
    if headersize=0 then
      raise exception.Create(rsPEThisIsNotaVakidPeFile);


    imagesize:=peinfo_getimagesize(memorycopy);

  finally
    freememandnil(memorycopy);

  end;


  if imagesize>256*1024*1024 then
  begin
    imagesizes:=inttostr(imagesize);
    if inputquery(rsPEPeinfoImageSize,rsPETheImagesizeIsMoreThan256, imagesizes) then
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
      messagedlg(rsPENotAllMemoryCouldBeReadWorkingWithaPartialCopyHere,mtwarning,[mbok],0);

    memorycopysize:=actualread;
  end else raise exception.Create(rsPEFailureReadingMemory);


  modulebase:=address;
  parsefile(true);
end;

procedure TfrmPEInfo.FormCreate(Sender: TObject);
begin
  pagecontrol1.TabIndex:=0;
end;

initialization
  {$i PEInfounit.lrs}

end.






