unit PEInfoFunctions;

{$MODE Delphi}

{
This unit will contain all functions used for PE-header inspection
}

{$warn 4056 off}

interface

uses
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf,SysUtils,classes, CEFuncProc,NewKernelHandler,FileMapping, commonTypeDefs;


const
  IMAGE_DOS_SIGNATURE                     = $5A4D;      { MZ }
  IMAGE_OS2_SIGNATURE                     = $454E;      { NE }
  IMAGE_OS2_SIGNATURE_LE                  = $454C;      { LE }
  IMAGE_VXD_SIGNATURE                     = $454C;      { LE }
  IMAGE_NT_SIGNATURE                      = $00004550;  { PE00 }

const
  IMAGE_SIZEOF_FILE_HEADER                 = 20;

  IMAGE_FILE_RELOCS_STRIPPED               = $0001;  { Relocation info stripped from file. }
  IMAGE_FILE_EXECUTABLE_IMAGE              = $0002;  { File is executable  (i.e. no unresolved externel references). }
  IMAGE_FILE_LINE_NUMS_STRIPPED            = $0004;  { Line nunbers stripped from file. }
  IMAGE_FILE_LOCAL_SYMS_STRIPPED           = $0008;  { Local symbols stripped from file. }
  IMAGE_FILE_AGGRESIVE_WS_TRIM             = $0010;  { Agressively trim working set }
  IMAGE_FILE_BYTES_REVERSED_LO             = $0080;  { Bytes of machine word are reversed. }
  IMAGE_FILE_32BIT_MACHINE                 = $0100;  { 32 bit word machine. }
  IMAGE_FILE_DEBUG_STRIPPED                = $0200;  { Debugging info stripped from file in .DBG file }
  IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP       = $0400;  { If Image is on removable media, copy and run from the swap file. }
  IMAGE_FILE_NET_RUN_FROM_SWAP             = $0800;  { If Image is on Net, copy and run from the swap file. }
  IMAGE_FILE_SYSTEM                        = $1000;  { System File. }
  IMAGE_FILE_DLL                           = $2000;  { File is a DLL. }
  IMAGE_FILE_UP_SYSTEM_ONLY                = $4000;  { File should only be run on a UP machine }
  IMAGE_FILE_BYTES_REVERSED_HI             = $8000;  { Bytes of machine word are reversed. }

  IMAGE_FILE_MACHINE_UNKNOWN               = 0;
  IMAGE_FILE_MACHINE_I386                  = $14c;   { Intel 386. }
  IMAGE_FILE_MACHINE_R3000                 = $162;   { MIPS little-endian, 0x160 big-endian }
  IMAGE_FILE_MACHINE_R4000                 = $166;   { MIPS little-endian }
  IMAGE_FILE_MACHINE_R10000                = $168;   { MIPS little-endian }
  IMAGE_FILE_MACHINE_ALPHA                 = $184;   { Alpha_AXP }
  IMAGE_FILE_MACHINE_POWERPC               = $1F0;   { IBM PowerPC Little-Endian }

  IMAGE_SIZEOF_SECTION_HEADER              = 40;
   {$EXTERNALSYM IMAGE_SIZEOF_SECTION_HEADER}


   IMAGE_SCN_TYPE_NO_PAD                    = $00000008;  { Reserved. }
   IMAGE_SCN_CNT_CODE                       = $00000020;  { Section contains code. }
   IMAGE_SCN_CNT_INITIALIZED_DATA           = $00000040;  { Section contains initialized data. }
   IMAGE_SCN_CNT_UNINITIALIZED_DATA         = $00000080;  { Section contains uninitialized data. }

   IMAGE_SCN_LNK_OTHER                      = $00000100;  { Reserved. }
   IMAGE_SCN_LNK_INFO                       = $00000200;  { Section contains comments or some other type of information. }
   IMAGE_SCN_LNK_REMOVE                     = $00000800;  { Section contents will not become part of image. }
   IMAGE_SCN_LNK_COMDAT                     = $00001000;  { Section contents comdat. }
   IMAGE_SCN_MEM_FARDATA                    = $00008000;
   IMAGE_SCN_MEM_PURGEABLE                  = $00020000;
   IMAGE_SCN_MEM_16BIT                      = $00020000;
   IMAGE_SCN_MEM_LOCKED                     = $00040000;
   IMAGE_SCN_MEM_PRELOAD                    = $00080000;

   IMAGE_SCN_ALIGN_1BYTES                   = $00100000;
   IMAGE_SCN_ALIGN_2BYTES                   = $00200000;
   IMAGE_SCN_ALIGN_4BYTES                   = $00300000;
   IMAGE_SCN_ALIGN_8BYTES                   = $00400000;
   IMAGE_SCN_ALIGN_16BYTES                  = $00500000;  { Default alignment if no others are specified. }
   IMAGE_SCN_ALIGN_32BYTES                  = $00600000;
   IMAGE_SCN_ALIGN_64BYTES                  = $00700000;

   IMAGE_SCN_LNK_NRELOC_OVFL                = $01000000;  { Section contains extended relocations. }
   IMAGE_SCN_MEM_DISCARDABLE                = $02000000;  { Section can be discarded. }
   IMAGE_SCN_MEM_NOT_CACHED                 = $04000000;  { Section is not cachable. }
   IMAGE_SCN_MEM_NOT_PAGED                  = $08000000;  { Section is not pageable. }
   IMAGE_SCN_MEM_SHARED                     = $10000000;  { Section is shareable. }
   IMAGE_SCN_MEM_EXECUTE                    = $20000000;  { Section is executable. }
   IMAGE_SCN_MEM_READ                       = $40000000;  { Section is readable. }
   IMAGE_SCN_MEM_WRITE                      = DWORD($80000000);  { Section is writeable. }

type
  TSectionInfo=class
    name: string;
    virtualAddress: ptruint;
    fileAddress: dword;
    size: dword;
  end;


  {$ifdef windows}
type
  _IMAGE_OPTIONAL_HEADER64 = packed record
    { Standard fields. }
    Magic: Word;
    MajorLinkerVersion: Byte;
    MinorLinkerVersion: Byte;
    SizeOfCode: DWORD;
    SizeOfInitializedData: DWORD;
    SizeOfUninitializedData: DWORD;
    AddressOfEntryPoint: DWORD;
    BaseOfCode: DWORD;
    //BaseOfData: DWORD;
    { NT additional fields. }
    ImageBase: UINT64;
    SectionAlignment: DWORD;
    FileAlignment: DWORD;
    MajorOperatingSystemVersion: Word;
    MinorOperatingSystemVersion: Word;
    MajorImageVersion: Word;
    MinorImageVersion: Word;
    MajorSubsystemVersion: Word;
    MinorSubsystemVersion: Word;
    Win32VersionValue: DWORD;
    SizeOfImage: DWORD;
    SizeOfHeaders: DWORD;
    CheckSum: DWORD;
    Subsystem: Word;
    DllCharacteristics: Word;
    SizeOfStackReserve: UINT64;
    SizeOfStackCommit: UINT64;
    SizeOfHeapReserve: UINT64;
    SizeOfHeapCommit: UINT64;
    LoaderFlags: DWORD;
    NumberOfRvaAndSizes: DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
  end;
  TImageOptionalHeader64 = _IMAGE_OPTIONAL_HEADER64;
  IMAGE_OPTIONAL_HEADER64 = _IMAGE_OPTIONAL_HEADER64;
  PImageOptionalHeader64 = ^TImageOptionalHeader64;


type PUINT64=^UINT64;
type TUINT64Array=array[0..0] of UINT64;
type PUINT64Array=^TDwordArray;

type TImportEntry=packed record
  xxx: word;
  name: pchar;
end;
type PInportEntry=^TImportEntry;


type TImageImportDirectory=record
  characteristicsOrFirstThunk: dword;  //0 for terminating null import descriptor || RVA to original unbound IAT (PIMAGE_THUNK_DATA)
  TimeDateStamp: dword;
  ForwarderChain: dword; //-1 if none
  Name: dword;
  FirstThunk: dword;
end;
type PImageImportDirectory=^TImageImportDirectory;

PImageExportDirectory = ^TImageExportDirectory;
_IMAGE_EXPORT_DIRECTORY = packed record
    Characteristics: DWord;      //0-3
    TimeDateStamp: DWord;        //4-7
    MajorVersion: Word;          //8-9
    MinorVersion: Word;          //a-b
    Name: DWord;                 //c-f
    Base: DWord;                 //10-13
    NumberOfFunctions: DWord;    //14-17
    NumberOfNames: DWord;        //18-1b
    AddressOfFunctions: DWORD;   //1c-1f
    AddressOfNames: DWORD;       //20-23
    AddressOfNameOrdinals: DWORD;//24-27
end;
TImageExportDirectory = _IMAGE_EXPORT_DIRECTORY;
IMAGE_EXPORT_DIRECTORY = _IMAGE_EXPORT_DIRECTORY;

PImageNtHeaders = ^TImageNtHeaders;
_IMAGE_NT_HEADERS = packed record
  Signature: DWORD;
  FileHeader: TImageFileHeader;
  OptionalHeader: TImageOptionalHeader32;
end;
TImageNtHeaders = _IMAGE_NT_HEADERS;
IMAGE_NT_HEADERS = _IMAGE_NT_HEADERS;


type TIMAGE_BASE_RELOCATION=record
  virtualaddress: dword;
  SizeOfBlock: dword;
  rel: TWordArray;
end;
type PIMAGE_BASE_RELOCATION=^TIMAGE_BASE_RELOCATION;


TISHMisc = packed record
  case Integer of
    0: (PhysicalAddress: DWORD);
    1: (VirtualSize: DWORD);
end;

PImageSectionHeader = ^TImageSectionHeader;
_IMAGE_SECTION_HEADER = packed record
  Name: packed array[0..IMAGE_SIZEOF_SHORT_NAME-1] of Byte;
  Misc: TISHMisc;
  VirtualAddress: DWORD;
  SizeOfRawData: DWORD;
  PointerToRawData: DWORD;
  PointerToRelocations: DWORD;
  PointerToLinenumbers: DWORD;
  NumberOfRelocations: Word;
  NumberOfLinenumbers: Word;
  Characteristics: DWORD;
end;
TImageSectionHeader = _IMAGE_SECTION_HEADER;
IMAGE_SECTION_HEADER = _IMAGE_SECTION_HEADER;

PImageDosHeader = ^TImageDosHeader;
_IMAGE_DOS_HEADER = packed record      { DOS .EXE header                  }
    e_magic: Word;                     { Magic number                     }
    e_cblp: Word;                      { Bytes on last page of file       }
    e_cp: Word;                        { Pages in file                    }
    e_crlc: Word;                      { Relocations                      }
    e_cparhdr: Word;                   { Size of header in paragraphs     }
    e_minalloc: Word;                  { Minimum extra paragraphs needed  }
    e_maxalloc: Word;                  { Maximum extra paragraphs needed  }
    e_ss: Word;                        { Initial (relative) SS value      }
    e_sp: Word;                        { Initial SP value                 }
    e_csum: Word;                      { Checksum                         }
    e_ip: Word;                        { Initial IP value                 }
    e_cs: Word;                        { Initial (relative) CS value      }
    e_lfarlc: Word;                    { File address of relocation table }
    e_ovno: Word;                      { Overlay number                   }
    e_res: array [0..3] of Word;       { Reserved words                   }
    e_oemid: Word;                     { OEM identifier (for e_oeminfo)   }
    e_oeminfo: Word;                   { OEM information; e_oemid specific}
    e_res2: array [0..9] of Word;      { Reserved words                   }
    _lfanew: LongInt;                  { File address of new exe header   }
end;
TImageDosHeader = _IMAGE_DOS_HEADER;
IMAGE_DOS_HEADER = _IMAGE_DOS_HEADER;


(*
typedef struct IMAGE_COR20_HEADER
{
    // Header versioning
    DWORD                   cb;
    WORD                    MajorRuntimeVersion;
    WORD                    MinorRuntimeVersion;

    // Symbol table and startup information
    IMAGE_DATA_DIRECTORY    MetaData;
    DWORD                   Flags;

    // If COMIMAGE_FLAGS_NATIVE_ENTRYPOINT is not set, EntryPointToken represents a managed entrypoint.
    // If COMIMAGE_FLAGS_NATIVE_ENTRYPOINT is set, EntryPointRVA represents an RVA to a native entrypoint.
    union {
        DWORD               EntryPointToken;
        DWORD               EntryPointRVA;
    } DUMMYUNIONNAME;

    // Binding information
    IMAGE_DATA_DIRECTORY    Resources;
    IMAGE_DATA_DIRECTORY    StrongNameSignature;

    // Regular fixup and binding information
    IMAGE_DATA_DIRECTORY    CodeManagerTable;
    IMAGE_DATA_DIRECTORY    VTableFixups;
    IMAGE_DATA_DIRECTORY    ExportAddressTableJumps;

    // Precompiled image info (internal use only - set to zero)
    IMAGE_DATA_DIRECTORY    ManagedNativeHeader;

} IMAGE_COR20_HEADER, *PIMAGE_COR20_HEADER;

*)
TImageCLRRuntimeDirectory=record
  cb: DWORD;
  MajorRuntimeVersion: WORD;
  MinorRuntimeVersion: WORD;
  MetaData: IMAGE_DATA_DIRECTORY;
  Flags: DWORD;
  EntryPointTokenOrRVA: DWORD;
  Resources: IMAGE_DATA_DIRECTORY;
  StrongNameSignature: IMAGE_DATA_DIRECTORY;
  CodeManagerTable: IMAGE_DATA_DIRECTORY;
  VTableFixups: IMAGE_DATA_DIRECTORY;
  ExportAddressTableJumps: IMAGE_DATA_DIRECTORY;
  ManagedNativeHeader: IMAGE_DATA_DIRECTORY;
end;

PImageCLRRuntimeDirectory=^TImageCLRRuntimeDirectory;


type
  TRunTimeEntry=packed record
    start: dword;
    stop: dword;
    unwind: dword;
  end;
  PRuntimeEntry=^TRunTimeEntry;

  TRunTimeList=array [0..10000] of TRunTimeEntry;
  PRuntimeList=^TRuntimeList;

  TExceptionList=class
  private
    fmodulebase: ptruint;
    exceptionAddress: ptruint;
    size: integer;


    list: PRuntimeList;
    function getEntry(index: integer): TRunTimeEntry;
    function getCount: integer;
  public
    function getRunTimeEntry(address: ptruint): PRuntimeEntry;
    constructor create(modulebase: ptruint; ela: ptruint; els: integer);
    destructor destroy; override;
    property ModuleBase: ptruint read fModuleBase;
    property Count: integer read getCount;
    property Entry[index: integer]: TRunTimeEntry read getEntry; default;


  end;



function peinfo_getSectionList(modulebase: ptruint; sectionList: Tstrings): boolean;
function peinfo_getImageNtHeaders(headerbase: pointer; maxsize: dword):PImageNtHeaders;
function peinfo_getExportList(filename: string; dllList: Tstrings): boolean; overload;
function peinfo_getExportList(modulebase: ptruint; dllList: Tstrings): boolean; overload;
function peinfo_is64bitfile(filename: string; var is64bit: boolean): boolean;
function peinfo_isdotnetfile(filename: string; var isdotnet: boolean): boolean;
function peinfo_getimagesizefromfile(filename: string; var size: dword): boolean;

function peinfo_getExceptionList(modulebase: ptruint): TExceptionList;

function peinfo_getImageSectionHeader(headerbase: pointer; maxsize: dword): PImageSectionHeader;

     {$endif}


implementation

{$ifdef windows}
uses ProcessHandlerUnit, PEInfounit;


function peinfo_getImageDosHeader(headerbase: pointer):PImageDosHeader;
{
basicly returns the headerbase, or returns nil if it isn't a valid MZ header
}
begin
  if PImageDosHeader(headerbase)^.e_magic<>IMAGE_DOS_SIGNATURE then
    result:=nil
  else
    result:=headerbase;
end;

function peinfo_getImageNtHeaders(headerbase: pointer; maxsize: dword):PImageNtHeaders;
{
Returns the base of the NT Headers
Returns nil if nto a valid exe/pe
}
var DH: PImageDosHeader;
    NTH: PImageNtHeaders;
begin
  result:=nil;
  DH:=peinfo_getImageDosHeader(headerbase);
  if dh=nil then exit;

  if DH^._lfanew>maxsize then exit; //out of range
  
  NTH:=PImageNtHeaders(ptrUint(headerbase)+DH^._lfanew);
  if NTH^.Signature<>IMAGE_NT_SIGNATURE then exit;

  result:=NTH;
end;

function peinfo_getOptionalHeaders(headerbase: pointer; maxsize: dword): pointer;
{
Returns a pointer to the optional header.
Can return that of both 64-bit and 32-bit
}
var NTH: PImageNtHeaders;
begin
  result:=nil;
  NTH:=peinfo_getImageNtHeaders(headerbase,maxsize);
  if NTH=nil then exit;

  result:=@NTH^.OptionalHeader;
end;

function peinfo_getImageSectionHeader(headerbase: pointer; maxsize: dword): PImageSectionHeader;
var NTH: PImageNtHeaders;
    ISH: PImageSectionHeader;
begin
  result:=nil;
  NTH:=peinfo_getImageNtHeaders(headerbase,maxsize);
  if NTH=nil then exit;

  ISH:=PImageSectionHeader(ptrUint(@NTH^.OptionalHeader)+NTH^.FileHeader.SizeOfOptionalHeader);

  if (ptrUint(ISH)-ptrUint(headerbase))>maxsize then exit; //overflow
  result:=ISH;
end;

function peinfo_VirtualAddressToFileAddress(header: pointer; maxsize: dword; VirtualAddress: dword): pointer;
{
Go through the sections to find out where this virtual address belongs and return the offset starting from the header, offset 0
}
var i: integer;
    ImageNTHeader: PImageNtHeaders;
    ImageSectionHeader: PImageSectionHeader;
begin
  result:=nil;
  ImageNTHeader:=peinfo_getImageNtHeaders(header,maxsize);
  if ImageNTHeader=nil then exit;

  //het the pointer to the first imagesection header
  ImageSectionHeader:=peinfo_getImageSectionHeader(header,maxsize);
  for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
  begin
    if (VirtualAddress>=ImageSectionHeader.VirtualAddress) and (VirtualAddress<(ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData)) then
    begin
      //found it
      if ImageSectionHeader.PointerToRawData+(VirtualAddress-ImageSectionHeader.VirtualAddress)>maxsize then exit; //too big

      //set the found address
      result:=pointer(ptrUint(header)+ImageSectionHeader.PointerToRawData+(VirtualAddress-ImageSectionHeader.VirtualAddress));
      exit;
    end;
    inc(ImageSectionHeader); //next one
  end;
end;

resourcestring
  strInvalidFile='Invalid file';
  rsPEIFNoExports = 'No exports';


function TExceptionList.getRunTimeEntry(address: ptruint): PRunTimeEntry;
var
  i, count: integer;
begin
  //todo: use a sorted scan
  result:=nil;
  count:=size div 12;

  address:=address-ModuleBase;

  for i:=0 to count-1 do
  begin
    if InRangeQ(address, list^[i].start, list^[i].stop-1) then
      exit(@list^[i]);
  end;
end;


function TExceptionList.getEntry(index: integer): TRunTimeEntry;
begin
  if index<count then
    result:=list[index]
  else
    raise exception.create('Invalid index');
end;

function TExceptionList.getCount: integer;
begin
  result:=size div 12;
end;

constructor TExceptionList.create(modulebase: ptruint; ela: ptruint; els: integer);
var br: ptruint;
begin
  fmodulebase:=modulebase;
  exceptionAddress:=ela;
  size:=els;

  getmem(list,size);

  readprocessmemory(processhandle, pointer(modulebase+exceptionAddress), list, size,br);

  size:=br;
end;

destructor TExceptionList.destroy;
begin
  freemem(list);
  inherited destroy;
end;

function peinfo_getExceptionList(modulebase: ptruint): TExceptionList;
var
  ar: ptruint;
  header: pointer;
  ImageNtHeader: PImageNtHeaders;
  OptionalHeader: PImageOptionalHeader;
  OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
  is64bit: boolean;
begin
  result:=nil;
  getmem(header, 8192);
  try
    if readProcessMemory(processhandle, pointer(modulebase),header, 8192, ar)=false then exit;
    ImageNtHeader:=peinfo_getImageNtHeaders(header, 8192);
    if ImageNTHeader=nil then exit;
    is64bit:=ImageNTHeader^.FileHeader.Machine=$8664;

    if is64bit=false then exit;


    OptionalHeader:=peinfo_getOptionalHeaders(header, 8192);
    if OptionalHeader=nil then exit;

    if OptionalHeader64^.DataDirectory[3].VirtualAddress<>0 then
      result:=TExceptionList.Create(modulebase, OptionalHeader64^.DataDirectory[3].VirtualAddress, OptionalHeader64^.DataDirectory[3].Size);

  finally
    freemem(header);
  end;
end;



function peinfo_getSectionList(modulebase: ptruint; sectionList: Tstrings): boolean;
//WARNING: The caller has to free the sectionlist items after the call
var
  ar: ptruint;
  header: pointer;
  headersize: integer;
  ImageNtHeader: PImageNtHeaders;
  Sectionheader: PImageSectionHeader;
  i: integer;

  si: TSectionInfo;
  s: string;
begin
  result:=false;
  getmem(header, 8192);
  try
    if readProcessMemory(processhandle, pointer(modulebase),header, 8192, ar)=false then exit;
    headersize:=peinfo_getheadersize(header);

    if (headersize>0) and (headersize<512*1024) then
    begin
      freemem(header);
      getmem(header,headersize);
      ar:=0;
      readProcessMemory(processhandle, pointer(modulebase),header, headersize, ar);
      headersize:=ar;
    end else exit(false);

    ImageNtHeader:=peinfo_getImageNtHeaders(header,headersize);
    if ImageNtHeader<>nil then
    begin

      Sectionheader:=peinfo_getImageSectionHeader(header, headersize);

      if sectionheader<>nil then
      begin
        for i:=0 to ImageNtHeader.FileHeader.NumberOfSections-1 do
        begin
          if ptruint(sectionheader)>ptruint(header)+headersize then break;

          s:=pchar(@sectionheader^.Name[0]);
          if s<>'' then
          begin
            si:=TSectionInfo.Create;
            si.name:=s;
            si.virtualAddress:=modulebase+sectionheader^.VirtualAddress;
            si.fileAddress:=sectionheader^.PointerToRawData;
            si.size:=sectionheader^.SizeOfRawData;

            sectionlist.AddObject(si.name, si);
            sectionheader:=PImageSectionHeader(ptruint(sectionheader)+sizeof(TImageSectionHeader));
          end;
        end;
        result:=true;
      end;

    end;


  finally
    freemem(header);
  end;
end;

function peinfo_getExportList(modulebase: ptruint; dllList: Tstrings): boolean;
var
  header: pointer;
  ImageNtHeader: PImageNtHeaders;
  OptionalHeader: PImageOptionalHeader;
  OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
  ImageExportDirectory: PImageExportDirectory;
  is64bit: boolean;

  addresslist: PDwordArray;
  exportlist: PDwordArray;
  functionname: pchar;
  i: integer;
  ar:ptruint;

  imagesize: dword;

  ordcount: integer;
begin
  result:=false;

  getmem(header, 4096);
  try
    if readProcessMemory(processhandle, pointer(modulebase),header, 4096, ar)=false then raise exception.create('Invalid memory address');

    ImageNtHeader:=peinfo_getImageNtHeaders(header, 4096);
    if ImageNTHeader=nil then raise exception.create(strInvalidFile);
    is64bit:=ImageNTHeader^.FileHeader.Machine=$8664;

    OptionalHeader:=peinfo_getOptionalHeaders(header, 4096);
    if OptionalHeader=nil then raise exception.create(strInvalidFile);

    imagesize:=OptionalHeader.SizeOfImage;
    freemem(header);
    getmem(header,imagesize);
    ar:=0;
    if readProcessMemory(processhandle, pointer(modulebase),header, imagesize, ar)=false then
    begin
      imagesize:=ar;
      if imagesize=0 then raise exception.create('Invalid header');
    end;

    ImageNtHeader:=peinfo_getImageNtHeaders(header, imagesize);
    OptionalHeader:=peinfo_getOptionalHeaders(header, imagesize);



    if ImageNTHeader=nil then raise exception.Create(strInvalidFile);
    if OptionalHeader=nil then raise exception.Create(strInvalidFile);

    if is64bit then
      ImageExportDirectory:=PImageExportDirectory(ptruint(header)+OptionalHeader64^.DataDirectory[0].VirtualAddress)
    else
      ImageExportDirectory:=PImageExportDirectory(ptruint(header)+OptionalHeader^.DataDirectory[0].VirtualAddress);


    if (ptruint(ImageExportDirectory)<=ptruint(header)) or (ptruint(ImageExportDirectory)>=(ptruint(header)+imagesize)) then
      raise exception.create(rsPEIFNoExports);

    exportlist:=PDWordArray(ptruint(header)+dword(ImageExportDirectory^.AddressOfNames));
    addresslist:=PDWordArray(ptruint(header)+dword(ImageExportDirectory^.AddressOfFunctions));

    if (ptruint(exportlist)<=ptruint(header)) or (ptruint(exportlist)>=(ptruint(header)+imagesize)) then
      raise exception.create(rsPEIFNoExports);

    if (ptruint(addresslist)<=ptruint(header)) or (ptruint(addresslist)>=(ptruint(header)+imagesize)) then
      raise exception.create(rsPEIFNoExports);

    ordcount:=ImageExportDirectory.NumberOfFunctions-ImageExportDirectory.NumberOfNames;

    for i:=0 to ImageExportDirectory.NumberOfNames-1 do
    begin
      functionname:=pchar(ptruint(header)+exportlist[i]);

      if functionname<>nil then
        dllList.AddObject(functionname, pointer(ptruint(modulebase+addresslist[i+ordcount])));
    end;
    result:=true;
  finally
    if header<>nil then
      freemem(header);
  end;
end;


function peinfo_getExportList(filename: string; dllList: Tstrings): boolean;
var fmap: TFileMapping;
    header: pointer;
    ImageNtHeader: PImageNtHeaders;
    OptionalHeader: PImageOptionalHeader;
    OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
    ImageExportDirectory: PImageExportDirectory;
    is64bit: boolean;

    addresslist: PDwordArray;
    exportlist: PDwordArray;
    functionname: pchar;
    i: integer;

    ordinaloffset: integer;

   // a: dword;
begin
  //open the file
  //parse the header
  //get rva 0
  //write rva0 to list
  result:=false;

  fmap:=TFileMapping.create(filename);
  if fmap<>nil then
  begin
    try
      header:=fmap.fileContent;

      ImageNtHeader:=peinfo_getImageNtHeaders(header,fmap.filesize);


      if ImageNTHeader=nil then raise exception.Create(strInvalidFile);
      if ImageNTHeader^.FileHeader.Machine=$8664 then
        is64bit:=true
      else
        is64bit:=false;

      OptionalHeader:=peinfo_getOptionalHeaders(header,fmap.filesize);
      if OptionalHeader=nil then raise exception.Create(strInvalidFile);

      if is64bit then
        ImageExportDirectory:=peinfo_VirtualAddressToFileAddress(header, fmap.filesize, OptionalHeader64^.DataDirectory[0].VirtualAddress)
      else
        ImageExportDirectory:=peinfo_VirtualAddressToFileAddress(header, fmap.filesize, OptionalHeader^.DataDirectory[0].VirtualAddress);

      if ImageExportDirectory=nil then raise exception.Create(rsPEIFNoExports);

      exportlist:=peinfo_VirtualAddressToFileAddress(header,fmap.filesize, dword(ImageExportDirectory.AddressOfNames));
      addresslist:=peinfo_VirtualAddressToFileAddress(header,fmap.filesize, dword(ImageExportDirectory.AddressOfFunctions));

      if exportlist=nil then raise exception.Create(rsPEIFNoExports);

      if ImageExportDirectory.NumberOfFunctions>ImageExportDirectory.NumberOfNames then
      begin
        ordinaloffset:=ImageExportDirectory.NumberOfFunctions-ImageExportDirectory.NumberOfNames;
        for i:=0 to ordinaloffset-1 do
          dllList.AddObject('Ordinal'+inttostr(ImageExportDirectory.Base+i), pointer(ptruint(addresslist[i])));
      end
      else
        ordinaloffset:=0;

      for i:=0 to ImageExportDirectory.NumberOfNames-1 do
      begin
        functionname:=peinfo_VirtualAddressToFileAddress(header,fmap.filesize, exportlist[i]);

        if functionname<>nil then
          dllList.AddObject(functionname, pointer(ptruint(addresslist[ordinaloffset+i])));
      end;
      result:=true;

    finally
      fmap.free;
    end;

  end;
end;

function peinfo_getimagesizefromfile(filename: string; var size: dword): boolean;
var
  fmap: TFileMapping;
  header: pointer;
  ImageNtHeader: PImageNtHeaders;
  OptionalHeader: PImageOptionalHeader;
  OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
begin
  result:=false;

  try
    fmap:=TFileMapping.create(filename);
    if fmap<>nil then
    begin
      try
        header:=fmap.fileContent;
        ImageNtHeader:=peinfo_getImageNtHeaders(header,fmap.filesize);
        OptionalHeader:=peinfo_getOptionalHeaders(header,fmap.filesize);
        if ImageNTHeader=nil then raise exception.Create(strInvalidFile);
        if ImageNTHeader^.FileHeader.Machine=$8664 then
          size:=OptionalHeader64.SizeOfImage
        else
          size:=OptionalHeader.SizeOfImage;

        result:=true; //success
      finally
        fmap.free;
      end;

    end;
  except

  end;
end;

function peinfo_isdotnetfile(filename: string; var isdotnet: boolean): boolean;
var fmap: TFileMapping;
    header: pointer;
    OptionalHeader: PImageOptionalHeader;
    OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
    is64bit: boolean;
    ImageNtHeader: PImageNtHeaders;
    netdata: dword;
begin
  result:=false;

  try
    fmap:=TFileMapping.create(filename);
    if fmap<>nil then
    begin
      try
        header:=fmap.fileContent;
        ImageNtHeader:=peinfo_getImageNtHeaders(header,fmap.filesize);
        if ImageNTHeader=nil then raise exception.Create(strInvalidFile);
        if ImageNTHeader^.FileHeader.Machine=$8664 then
          is64bit:=true
        else
          is64bit:=false;

        OptionalHeader:=peinfo_getOptionalHeaders(header,fmap.filesize);
        if OptionalHeader<>nil then
        begin
          if is64bit then
            netdata:=OptionalHeader64.DataDirectory[14].Size
          else
            netdata:=OptionalHeader.DataDirectory[14].Size;

          isdotnet:=netdata<>0;
          result:=true;
        end;
      finally
        fmap.free;
      end;

    end;
  except

  end;
end;

function peinfo_is64bitfile(filename: string; var is64bit: boolean): boolean;
var fmap: TFileMapping;
    header: pointer;
    ImageNtHeader: PImageNtHeaders;
begin
  //open the file
  //parse the header
  //fill in is64bit and return true.
  //On exception, false

  result:=false;

  try

    fmap:=TFileMapping.create(filename);
    if fmap<>nil then
    begin
      try
        header:=fmap.fileContent;
        ImageNtHeader:=peinfo_getImageNtHeaders(header,fmap.filesize);
        if ImageNTHeader=nil then raise exception.Create(strInvalidFile);
        if ImageNTHeader^.FileHeader.Machine=$8664 then
          is64bit:=true
        else
          is64bit:=false;

        result:=true; //success

      finally
        fmap.free;
      end;

    end;
  except

  end;
end;

{$endif}


end.


