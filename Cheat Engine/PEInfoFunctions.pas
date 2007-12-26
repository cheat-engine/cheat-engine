unit PEInfoFunctions;
{
This unit will contain all functions used for PE-header inspection
}

interface

uses windows,SysUtils,classes, cefuncproc,newkernelhandler,filemapping;

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

type TIMAGE_BASE_RELOCATION=record
  virtualaddress: dword;
  SizeOfBlock: dword;
  rel: TWordArray;
end;
type PIMAGE_BASE_RELOCATION=^TIMAGE_BASE_RELOCATION;


function peinfo_getImageNtHeaders(headerbase: pointer; maxsize: dword):PImageNtHeaders;
function peinfo_getExportList(filename: string; dllList: Tstrings): boolean;

implementation

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
  
  NTH:=PImageNtHeaders(dword(headerbase)+DH^._lfanew);
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

  ISH:=PImageSectionHeader(dword(@NTH^.OptionalHeader)+NTH^.FileHeader.SizeOfOptionalHeader);

  if (dword(ISH)-dword(headerbase))>maxsize then exit; //overflow
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
      result:=pointer(dword(header)+ImageSectionHeader.PointerToRawData+(VirtualAddress-ImageSectionHeader.VirtualAddress));
      exit;
    end;
    inc(ImageSectionHeader); //next one
  end;
end;

function peinfo_getExportList(filename: string; dllList: Tstrings): boolean;
resourcestring strInvalidFile='Invalid file';
var fmap: TFileMapping;
    header: pointer;
    ImageNtHeader: PImageNtHeaders;
    OptionalHeader: PImageOptionalHeader;
    OptionalHeader64: PImageOptionalHeader64 absolute OptionalHeader;
    ImageExportDirectory: PImageExportDirectory;
    is64bit: boolean;

    exportlist: PDwordArray;
    functionname: pchar;
    i: integer;
begin
  //open the file
  //parse the header
  //get rva 0
  //write rva0 to list
  fmap:=TFileMapping.create(filename);
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

    if ImageExportDirectory=nil then raise exception.Create(strInvalidFile);

    exportlist:=peinfo_VirtualAddressToFileAddress(header,fmap.filesize, dword(ImageExportDirectory.AddressOfNames));
    if exportlist=nil then raise exception.Create('No exports');

    for i:=0 to ImageExportDirectory.NumberOfNames-1 do
    begin
      functionname:=peinfo_VirtualAddressToFileAddress(header,fmap.filesize, exportlist[i]);

      if functionname<>nil then
        dllList.add(functionname);
    end;

  finally
    fmap.free;
  end;
  
  result:=true;
end;

end.


