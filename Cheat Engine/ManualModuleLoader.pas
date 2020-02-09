unit ManualModuleLoader;

{$MODE Delphi}

{
This routine will examine a module and then load it into memory, taking care of the sections and IAT addresses
}

interface

{$ifdef windows}

uses windows, LCLIntf, classes, sysutils, imagehlp, dialogs, PEInfoFunctions,CEFuncProc,
     NewKernelHandler, symbolhandler, dbk32functions, vmxfunctions, commonTypeDefs,
     SymbolListHandler, symbolhandlerstructs, StringHashList;

resourcestring
  rsMMLNotAValidFile = 'not a valid file';
  rsMMLNotAValidHeader = 'Not a valid header';
  rsMMLTriedLoadingA64BitModuleOnA32BitSystem = 'Tried loading a 64-bit module on a 32-bit system';
  rsMMLAllocationError = 'Allocation error';
  rsMMLFailedFindingAddressOf = 'failed finding address of ';

type TModuleLoader=class
  private
    filename: string;
    FLoaded: boolean;
    FEntrypoint: ptruint;
    fSymbolList: TSymbolListHandler;

    destinationBase: ptruint;
    modulesize: integer;
    is64bit: boolean;
    isdriver: boolean;

    pid: dword;

    importlist: TStringHashList;
    procedure cleanupExportList;
    function FindKernelModuleExport(modulename: string; exportname: string): ptruint;
  public
    Exporttable: TStringlist;
    procedure createSymbolListHandler;
    constructor create(filename: string);
  published
    property BaseAddress: ptruint read destinationBase;
    property Loaded: boolean read FLoaded;
    property EntryPoint: ptruint read FEntryPoint;
    property SymbolList: TSymbollistHandler read fSymbolList;
end;

{$endif}

implementation


{$ifdef windows}
uses ProcessHandlerUnit;

procedure TModuleLoader.createSymbolListHandler;
var
  i: integer;
  a: ptruint;
  module: string;
  s: string;
  size: integer;
begin
  module:=ExtractFileName(filename);
  fSymbolList:=TSymbolListHandler.create;

  fSymbolList.AddModule(module,filename,destinationbase,modulesize,is64bit);

  module:=ChangeFileExt(module,'');
  for i:=0 to Exporttable.Count-1 do
  begin
    s:=exporttable[i];
    a:=ptruint(exporttable.Objects[i]);
    s:=copy(s,pos('-',s)+2,length(s));
    fSymbolList.AddSymbol(module, Module+'.'+s, a, 4,false, nil);
    fSymbolList.AddSymbol(module,s,a,4,true);
  end;

  fSymbolList.PID:=pid;

  symhandler.AddSymbolList(fSymbolList);
end;

procedure TModuleLoader.cleanupExportList;
begin
  if importlist<>nil then
  begin
    freeandnil(importlist);
  end;

end;

function TModuleLoader.FindKernelModuleExport(modulename: string; exportname: string): ptruint;
begin
  //check if this module is already exported
  if importlist=nil then
    importlist:=TStringHashList.Create(true);

  //todo: implement this
  result:=0;
end;

constructor TModuleLoader.create(filename: string);
var
  i,j,k: integer;
  filemap: TMemorystream;
  tempmap: tmemorystream;
  ImageNTHeader: PImageNtHeaders;
  ImageBaseRelocation: PIMAGE_BASE_RELOCATION;
  ImageSectionHeader: PImageSectionHeader;
  ImageExportDirectory: PImageExportDirectory;
  ImageImportDirectory: PImageImportDirectory;
  importmodulename: string;
  importaddress: ptruint;
  importfunctionname: string;
  importfunctionnamews: widestring;  


  numberofrva: integer;
  maxaddress: ptrUint;



  basedifference: dword;
  basedifference64: uint64;
  x: PtrUInt;
  funcaddress: uint64;
  haserror: boolean;

  processhandle: thandle;
  mi: TModuleInfo;
begin
  inherited create;


  self.filename:=filename;

  exporttable:=tstringlist.create;

  pid:=processid;

  if pid=0 then
    pid:=GetCurrentProcessId;
    
  processhandle:=dbk32functions.OP(ifthen<dword>(GetSystemType<=6,$1f0fff, process_all_access), true, pid);

  filemap:=tmemorystream.Create;
  try
    //showmessage('Loading '+filename);


    //todo: add a filesearch if no patch is given

    filemap.LoadFromFile(filename);
    
    if PImageDosHeader(filemap.Memory)^.e_magic<>IMAGE_DOS_SIGNATURE then
      raise exception.create(rsMMLNotAValidFile);


    tempmap:=tmemorystream.Create;
    try
      ImageNtHeader:=peinfo_getImageNtHeaders(filemap.Memory, filemap.Size);
      if ImageNtHeader=nil then
        raise exception.create(rsMMLNotAValidHeader);

      if ImageNTHeader^.FileHeader.Machine=$8664 then
      begin
        is64bit:=true;
        if not Is64bitOS then
          raise exception.create(rsMMLTriedLoadingA64BitModuleOnA32BitSystem);

        numberofrva:=PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.NumberOfRvaAndSizes;
      end else
      begin
        is64bit:=false;
        numberofrva:=ImageNTHeader^.OptionalHeader.NumberOfRvaAndSizes;        
      end;

      ImageSectionHeader:=PImageSectionHeader(ptrUint(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);

      maxaddress:=0;
      for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
      begin
        if maxaddress<(ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData) then
          maxaddress:=ImageSectionHeader.VirtualAddress+ImageSectionHeader.SizeOfRawData;

        inc(ImageSectionHeader);
      end;

      //maxaddress is now known
      ImageSectionHeader:=PImageSectionHeader(ptrUint(@ImageNTHeader^.OptionalHeader)+ImageNTHeader^.FileHeader.SizeOfOptionalHeader);

      tempmap.Size:=maxaddress;
      ZeroMemory(tempmap.memory, tempmap.size);

      modulesize:=tempmap.size;

      //place the sections at the appropriate locations
      for i:=0 to ImageNTHeader^.FileHeader.NumberOfSections-1 do
      begin
        CopyMemory(@pbytearray(tempmap.memory)[ImageSectionHeader.VirtualAddress], @pbytearray(filemap.memory)[ImageSectionHeader.PointerToRawData], ImageSectionHeader.SizeOfRawData);
        inc(ImageSectionHeader);
      end;

      if uppercase(ExtractFileExt(filename))='.SYS' then
      begin
        //kernel memory location
        //LoadDBK32;
        isdriver:=true;


        destinationBase:=KernelAlloc64(maxaddress);

      end
      else
      begin
        //normal memory location
        isdriver:=false;
        destinationBase:=ptrUint(VirtualAllocEx(processhandle, nil,maxaddress, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE));

      end;

      if destinationBase=0 then raise exception.create(rsMMLAllocationError);

      FEntryPoint:=destinationBase+ImageNTHeader^.OptionalHeader.AddressOfEntryPoint;


      //copy the header and get the base difference (needed for relocs)
      if is64bit then
      begin
        CopyMemory(tempmap.memory, filemap.memory, PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.SizeOfHeaders);

        basedifference:=dword(destinationBase)-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
        basedifference64:=destinationBase-PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.ImageBase;
      end
      else
      begin
        CopyMemory(tempmap.memory, filemap.memory, ImageNTHeader^.OptionalHeader.SizeOfHeaders);
        basedifference:=dword(destinationBase)-ImageNTHeader^.OptionalHeader.ImageBase;
        basedifference64:=0;
      end;

      for i:=0 to numberofrva-1 do
      begin
        if (is64bit and (PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress=0)) or
           ((not is64bit) and (ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress=0)) then
          continue; //don't look into it (virtual address=0) bug...

        case i of
          0: //exports
          begin

            if is64bit then
              ImageExportDirectory:=PImageExportDirectory(ptrUint(tempmap.memory)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
            else
              ImageExportDirectory:=PImageExportDirectory(ptrUint(tempmap.memory)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);


            if ImageExportDirectory.NumberOfFunctions=ImageExportDirectory.NumberOfNames then
            begin //consistent
              for j:=0 to ImageExportDirectory.NumberOfFunctions-1 do
              begin
                k:=pwordarray(ptrUint(tempmap.memory)+ImageExportDirectory.AddressOfNameOrdinals)[j];
                exporttable.AddObject(format('%s - %s',[inttohex(destinationBase+pdwordarray(ptrUint(tempmap.memory)+ImageExportDirectory.AddressOfFunctions)[k],1), pchar(ptrUint(tempmap.memory)+pdwordarray(ptrUint(tempmap.memory)+ImageExportDirectory.AddressOfNames)[j])]), pointer(destinationBase+pdwordarray(ptrUint(tempmap.memory)+ImageExportDirectory.AddressOfFunctions)[k]));
              end;
            end;
          end;

          1: //imports
          begin
            j:=0;
            if is64bit then
              ImageImportDirectory:=PImageImportDirectory(ptrUint(tempmap.memory)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress)
            else
              ImageImportDirectory:=PImageImportDirectory(ptrUint(tempmap.memory)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);


            while (j<45) do
            begin
              if ImageImportDirectory.name=0 then break;

              if ImageImportDirectory.ForwarderChain<>$ffffffff then
              begin
                importmodulename:=pchar(ptrUint(tempmap.memory)+ImageImportDirectory.name);
                if not isdriver then
                begin
                  if symhandler.getmodulebyname(importmodulename, mi)=false then
                    InjectDll(importmodulename);
                end;

                importmodulename:=ChangeFileExt(importmodulename, '');

                //--
                k:=0;
                if is64bit then
                begin
                  while PUINT64(ptrUint(tempmap.memory)+ImageImportDirectory.FirstThunk+8*k)^<>0 do
                  begin
                    importaddress:=ptrUint(tempmap.memory)+ImageImportDirectory.FirstThunk+8*k;
                    importfunctionname:=pchar(ptrUint(tempmap.memory)+pdword(importaddress)^+2);


                    if isDriver then
                    begin
                      importfunctionnamews:=importfunctionname;
                      funcaddress:=GetKProcAddress64(@importfunctionnamews[1]);
                      if funcaddress=0 then
                      begin
                        funcaddress:=FindKernelModuleExport(importmodulename, importfunctionname);
                        if funcaddress=0 then
                          raise exception.create(rsMMLFailedFindingAddressOf+pwidechar(@importfunctionnamews[1]));
                      end;
                    end
                    else
                    begin
                      funcaddress:=symhandler.getAddressFromName(importmodulename+'!'+importfunctionname, true, haserror);
                      if haserror then
                        raise exception.create(rsMMLFailedFindingAddressOf+importmodulename+'!'+importfunctionname);
                    end;

                    PQWORD(importaddress)^:=funcaddress;
                         
                    inc(k);
                  end;
                end
                else
                begin
                  while PDWORD(ptrUint(tempmap.memory)+ImageImportDirectory.FirstThunk+4*k)^<>0 do
                  begin
                    importaddress:=ptrUint(@pdwordarray(ptrUint(tempmap.memory)+ImageImportDirectory.FirstThunk)[k]);
                    importfunctionname:=pchar(ptrUint(tempmap.memory)+pdwordarray(ptrUint(tempmap.memory)+ImageImportDirectory.FirstThunk)[k]+2);

                    if isDriver then
                    begin
                      importfunctionnamews:=importfunctionname;
                      funcaddress:=GetKProcAddress64(@importfunctionnamews[1]);
                      if funcaddress=0 then
                        raise exception.create(rsMMLFailedFindingAddressOf+pwidechar(@importfunctionnamews[1]));
                    end
                    else
                    begin
                      funcaddress:=symhandler.getAddressFromName(importmodulename+'!'+importfunctionname, true, haserror);
                      if haserror then
                        raise exception.create(rsMMLFailedFindingAddressOf+importmodulename+'!'+importfunctionname);

                    end;

                    pdword(importaddress)^:=funcaddress;
                    inc(k);
                  end;
                end;
                //--


              end;



              inc(j);
              ImageImportDirectory:=PImageImportDirectory(ptrUint(ImageImportDirectory)+sizeof(TImageImportDirectory));
            end;


          end;

          5: //relocation table
          begin
            if is64bit then
            begin
              ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(tempmap.memory)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress);
              maxaddress:=ptrUint(tempmap.memory)+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].VirtualAddress+PImageOptionalHeader64(@ImageNTHeader^.OptionalHeader)^.DataDirectory[i].Size;
            end
            else
            begin
              ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(tempmap.memory)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress);
              maxaddress:=ptrUint(tempmap.memory)+ImageNTHeader^.OptionalHeader.DataDirectory[i].VirtualAddress+ImageNTHeader^.OptionalHeader.DataDirectory[i].Size;
            end;

            while ptrUint(ImageBaseRelocation)<maxaddress do
            begin
              if ImageBaseRelocation.SizeOfBlock=0 then break;

              for j:=0 to ((ImageBaseRelocation.SizeOfBlock-8) div 2)-1 do
              begin
                if (ImageBaseRelocation.rel[j] shr 12)=3 then            //replace the address at this address with a relocated one (dword)
                  pdword(ptrUint(tempmap.memory)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=pdword(ptrUint(tempmap.memory)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference;

                if (ImageBaseRelocation.rel[j] shr 12)=10 then            //replace the address at this address with a relocated one (dword)
                  PUINT64(ptrUint(tempmap.memory)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^:=PUINT64(ptrUint(tempmap.memory)+ImageBaseRelocation.virtualaddress+(ImageBaseRelocation.rel[j] and $fff))^+basedifference64;
              end;

              ImageBaseRelocation:=PIMAGE_BASE_RELOCATION(ptrUint(ImageBaseRelocation)+ImageBaseRelocation.SizeOfBlock);
            end;
          end;

        end;

      end;


      //ShowMessage(format('I would have written this code. From %x to %x (%d bytes)',[qword(tempmap.memory), qword(destinationbase), tempmap.size]));
      WriteProcessMemory64(processhandle, destinationbase, tempmap.memory, tempmap.size, x);
      floaded:=true;

      

      //at the end, copy the tempmap contexts to the actual allocated memory using WriteProcessMemory(64)
    finally
      tempmap.free;
    end;
  finally
    filemap.free;

    cleanupExportList;
  end;
end;

{$endif}

end.
