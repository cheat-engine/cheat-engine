/*
 * main.c
 *
 *  Created on: Nov 10, 2017
 *      Author: eric
 */


#include <efi.h>
#include <efilib.h>
#include "helpers.h"
#include "dbvmoffload.h"

EFI_SYSTEM_TABLE *st;
EFI_BOOT_SERVICES *bs;
EFI_RUNTIME_SERVICES *rs;
EFI_LOAD_FILE_INTERFACE *file;

EFI_GUID fileprotocol=LOAD_FILE_PROTOCOL;


EFI_STATUS OpenProtocol(
    IN EFI_HANDLE               Handle,
    IN EFI_GUID                 *Protocol,
    OUT VOID                    **Interface OPTIONAL,
    IN EFI_HANDLE               AgentHandle,
    IN EFI_HANDLE               ControllerHandle,
    IN UINT32                   Attributes
    )
{
  return uefi_call_wrapper(bs->OpenProtocol, 6, Handle, Protocol, Interface, AgentHandle, ControllerHandle, Attributes);
}


EFI_STATUS LocateProtocol(IN EFI_GUID                 *Protocol,
    IN VOID                     *Registration OPTIONAL,
    OUT VOID                    **Interface)
{
  return uefi_call_wrapper(bs->LocateProtocol, 3, Protocol, Registration, Interface);
}

EFI_STATUS GetVariable(
    IN CHAR16                       *VariableName,
    IN EFI_GUID                     *VendorGuid,
    OUT UINT32                      *Attributes OPTIONAL,
    IN OUT UINTN                    *DataSize,
    OUT VOID                        *Data
    )
{
  return uefi_call_wrapper(rs->GetVariable, 5, VariableName, VendorGuid, Attributes, DataSize, Data);
}

EFI_STATUS SetVariable(
    IN CHAR16                       *VariableName,
    IN EFI_GUID                     *VendorGuid,
    IN UINT32                       Attributes,
    IN UINTN                        DataSize,
    IN VOID                         *Data
    )
{
  return uefi_call_wrapper(rs->SetVariable, 5, VariableName, VendorGuid, Attributes, DataSize, Data);
}

void initFunctions(EFI_HANDLE Handle, EFI_SYSTEM_TABLE *SystemTable)
{
  st=SystemTable;
  bs=st->BootServices;
  rs=st->RuntimeServices;

}

EFI_STATUS GetNextMonotonicCount(OUT UINT64 *Count)
{
  return uefi_call_wrapper(bs->GetNextMonotonicCount, 1, Count);
}

EFI_STATUS AllocatePages(
    IN EFI_ALLOCATE_TYPE            Type,
    IN EFI_MEMORY_TYPE              MemoryType,
    IN UINTN                        NoPages,
    OUT EFI_PHYSICAL_ADDRESS        *Memory
    )
{
  return uefi_call_wrapper(bs->AllocatePages, 4, Type, MemoryType, NoPages, Memory);
}

VOID *AllocatePersistentMemory(int size)
{
  EFI_PHYSICAL_ADDRESS pa=0;
  int pages=size/4096;

  if (size % 4096)
    pages++;

  if (AllocatePages(AllocateAnyPages, EfiRuntimeServicesData, pages, &pa)==EFI_SUCCESS)
    return (VOID *)pa;
  else
    return NULL;
}

void printDevicePath(EFI_DEVICE_PATH *path)
{
  Print(L"type=%d\nsubtype=%d\nlength[0]=%d\nlength[2]=%d\n",path->Type,path->SubType, path->Length[0], path->Length[1]);
  if ((path->Type==4) && (path->SubType==4))
  {
    CHAR16 *s;
    FILEPATH_DEVICE_PATH *fp=(FILEPATH_DEVICE_PATH*)path;
    s=&fp->PathName[0];
    Print(L"Path=");
    Print(s);
    Print(L"\n");
  }
}

/*
inline uint64_t rdmsr(uint32_t msr_id)
{
    uint64_t msr_value;
    asm volatile ( "rdmsr" : "=A" (msr_value) : "c" (msr_id) );
    return msr_value;
}*/

EFI_STATUS
EFIAPI
efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable)
{

  EFI_PHYSICAL_ADDRESS dbvmimage;
  CHAR16 something[200];

  //UINT64 c=testfunction();
  EFI_STATUS s;

  InitializeLib(ImageHandle, SystemTable);
  initFunctions(ImageHandle, SystemTable);


  Print(L"efi_main at %lx\n",(UINT64)efi_main);

  //s=SystemTable->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, 4, &pa);
  s=AllocatePages(AllocateAnyPages,EfiRuntimeServicesCode,16384,&dbvmimage); //64MB

  if (s!=EFI_SUCCESS)
  {
    Print(L"Failed allocating memory for the VMM\n");
    return s;
  }

  Print(L"dbvm image space allocated at s=%x\n",s);

  ZeroMem(dbvmimage, 16384*4096);


/*
  pa2=0xfffff;
  s=AllocatePages(AllocateMaxAddress,EfiRuntimeServicesCode,4,&pa2);
  Print(L"AllocatePages: s=%x\n",s);
  Print(L"after pa2=%lx\n",pa2);

*/
  //EFI_INVALID_PARAMETER
 // s=SystemTable->BootServices->Exit(ImageHandle,1,0, NULL);
  //Print(L"Exit: s=%x\n",s);

  CHAR16 filepath[128];
  FILEPATH_DEVICE_PATH *vmdisk;

  EFI_HANDLE dh;
  SIMPLE_READ_FILE srh;




  SetMem(filepath, 128*2,0xce);


  EFI_GUID lip=LOADED_IMAGE_PROTOCOL;
  EFI_LOADED_IMAGE *li=NULL;

  s=OpenProtocol(ImageHandle, &lip, (void **)&li, ImageHandle, NULL, EFI_OPEN_PROTOCOL_BY_HANDLE_PROTOCOL);
  Print(L"OpenProtocol: s=%d\n",s);

  vmdisk=(FILEPATH_DEVICE_PATH *)FileDevicePath(li->DeviceHandle,L"vmdisk.img");

  EFI_DEVICE_PATH *d=(EFI_DEVICE_PATH *)vmdisk;


  Print(L"calling OpenSimpleReadFile\n",s);



  //bs->LocateDevicePath

  s=OpenSimpleReadFile(FALSE,NULL, 0, &d, &dh, &srh);
  Print(L"OpenSimpleReadFile: s=%d\n",s);
  if (s==EFI_SUCCESS)
  {
    UINTN size=2; //16384*4096;
    UINT16 startsector;

    s=ReadSimpleReadFile(srh, 8, &size, &startsector);
    if (s!=EFI_SUCCESS)
    {
      Print(L"Failure reading vmdisk.img offset 8\n");
      return s;
    }
    Print(L"startsector=%d\n",startsector);

    size=16384*4096;

    s=ReadSimpleReadFile(srh,startsector*512,&size,(void *)dbvmimage);
    Print(L"ReadSimpleReadFile: s=%d size=%d\n",s,size);

    CloseSimpleReadFile(srh);

    if (size % 4096)
      size=(size+4096)-(size % 4096);

    InitializeDBVM(dbvmimage, size);
  }

  FreePool(vmdisk);


  UINT64 base=(UINT64)dbvmimage; //filepath;
  int i;
  for (i=0; i<128; i++)
  {
    Print(L"%x ",*(unsigned char *)(base+i));
    //*(char *)i=0xce;
  }

  Print(L"\n");

  //offload current state into DBVM



  //OpenSimpleReadFile(1,NULL,0 )


  #define DB_SETUP_GUID { 0xEC87D643, 0xEBA4, 0x4BB5, {0xa1, 0xe5, 0x3f, 0x3e, 0x36, 0xb2, 0x0d, 0xa9} }
//EC87D643-EBA4-4BB5-A1E5-3F3E36B20DA9
 // EFI_GUID dbsetupguid=DB_SETUP_GUID;
 // UINTN Size=0;
  //UINT8 *Setup;
 // UINT32 attrib=0;

  //s=GetVariable(L"Setup",&dbsetupguid, NULL, &Size, NULL);
  //Print(L"GetVariable1 s=%x  -  (Size=%d)\n",s, Size);


  //Setup=AllocatePool(Size+64);
 // Setup[905]=123;

  //s=GetVariable(L"Setup",&dbsetupguid, &attrib, &Size, Setup);
  //Print(L"GetVariable2 s=%x  -  (Size=%d attrib=%x)\n",s, Size, attrib);

  //Print(L"Debug interface (0x5da)=%x\n",(int)Setup[0x5da]);
  //Print(L"Direct Connect Interface (0x5dc)=%x\n",(int)Setup[0x5dc]);
  //Print(L"Debug Interface Lock   (0x5db)=%x\n",(int)Setup[0x5dc]);

  //Print(L"DCI Enable (0x905)=%x\n",(int)Setup[0x905]);


  //FreePool(Setup);

 // Size=0;
  //s=GetVariable(L"SetupCpuFeatures",&dbsetupguid, NULL, &Size, NULL);
 // Print(L"GetVariable2 s=%x\n (Size=%d)\n",s, Size);

  //Size=0x27;
  //Setup=AllocatePool(Size);
  //SetMem(Setup,Size,1);
  //s=SetVariable(L"SetupCpuFeatures",&dbsetupguid, attrib, Size, Setup);
  //Print(L"SetVariable s=%x\n (Size=%d attrib=%x)\n",s, Size, attrib);

  Print(L"Reading msr 0x10:\n");
  s=readMSR(0x10);
  Print(L"readMSR s=%x\n",s);

  s=readMSR(0x10);
  Print(L"readMSR s=%x\n",s);

  s=readMSR(0x10);
  Print(L"readMSR s=%x\n",s);

  s=readMSR(0xc80);
  Print(L"readMSR s=%x\n",s);

 // s=rdmsr(0xc80);
// Print(L"readMSR s=%x\n",s);


  {
    EFI_LOADED_IMAGE *loaded_image = NULL;

    s = uefi_call_wrapper(SystemTable->BootServices->HandleProtocol, 3, ImageHandle, &LoadedImageProtocol, (void **)&loaded_image);

    Print(L"Image base: 0x%lx\n", loaded_image->ImageBase);
  }


  Input(L"Type something : ", something, 200);
  Print(L"\n");

  if (StrnCmp(something,L"1",2)==0)
  {
    Print(L"launching DBVM\n");
    LaunchDBVM();
  }



  Print(L"Something is %S", something);


  SystemTable->BootServices->Exit(ImageHandle, 1,0,NULL);
  return EFI_SUCCESS;
}
