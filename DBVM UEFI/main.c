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
#include "mpService.h"

EFI_SYSTEM_TABLE *st;
EFI_BOOT_SERVICES *bs;
EFI_RUNTIME_SERVICES *rs;
EFI_LOAD_FILE_INTERFACE *file;

EFI_GUID fileprotocol=LOAD_FILE_PROTOCOL;

UINTN cpucount=0;


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

EFIAPI VOID FunctionX (IN VOID *Buffer)
{
  UINT64 t1,t2,t3, a;
  t1=getTSC();
  t2=readMSR(0x10);
  t3=getTSC();
  a=readMSR(0x3b);
  Print(L"AP CPU %d:\n      %ld - %ld - %ld\n      Adjust:%ld\n", (int)Buffer, t1,t2,t3, a);

  writeMSR(0x3b,-getTSC());

  t1=getTSC();
  t2=readMSR(0x10);
  t3=getTSC();
  a=readMSR(0x3b);

  Print(L"AP CPU %d:\n      %ld - %ld - %ld\n      Adjust:%ld\n", (int)Buffer, t1,t2,t3, a);
}


EFIAPI VOID LaunchDBVMAP (IN VOID *Buffer)
{
  writeMSR(0x3b,0);
  Print(L"AP CPU %d entering DBVM mode\n", (int)Buffer);

  Print(L"CR0 before = 0x%lx\n", getCR0());
  LaunchDBVM();
  Print(L"CR0 after = 0x%lx\n", getCR0());

  Print(L"AP CPU %d is alive\n", (int)Buffer);
}

void enableSerial(void);
void sendchar32(char x);


unsigned char inportb(unsigned int port)
{
   unsigned char ret;
   asm volatile ("inb %%dx,%%al":"=a" (ret):"d" (port));
   return ret & 0xff;
}

void outportb(unsigned int port,unsigned char value)
{
   asm volatile ("outb %%al,%%dx": :"d" (port), "a" (value));
}


extern int SerialPort;

EFI_STATUS
//EFIAPI //Not with GNU_EFI_USE_MS_ABI enabled we are
efi_main (EFI_HANDLE ImageHandle, EFI_SYSTEM_TABLE *SystemTable)
{

  EFI_PHYSICAL_ADDRESS dbvmimage;
  CHAR16 something[200];

  //UINT64 c=testfunction();
  EFI_STATUS s;
  int i;

  InitializeLib(ImageHandle, SystemTable);
  initFunctions(ImageHandle, SystemTable);

  UINT64 tx[5];





  Print(L"efi_main at %lx\n",(UINT64)efi_main);
  FunctionX(NULL);
/*
  Print(L"Testing 2:\n");

  int counter=0;

  for (i=7; i<0x10000; i+=8)
  {
	  outportb(i,0xce);
	  if (inportb(i)==0xce)
	  {
		  counter++;
		  Print(L"%d: Correct at %x: ",counter, i);

		  if (counter>0)
		  {

			  //SerialPort=i-7;

			 // Print(L"a");
			  //enableSerial();

			  //int j;
			  //Print(L"b");
			  //for (j=0; j<10; j++)
			  //{
			//	  Print(L"c");
			//	  sendchar32('a'+counter);
			//	  Print(L"d");
			 // }
		  }

		  Print(L"\n");
	  }

  }

  Print(L"Done testing");

  outportb(0x80,0xce);


  SerialPort=0xff10;
  enableSerial();
  while (1)
	  sendchar32('x');

  outportb(0x80,0xcd);
*/

  tx[0]=0;
  tx[1]=0;
  tx[2]=0;
  tx[3]=0;
  tx[4]=0;

  timeCheck(tx);

  {
    int i;

    for (i=0; i<5; i++)
    {
      int delta;
      if (i>0)
      {
        delta=tx[i]-tx[i-1];
        Print(L"tx[%d]=%ld  (delta=%d)\n", i, tx[i], delta);
      }
      else
        Print(L"tx[%d]=%ld\n", i, tx[i]);
    }
    int avg=(tx[4]-tx[0]) / 5;
    Print(L"Avg time=%d\n", avg);
  }



  Input(L"Type something : ", something, 200);


  EFI_MP_SERVICES_PROTOCOL *MpProto=NULL;
  EFI_GUID z=EFI_MP_SERVICES_PROTOCOL_GUID;

  s=LocateProtocol(&z, NULL, (void *)&MpProto);

  if(EFI_ERROR(s)){Print(L"Unable to locate the MpService protocol:%r\n",s);}

  Print(L"MpProto=%lx\n", (UINT64)MpProto);

  if (MpProto)
  {
    UINTN NumEnabled;

    //s=uefi_call_wrapper(MpProto->GetNumberOfProcessors,3,MpProto,&NumProc,&NumEnabled);
    s=MpProto->GetNumberOfProcessors(MpProto,&cpucount,&NumEnabled);
    if(EFI_ERROR(s))
    {
      Print(L"Unable to get the number of processors:%r\n",s);
    }
    else
    {
      Print(L"%d Processors of which %d are enabled\n", cpucount, NumEnabled);
      /*
      int i;
      for (i=1; i<cpucount; i++)
      {
        s=MpProto->StartupThisAP(MpProto, FunctionX,i,NULL,0,(void*)(uintptr_t)i,NULL);
        if(EFI_ERROR(s))
        {
          Print(L"%d Failed to launch AP:%r\n",i,s);
        }
      }*/


    }
  }

  Input(L"Type something : ", something, 200);





  //s=SystemTable->BootServices->AllocatePages(AllocateAnyPages, EfiLoaderData, 4, &pa);
  s=AllocatePages(AllocateAnyPages,EfiRuntimeServicesCode,1024,&dbvmimage); //4MB

  if (s!=EFI_SUCCESS)
  {
    Print(L"Failed allocating memory for the VMM\n");
    return s;
  }

  Print(L"dbvm image space allocated at s=%x\n",s);

  ZeroMem((void *)dbvmimage, 1024*4096);


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

    size=1024*4096;

    s=ReadSimpleReadFile(srh,startsector*512,&size,(void *)dbvmimage);
    Print(L"ReadSimpleReadFile: s=%d size=%d\n",s,size);

    CloseSimpleReadFile(srh);

    if (size % 4096)
      size=(size+4096)-(size % 4096);

    InitializeDBVM(dbvmimage, size);
  }

  FreePool(vmdisk);


  UINT64 base=(UINT64)dbvmimage; //filepath;
  for (i=0; i<128; i++)
  {
    Print(L"%x ",*(unsigned char *)(base+i));
    //*(char *)i=0xce;
  }

  Print(L"\n");

  //offload current state into DBVM



  //OpenSimpleReadFile(1,NULL,0 )


  #define DB_SETUP_GUID { 0xEC87D643, 0xEBA4, 0x4BB5, {0xa1, 0xe5, 0x3f, 0x3e, 0x36, 0xb2, 0x0d, 0xa9} }


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

  //get the memory map

 // asm volatile (".byte 0xf1");




  Input(L"Type something : ", something, 200);
  Print(L"\n");

  if (StrnCmp(something,L"Q",2)!=0)
  {
    Print(L"launching DBVM\n");

    writeMSR(0x3b,0);

    setCR0(getCR0() | (1 << 5));
    Print(L"WEEE\n");
    Print(L"CR0 before = 0x%lx\n", getCR0());
    LaunchDBVM();
    Print(L"CR0 after = 0x%lx\n", getCR0());

    Print(L"Main DBVM CPU loaded. Loading AP cpu\'s:");

    Input(L"Type something : ", something, 200);


    int i;

    //cpucount=1; //test while I fix something

    for (i=1; i<cpucount; i++)
    {
      s=MpProto->StartupThisAP(MpProto, LaunchDBVMAP,i,NULL,0,(void*)(uintptr_t)i,NULL);
      if(EFI_ERROR(s))
      {
        Print(L"Failed to launch CPU %d (%r)\n",i,s);
      }
    }

  }

  cleanupMemory();

  Input(L"Type something : ", something, 200);

  Print(L"Something is %S", something);


  SystemTable->BootServices->Exit(ImageHandle, 1,0,NULL);
  return EFI_SUCCESS;
}
