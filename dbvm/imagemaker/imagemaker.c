/* imagemaker.c: combines the binary files into one diskimage */


#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>


int main(void)
{
  FILE *fpBootloader;
  FILE *fpVmloader;
  FILE *fpVmm;
  FILE *fpDisk;
  FILE *fpFullDisk;
  
  
  unsigned char *bootloader;
  unsigned char *vmloader;
  unsigned char *vmm;
  unsigned char *vmdisk;
  
  unsigned short int VMMlocation;

  unsigned char sector[512];
  
  int bootloader_size;
  int vmloader_size;
  int vmm_size;
  int vmdisk_size;
  int *sectornumber=(int *)sector;
  
  long fpos;
  
  struct stat tempstat;
  
  memset(sector,0,512);
  printf("Image being created\n");
  
  printf("Opening bin files\n");
  printf("Opening and reading bootloader.bin...");  
  if (stat("bootloader.bin",&tempstat)==-1)
  {
    printf("File can't be found\n");
    return 1;
  }
  bootloader_size=tempstat.st_size;
  bootloader=malloc(bootloader_size);  
  fpBootloader=fopen("bootloader.bin","r");    
  fread(bootloader,tempstat.st_size,1,fpBootloader);
  printf("done\n");
  
    
  printf("Opening and reading vmloader.bin...");  
  if (stat("vmloader.bin",&tempstat))
  {
    printf("File can't be found\n");
    return 1;
  }  
  vmloader_size=tempstat.st_size;
  vmloader=malloc(vmloader_size);  
  fpVmloader=fopen("vmloader.bin","r");    
  fread(vmloader,vmloader_size,1,fpVmloader);
  printf("done\n");  
  
  
  printf("Opening and reading vmm.bin...");  
  if (stat("vmm.bin",&tempstat))
  {
    printf("File can't be found\n");
    return 1;
  }  
  vmm_size=tempstat.st_size;
  vmm=malloc(vmm_size);  
  fpVmm=fopen("vmm.bin","r");    
  fread(vmm,tempstat.st_size,1,fpVmm);
  printf("done\n");
  
  printf("Creating vmdisk.img...\n");
  fpDisk=fopen("vmdisk.img","w");
  if (fpDisk==NULL)
  {
    printf("Failed creating file\n");
    return 1;    
  }
  
  printf("Writing VMMlocation in bootsector\n");
  VMMlocation=2+(1+vmloader_size/512);  
  *(unsigned short int *)&bootloader[0x8]=VMMlocation;
  
  fwrite(bootloader,bootloader_size,1,fpDisk);
  fwrite(vmloader,vmloader_size,1,fpDisk);
  
  //now seek to the VMM startsector
  fseek(fpDisk, VMMlocation*512, SEEK_SET); //go to next sector pos  
  fwrite(vmm,vmm_size,1,fpDisk);
  
  //fill till dividable by 512
  bzero(sector,512);
  fwrite(sector,512-((VMMlocation*512+vmm_size) % 512),1,fpDisk);
  
  
  
  fclose(fpDisk);
  fclose(fpVmm);
  fclose(fpVmloader);
  fclose(fpBootloader);    

  //full disk image (cdrom capable)
  printf("Reopening and reading vmdisk.img...");  
  if (stat("vmdisk.img",&tempstat))
  {
    printf("File can't be found\n");
    return 1;
  }  
  vmdisk_size=tempstat.st_size;
  vmdisk=malloc(vmdisk_size);  
    
  fpDisk=fopen("vmdisk.img","r");
  fpFullDisk=fopen("vmdisk144.img","w");
  
  fread(vmdisk,vmdisk_size,1,fpDisk);
  fwrite(vmdisk,vmdisk_size,1,fpFullDisk);  
  
  fseek(fpFullDisk, 1474559, SEEK_SET);
  fwrite(sector,1,1,fpFullDisk);
  
  printf("done\n");
  
  printf("Completed\n");
  return 0;  
}
