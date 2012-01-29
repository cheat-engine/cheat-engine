/* this program will parse the vmm file and generate a vmminfo.dat file that will
 * contain the data needed for the memorymanager inside the vmm 
 * It can also be used to grow the vmm.bin to include 0's */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

int main(void)
{
	int errorcode=0;
	FILE *fpVMLOADERMAP;
	FILE *fpVMLOADERBIN;
	char temp[256000];

	fpVMLOADERMAP=fopen("vmloader.map","r"); 
	if (fpVMLOADERMAP)
	{
		unsigned int startofbss;
		unsigned int sizeofbss;
		unsigned int memorylist=0;
		temp[0]=0;

		//find the line start starts with ".bss  "
		while (strcmp(temp,".bss")!=0)
			fscanf(fpVMLOADERMAP,"%s",temp);

		fscanf(fpVMLOADERMAP,"%x",&startofbss);
		fscanf(fpVMLOADERMAP,"%x",&sizeofbss);
		fclose(fpVMLOADERMAP);


		printf("startofbss=%x\n",startofbss);
		printf("sizeofbss=%x\n",sizeofbss);
		
	  	fpVMLOADERBIN=fopen("vmloader.bin","r+");
		if (fpVMLOADERBIN)
		{
			unsigned char *buf=calloc(1,sizeofbss);
			unsigned int startoffreemem=startofbss+sizeofbss;
			int count;
			startoffreemem+=4096-(startoffreemem % 4096);


			printf("startoffreemem=%x\n",startoffreemem);

			//seek to the end
			printf("Seeking to %x\n",startofbss-0x30000);
			if (fseek(fpVMLOADERBIN,startofbss-0x30000,SEEK_SET)!=-1)
			{
				if (count=fwrite(buf,sizeofbss,1,fpVMLOADERBIN)<1)
				{
					printf("fwrite failed. Count=%d\n",count);				
					errorcode=1;
				}
			}
			else
			{
				printf("Seek failed\n");
				errorcode=1;
			}
				
			fclose(fpVMLOADERBIN);
			free(buf);
		}
		else
		{
			printf("Failed opening vmm.bin\n");
			errorcode=1;
		}

		
	}  
	else
	{
		errorcode=1;
		printf("Failed opening vmm.map\n");
	}
	
	return errorcode;
}
