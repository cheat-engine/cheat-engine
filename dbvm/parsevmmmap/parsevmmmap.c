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
	FILE *fpVMMMAP;
	FILE *fpVMMBIN;
	char temp[256000];

	fpVMMMAP=fopen("vmm.map","r"); 
  	if (fpVMMMAP)
	{
		unsigned int startofbss;
		unsigned int sizeofbss;
		unsigned int memorylist=0;
		temp[0]=0;

		//find the line start starts with ".bss  "
		while (strcmp(temp,".bss")!=0)
			fscanf(fpVMMMAP,"%s",temp);

		fscanf(fpVMMMAP,"%x",&startofbss);
		fscanf(fpVMMMAP,"%x",&sizeofbss);

		temp[0]=0;
		while (strcmp(temp,"memorylist")!=0)
		{
			memorylist=strtol(temp,NULL,16); 
			fscanf(fpVMMMAP,"%s",(char *)&temp);
		}
		fclose(fpVMMMAP);



	
		if ((startofbss+sizeofbss) % 0x1000)
		{
		    //add a little bit more for a nice page boundary
		    int difference=(startofbss+sizeofbss+0x1000) % 0x1000;
		    sizeofbss+=0x1000-difference;
		}

                printf("startofbss=%x\n",startofbss);
                printf("sizeofbss=%x\n",sizeofbss);
                printf("memorylist=%x\n",memorylist);

	  	fpVMMBIN=fopen("vmm.bin","r+");
		if (fpVMMBIN)
		{
			unsigned char *buf=calloc(1,sizeofbss);
			unsigned long long startoffreemem=startofbss+sizeofbss;
			int count;
			startoffreemem+=4096-(startoffreemem % 4096);



			printf("startoffreemem=%x\n",(int)startoffreemem);
			*(unsigned long long *)(&buf[memorylist-startofbss])=startoffreemem+0x7f00;

			//seek to the end
			printf("Seeking to %x\n",startofbss-0x00400000);
			if (fseek(fpVMMBIN,startofbss-0x00400000,SEEK_SET)!=-1)
			{
				if (count=fwrite(buf,sizeofbss,1,fpVMMBIN)<1)
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
				
			fclose(fpVMMBIN);
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
		printf("Failed opening vmm.map\n");
		errorcode=1;
	}

	return errorcode;
}
