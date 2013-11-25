#define __MVS__
#define WINDOWS
#define ZLIB_DLL
#define ZLIB_WINAPI

#include <windows.h>
#include <zlib.h>

#include <stdio.h>

#include "cudapointervaluelist.cuh"

HANDLE f;
z_stream strm;



unsigned char in[16*1024];

    

int readBytes(void *buf, int count)
{
	int r;
	
	strm.avail_out=count;
	strm.next_out=(Bytef *)buf;

	while (strm.avail_out) //while there are still bytes to extract
	{
		if (strm.avail_in==0) //read a chunk
		{
			DWORD br=0;			
			if (ReadFile(f, in, 16*1024, &br, NULL)==FALSE)
			{
				printf("read error...\n");
			
			}
			strm.avail_in=br;
			
			if (strm.avail_in==0)
			{
				printf("read error...\n");
			}

			strm.next_in=in;
		}

		r = inflate(&strm, Z_NO_FLUSH);
		if (r==Z_STREAM_END) 
		{
			printf("Stream end reached");
			break;
		}

		if (r!=Z_OK)
		{
			printf("inflate error\n");
			return r;
		}
	}

	return r;
}

DWORD readBYTE()
{
	unsigned char r;
	if (readBytes(&r, 1)!=S_OK)
	{
	  printf("readBYTE failed");
	}
	
	return r;
}

DWORD readDWORD()
{
	DWORD r;
	if (readBytes(&r, 4)!=S_OK)	
	{
	  printf("readDWORD failed");
	}	
	return r;
}

UINT64 readQWORD()
{
	UINT64 r;
	if (readBytes(&r, 8)!=S_OK)
	{
		printf("readQWORD failed");	
	}
	return r;
}

int pscaninit()
{

	int ret;
	

	//char filename[]="E:\\ptr\\packed\\scanner1\\tutorial.ptr.scandata";
	char filename[]="E:\\ptr\\packed\\tst\\test.PTR.scandata";
	
	

    f=CreateFile(filename,FILE_READ_DATA, FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, NULL);
	


	if (f!=INVALID_HANDLE_VALUE)
	{
		int i;
		DWORD modulelistlength;
		DWORD maxlevel;
		UINT64 totalcount;
		
		
		unsigned char *cudainitdata=NULL;
		unsigned char *initdata=NULL;
		int datasize=0;
		int cudadatasize=0;
		DWORD bytes=0;
		int dp=0;
		

	/* allocate inflate state */
		strm.zalloc = Z_NULL;
		strm.zfree = Z_NULL;
		strm.opaque = Z_NULL;
		strm.avail_in = 0;
		strm.next_in = Z_NULL;
		ret = inflateInit(&strm);
		if (ret != Z_OK)
			return ret;

		modulelistlength=readDWORD();
		for (i=0; i<modulelistlength; i++)
		{
			char *modulename;
			DWORD modulenamelength=readDWORD();
			modulename=(char *)malloc(modulenamelength+1);
			readBytes(modulename, modulenamelength);
			modulename[modulenamelength]=0;
			printf("%s\n", modulename);

			free(modulename);			
		}

		maxlevel=readDWORD();
		totalcount=readQWORD();
		
		setMaxLevel(maxlevel);
		
		
		

		{
			UINT64 count=0;
			while (count<totalcount)
			{
			
				//calculate the length in bytes first...
			
				
				UINT64 pvalue=readQWORD();
				DWORD pointercount=readDWORD();
				TPointerData *pd=(TPointerData*)malloc(sizeof(TPointerData)*pointercount);
				
				if (pvalue==0x201be8)
				{
				  printf("it's in the list\n");
				}
				
				bytes+=12;
							
				
				
				
				for (i=0; i<pointercount; i++)
				{
					pd[i].address=readQWORD();				
				
					bytes+=9;
					
					if (readBYTE()==1)
					{
						pd[i].staticdata=(TStaticData*)malloc(sizeof(TStaticData));
						pd[i].staticdata->moduleindex=readDWORD();
						pd[i].staticdata->offset=readDWORD();
						
						bytes+=8;
					}
					else
					{
						pd[i].staticdata=NULL;
					}
				}
				
				//convert it back into a single bytelist				
				if (datasize<bytes)
				{
					datasize=bytes+128;
					if (initdata)
					{
						initdata=(unsigned char *)realloc(initdata, datasize);
					}
					else									
						initdata=(unsigned char *)malloc(datasize);
					
					
				}				
				
				*(UINT64 *)&initdata[dp]=pvalue;
				dp+=8;
				
				*(DWORD *)&initdata[dp]=pointercount;
				dp+=4;
				
				for (i=0; i<pointercount; i++)
				{				
					*(UINT64 *)&initdata[dp]=pd[i].address;
					dp+=8;
					
					*(unsigned char *)&initdata[dp]=pd[i].staticdata?1:0;
					dp+=1;
					
					if (pd[i].staticdata)
					{
						*(DWORD *)&initdata[dp]=pd[i].staticdata->moduleindex;
						dp+=4;	
						
						*(DWORD *)&initdata[dp]=pd[i].staticdata->offset;
						dp+=4;						
					}									
				}	
				
				if (bytes!=dp)			
				{
					printf("Error during initialization\n");
					exit(1);
				
				}
				
				if (bytes>4096) //enough data gathered
				{				
					//pass the bytestring to cuda
					
					cudaDeviceSynchronize(); //wait till the previous findoraddpointervalue operation is done
					
					cudaError_t error=cudaGetLastError();
					
					if (error!=cudaSuccess)
					{
						printf("Error\n");
						printf("CUDA error: %s\n", cudaGetErrorString(error));  
					}

	  				
					if (cudadatasize<bytes)
					{
						if (cudainitdata)
						{
							cudaFree(cudainitdata);
							cudainitdata=NULL;
						}
							
						cudadatasize=bytes+128;
						cudaMalloc(&cudainitdata, cudadatasize);				
					}
									
					//send the data to the gpu
					
				//	printf("%d->%d(%d): %x - bytes=%d\n", (DWORD)pointercount, (DWORD)count, (DWORD)totalcount, (DWORD)pvalue, bytes);
				//	printf("CUDA error: %s\n", cudaGetErrorString(cudaGetLastError()));  
					
					 
					cudaError_t r=cudaMemcpy(cudainitdata, initdata, bytes, cudaMemcpyHostToDevice);
					findoraddpointervalue<<<1,1>>>(cudainitdata, bytes);
					bytes=0;
					dp=0;									  					

				}
				
				//cleanup the temporary pointerdata 
				for (i=0; i<pointercount; i++)
				{
				  if (pd[i].staticdata)
					free(pd[i].staticdata);
				}
				free(pd);
								
			  				
	
				count+=pointercount;
				
			}
		}
	

		if (bytes)
		{				
		    //send the last bit
			cudaError_t r=cudaMemcpy(cudainitdata, initdata, bytes, cudaMemcpyHostToDevice);
			findoraddpointervalue<<<1,1>>>(cudainitdata, bytes);
			bytes=0;
		}
		
		
		
		
					
		if (initdata)
		  free(initdata);
		  		
		cudaDeviceSynchronize();
		if (cudainitdata)
		  cudaFree(cudainitdata);
		  
		cudaDeviceSynchronize();  
		//build the linked list
		generateLinkedList<<<1,1>>>();
		cudaDeviceSynchronize(); 
		
			
					  		 
	}

	return 0;
}

