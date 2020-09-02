/*
sigcheck is responsible for checking if the controlling process is Cheat Engine or another program signed by Dark Byte and not someone elses/modified one
This is only in case of a signed release build
*/

#include <ntifs.h>
#include <windef.h>
#include <bcrypt.h>
#include <Ntstrsafe.h>
#include <ntimage.h>


#include "sigcheck.h"


unsigned char publicKey[]={0x45, 0x43, 0x53, 0x35, 0x42, 0x00, 0x00, 0x00, 0x00, 0x3A, 0xBA, 0x72, 0xCF, 0xA7, 0x79, 0xFA, 0x92, 0x96, 0x15, 0x8E, 0x69, 0x35, 0x19, 0x09, 0x99, 0x3C, 0x97, 0xE8, 0x18, 0x0B, 0xC6, 0x2C, 0x8B, 0x24, 0x5A, 0xD8, 0x1C, 0x86, 0x83, 0x89, 0xE7, 0xA4, 0xA9, 0x47, 0x11, 0x7E, 0x07, 0x74, 0x69, 0x74, 0x33, 0x0B, 0x1A, 0xB8, 0x63, 0x11, 0x51, 0xEA, 0x00, 0xD6, 0x26, 0xE7, 0x7C, 0x6D, 0x77, 0xA5, 0x0E, 0x9F, 0x37, 0x87, 0x7B, 0x79, 0x2F, 0xEE, 0x00, 0x65, 0x7A, 0xBF, 0x44, 0x79, 0xD1, 0x7E, 0x47, 0xBC, 0xF9, 0x6F, 0x31, 0x81, 0x85, 0x70, 0x78, 0x5D, 0xED, 0xA5, 0xC6, 0x15, 0x0F, 0x2C, 0x0A, 0x27, 0x3B, 0x3E, 0x36, 0xEB, 0x53, 0x3E, 0x3E, 0x75, 0xC1, 0xA3, 0x0A, 0xC0, 0xC1, 0x53, 0x3A, 0x77, 0xFB, 0x84, 0x88, 0x35, 0xE8, 0x86, 0xF0, 0xA2, 0x52, 0x86, 0x5D, 0x12, 0x2D, 0x03, 0x88, 0x00, 0x36, 0x2B, 0x8D, 0x21, 0x13, 0x99, 0x7F, 0x62};

NTSYSAPI NTSTATUS NTAPI ZwQueryInformationProcess(IN HANDLE ProcessHandle, IN PROCESSINFOCLASS ProcessInformationClass, OUT PVOID ProcessInformation, IN ULONG ProcessInformationLength, OUT PULONG ReturnLength OPTIONAL);
NTSYSAPI NTSTATUS NTAPI ZwQueryInformationThread(IN HANDLE ThreadHandle, IN THREADINFOCLASS ThreadInformationClass, OUT PVOID ThreadInformation, IN ULONG ThreadInformationLength, OUT PULONG ReturnLength OPTIONAL);


NTSTATUS LoadFile(PUNICODE_STRING filename, PVOID *buffer, DWORD *size)
/*
Loads the specified file into paged memory
pre: filename must be valid
post: 
  buffer will get a pointer to the loaded file
  Size will get the size of the file

Caller is responsible for calling ExFreePool on the buffer
*/
{
	HANDLE hFile;
	OBJECT_ATTRIBUTES oa;
	FILE_STANDARD_INFORMATION fsi;
	IO_STATUS_BLOCK statusblock;
	NTSTATUS s=STATUS_UNSUCCESSFUL;	

	InitializeObjectAttributes(&oa, filename, 0, NULL, NULL);
	s=ZwCreateFile(&hFile,SYNCHRONIZE|STANDARD_RIGHTS_READ , &oa, &statusblock, NULL, FILE_SYNCHRONOUS_IO_NONALERT| FILE_ATTRIBUTE_NORMAL, 0, FILE_OPEN, 0, NULL, 0);

	if (s==STATUS_SUCCESS)
	{	
		s=ZwQueryInformationFile(hFile, &statusblock, &fsi, sizeof(fsi),  FileStandardInformation);

		if (fsi.EndOfFile.HighPart==0)
		{
			
			*size=fsi.EndOfFile.LowPart;
			*buffer=ExAllocatePool(PagedPool, fsi.EndOfFile.LowPart);	 //caller MUST free this
			

			if (*buffer)
			{
				LARGE_INTEGER ByteOffset;
				ByteOffset.QuadPart=0;
				s=ZwReadFile(hFile, NULL, NULL, NULL, &statusblock, *buffer, fsi.EndOfFile.LowPart, &ByteOffset, NULL);

				if (s==STATUS_PENDING)
					s=ZwWaitForSingleObject(hFile, FALSE, NULL);

				if (s==STATUS_SUCCESS)
					s=statusblock.Status;

				if (s!=STATUS_SUCCESS) //read error, free the buffer		
					ExFreePool(*buffer); 
			}
			
		}

		ZwClose(hFile);
	}

	return s;
}

NTSTATUS CheckSignature(PVOID buffer, DWORD buffersize, PVOID sig, DWORD sigsize)
/*
Calculates a hash from the buffer and then checks the signature
*/
{
	NTSTATUS s=STATUS_UNSUCCESSFUL;
	BCRYPT_ALG_HANDLE hashAlgoritm;

	s=BCryptOpenAlgorithmProvider(&hashAlgoritm, BCRYPT_SHA512_ALGORITHM, NULL, 0); 
	if (s==STATUS_SUCCESS)
	{
		DbgPrint("Created a hash algoritm\n");
		if (s==STATUS_SUCCESS)
		{
			DWORD objectlength;
			ULONG size;
			s=BCryptGetProperty(hashAlgoritm, BCRYPT_OBJECT_LENGTH, (PUCHAR)&objectlength, sizeof(DWORD), &size, 0);  
			if (s==STATUS_SUCCESS)
			{
				BCRYPT_HASH_HANDLE hHash;
				PUCHAR pbHashObject=ExAllocatePool(PagedPool, objectlength);

				if (pbHashObject)
				{
					s=BCryptCreateHash(hashAlgoritm, &hHash, pbHashObject, objectlength, (PUCHAR)NULL, 0, 0); 
					if (s==STATUS_SUCCESS)
					{
						s=BCryptHashData(hHash, buffer, buffersize, 0);  
						if (s==STATUS_SUCCESS)
						{
							DWORD hashlength;
							s=BCryptGetProperty(hashAlgoritm, BCRYPT_HASH_LENGTH, (PUCHAR)&hashlength, sizeof(DWORD), &size, 0); 
							if (s==STATUS_SUCCESS)
							{
								PUCHAR pbHashBuffer=ExAllocatePool(PagedPool, hashlength);
								if (pbHashBuffer)
								{
									s=BCryptFinishHash(hHash, pbHashBuffer, hashlength, 0);
									if (s==STATUS_SUCCESS)
									{
										BCRYPT_ALG_HANDLE SignAlgoritm;															
										s=BCryptOpenAlgorithmProvider(&SignAlgoritm, BCRYPT_ECDSA_P521_ALGORITHM, NULL, 0);   

										if (s==STATUS_SUCCESS)
										{
											BCRYPT_KEY_HANDLE hKey;
											s=BCryptImportKeyPair(SignAlgoritm, 0, BCRYPT_ECCPUBLIC_BLOB, &hKey, publicKey, 140, 0);   
											if (s==STATUS_SUCCESS)
											{
												s=BCryptVerifySignature(hKey, NULL, pbHashBuffer, hashlength, sig, sigsize, 0);
												if (s==STATUS_SUCCESS)
												{
													DbgPrint("Valid signature");
												}
												else
												{													
													//Invalid signature. Wait a bit before returning
													LARGE_INTEGER timeout;
													timeout.QuadPart=-(LONGLONG)2* 10 * 1000 * 1000; // WDF_REL_TIMEOUT_IN_SEC(2);
													KeDelayExecutionThread(UserMode, FALSE, &timeout);
													DbgPrint("Signature failure: %x\n", s);																	
												}

												BCryptDestroyKey(hKey);
											}
											else
												DbgPrint("ImportKeyPair fail\n");

											BCryptCloseAlgorithmProvider(SignAlgoritm,0);

										}
										else
											DbgPrint("OpenSignAlgoritm fail\n");
									}
									else
										DbgPrint("FinishHash Failed\n");

									ExFreePool(pbHashBuffer);
								}
								else
									DbgPrint("Failed allocating pbHashBuffer\n");
							}
							else
								DbgPrint("BCRYPT_OBJECT_LENGTH hash failure\n");

						}
						else
							DbgPrint("Failure hashing data\n");

						BCryptDestroyHash(hHash); 
					}
					else
						DbgPrint("CreateHash failed\n");

					ExFreePool(pbHashObject);
				}
				else
					DbgPrint("Failure allocating room for pbHashObject\n");

			}
			else
				DbgPrint("BCRYPT_OBJECT_LENGTH alg failure\n");
		}

		BCryptCloseAlgorithmProvider(hashAlgoritm,0);
	}
	else
		DbgPrint("Failed getting a hash algoritm\n");

	//DbgPrint("returning %x\n", s);
	return s;
}

NTSTATUS TestProcess(PIMAGE_DOS_HEADER buf, DWORD size)
{
	UINT_PTR maxAddress = (UINT_PTR)buf + size;
	if (buf->e_magic != IMAGE_DOS_SIGNATURE)
		return STATUS_UNSUCCESSFUL;

	if ((DWORD)buf->e_lfanew >= size)
		return STATUS_UNSUCCESSFUL;

	PIMAGE_NT_HEADERS nth = (PIMAGE_NT_HEADERS)((UINT_PTR)buf + buf->e_lfanew);
	if (nth->Signature != IMAGE_NT_SIGNATURE)
		return STATUS_UNSUCCESSFUL;

	PIMAGE_SECTION_HEADER sections = IMAGE_FIRST_SECTION(nth);
	if ((UINT_PTR)sections >= maxAddress)
		return STATUS_UNSUCCESSFUL;

	int i;
	for (i = 0; i < nth->FileHeader.NumberOfSections; i++)
	{
		if (strcmp((char *)sections[i].Name, ".text") == 0)
		{
			if (((UINT_PTR)buf + sections[i].PointerToRawData + sections[i].SizeOfRawData) >= maxAddress)
				return STATUS_UNSUCCESSFUL;

			//BSOD on purpose if this isn't a match
			if (RtlCompareMemory((void*)(0x00400000 + sections[i].VirtualAddress), (void*)((UINT_PTR)buf + sections[i].PointerToRawData), sections[i].SizeOfRawData) == sections[i].SizeOfRawData)
			{
				//seems ok. Confirm the caller is from this .text section

				UINT_PTR startaddress;
				DWORD length;
				if (ZwQueryInformationThread(ZwCurrentThread(), (THREADINFOCLASS)ThreadQuerySetWin32StartAddress, &startaddress, sizeof(startaddress), &length) == STATUS_SUCCESS)
				{
					if ((startaddress >= (0x00400000 + sections[i].VirtualAddress)) && (startaddress < (0x00400000 + sections[i].VirtualAddress + sections[i].SizeOfRawData)))
						return STATUS_SUCCESS;
					else
						return STATUS_UNSUCCESSFUL;
				}
				else
					return STATUS_UNSUCCESSFUL;
			}
			else
				return STATUS_UNSUCCESSFUL;
		}
	}

	return STATUS_UNSUCCESSFUL;
}

NTSTATUS CheckSignatureOfFile(PUNICODE_STRING originalpath, BOOL isProcess)
{
	NTSTATUS s=STATUS_UNSUCCESSFUL;
	PVOID file=NULL;
	DWORD filesize;
	
	PVOID sig=NULL;
	DWORD sigsize;

	WCHAR MyBuffer[MAX_PATH*2];
	UNICODE_STRING p;
	PUNICODE_STRING path=&p;

	DbgPrint("CheckSignatureOfFile: ");



	p.Buffer=MyBuffer;
	p.Length=0;
	p.MaximumLength=MAX_PATH*2;
	s=RtlUnicodeStringCopy(path, originalpath);
	if (s!=STATUS_SUCCESS)
	{
		DbgPrint("Failure duplicating path: %x\n", s);
		return s;
	}
	


	s=LoadFile(path, &file, &filesize);
	if (s==STATUS_SUCCESS)
	{

		s=RtlAppendUnicodeToString(path, L".sig");
		if (s==STATUS_SUCCESS)
		{
			s=LoadFile(path, &sig, &sigsize);	

			if (s==STATUS_SUCCESS)
			{
				s=CheckSignature(file,filesize,sig,sigsize);		
				ExFreePool(sig);

				if ((s == STATUS_SUCCESS) && isProcess) //one extra check to see if it's actually CE and not just something renamed afterwards
					s=TestProcess((PIMAGE_DOS_HEADER)file, filesize);				
			}
			else
				DbgPrint("Failure loading %S\n", path->Buffer);
		}
	
		ExFreePool(file);
	}
	else
		DbgPrint("Failure loading %S\n", path->Buffer);
		

	//DbgPrint("returning %x\n", s);
	return s;
}



NTSTATUS SecurityCheck(void)
/*
Checks the current process for a valid signature
*/
{

	NTSTATUS s=STATUS_UNSUCCESSFUL;
	char *buffer[MAX_PATH*2+sizeof(UNICODE_STRING)];
	PUNICODE_STRING path=(PUNICODE_STRING)&buffer[0];
	ULONG length;

	if (KeGetCurrentIrql() != PASSIVE_LEVEL)
		return STATUS_UNSUCCESSFUL;


				
	if (ZwQueryInformationProcess(ZwCurrentProcess(), ProcessImageFileName, buffer, MAX_PATH*2, &length)==STATUS_SUCCESS)
	{
		path->MaximumLength=MAX_PATH*2;
		s=CheckSignatureOfFile(path,1);
		//DbgPrint("returning %x\n", s);
		return s;
	}
	else
		return STATUS_UNSUCCESSFUL;	
}
