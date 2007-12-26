NTSTATUS CETC_CreateFile(PHANDLE file, PCWSTR filename);
NTSTATUS CETC_OpenFile(PHANDLE file, PCWSTR filename);

#define CETC_Write(file,buffer,size,iosb) ZwWriteFile(file,NULL,NULL,NULL,iosb,buffer,size,NULL,NULL)