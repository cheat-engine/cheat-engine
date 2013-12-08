#include "StdAfx.h"
#include "PipeServer.h"

BOOL ExpectingAccessViolations=FALSE;

void ErrorThrow(void)
{
	throw ("Access violation caught");
}

LONG NTAPI ErrorFilter(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
	if ((ExpectingAccessViolations) && (ExceptionInfo->ExceptionRecord->ExceptionCode==0xc0000005))
	{
		ExceptionInfo->ContextRecord->Eip=(DWORD)ErrorThrow;		
		return EXCEPTION_CONTINUE_EXECUTION;
	}

	return EXCEPTION_CONTINUE_SEARCH;
}

	


CPipeServer::CPipeServer(void)
{
	attached=FALSE;
	swprintf(datapipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d", GetCurrentProcessId());
	swprintf(eventpipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d_events", GetCurrentProcessId());

	AddVectoredExceptionHandler(1, ErrorFilter);
}

CPipeServer::~CPipeServer(void)
{
}

void CPipeServer::CreatePipeandWaitForconnect(void)
{
	OutputDebugStringA("CreatePipeandWaitForconnect\n");
	if ((pipehandle) && (pipehandle!=INVALID_HANDLE_VALUE))
	{
		CloseHandle(pipehandle);
		pipehandle=0;
	}

	pipehandle=CreateNamedPipe(datapipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 16, INFINITE, NULL);
	//eventpipe=CreateNamedPipe(eventpipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 0, INFINITE, NULL);

	ConnectNamedPipe(pipehandle, NULL);
}

void CPipeServer::InitMono()
{
	HMODULE hMono=GetModuleHandle(L"mono.dll");

	WriteQword((UINT64)hMono);
	if (hMono)
	{		
		std::stringstream x;
		x.clear();
		x << "Mono dll found at " << std::hex << hMono <<"\n";
		OutputDebugStringA(x.str().c_str());

		if (attached==FALSE)
		{
			void *thread;
			mono_get_root_domain=(MONO_GET_ROOT_DOMAIN)GetProcAddress(hMono, "mono_get_root_domain");
			mono_thread_attach=(MONO_THREAD_ATTACH)GetProcAddress(hMono, "mono_thread_attach");
			mono_object_get_class=(MONO_OBJECT_GET_CLASS)GetProcAddress(hMono, "mono_object_get_class");
			mono_class_get_name=(MONO_CLASS_GET_NAME)GetProcAddress(hMono, "mono_class_get_name");
			mono_domain_foreach=(MONO_DOMAIN_FOREACH)GetProcAddress(hMono, "mono_domain_foreach");
			mono_domain_set=(MONO_DOMAIN_SET)GetProcAddress(hMono, "mono_domain_set");
			mono_assembly_foreach=(MONO_ASSEMBLY_FOREACH)GetProcAddress(hMono, "mono_assembly_foreach");
			mono_assembly_get_image=(MONO_ASSEMBLY_GET_IMAGE)GetProcAddress(hMono, "mono_assembly_get_image");
			mono_image_get_name=(MONO_IMAGE_GET_NAME)GetProcAddress(hMono, "mono_image_get_name");

			mono_image_get_table_info=(MONO_IMAGE_GET_TABLE_INFO)GetProcAddress(hMono, "mono_image_get_table_info");
			mono_table_info_get_rows=(MONO_TABLE_INFO_GET_ROWS)GetProcAddress(hMono, "mono_table_info_get_rows");
			mono_metadata_decode_row_col=(MONO_METADATA_DECODE_ROW_COL)GetProcAddress(hMono, "mono_metadata_decode_row_col");
			mono_metadata_string_heap=(MONO_METADATA_STRING_HEAP)GetProcAddress(hMono, "mono_metadata_string_heap");
			mono_class_get=(MONO_CLASS_GET)GetProcAddress(hMono, "mono_class_get");		
			mono_class_get_methods=(MONO_CLASS_GET_METHODS)GetProcAddress(hMono, "mono_class_get_methods");		
			mono_class_get_fields=(MONO_CLASS_GET_FIELDS)GetProcAddress(hMono, "mono_class_get_fields");		
			
			mono_class_num_fields=(MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "mono_class_num_fields");	
			mono_class_num_methods=(MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "mono_class_num_methods");		

			mono_field_get_name=(MONO_FIELD_GET_NAME)GetProcAddress(hMono, "mono_field_get_name");	
			mono_field_get_type=(MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "mono_field_get_type");	
			mono_field_get_parent=(MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "mono_field_get_parent");	
			mono_field_get_offset=(MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "mono_field_get_offset");				

			mono_method_get_name=(MONO_METHOD_GET_NAME)GetProcAddress(hMono, "mono_method_get_name");	


			if (mono_get_root_domain==NULL) OutputDebugStringA("mono_get_root_domain not assigned");
			if (mono_thread_attach==NULL) OutputDebugStringA("mono_thread_attach not assigned");
			if (mono_object_get_class==NULL) OutputDebugStringA("mono_object_get_class not assigned");
			if (mono_class_get_name==NULL) OutputDebugStringA("mono_class_get_name not assigned");
			if (mono_domain_foreach==NULL) OutputDebugStringA("mono_domain_foreach not assigned");
			if (mono_domain_set==NULL) OutputDebugStringA("mono_domain_set not assigned");
			if (mono_assembly_foreach==NULL) OutputDebugStringA("mono_assembly_foreach not assigned");
			if (mono_assembly_get_image==NULL) OutputDebugStringA("mono_assembly_get_image not assigned");
			if (mono_image_get_name==NULL) OutputDebugStringA("mono_image_get_name not assigned");


			thread=mono_thread_attach(mono_get_root_domain());
			attached=TRUE;
		}
		else
			OutputDebugStringA("Already attached");
	}
}



void CPipeServer::Object_GetClass()
{
	void *object=(void *)ReadQword();
	char *classname;
	void *klass;

	OutputDebugStringA("MONOCMD_OBJECT_GETCLASS");

	
	ExpectingAccessViolations=TRUE; //cause access violations to throw an exception
	try
	{
		
		klass=mono_object_get_class(object);
		classname=mono_class_get_name(klass);

		WriteQword((UINT64)klass);
		WriteWord(strlen(classname));
		Write(classname, strlen(classname));

	}
	catch (char *e)	
	{
		OutputDebugStringA("Error getting the class:\n");
		OutputDebugStringA(e);
		WriteQword(0); //failure. Invalid object
	}

	ExpectingAccessViolations=FALSE; //back to normal behaviour
}

void _cdecl DomainEnumerator(void *domain, std::vector<UINT64> *v)
{
	v->push_back((UINT_PTR)domain);
}

void CPipeServer::EnumDomains(void)
{
	unsigned int i;
	std::vector<UINT64> v;
	OutputDebugStringA("EnumDomains");
	mono_domain_foreach((MonoDomainFunc)DomainEnumerator, &v);

	
	WriteDword(v.size());
	for (i=0; i<v.size(); i++)
		WriteQword(v[i]);	
}

void CPipeServer::SetCurrentDomain(void)
{
	void *domain=(void *)ReadQword();
	int r=mono_domain_set(domain, FALSE);

	WriteDword(r);
}

void _cdecl AssemblyEnumerator(void *domain, std::vector<UINT64> *v)
{
	v->push_back((UINT_PTR)domain);
}

void CPipeServer::EnumAssemblies()
{
	unsigned int i;
	std::vector<UINT64> v;
	OutputDebugStringA("EnumAssemblies");
	mono_assembly_foreach((GFunc)AssemblyEnumerator, &v);
	
	WriteDword(v.size());
	for (i=0; i<v.size(); i++)
		WriteQword(v[i]);	
}

void CPipeServer::GetImageFromAssembly()
{
	void *assembly=(void *)ReadQword();
	void *image=mono_assembly_get_image(assembly);
    WriteQword((UINT_PTR)image);
}

void CPipeServer::GetImageName()
{
	void *image=(void *)ReadQword();
	char *s=mono_image_get_name(image);

	WriteWord(strlen(s));
	Write(s, strlen(s));
}

void CPipeServer::EnumClassesInImage()
{
	int i;
	void *image=(void *)ReadQword();
	void *tdef=mono_image_get_table_info (image, MONO_TABLE_TYPEDEF);
	int tdefcount=mono_table_info_get_rows(tdef);


	WriteDword(tdefcount);

	for (i = 0; i < tdefcount; i++)
	{		
		void *c = mono_class_get(image, MONO_TOKEN_TYPE_DEF | i+1);
		char *name=mono_class_get_name(c);

		WriteQword((UINT_PTR)c);

		WriteWord(strlen(name));
		Write(name, strlen(name));				
	}
}

void CPipeServer::EnumFieldsInClass()
{
	void *c=(void *)ReadQword();
	void *iter=NULL;
	void *field;

	do
	{
		field=mono_class_get_fields(c, &iter);
		WriteQword((UINT_PTR)field);

		if (field)
		{
			char *name;
			WriteQword((UINT_PTR)mono_field_get_type(field));
			WriteQword((UINT_PTR)mono_field_get_parent(field));
			WriteDword((UINT_PTR)mono_field_get_offset(field));

			name=mono_field_get_name(field);
			WriteWord(strlen(name));
			Write(name, strlen(name));			
		}
	} while (field);	
}


void CPipeServer::EnumMethodsInClass()
{
	void *c=(void *)ReadQword();
	void *iter=NULL;
	void *method;

	do
	{
		method=mono_class_get_methods(c, &iter);
		WriteQword((UINT_PTR)method);

		if (method)
		{
			char *name;

			name=mono_method_get_name(method);
			WriteWord(strlen(name));
			Write(name, strlen(name));			
		}
	} while (method);

}

void CPipeServer::Start(void)
{
	BYTE command;
	while (TRUE)
	{
		CreatePipeandWaitForconnect();

		try
		{
			while (TRUE)
			{
				command=ReadByte();

				switch(command)
				{
					case MONOCMD_INITMONO:
						InitMono();					
						break;	

					case MONOCMD_OBJECT_GETCLASS:
						Object_GetClass();
						break;

					case MONOCMD_ENUMDOMAINS:
						EnumDomains();
						break;

					case MONOCMD_SETCURRENTDOMAIN:
						SetCurrentDomain();
						break;

					case MONOCMD_ENUMASSEMBLIES:
						EnumAssemblies();
						break;

					case MONOCMD_GETIMAGEFROMASSEMBLY:
						GetImageFromAssembly();
						break;

					case MONOCMD_GETIMAGENAME:
						GetImageName();
						break;

					case MONOCMD_ENUMCLASSESINIMAGE:
						EnumClassesInImage();
						break;

					case MONOCMD_ENUMFIELDSINCLASS:
						EnumFieldsInClass();
						break;

					case MONOCMD_ENUMMETHODSINCLASS:
						EnumMethodsInClass();
						break;

				}
			}			
		}
		catch (char *e)
		{
			//Pipe error, or something else that wasn't caught. Exit the connection and start over	
			OutputDebugStringA("Pipe error:\n");
			OutputDebugStringA(e);
		}
		catch (...)
		{
			OutputDebugStringA("Unexcpected pipe error\n");
		}

	}

}


