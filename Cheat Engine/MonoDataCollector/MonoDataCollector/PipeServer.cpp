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
	//swprintf(eventpipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d_events", GetCurrentProcessId());

	AddVectoredExceptionHandler(1, ErrorFilter);
}

CPipeServer::~CPipeServer(void)
{
	if (attached)
	{
		mono_thread_detach(mono_selfthread);
		attached=FALSE;
	}
}

void CPipeServer::CreatePipeandWaitForconnect(void)
{
	OutputDebugStringA("CreatePipeandWaitForconnect called\n");
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
			

			g_free=(G_FREE)GetProcAddress(hMono, "g_free");
			mono_get_root_domain=(MONO_GET_ROOT_DOMAIN)GetProcAddress(hMono, "mono_get_root_domain");
			mono_thread_attach=(MONO_THREAD_ATTACH)GetProcAddress(hMono, "mono_thread_attach");
			mono_thread_detach=(MONO_THREAD_DETACH)GetProcAddress(hMono, "mono_thread_detach");

			mono_object_get_class=(MONO_OBJECT_GET_CLASS)GetProcAddress(hMono, "mono_object_get_class");
			
			mono_domain_foreach=(MONO_DOMAIN_FOREACH)GetProcAddress(hMono, "mono_domain_foreach");
			mono_domain_set=(MONO_DOMAIN_SET)GetProcAddress(hMono, "mono_domain_set");
			mono_assembly_foreach=(MONO_ASSEMBLY_FOREACH)GetProcAddress(hMono, "mono_assembly_foreach");
			mono_assembly_get_image=(MONO_ASSEMBLY_GET_IMAGE)GetProcAddress(hMono, "mono_assembly_get_image");
			
			mono_image_get_name=(MONO_IMAGE_GET_NAME)GetProcAddress(hMono, "mono_image_get_name");
			mono_image_get_table_info=(MONO_IMAGE_GET_TABLE_INFO)GetProcAddress(hMono, "mono_image_get_table_info");
			mono_image_rva_map=(MONO_IMAGE_RVA_MAP)GetProcAddress(hMono, "mono_image_rva_map");

			mono_table_info_get_rows=(MONO_TABLE_INFO_GET_ROWS)GetProcAddress(hMono, "mono_table_info_get_rows");
			mono_metadata_decode_row_col=(MONO_METADATA_DECODE_ROW_COL)GetProcAddress(hMono, "mono_metadata_decode_row_col");
			mono_metadata_string_heap=(MONO_METADATA_STRING_HEAP)GetProcAddress(hMono, "mono_metadata_string_heap");

			
			mono_class_get=(MONO_CLASS_GET)GetProcAddress(hMono, "mono_class_get");		
			mono_class_from_name_case=(MONO_CLASS_FROM_NAME_CASE)GetProcAddress(hMono, "mono_class_from_name_case");	
			mono_class_get_name=(MONO_CLASS_GET_NAME)GetProcAddress(hMono, "mono_class_get_name");
			mono_class_get_namespace=(MONO_CLASS_GET_NAMESPACE)GetProcAddress(hMono, "mono_class_get_namespace");
			mono_class_get_methods=(MONO_CLASS_GET_METHODS)GetProcAddress(hMono, "mono_class_get_methods");		
			mono_class_get_method_from_name=(MONO_CLASS_GET_METHOD_FROM_NAME)GetProcAddress(hMono, "mono_class_get_method_from_name");		
			mono_class_get_fields=(MONO_CLASS_GET_FIELDS)GetProcAddress(hMono, "mono_class_get_fields");		
			
			mono_class_num_fields=(MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "mono_class_num_fields");	
			mono_class_num_methods=(MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "mono_class_num_methods");		
			

			mono_field_get_name=(MONO_FIELD_GET_NAME)GetProcAddress(hMono, "mono_field_get_name");	
			mono_field_get_type=(MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "mono_field_get_type");	
			mono_field_get_parent=(MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "mono_field_get_parent");	
			mono_field_get_offset=(MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "mono_field_get_offset");				

			mono_type_get_name=(MONO_TYPE_GET_NAME)GetProcAddress(hMono, "mono_type_get_name");	

			mono_method_get_name=(MONO_METHOD_GET_NAME)GetProcAddress(hMono, "mono_method_get_name");	
			mono_method_get_class=(MONO_METHOD_GET_CLASS)GetProcAddress(hMono, "mono_method_get_class");	
			mono_method_get_header=(MONO_METHOD_GET_HEADER)GetProcAddress(hMono, "mono_method_get_header");	

			mono_compile_method=(MONO_COMPILE_METHOD)GetProcAddress(hMono, "mono_compile_method");	
			mono_free_method=(MONO_FREE_METHOD)GetProcAddress(hMono, "mono_free_method");	
			mono_jit_info_table_find=(MONO_JIT_INFO_TABLE_FIND)GetProcAddress(hMono, "mono_jit_info_table_find");	
			mono_jit_info_get_method=(MONO_JIT_INFO_GET_METHOD)GetProcAddress(hMono, "mono_jit_info_get_method");	
			mono_jit_info_get_code_start=(MONO_JIT_INFO_GET_CODE_START)GetProcAddress(hMono, "mono_jit_info_get_code_start");	
			mono_jit_info_get_code_size=(MONO_JIT_INFO_GET_CODE_SIZE)GetProcAddress(hMono, "mono_jit_info_get_code_size");	

			mono_method_header_get_code=(MONO_METHOD_HEADER_GET_CODE)GetProcAddress(hMono, "mono_method_header_get_code");	
			mono_disasm_code=(MONO_DISASM_CODE)GetProcAddress(hMono, "mono_disasm_code");	


			
			if (mono_get_root_domain==NULL) OutputDebugStringA("mono_get_root_domain not assigned");
			if (mono_thread_attach==NULL) OutputDebugStringA("mono_thread_attach not assigned");
			if (mono_object_get_class==NULL) OutputDebugStringA("mono_object_get_class not assigned");
			if (mono_class_get_name==NULL) OutputDebugStringA("mono_class_get_name not assigned");
			if (mono_domain_foreach==NULL) OutputDebugStringA("mono_domain_foreach not assigned");
			if (mono_domain_set==NULL) OutputDebugStringA("mono_domain_set not assigned");
			if (mono_assembly_foreach==NULL) OutputDebugStringA("mono_assembly_foreach not assigned");
			if (mono_assembly_get_image==NULL) OutputDebugStringA("mono_assembly_get_image not assigned");
			if (mono_image_get_name==NULL) OutputDebugStringA("mono_image_get_name not assigned");


			mono_selfthread=mono_thread_attach(mono_get_root_domain());
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

	//OutputDebugStringA("MONOCMD_OBJECT_GETCLASS");

	
	ExpectingAccessViolations=TRUE; //cause access violations to throw an exception
	try
	{
		unsigned int i;
		
		klass=mono_object_get_class(object);
		classname=mono_class_get_name(klass);

		//small test to see if the classname is readable
		for (i=0; i<strlen(classname); i++)
		{
			char x=classname[i];
			if (x=='\0')
				break;			
		}		

		if (klass!=0)
		{		
			WriteQword((UINT64)klass);
			WriteWord(strlen(classname));
			Write(classname, strlen(classname));
		}
		else
		{
			WriteQword(0);
		}


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
	//OutputDebugStringA("EnumDomains");
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
	//OutputDebugStringA("EnumAssemblies");
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

		name=mono_class_get_namespace(c);
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
			void *fieldtype=mono_field_get_type(field);
			WriteQword((UINT_PTR)fieldtype);
			WriteQword((UINT_PTR)mono_field_get_parent(field));
			WriteDword((UINT_PTR)mono_field_get_offset(field));

			name=mono_field_get_name(field);
			WriteWord(strlen(name));
			Write(name, strlen(name));		

			name=mono_type_get_name(fieldtype);
			if (name)
			{
				WriteWord(strlen(name));
				Write(name, strlen(name));		
				g_free(name);
			}
			else
				WriteWord(0);
			
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

void CPipeServer::CompileMethod()
{
	void *method=(void *)ReadQword();
	void *result=mono_compile_method(method);
	WriteQword((UINT_PTR)result);
}

void CPipeServer::GetMethodHeader()
{
	void *method=(void *)ReadQword();
	void *result=mono_method_get_header(method);
	WriteQword((UINT_PTR)result);
}

void CPipeServer::GetILCode()
{
	void *methodheader=(void *)ReadQword();
	UINT32 code;
	void *result=mono_method_header_get_code(methodheader, &code, NULL);
	WriteQword((UINT_PTR)result);
	WriteDword(code);
}

void CPipeServer::RvaMap()
{
	void *image=(void *)ReadQword();
	UINT32 offset=ReadDword();
	void *result=mono_image_rva_map(image, offset);

	WriteQword((UINT_PTR)result);
}

void CPipeServer::GetJitInfo()
{
	void *domain=(void *)ReadQword();
	void *address=(void *)ReadQword();
	void *jitinfo=mono_jit_info_table_find(domain, address);
	WriteQword((UINT_PTR)jitinfo);
	if (jitinfo)
	{
		WriteQword((UINT_PTR)mono_jit_info_get_method(jitinfo));
		WriteQword((UINT_PTR)mono_jit_info_get_code_start(jitinfo));
		WriteDword((UINT_PTR)mono_jit_info_get_code_size(jitinfo));
	}
}

void CPipeServer::FindClass()
{
	void *image=(void *)ReadQword();
	WORD length=ReadWord();
	char *cn=NULL;
	char *ns=NULL;
	void *klass;

	cn=(char *)malloc(length+1);
	if (length)
		Read(cn, length);

	cn[length]=0;
	
	length=ReadWord();
	
	ns=(char *)malloc(length+1);
	if (length)
		Read(ns, length);

	ns[length]=0;
	
	klass=mono_class_from_name_case(image, ns, cn);

	if (cn)
		free(cn);	

	if (ns)
		free(ns);

	WriteQword((UINT_PTR)klass);

}

void CPipeServer::FindMethod()
{
	void *klass=(void *)ReadQword();
	WORD length=ReadWord();
	char *methodname=(char *)malloc(length+1);
	void *method=NULL;
	if (length)
		Read(methodname, length);
	methodname[length]=0;

	
	method=mono_class_get_method_from_name(klass, methodname, -1);
	WriteQword((UINT_PTR)method);
}

void CPipeServer::GetMethodName()
{
	void *method=(void *)ReadQword();
	char *methodname=mono_method_get_name(method);

	WriteWord(strlen(methodname));
	Write(methodname, strlen(methodname));	
}

void CPipeServer::GetMethodClass()
{
	void *method=(void *)ReadQword();
	void *result=mono_method_get_class(method);
	WriteQword((UINT_PTR)result);
}

void CPipeServer::GetClassName()
{
	void *klass=(void *)ReadQword();
	char *methodname=mono_class_get_name(klass);

	WriteWord(strlen(methodname));
	Write(methodname, strlen(methodname));	
}

void CPipeServer::GetClassNamespace()
{
	void *klass=(void *)ReadQword();
	char *methodname=mono_class_get_namespace(klass);

	WriteWord(strlen(methodname));
	Write(methodname, strlen(methodname));	
}

void CPipeServer::FreeMethod()
{
	mono_free_method((void *)ReadQword());
}

void CPipeServer::DisassembleMethod()
{
	void *method=(void *)ReadQword();
	void *methodheader=mono_method_get_header(method);
	UINT32 codesize, maxstack;
	void *ilcode=mono_method_header_get_code(methodheader, &codesize, &maxstack);
	char *disassembly=mono_disasm_code(NULL, method, ilcode, (void *)((UINT_PTR)ilcode+codesize));

	WriteWord(strlen(disassembly));
	Write(disassembly, strlen(disassembly));
	g_free(disassembly);
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

					case MONOCMD_COMPILEMETHOD:
						CompileMethod();
						break;

					case MONOCMD_GETMETHODHEADER:
						GetMethodHeader();
						break;

					case MONOCMD_GETMETHODHEADER_CODE:
						GetILCode();
						break;

					case MONOCMD_LOOKUPRVA:
						RvaMap();
						break;

					case MONOCMD_GETJITINFO:
						GetJitInfo();
						break;

					case MONOCMD_FINDCLASS:
						FindClass();
						break;

					case MONOCMD_FINDMETHOD:
						FindMethod();
						break;

					case MONOCMD_GETMETHODNAME:
						GetMethodName();
						break;		

					case MONOCMD_GETMETHODCLASS:
						GetMethodClass();
						break;

					case MONOCMD_GETCLASSNAME:
						GetClassName();
						break;

					case MONOCMD_GETCLASSNAMESPACE:
						GetClassNamespace();
						break;

					case MONOCMD_FREEMETHOD:
						FreeMethod();
						break;

					case MONOCMD_TERMINATE:												
						return;

					case MONOCMD_DISASSEMBLE:
						DisassembleMethod();
						break;
				}
			}			
		}
		catch (char *e)
		{
			//Pipe error, or something else that wasn't caught. Exit the connection and start over	
			OutputDebugStringA("Pipe error:\n");
			OutputDebugStringA(e);

			if (attached)
			{
				mono_thread_detach(mono_selfthread);
				attached=FALSE;
			}
			

		}
		catch (...)
		{
			OutputDebugStringA("Unexcpected pipe error\n");
			if (attached)
			{
				mono_thread_detach(mono_selfthread);
				attached=FALSE;
			}	
		}

	}

}


