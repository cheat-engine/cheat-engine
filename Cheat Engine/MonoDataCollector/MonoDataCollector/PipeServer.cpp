#include "StdAfx.h"
#include "PipeServer.h"

BOOL ExpectingAccessViolations = FALSE;
DWORD ExpectingAccessViolationsThread = 0;

void ErrorThrow(void)
{
	throw ("Access violation caught");
}

int looper = 0;
LONG NTAPI ErrorFilter(struct _EXCEPTION_POINTERS *ExceptionInfo)
{
	if ((ExpectingAccessViolations) && (GetCurrentThreadId() == ExpectingAccessViolationsThread) && (ExceptionInfo->ExceptionRecord->ExceptionCode == 0xc0000005))
	{
#ifdef _AMD64_
		ExceptionInfo->ContextRecord->Rip = (UINT_PTR)ErrorThrow;
#else
		ExceptionInfo->ContextRecord->Eip = (DWORD)ErrorThrow;
#endif
		return EXCEPTION_CONTINUE_EXECUTION;
	}



	return EXCEPTION_CONTINUE_SEARCH;
}


CPipeServer::CPipeServer(void)
{
	attached = FALSE;
	swprintf(datapipename, 256, L"\\\\.\\pipe\\cemonodc_pid%d", GetCurrentProcessId());
	//swprintf(eventpipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d_events", GetCurrentProcessId());

	AddVectoredExceptionHandler(1, ErrorFilter);
}

CPipeServer::~CPipeServer(void)
{
	if (attached)
	{
		mono_thread_detach(mono_selfthread);
		attached = FALSE;
	}
}

char* CPipeServer::ReadString(void)
{
	WORD length = ReadWord();
	char *result = (char *)malloc(length + 1);
	void *method = NULL;
	if (length)
		Read(result, length);
	result[length] = 0;
	return result;
}

void CPipeServer::WriteString1(const char* value) //for 1 byte length strings
{
	if (value)
	{
		int n = strlen(value);
		WriteByte(n);
		Write((PVOID)value, n);

		//OutputDebugStringA(value);
	}
	else
	{
		WriteWord(0);
	}
}

void CPipeServer::WriteString(const char* value)
{
	if (value)
	{
		int n = strlen(value);
		WriteWord(n);
		Write((PVOID)value, n);

		//OutputDebugStringA(value);
	}
	else
	{
		WriteWord(0);
	}
}

void CPipeServer::FreeString(char* value)
{
	if (value)
		free(value);
}


void CPipeServer::CreatePipeandWaitForconnect(void)
{
	OutputDebugStringA("CreatePipeandWaitForconnect called\n");
	if ((pipehandle) && (pipehandle != INVALID_HANDLE_VALUE))
	{
		CloseHandle(pipehandle);
		pipehandle = 0;
	}

	pipehandle = CreateNamedPipe(datapipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1, 256 * 1024, 16, INFINITE, NULL);
	//eventpipe=CreateNamedPipe(eventpipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 0, INFINITE, NULL);

	ConnectNamedPipe(pipehandle, NULL);
}

void __cdecl customfreeimplementation(PVOID address)
{
//	free(address);
	//freaking memleak !!!!!
}


void CPipeServer::InitMono()
{
	HMODULE hMono = GetModuleHandle(L"mono.dll");
	il2cpp = FALSE;
		

	if (!hMono)
	{
		//this process doesn't use mono.dll  Perhaps it's renamed.  Find a module that exports mono_thread_attach
		HANDLE ths = CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, GetCurrentProcessId());
		if (ths != INVALID_HANDLE_VALUE)
		{
			MODULEENTRY32 me;
			me.dwSize = sizeof(me);

			if (Module32First(ths, &me))
			{
				do
				{
					if (GetProcAddress(me.hModule, "mono_thread_attach"))
					{
						hMono = me.hModule;
						break;
					}

					if (GetProcAddress(me.hModule, "il2cpp_thread_attach"))
					{
						il2cpp = true;
						hMono = me.hModule;
						break;
					}

				} while (Module32Next(ths, &me));

			}
			CloseHandle(ths);
		}
	}

	WriteQword((UINT64)hMono);
	if (hMono)
	{
		std::stringstream x;
		x.clear();
		x << "Mono dll found at " << std::hex << hMono << "\n";
		//OutputDebugStringA(x.str().c_str());

		if (attached == FALSE)
		{


			if (il2cpp)
			{
				g_free = (G_FREE)GetProcAddress(hMono, "g_free");

				if (!g_free)
					g_free = (G_FREE)GetProcAddress(hMono, "il2cpp_unity_g_free");

				if (!g_free)
					g_free = customfreeimplementation;

				mono_free = (MONO_FREE)GetProcAddress(hMono, "il2cpp_free");

				mono_get_root_domain = (MONO_GET_ROOT_DOMAIN)GetProcAddress(hMono, "il2cpp_get_root_domain");
				mono_thread_attach = (MONO_THREAD_ATTACH)GetProcAddress(hMono, "il2cpp_thread_attach");
				mono_thread_detach = (MONO_THREAD_DETACH)GetProcAddress(hMono, "il2cpp_thread_detach");

				mono_object_get_class = (MONO_OBJECT_GET_CLASS)GetProcAddress(hMono, "il2cpp_object_get_class");

				mono_domain_foreach = (MONO_DOMAIN_FOREACH)GetProcAddress(hMono, "il2cpp_domain_foreach");
				mono_domain_set = (MONO_DOMAIN_SET)GetProcAddress(hMono, "il2cpp_domain_set");
				mono_domain_get = (MONO_DOMAIN_GET)GetProcAddress(hMono, "il2cpp_domain_get");
				mono_assembly_foreach = (MONO_ASSEMBLY_FOREACH)GetProcAddress(hMono, "il2cpp_assembly_foreach");
				mono_assembly_get_image = (MONO_ASSEMBLY_GET_IMAGE)GetProcAddress(hMono, "il2cpp_assembly_get_image");
				mono_image_get_assembly = (MONO_IMAGE_GET_ASSEMBLY)GetProcAddress(hMono, "il2cpp_image_get_assembly");

				mono_image_get_name = (MONO_IMAGE_GET_NAME)GetProcAddress(hMono, "il2cpp_image_get_name");
				mono_image_get_table_info = (MONO_IMAGE_GET_TABLE_INFO)GetProcAddress(hMono, "mono_image_get_table_info");
				mono_image_rva_map = (MONO_IMAGE_RVA_MAP)GetProcAddress(hMono, "il2cpp_image_rva_map");

				mono_table_info_get_rows = (MONO_TABLE_INFO_GET_ROWS)GetProcAddress(hMono, "il2cpp_table_info_get_rows");
				mono_metadata_decode_row_col = (MONO_METADATA_DECODE_ROW_COL)GetProcAddress(hMono, "il2cpp_metadata_decode_row_col");
				mono_metadata_string_heap = (MONO_METADATA_STRING_HEAP)GetProcAddress(hMono, "il2cpp_metadata_string_heap");


				mono_class_get = (MONO_CLASS_GET)GetProcAddress(hMono, "il2cpp_class_get");
				mono_class_from_typeref = (MONO_CLASS_FROM_TYPEREF)GetProcAddress(hMono, "il2cpp_class_from_typeref");
				mono_class_name_from_token = (MONO_CLASS_NAME_FROM_TOKEN)GetProcAddress(hMono, "il2cpp_class_name_from_token");
				mono_class_from_name_case = (MONO_CLASS_FROM_NAME_CASE)GetProcAddress(hMono, "il2cpp_class_from_name_case");
				mono_class_from_name = (MONO_CLASS_FROM_NAME_CASE)GetProcAddress(hMono, "il2cpp_class_from_name");
				mono_class_get_name = (MONO_CLASS_GET_NAME)GetProcAddress(hMono, "il2cpp_class_get_name");
				mono_class_get_namespace = (MONO_CLASS_GET_NAMESPACE)GetProcAddress(hMono, "il2cpp_class_get_namespace");
				mono_class_get_methods = (MONO_CLASS_GET_METHODS)GetProcAddress(hMono, "il2cpp_class_get_methods");
				mono_class_get_method_from_name = (MONO_CLASS_GET_METHOD_FROM_NAME)GetProcAddress(hMono, "il2cpp_class_get_method_from_name");
				mono_class_get_fields = (MONO_CLASS_GET_FIELDS)GetProcAddress(hMono, "il2cpp_class_get_fields");
				mono_class_get_parent = (MONO_CLASS_GET_PARENT)GetProcAddress(hMono, "il2cpp_class_get_parent");
				mono_class_is_generic = (MONO_CLASS_IS_GENERIC)GetProcAddress(hMono, "il2cpp_class_is_generic");
				mono_class_vtable = (MONO_CLASS_VTABLE)GetProcAddress(hMono, "il2cpp_class_vtable");
				mono_class_from_mono_type = (MONO_CLASS_FROM_MONO_TYPE)GetProcAddress(hMono, "il2cpp_class_from_mono_type");
				mono_class_get_element_class = (MONO_CLASS_GET_ELEMENT_CLASS)GetProcAddress(hMono, "il2cpp_class_get_element_class");

				mono_class_num_fields = (MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "il2cpp_class_num_fields");
				mono_class_num_methods = (MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "il2cpp_class_num_methods");


				mono_field_get_name = (MONO_FIELD_GET_NAME)GetProcAddress(hMono, "il2cpp_field_get_name");
				mono_field_get_type = (MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "il2cpp_field_get_type");
				mono_field_get_parent = (MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "il2cpp_field_get_parent");
				mono_field_get_offset = (MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "il2cpp_field_get_offset");
				mono_field_get_flags = (MONO_FIELD_GET_FLAGS)GetProcAddress(hMono, "il2cpp_field_get_flags");

				mono_type_get_name = (MONO_TYPE_GET_NAME)GetProcAddress(hMono, "il2cpp_type_get_name");
				mono_type_get_type = (MONO_TYPE_GET_TYPE)GetProcAddress(hMono, "il2cpp_type_get_type");
				mono_type_get_name_full = (MONO_TYPE_GET_NAME_FULL)GetProcAddress(hMono, "il2cpp_type_get_name_full");

				mono_method_get_name = (MONO_METHOD_GET_NAME)GetProcAddress(hMono, "il2cpp_method_get_name");
				mono_method_get_class = (MONO_METHOD_GET_CLASS)GetProcAddress(hMono, "il2cpp_method_get_class");
				mono_method_get_header = (MONO_METHOD_GET_HEADER)GetProcAddress(hMono, "il2cpp_method_get_header");
				mono_method_signature = (MONO_METHOD_SIG)GetProcAddress(hMono, "il2cpp_method_signature");
				mono_method_get_param_names = (MONO_METHOD_GET_PARAM_NAMES)GetProcAddress(hMono, "il2cpp_method_get_param_names");



				mono_signature_get_desc = (MONO_SIGNATURE_GET_DESC)GetProcAddress(hMono, "il2cpp_signature_get_desc");
				mono_signature_get_params = (MONO_SIGNATURE_GET_PARAMS)GetProcAddress(hMono, "il2cpp_signature_get_params");
				mono_signature_get_param_count = (MONO_SIGNATURE_GET_PARAM_COUNT)GetProcAddress(hMono, "il2cpp_signature_get_param_count");
				mono_signature_get_return_type = (MONO_SIGNATURE_GET_RETURN_TYPE)GetProcAddress(hMono, "il2cpp_signature_get_return_type");



				mono_compile_method = (MONO_COMPILE_METHOD)GetProcAddress(hMono, "il2cpp_compile_method");
				mono_free_method = (MONO_FREE_METHOD)GetProcAddress(hMono, "il2cpp_free_method");
				mono_jit_info_table_find = (MONO_JIT_INFO_TABLE_FIND)GetProcAddress(hMono, "il2cpp_jit_info_table_find");
				mono_jit_info_get_method = (MONO_JIT_INFO_GET_METHOD)GetProcAddress(hMono, "il2cpp_jit_info_get_method");
				mono_jit_info_get_code_start = (MONO_JIT_INFO_GET_CODE_START)GetProcAddress(hMono, "il2cpp_jit_info_get_code_start");
				mono_jit_info_get_code_size = (MONO_JIT_INFO_GET_CODE_SIZE)GetProcAddress(hMono, "il2cpp_jit_info_get_code_size");
				mono_jit_exec = (MONO_JIT_EXEC)GetProcAddress(hMono, "il2cpp_jit_exec");

				mono_method_header_get_code = (MONO_METHOD_HEADER_GET_CODE)GetProcAddress(hMono, "il2cpp_method_header_get_code");
				mono_disasm_code = (MONO_DISASM_CODE)GetProcAddress(hMono, "il2cpp_disasm_code");

				mono_vtable_get_static_field_data = (MONO_VTABLE_GET_STATIC_FIELD_DATA)GetProcAddress(hMono, "il2cpp_vtable_get_static_field_data");

				mono_method_desc_new = (MONO_METHOD_DESC_NEW)GetProcAddress(hMono, "il2cpp_method_desc_new");;
				mono_method_desc_from_method = (MONO_METHOD_DESC_FROM_METHOD)GetProcAddress(hMono, "il2cpp_method_desc_from_method");;
				mono_method_desc_free = (MONO_METHOD_DESC_FREE)GetProcAddress(hMono, "il2cpp_method_desc_free");;

				mono_string_new = (MONO_STRING_NEW)GetProcAddress(hMono, "il2cpp_string_new");
				mono_string_to_utf8 = (MONO_STRING_TO_UTF8)GetProcAddress(hMono, "il2cpp_string_to_utf8");
				mono_array_new = (MONO_ARRAY_NEW)GetProcAddress(hMono, "il2cpp_array_new");
				mono_value_box = (MONO_VALUE_BOX)GetProcAddress(hMono, "il2cpp_value_box");
				mono_object_unbox = (MONO_OBJECT_UNBOX)GetProcAddress(hMono, "il2cpp_object_unbox");
				mono_object_new = (MONO_OBJECT_NEW)GetProcAddress(hMono, "il2cpp_object_new");

				mono_class_get_type = (MONO_CLASS_GET_TYPE)GetProcAddress(hMono, "il2cpp_class_get_type");

				mono_method_desc_search_in_image = (MONO_METHOD_DESC_SEARCH_IN_IMAGE)GetProcAddress(hMono, "il2cpp_method_desc_search_in_image");
				mono_runtime_invoke = (MONO_RUNTIME_INVOKE)GetProcAddress(hMono, "il2cpp_runtime_invoke");
				mono_runtime_object_init = (MONO_RUNTIME_OBJECT_INIT)GetProcAddress(hMono, "il2cpp_runtime_object_init");


				mono_assembly_name_new = (MONO_ASSEMBLY_NAME_NEW)GetProcAddress(hMono, "il2cpp_assembly_name_new");
				mono_assembly_loaded = (MONO_ASSEMBLY_LOADED)GetProcAddress(hMono, "il2cpp_assembly_loaded");
				mono_assembly_open = (MONO_ASSEMBLY_OPEN)GetProcAddress(hMono, "il2cpp_assembly_open");
				mono_image_open = (MONO_IMAGE_OPEN)GetProcAddress(hMono, "il2cpp_image_open");


				il2cpp_domain_get_assemblies = (IL2CPP_DOMAIN_GET_ASSEMBLIES)GetProcAddress(hMono, "il2cpp_domain_get_assemblies");
				il2cpp_image_get_class_count = (IL2CPP_IMAGE_GET_CLASS_COUNT)GetProcAddress(hMono, "il2cpp_image_get_class_count");
				il2cpp_image_get_class = (IL2CPP_IMAGE_GET_CLASS)GetProcAddress(hMono, "il2cpp_image_get_class");

				il2cpp_type_get_name = (IL2CPP_TYPE_GET_NAME)GetProcAddress(hMono, "il2cpp_type_get_name");
				il2cpp_type_get_assembly_qualified_name = (IL2CPP_TYPE_GET_ASSEMBLY_QUALIFIED_NAME)GetProcAddress(hMono, "il2cpp_type_get_assembly_qualified_name");

				il2cpp_method_get_param_count = (IL2CPP_METHOD_GET_PARAM_COUNT)GetProcAddress(hMono, "il2cpp_method_get_param_count");
				il2cpp_method_get_param_name = (IL2CPP_METHOD_GET_PARAM_NAME)GetProcAddress(hMono, "il2cpp_method_get_param_name");
				il2cpp_method_get_param = (IL2CPP_METHOD_GET_PARAM)GetProcAddress(hMono, "il2cpp_method_get_param");
				il2cpp_method_get_return_type = (IL2CPP_METHOD_GET_RETURN_TYPE)GetProcAddress(hMono, "il2cpp_method_get_return_type");

				il2cpp_class_from_type = (IL2CPP_CLASS_FROM_TYPE)GetProcAddress(hMono, "il2cpp_class_from_type");
				il2cpp_string_chars = (IL2CPP_STRING_CHARS)GetProcAddress(hMono, "il2cpp_string_chars");


				mono_selfthread = mono_thread_attach(mono_domain_get());
			}
			else
			{
				g_free = (G_FREE)GetProcAddress(hMono, "g_free");

				if (!g_free)
					g_free = (G_FREE)GetProcAddress(hMono, "mono_unity_g_free");

				if (!g_free)
					g_free = customfreeimplementation;

				mono_free = (MONO_FREE)GetProcAddress(hMono, "mono_free");

				mono_get_root_domain = (MONO_GET_ROOT_DOMAIN)GetProcAddress(hMono, "mono_get_root_domain");
				mono_thread_attach = (MONO_THREAD_ATTACH)GetProcAddress(hMono, "mono_thread_attach");
				mono_thread_detach = (MONO_THREAD_DETACH)GetProcAddress(hMono, "mono_thread_detach");

				mono_object_get_class = (MONO_OBJECT_GET_CLASS)GetProcAddress(hMono, "mono_object_get_class");

				mono_domain_foreach = (MONO_DOMAIN_FOREACH)GetProcAddress(hMono, "mono_domain_foreach");
				mono_domain_set = (MONO_DOMAIN_SET)GetProcAddress(hMono, "mono_domain_set");
				mono_domain_get = (MONO_DOMAIN_GET)GetProcAddress(hMono, "mono_domain_get");
				mono_assembly_foreach = (MONO_ASSEMBLY_FOREACH)GetProcAddress(hMono, "mono_assembly_foreach");
				mono_assembly_get_image = (MONO_ASSEMBLY_GET_IMAGE)GetProcAddress(hMono, "mono_assembly_get_image");
				mono_image_get_assembly = (MONO_IMAGE_GET_ASSEMBLY)GetProcAddress(hMono, "mono_image_get_assembly");

				mono_image_get_name = (MONO_IMAGE_GET_NAME)GetProcAddress(hMono, "mono_image_get_name");
				mono_image_get_table_info = (MONO_IMAGE_GET_TABLE_INFO)GetProcAddress(hMono, "mono_image_get_table_info");
				mono_image_rva_map = (MONO_IMAGE_RVA_MAP)GetProcAddress(hMono, "mono_image_rva_map");

				mono_table_info_get_rows = (MONO_TABLE_INFO_GET_ROWS)GetProcAddress(hMono, "mono_table_info_get_rows");
				mono_metadata_decode_row_col = (MONO_METADATA_DECODE_ROW_COL)GetProcAddress(hMono, "mono_metadata_decode_row_col");
				mono_metadata_string_heap = (MONO_METADATA_STRING_HEAP)GetProcAddress(hMono, "mono_metadata_string_heap");


				mono_class_get = (MONO_CLASS_GET)GetProcAddress(hMono, "mono_class_get");
				mono_class_from_typeref = (MONO_CLASS_FROM_TYPEREF)GetProcAddress(hMono, "mono_class_from_typeref");
				mono_class_name_from_token = (MONO_CLASS_NAME_FROM_TOKEN)GetProcAddress(hMono, "mono_class_name_from_token");
				mono_class_from_name_case = (MONO_CLASS_FROM_NAME_CASE)GetProcAddress(hMono, "mono_class_from_name_case");
				mono_class_from_name = (MONO_CLASS_FROM_NAME_CASE)GetProcAddress(hMono, "mono_class_from_name");
				mono_class_get_name = (MONO_CLASS_GET_NAME)GetProcAddress(hMono, "mono_class_get_name");
				mono_class_get_namespace = (MONO_CLASS_GET_NAMESPACE)GetProcAddress(hMono, "mono_class_get_namespace");
				mono_class_get_methods = (MONO_CLASS_GET_METHODS)GetProcAddress(hMono, "mono_class_get_methods");
				mono_class_get_method_from_name = (MONO_CLASS_GET_METHOD_FROM_NAME)GetProcAddress(hMono, "mono_class_get_method_from_name");
				mono_class_get_fields = (MONO_CLASS_GET_FIELDS)GetProcAddress(hMono, "mono_class_get_fields");
				mono_class_get_parent = (MONO_CLASS_GET_PARENT)GetProcAddress(hMono, "mono_class_get_parent");
				mono_class_is_generic = (MONO_CLASS_IS_GENERIC)GetProcAddress(hMono, "mono_class_is_generic");
				mono_class_vtable = (MONO_CLASS_VTABLE)GetProcAddress(hMono, "mono_class_vtable");
				mono_class_from_mono_type = (MONO_CLASS_FROM_MONO_TYPE)GetProcAddress(hMono, "mono_class_from_mono_type");
				mono_class_get_element_class = (MONO_CLASS_GET_ELEMENT_CLASS)GetProcAddress(hMono, "mono_class_get_element_class");

				mono_class_num_fields = (MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "mono_class_num_fields");
				mono_class_num_methods = (MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "mono_class_num_methods");


				mono_field_get_name = (MONO_FIELD_GET_NAME)GetProcAddress(hMono, "mono_field_get_name");
				mono_field_get_type = (MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "mono_field_get_type");
				mono_field_get_parent = (MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "mono_field_get_parent");
				mono_field_get_offset = (MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "mono_field_get_offset");
				mono_field_get_flags = (MONO_FIELD_GET_FLAGS)GetProcAddress(hMono, "mono_field_get_flags");

				mono_type_get_name = (MONO_TYPE_GET_NAME)GetProcAddress(hMono, "mono_type_get_name");
				mono_type_get_type = (MONO_TYPE_GET_TYPE)GetProcAddress(hMono, "mono_type_get_type");
				mono_type_get_name_full = (MONO_TYPE_GET_NAME_FULL)GetProcAddress(hMono, "mono_type_get_name_full");

				mono_method_get_name = (MONO_METHOD_GET_NAME)GetProcAddress(hMono, "mono_method_get_name");
				mono_method_get_class = (MONO_METHOD_GET_CLASS)GetProcAddress(hMono, "mono_method_get_class");
				mono_method_get_header = (MONO_METHOD_GET_HEADER)GetProcAddress(hMono, "mono_method_get_header");
				mono_method_signature = (MONO_METHOD_SIG)GetProcAddress(hMono, "mono_method_signature");
				mono_method_get_param_names = (MONO_METHOD_GET_PARAM_NAMES)GetProcAddress(hMono, "mono_method_get_param_names");



				mono_signature_get_desc = (MONO_SIGNATURE_GET_DESC)GetProcAddress(hMono, "mono_signature_get_desc");
				mono_signature_get_params = (MONO_SIGNATURE_GET_PARAMS)GetProcAddress(hMono, "mono_signature_get_params");
				mono_signature_get_param_count = (MONO_SIGNATURE_GET_PARAM_COUNT)GetProcAddress(hMono, "mono_signature_get_param_count");
				mono_signature_get_return_type = (MONO_SIGNATURE_GET_RETURN_TYPE)GetProcAddress(hMono, "mono_signature_get_return_type");



				mono_compile_method = (MONO_COMPILE_METHOD)GetProcAddress(hMono, "mono_compile_method");
				mono_free_method = (MONO_FREE_METHOD)GetProcAddress(hMono, "mono_free_method");
				mono_jit_info_table_find = (MONO_JIT_INFO_TABLE_FIND)GetProcAddress(hMono, "mono_jit_info_table_find");
				mono_jit_info_get_method = (MONO_JIT_INFO_GET_METHOD)GetProcAddress(hMono, "mono_jit_info_get_method");
				mono_jit_info_get_code_start = (MONO_JIT_INFO_GET_CODE_START)GetProcAddress(hMono, "mono_jit_info_get_code_start");
				mono_jit_info_get_code_size = (MONO_JIT_INFO_GET_CODE_SIZE)GetProcAddress(hMono, "mono_jit_info_get_code_size");
				mono_jit_exec = (MONO_JIT_EXEC)GetProcAddress(hMono, "mono_jit_exec");

				mono_method_header_get_code = (MONO_METHOD_HEADER_GET_CODE)GetProcAddress(hMono, "mono_method_header_get_code");
				mono_disasm_code = (MONO_DISASM_CODE)GetProcAddress(hMono, "mono_disasm_code");

				mono_vtable_get_static_field_data = (MONO_VTABLE_GET_STATIC_FIELD_DATA)GetProcAddress(hMono, "mono_vtable_get_static_field_data");

				mono_method_desc_new = (MONO_METHOD_DESC_NEW)GetProcAddress(hMono, "mono_method_desc_new");;
				mono_method_desc_from_method = (MONO_METHOD_DESC_FROM_METHOD)GetProcAddress(hMono, "mono_method_desc_from_method");;
				mono_method_desc_free = (MONO_METHOD_DESC_FREE)GetProcAddress(hMono, "mono_method_desc_free");;

				mono_string_new = (MONO_STRING_NEW)GetProcAddress(hMono, "mono_string_new");
				mono_string_to_utf8 = (MONO_STRING_TO_UTF8)GetProcAddress(hMono, "mono_string_to_utf8");
				mono_array_new = (MONO_ARRAY_NEW)GetProcAddress(hMono, "mono_array_new");
				mono_value_box = (MONO_VALUE_BOX)GetProcAddress(hMono, "mono_value_box");
				mono_object_unbox = (MONO_OBJECT_UNBOX)GetProcAddress(hMono, "mono_object_unbox");
				mono_object_new = (MONO_OBJECT_NEW)GetProcAddress(hMono, "mono_object_new");

				mono_class_get_type = (MONO_CLASS_GET_TYPE)GetProcAddress(hMono, "mono_class_get_type");

				mono_method_desc_search_in_image = (MONO_METHOD_DESC_SEARCH_IN_IMAGE)GetProcAddress(hMono, "mono_method_desc_search_in_image");
				mono_runtime_invoke = (MONO_RUNTIME_INVOKE)GetProcAddress(hMono, "mono_runtime_invoke");
				mono_runtime_object_init = (MONO_RUNTIME_OBJECT_INIT)GetProcAddress(hMono, "mono_runtime_object_init");


				mono_assembly_name_new = (MONO_ASSEMBLY_NAME_NEW)GetProcAddress(hMono, "mono_assembly_name_new");
				mono_assembly_loaded = (MONO_ASSEMBLY_LOADED)GetProcAddress(hMono, "mono_assembly_loaded");
				mono_assembly_open = (MONO_ASSEMBLY_OPEN)GetProcAddress(hMono, "mono_assembly_open");
				mono_image_open = (MONO_IMAGE_OPEN)GetProcAddress(hMono, "mono_image_open");

				/*
				if (mono_get_root_domain == NULL) OutputDebugStringA("mono_get_root_domain not assigned");
				if (mono_thread_attach == NULL) OutputDebugStringA("mono_thread_attach not assigned");
				if (mono_object_get_class == NULL) OutputDebugStringA("mono_object_get_class not assigned");
				if (mono_class_get_name == NULL) OutputDebugStringA("mono_class_get_name not assigned");
				if (mono_domain_foreach == NULL) OutputDebugStringA("mono_domain_foreach not assigned");
				if (mono_domain_set == NULL) OutputDebugStringA("mono_domain_set not assigned");
				if (mono_assembly_foreach == NULL) OutputDebugStringA("mono_assembly_foreach not assigned");
				if (mono_assembly_get_image == NULL) OutputDebugStringA("mono_assembly_get_image not assigned");
				if (mono_image_get_name == NULL) OutputDebugStringA("mono_image_get_name not assigned");
				*/

				mono_selfthread = mono_thread_attach(mono_get_root_domain());
			}
			attached = TRUE;
			
		}
		//else
		//	OutputDebugStringA("Already attached");
	}
}

void CPipeServer::Object_New()
{
	void *domain;
	if (mono_get_root_domain)
		domain = (void *)mono_get_root_domain();
	else
		domain = (void *)mono_domain_get();

	void *klass = (void *)ReadQword();
	void *object=mono_object_new(domain, klass);
	WriteQword((UINT64)object);
}

void CPipeServer::Object_Init()
{
	void *object = (void *)ReadQword();
	try
	{
		mono_runtime_object_init(object);
		WriteByte(1);
	}
	catch (char *e)
	{
		WriteByte(0);
		//OutputDebugStringA("Error initializing object:\n");
		//OutputDebugStringA(e);
	}
	

}

void CPipeServer::Object_GetClass()
{
	void *object = (void *)ReadQword();
	char *classname;
	void *klass;

	//OutputDebugStringA("MONOCMD_OBJECT_GETCLASS");


	//ExpectingAccessViolations = TRUE; //cause access violations to throw an exception
	try
	{
		unsigned int i;

		klass = mono_object_get_class(object);
		classname = mono_class_get_name(klass);

		//small test to see if the classname is readable
		for (i = 0; i < strlen(classname); i++)
		{
			char x = classname[i];
			if (x == '\0')
				break;
		}

		if (klass != 0)
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
		//OutputDebugStringA("Error getting the class:\n");
		//OutputDebugStringA(e);
		WriteQword(0); //failure. Invalid object
	}

	//ExpectingAccessViolations = FALSE; //back to normal behaviour
}

void _cdecl DomainEnumerator(void *domain, std::vector<UINT64> *v)
{
	v->push_back((UINT_PTR)domain);
}

void CPipeServer::EnumDomains(void)
{
	unsigned int i;
	std::vector<UINT64> v;

	if (il2cpp)
	{
		WriteDword(1);
		WriteQword(UINT_PTR(mono_domain_get()));
	}
	else
	{
		//OutputDebugStringA("EnumDomains");
		mono_domain_foreach((MonoDomainFunc)DomainEnumerator, &v);


		WriteDword(v.size());
		for (i = 0; i < v.size(); i++)
			WriteQword(v[i]);
	}
}

void CPipeServer::SetCurrentDomain(void)
{
	void *domain = (void *)ReadQword();
	int r;
	if (mono_domain_set)
		r = mono_domain_set(domain, FALSE);
	else
		r = 0;

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

	if (il2cpp)
	{
		int i;
		
		SIZE_T nrofassemblies=0;	
		UINT_PTR *assemblies;
		assemblies = il2cpp_domain_get_assemblies(mono_domain_get(), &nrofassemblies);

		WriteDword(nrofassemblies);
		for (i = 0; i < nrofassemblies; i++)
			WriteQword(assemblies[i]);

	}
	else
	{
		if (mono_assembly_foreach)
		{
			mono_assembly_foreach((GFunc)AssemblyEnumerator, &v);

			WriteDword(v.size());
			for (i = 0; i < v.size(); i++)
				WriteQword(v[i]);
		}
		else
		{
			WriteDword(0);
		}
	}
}

void CPipeServer::GetImageFromAssembly()
{
	void *assembly = (void *)ReadQword();
	void *image = mono_assembly_get_image(assembly);
	WriteQword((UINT_PTR)image);
}

void CPipeServer::GetImageName()
{
	void *image = (void *)ReadQword();
	char *s = mono_image_get_name(image);

	WriteWord(strlen(s));
	Write(s, strlen(s));
}

#include <locale> 
#include <codecvt> 
WORD UTF8TOUTF16(char* szUtf8) {

	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
	std::u16string dest = convert.from_bytes(szUtf8);
	return *(WORD*)&dest[0];
}
void CPipeServer::EnumClassesInImage()
{
	int i;
	void *image = (void *)ReadQword();
	if (image == NULL)
	{
		WriteDword(0);
		return;
	}

	if (il2cpp)
	{
		int count = 0;
		if (il2cpp_image_get_class_count)
		{
			count = il2cpp_image_get_class_count(image);
		}
		/*
		else
		{
			count = *(DWORD*)(((UINT_PTR)image) + 0x1c);
		}*/
		
		WriteDword(count);


		for (i = 0; i < count; i++)
		{	
			if (il2cpp_image_get_class)
			{
				void* c = il2cpp_image_get_class(image, i);
				WriteQword((UINT_PTR)c);

				if (c)
				{
					char *name = mono_class_get_name(c);
					WriteString(name);

					name = mono_class_get_namespace(c);
					WriteString(name);
				}
			}
			else
				WriteQword((UINT_PTR)0);

		}

	}
	else
	{

		void *tdef = mono_image_get_table_info(image, MONO_TABLE_TYPEDEF);
		if (tdef)
		{
			int tdefcount = mono_table_info_get_rows(tdef);
			WriteDword(tdefcount);

			for (i = 0; i < tdefcount; i++)
			{
				void *c = mono_class_get(image, MONO_TOKEN_TYPE_DEF | i + 1);
				if (c != NULL)
				{
					char *name = mono_class_get_name(c);

				WriteQword((UINT_PTR)c);


				std::string sName = std::string(name);

				if ((BYTE)name[0] == 0xEE) {
					char szUeName[32];
					sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(name));
					sName = szUeName;
				}

				if (c)
				{
					WriteWord(sName.size());
					Write((PVOID)sName.c_str(), sName.size());
				}
				else
					WriteWord(0);

				name = mono_class_get_namespace(c);
				if (name)
				{
					WriteWord(strlen(name));
					Write(name, strlen(name));
				}
				else
					WriteWord(0);
				}
				else
					WriteQword(0);
			}
		}
		else
		{
			WriteDword(0);
		}
	}
	

	/*
	void *tdef = mono_image_get_table_info(image, MONO_TABLE_TYPEREF);
	if (tdef)
	{
		int tdefcount = mono_table_info_get_rows(tdef);
		WriteDword(tdefcount);

		for (i = 0; i < tdefcount; i++)
		{
			void *c;
			char *name = mono_class_name_from_token(image, MONO_TOKEN_TYPE_REF | (i + 1));
			
			c = mono_class_from_typeref(image, MONO_TOKEN_TYPE_REF | (i + 1));
			if (c != NULL)
			{
				char *name = mono_class_get_name(c);

				WriteQword((UINT_PTR)c);

				if (c)
				{
					WriteWord(strlen(name));
					Write(name, strlen(name));
				}
				else
					WriteWord(0);

				name = mono_class_get_namespace(c);
				if (name)
				{
					WriteWord(strlen(name));
					Write(name, strlen(name));
				}
				else
					WriteWord(0);
			}
			else
				WriteQword(0);
		}
	}
	else
	{
		WriteDword(0);
	}
	*/
}

void CPipeServer::EnumFieldsInClass()
{
	void *c = (void *)ReadQword();
	void *iter = NULL;
	void *field;

	do
	{
		field = mono_class_get_fields(c, &iter);
		WriteQword((UINT_PTR)field);

		if (field)
		{
			void *fieldtype = mono_field_get_type(field);
			WriteQword((UINT_PTR)fieldtype);
			WriteDword(mono_type_get_type(fieldtype));
			WriteQword((UINT_PTR)mono_field_get_parent(field));
			WriteDword((UINT_PTR)mono_field_get_offset(field));
			WriteDword(mono_field_get_flags(field));



			char* name = mono_field_get_name(field);
			char* type = mono_type_get_name(fieldtype);
			std::string sName = std::string(name);
			std::string sType = std::string(type);
			//check if name is x ...
			char szUeName[32];
			if ((BYTE)name[0] == 0xEE) {
				char szUeName[32];
				sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(name));
				sName = szUeName;
			}
			if ((BYTE)type[0] == 0xEE) {
				char szUeName[32];
				sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(type));
				sType = szUeName;
			}

			WriteWord(sName.size());
			Write((LPVOID)sName.c_str(), sName.size());

			if (type)
			{
				WriteWord(sType.size());
				Write((LPVOID)sType.c_str(), sType.size());
				//g_free(name);
			}
			else
				WriteWord(0);

		}
	} while (field);
}


void CPipeServer::EnumMethodsInClass()
{
	void *c = (void *)ReadQword();
	void *iter = NULL;
	void *method;

	do
	{
		method = mono_class_get_methods(c, &iter);
		WriteQword((UINT_PTR)method);

		if (method)
		{
			char *name;

			name = mono_method_get_name(method);

			std::string sName = std::string(name);

			if ((BYTE)name[0] == 0xEE) {
				char szUeName[32];
				sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(name));
				sName = szUeName;
			}

				WriteWord(sName.size());
				Write((PVOID)sName.c_str(), sName.size());
		}
	} while (method);

}

void CPipeServer::CompileMethod()
{	
	void *method = (void *)ReadQword();	
	

	if (il2cpp)
	{
		WriteQword(*(PUINT_PTR)method); //fist pointer points to compiled code
	}
	else
	{
		void *result = NULL;
		try
		{
			void *klass = mono_method_get_class(method);
			if (klass)
			{
				if (mono_class_is_generic(klass) == 0)
				{
					result = mono_compile_method(method);
				}
				//else
				//	OutputDebugString(L"This is a generic class which is currently not implemented. Skipping");
			}
		}
		catch (...)
		{
			result = NULL;
		}
		WriteQword((UINT_PTR)result);
	}
	
	
	
	
}

void CPipeServer::GetMethodHeader()
{
	if (mono_method_get_header)
	{
		void *method = (void *)ReadQword();
		void *result = mono_method_get_header(method);
		WriteQword((UINT_PTR)result);
	}
	else
		WriteQword(0);
}

void CPipeServer::GetILCode()
{
	if (il2cpp)
	{
		WriteQword(0);
		WriteDword(0);
	}
	else
	{
		void *methodheader = (void *)ReadQword();
		UINT32 code;
		void *result = mono_method_header_get_code(methodheader, &code, NULL);
		WriteQword((UINT_PTR)result);
		WriteDword(code);
	}
}

void CPipeServer::RvaMap()
{
	if (mono_image_rva_map)
	{
		void *image = (void *)ReadQword();
		UINT32 offset = ReadDword();
		void *result = mono_image_rva_map(image, offset);

		WriteQword((UINT_PTR)result);
	}
	else
		WriteQword(0);

}

void CPipeServer::GetJitInfo()
{
	if (il2cpp)
	{
		//not available, use the method enum function for il2cpp instead
		void *domain = (void *)ReadQword();
		void *address = (void *)ReadQword();
		WriteQword(0);
	}
	else
	{
		void *domain = (void *)ReadQword();
		if (domain == NULL)
			domain = (void *)mono_get_root_domain();
		void *address = (void *)ReadQword();
		void *jitinfo = mono_jit_info_table_find(domain, address);
		WriteQword((UINT_PTR)jitinfo);
		if (jitinfo)
		{
			WriteQword((UINT_PTR)mono_jit_info_get_method(jitinfo));
			WriteQword((UINT_PTR)mono_jit_info_get_code_start(jitinfo));
			WriteDword((UINT_PTR)mono_jit_info_get_code_size(jitinfo));
		}
	}
}

void CPipeServer::FindClass()
{
	void *image = (void *)ReadQword();
	WORD length = ReadWord();
	char *cn = NULL;
	char *ns = NULL;
	void *klass;

	cn = (char *)malloc(length + 1);
	if (length)
		Read(cn, length);

	cn[length] = 0;

	length = ReadWord();

	ns = (char *)malloc(length + 1);
	if (length)
		Read(ns, length);

	ns[length] = 0;

	if (mono_class_from_name_case)
		klass = mono_class_from_name_case(image, ns, cn);
	else
		klass = mono_class_from_name(image, ns, cn);

	if (cn)
		free(cn);

	if (ns)
		free(ns);

	WriteQword((UINT_PTR)klass);

}

void CPipeServer::FindMethod()
{
	void *klass = (void *)ReadQword();
	WORD length = ReadWord();
	char *methodname = (char *)malloc(length + 1);
	void *method = NULL;
	if (length)
		Read(methodname, length);
	methodname[length] = 0;


	method = mono_class_get_method_from_name(klass, methodname, -1);
	WriteQword((UINT_PTR)method);
}

void CPipeServer::GetMethodName()
{
	void *method = (void *)ReadQword();
	char *methodname = mono_method_get_name(method);

	WriteWord(strlen(methodname));
	Write(methodname, strlen(methodname));
}

void CPipeServer::GetMethodClass()
{
	void *method = (void *)ReadQword();
	void *result = mono_method_get_class(method);
	WriteQword((UINT_PTR)result);
}


void CPipeServer::GetKlassName()
{
	void *klass = (void *)ReadQword();
	char *methodname = mono_class_get_name(klass);

	std::string sName = std::string(methodname);

	if ((BYTE)methodname[0] == 0xEE) {
		char szUeName[32];
		sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(methodname));
		sName = szUeName;
	}

		WriteWord(sName.size());
		Write((PVOID)sName.c_str(), sName.size());
}

void CPipeServer::GetClassNamespace()
{
	void *klass = (void *)ReadQword();
	char *methodname = mono_class_get_namespace(klass);

	WriteWord(strlen(methodname));
	Write(methodname, strlen(methodname));
}

void CPipeServer::FreeMethod()
{
	if (mono_free_method)
		mono_free_method((void *)ReadQword());
}

void CPipeServer::DisassembleMethod()
{	
	void *method = (void *)ReadQword();
	if (il2cpp)
	{
		void *methodheader = mono_method_get_header(method);
		UINT32 codesize, maxstack;
		void *ilcode = mono_method_header_get_code(methodheader, &codesize, &maxstack);
		char *disassembly = mono_disasm_code(NULL, method, ilcode, (void *)((UINT_PTR)ilcode + codesize));

		WriteWord(strlen(disassembly));
		Write(disassembly, strlen(disassembly));
		g_free(disassembly);
	}
}

void CPipeServer::GetMethodParameters()
{
	void *method = (void *)ReadQword();
	int i;

	if (il2cpp)
	{
		int paramcount = il2cpp_method_get_param_count(method);
		WriteByte(paramcount);
		for (i = 0; i < paramcount; i++)
		{
			char *paramname = il2cpp_method_get_param_name(method, i);
			WriteString1(paramname);
		}

		if (paramcount)
		{
			void *paramtype = il2cpp_method_get_param(method, i);
			
			if (paramtype)
				WriteDword(mono_type_get_type(paramtype));
			else
				WriteDword(0);
		}

		{
			void *returntype = il2cpp_method_get_return_type(method);
			if (returntype)
				WriteDword(mono_type_get_type(returntype));
			else
				WriteDword(0);
		}

	}
	else
	{

		void *methodsignature = mono_method_signature(method);
		

		if (methodsignature)
		{
			int paramcount = mono_signature_get_param_count(methodsignature);
			char **names = (char **)calloc(sizeof(char *), paramcount);
			mono_method_get_param_names(method, (const char **)names);
			WriteByte(paramcount);
			for (i = 0; i < paramcount; i++)
			{
				if (names[i])
				{
					WriteByte(strlen(names[i]));
					Write(names[i], strlen(names[i]));
				}
				else
					WriteByte(0);
			}

			if (paramcount)
			{
				gpointer iter = NULL;
				MonoType *paramtype = mono_signature_get_params((MonoMethodSignature*)methodsignature, &iter);

				if (paramtype)
					WriteDword(mono_type_get_type(paramtype));
				else
					WriteDword(0);
			}

			{
				MonoType *returntype = mono_signature_get_return_type(methodsignature);
				if (returntype)
					WriteDword(mono_type_get_type(returntype));
				else
					WriteDword(0);
			}






		}
		else
			WriteByte(0);
	}
}

void CPipeServer::GetMethodSignature()
{
	void *method = (void *)ReadQword();
	int i;

	if (il2cpp)
	{
		int paramcount = il2cpp_method_get_param_count(method);
		char *name;
		void* type;

		WriteByte(paramcount);

		for (i = 0; i < paramcount; i++)
		{
			name=il2cpp_method_get_param_name(method, i);
			WriteString1(name);				
		}

		for (i = 0; i < paramcount; i++)
		{
			type=il2cpp_method_get_param(method, i);
			name=il2cpp_type_get_name(type);
			WriteString(name);			
		}
			

		type = il2cpp_method_get_return_type(method);
		name = il2cpp_type_get_name(type);
		WriteString1(name);

	}
	else
	{

		void *methodsignature = mono_method_signature(method);
		char *sig = mono_signature_get_desc(methodsignature, TRUE);
		int paramcount = mono_signature_get_param_count(methodsignature);

		

		WriteByte(paramcount);

		if (paramcount)
		{
			char **names = (char **)calloc(sizeof(char *), paramcount);

			mono_method_get_param_names(method, (const char **)names);

			for (i = 0; i < paramcount; i++)
			{
				if (names[i])
				{
					WriteByte(strlen(names[i]));
					Write(names[i], strlen(names[i]));
				}
				else
					WriteByte(0);
			}
			free(names);
		}



		WriteWord(strlen(sig));
		Write(sig, strlen(sig));
		g_free(sig);

		//12/5/2014:send the returntype as well
		void *returntype = mono_signature_get_return_type(methodsignature);

		if (returntype)
		{
			char *tname = mono_type_get_name(returntype);
			if (tname)
			{
				WriteByte(strlen(tname));
				Write(tname, strlen(tname));
				g_free(tname);
			}
			else
				WriteByte(0);
		}
		else
			WriteByte(0);
	}
}

void CPipeServer::GetParentClass(void)
{
	void *klass = (void *)ReadQword();
	UINT_PTR parent = (UINT_PTR)mono_class_get_parent(klass);

	WriteQword(parent);
}

void CPipeServer::GetVTableFromClass(void)
{
	void *domain = (void *)ReadQword();
	void *klass = (void *)ReadQword();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{		
		if (domain == NULL)
			domain = (void *)mono_get_root_domain();
		
		void *vtable = (domain && klass) ? mono_class_vtable(domain, klass) : NULL;

		WriteQword((UINT_PTR)vtable);
	}

}

void CPipeServer::GetStaticFieldAddressFromClass(void)
{
	void *domain = (void *)ReadQword();
	void *klass = (void *)ReadQword();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{
		if (domain == NULL)
			domain = (void *)mono_get_root_domain();
		
		void *vtable = (domain && klass) ? mono_class_vtable(domain, klass) : NULL;
		if (vtable)
		{
			void *staticdata = mono_vtable_get_static_field_data(vtable);
			WriteQword((UINT_PTR)staticdata);
		}
		else
			WriteQword(0);
	}
}

void CPipeServer::GetTypeClass(void)
{
	void *field = (void *)ReadQword();  // TODO: this should be monotype but EnumFieldsInClass effectively returns fieldtype ptr
	void *type = field ? mono_field_get_type(field) : NULL;
	void *klass;

	if (il2cpp)		
		klass = type ? il2cpp_class_from_type(type) : NULL;
	else
		klass = type ? mono_class_from_mono_type(type) : NULL;

	WriteQword((UINT_PTR)klass);
}

void CPipeServer::GetArrayElementClass(void)
{
	void *klass = (void *)ReadQword();
	void *eklass = klass ? mono_class_get_element_class(klass) : NULL;
	WriteQword((UINT_PTR)eklass);
}

void CPipeServer::FindMethodByDesc(void)
{
	void *image = (void *)ReadQword();
	char *fqMethodName = ReadString();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{	
		void *mmd = fqMethodName ? mono_method_desc_new(fqMethodName, 1) : NULL;
		void *method = mmd && image ? mono_method_desc_search_in_image(mmd, image) : NULL;
		WriteQword((UINT_PTR)method);
		FreeString(fqMethodName);
	}
}

void *CPipeServer::ReadObjectArray(void *domain)
{
	int nargs = ReadWord();
	if (nargs == 0)
		return NULL;

	// Calc default block size
	char *types = (char*)calloc(nargs, sizeof(char));
	Read(types, nargs);
	int data_size = 0;
	for (int i = 0; i < nargs; ++i) {
		int size = GetObjectSize(types[i]);
		if (size > 0) data_size += size;
	}

	// array is allocated as size, ptr array, type array, data segment
	void *arr = (char*)calloc(sizeof(int) + nargs * sizeof(void*) + nargs * sizeof(char) + data_size, 1);
	char *ptr = (char*)arr;
	*(int*)ptr = nargs;
	ptr += sizeof(int);
	
	void **args = (void**)(ptr);
	ptr += nargs * sizeof(void*);
	
	// copy types so free can be properly executed
	memcpy(ptr, types, nargs * sizeof(char));

	// fill in pointer array using data segment
	ptr += nargs * sizeof(char);
	for (int i = 0; i < nargs; ++i) {
		int size = GetObjectSize(types[i]);
		args[i] = ReadObject(domain, (MonoTypeEnum)types[i], (void*)ptr);
		if (size > 0) ptr += size;
	}
	free(types);
	return arr;
}

void CPipeServer::FreeObjectArray(void * arr)
{
	if (arr) {
		int nargs = *(int*)arr;
		void **args = (void**)((char*)arr + sizeof(int));
		char *types = (char*)args + nargs * sizeof(void*);
		for (int i = 0; i < nargs; ++i)
		{
			switch (types[i])
			{
			case MONO_TYPE_ARRAY:
				FreeObjectArray(args[i]);
				break;
			}
		}
		free(arr);
	}
}

int CPipeServer::GetObjectSize(int type)
{
	switch (type) {
	case MONO_TYPE_BOOLEAN:
	case MONO_TYPE_I1:
	case MONO_TYPE_U1:
		return sizeof(char);
	case MONO_TYPE_I2:
	case MONO_TYPE_U2:
	case MONO_TYPE_CHAR:
		return sizeof(short);
	case MONO_TYPE_I4:
	case MONO_TYPE_U4:
		return sizeof(long);
	case MONO_TYPE_I:
	case MONO_TYPE_U:
	case MONO_TYPE_PTR:
	case MONO_TYPE_BYREF:
	case MONO_TYPE_CLASS:
	case MONO_TYPE_FNPTR:
	case MONO_TYPE_GENERICINST:
	case MONO_TYPE_ARRAY:
	case MONO_TYPE_SZARRAY:
		return sizeof(long long);
	case MONO_TYPE_I8:
	case MONO_TYPE_U8:
		return sizeof(long long);
	case MONO_TYPE_R4:
		return sizeof(float);
	case MONO_TYPE_R8:
		return sizeof(double);
	}
	return -1;
}

void* CPipeServer::ReadObject(void* domain, MonoTypeEnum type, void *addr)
{
	//int type = ReadWord();
	int size = GetObjectSize(type);
	void *result = addr;
	switch (type)
	{
	case MONO_TYPE_STRING:
		{
			char* ptr = ReadString();
			result = mono_string_new(domain, ptr);
			FreeString(ptr);
		} break;
	case MONO_TYPE_I1:
	case MONO_TYPE_U1:
	case MONO_TYPE_BOOLEAN:
	case MONO_TYPE_I2:
	case MONO_TYPE_U2:
	case MONO_TYPE_CHAR:
	case MONO_TYPE_I4:
	case MONO_TYPE_U4:
	case MONO_TYPE_I:
	case MONO_TYPE_U:
	case MONO_TYPE_I8:
	case MONO_TYPE_U8:
	case MONO_TYPE_R4:
	case MONO_TYPE_R8:
		if (size > 0)
			Read(addr, size);
		break;
	case MONO_TYPE_PTR:
	{
		if (size > 0)
		{
			Read(addr, size);
			result = (void *) *((UINT64*)addr);			
		}
		break;
	}
	case MONO_TYPE_BYREF:
	case MONO_TYPE_CLASS:
	case MONO_TYPE_FNPTR:
	case MONO_TYPE_GENERICINST:
	case MONO_TYPE_ARRAY:
	case MONO_TYPE_SZARRAY:
		if (size > 0)
		{
			Read(addr, size);
		}
		break;
	}
	return result;
}

void CPipeServer::WriteObject(void* object)
{
	if (object)
	{
		void *klass = mono_object_get_class(object);
		void *type = klass ? mono_class_get_type(klass) : NULL;
		int datatype = type ? mono_type_get_type(type) : MONO_TYPE_VOID;
		int size = GetObjectSize(datatype);
		WriteByte(datatype);
		switch (datatype)
		{
		case MONO_TYPE_STRING:
		{
			//void *string = mono_object_to_string(object, NULL);
			if (il2cpp)
			{
				wchar_t *ptr = il2cpp_string_chars(object);
				int l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, NULL, 0, NULL, NULL);
				char *c = (char *)malloc(l+1);
				l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, c, l, NULL, NULL);
				c[l] = 0;
				WriteString(c);
				free(c);
			}
			else
			{
				char *ptr = mono_string_to_utf8(object);
				WriteString(ptr);
				if (mono_free)
					mono_free(ptr);
				else
				{
					if (g_free)
						g_free(ptr);
				}
			}
		} break;
		case MONO_TYPE_I1:
		case MONO_TYPE_U1:
		case MONO_TYPE_BOOLEAN:
		case MONO_TYPE_I2:
		case MONO_TYPE_U2:
		case MONO_TYPE_CHAR:
		case MONO_TYPE_I4:
		case MONO_TYPE_U4:
		case MONO_TYPE_I:
		case MONO_TYPE_U:
		case MONO_TYPE_I8:
		case MONO_TYPE_U8:
		case MONO_TYPE_R4:
		case MONO_TYPE_R8:
			if (size > 0)
			{
				void *ptr = mono_object_unbox(object);
				Write(ptr, size);
			}
			break;
		case MONO_TYPE_PTR:
		case MONO_TYPE_BYREF:
		case MONO_TYPE_CLASS:
		case MONO_TYPE_FNPTR:
		case MONO_TYPE_GENERICINST:
		case MONO_TYPE_ARRAY:
		case MONO_TYPE_SZARRAY:
			if (size > 0)
			{
				long long objectBacking = (long long)object;
				Write(&objectBacking, size);
			}
			break;
		}
	}
	else
	{
		WriteByte(MONO_TYPE_VOID);
	}
}

int CPipeServer::GetObjectArraySize(void *arr)
{
	if (arr) return *(int*)arr;
	return 0;
}

void **CPipeServer::GetObjectArrayArgs(void *arr)
{
	if (arr) return (void **)((char*)arr + sizeof(int));
	return NULL;
}

void CPipeServer::InvokeMethod(void)
{
	// mono_get_root_domain
	void * result = NULL;
	void *domain = (void *)ReadQword();
	void *olddomain = NULL;

	if (mono_domain_get)
		olddomain = mono_domain_get();

	if ((!il2cpp) && (olddomain == NULL))
	{
		olddomain=mono_get_root_domain();
		mono_domain_set(mono_get_root_domain(), true);
	}

	if ((!il2cpp) && (domain == NULL))
	{
		domain = olddomain;

		if (domain == NULL)
			domain = (void *)mono_get_root_domain();
	}


	void *method = (void *)ReadQword();
	void *pThis = (void *)ReadQword();
	void *arr = ReadObjectArray(domain);
	if (domain && method)
	{
		int paramcount;

		if (il2cpp)
		{
			paramcount = il2cpp_method_get_param_count(method);
		}
		else
		{
			void *methodsignature = mono_method_signature(method);
			paramcount = methodsignature ? mono_signature_get_param_count(methodsignature) : 0;
		}
		
		
		int nargs = GetObjectArraySize(arr);
		void **args = GetObjectArrayArgs(arr);
		if (nargs == paramcount)
		{
			if ((!il2cpp) && (domain != olddomain))
				mono_domain_set(domain, FALSE);

			try
			{
				result = mono_runtime_invoke(method, pThis, args, NULL /* exception */);				
			}
			catch (...)
			{
				result = NULL;
			}
			

			if ((!il2cpp) && (domain != olddomain))
				mono_domain_set(olddomain, FALSE);
		}
	}
	FreeObjectArray(arr);
	WriteObject(result);
}

void CPipeServer::LoadAssemblyFromFile(void)
{
	char *imageName = ReadString();
	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{

		int status;
		void *domain = (void *)mono_get_root_domain();
		mono_domain_set(domain, FALSE);

		void *assembly = mono_assembly_open(imageName, &status);
		WriteQword((UINT_PTR)assembly);

		if (mono_jit_exec)
			mono_jit_exec(domain, assembly, 0, NULL);
	}
}

void CPipeServer::GetFullTypeName(void)
{
	//ExpectingAccessViolations = TRUE;

	try
	{
		void *klass = (void *)ReadQword();
		char isKlass = ReadByte();
		void *ptype = klass && isKlass ? mono_class_get_type(klass) : klass;
		int nameformat = ReadDword();

		if (ptype)
		{
			char *fullname;
			if (il2cpp)
				fullname=il2cpp_type_get_name(ptype);								
			else
				fullname = mono_type_get_name_full ? mono_type_get_name_full(ptype, nameformat) : NULL;

			if (fullname) 
			{
				std::string sName = std::string(fullname);

			if ((BYTE)fullname[0] == 0xEE) {
				char szUeName[32];
				auto plus = strchr(fullname, '+');
				if (plus) {
					sprintf_s(szUeName, 32, "\\u%04X+\\u%04X", UTF8TOUTF16(fullname), UTF8TOUTF16(plus+1));
					sName = szUeName;
				}
				else {
					sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(fullname));
					sName = szUeName;
				}
			}

			WriteWord(sName.size());
			Write((PVOID)sName.c_str(), sName.size());
			}
		}
		else
		{
			WriteWord(0);
		}
	}
	catch (...)
	{
		//OutputDebugStringA("GetFullTypeName exception\n");
		WriteWord(0);		
	}
	//ExpectingAccessViolations = FALSE;

		
}

void CPipeServer::IsGenericClass()
{
	void *klass = (void *)ReadQword();
	WriteByte(mono_class_is_generic(klass));
}

void CPipeServer::IsIL2CPP()
{
	WriteByte(il2cpp);
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
				command = ReadByte();

				ExpectingAccessViolations = TRUE;
				ExpectingAccessViolationsThread = GetCurrentThreadId();


				switch (command)
				{
				case MONOCMD_INITMONO:
					InitMono();
					break;

				case MONOCMD_OBJECT_GETCLASS:
					Object_GetClass();
					break;

				case MONOCMD_OBJECT_NEW:
					Object_New();
					break;

				case MONOCMD_OBJECT_INIT:
					Object_Init();
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
					GetKlassName();
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

				case MONOCMD_GETMETHODPARAMETERS:
					GetMethodParameters();
					break;

				case MONOCMD_GETMETHODSIGNATURE:
					GetMethodSignature();
					break;

				case MONOCMD_GETPARENTCLASS:
					GetParentClass();
					break;

				case MONOCMD_GETVTABLEFROMCLASS:
					GetVTableFromClass();
					break;

				case MONOCMD_GETSTATICFIELDADDRESSFROMCLASS:
					GetStaticFieldAddressFromClass();
					break;

				case MONOCMD_GETTYPECLASS:
					GetTypeClass();
					break;

				case MONOCMD_GETARRAYELEMENTCLASS:
					GetArrayElementClass();
					break;

				case MONOCMD_FINDMETHODBYDESC:
					FindMethodByDesc();
					break;

				case MONOCMD_INVOKEMETHOD:
					InvokeMethod();
					break;

				case MONOCMD_LOADASSEMBLY:
					LoadAssemblyFromFile();
					break;

				case MONOCMD_GETFULLTYPENAME:
					GetFullTypeName();
					break;

				case MONOCMD_ISCLASSGENERIC:
					IsGenericClass();
					break;

				case MONOCMD_ISIL2CPP:
					IsIL2CPP();
					break;
				}

				

				
				ExpectingAccessViolations = FALSE;
			}
		}
		catch (char *e)
		{
			//Pipe error, or something else that wasn't caught. Exit the connection and start over	
			//OutputDebugStringA("Pipe error:\n");
			//OutputDebugStringA(e);

			if (attached)
			{
				try
				{
					mono_thread_detach(mono_selfthread);
					attached = FALSE;
				}
				catch (...)
				{

				}
			}


		}
		catch (...)
		{
			//OutputDebugStringA("Unexpected pipe error\n");
			if (attached)
			{
				try
				{
					mono_thread_detach(mono_selfthread);
					attached = FALSE;
				}
				catch (...)
				{

				}
				
				
			}
		}

	}

}


