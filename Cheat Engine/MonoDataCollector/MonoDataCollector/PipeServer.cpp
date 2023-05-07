#ifdef _WINDOWS
#include "StdAfx.h"
#endif

#ifdef __APPLE__
#include "macport.h"
#endif

#include <setjmp.h>
#ifdef __linux__
#include <signal.h>
#include <sys/types.h>
#include <string.h>
#include <unistd.h>
#include <sys/syscall.h>

#if __GLIBC__ == 2 && __GLIBC_MINOR__ < 30
#define gettid() syscall(SYS_gettid)
#endif

#endif //linux



#include <signal.h>
#include <sys/types.h>

#include "PipeServer.h"
#include "CMemStream.h"

//todo: Make this multithreaded. So: Make a list of threads that can AV


BOOL ExpectingAccessViolations = FALSE;

#ifdef _WINDOWS
#pragma warning( disable : 4101)
HANDLE MDC_ServerPipe = 0;
DWORD ExpectingAccessViolationsThread = 0;
#else
uint64_t ExpectingAccessViolationsThread = 0;
#endif

typedef uint64_t QWORD;


jmp_buf onError;

void ErrorThrow(void)
{
	longjmp(onError, 1);
}


#ifdef _WINDOWS


int looper = 0;
LONG NTAPI ErrorFilter(struct _EXCEPTION_POINTERS* ExceptionInfo)
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
#else

struct sigaction old_sa[33], new_sa[33];

uint64_t ErrorRIP;
uint64_t ErrorRBP;
uint64_t ErrorRSP;


void ErrorFilter(int signr, siginfo_t* info, void* uap)
{
	char s[200];
	snprintf(s, 200, "Errorfilter %d", signr);
	OutputDebugString(s);
	ucontext_t* uap_c = (ucontext_t*)uap;
	//check if it's the serverthread, and if so, change the instruction pointer to ErrorThrow
	uint64_t tid;

#ifdef __APPLE__
	pthread_threadid_np(NULL, &tid);
#else
	tid = (uint64_t)gettid();
#endif

	if ((ExpectingAccessViolations) && (tid == ExpectingAccessViolationsThread))
	{
		OutputDebugString("It is this thread. Jumping to state saved in onError");

		longjmp(onError, 1);

	}
	else
	{
		OutputDebugString("calling original handler");

		if (old_sa[signr].sa_flags & SA_SIGINFO)
			old_sa[signr].sa_sigaction(signr, info, uap);
		else
			old_sa[signr].sa_handler(signr);
	}
}
#endif


CPipeServer::CPipeServer(void)
{
	attached = FALSE;
	limitedConnection = FALSE;
	il2cpp = FALSE;
	UWPMode = FALSE;
	mono_selfthread = NULL;
	mono_runtime_is_shutting_down = NULL;



#ifdef _WINDOWS
	swprintf(datapipename, 256, L"\\\\.\\pipe\\cemonodc_pid%d", GetCurrentProcessId());
	//swprintf(eventpipename, 256,L"\\\\.\\pipe\\cemonodc_pid%d_events", GetCurrentProcessId());

	AddVectoredExceptionHandler(1, ErrorFilter);
#else
	sprintf((char*)datapipename, "cemonodc_pid%d", GetCurrentProcessId());

	int i;
	for (i = 10; i < 12; i++)
	{
		new_sa[i].sa_sigaction = ErrorFilter;
		new_sa[i].sa_flags = SA_SIGINFO;
		sigemptyset(&new_sa[i].sa_mask);

		//sigaction(10, &new_sa, &old_sa);
		sigaction(i, &new_sa[i], &old_sa[i]);
	}

#endif
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
	char* result = (char*)malloc(length + 1);
	if (length)
		Read(result, length);
	result[length] = 0;
	return result;
}

void CPipeServer::WriteString1(const char* value) //for 1 byte length strings
{
	if (value)
	{
		int n = (int)strlen(value);
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
		int n = (int)strlen(value);
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
	//OutputDebugStringA((char*)"CreatePipeandWaitForconnect called\n");
#ifdef _WINDOWS

	//OutputDebugStringA((char*)"Closing pipe if needed\n");
	if ((pipehandle) && (pipehandle != INVALID_HANDLE_VALUE))
	{
		CloseHandle(pipehandle);
		pipehandle = 0;
	}

	if (!UWPMode)
	{
		//OutputDebugStringA("datapipename=\n");
		//OutputDebugStringW(datapipename);

		pipehandle = CreateNamedPipeW(datapipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1, 256 * 1024, 16, INFINITE, NULL);
		//eventpipe=CreateNamedPipe(eventpipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 0, INFINITE, NULL);

		if ((pipehandle) && (pipehandle != INVALID_HANDLE_VALUE))
		{
			//OutputDebugStringA("calling ConnectNamedPipe\n");

			ConnectNamedPipe(pipehandle, NULL);

			//OutputDebugStringA("after ConnectNamedPipeW\n");
		}
		else
		{
			//OutputDebugStringA("CreateNamedPipeW failed.  Likely in a UWP app. Switching to client mode and waiting for g_ClientPipe to be set\n");
			UWPMode = 1;
		}
	}

	if (UWPMode)
	{
		//OutputDebugStringA("UWPMode connection. Fetching the new serverpipe\n");

		MDC_ServerPipe = (HANDLE)(UINT_PTR)0xdeadbeef; //tell monoscript that it has to provide a serverpipe itself
		while (MDC_ServerPipe == (HANDLE)(UINT_PTR)0xdeadbeef)
			Sleep(50);

		pipehandle = MDC_ServerPipe;
		MDC_ServerPipe = 0; //indicates that it got read out

		//OutputDebugStringA("Retrieved a pipe\n");
		//OutputDebugStringA("calling ConnectNamedPipe\n");

		if (ConnectNamedPipe(pipehandle, NULL))
			OutputDebugStringA("ConnectNamedPipe returned TRUE");
		else
		{
			char bla[255];
			int error = GetLastError();
			if (error == ERROR_PIPE_CONNECTED)
				OutputDebugStringA("ConnectNamedPipe returned FALSE because the client was already connected");
			else
			{
				snprintf(bla, 255, "ConnectNamedPipe failed! Error: %d\n", GetLastError());
				OutputDebugStringA(bla);
			}
		}
	}

#else
	if ((pipehandle) && (pipehandle != INVALID_HANDLE_VALUE))
		ClosePipe(pipehandle);

	pipehandle = INVALID_HANDLE_VALUE;
	while (pipehandle == INVALID_HANDLE_VALUE)
		pipehandle = ::CreateNamedPipe((char*)datapipename);
#endif

	//OutputDebugStringA("At the end of CreatePipeandWaitForconnect\n");

}

void __cdecl customfreeimplementation(PVOID address)
{
	//	free(address);
		//freaking memleak !!!!!
}

#if defined __linux__ || defined __ANDROID__
char* findModulePath(char* modulename)
{
	//On android I could use /proc/self/filemap_list but it's not a thing on linux, so...
	//read out /proc/self/maps
	char* result = NULL;
	FILE* f = NULL;
	OutputDebugString("findModulePath(\"%s\")", modulename);
	f = fopen("/proc/self/maps", "r");
	if (f)
	{
		char s[512];
		while (fgets(s, 511, f))
		{
			if (strstr(s, modulename))
			{
				char modulepath[256];
				modulepath[0] = '\0';

				OutputDebugString("found libmono: %s\n", s);
				//here I can choose between parsing the string, for the path, or look up the address range in /proc/self/map_files
				//we've got the string anyhow, so...
				sscanf(s, "%*llx-%*llx %*s %*s %*s %*s %[^\t\n]\n", modulepath);

				OutputDebugString("modulepath=%s\n", modulepath);

				result = strdup(modulepath);
				break;
			}
		}
		fclose(f);
	}

	return result;
}
#endif


void CPipeServer::InitMono()
{


	OutputDebugStringA((char*)"CPipeServer::InitMono");
	il2cpp = FALSE;

#ifndef _WINDOWS
	void* hMono = NULL;
	int hasRealMonoHandle = 0;
	OutputDebugStringA((char*)"InitMono on non windows");
	void* fp = dlsym(RTLD_DEFAULT, "mono_thread_attach");
	OutputDebugStringA("dlsym(RTLD_DEFAULT, \"mono_thread_attach\") returned %p\n", fp);

	if (fp)
	{
		hMono = fp;
	}
	else
	{
		OutputDebugString("dlerror()=%s\n", dlerror);

		fp = dlsym(RTLD_DEFAULT, "il2cpp_thread_attach");
		OutputDebugStringA("dlsym(RTLD_DEFAULT, \"il2cpp_thread_attach\") returned %p\n", fp);

		if (fp)
		{
			il2cpp = TRUE;
			hMono = fp;
		}
	}

#if defined __linux__ || defined __ANDROID__
	if (fp == NULL)
	{
		//find the path to libmono.so and load that
		char* path = findModulePath((char*)"libmono.so");
		if (path)
		{
			OutputDebugString("calling dlopen on %s\n", path);
			hMono = dlopen(path, RTLD_NOW);
			OutputDebugString("hMono is now %p\n", hMono);

			if (hMono)
				hasRealMonoHandle = 1;

			free(path);
		}
	}
#endif
#endif

#ifdef _WINDOWS
	HMODULE hMono = GetModuleHandleA("mono.dll");



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
						hMono = me.hModule;

					if (GetProcAddress(me.hModule, "il2cpp_thread_attach"))
					{
						il2cpp = true;
						hMono = me.hModule;
					}

				} while (!hMono && Module32Next(ths, &me));

			}
			CloseHandle(ths);
		}
	}
#endif //WINDOWS

	WriteQword((UINT64)hMono);

	if (hMono)
	{
		std::stringstream x;
		x.clear();
		x << "Mono dll found at " << std::hex << hMono << "\n";
		//OutputDebugStringA(x.str().c_str());

#ifndef _WINDOWS
		if (hasRealMonoHandle == 0)
			hMono = RTLD_DEFAULT;
#endif


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
				mono_class_get_image = (MONO_CLASS_GET_IMAGE)GetProcAddress(hMono, "il2cpp_class_get_image");

				mono_class_is_generic = (MONO_CLASS_IS_GENERIC)GetProcAddress(hMono, "il2cpp_class_is_generic");
				mono_class_is_enum = (MONO_CLASS_IS_ENUM)GetProcAddress(hMono, "il2cpp_class_is_enum");
				mono_class_is_valuetype = (MONO_CLASS_IS_VALUETYPE)GetProcAddress(hMono, "il2cpp_class_is_valuetype");
				mono_class_is_subclass_of = (MONO_CLASS_IS_SUBCLASS_OF)GetProcAddress(hMono, "il2cpp_class_is_subclass_of");
				mono_class_vtable = (MONO_CLASS_VTABLE)GetProcAddress(hMono, "il2cpp_class_vtable");
				mono_class_from_mono_type = (MONO_CLASS_FROM_MONO_TYPE)GetProcAddress(hMono, "il2cpp_class_from_mono_type");
				mono_class_get_element_class = (MONO_CLASS_GET_ELEMENT_CLASS)GetProcAddress(hMono, "il2cpp_class_get_element_class");
				mono_class_instance_size = (MONO_CLASS_INSTANCE_SIZE)GetProcAddress(hMono, "il2cpp_class_instance_size");

				mono_class_num_fields = (MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "il2cpp_class_num_fields");
				mono_class_num_methods = (MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "il2cpp_class_num_methods");


				mono_field_get_name = (MONO_FIELD_GET_NAME)GetProcAddress(hMono, "il2cpp_field_get_name");
				mono_field_get_type = (MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "il2cpp_field_get_type");
				mono_field_get_parent = (MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "il2cpp_field_get_parent");
				mono_field_get_offset = (MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "il2cpp_field_get_offset");
				mono_field_get_flags = (MONO_FIELD_GET_FLAGS)GetProcAddress(hMono, "il2cpp_field_get_flags");
				mono_field_get_value_object = (MONO_FIELD_GET_VALUE_OBJECT)GetProcAddress(hMono, "il2cpp_field_get_value_object");

				mono_type_get_name = (MONO_TYPE_GET_NAME)GetProcAddress(hMono, "il2cpp_type_get_name");
				mono_type_get_type = (MONO_TYPE_GET_TYPE)GetProcAddress(hMono, "il2cpp_type_get_type");
				il2cpp_type_get_object = (IL2CPP_TYPE_GET_OBJECT)GetProcAddress(hMono, "il2cpp_type_get_object");
				mono_type_get_name_full = (MONO_TYPE_GET_NAME_FULL)GetProcAddress(hMono, "il2cpp_type_get_name_full");

				mono_method_get_name = (MONO_METHOD_GET_NAME)GetProcAddress(hMono, "il2cpp_method_get_name");
				mono_method_get_class = (MONO_METHOD_GET_CLASS)GetProcAddress(hMono, "il2cpp_method_get_class");
				mono_method_get_header = (MONO_METHOD_GET_HEADER)GetProcAddress(hMono, "il2cpp_method_get_header");
				mono_method_get_flags = (MONO_METHOD_GET_FLAGS)GetProcAddress(hMono, "il2cpp_method_get_flags");
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
				mono_array_element_size = (MONO_ARRAY_ELEMENT_SIZE)GetProcAddress(hMono, "il2cpp_array_element_size");
				mono_value_box = (MONO_VALUE_BOX)GetProcAddress(hMono, "il2cpp_value_box");
				mono_object_unbox = (MONO_OBJECT_UNBOX)GetProcAddress(hMono, "il2cpp_object_unbox");
				mono_object_new = (MONO_OBJECT_NEW)GetProcAddress(hMono, "il2cpp_object_new");

				mono_class_get_type = (MONO_CLASS_GET_TYPE)GetProcAddress(hMono, "il2cpp_class_get_type");
				mono_type_get_class = (MONO_TYPE_GET_CLASS)GetProcAddress(hMono, "il2cpp_type_get_class");
				mono_type_get_class = mono_type_get_class ? mono_type_get_class : (MONO_TYPE_GET_CLASS)GetProcAddress(hMono, "il2cpp_type_get_class_or_element_class");

				mono_method_desc_search_in_image = (MONO_METHOD_DESC_SEARCH_IN_IMAGE)GetProcAddress(hMono, "il2cpp_method_desc_search_in_image");
				mono_runtime_invoke = (MONO_RUNTIME_INVOKE)GetProcAddress(hMono, "il2cpp_runtime_invoke");
				mono_runtime_object_init = (MONO_RUNTIME_OBJECT_INIT)GetProcAddress(hMono, "il2cpp_runtime_object_init");


				mono_assembly_name_new = (MONO_ASSEMBLY_NAME_NEW)GetProcAddress(hMono, "il2cpp_assembly_name_new");
				mono_assembly_loaded = (MONO_ASSEMBLY_LOADED)GetProcAddress(hMono, "il2cpp_assembly_loaded");
				mono_assembly_open = (MONO_ASSEMBLY_OPEN)GetProcAddress(hMono, "il2cpp_assembly_open");
				mono_image_open = (MONO_IMAGE_OPEN)GetProcAddress(hMono, "il2cpp_image_open");
				mono_image_get_filename = (MONO_IMAGE_GET_FILENAME)GetProcAddress(hMono, "il2cpp_image_get_filename");

				mono_class_get_nesting_type = (MONO_CLASS_GET_NESTING_TYPE)GetProcAddress(hMono, "mono_class_get_nesting_type");

				il2cpp_field_static_get_value = (IL2CPP_FIELD_STATIC_GET_VALUE)GetProcAddress(hMono, "il2cpp_field_static_get_value");
				il2cpp_field_static_set_value = (IL2CPP_FIELD_STATIC_SET_VALUE)GetProcAddress(hMono, "il2cpp_field_static_set_value");


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

				//mono_runtime_is_shutting_down = (MONO_RUNTIME_IS_SHUTTING_DOWN)GetProcAddress(hMono, "il2cpp_runtime_is_shutting_down");  //doesn't seem to exist in il2cpp....

				mono_runtime_is_shutting_down = (MONO_RUNTIME_IS_SHUTTING_DOWN)GetProcAddress(hMono, "mono_runtime_is_shutting_down"); //some do, with this name...
				domain = mono_domain_get();

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
				mono_thread_cleanup = (MONO_THREAD_CLEANUP)GetProcAddress(hMono, "mono_thread_cleanup");

				mono_object_get_class = (MONO_OBJECT_GET_CLASS)GetProcAddress(hMono, "mono_object_get_class");

				mono_domain_foreach = (MONO_DOMAIN_FOREACH)GetProcAddress(hMono, "mono_domain_foreach");
				mono_domain_set = (MONO_DOMAIN_SET)GetProcAddress(hMono, "mono_domain_set");
				mono_domain_get = (MONO_DOMAIN_GET)GetProcAddress(hMono, "mono_domain_get");
				mono_assembly_foreach = (MONO_ASSEMBLY_FOREACH)GetProcAddress(hMono, "mono_assembly_foreach");
				mono_assembly_get_image = (MONO_ASSEMBLY_GET_IMAGE)GetProcAddress(hMono, "mono_assembly_get_image");
				mono_image_get_assembly = (MONO_IMAGE_GET_ASSEMBLY)GetProcAddress(hMono, "mono_image_get_assembly");

				mono_image_get_name = (MONO_IMAGE_GET_NAME)GetProcAddress(hMono, "mono_image_get_name");
				mono_image_get_filename = (MONO_IMAGE_GET_FILENAME)GetProcAddress(hMono, "mono_image_get_filename");

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
				mono_class_get_image = (MONO_CLASS_GET_IMAGE)GetProcAddress(hMono, "mono_class_get_image");
				mono_class_is_generic = (MONO_CLASS_IS_GENERIC)GetProcAddress(hMono, "mono_class_is_generic"); 
				mono_class_is_enum = (MONO_CLASS_IS_ENUM)GetProcAddress(hMono, "mono_class_is_enum");
				mono_class_is_valuetype = (MONO_CLASS_IS_VALUETYPE)GetProcAddress(hMono, "mono_class_is_valuetype");
				mono_class_is_subclass_of = (MONO_CLASS_IS_SUBCLASS_OF)GetProcAddress(hMono, "mono_class_is_subclass_of");

				mono_class_vtable = (MONO_CLASS_VTABLE)GetProcAddress(hMono, "mono_class_vtable");
				mono_class_from_mono_type = (MONO_CLASS_FROM_MONO_TYPE)GetProcAddress(hMono, "mono_class_from_mono_type");
				mono_class_get_element_class = (MONO_CLASS_GET_ELEMENT_CLASS)GetProcAddress(hMono, "mono_class_get_element_class");
				mono_class_instance_size = (MONO_CLASS_INSTANCE_SIZE)GetProcAddress(hMono, "mono_class_instance_size");

				mono_class_num_fields = (MONO_CLASS_NUM_FIELDS)GetProcAddress(hMono, "mono_class_num_fields");
				mono_class_num_methods = (MONO_CLASS_NUM_METHODS)GetProcAddress(hMono, "mono_class_num_methods");


				mono_field_get_name = (MONO_FIELD_GET_NAME)GetProcAddress(hMono, "mono_field_get_name");
				mono_field_get_type = (MONO_FIELD_GET_TYPE)GetProcAddress(hMono, "mono_field_get_type");
				mono_field_get_parent = (MONO_FIELD_GET_PARENT)GetProcAddress(hMono, "mono_field_get_parent");
				mono_field_get_offset = (MONO_FIELD_GET_OFFSET)GetProcAddress(hMono, "mono_field_get_offset");
				mono_field_get_flags = (MONO_FIELD_GET_FLAGS)GetProcAddress(hMono, "mono_field_get_flags");
				mono_field_get_value_object = (MONO_FIELD_GET_VALUE_OBJECT)GetProcAddress(hMono, "mono_field_get_value_object");

				mono_type_get_name = (MONO_TYPE_GET_NAME)GetProcAddress(hMono, "mono_type_get_name");
				mono_type_get_type = (MONO_TYPE_GET_TYPE)GetProcAddress(hMono, "mono_type_get_type");
				mono_type_get_object = (MONO_TYPE_GET_OBJECT)GetProcAddress(hMono, "mono_type_get_object");
				mono_type_get_name_full = (MONO_TYPE_GET_NAME_FULL)GetProcAddress(hMono, "mono_type_get_name_full");

				mono_method_get_name = (MONO_METHOD_GET_NAME)GetProcAddress(hMono, "mono_method_get_name");
				mono_method_get_class = (MONO_METHOD_GET_CLASS)GetProcAddress(hMono, "mono_method_get_class");
				mono_method_get_header = (MONO_METHOD_GET_HEADER)GetProcAddress(hMono, "mono_method_get_header");
				mono_method_get_flags = (MONO_METHOD_GET_FLAGS)GetProcAddress(hMono, "mono_method_get_flags");
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
				mono_array_element_size = (MONO_ARRAY_ELEMENT_SIZE)GetProcAddress(hMono, "mono_array_element_size");
				mono_value_box = (MONO_VALUE_BOX)GetProcAddress(hMono, "mono_value_box");
				mono_object_unbox = (MONO_OBJECT_UNBOX)GetProcAddress(hMono, "mono_object_unbox");
				mono_object_new = (MONO_OBJECT_NEW)GetProcAddress(hMono, "mono_object_new");
				mono_object_isinst = (MONO_OBJECT_ISINST)GetProcAddress(hMono, "mono_object_isinst");
				mono_get_enum_class = (MONO_GET_ENUM_CLASS)GetProcAddress(hMono, "mono_get_enum_class");

				mono_class_get_type = (MONO_CLASS_GET_TYPE)GetProcAddress(hMono, "mono_class_get_type");
				mono_type_get_class = (MONO_TYPE_GET_CLASS)GetProcAddress(hMono, "mono_type_get_class");
				mono_class_get_nesting_type = (MONO_CLASS_GET_NESTING_TYPE)GetProcAddress(hMono, "mono_class_get_nesting_type");

				mono_method_desc_search_in_image = (MONO_METHOD_DESC_SEARCH_IN_IMAGE)GetProcAddress(hMono, "mono_method_desc_search_in_image");
				mono_runtime_invoke = (MONO_RUNTIME_INVOKE)GetProcAddress(hMono, "mono_runtime_invoke");
				mono_runtime_object_init = (MONO_RUNTIME_OBJECT_INIT)GetProcAddress(hMono, "mono_runtime_object_init");


				mono_assembly_name_new = (MONO_ASSEMBLY_NAME_NEW)GetProcAddress(hMono, "mono_assembly_name_new");
				mono_assembly_loaded = (MONO_ASSEMBLY_LOADED)GetProcAddress(hMono, "mono_assembly_loaded");
				mono_assembly_open = (MONO_ASSEMBLY_OPEN)GetProcAddress(hMono, "mono_assembly_open");
				mono_image_open = (MONO_IMAGE_OPEN)GetProcAddress(hMono, "mono_image_open");

				mono_field_static_get_value = (MONO_FIELD_STATIC_GET_VALUE)GetProcAddress(hMono, "mono_field_static_get_value");
				mono_field_static_set_value = (MONO_FIELD_STATIC_SET_VALUE)GetProcAddress(hMono, "mono_field_static_set_value");

				mono_runtime_is_shutting_down = (MONO_RUNTIME_IS_SHUTTING_DOWN)GetProcAddress(hMono, "mono_runtime_is_shutting_down");
				domain = mono_get_root_domain();
			}

			ConnectThreadToMonoRuntime();
		}
		//else
		//	OutputDebugStringA("Already attached");
	}
	//todo: else do an init2 where CE will feed all the addresses to this module
}

void CPipeServer::ConnectThreadToMonoRuntime()
{
	mono_selfthread = NULL;
	//OutputDebugString("CPipeServer::ConnectThreadToMonoRuntime() : mono_thread_attach=%p\n",mono_thread_attach);
	if (mono_thread_attach)
	{

		if (il2cpp)
			mono_selfthread = mono_thread_attach(mono_domain_get());
		else
		{
			if (mono_get_root_domain)
			{
				void* domain = mono_get_root_domain();
				mono_selfthread = mono_thread_attach(domain);
			}
		}
	}

	attached = mono_selfthread != NULL;
}

void CPipeServer::Object_New()
{
	void* domain;
	if (mono_get_root_domain)
		domain = (void*)mono_get_root_domain();
	else
		domain = (void*)mono_domain_get();

	void* klass = (void*)ReadQword();
	void* object = mono_object_new(domain, klass);
	WriteQword((UINT64)object);
}

void CPipeServer::Object_Init()
{
	void* object = (void*)ReadQword();
	try
	{
		mono_runtime_object_init(object);
		WriteByte(1);
	}
	catch (...)
	{
		WriteByte(0);
		//OutputDebugStringA("Error initializing object:\n");
		//OutputDebugStringA(e);
	}


}

void CPipeServer::Object_GetClass()
{
	void* object = (void*)ReadQword();
	char* classname;
	void* klass;

	ExpectingAccessViolations = FALSE;


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
			WriteWord((WORD)strlen(classname));
			Write(classname, (int)strlen(classname));
		}
		else
		{
			WriteQword(0);
		}


	}
	catch (...)
	{
		//OutputDebugStringA("Object_GetClass exception caught");
		WriteQword(0); //failure. Invalid object
	}
}

void _cdecl DomainEnumerator(void* domain, std::vector<UINT64>* v)
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


		WriteDword((DWORD)v.size());
		for (i = 0; i < v.size(); i++)
			WriteQword(v[i]);
	}
}

void CPipeServer::SetCurrentDomain(void)
{
	void* domain = (void*)ReadQword();
	int r;
	if (mono_domain_set)
		r = mono_domain_set(domain, FALSE);
	else
		r = 0;

	WriteDword(r);
}

void _cdecl AssemblyEnumerator(void* assembly, std::vector<UINT64>* v)
{
	v->push_back((UINT_PTR)assembly);
}

void CPipeServer::EnumAssemblies()
{
	unsigned int i;
	std::vector<UINT64> v;


	if (il2cpp)
	{
		DWORD i;

		SIZE_T nrofassemblies = 0;
		UINT_PTR* assemblies;
		assemblies = il2cpp_domain_get_assemblies(mono_domain_get(), &nrofassemblies); //in il2cpp you don't need to free this

		WriteDword((DWORD)nrofassemblies);
		for (i = 0; i < (DWORD)nrofassemblies; i++)
			WriteQword(assemblies[i]);

	}
	else
	{
		if (mono_assembly_foreach)
		{
			mono_assembly_foreach((GFunc)AssemblyEnumerator, &v);

			WriteDword((DWORD)v.size());
			for (i = 0; i < v.size(); i++)
				WriteQword(v[i]);
		}
		else
		{
			WriteDword(0);
		}
	}
}


void CPipeServer::EnumImages()
{
	//first enum all assemblies
	unsigned int i;
	std::vector<UINT64> v;

	if (il2cpp)
	{
		SIZE_T nrofassemblies = 0;
		UINT_PTR* assemblies;
		assemblies = il2cpp_domain_get_assemblies(mono_domain_get(), &nrofassemblies);

		for (i = 0; i < nrofassemblies; i++)
			v.push_back((UINT_PTR)assemblies[i]);
	}
	else
	{
		if (mono_assembly_foreach)
			mono_assembly_foreach((GFunc)AssemblyEnumerator, &v);
	}

	//now build the reply
	int32_t replypos = 0;
	int replysize = (int)v.size() * 256;
	char* reply = (char*)malloc(replysize);

	for (i = 0; i < v.size(); i++)
	{
		if ((replypos + 512 + 8 + 2) >= replysize) //512+8+2 : max size of path allowed, image pointer and stringlength
		{
			replysize = replysize * 2 + 512 + 8 + 2;
			reply = (char*)realloc(reply, replysize);
		}

		void* image = mono_assembly_get_image((void*)v[i]);
		*(UINT64*)&reply[replypos] = (UINT_PTR)image;

		replypos += 8;

		char* name = mono_image_get_name(image);
		int len;
		if (name)
			len = (int)strlen(name);
		else
			len = 0;

		if (len > 512)
			len = 512;

		*(WORD*)&reply[replypos] = (WORD)len;
		replypos += 2;

		memcpy(&reply[replypos], name, len);
		replypos += len;
	}

	Write(&replypos, sizeof(replypos));

	Write(reply, replypos);

	free(reply);
}


void CPipeServer::GetImageFromAssembly()
{
	void* assembly = (void*)ReadQword();
	void* image = mono_assembly_get_image(assembly);
	WriteQword((UINT_PTR)image);
}

void CPipeServer::GetImageName()
{
	void* image = (void*)ReadQword();
	char* s = mono_image_get_name(image);

	WriteWord((WORD)strlen(s));
	Write(s, (int)strlen(s));
}

void CPipeServer::GetImageFileName()
{
	void* image = (void*)ReadQword();
	char* s = mono_image_get_filename(image);

	WriteWord((WORD)strlen(s));
	Write(s, (int)strlen(s));
}

#include <locale> 
#include <codecvt> 


WORD UTF8TOUTF16(char* szUtf8) {
#if (_WINDOWS && (_MSC_VER <= 1916))
	std::wstring_convert<std::codecvt_utf8<wchar_t>, wchar_t> convert;

	try {
		std::wstring dest = convert.from_bytes(szUtf8);
		return *(WORD*)&dest[0];
	}
	catch (const std::range_error&) {
		return NULL;
	}
#else
	std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
	std::u16string dest = convert.from_bytes(szUtf8);
	return *(WORD*)&dest[0];
#endif
}

void CPipeServer::EnumClassesInImageEx()
{

	void* image = (void*)ReadQword();
	if (image == NULL)
	{
		WriteDword(0);
		return;
	}

	std::vector<UINT64> classes;
	CMemStream reply;
	int i;

	if (il2cpp)
	{
		int count = 0;
		if ((il2cpp_image_get_class_count) && (il2cpp_image_get_class))
			count = il2cpp_image_get_class_count(image);

		reply.WriteDword(count);

		for (i = 0; i < count; i++)
		{
			void* c = il2cpp_image_get_class(image, i);
			reply.WriteQword((UINT64)c);

			if (c)
			{

				void* parent = mono_class_get_parent ? mono_class_get_parent(c) : 0;
				reply.WriteQword((UINT64)parent);

				void* nestingtype = mono_class_get_nesting_type ? mono_class_get_nesting_type(c) : 0;
				reply.WriteQword((UINT64)parent);

				char* name = mono_class_get_name(c);
				WORD sl = (WORD)strlen(name);
				reply.WriteWord(sl);
				reply.Write(name, sl);

				char* ns = mono_class_get_namespace(c);
				sl = (WORD)strlen(ns);
				reply.WriteWord(sl);
				reply.Write(ns, sl);

				std::string fullname = GetFullTypeNameStr(c, 1, MONO_TYPE_NAME_FORMAT_REFLECTION);

				reply.WriteWord((WORD)fullname.size());
				reply.Write((PVOID)fullname.c_str(), (unsigned int)fullname.size());
			}
			else
			{
				reply.WriteQword(0);
				reply.WriteQword(0);
				reply.WriteWord(0);
				reply.WriteWord(0);
				reply.WriteWord(0);
			}

		}

	}
	else
	{
		//mono

		void* tdef = mono_image_get_table_info ? mono_image_get_table_info(image, MONO_TABLE_TYPEDEF) : NULL;
		if (tdef)
		{
			int tdefcount = mono_table_info_get_rows ? mono_table_info_get_rows(tdef) : NULL;
			reply.WriteDword(tdefcount);

			for (i = 0; i < tdefcount; i++)
			{
				void* c = mono_class_get(image, MONO_TOKEN_TYPE_DEF | (i + 1));
				reply.WriteQword((UINT64)c);
				if (c != NULL)
				{

					void* parent = mono_class_get_parent ? mono_class_get_parent(c) : 0;
					reply.WriteQword((UINT64)parent);

					void* nestingtype = mono_class_get_nesting_type ? mono_class_get_nesting_type(c) : 0;
					reply.WriteQword((UINT64)nestingtype);

					char* name = mono_class_get_name(c);
					WORD sl = (WORD)strlen(name);
					reply.WriteWord(sl);
					reply.Write(name, sl);

					char* ns = mono_class_get_namespace(c);
					sl = (WORD)strlen(ns);
					reply.WriteWord(sl);
					reply.Write(ns, sl);

					std::string fullname = GetFullTypeNameStr(c, 1, MONO_TYPE_NAME_FORMAT_REFLECTION);

					reply.WriteWord((WORD)fullname.size());
					reply.Write((PVOID)fullname.c_str(), (unsigned int)fullname.size());
				}
				else
				{
					reply.WriteQword(0);
					reply.WriteQword(0);
					reply.WriteWord(0);
					reply.WriteWord(0);
					reply.WriteWord(0);
				}
			}
		}
		else
			reply.WriteDword(0);

	}

	WriteDword(reply.GetSize());
	Write(reply.GetMemory(), reply.GetSize());
}

void CPipeServer::EnumClassesInImage()
{
	int i;
	void* image = (void*)ReadQword();
	if (image == NULL)
	{
		WriteDword(0);
		return;
	}

	if (il2cpp)
	{
		int count = 0;
		if (il2cpp_image_get_class_count)
			count = il2cpp_image_get_class_count(image);


		WriteDword(count);


		for (i = 0; i < count; i++)
		{
			if (il2cpp_image_get_class)
			{
				void* c = il2cpp_image_get_class(image, i);
				WriteQword((UINT_PTR)c);

				if (c)
				{
					char* name = mono_class_get_name(c);
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

		void* tdef = mono_image_get_table_info(image, MONO_TABLE_TYPEDEF);
		if (tdef)
		{
			int tdefcount = mono_table_info_get_rows(tdef);
			WriteDword(tdefcount);

			for (i = 0; i < tdefcount; i++)
			{
				void* c = mono_class_get(image, MONO_TOKEN_TYPE_DEF | (i + 1));
				if (c != NULL)
				{
					char* name = mono_class_get_name(c);

					WriteQword((UINT_PTR)c);


					std::string sName = std::string(name);

					if ((BYTE)name[0] == 0xEE) {
						char szUeName[32];
						sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(name));
						sName = szUeName;
					}

					if (c)
					{
						WriteWord((WORD)sName.size());
						Write((PVOID)sName.c_str(), (WORD)sName.size());
					}
					else
						WriteWord(0);

					name = mono_class_get_namespace(c);
					if (name)
					{
						WriteWord((WORD)strlen(name));
						Write(name, (WORD)strlen(name));
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
}

void CPipeServer::EnumFieldsInClass()
{
	void* c = (void*)ReadQword();
	void* iter = NULL;
	void* field;

	do
	{
		field = mono_class_get_fields(c, &iter);
		WriteQword((UINT_PTR)field);

		if (field)
		{
			void* fieldtype = mono_field_get_type(field);
			WriteQword((UINT_PTR)fieldtype);
			WriteDword(mono_type_get_type(fieldtype));
			WriteQword((UINT_PTR)mono_field_get_parent(field));
			WriteDword((DWORD)mono_field_get_offset(field));
			WriteDword(mono_field_get_flags(field));



			char* name = mono_field_get_name(field);
			char* type = mono_type_get_name(fieldtype);
			std::string sName = std::string(name);
			std::string sType = std::string(type);
			//check if name is x ...

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

			WriteWord((WORD)sName.size());
			Write((LPVOID)sName.c_str(), (WORD)sName.size());

			if (type)
			{
				WriteWord((WORD)sType.size());
				Write((LPVOID)sType.c_str(), (WORD)sType.size());
				//g_free(name);
			}
			else
				WriteWord(0);

		}
	} while (field);
}


void CPipeServer::EnumMethodsInClass()
{
	void* c = (void*)ReadQword();
	void* iter = NULL;
	void* method;

	do
	{
		method = mono_class_get_methods(c, &iter);
		WriteQword((UINT_PTR)method);

		if (method)
		{
			char* name;
			uint32_t flags;

			name = mono_method_get_name(method);
			flags = mono_method_get_flags(method, NULL);

			std::string sName = std::string(name);

			if ((BYTE)name[0] == 0xEE) {
				char szUeName[32];
				sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(name));

				sName = szUeName;
			}

			WriteWord((WORD)sName.size());
			Write((PVOID)sName.c_str(), (WORD)sName.size());

			WriteDword(flags);
		}
	} while (method);

}

void CPipeServer::CompileMethod()
{
	void* method = (void*)ReadQword();


	if (il2cpp)
	{
		WriteQword(*(PUINT_PTR)method); //fist pointer points to compiled code
	}
	else
	{
		void* result = NULL;
		try
		{
			void* klass = mono_method_get_class(method);
			if (klass)
			{
				if ((mono_class_is_generic && (mono_class_is_generic(klass) == 0)) || (mono_class_is_generic == NULL)) //good luck if is_generic is missing (unity adds it but not all mono games are unity)
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
		void* method = (void*)ReadQword();
		void* result = mono_method_get_header(method);
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
		void* methodheader = (void*)ReadQword();
		UINT32 code;
		void* result = mono_method_header_get_code(methodheader, &code, NULL);
		WriteQword((UINT_PTR)result);
		WriteDword(code);
	}
}

void CPipeServer::RvaMap()
{
	if (mono_image_rva_map)
	{
		void* image = (void*)ReadQword();
		UINT32 offset = ReadDword();
		void* result = mono_image_rva_map(image, offset);

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
		ReadQword(); //Domain
		ReadQword(); //Address
		WriteQword(0);
	}
	else
	{
		void* domain = (void*)ReadQword();
		if (domain == NULL)
			domain = (void*)mono_get_root_domain();
		void* address = (void*)ReadQword();
		void* jitinfo = mono_jit_info_table_find(domain, address);
		WriteQword((UINT_PTR)jitinfo);
		if (jitinfo)
		{
			WriteQword((UINT_PTR)mono_jit_info_get_method(jitinfo));
			WriteQword((UINT_PTR)mono_jit_info_get_code_start(jitinfo));
			WriteDword((DWORD)mono_jit_info_get_code_size(jitinfo));
		}
	}
}

void CPipeServer::FindClass()
{
	void* image = (void*)ReadQword();
	WORD length = ReadWord();
	char* cn = NULL;
	char* ns = NULL;
	void* klass;

	cn = (char*)malloc(length + 1);
	if (length)
		Read(cn, length);

	cn[length] = 0;

	length = ReadWord();

	ns = (char*)malloc(length + 1);
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
	void* klass = (void*)ReadQword();
	WORD length = ReadWord();
	char* methodname = (char*)malloc(length + 1);
	void* method = NULL;
	if (length)
		Read(methodname, length);
	methodname[length] = 0;


	method = mono_class_get_method_from_name(klass, methodname, -1);
	WriteQword((UINT_PTR)method);
}

void CPipeServer::GetMethodName()
{
	void* method = (void*)ReadQword();
	char* methodname = mono_method_get_name(method);

	WriteWord((WORD)strlen(methodname));
	Write(methodname, (WORD)strlen(methodname));
}

void CPipeServer::GetMethodClass()
{
	void* method = (void*)ReadQword();
	void* result = mono_method_get_class(method);
	WriteQword((UINT_PTR)result);
}


void CPipeServer::GetKlassName()
{
	void* klass = (void*)ReadQword();
	if (klass && mono_class_get_name)
	{

		char* classname = mono_class_get_name(klass);

		std::string sName = std::string(classname);

		if ((BYTE)classname[0] == 0xEE) {
			char szUeName[32];
			sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(classname));

			sName = szUeName;
		}

		WriteWord((WORD)sName.size());
		Write((PVOID)sName.c_str(), (WORD)(sName.size()));
	}
	else
	{
		WriteWord(0);
	}
}

void CPipeServer::GetClassNamespace()
{

	void* klass = (void*)ReadQword();
	if (klass && mono_class_get_namespace)
	{
		char* classname = mono_class_get_namespace(klass);

		WriteWord((WORD)strlen(classname));
		Write(classname, (WORD)strlen(classname));
	}
	else
		WriteWord(0);
}

void CPipeServer::FreeMethod()
{
	if (mono_free_method)
		mono_free_method((void*)ReadQword());
}

void CPipeServer::FreeObject()
{

	//not yet implemented
}

void CPipeServer::GetMonoDataCollectorVersion()
{
	WriteDword(MONO_DATACOLLECTORVERSION);
}

void CPipeServer::NewString()
{
	void* domain = (void*)ReadQword();
	if (domain == NULL)
		domain = (void*)mono_get_root_domain();

	char* s = ReadString();
	free(s);

	void* string = mono_string_new(domain, s);
	WriteQword((UINT_PTR)string);
}

void CPipeServer::DisassembleMethod()
{
	void* method = (void*)ReadQword();
	if (!il2cpp)
	{
		void* methodheader = mono_method_get_header(method);
		UINT32 codesize, maxstack;
		void* ilcode = mono_method_header_get_code(methodheader, &codesize, &maxstack);
		char* disassembly = mono_disasm_code(NULL, method, ilcode, (void*)((UINT_PTR)ilcode + codesize));

		WriteWord((WORD)strlen(disassembly));
		Write(disassembly, (WORD)strlen(disassembly));
		g_free(disassembly);
	}
}

void CPipeServer::GetMethodParameters()
{
	void* method = (void*)ReadQword();
	int i;

	if (il2cpp)
	{
		int paramcount = il2cpp_method_get_param_count(method);
		WriteByte(paramcount);
		for (i = 0; i < paramcount; i++)
		{
			char* paramname = il2cpp_method_get_param_name(method, i);
			WriteString1(paramname);
		}

		if (paramcount)
		{
			for (i = 0; i < paramcount; i++)
			{
				void* paramtype = il2cpp_method_get_param(method, i);

				if (paramtype)
					WriteDword(mono_type_get_type(paramtype));
				else
					WriteDword(0);
			}
		}

		{
			void* returntype = il2cpp_method_get_return_type(method);
			if (returntype)
				WriteDword(mono_type_get_type(returntype));
			else
				WriteDword(0);
		}

	}
	else
	{

		void* methodsignature = mono_method_signature(method);


		if (methodsignature)
		{
			int paramcount = mono_signature_get_param_count(methodsignature);
			char** names = (char**)calloc(sizeof(char*), paramcount);
			mono_method_get_param_names(method, (const char**)names);
			WriteByte(paramcount);
			for (i = 0; i < paramcount; i++)
			{
				if (names[i])
				{
					WriteByte((BYTE)strlen(names[i]));
					Write(names[i], (BYTE)strlen(names[i]));
				}
				else
					WriteByte(0);
			}

			if (paramcount)
			{
				gpointer iter = NULL;
				MonoType* paramtype;

				for (i = 0; i < paramcount; i++)
				{
					paramtype = mono_signature_get_params((MonoMethodSignature*)methodsignature, &iter);

					if (paramtype)
						WriteDword(mono_type_get_type(paramtype));
					else
						WriteDword(0);
				};
			}

			{
				MonoType* returntype = mono_signature_get_return_type(methodsignature);
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
	void* method = (void*)ReadQword();
	int i;

	if (il2cpp)
	{
		int paramcount = il2cpp_method_get_param_count(method);
		char* name;
		void* type;

		WriteByte(paramcount);

		for (i = 0; i < paramcount; i++)
		{
			name = il2cpp_method_get_param_name(method, i);
			WriteString1(name);
		}

		for (i = 0; i < paramcount; i++)
		{
			type = il2cpp_method_get_param(method, i);
			name = il2cpp_type_get_name(type);
			WriteString(name);
		}


		type = il2cpp_method_get_return_type(method);
		name = il2cpp_type_get_name(type);
		WriteString1(name);

	}
	else
	{

		void* methodsignature = mono_method_signature(method);
		char* sig = mono_signature_get_desc(methodsignature, TRUE);
		int paramcount = mono_signature_get_param_count(methodsignature);



		WriteByte(paramcount);

		if (paramcount)
		{
			char** names = (char**)calloc(sizeof(char*), paramcount);

			mono_method_get_param_names(method, (const char**)names);

			for (i = 0; i < paramcount; i++)
			{
				if (names[i])
				{
					WriteByte((BYTE)strlen(names[i]));
					Write(names[i], (BYTE)strlen(names[i]));
				}
				else
					WriteByte(0);
			}
			free(names);
		}



		WriteWord((WORD)strlen(sig));
		Write(sig, (WORD)strlen(sig));
		g_free(sig);

		//12/5/2014:send the returntype as well
		void* returntype = mono_signature_get_return_type(methodsignature);

		if (returntype)
		{
			char* tname = mono_type_get_name(returntype);
			if (tname)
			{
				WriteByte((BYTE)strlen(tname));
				Write(tname, (unsigned int)(BYTE)strlen(tname));
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
	void* klass = (void*)ReadQword();
	UINT_PTR parent = 0;
	if (klass)
		parent = mono_class_get_parent ? (UINT_PTR)mono_class_get_parent(klass) : 0;

	WriteQword(parent);
}

void CPipeServer::GetClassNestingType(void)
{
	UINT_PTR nestingtype = 0;
	void* klass = (void*)ReadQword();
	if (klass)
		nestingtype = mono_class_get_nesting_type ? (UINT_PTR)mono_class_get_nesting_type(klass) : 0;

	WriteQword(nestingtype);
}


void CPipeServer::GetClassImage(void)
{
	UINT_PTR image = 0;
	void* klass = (void*)ReadQword();
	if (klass)
		image = mono_class_get_image ? (UINT_PTR)mono_class_get_image(klass) : 0;

	WriteQword(image);
}

void CPipeServer::GetClassType()
{
	void* kls = (void*)ReadQword();
	WriteQword((UINT64)mono_class_get_type(kls));
}

void CPipeServer::GetClassOfType()
{
	void* type = (void*)ReadQword();
	WriteQword((UINT64)mono_type_get_class(type));
}

void CPipeServer::GetTypeOfMonoType()
{
	void* type = (void*)ReadQword();
	WriteDword((UINT64)mono_type_get_type(type));
}

void CPipeServer::GetReflectionTypeOfClassType()
{
	void* type = (void*)ReadQword(); //MonoType*

	try
	{
		if (il2cpp)
			WriteQword((UINT64)il2cpp_type_get_object(type)); //ReflectionType*
		else		
			WriteQword((UINT64)mono_type_get_object(domain, type)); //ReflectionType*
	}
	catch (...)
	{
		OutputDebugString("error at GetReflectionTypeOfClassType");
		WriteQword(0);
	}
}


void CPipeServer::GetVTableFromClass(void)
{
	void* domain = (void*)ReadQword();
	void* klass = (void*)ReadQword();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{
		if (domain == NULL)
			domain = (void*)mono_get_root_domain();

		void* vtable = (domain && klass) ? mono_class_vtable(domain, klass) : NULL;

		WriteQword((UINT_PTR)vtable);
	}

}

void CPipeServer::GetStaticFieldAddressFromClass(void)
{
	void* domain = (void*)ReadQword();
	void* klass = (void*)ReadQword();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{
		if (domain == NULL)
			domain = (void*)mono_get_root_domain();

		void* vtable = (domain && klass) ? mono_class_vtable(domain, klass) : NULL;
		if (vtable)
		{
			void* staticdata = mono_vtable_get_static_field_data(vtable);
			WriteQword((UINT_PTR)staticdata);
		}
		else
			WriteQword(0);
	}
}

void CPipeServer::GetTypeClass(void)
{
	void* field = (void*)ReadQword();  // TODO: this should be monotype but EnumFieldsInClass effectively returns fieldtype ptr
	void* type = field ? mono_field_get_type(field) : NULL;
	void* klass;

	if (il2cpp)
		klass = type ? il2cpp_class_from_type(type) : NULL;
	else
		klass = type ? mono_class_from_mono_type(type) : NULL;

	WriteQword((UINT_PTR)klass);
}

void CPipeServer::GetArrayElementClass(void)
{
	void* klass = (void*)ReadQword();
	void* eklass = klass ? mono_class_get_element_class(klass) : NULL;
	WriteQword((UINT_PTR)eklass);
}

void CPipeServer::FindMethodByDesc(void)
{
	void* image = (void*)ReadQword();
	char* fqMethodName = ReadString();

	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{
		void* mmd = fqMethodName ? mono_method_desc_new(fqMethodName, 1) : NULL;
		void* method = mmd && image ? mono_method_desc_search_in_image(mmd, image) : NULL;
		WriteQword((UINT_PTR)method);
		FreeString(fqMethodName);
	}
}
/*
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

	default:
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
#ifdef _WINDOWS
				int l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, NULL, 0, NULL, NULL);
				char *c = (char *)malloc(l+1);
				l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, c, l, NULL, NULL);
				c[l] = 0;
				WriteString(c);
				free(c);
#else
				//todo: unsure about this
				std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
				std::string dest = convert.to_bytes((char16_t *)ptr);
				WriteString(dest.c_str());
#endif
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
				result = mono_runtime_invoke(method, pThis, args, NULL);
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
*/

void CPipeServer::InvokeMethod(void)
{

	void* method = (void*)ReadQword();
	void* pThis = (void*)ReadQword();
	void* result;
	WORD nargs;
	if (il2cpp)
	{
		nargs = il2cpp_method_get_param_count(method);
	}
	else
	{
		void* methodsignature = mono_method_signature(method);
		nargs = methodsignature ? mono_signature_get_param_count(methodsignature) : 0;
	}
	void* arry[16];
	UINT64 args[32];

	for (int i = 0; i < nargs; i++)
	{
		switch (ReadByte())
		{
		case MONO_TYPE_VOID:
			args[i] = ReadQword();
			arry[i] = &args[i];
			break;
		case MONO_TYPE_CHAR:
			arry[i] = ReadString();
			break;
		case MONO_TYPE_BOOLEAN:
		case MONO_TYPE_U1:
		case MONO_TYPE_I1:
			args[i] = ReadByte();
			arry[i] = &args[i];
			break;
		case MONO_TYPE_U2:
		case MONO_TYPE_I2:
			args[i] = ReadWord();
			arry[i] = &args[i];
			break;
		case MONO_TYPE_U4:
		case MONO_TYPE_I4:
			args[i] = ReadDword();
			arry[i] = &args[i];
			break;
		case MONO_TYPE_U8:
		case MONO_TYPE_I8:
		case MONO_TYPE_U:
		case MONO_TYPE_I:
			args[i] = ReadQword();
			arry[i] = &args[i];
			break;
		case MONO_TYPE_R4:
		{
			void* f;
			Read(&f, 4);
			args[i] = (UINT64)(void*)f;
			arry[i] = &args[i];
		}break;
		case MONO_TYPE_R8:
		{
			void* d;
			Read(&d, 8);
			args[i] = (UINT64)(void*)d;
			arry[i] = &args[i];
		}break;
		case MONO_TYPE_STRING:
		{
			char* ptr = ReadString();
			arry[i] = mono_string_new(domain, ptr);
		}break;
		case MONO_TYPE_OBJECT:
		case MONO_TYPE_PTR:
		case MONO_TYPE_FNPTR:
			arry[i] = (void*)ReadQword();
			break;
		default:
			arry[i] = (void*)ReadQword();
			break;
		}
	}
	try
	{
		MonoObject* exception = {};
		result = mono_runtime_invoke(method, pThis, arry, &exception);
		if (!result)
		{
			WriteByte(MONO_TYPE_VOID);
			WriteQword((UINT64)result);
			return;
		}
		void* klass = mono_object_get_class(result);
		void* type = klass ? mono_class_get_type(klass) : NULL;
		int returntype = type ? mono_type_get_type(type) : MONO_TYPE_VOID;
		WriteByte(returntype);
		switch (returntype)
		{
		case MONO_TYPE_STRING:
		{
			if (il2cpp)
			{
				wchar_t* ptr = il2cpp_string_chars(result);
#ifdef _WINDOWS
				int l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, NULL, 0, NULL, NULL);
				char* c = (char*)malloc(l + 1);
				l = WideCharToMultiByte(CP_UTF8, 0, ptr, -1, c, l, NULL, NULL);
				c[l] = 0;
				WriteString(c);
				free(c);
#else
				//todo: unsure about this
				std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> convert;
				std::string dest = convert.to_bytes((char16_t*)ptr);
				WriteString(dest.c_str());
#endif
				/*_bstr_t b((wchar_t*)il2cpp_string_chars(result));
				WriteString((char*)b);*/

			}
			else
			{
				char* ptr = mono_string_to_utf8(result);
				WriteString(ptr);
				g_free(ptr);
			}
		}
		break;
		case MONO_TYPE_CHAR:
			WriteString((char*)mono_object_unbox(result));
			break;
		case MONO_TYPE_R4:
		{
			float f = *(float*)mono_object_unbox(result);
			Write(&f, 4);
		}break;
		case MONO_TYPE_R8:
		{
			double d = *(double*)mono_object_unbox(result);
			Write(&d, 8);
		}break;
		case MONO_TYPE_I1:
		case MONO_TYPE_U1:
		case MONO_TYPE_BOOLEAN:
			WriteByte(*(BYTE*)mono_object_unbox(result));
			break;
		case MONO_TYPE_I2:
		case MONO_TYPE_U2:
			WriteWord(*(WORD*)mono_object_unbox(result));
			break;
		case MONO_TYPE_I4:
		case MONO_TYPE_U4:
			WriteDword(*(DWORD*)mono_object_unbox(result));
			break;
		case MONO_TYPE_I:
		case MONO_TYPE_U:
		case MONO_TYPE_I8:
		case MONO_TYPE_U8:
			WriteQword(*(UINT64*)mono_object_unbox(result));
			break;
		case MONO_TYPE_VALUETYPE:
			WriteQword((UINT64)mono_object_unbox(result));
			break;
			/*case MONO_TYPE_PTR:
			case MONO_TYPE_BYREF:
			case MONO_TYPE_CLASS:
			case MONO_TYPE_FNPTR:
			case MONO_TYPE_GENERICINST:
			case MONO_TYPE_ARRAY:
			case MONO_TYPE_SZARRAY:
			case MONO_TYPE_VALUETYPE:
			{
				WriteQword((INT64)result);
			}
			break;*/

		default:
			WriteQword((INT64)result);
			break;
		}
	}
	catch (...)
	{
		WriteByte(0);
	}
}

void CPipeServer::LoadAssemblyFromFile(void)
{
	char* imageName = ReadString();
	if (il2cpp)
	{
		WriteQword(0);
	}
	else
	{

		int status;
		void* domain = (void*)mono_get_root_domain();
		mono_domain_set(domain, FALSE);

		void* assembly = mono_assembly_open(imageName, &status);
		void* image = mono_assembly_get_image(assembly);
		void* c = mono_class_from_name(image, (char*)"", (char*)"test");

		WriteQword((UINT_PTR)assembly);

		if (mono_jit_exec)
		{
			int argc;
			char* argv = (char*)"BLA";
			mono_jit_exec(domain, assembly, 1, &argv);
		}
	}
}

std::string CPipeServer::GetFullTypeNameStr(void* klass, char isKlass, int nameformat)
//Returns a string with the full typename
{
	try
	{
		void* ptype = klass && isKlass ? mono_class_get_type(klass) : klass;

		if (ptype)
		{
			char* fullname;
			if (il2cpp)
				fullname = il2cpp_type_get_name(ptype);
			else
				fullname = mono_type_get_name_full ? mono_type_get_name_full(ptype, nameformat) : mono_type_get_name(ptype); //fallback on type_get_name in case of non exported mono_type_get_name_full

			if (fullname)
			{
				std::string sName = std::string(fullname);

				if ((BYTE)fullname[0] == 0xEE) {
					char szUeName[32];
					auto plus = strchr(fullname, '+');
					if (plus) {
						sprintf_s(szUeName, 32, "\\u%04X+\\u%04X", UTF8TOUTF16(fullname), UTF8TOUTF16(plus + 1));
						sName = szUeName;
					}
					else {
						sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(fullname));
						sName = szUeName;
					}
				}

				return sName;
			}
			else
				return "";
		}
		else
		{
			return "<invalid ptype>";
		}
	}
	catch (...)
	{
		return "(exception)";
	}


}

void CPipeServer::GetFullTypeName(void)
{
	//ExpectingAccessViolations = TRUE;

	//1-(8-1-4)

	try
	{
		void* klass = (void*)ReadQword();
		char isKlass = ReadByte();
		void* ptype = klass && isKlass ? mono_class_get_type(klass) : klass;
		int nameformat = ReadDword();

		if (ptype)
		{
			char* fullname;
			if (il2cpp)
				fullname = il2cpp_type_get_name(ptype);
			else
				fullname = mono_type_get_name_full ? mono_type_get_name_full(ptype, nameformat) : mono_type_get_name(ptype); //fallback on type_get_name in case of non exported mono_type_get_name_full

			if (fullname)
			{
				std::string sName = std::string(fullname);

				if ((BYTE)fullname[0] == 0xEE) {
					char szUeName[32];
					auto plus = strchr(fullname, '+');
					if (plus) {
						sprintf_s(szUeName, 32, "\\u%04X+\\u%04X", UTF8TOUTF16(fullname), UTF8TOUTF16(plus + 1));
						sName = szUeName;
					}
					else {
						sprintf_s(szUeName, 32, "\\u%04X", UTF8TOUTF16(fullname));
						sName = szUeName;
					}
				}

				WriteWord((WORD)sName.size());
				Write((PVOID)sName.c_str(), (WORD)sName.size());
			}
			else
				WriteWord(0);
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
	void* klass = (void*)ReadQword();
	if (mono_class_is_generic)
		WriteByte(mono_class_is_generic(klass));
	else
		WriteByte(0); //not supported
}

void CPipeServer::IsEnumClass()
{
	void* klass = (void*)ReadQword();
	if (mono_class_is_enum)
		WriteByte(mono_class_is_enum(klass));
	else
		WriteByte(0); //not supported
}

void CPipeServer::IsValueTypeClass()
{
	void* klass = (void*)ReadQword();
	if (mono_class_is_valuetype)
		WriteByte(mono_class_is_valuetype(klass));
	else
		WriteByte(0); //not supported
}

void CPipeServer::IsSubClassOf()
{
	void* klass = (void*)ReadQword();
	void* parentklass = (void*)ReadQword();
	bool checkInteface = (void*)ReadByte();
	if (mono_class_is_subclass_of)
		WriteByte(mono_class_is_subclass_of(klass,parentklass,checkInteface));
	else
		WriteByte(0); //not supported
}

void CPipeServer::GetArrayElementSize()
{
	void* klass = (void*)ReadQword();
	if (mono_array_element_size)
		WriteDword(mono_array_element_size(klass));
	else
		WriteDword(0);
}

void CPipeServer::IsIL2CPP()
{
	WriteByte(il2cpp);
}

void CPipeServer::FillOptionalFunctionList()
{
	QWORD _mono_type_get_name_full = ReadQword();

	if ((mono_type_get_name_full == NULL) && (_mono_type_get_name_full != 0)) mono_type_get_name_full = (MONO_TYPE_GET_NAME_FULL)_mono_type_get_name_full;

	WriteByte(1);
}

void CPipeServer::GetStaticFieldValue()
{
	void* Vtable = (void*)ReadQword();
	void* Field = (void*)ReadQword();

	QWORD val = 0;
	if (il2cpp)
	{
		if (il2cpp_field_static_get_value)
		{
			il2cpp_field_static_get_value(Field, &val);
		}

	}
	else
	{
		if (mono_field_static_get_value)
		{
			if (mono_field_get_flags)
			{
				int flags = mono_field_get_flags(Field);
				if (flags & 0x10) //0x10=FIELD_ATTRIBUTE_STATIC
				{
					if ((flags & 0x8040) && mono_field_get_value_object && mono_object_unbox && mono_object_isinst && mono_get_enum_class) //FIELD_ATTRIBUTE_LITERAL | FIELD_ATTRIBUTE_HAS_DEFAULT (for constant/enum fields)
					{
						void* obj = mono_field_get_value_object(mono_get_root_domain(), Field, nullptr);
						static void* EnumClass = mono_get_enum_class();
						if (EnumClass && mono_object_isinst(obj, EnumClass))
						{
							int v = *(int*)mono_object_unbox(obj);
							val = v;
						}
						else
							goto NOTENUM;
					}
					else
					{
					NOTENUM:
						if (mono_vtable_get_static_field_data)
						{
							void* sfd = mono_vtable_get_static_field_data(Vtable);

							if (sfd > (void*)0x10000)
							{
								if (mono_class_instance_size)
								{
									int sizeNeeded = mono_class_instance_size(mono_class_from_mono_type(mono_field_get_type(Field)));
									if (sizeNeeded <= 8)
									{
										mono_field_static_get_value(Vtable, Field, &val);
									}
								}
							}
						}
						else
						{
							int sizeNeeded = mono_class_instance_size(mono_class_from_mono_type(mono_field_get_type(Field)));
							if (sizeNeeded <= 8)
							{
								mono_field_static_get_value(Vtable, Field, &val);
							}
						}
					}
				}
			}
		}
	}

	WriteQword(val);
}


void CPipeServer::SetStaticFieldValue()
{
	void* Vtable = (void*)ReadQword();
	void* Field = (void*)ReadQword();
	QWORD val = ReadQword();

	if (il2cpp)
	{
		if (il2cpp_field_static_set_value)
			il2cpp_field_static_set_value(Field, &val);
	}
	else
	{
		if (mono_field_static_set_value)
			mono_field_static_set_value(Vtable, Field, &val);
	}
}



void CPipeServer::Start(void)
{

	BYTE command;

	OutputDebugString("CPipeServer::Start\n");
	while (1)
	{
		if ((mono_runtime_is_shutting_down) && (mono_runtime_is_shutting_down()))
			return;

		CreatePipeandWaitForconnect();

		try
		{


			while (TRUE)
			{
				command = ReadByte();
				ExpectingAccessViolations = TRUE;
#ifdef _WINDOWS
				ExpectingAccessViolationsThread = GetCurrentThreadId();
#elif __APPLE__
				pthread_threadid_np(NULL, &ExpectingAccessViolationsThread);
#else
				ExpectingAccessViolationsThread = gettid();
#endif

				if (setjmp(onError))
				{
					//OutputDebugString("setjmp returned 1");
					throw("Error during execution");
				}

				if (limitedConnection)
					ConnectThreadToMonoRuntime();


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

				case MONOCMD_ENUMIMAGES:
					EnumImages();
					break;

				case MONOCMD_GETIMAGEFROMASSEMBLY:
					GetImageFromAssembly();
					break;

				case MONOCMD_GETIMAGENAME:
					GetImageName();
					break;

				case MONOCMD_GETIMAGEFILENAME:
					GetImageFileName();
					break;


				case MONOCMD_ENUMCLASSESINIMAGE:
					EnumClassesInImage();
					break;

				case MONOCMD_ENUMCLASSESINIMAGEEX:
					EnumClassesInImageEx();
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

				case MONOCMD_ISCLASSENUM:
					IsEnumClass();
					break;

				case MONOCMD_ISCLASSVALUETYPE:
					IsValueTypeClass();
					break;

				case MONOCMD_ISCLASSISSUBCLASSOF:
					IsSubClassOf();
					break;

				case MONOCMD_ISIL2CPP:
					IsIL2CPP();
					break;

				case MONOCMD_FILLOPTIONALFUNCTIONLIST:
					FillOptionalFunctionList();
					break;

				case MONOCMD_GETSTATICFIELDVALUE:
					GetStaticFieldValue();
					break;

				case MONOCMD_SETSTATICFIELDVALUE:
					SetStaticFieldValue();
					break;

				case MONOCMD_GETCLASSIMAGE:
					GetClassImage();
					break;

				case MONOCMD_GETCLASSNESTINGTYPE:
					GetClassNestingType();
					break;

				case MONOCMD_GETCLASSTYPE:
					GetClassType();
					break;

				case MONOCMD_GETCLASSOFTYPE:
					GetClassOfType();
					break;
					
				case MONOCMD_GETTYPEOFMONOTYPE:
					GetTypeOfMonoType();
					break;

				case MONOCMD_GETREFLECTIONTYPEOFCLASSTYPE:
					GetReflectionTypeOfClassType();
					break;


				case MONOCMD_FREE:
					FreeObject();
					break;

				case MONOCMD_GETMONODATACOLLECTORVERSION:
					GetMonoDataCollectorVersion();
					break;

				case MONOCMD_NEWSTRING:
					NewString();
					break;

				case MONOCMD_LIMITEDCONNECTION:
					limitedConnection = true;
					break;

				case MONOCMD_ARRAYELEMENTSIZE:
					GetArrayElementSize();
					break;

				}

				ExpectingAccessViolations = FALSE;

				if (limitedConnection) //beware: If profiling gets added someday, this will likely cause issues
				{
					if (mono_thread_detach)
					{
						mono_thread_detach(mono_selfthread);
						attached = false;
						mono_selfthread = NULL;
					}
				}

			}
		}
		catch (char* e)
		{
			//Pipe error, or something else that wasn't caught. Exit the connection and start over
			char s[200];
			snprintf(s, 200, "Pipe error: %s", e);
			OutputDebugStringA(s);

			if (attached)
			{
				try
				{
					if (mono_thread_detach && mono_selfthread)
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
			OutputDebugStringA("Unexpected pipe error\n");
			if (attached)
			{
				try
				{
					if (mono_thread_detach && mono_selfthread)
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

