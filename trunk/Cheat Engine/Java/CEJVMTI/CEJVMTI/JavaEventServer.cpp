#include "StdAfx.h"
#include "JavaEventServer.h"


CJavaEventServer *old_eventserver=NULL;
CJavaEventServer *eventserver=NULL;

jvmtiEventCallbacks callbacks;


void JNICALL MethodLoad(jvmtiEnv *jvmti_env, jmethodID method, jint code_size, const void* code_addr, jint map_length, 
								const jvmtiAddrLocationMap* map, const void* compile_info)
{
	if (eventserver)
		eventserver->MethodLoad(jvmti_env, method, code_size, code_addr);

}

void JNICALL MethodUnload(jvmtiEnv *jvmti_env, jmethodID method, const void* code_addr)
{
	if (eventserver)
		eventserver->MethodUnload(jvmti_env, method, code_addr);
}

void JNICALL DynamicCodeGenerated(jvmtiEnv *jvmti_env, const char* name, const void* address, jint length)
{
	if (eventserver)
		eventserver->DynamicCodeGenerated(jvmti_env, name, address,length);

}

CJavaEventServer::CJavaEventServer(jvmtiEnv *jvmti_env)
{

	jvmtiError error;	
	jvmtiCapabilities cap, wantedcap;

	this->jvmti_env=jvmti_env;
 
	if (eventserver) //extra check
	{
		CJavaEventServer *old=eventserver;
		eventserver=NULL;
		delete eventserver;
	}


	
	swprintf(pipename, 256,L"\\\\.\\pipe\\cejavaevents_pid%d", GetCurrentProcessId());
	pipehandle=CreateNamedPipe(pipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 16, INFINITE, NULL);
	ConnectNamedPipe(pipehandle, NULL);

	
	eventserver=this;

	

	jvmti_env->GetPotentialCapabilities(&cap);	
	
	
	if (cap.can_generate_compiled_method_load_events)
	{

		jvmti_env->GetCapabilities(&wantedcap);
		wantedcap.can_generate_compiled_method_load_events=1;
		error=jvmti_env->AddCapabilities(&wantedcap);
		if (error!=JVMTI_ERROR_NONE)
		{
			OutputDebugStringA("Failure adding can_generate_compiled_method_load_events to my capabilities");
		}
		else
		{



			callbacks.CompiledMethodLoad=::MethodLoad;
			callbacks.CompiledMethodUnload=::MethodUnload;
			callbacks.DynamicCodeGenerated=::DynamicCodeGenerated;

			error=jvmti_env->SetEventCallbacks(&callbacks, sizeof(callbacks));

			if (error==JVMTI_ERROR_NONE)
			{			
				jvmti_env->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_COMPILED_METHOD_LOAD, NULL);
				jvmti_env->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_COMPILED_METHOD_UNLOAD, NULL);
				jvmti_env->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_DYNAMIC_CODE_GENERATED, NULL);
				error=jvmti_env->GenerateEvents(JVMTI_EVENT_COMPILED_METHOD_LOAD);
				error=jvmti_env->GenerateEvents(JVMTI_EVENT_DYNAMIC_CODE_GENERATED);		
			}

		}
	}
	else
	{
		OutputDebugStringA("can_generate_compiled_method_load_events == FALSE");
	}



	
	
}

CJavaEventServer::~CJavaEventServer(void)
{
	Terminate();

	
}

void CJavaEventServer::MethodLoad(jvmtiEnv *jvmti_env, jmethodID method, jint code_size, const void* code_addr)
{
	/*
OutputDebugStringA("MethodLoad");
	char *name=NULL, *sig=NULL, *gen=NULL;
	char *classsig=NULL, *classgen=NULL;
	
	jclass klass;

	jvmti_env->GetMethodDeclaringClass(method, &klass);
	jvmti_env->GetClassSignature(klass, &classsig, &classgen);

	OutputDebugStringA(classsig);

	
	jvmti_env->GetMethodName(method, &name, &sig, &gen);

	OutputDebugStringA(name);
	OutputDebugStringA("\n");

	


	*/
	Lock();
	try
	{
		jclass klass; 
		char *name=NULL, *sig=NULL, *gen=NULL;
		char *classsig=NULL, *classgen=NULL;
		WORD len;

		WriteByte(EVENTCMD_METHODLOAD);
		WriteQword((UINT_PTR)method);
		WriteDword(code_size);
		WriteQword((UINT_PTR)code_addr);

		jvmti_env->GetMethodDeclaringClass(method, &klass);  //when this function returns jklass gets dereferenced.  If this was the server I'd have to use DeleteLocalRef on this
		jvmti_env->GetClassSignature(klass, &classsig, &classgen);
		jvmti_env->GetMethodName(method, &name, &sig, &gen);

		if (classsig)
		{
			len=strlen(classsig);
			WriteWord(len);
			if (len)
				Write(classsig, len);

			jvmti_env->Deallocate((unsigned char *)classsig);
		}
		else
			WriteWord(0);
		

		if (classgen)
			jvmti_env->Deallocate((unsigned char *)classgen);


		if (name)
		{
			len=strlen(name);
			WriteWord(len);
			if (len)
				Write(name, len);

			jvmti_env->Deallocate((unsigned char *)name);
		}
		else
			WriteWord(0);

		if (sig)
		{
			len=strlen(sig);
			WriteWord(len);
			if (len)
			  Write(sig, len);

			jvmti_env->Deallocate((unsigned char *)sig);
		}
		else
			WriteWord(0);

		if (gen)
			jvmti_env->Deallocate((unsigned char *)gen);

		

	}
	catch (char *e)
	{
		OutputDebugStringA(e);
		//no connection yet
	}

	Unlock();	

}

void CJavaEventServer::MethodUnload(jvmtiEnv *jvmti_env, jmethodID method, const void* code_addr)
{
	Lock();
	try
	{
		WriteByte(EVENTCMD_METHODUNLOAD);
		WriteQword((UINT_PTR)method);
		WriteQword((UINT_PTR)code_addr);
	}
	catch (char *e)
	{
		OutputDebugStringA(e);
		//no connection yet
	}
	Unlock();
}

void CJavaEventServer::DynamicCodeGenerated(jvmtiEnv *jvmti_env, const char* name, const void* address, jint length)
{
	Lock();
	try
	{
		WriteByte(EVENTCMD_DYNAMICCODEGENERATED);
		WriteQword((UINT_PTR)address);
		WriteDword(length);
		WriteWord(strlen(name));
		Write((void *)name, strlen(name));
		
	}
	catch (char *e)
	{
		OutputDebugStringA(e);
		//no connection yet
	}
	Unlock();
}

void CJavaEventServer::Terminate(void)
{
	
	jvmtiCapabilities caps;

	jvmti_env->SetEventNotificationMode(JVMTI_DISABLE, JVMTI_EVENT_COMPILED_METHOD_LOAD, NULL);
	jvmti_env->SetEventNotificationMode(JVMTI_DISABLE, JVMTI_EVENT_COMPILED_METHOD_UNLOAD, NULL);
	jvmti_env->SetEventNotificationMode(JVMTI_DISABLE, JVMTI_EVENT_DYNAMIC_CODE_GENERATED, NULL);

	ZeroMemory(&caps, sizeof(caps));

	caps.can_generate_compiled_method_load_events=1;

	jvmti_env->RelinquishCapabilities(&caps);	

	eventserver=NULL;


	Lock();
	try
	{
		WriteByte(EVENTCMD_TERMINATED);
	}
	catch (char *e)
	{
		OutputDebugStringA(e);
	}
	Unlock();
	Sleep(500);
}

