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

void JNICALL FieldModification(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method, jlocation location, jclass field_klass, jobject object, jfieldID field, char signature_type, jvalue new_value)
{
	if (eventserver)
		eventserver->FieldModification(jvmti_env, jni_env, thread, method, location, field_klass, object, field, signature_type, new_value);

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
			callbacks.FieldModification=::FieldModification;

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
			len=(WORD)strlen(classsig);
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
			len=(WORD)strlen(name);
			WriteWord(len);
			if (len)
				Write(name, len);

			jvmti_env->Deallocate((unsigned char *)name);
		}
		else
			WriteWord(0);

		if (sig)
		{
			len=(WORD)strlen(sig);
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
		WriteWord((WORD)strlen(name));
		Write((void *)name, (int)strlen(name));
		
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

void CJavaEventServer::FieldModification(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method, jlocation location, jclass field_klass, jobject object, jfieldID field, char signature_type, jvalue new_value)
{
	unsigned int i;
	//go through the list and check which one caused this

	
	for (i=0; i<FindWhatWritesList.size(); i++)
	{
		PFindWhatWritesEntry e=FindWhatWritesList[i];
		if (e)
		{			
			if ((e->fieldid==field) && ((object==NULL) || (jni_env->IsSameObject(e->object, object))))
			{
				//signal ce
				Lock();
				try
				{
					jvmtiFrameInfo frames[10];
					jint count;

					WriteByte(EVENTCMD_FIELDMODIFICATION);
					WriteDword(i); //id
					WriteQword((UINT_PTR)method);
					WriteQword((UINT_PTR)location);

					//and a stack snapshot as well
					
					if (jvmti_env->GetStackTrace(thread, 0, 10, frames, &count)==JVMTI_ERROR_NONE)
					{						
						int i;
						WriteByte((BYTE)count);

						for (i=0; i<count; i++)
						{
							WriteQword((UINT_PTR)frames[i].method);
							WriteQword((UINT_PTR)frames[i].location);													
						}
					}
					else
						WriteByte(0);
				}
				catch (char *e)
				{
					OutputDebugStringA(e);
					//no connection yet
				}
				Unlock();

			}			
		}

	}
}

int CJavaEventServer::RegisterFindWhatWrites(JNIEnv* jni, jobject object, jclass klass, jfieldID fieldid)
{
	int id=-1;

	//find an usable entry in the FindWhatWritesList
	unsigned int i;
		
	for (i=0; i<FindWhatWritesList.size(); i++)
	{
		if (FindWhatWritesList[i]==NULL)
		{
			id=i;
			break;
		}
	}

	


	if (id==-1) 
	{
		PFindWhatWritesEntry newentry=(PFindWhatWritesEntry)malloc(sizeof(FindWhatWritesEntry));
		FindWhatWritesList.push_back(newentry);

		id=(int)FindWhatWritesList.size()-1;
	}

	klass=(jclass)jni->NewGlobalRef(klass); //upgrade to a global

	FindWhatWritesList[id]->klass=klass;
	FindWhatWritesList[id]->fieldid=fieldid;
	FindWhatWritesList[id]->object=object;
	

	jvmti_env->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_FIELD_MODIFICATION, NULL);

	if (jvmti_env->SetFieldModificationWatch(klass, fieldid)!=JVMTI_ERROR_NONE)	
		UnregisterFindWhatWrites(jni, id);		
		
	return id;
}

void CJavaEventServer::UnregisterFindWhatWrites(JNIEnv* jni, int id)
{
	
	if (FindWhatWritesList[id])
	{
		jvmti_env->ClearFieldModificationWatch(FindWhatWritesList[id]->klass, FindWhatWritesList[id]->fieldid);

		jni->DeleteGlobalRef(FindWhatWritesList[id]->klass);

		free(FindWhatWritesList[id]);
		FindWhatWritesList[id]=NULL;
	}

	
}
