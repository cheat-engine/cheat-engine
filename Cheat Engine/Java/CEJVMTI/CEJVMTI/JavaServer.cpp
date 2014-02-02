#include "StdAfx.h"
#include "JavaServer.h"


CJavaServer::CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env)
{
	//create a named pipe
	jvmtiCapabilities cap;


	this->jni=jni_env;
	this->jvmti=jvmti_env;


	jvmti->GetCapabilities(&cap);
	swprintf(pipename, 256,L"\\\\.\\pipe\\cejavadc_pid%d", GetCurrentProcessId());

	


}

void CJavaServer::CreatePipeandWaitForconnect(void)
{
	if ((pipehandle) && (pipehandle!=INVALID_HANDLE_VALUE))
	{
		CloseHandle(pipehandle);
		pipehandle=0;
	}

	pipehandle=CreateNamedPipe(pipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 1,256*1024, 16, INFINITE, NULL);
	ConnectNamedPipe(pipehandle, NULL);
}

CJavaServer::~CJavaServer(void)
{

}

void CJavaServer::StartCodeCallbacks(void)
{
	if (old_eventserver)
	  delete old_eventserver;

	eventserver=new CJavaEventServer(jvmti);
}

void CJavaServer::StopCodeCallbacks(void)
{
	if (eventserver)
		delete eventserver;
}

void CJavaServer::GetLoadedClasses(void)
{
	int i;
	jint classcount;
	jclass *classes;
	if (jvmti->GetLoadedClasses(&classcount, &classes)==JVMTI_ERROR_NONE) //note: this creates to the returned classes. Should be managed
	{
		

		WriteDword(classcount);
		for (i=0; i<classcount; i++)
		{
			char *sig=NULL;
			char *gen=NULL;

			WriteQword((UINT_PTR)classes[i]);

			jvmti->SetTag(classes[i], i+1); //let's tag this class while we're at it
			
			if (jvmti->GetClassSignature(classes[i], &sig, &gen)==JVMTI_ERROR_NONE)
			{
				int len;
				if (sig)
				{
					len=(int)strlen(sig);
					WriteWord(len);
					Write(sig, len);
					jvmti->Deallocate((unsigned char *)sig);
				}
				else
					WriteWord(0);

				if (gen)
				{
					len=(int)strlen(gen);
					WriteWord(len);
					Write(gen, len);
					jvmti->Deallocate((unsigned char *)gen);
				}
				else
					WriteWord(0);

			}
			else
			{
				WriteWord(0);
				WriteWord(0);
			}

		}

		jvmti->Deallocate((unsigned char *)classes);
	}
	else
		WriteDword(0); //0 classes
}

void CJavaServer::DereferenceLocalObject(void)
{
	jobject object;
	object=(jobject)ReadQword();

	jni->DeleteLocalRef(object);

}

void CJavaServer::GetClassMethods(void)
{
	jclass klass=(jclass)ReadQword();
	jint count;
	jmethodID *methods=NULL;

	if (jvmti->GetClassMethods(klass, &count, &methods)==JVMTI_ERROR_NONE)
	{
		int i;
		WriteDword(count);
		for (i=0; i<count; i++)
		{
			int len;
			char *name, *sig, *gen;
			WriteQword(UINT64(methods[i]));
			if (jvmti->GetMethodName(methods[i], &name, &sig, &gen)==JVMTI_ERROR_NONE)
			{
				if (name)
				{
					len=(int)strlen(name);
					WriteWord(len);
					Write(name, len);					
					jvmti->Deallocate((unsigned char *)name);
				}
				else
					WriteWord(0);

				if (sig)
				{
					len=(int)strlen(sig);
					WriteWord(len);
					Write(sig, len);					
					jvmti->Deallocate((unsigned char *)sig);
				}
				else
					WriteWord(0);

				if (gen)
				{
					len=(int)strlen(gen);
					WriteWord(len);
					Write(gen, len);					
					jvmti->Deallocate((unsigned char *)gen);
				}
				else
					WriteWord(0);
			}
			else
			{
				WriteWord(0);
				WriteWord(0);
				WriteWord(0);
			}
				
		}
		jvmti->Deallocate((unsigned char *)methods);

	}
	else
		WriteDword(0);

	
}

void CJavaServer::GetClassFields(void)
{
	jint count;
	jfieldID *fields=NULL;	
	jclass klass=(jclass)ReadQword();
	if (jvmti->GetClassFields(klass, &count, &fields)==JVMTI_ERROR_NONE)
	{
		int i;
		WriteDword(count);
		for (i=0; i<count; i++)
		{		
			int len;
			char *name=NULL;
			char *sig=NULL;
			char *gen=NULL;

			WriteQword((UINT_PTR)fields[i]);

			if (jvmti->GetFieldName(klass, fields[i], &name, &sig, &gen)==JVMTI_ERROR_NONE)
			{
				if (name)
				{
					len=(int)strlen(name);
					WriteWord(len);
					Write(name, len);					
					jvmti->Deallocate((unsigned char *)name);
				}
				else
					WriteWord(0);

				if (sig)
				{
					len=(int)strlen(sig);
					WriteWord(len);
					Write(sig, len);					
					jvmti->Deallocate((unsigned char *)sig);
				}
				else
					WriteWord(0);

				if (gen)
				{
					len=(int)strlen(gen);
					WriteWord(len);
					Write(gen, len);					
					jvmti->Deallocate((unsigned char *)gen);
				}
				else
					WriteWord(0);
			}
			else
			{
				WriteWord(0);
				WriteWord(0);
				WriteWord(0);
			}
				

		}
	}
	else
		WriteDword(0);

}

void CJavaServer::GetImplementedInterfaces(void)
{
	//instead of returning a reference, return the tags of the classes
	jint count;
	jclass *interfaces;
	jclass klass=(jclass)ReadQword();
	if (jvmti->GetImplementedInterfaces(klass, &count, &interfaces)==JVMTI_ERROR_NONE)
	{
		int i;
		WriteDword(count);
		
		for (i=0; i<count; i++)			
		{
			jlong tag=0;
			jvmti->GetTag(interfaces[i], &tag);			
			WriteDword(tag);

			jni->DeleteLocalRef(interfaces[i]);
		}


		if (interfaces)
			jvmti->Deallocate((unsigned char *)interfaces);
		
	}
	else
		WriteDword(0);

}


void CJavaServer::Start(void)
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
				switch (command)
				{
					case JAVACMD_STARTCODECALLBACKS:
						StartCodeCallbacks();					
						break;

					case JAVACMD_STOPCODECALLBACKS:
						StopCodeCallbacks();
						break;

					case JAVACMD_GETLOADEDCLASSES:
						GetLoadedClasses();
						break;

					case JAVACMD_DEREFERENCELOCALOBJECT:
						DereferenceLocalObject();
						break;

					case JAVACMD_GETCLASSMETHODS:
						GetClassMethods();
						break;

					case JAVACMD_GETCLASSFIELDS:
						GetClassFields();
						break;

					case JAVACMD_GETIMPLEMENTEDINTERFACES:
						GetImplementedInterfaces();
						break;

				}
			}
		}
		catch (char *e)
		{
			//Pipe error, or something else that wasn't caught. Exit the connection and start over	
			OutputDebugStringA("Pipe error:\n");
			OutputDebugStringA(e);

			old_eventserver=eventserver;
			eventserver=NULL;
			if (old_eventserver)
				old_eventserver->Terminate();			
		}
		catch (...)
		{
			OutputDebugStringA("Unexpected pipe error\n");			

			old_eventserver=eventserver;
			eventserver=NULL;
		}
	}

}
