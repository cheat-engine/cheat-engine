// CEJVMTI.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "CEJVMTI.h"
#include "JavaServer.h"

void JNICALL AgentThread(jvmtiEnv* jvmti_env, JNIEnv* jni_env, void* arg)
{
	CJavaServer *s=new CJavaServer(jvmti_env, jni_env);
	s->Start();

	delete s;
	OutputDebugStringA("Still alive");

}

JNIEXPORT jint JNICALL Agent_OnAttach(JavaVM* vm, char* options, void* reserved)
{
	jvmtiEnv *env;
	JNIEnv *jni;
	jint r=vm->GetEnv((void **)&env, JVMTI_VERSION);
	if (r!=JNI_OK)
	{
		OutputDebugStringA("GetEnv(JVMTI_VERSION) failed");
		return 1;		
	}

	r=vm->GetEnv((void **)&jni, JNI_VERSION_1_6);
	if (r!=JNI_OK)
	{
		OutputDebugStringA("GetEnv(JNI_VERSION_1_6) failed");
		return 1;	
	}


	jclass threadclass=jni->FindClass("java/lang/Thread");
	if (threadclass==0)
	{
		OutputDebugStringA("jni->FindClass(\"java/lang/Thread\") failure");
		return 1;
	}

	jmethodID threadinit=jni->GetMethodID(threadclass, "<init>", "()V");
	if (threadinit==0)
	{
		OutputDebugStringA("jni->GetMethodID failure");
		return 1;
	}

	jthread t=jni->NewObject(threadclass, threadinit);

	env->RunAgentThread(t, AgentThread, NULL, JVMTI_THREAD_NORM_PRIORITY);

	
	return 1; //die

}