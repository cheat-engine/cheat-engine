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



jvmtiIterationControl JNICALL initialHeapIterate(jlong class_tag, jlong size, jlong* tag_ptr, void* user_data)
{
	//OutputDebugStringA("Tagging object\n");
	*tag_ptr=1;
	return JVMTI_ITERATION_CONTINUE;
}


JNIEXPORT jint JNICALL Agent_OnAttach(JavaVM* vm, char* options, void* reserved)
{
	jvmtiEnv *env;
	JNIEnv *jni;
	jvmtiError error;

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


	jvmtiCapabilities cap, wantedcap;

	env->GetPotentialCapabilities(&cap);	
	


	if (cap.can_tag_objects)
	{
		env->GetCapabilities(&wantedcap);
		wantedcap.can_tag_objects=1;
		error=env->AddCapabilities(&wantedcap);

		/*
		if (error==JVMTI_ERROR_NONE)
		{
			error=env->IterateOverHeap(JVMTI_HEAP_OBJECT_EITHER, initialHeapIterate, NULL);

			if (error==JVMTI_ERROR_NONE)
			{
				int i;
				jlong tags[1];
				jint count;
				jobject *list;
				jlong *taglist;

				tags[0]=1;
				error=env->GetObjectsWithTags(1, tags, &count, &list, &taglist);
				for (i=0; i<count; i++)
				{
					jobject j1,j2;
					j1=list[i];
					j2=jni->NewGlobalRef(j1);

					if (j2==NULL)
						OutputDebugStringA("Failure\n");
				}
			}
		}
		*/
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