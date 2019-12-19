// CEJVMTI.cpp : Defines the exported functions for the DLL application.
//

#include "stdafx.h"
#include "CEJVMTI.h"
#include "JavaServer.h"
#include "JavaEventServer.h"

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

int LaunchServer(jvmtiEnv *env, JNIEnv *jni)
{
	jclass threadclass=jni->FindClass("java/lang/Thread");
	if (threadclass==0)
	{
		OutputDebugStringA("jni->FindClass(\"java/lang/Thread\") failure");
		return 0;
	}

	jmethodID threadinit=jni->GetMethodID(threadclass, "<init>", "()V");
	if (threadinit==0)
	{
		OutputDebugStringA("jni->GetMethodID failure");
		return 0;
	}


	
	//example of calling a superclass method and calling it on a inherited object
	/*
	jclass classclass=jni->FindClass("java/lang/Class");
	jmethodID getSuperclass=jni->GetMethodID(classclass, "getSuperclass","()Ljava/lang/Class;");
	jobject sc=NULL, sc2=NULL;

	if (getSuperclass)
	{
		sc=jni->CallObjectMethod(threadclass, getSuperclass);
		sc2=jni->GetSuperclass(threadclass);

		if (*(UINT_PTR*)sc==*(UINT_PTR*)sc2)
			OutputDebugString('Success');

		jni->DeleteLocalRef(sc);
		jni->DeleteLocalRef(sc2);
	}
	*/

	jthread t=jni->NewObject(threadclass, threadinit);

	env->RunAgentThread(t, AgentThread, NULL, JVMTI_THREAD_NORM_PRIORITY);

	return 0;

}

void JNICALL VMInit(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread)
{
	LaunchServer(jvmti_env, jni_env);
}


jint Agent(JavaVM *vm, char *options, void *reserved, BOOL OnLoad)
{
	jvmtiError error;
	jvmtiCapabilities potcap, wantedcap;	
	jvmtiEnv *env;
	jint r=vm->GetEnv((void **)&env, JVMTI_VERSION);

	//I should probably have full control
	
	env->GetPotentialCapabilities(&potcap);	 //if everything is correct all potcap fields should be 1 here

	memset(&wantedcap,0, sizeof(wantedcap));

	OutputDebugStringA("Loaded");

	
	//runtime capabilities are caps that should be able to be used if the agent is loaded at runtime as well.
	//debug can only be used when loaded at start
	wantedcap.can_access_local_variables=potcap.can_access_local_variables; //debug
	wantedcap.can_generate_all_class_hook_events=potcap.can_generate_all_class_hook_events; //runtime class editing
	wantedcap.can_generate_breakpoint_events=potcap.can_generate_breakpoint_events; //debug
	wantedcap.can_generate_compiled_method_load_events=potcap.can_generate_compiled_method_load_events; //runtime lookup
	wantedcap.can_generate_field_access_events=potcap.can_generate_field_access_events; //find what reads
	wantedcap.can_generate_field_modification_events=potcap.can_generate_field_modification_events; //find what writes
	wantedcap.can_generate_single_step_events=potcap.can_generate_single_step_events; //debug, because why not...
	wantedcap.can_get_bytecodes=potcap.can_get_bytecodes;  //runtime lookup and debug
	wantedcap.can_get_constant_pool=potcap.can_get_constant_pool; //runtime lookup and debug
	wantedcap.can_maintain_original_method_order=potcap.can_maintain_original_method_order; //debug, whatever
	wantedcap.can_redefine_any_class=potcap.can_redefine_any_class; //runtime class editing
	wantedcap.can_redefine_classes=potcap.can_redefine_classes; //runtime class editing
	wantedcap.can_retransform_any_class=potcap.can_retransform_any_class; //runtime class editing
	wantedcap.can_retransform_classes=potcap.can_redefine_classes; //runtime class editing
	wantedcap.can_tag_objects=potcap.can_tag_objects; //runtime object searching


	error=env->AddCapabilities(&wantedcap);

	if (error!=JVMTI_ERROR_NONE)
	{
		OutputDebugStringA("Failure setting the wanted capabilities");
		return 0;
	}

	memset(&callbacks, 0, sizeof(callbacks));

	if (OnLoad)
	{
		//setup the callbacks
		callbacks.VMInit=VMInit;
		env->SetEventCallbacks(&callbacks, sizeof(callbacks));
		env->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_VM_INIT, NULL);

		return 0;
	}
	else
	{
		JNIEnv *jni;
		jint r;
		
		r=vm->GetEnv((void **)&jni, JNI_VERSION_1_6);
		if (r!=JNI_OK)
		{
			OutputDebugStringA("GetEnv(JNI_VERSION_1_6) failed");
			return 0;	
		}

		return LaunchServer(env, jni);
	}
}

JNIEXPORT jint JNICALL Agent_OnLoad(JavaVM *vm, char *options, void *reserved)
{
	return Agent(vm, options, reserved, TRUE);
}

JNIEXPORT jint JNICALL Agent_OnAttach(JavaVM* vm, char* options, void* reserved)
{
	return Agent(vm, options, reserved, FALSE);
}