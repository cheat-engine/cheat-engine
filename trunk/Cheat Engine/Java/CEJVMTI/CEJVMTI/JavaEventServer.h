#pragma once
#include "pipe.h"

//pipe for transmitting java events like method load/free

#define EVENTCMD_METHODLOAD 0
#define EVENTCMD_METHODUNLOAD 1
#define EVENTCMD_DYNAMICCODEGENERATED 2
#define EVENTCMD_TERMINATED 255


class CJavaEventServer :
	public Pipe
{
private:
	wchar_t pipename[256];
	jvmtiEnv *jvmti_env;

public:
	CJavaEventServer(jvmtiEnv *jvmti_env);
	~CJavaEventServer(void);

	void MethodLoad(jvmtiEnv *jvmti_env, jmethodID method, jint code_size, const void* code_addr);
	void MethodUnload(jvmtiEnv *jvmti_env, jmethodID method, const void* code_addr);
	void DynamicCodeGenerated(jvmtiEnv *jvmti_env, const char* name, const void* address, jint length);

	void Terminate(void);
};

extern CJavaEventServer *eventserver;
extern CJavaEventServer *old_eventserver;
extern jvmtiEventCallbacks callbacks;
