#pragma once

#include "JavaEventServer.h"

#define JAVACMD_STARTCODECALLBACKS 0
#define JAVACMD_STOPCODECALLBACKS 1
#define JAVACMD_GETLOADEDCLASSES 2
#define JAVACMD_DEREFERENCELOCALOBJECT 3
#define JAVACMD_GETCLASSMETHODS 4
#define JAVACMD_GETCLASSFIELDS 5
#define JAVACMD_GETIMPLEMENTEDINTERFACES 6

class CJavaServer : Pipe
{
private:
	wchar_t pipename[256];
	jvmtiEnv* jvmti;
	JNIEnv* jni;


	

	void CreatePipeandWaitForconnect(void);
public:
	CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env);
	~CJavaServer(void);

	void Start(void);


	void StartCodeCallbacks(void);
	void StopCodeCallbacks(void);
	void GetLoadedClasses(void);
	void DereferenceLocalObject(void);
	void GetClassMethods(void);
	void GetClassFields(void);
	void GetImplementedInterfaces(void);
};
