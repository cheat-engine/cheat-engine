#pragma once
#include "pipe.h"

//pipe for transmitting java events like method load/free

#define EVENTCMD_METHODLOAD 0
#define EVENTCMD_METHODUNLOAD 1
#define EVENTCMD_DYNAMICCODEGENERATED 2
#define EVENTCMD_FIELDMODIFICATION 3
#define EVENTCMD_TERMINATED 255

using namespace std;

typedef struct
{
	jfieldID fieldid;
	jclass klass;
	jobject object;
} FindWhatWritesEntry, *PFindWhatWritesEntry;

class CJavaEventServer :
	public Pipe
{
private:
	wchar_t pipename[256];
	jvmtiEnv *jvmti_env;
	vector<PFindWhatWritesEntry> FindWhatWritesList;

public:
	CJavaEventServer(jvmtiEnv *jvmti_env);
	~CJavaEventServer(void);

	void MethodLoad(jvmtiEnv *jvmti_env, jmethodID method, jint code_size, const void* code_addr);
	void MethodUnload(jvmtiEnv *jvmti_env, jmethodID method, const void* code_addr);
	void DynamicCodeGenerated(jvmtiEnv *jvmti_env, const char* name, const void* address, jint length);
	void FieldModification(jvmtiEnv *jvmti_env, JNIEnv* jni_env, jthread thread, jmethodID method, jlocation location, jclass field_klass, jobject object, jfieldID field, char signature_type, jvalue new_value);
	int RegisterFindWhatWrites(JNIEnv* jni, jobject object, jclass klass, jfieldID fieldid);
	void UnregisterFindWhatWrites(JNIEnv* jni, int id);
	void Terminate(void);
};

extern CJavaEventServer *eventserver;
extern CJavaEventServer *old_eventserver;
extern jvmtiEventCallbacks callbacks;
