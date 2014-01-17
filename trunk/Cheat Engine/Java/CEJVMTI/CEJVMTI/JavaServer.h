#pragma once



class CJavaServer : Pipe
{
private:
	jvmtiEnv* jvmti;
	JNIEnv* jni;

	wchar_t datapipename[256];
public:
	CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env);
	~CJavaServer(void);

	void Start(void);

};
