#pragma once

#include "JavaEventServer.h"

#define JAVACMD_STARTCODECALLBACKS 0
#define JAVACMD_STOPCODECALLBACKS 1
#define JAVACMD_GETLOADEDCLASSES 2
#define JAVACMD_DEREFERENCELOCALOBJECT 3
#define JAVACMD_GETCLASSMETHODS 4
#define JAVACMD_GETCLASSFIELDS 5
#define JAVACMD_GETIMPLEMENTEDINTERFACES 6
#define JAVAVMD_FINDREFERENCESTOOBJECT 7
#define JAVACMD_FINDJOBJECT 8
#define JAVACMD_GETCLASSSIGNATURE 9
#define JAVACMD_GETSUPERCLASS 10
#define JAVACMD_GETOBJECTCLASS 11
#define JAVACMD_GETCLASSDATA 12
#define JAVACMD_REDEFINECLASS 13
#define JAVACMD_FINDCLASS 14
#define JAVACMD_GETCAPABILITIES 15
#define JAVACMD_GETMETHODNAME 16
#define JAVACMD_INVOKEMETHOD 17
#define JAVACMD_FINDCLASSOBJECTS 18
#define JAVACMD_ADDTOBOOTSTRAPCLASSLOADERPATH 19
#define JAVACMD_ADDTOSYSTEMCLASSLOADERPATH 20
#define JAVACMD_PUSHLOCALFRAME 21
#define JAVACMD_POPLOCALFRAME 22


using namespace std;

class CJavaServer : public Pipe
{
private:
	wchar_t pipename[256];
	jvmtiEnv* jvmti;
	JNIEnv* jni;

	void SendClassSignature(jclass klass);
	void SendMethodName(jmethodID methodid);
	

	void CreatePipeandWaitForconnect(void);
public:
	CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env);
	~CJavaServer(void);

	void Start(void);


	void StartCodeCallbacks(void);
	void StopCodeCallbacks(void);
	void GetLoadedClasses(void);
	void DereferenceLocalObject(void);
	void PushLocalFrame(void);
	void PopLocalFrame(void);
	void GetClassMethods(void);
	void GetClassFields(void);
	void GetImplementedInterfaces(void);
	void FindReferencesToObject(void);
	void FindjObject(void);
	void GetClassSignature(void);
	void GetSuperClass(void);
	void GetObjectClass(void);
	void GetClassData(void);
	void RedefineClass(void);
	void FindClass(void);
	void GetCapabilities(void);
	void GetMethodName(void);
	void InvokeMethod(void);
	void FindClassObjects(void);
	void AddToBootstrapClassLoaderPath(void);
	void AddToSystemClassLoaderPath(void);

};
