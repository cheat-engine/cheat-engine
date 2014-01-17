#include "StdAfx.h"
#include "JavaServer.h"

CJavaServer::CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env)
{
	//create a named pipe
	swprintf(datapipename, 256,L"\\\\.\\pipe\\cejavadc_pid%d", GetCurrentProcessId());
}

CJavaServer::~CJavaServer(void)
{

}

void CJavaServer::Start(void)
{

}
