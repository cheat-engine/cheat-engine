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

int tagcount=0;
jint fr_heap_reference_callback(jvmtiHeapReferenceKind reference_kind, const jvmtiHeapReferenceInfo* reference_info, jlong class_tag, 
								jlong referrer_class_tag, jlong size, jlong* tag_ptr, jlong* referrer_tag_ptr, jint length, void* user_data)
{
	CJavaServer *js=(CJavaServer *)user_data;

	js->WriteByte(0); //fr_heap_reference_callback

	js->WriteDword((DWORD)reference_kind);

	
	js->WriteDword(referrer_class_tag);
	js->WriteDword(class_tag);
	

	if (*tag_ptr==0) //assign a new tag
		*tag_ptr=0xce000000+tagcount++;

    if (referrer_tag_ptr)
	{
		if (*referrer_tag_ptr==0) //assign a new tag
			*referrer_tag_ptr=0xce000000+tagcount++;

		js->WriteDword(*referrer_tag_ptr);	
	}
	else
		js->WriteDword(0);

	js->WriteDword(*tag_ptr);


	

	return JVMTI_VISIT_OBJECTS;
}

jint JNICALL fr_primitive_field_callback(jvmtiHeapReferenceKind kind, const jvmtiHeapReferenceInfo* info, jlong object_class_tag, jlong* object_tag_ptr, 
								 jvalue value, jvmtiPrimitiveType value_type, void* user_data)
{
	CJavaServer *js=(CJavaServer *)user_data;

	js->WriteByte(1); //fr_primitive_field_callback
	js->WriteDword(object_class_tag);

	if (*object_tag_ptr==0) //assign a new tag
		*object_tag_ptr=0xce000000+tagcount++;

	js->WriteDword(*object_tag_ptr);

	return JVMTI_VISIT_OBJECTS;
}

jint JNICALL fr_array_primitive_value_callback(jlong class_tag, jlong size, jlong* tag_ptr, jint element_count, jvmtiPrimitiveType element_type, 
									   const void* elements, void* user_data)
{
	CJavaServer *js=(CJavaServer *)user_data;

	js->WriteByte(2); //fr_array_primitive_value_callback
	js->WriteDword(class_tag);

	if (*tag_ptr==0) //assign a new tag
		*tag_ptr=0xce000000+tagcount++;

	js->WriteDword(*tag_ptr);

	return JVMTI_VISIT_OBJECTS;
}

jint JNICALL fr_string_primitive_value_callback(jlong class_tag, jlong size, jlong* tag_ptr, const jchar* value, jint value_length, void* user_data)
{
	CJavaServer *js=(CJavaServer *)user_data;

	js->WriteByte(3); //fr_array_primitive_value_callback
	js->WriteDword(class_tag);

	if (*tag_ptr==0) //assign a new tag
		*tag_ptr=0xce000000+tagcount++;

	js->WriteDword(*tag_ptr);

	return JVMTI_VISIT_OBJECTS;
}


void CJavaServer::FollowReferences(void)
{
	jvmtiHeapCallbacks callbacks;
	jclass klass=(jclass)ReadQword();
	ZeroMemory(&callbacks, sizeof(jvmtiHeapCallbacks));

	callbacks.heap_reference_callback=fr_heap_reference_callback;
	callbacks.primitive_field_callback=fr_primitive_field_callback;
	callbacks.array_primitive_value_callback=fr_array_primitive_value_callback;
	callbacks.string_primitive_value_callback=fr_string_primitive_value_callback;

	jvmti->FollowReferences(0,klass,NULL, &callbacks, this);	

	WriteByte(0xff);
}


jint JNICALL fjo_heap_iteration_callback1(jlong class_tag, jlong size, jlong* tag_ptr, jint length, void* user_data)
{
	*tag_ptr=0xce;	
	return JVMTI_VISIT_OBJECTS;
}


void CJavaServer::FindjObject(void)
{	
	int i;
	jint count;
	jobject *objects;
	jlong *objecttags;
	jvmtiHeapCallbacks callbacks;
	UINT64 result=0;	
	UINT64 address=ReadQword();
	jlong tags[1];
	

	ZeroMemory(&callbacks, sizeof(jvmtiHeapCallbacks));

	

	callbacks.heap_iteration_callback=fjo_heap_iteration_callback1;

	jvmti->IterateThroughHeap(0, NULL, &callbacks, &tags);

	tags[0]=0xce;
	count=0;
	if (jvmti->GetObjectsWithTags(1, tags, &count, &objects, &objecttags)==JVMTI_ERROR_NONE)
	{
		for (i=0; i<count; i++)
		{
			UINT_PTR objectaddress=*(UINT_PTR*)(objects[i]);
			jlong size=0;
			jvmti->GetObjectSize(objects[i], &size);

			if ((address>=objectaddress) && (address<objectaddress+size))
			{			
				result=(UINT_PTR)objects[i];			
			}
			else
				jni->DeleteLocalRef(objects[i]);
		}

		jvmti->Deallocate((unsigned char *)objects);
		jvmti->Deallocate((unsigned char *)objecttags);
	}


	WriteQword(result);

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

					case JAVACMD_FOLLOWREFERENCES:
						FollowReferences();
						break;

					case JAVACMD_FINDJOBJECT:
						FindjObject();
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
