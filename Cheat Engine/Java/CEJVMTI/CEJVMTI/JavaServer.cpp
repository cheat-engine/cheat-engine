#include "StdAfx.h"
#include "JavaServer.h"

int serverid=0;
int tagcount=0;

CJavaServer::CJavaServer(jvmtiEnv* jvmti_env, JNIEnv* jni_env)
{
	//create a named pipe
	jvmtiCapabilities cap;


	this->jni=jni_env;
	this->jvmti=jvmti_env;


	jvmti->GetCapabilities(&cap);

	if (serverid==0)
		swprintf(pipename, 256,L"\\\\.\\pipe\\cejavadc_pid%d", GetCurrentProcessId());
	else
		swprintf(pipename, 256,L"\\\\.\\pipe\\cejavadc_pid%d_%d", GetCurrentProcessId(),serverid);


	serverid++;
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
			SendClassSignature(classes[i]);
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

void CJavaServer::PushLocalFrame(void)
{
	jint count=ReadWord();	
	jni->PushLocalFrame(count);	
}

void CJavaServer::PopLocalFrame(void)
{
	jobject result=(jobject)ReadQword();
	result=jni->PopLocalFrame(result);

	WriteQword((UINT_PTR)result);
}


void CJavaServer::SendMethodName(jmethodID methodid)
{
	char *name, *sig, *gen;
	int len;

	if (jvmti->GetMethodName(methodid, &name, &sig, &gen)==JVMTI_ERROR_NONE)
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
			WriteQword(UINT64(methods[i]));
			SendMethodName(methods[i]);				
		}
		jvmti->Deallocate((unsigned char *)methods);

	}
	else
		WriteDword(0);

	
}

void CJavaServer::SendFieldName(jclass klass, jfieldID field)
{
	int len;
	char *name=NULL;
	char *sig=NULL;
	char *gen=NULL;
	if (jvmti->GetFieldName(klass, field, &name, &sig, &gen)==JVMTI_ERROR_NONE)
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
			WriteQword((UINT_PTR)fields[i]);
			SendFieldName(klass, fields[i]);
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
			WriteQword((UINT_PTR)interfaces[i]);
		
		if (interfaces)
			jvmti->Deallocate((unsigned char *)interfaces);
		
	}
	else
		WriteQword(0);

}


jint JNICALL fr_heap_reference_callback(jvmtiHeapReferenceKind reference_kind, const jvmtiHeapReferenceInfo* reference_info, jlong class_tag, 
								jlong referrer_class_tag, jlong size, jlong* tag_ptr, jlong* referrer_tag_ptr, jint length, void* user_data)
{
	if ((*tag_ptr==0xce) && (referrer_tag_ptr) && (*referrer_tag_ptr!=0xce))		
		*referrer_tag_ptr=0xcd;

	return JVMTI_VISIT_OBJECTS;
}

/*
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
}*/


void CJavaServer::FindReferencesToObject(void)
{
	jvmtiHeapCallbacks callbacks;
	jobject object=(jclass)ReadQword();
	jlong tags[1];
	jint count;
	jobject *objects;
	jlong *objecttags;
	int i;
	

	ZeroMemory(&callbacks, sizeof(jvmtiHeapCallbacks));

	jvmti->SetTag(object, 0xce);


	callbacks.heap_reference_callback=fr_heap_reference_callback;
	//callbacks.primitive_field_callback=fr_primitive_field_callback;
	//callbacks.array_primitive_value_callback=fr_array_primitive_value_callback;
	//callbacks.string_primitive_value_callback=fr_string_primitive_value_callback;

	jvmti->FollowReferences(0,NULL,NULL, &callbacks, this);	

	tags[0]=0xcd;
	if (jvmti->GetObjectsWithTags(1, tags, &count, &objects, &objecttags)==JVMTI_ERROR_NONE)
	{
		WriteDword(count);
		for (i=0; i<count; i++)
		{			
			WriteQword((UINT_PTR)objects[i]);
			jvmti->SetTag(objects[i],0); //untag
		}

		jvmti->Deallocate((unsigned char *)objects);
		jvmti->Deallocate((unsigned char *)objecttags);
	}
	else
	{
		WriteDword(0);
	}

	jvmti->SetTag(object, 0); //untag this object
}


jint JNICALL fjo_heap_iteration_callback1(jlong class_tag, jlong size, jlong* tag_ptr, jint length, void* user_data)
{
	*tag_ptr=0xf0;	
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

	tags[0]=0xf0;
	count=0;
	if (jvmti->GetObjectsWithTags(1, tags, &count, &objects, &objecttags)==JVMTI_ERROR_NONE)
	{
		jobject possible=NULL;
			 
		for (i=0; i<count; i++)
		{
			UINT_PTR objectaddress=*(UINT_PTR*)(objects[i]);
			jlong size=0;
			jvmti->GetObjectSize(objects[i], &size);

			if ((address>=objectaddress) && (address<objectaddress+size))
			{
				//this can be called multiple times.
				//pick the one with the closest addresses
				if (possible)
				{					
					UINT_PTR previousObjectAddress=*(UINT_PTR*)possible;

					if (objectaddress>previousObjectAddress)
					{	
						//this one is closer
						jni->DeleteLocalRef(possible);
						possible=objects[i];
					}
					else
						jni->DeleteLocalRef(objects[i]); //not closer
				}
				else
					possible=objects[i];
				
				result=(UINT_PTR)possible;		//keep the reference

			}
			else
				jni->DeleteLocalRef(objects[i]);

			jvmti->SetTag(objects[i],0); //untag
		}

		jvmti->Deallocate((unsigned char *)objects);
		jvmti->Deallocate((unsigned char *)objecttags);
	}

	


	WriteQword(result);

}


void CJavaServer::SendClassSignature(jclass klass)
{
	char *sig,*gen;

	if (jvmti->GetClassSignature(klass, &sig, &gen)==JVMTI_ERROR_NONE)
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

void CJavaServer::GetClassSignature(void)
{
	jclass klass=(jclass)ReadQword();
	SendClassSignature(klass);
}

void CJavaServer::GetSuperClass()
{	
	jclass klass=(jclass)ReadQword();
	jclass sklass=jni->GetSuperclass(klass);
	
	
	WriteQword((UINT_PTR)sklass);
}

void CJavaServer::GetObjectClass()
{
	jobject object=(jobject)ReadQword();
	jclass klass=jni->GetObjectClass(object);
	WriteQword((UINT_PTR)klass);

	
	
}


jclass GetClassDataTargetClass=NULL;
CJavaServer *GetClassDataCaller=NULL;
void JNICALL GetClassDataClassFileLoadHook(jvmtiEnv *jvmti_env,
     JNIEnv* jni_env,
     jclass class_being_redefined,
     jobject loader,
     const char* name,
     jobject protection_domain,
     jint class_data_len,
     const unsigned char* class_data,
     jint* new_class_data_len,
     unsigned char** new_class_data)
{
	if ((GetClassDataTargetClass) && (class_being_redefined) && ((*(UINT_PTR*)class_being_redefined)==(*(UINT_PTR*)GetClassDataTargetClass)))
	{
		//this is the class...
		GetClassDataCaller->WriteDword(class_data_len);
		GetClassDataCaller->Write((PVOID)class_data, class_data_len);
	}

}

void CJavaServer::GetClassData()
{
	jvmtiError error;
	jvmtiCapabilities cap, potcap, newcap;
	jclass klass=(jclass)ReadQword();


	jvmti->GetCapabilities(&cap);
	if (cap.can_retransform_classes==0)
	{
		memset(&newcap, 0, sizeof(newcap));
		jvmti->GetPotentialCapabilities(&potcap);

		if (potcap.can_retransform_classes)
			newcap.can_retransform_classes=1;

		if (potcap.can_retransform_any_class)
			newcap.can_retransform_any_class=1;

		jvmti->AddCapabilities(&newcap);
	}

	GetClassDataTargetClass=klass;
	GetClassDataCaller=this;

	callbacks.ClassFileLoadHook=GetClassDataClassFileLoadHook;
	error=jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));	
	error=jvmti->SetEventNotificationMode(JVMTI_ENABLE, JVMTI_EVENT_CLASS_FILE_LOAD_HOOK, NULL);



	error=jvmti->RetransformClasses(1, &klass);

	error=jvmti->SetEventNotificationMode(JVMTI_DISABLE, JVMTI_EVENT_CLASS_FILE_LOAD_HOOK, NULL);
	callbacks.ClassFileLoadHook=NULL;
	error=jvmti->SetEventCallbacks(&callbacks, sizeof(callbacks));	
}

void CJavaServer::RedefineClass(void)
{
	jvmtiError error;
	jvmtiCapabilities cap, potcap, newcap;
	jvmtiClassDefinition class_definition;
	class_definition.klass=(jclass)ReadQword();
	class_definition.class_byte_count=ReadDword();
	class_definition.class_bytes=(unsigned char *)malloc(class_definition.class_byte_count);
	Read((PVOID)class_definition.class_bytes, class_definition.class_byte_count); 


	jvmti->GetCapabilities(&cap);
	if (cap.can_redefine_classes==0)
	{
		memset(&newcap, 0, sizeof(newcap));
		jvmti->GetPotentialCapabilities(&potcap);

		if (potcap.can_redefine_classes)
			newcap.can_redefine_classes=1;

		if (potcap.can_redefine_any_class)
			newcap.can_redefine_any_class=1;

		jvmti->AddCapabilities(&newcap);
	}
	error=jvmti->RedefineClasses(1, &class_definition);

    if (class_definition.class_bytes)
		free((void *)class_definition.class_bytes);
}

void CJavaServer::FindClass(void)
{
	jclass klass;
	char *sig;
	WORD siglen=ReadWord();
	sig=(char *)malloc(siglen+1);
	Read(sig, siglen);
	sig[siglen]=0;

	klass=jni->FindClass(sig);
	WriteQword((UINT_PTR)klass);

	if (sig)
		free((void *)sig);
}

void CJavaServer::GetCapabilities(void)
{
	jvmtiCapabilities cap;
	jvmti->GetCapabilities(&cap);

	WriteByte(cap.can_access_local_variables);
	WriteByte(cap.can_generate_all_class_hook_events);
	WriteByte(cap.can_generate_breakpoint_events);
	WriteByte(cap.can_generate_compiled_method_load_events);
	WriteByte(cap.can_generate_field_access_events);
	WriteByte(cap.can_generate_field_modification_events);
	WriteByte(cap.can_generate_single_step_events);
	WriteByte(cap.can_get_bytecodes);
	WriteByte(cap.can_get_constant_pool);
	WriteByte(cap.can_maintain_original_method_order);
	WriteByte(cap.can_redefine_any_class);
	WriteByte(cap.can_redefine_classes);
	WriteByte(cap.can_retransform_any_class);
	WriteByte(cap.can_retransform_classes);
	WriteByte(cap.can_tag_objects);	
}

void CJavaServer::GetMethodName(void)
{
	jmethodID methodid=(jmethodID)ReadQword();
	SendMethodName(methodid);
}

void CJavaServer::InvokeMethod(void)
{
	jobject obj=(jobject)ReadQword();
	jmethodID methodid=(jmethodID)ReadQword();


	int returnType=ReadByte();
	int argcount=ReadByte();

    jvalue *args=(jvalue *)malloc(sizeof(jvalue)*argcount);
	jobjectArray *arrays=(jobjectArray *)calloc(argcount, sizeof(jobjectArray));

	int i;		
	
	for (i=0; i<argcount; i++)
	{
		int typeID=ReadByte();
		args[i].j=0; //clean it

		switch (typeID)
		{
			case 1:  //boolean
				args[i].z=ReadByte()!=0;
				break;

			case 2:  //byte
				args[i].b=ReadByte();
				break;

			case 3:  //char
			case 4:  //short
				args[i].c=ReadWord();
				break;

			case 5:  //int
			case 7:  //float
				args[i].i=ReadDword();					
				break;

			case 6:  //long
			case 8:  //double
			case 9:  //object
				args[i].j=ReadQword();					
				break;

			case 11: //boolean array
			{
				int len=ReadDword();
				jboolean *b=(jboolean *)malloc(sizeof(jboolean)*len);

				Read(b, len*sizeof(jboolean));

				//create an array
				arrays[i]=(jobjectArray)jni->NewBooleanArray(len);
				jni->SetBooleanArrayRegion((jbooleanArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 12: //byte array
			{
				int len=ReadDword();
				jbyte *b=(jbyte *)malloc(sizeof(jbyte)*len);

				Read(b, len*sizeof(jbyte));

				//create an array
				arrays[i]=(jobjectArray)jni->NewByteArray(len);
				jni->SetByteArrayRegion((jbyteArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 13: //char array
			{
				int len=ReadDword();
				jchar *b=(jchar *)malloc(sizeof(jchar)*len);

				Read(b, len*sizeof(jchar));

				//create an array
				arrays[i]=(jobjectArray)jni->NewCharArray(len);
				jni->SetCharArrayRegion((jcharArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 14: //short array
			{
				int len=ReadDword();
				jshort *b=(jshort *)malloc(sizeof(jshort)*len);

				Read(b, len*sizeof(jshort));

				//create an array
				arrays[i]=(jobjectArray)jni->NewShortArray(len);
				jni->SetShortArrayRegion((jshortArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 15: //int array
			{
				int len=ReadDword();
				jint *b=(jint *)malloc(sizeof(jint)*len);

				Read(b, len*sizeof(jint));			

				//create an array
				arrays[i]=(jobjectArray)jni->NewIntArray(len);
				jni->SetIntArrayRegion((jintArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 16: //long array
			{
				int len=ReadDword();
				jlong *b=(jlong *)malloc(sizeof(jlong)*len);

				Read(b, len*sizeof(jlong));			

				//create an array
				arrays[i]=(jobjectArray)jni->NewLongArray(len);
				jni->SetLongArrayRegion((jlongArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 17: //float array
			{
				int len=ReadDword();
				jfloat *b=(jfloat *)malloc(sizeof(jfloat)*len);

				Read(b, len*sizeof(jfloat));			

				//create an array
				arrays[i]=(jobjectArray)jni->NewFloatArray(len);
				jni->SetFloatArrayRegion((jfloatArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 18: //double array
			{
				int len=ReadDword();
				jdouble *b=(jdouble *)malloc(sizeof(jdouble)*len);

				Read(b, len*sizeof(jdouble));			

				//create an array
				arrays[i]=(jobjectArray)jni->NewDoubleArray(len);
				jni->SetDoubleArrayRegion((jdoubleArray)arrays[i], 0, len, b); //fill it
				args[i].l=arrays[i];

				free(b);

				break;
			}

			case 19: //object array
			{
				int len=ReadDword();
				jobject *b=(jobject *)malloc(sizeof(jobject)*len);

				int j;
				for (j=0; j<len; j++) //loop because the array sent is using 64-bit at all time, even in 32-bit
					b[j]=(jobject)ReadQword();			

				//create an array
				if (len)
				{
					jclass clazz=jni->GetObjectClass(b[0]);					
					arrays[i]=(jobjectArray)jni->NewObjectArray(len,clazz, NULL);
					jni->DeleteLocalRef(clazz);

					for (j=0; j<len; j++)
						jni->SetObjectArrayElement((jobjectArray)arrays[i], j, b[j]);					
				}
				args[i].l=arrays[i];
				

				free(b);

				break;
			}
		}
	} 

	//parameters are setup, do the call

	switch (returnType)
	{
		case 0:
		{
			jni->CallVoidMethodA(obj, methodid, args);
			WriteQword(0); //tell it it's done
			break;
		}

		case 1:
		{
			jboolean r=jni->CallBooleanMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 2:
		{
			jbyte r=jni->CallByteMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 3:
		{
			jchar r=jni->CallCharMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 4:
		{
			jshort r=jni->CallShortMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 5:
		{
			jint r=jni->CallIntMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 6:
		{
			jlong r=jni->CallLongMethodA(obj, methodid, args);
			WriteQword(r);
			break;
		}

		case 7:
		{
			jfloat r=jni->CallFloatMethodA(obj, methodid, args);
			WriteQword(*(DWORD *)&r);
			break;
		}

		case 8:
		{
			jdouble r=jni->CallDoubleMethodA(obj, methodid, args);
			WriteQword(*(UINT64 *)&r);
			break;
		}

		case 9:
		{
			jobject r=jni->CallObjectMethodA(obj, methodid, args);
			WriteQword((UINT_PTR)r);
			break;
		}
	}




	//cleanup
	free(args);
	for (i=0; i<argcount; i++)
	{
		if (arrays[i])
		{
			//free this array
			jni->DeleteLocalRef(arrays[i]);
		}
	}
	free(arrays);
}

//void 
jint JNICALL FindClassObjects_heap_reference_callback(jlong class_tag, jlong size, jlong* tag_ptr, jint length, void* user_data)
{
	jlong tagtofind=*(jlong *)user_data;
	if (class_tag==tagtofind)
		*tag_ptr=tagtofind+1;

	
	return JVMTI_VISIT_OBJECTS;
}

void CJavaServer::FindClassObjects(void)
{
	jvmtiHeapCallbacks callbacks;
	jint objects_found=0;
	jobject *results=NULL;
	jlong *tags;


	jclass clazz=(jclass)ReadQword(); 

	//tag the class
	jlong tagtofind=0xce000000+tagcount++;
	jvmti->SetTag(clazz, tagtofind);

	

	

	//now find objects that have a class with this tag
	
	ZeroMemory(&callbacks, sizeof(jvmtiHeapCallbacks));
	callbacks.heap_iteration_callback=FindClassObjects_heap_reference_callback;
	jvmti->IterateThroughHeap(JVMTI_HEAP_FILTER_CLASS_UNTAGGED,clazz, &callbacks, &tagtofind);	

	jvmti->SetTag(clazz, 0);

	tagtofind=0xce000000+tagcount++;
	if (jvmti->GetObjectsWithTags(1, &tagtofind, &objects_found, &results, &tags)==JVMTI_ERROR_NONE)
	{
		int i;
		int count=0;

		for (i=0; i<objects_found; i++)
		{
			jvmti->SetTag(results[i],0);
			
			if (jni->IsInstanceOf(results[i], clazz))
				count++;
			else
			{
				jni->DeleteLocalRef(results[i]);
				results[i]=NULL;
			}
		}


		WriteDword(count);
		for (i=0; i<objects_found; i++)
		{
			if (results[i])
				WriteQword((UINT_PTR)results[i]);			
		}
	}
	else
		WriteDword(0);


}

void CJavaServer::AddToBootstrapClassLoaderPath(void)
{
	WORD len=ReadWord();
	char *path=(char *)malloc(len+1);
	Read(path, len);
	path[len]=0;

	jvmti->AddToBootstrapClassLoaderSearch(path);
	free(path);
}

void CJavaServer::AddToSystemClassLoaderPath(void)
{
	WORD len=ReadWord();
	char *path=(char *)malloc(len+1);
	Read(path, len);
	path[len]=0;

	jvmti->AddToSystemClassLoaderSearch(path);
	free(path);
}

void CJavaServer::GetFieldDeclaringClass(void)
{
	jclass klass=(jclass)ReadQword();
	jfieldID field=(jfieldID)ReadQword();
	jclass declaring_class=NULL;
	jvmti->GetFieldDeclaringClass(klass, field, &declaring_class);

	WriteQword((UINT_PTR)declaring_class);
}

void CJavaServer::GetFieldSignature(void)
{
	jclass klass=(jclass)ReadQword();
	jfieldID field=(jfieldID)ReadQword();

	SendFieldName(klass, field);
}

void CJavaServer::GetField(void)
{
	jobject object=(jobject)ReadQword();
	jfieldID fieldid=(jfieldID)ReadQword();

	int type=ReadByte();

	switch (type)
	{
		case 1:
		{
			jboolean r=jni->GetBooleanField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 2:
		{
			jbyte r=jni->GetByteField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 3:
		{
			jchar r=jni->GetCharField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 4:
		{
			jshort r=jni->GetShortField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 5:
		{
			jint r=jni->GetIntField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 6:
		{
			jlong r=jni->GetLongField(object, fieldid);
			WriteQword(r);
			break;
		}

		case 7:
		{
			jfloat r=jni->GetFloatField(object, fieldid);
			WriteQword(*(DWORD *)&r);
			break;
		}

		case 8:
		{
			jdouble r=jni->GetDoubleField(object, fieldid);
			WriteQword(*(UINT64 *)&r);
			break;
		}

		case 9:
		{
			jobject r=jni->GetObjectField(object, fieldid);
			WriteQword((UINT_PTR)r);
			break;
		}
	}
	

	
}

void CJavaServer::SetField(void)
{
	jobject object=(jobject)ReadQword();
	jfieldID fieldid=(jfieldID)ReadQword();

	int type=ReadByte();
	UINT64 newvalue=ReadQword();

	switch (type)
	{
		case 1:
		{			
			jboolean v=(jboolean)newvalue;
			jni->SetBooleanField(object, fieldid, v);			
			break;
		}

		case 2:
		{
			jbyte v=(jbyte)newvalue;
			jni->SetByteField(object, fieldid, v);	
			break;
		}

		case 3:
		{
			jchar v=(jchar)newvalue;
			jni->SetCharField(object, fieldid, v);	
			break;
		}

		case 4:
		{
			jshort v=(jshort)newvalue;
			jni->SetShortField(object, fieldid, v);	
			break;
		}

		case 5:
		{
			jint v=(jint)newvalue;
			jni->SetIntField(object, fieldid, v);	
			break;
		}

		case 6:
		{
			jlong v=(jlong)newvalue;
			jni->SetLongField(object, fieldid, v);	
			break;
		}

		case 7:
		{
			jfloat v=*(jfloat *)&newvalue;
			jni->SetFloatField(object, fieldid, v);
			break;
		}

		case 8:
		{
			jdouble v=*(jdouble *)&newvalue;
			jni->SetDoubleField(object, fieldid, v);
			break;
		}

		case 9:
		{
			jobject v=(jobject)newvalue;
			jni->SetObjectField(object, fieldid, v);
			break;
		}
	}

	
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

					case JAVAVMD_FINDREFERENCESTOOBJECT:
						FindReferencesToObject();
						break;

					case JAVACMD_FINDJOBJECT:
						FindjObject();
						break;

					case JAVACMD_GETCLASSSIGNATURE:
						GetClassSignature();
						break;

					case JAVACMD_GETSUPERCLASS:
						GetSuperClass();
						break;

					case JAVACMD_GETOBJECTCLASS:
						GetObjectClass();
						break;

					case JAVACMD_GETCLASSDATA:
						GetClassData();
						break;

					case JAVACMD_REDEFINECLASS:
						RedefineClass();
						break;

					case JAVACMD_FINDCLASS:
						FindClass();
						break;

					case JAVACMD_GETCAPABILITIES:
						GetCapabilities();
						break;

					case JAVACMD_GETMETHODNAME:
						GetMethodName();
						break;

					case JAVACMD_INVOKEMETHOD:
						InvokeMethod();
						break;

					case JAVACMD_FINDCLASSOBJECTS:
						FindClassObjects();
						break;

					case JAVACMD_ADDTOBOOTSTRAPCLASSLOADERPATH:
						AddToBootstrapClassLoaderPath();
						break;

					case JAVACMD_ADDTOSYSTEMCLASSLOADERPATH:
						AddToSystemClassLoaderPath();
						break;

					case JAVACMD_PUSHLOCALFRAME:
						PushLocalFrame();
						break;

					case JAVACMD_POPLOCALFRAME:
						PopLocalFrame();
						break;

					case JAVACMD_GETFIELDDECLARINGCLASS:
						GetFieldDeclaringClass();
						break;

					case JAVACMD_GETFIELDSIGNATURE:
						GetFieldSignature();
						break;

					case JAVACMD_GETFIELD:
						GetField();
						break;

					case JAVACMD_SETFIELD:
						SetField();
						break;

					default:						
						throw("Unexpected command\n");
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
