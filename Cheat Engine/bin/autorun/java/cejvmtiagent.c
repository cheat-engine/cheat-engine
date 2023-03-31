//android JVMTI agent

#ifndef CEJVMTIAGENT_H
#define CEJVMTIAGENT_H

#include <cepipelib.c>
#include <celog.h>
#include <jni.h>
#include <jvmti.h>  //get it yourself

typedef size_t UINT_PTR;

int getpid();

int sprintf(char *str, const char *format, ...);
int snprintf(char *str, int max, const char *format, ...);
void *memset(void *s, int c, size_t n);
char *strstr(const char *haystack, const char *needle);
char *strcasestr(const char *haystack, const char *needle);

void* jit_load(void* something);
void jit_unload(void* handle);
void* jit_compile_method(void* handle, void* method, void* self, int baseline, int osr);

jobjectArray _ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh(JNIEnv* env, jclass, jobjectArray javaClasses, jboolean includeAssignable);

void *_ZNK3art6Thread13DecodeJObjectEP8_jobject(void* thread, jobject o);

int _ZN3art9ArtMethod18HasAnyCompiledCodeEv(void* ArtMethod);

#define Thread_DecodeJObject _ZNK3art6Thread13DecodeJObjectEP8_jobject
#define VMDebug_getInstancesOfClasses _ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh
#define ArtMethod_HasAnyCompiledCode _ZN3art9ArtMethod18HasAnyCompiledCodeEv 


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
#define JAVACMD_FINDCLASSINSTANCES 18
#define JAVACMD_ADDTOBOOTSTRAPCLASSLOADERPATH 19
#define JAVACMD_ADDTOSYSTEMCLASSLOADERPATH 20
#define JAVACMD_PUSHLOCALFRAME 21
#define JAVACMD_POPLOCALFRAME 22
#define JAVACMD_GETFIELDDECLARINGCLASS 23
#define JAVACMD_GETFIELDSIGNATURE 24
#define JAVACMD_GETFIELD 25
#define JAVACMD_SETFIELD 26
#define JAVACMD_STARTSCAN 27
#define JAVACMD_REFINESCANRESULTS 28
#define JAVACMD_GETSCANRESULTS 29
#define JAVACMD_FINDWHATWRITES 30
#define JAVACMD_STOPFINDWHATWRITES 31
#define JAVACMD_GETMETHODDECLARINGCLASS 32

#define JAVACMD_RELEASECLASSLIST 33
#define JAVACMD_COMPILEMETHOD 34

#define JAVACMD_DEREFERENCEGLOBALOBJECTS 35
#define JAVACMD_GETFIELDVALUES 36
#define JAVACMD_COLLECTGARBAGE 37
#define JAVACMD_ANDROID_DECODEOBJECT 38

#define JAVACMD_FINDFIELDSWITHSPECIFICSIGNATURE 39
#define JAVACMD_FINDFIELDSWITHSPECIFICNAME 40
#define JAVAVMD_GETOBJECTCLASSNAME 41



#define _jvmti (*(agent->jvmti))
#define _env (*(agent->env))

#define ACC_STATIC 8

typedef struct
{
  void* thread;
  void* env;
  void* javavm;
} _soa, *psoa;

typedef struct 
{
  JNIEnv *env;
  jvmtiEnv *jvmti;
  psoa soa;
  jclass *classlist; //js_getLoadedClasses fills this
  jint classcount;
  PPipeServer pipe;   
  jmethodID m_Field_getOffset;
  jlong nextSearchTag; 
  
} CEJVMTIAgent, *PCEJVMTIAgent;


void cleanupClassList(PCEJVMTIAgent agent)
{
  if (agent->classlist)
  {    
    _jvmti->Deallocate(agent->jvmti, (unsigned char*)agent->classlist);
    agent->classlist=0;    
  }    
}

void destroyAgent(PCEJVMTIAgent agent)
{
  cleanupClassList(agent);
  
  if (agent->pipe)
  {
    ps_destroy(agent->pipe);  
    agent->pipe=NULL;
  }
  
  free(agent);
}


void js_releaseClassList(PCEJVMTIAgent agent) 
{
  debug_log("js_releaseClassList");
  if (agent->classlist)
  {
    int i;
    
    debug_log("releasing %d classes",agent->classcount);
    for (i=0; i<agent->classcount; i++)
      _env->DeleteLocalRef(agent->env, agent->classlist[i]);
    
    _jvmti->Deallocate(agent->jvmti, (unsigned char*)agent->classlist);
    agent->classlist=NULL;
  }
	  
}

void js_dereferenceGlobalObjects(PCEJVMTIAgent agent) 
{
  int count=ps_readDword(agent->pipe);
  
  uint64_t *objects=malloc(count*8);
  ps_read(agent->pipe, objects, count*8);
  
  int i;
  for (i=0; i<count; i++)
  {
    jobject o=(jobject)objects[i];
    (_env)->DeleteGlobalRef(agent->env, o);    
  }
}


void js_dereferenceLocalObject(PCEJVMTIAgent agent) 
{
  jobject object;
  object=(jobject)ps_readQword(agent->pipe);
	(*(agent->env))->DeleteLocalRef(agent->env,object);
}

void js_getClassFields(PCEJVMTIAgent agent) 
{
  //debug_log("java_getAllClassFields");
  jclass klass=(jclass)ps_readQword(agent->pipe);
  
  if (klass==0)
  {
    debug_log("Error: js_getClassFields : klass==0");
    ps_writeDword(agent->pipe,0);
    return;         
  }
	jint count;
  
	jfieldID *fields=NULL;  
  if (_jvmti->GetClassFields(agent->jvmti, klass, &count, &fields)==JVMTI_ERROR_NONE)
	{
    PMemoryStream ms=ms_create(8+count*8*32);
    ms_writeDword(ms, count);
        
		int i;
		for (i=0; i<count; i++)
		{
    	char *name=NULL, *sig=NULL, *gen=NULL;
	    int len;  
  
      ms_writeQword(ms,(uint64_t)fields[i]);
      
      
      
      if ((fields[i]) && (_jvmti->GetFieldName(agent->jvmti, klass, fields[i], &name, &sig, &gen)==JVMTI_ERROR_NONE))
      {
        if (name)
        {
          len=(int)strlen(name);
          ms_writeWord(ms, len);
          ms_write(ms, name, len);		
          //debug_log("fieldname: %s", name);
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);
        }
        else
          ms_writeWord(ms, 0);

        if (sig)
        {
          len=(int)strlen(sig);
          ms_writeWord(ms, len);
          ms_write(ms, sig, len);				
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
        }
        else
          ms_writeWord(ms, 0);

        if (gen)
        {
          len=(int)strlen(gen);
          ms_writeWord(ms, len);
          ms_write(ms, gen, len);					
          _jvmti->Deallocate(agent->jvmti,(unsigned char *)gen);
        }
        else
          ms_writeWord(ms, 0);
      }
      else
      {
        ms_writeWord(ms, 0);
        ms_writeWord(ms, 0);
        ms_writeWord(ms, 0);
      } 
      
      int offset=-1;
      jint modifiers=0;
      
      if ((fields[i]) && (_jvmti->GetFieldModifiers(agent->jvmti, klass, fields[i], &modifiers)==JVMTI_ERROR_NONE))
      {        
        //debug_log("field modifiers=%x",modifiers);
        
        //if the modifiers are unreadable, then getOffset may fail as well
        if (((modifiers & ACC_STATIC)==0) && (agent->m_Field_getOffset))
        {
         // debug_log("Not static so getting offset");
          
          jobject fieldo=_env->ToReflectedField(agent->env, klass, fields[i], 0); //assume non-static (todo: check that)
         // debug_log("fieldo=%d", fieldo);
          if (fieldo)
          {            
            offset=_env->CallIntMethod(agent->env, fieldo, agent->m_Field_getOffset);
           // debug_log("offset=%d", offset);
            _env->ExceptionClear(agent->env);
            
            _env->DeleteLocalRef(agent->env, fieldo);                        
          }
        }
      }

      ms_writeDword(ms, modifiers);
      ms_writeDword(ms, offset);     
		}
		_jvmti->Deallocate(agent->jvmti, (unsigned char *)fields);
    
    ps_writeDword(agent->pipe, ms->pos);
    ps_write(agent->pipe, ms->buf, ms->pos);    
    
    ms_destroy(ms);

	}
	else
		ps_writeDword(agent->pipe,0); //0 byte stream
}



void js_getClassMethods(PCEJVMTIAgent agent) 
{
  jclass klass=(jclass)ps_readQword(agent->pipe);
	jint count;
  
	jmethodID *methods=NULL;  
  if (_jvmti->GetClassMethods(agent->jvmti, klass, &count, &methods)==JVMTI_ERROR_NONE)
	{
    PMemoryStream ms=ms_create(8+count*8*32);
    ms_writeDword(ms, count);
        
		int i;
		for (i=0; i<count; i++)
		{
    	char *name=NULL, *sig=NULL, *gen=NULL;
	    int len;
  
      ms_writeQword(ms,(uint64_t)methods[i]);
      
      if (_jvmti->GetMethodName(agent->jvmti, methods[i], &name, &sig, &gen)==JVMTI_ERROR_NONE)
      {
        if (name)
        {
          len=(int)strlen(name);
          ms_writeWord(ms, len);
          ms_write(ms, name, len);					
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);
        }
        else
          ms_writeWord(ms, 0);

        if (sig)
        {
          len=(int)strlen(sig);
          ms_writeWord(ms, len);
          ms_write(ms, sig, len);				
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
        }
        else
          ms_writeWord(ms, 0);

        if (gen)
        {
          len=(int)strlen(gen);
          ms_writeWord(ms, len);
          ms_write(ms, gen, len);					
          _jvmti->Deallocate(agent->jvmti,(unsigned char *)gen);
        }
        else
          ms_writeWord(ms, 0);
        
        jint modifiers=0;
        _jvmti->GetMethodModifiers(agent->jvmti, methods[i], &modifiers);
        ms_writeDword(ms, modifiers);
          
      }
      else
      {
        ms_writeWord(ms, 0);
        ms_writeWord(ms, 0);
        ms_writeWord(ms, 0);
        ms_writeDword(ms, 0);
      }      
		}
		_jvmti->Deallocate(agent->jvmti, (unsigned char *)methods);
    
    ps_writeDword(agent->pipe, ms->pos);
    ps_write(agent->pipe, ms->buf, ms->pos);    
    
    ms_destroy(ms);

	}
	else
		ps_writeDword(agent->pipe,0); //0 byte stream
}


jint fillClassList(PCEJVMTIAgent agent) 
{
	return (*(agent->jvmti))->GetLoadedClasses(agent->jvmti, &agent->classcount, &agent->classlist);	
}

void js_getObjectClassName(PCEJVMTIAgent agent) 
{
  debug_log("js_getObjectClassName");
  jobject o=(jobject)ps_readQword(agent->pipe);
  jclass oc=_env->GetObjectClass(agent->env, o);
  if (oc)  
  {
    char *sig;
    char *gen;
    if (_jvmti->GetClassSignature(agent->jvmti, oc, &sig, &gen)==JVMTI_ERROR_NONE)
    {
      if (sig)
      {
        int sl=strlen(sig);
        ps_writeWord(agent->pipe, sl);
        ps_write(agent->pipe, sig, sl);
        
        _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);              
      }
      else
        ps_writeWord(agent->pipe,0);  
      
      if (gen)
        _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);      
  
    }
    else
      ps_writeWord(agent->pipe,0);
      
    _env->DeleteLocalRef(agent->env, oc);    
  }
  else 
    ps_writeWord(agent->pipe,0);
}

void js_getLoadedClasses(PCEJVMTIAgent agent) 
{
	int i;
	
  js_releaseClassList(agent);
  if (fillClassList(agent)==JVMTI_ERROR_NONE)
	{
    debug_log("jvmti->GetLoadedClasses success. Classcount=%d", agent->classcount);
    PMemoryStream ms=ms_create(96*agent->classcount);   
    
    char *sig=NULL,*gen=NULL;

    ms_writeDword(ms, agent->classcount);    
    
    for (i=0; i<agent->classcount; i++)
    { 
      ms_writeQword(ms, (uint64_t)agent->classlist[i]);
      
      if (_jvmti->GetClassSignature(agent->jvmti, agent->classlist[i], &sig, &gen)==JVMTI_ERROR_NONE)
      {
        if (sig)
        {
          int l=strlen(sig);
          ms_writeWord(ms,l);
          ms_write(ms, sig,l);
          
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);          
        }
        else
          ms_writeWord(ms,0);
        
        if (gen)
        {
          int l=strlen(gen);
          ms_writeWord(ms,l);
          ms_write(ms, gen,l);
          
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);          
        }        
        else
          ms_writeWord(ms,0);
        
        //get the superclass name and sig
        jclass superclass=_env->GetSuperclass(agent->env, agent->classlist[i]);
        if (superclass)
        {
          if (_jvmti->GetClassSignature(agent->jvmti, superclass, &sig, &gen)==JVMTI_ERROR_NONE)
          {
            if (sig)
            {
              int l=strlen(sig);
              ms_writeWord(ms,l);
              ms_write(ms, sig,l);
              
              _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);          
            }
            else
              ms_writeWord(ms,0);
            
            if (gen)
            {
              int l=strlen(gen);
              ms_writeWord(ms,l);
              ms_write(ms, gen,l);
              
              _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);          
            }        
            else
              ms_writeWord(ms,0);          
          }
          
        }
        else
          ms_writeDword(ms,0); 
        
        
      }
      else
      {
        ms_writeDword(ms,0);        
        ms_writeDword(ms,0);        
      }
      
    }
    
    ps_writeDword(agent->pipe, ms->pos);
    ps_write(agent->pipe, ms->buf, ms->pos);
    
    ms_destroy(ms);    
	}
	else
  {
    debug_log("jvmti->GetLoadedClasses failed");
		ps_writeDword(agent->pipe, 0); //0 byte stream
  }
}

void js_getCapabilities(PCEJVMTIAgent agent) 
{
  jvmtiCapabilities cap;
	(*(agent->jvmti))->GetCapabilities(agent->jvmti, &cap);
  
  debug_log("js_getCapabilities: writing cap which is %d bytes long (should be 16)", sizeof(cap));
  ps_write(agent->pipe, &cap, sizeof(cap));
}

void js_compileMethod(PCEJVMTIAgent agent) 
{
  debug_log("js_compileMethod");
  void* Method=(void*)ps_readQword(agent->pipe);
  debug_log("Method=%p",Method);
  void* something=NULL;
  void* r=jit_load(&something);
  
  debug_log("jit_load returned %p",r);
  if (r)
  {
    void* cr;
    debug_log("calling jit_compile_method");
    cr=jit_compile_method(r, Method, *(void**)(agent->soa), 0,0);
    
    debug_log("cr=%p", cr);
    
   // jit_unload(cr);
  }
}

jvmtiIterationControl JNICALL FindClassObjects_callback(jlong class_tag, jlong size, jlong* tag_ptr, void* user_data)
{
  debug_log("found one");
	*tag_ptr=*(jlong *)user_data;

	
	return JVMTI_ITERATION_CONTINUE;
}


jint ce_heap_iteration_callback(jlong class_tag, jlong size, jlong* tag_ptr, jint length, PCEJVMTIAgent agent)
{
  //debug_log("ce_heap_iteration_callback called");
  //debug_log("class_tag==%x", class_tag);
  if (class_tag)
    debug_log("class_tag==%x", class_tag);    
  
  if (class_tag==agent->nextSearchTag)
  {
    debug_log("FOUND MATCH!");    
    *tag_ptr=agent->nextSearchTag+1;
  }
  
  return 0;  
}

void js_findClassInstances(PCEJVMTIAgent agent) 
{
  unsigned char lookupmethod=ps_readByte(agent->pipe);
  jclass klass=(jclass)ps_readQword(agent->pipe);
  char *sig=NULL,*gen=NULL;
  if ((*(agent->jvmti))->GetClassSignature(agent->jvmti, klass, &sig, &gen)==JVMTI_ERROR_NONE)
  {
    if (sig)
    {
      debug_log("finding instances of class %s", sig);
      _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
    }
    
    if (gen)
     _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
    
  }
  else
  {
    debug_log("This doesn't seem like a valid class");
    ps_writeDword(agent->pipe, 0);
  }
  

  
 /* jlong tag=agent->nextSearchTag;
  agent->nextSearchTag++;
  
  debug_log("Configuring objects of this class with tag %x", (int)tag);*/
  
 /* 
 //does not work on android:
  jint error=_jvmti->IterateOverInstancesOfClass(agent->jvmti, klass, JVMTI_HEAP_OBJECT_EITHER, FindClassObjects_callback, &tag);
  
  if (error==JVMTI_ERROR_NONE)
  {
    debug_log("Properly set the tags. Now get all objects with that tag");
    
  }  
  else
  does work:  (*jvmtienv)->IterateThroughHeap(jvmtienv, stringclass,0, &cbs, 0);  
  (*jvmtienv)->SetTag(
  (*jvmtienv)->GetObjectsWithTags(
  */
  debug_log("lookupmethod=%d", lookupmethod);
  if (lookupmethod==1)
  {
    jint r;
    jvmtiHeapCallbacks callbacks;
    
    
    agent->nextSearchTag++;    
    debug_log("Tagging this class with tag %x", agent->nextSearchTag);    
    r=_jvmti->SetTag(agent->jvmti, klass, agent->nextSearchTag); 
    debug_log("SetTag returned %d", r);
        
    debug_log("initializing callbacks");
    memset(&callbacks,0,sizeof(callbacks));
    callbacks.heap_iteration_callback=ce_heap_iteration_callback;
    callbacks.heap_reference_callback=NULL;
    callbacks.primitive_field_callback=NULL;
    callbacks.array_primitive_value_callback=NULL;
    callbacks.string_primitive_value_callback=NULL;
    
    debug_log("calling IterateThroughHeap");
    r=_jvmti->IterateThroughHeap(agent->jvmti, 0, 0, &callbacks, agent);    
    
    debug_log("IterateThroughHeap returned %d",r);
    
    jint count;
    jobject *results=NULL;
    jlong *tags=NULL;
    
    agent->nextSearchTag++;
    r=_jvmti->GetObjectsWithTags(agent->jvmti, 1, &agent->nextSearchTag, &count, &results, &tags);
    debug_log("GetObjectsWithTags returned %d",r);
    if (r==0)
    {
      PMemoryStream ms=ms_create(4+count*16);
      ms_writeDword(ms, count);           
      
      debug_log("count=%d", count);
      if (results)
      {
        int i;
    
        
        for (i=0; i<count; i++)
        {          
          jobject g_c=_env->NewGlobalRef(agent->env, results[i]);
          
          void *objptr;
          objptr=Thread_DecodeJObject(agent->soa->thread, g_c);
          debug_log("%d: %d -> %p", i, g_c, objptr);
                
          ms_writeQword(ms,g_c);  //ce has to delete this global ref manually
          ms_writeQword(ms,objptr);

          _env->DeleteLocalRef(agent->env, results[i]);
        }
              
        
        _jvmti->Deallocate(agent->jvmti, results);
      }
      
      if (tags)
        _jvmti->Deallocate(agent->jvmti, tags);
      
      ps_writeMemStream(agent->pipe, ms);
      ms_destroy(ms);      
    }
    else    
      ps_writeDword(agent->pipe,0);
     
  }
  else  
  if (lookupmethod==0) //does work on android if called from native (dalvik.system.VMDebug.getInstancesOfClasses)
  {
   // debug_log("Failure setting the tags: %d",error);
   // debug_log("falling back on _ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh");

    jclass classclass = _env->FindClass(agent->env, "java/lang/Class");
    
    jclass vmdebugclass = _env->FindClass(agent->env, "dalvik/system/VMDebug");
    debug_log("vmdebugclass=%d", vmdebugclass);
    
    jmethodID m_getInstancesOfClasses=0;
    if (vmdebugclass)
    {
      m_getInstancesOfClasses=_env->GetMethodID(agent->env, vmdebugclass, "getInstancesOfClasses", "([Ljava/lang/Class;Z)[[Ljava/lang/Object;");
      debug_log("m_getInstancesOfClasses=%d",m_getInstancesOfClasses);
      
      if (m_getInstancesOfClasses==0)
        _env->ExceptionClear(agent->env);
        
      
    }
    //else
    //  _env->ExceptionClear(agent->env);
    
    jobjectArray classestofindarray = _env->NewObjectArray(agent->env, 1, classclass, klass);  
   
    debug_log("calling getInstancesOfClasses");
    jobjectArray result;
    
    if (m_getInstancesOfClasses)
    {
      debug_log("Using jni method");
      result=_env->CallStaticObjectMethod(agent->env, vmdebugclass, m_getInstancesOfClasses, classestofindarray, 1);  
    }
    else
    {
      debug_log("Using C++ call");
      result=_ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh(agent->env, 0, classestofindarray,0);
    }
    
    if (vmdebugclass)
      _env->DeleteLocalRef(agent->env, vmdebugclass);
    
    debug_log("getInstancesOfClasses returned %d", result);
    
    int count=_env->GetArrayLength(agent->env, result);
    
    if (count) //should be 1
    {    
      jobjectArray r= _env->GetObjectArrayElement(agent->env,result, 0);  
      if (r)
      {        
        count=_env->GetArrayLength(agent->env, r);
        debug_log("count=%d", count);
        
        if (count)       
        {
          int i;
          PMemoryStream ms=ms_create(4+count*16);
          ms_writeDword(ms, count);         
          
        
          for (i=0; i<count; i++)
          {
            jobject c= _env->GetObjectArrayElement(agent->env,r, i);   
            if (c)   
            {              
              //jobjectRefType otype=_env->GetObjectRefType(agent->env,c);    

              
              jobject g_c=_env->NewGlobalRef(agent->env, c);
              
              //debug_log("c=%d g_c=%d otype=%d",c, g_c, otype);
              
              void *objptr;
              objptr=Thread_DecodeJObject(agent->soa->thread, g_c);
              debug_log("%d -> %p", g_c,objptr);
              
              ms_writeQword(ms,g_c);  //ce has to delete this global ref manually
              ms_writeQword(ms,objptr);

              _env->DeleteLocalRef(agent->env, c);
            }
            else
            {
              debug_log("Entry %d is nil",i);
              
            }
          }
          
          ps_writeMemStream(agent->pipe, ms);
          ms_destroy(ms);
        }
        else
          ps_writeDword(agent->pipe,0);
        
        _env->DeleteLocalRef(agent->env, r);
      }

    }
    else
      ps_writeDword(agent->pipe,0);
    
    _env->DeleteLocalRef(agent->env, classclass);
    _env->DeleteLocalRef(agent->env, classestofindarray);
    _env->DeleteLocalRef(agent->env, result);

    
    
  }
}

void js_getFieldValues(PCEJVMTIAgent agent) 
/*
Retrieve a list of strings for the given fieldid's and the object provided
*/
{
  debug_log("js_getFieldValues");
  jobject obj=(jobject)ps_readQword(agent->pipe);
  jclass class=(jclass)ps_readQword(agent->pipe);  
  
  debug_log("Getting field values of jobject %d",obj);
  
  int count=ps_readDword(agent->pipe);
  debug_log("count=%d", count);
  PMemoryStream ms=ms_create(count*16);  
  
  int i;

  for (i=0; i<count; i++)
  {
    jfieldID field=ps_readQword(agent->pipe);
    int fieldtype=ps_readByte(agent->pipe);
    int isstatic=ps_readByte(agent->pipe);
    

    if (isstatic)
      debug_log("Type of field nr %d = %d (static)", i, fieldtype);
    else
      debug_log("Type of field nr %d = %d", i, fieldtype);    
      
    
    switch (fieldtype)
    {
      case 0:  //void
        break;         
      
      case 1: //boolean
      {
        jboolean r;
        if (isstatic)
          r=_env->GetStaticBooleanField(agent->env, class, field);
        else
          r=_env->GetBooleanField(agent->env, obj, field);
        
        ms_writeByte(ms,r);        
        break;
      }
        
      case 2: //byte
      {
        jbyte r;
        if (isstatic)
          r=_env->GetStaticByteField(agent->env, class, field);
        else
          r=_env->GetByteField(agent->env, obj, field);
        
        ms_writeByte(ms,r);
        break;
      }
        
      case 3: //char
      {
        jchar r;
        
        if (isstatic)
          r = _env->GetStaticCharField(agent->env, class, field);
        else
          r = _env->GetCharField(agent->env, obj, field);
        
        ms_writeWord(ms,r);
        break; 
      }

      case 4: //short
      {
        jshort r;
        if (isstatic)
          r =_env->GetStaticShortField(agent->env, class, field);
        else
          r =_env->GetShortField(agent->env, obj, field);
        
        ms_writeWord(ms,r);
        break;
      }
        
        
        
      case 5: //int
      {
        jint r;
        if (isstatic)
          r = _env->GetStaticIntField(agent->env, class, field);
        else
          r = _env->GetIntField(agent->env, obj, field);
          
        ms_writeDword(ms,r);
        break;  
      }

      case 6: //long
      {
        jlong r;
        if (isstatic)
          r = _env->GetStaticLongField(agent->env, class, field);
        else
          r = _env->GetLongField(agent->env, obj, field);
        
        ms_writeQword(ms,r);
        break;  
      }

      case 7: //float
      {
        jfloat r;
        if (isstatic)
          r = _env->GetStaticFloatField(agent->env, class, field);
        else 
          r = _env->GetFloatField(agent->env, obj, field);
          
        ms_writeDword(ms,r);
        break;  
      }        
        
      case 8: //double
      {        
        jdouble r;
        if (isstatic)
          r = _env->GetStaticDoubleField(agent->env, class, field);
        else
          r = _env->GetDoubleField(agent->env, obj, field);
        
        ms_writeQword(ms,r);
        break;    
      }

      case 9: //object
      {
        debug_log("9: calling GetObjectField");
        jobject r;
        if (isstatic)
          r=_env->GetStaticObjectField(agent->env, class, field);
        else
          r=_env->GetObjectField(agent->env, obj, field);
          
        if (r)
        {
          jobject g_r=_env->NewGlobalRef(agent->env,r);
          ms_writeQword(ms,g_r);
          _env->DeleteLocalRef(agent->env, r);
        }
        else
          ms_writeQword(ms,0);
        
        break;  
      }        

      case 10: //array
      {
        debug_log("10: calling GetObjectField");
        jobject r;
        if (isstatic)
        {
          debug_log("10: static");
          r=_env->GetStaticObjectField(agent->env, class, field);
        }
        else
        {
          debug_log("10: normal");
          r=_env->GetObjectField(agent->env, obj, field);
        }
        
        debug_log("10: r=%d",r);
        
        if (r)
        {
          jobject g_r=_env->NewGlobalRef(agent->env,r);
          ms_writeQword(ms,g_r);
          
          debug_log("10: is now globalref %d",g_r);
          _env->DeleteLocalRef(agent->env, r);
        }
        else
        {          
          ms_writeQword(ms,0);
        }
        
        break;  
      }

      case 11:   //java string
      {
        //debug_log("calling GetObjectField");
        
        jobject stringobj;
        if (isstatic)
          stringobj=_env->GetStaticObjectField(agent->env, class, field);
        else
          stringobj=_env->GetObjectField(agent->env, obj, field);        
        
        if (stringobj)
        {
          debug_log("stringobj is valid");
          
          ms_writeByte(ms,1);          
          char *r=_env->GetStringUTFChars(agent->env, stringobj, NULL);
          ms_writeString(ms,r);
          
          _env->ReleaseStringUTFChars(agent->env, stringobj, r);
          _env->DeleteLocalRef(agent->env, stringobj);
        }
        else
        {
          debug_log("stringobj is invalid");
          ms_writeByte(ms,0);
        }
          
        break;
      }
      
      default:
        break;
    }    
  }

  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms);
}

void js_collectGarbage(PCEJVMTIAgent agent) 
{  
  jclass systemClass = (_env)->FindClass(agent->env, "java/lang/System");
  debug_log("systemClass=%d", systemClass);
  if (systemClass)
  {              
    jmethodID systemGCMethod = (_env)->GetStaticMethodID(agent->env, systemClass, "gc", "()V");
    debug_log("systemGCMethod=%d", systemGCMethod);
    if (systemGCMethod)
      (_env)->CallStaticVoidMethod(agent->env, systemClass, systemGCMethod);                
    
    (_env)->DeleteLocalRef(agent->env, systemClass);
  }
  
  (_jvmti)->ForceGarbageCollection(agent->jvmti);
}

//todo: ifdef android in case I ever port this to windows
void js_android_decodeObject(PCEJVMTIAgent agent) 
{
  jobject obj=(jobject)ps_readQword(agent->pipe);
  void *objptr;
  objptr=Thread_DecodeJObject(agent->soa->thread, obj);
  ps_writeQword(agent->pipe, objptr);
}

void js_findFieldsWithSpecificName(PCEJVMTIAgent agent) 
{
  debug_log("js_findFieldsWithSpecificName");
  if (agent->classlist==NULL)
    fillClassList(agent);
  
  if (agent->classlist==NULL)
  {
    debug_log("Unable to obtain classlist");
    ps_writeDword(agent->pipe,0);
    return;
  }
  
  typedef (*comparefunction)(const char *haystack, const char *needle);
  
  comparefunction comp;
  
  int caseSensitive=ps_readByte(agent->pipe);
  if (caseSensitive)
  {
    comp=strstr;
    debug_log("case sensitive");
  }
  else
  {
    comp=strcasestr;
    debug_log("case insensitive");
  }
  

  short l=ps_readWord(agent->pipe);
  char *searchname=(char*)malloc(l+1);
  ps_read(agent->pipe, searchname,l);
  searchname[l]=0; 
  
  
  
  debug_log("scanning for %s",searchname);
  
  int i;
  jint count;
  jfieldID *fields=NULL;  

  PMemoryStream ms=ms_create(16*1024);
  
  for (i=0; i<agent->classcount; i++)
  {   
    if (_jvmti->GetClassFields(agent->jvmti, agent->classlist[i], &count, &fields)==JVMTI_ERROR_NONE)
	  {
      int j;
      for (j=0; j<count; j++)
      {
        char *name;
        char *sig;
        char *gen;
        
        if ((fields[j]) && (_jvmti->GetFieldName(agent->jvmti, agent->classlist[i], fields[j], &name, &sig, &gen)==JVMTI_ERROR_NONE))
        {
          if (name)
          {
            if (comp(name, searchname))
            {
              ms_writeQword(ms, agent->classlist[i]);
              ms_writeQword(ms, fields[j]);
              
              char *sig2, *gen2;
              if ((*(agent->jvmti))->GetClassSignature(agent->jvmti, agent->classlist[i], &sig2, &gen2)==JVMTI_ERROR_NONE)
              {        
                debug_log("found %s : %s in class %s",name,sig,sig2);
                
                if (sig2)
                  _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig2); 
                
                if (gen2)
                  _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen2);                 
              }
              
              
            }            
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);  
          }
          
          if (sig)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
          
          if (gen)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);  
        }
      }
    } 
    _jvmti->Deallocate(agent->jvmti, (unsigned char *)fields);    
  }
  
  debug_log("ms->pos=%d", ms->pos);
  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms);
  
  debug_log("scan done");
  
  free(searchname);
}

void js_findFieldsWithSpecificSignature(PCEJVMTIAgent agent) 
{  
  debug_log("js_findFieldsWithSpecificSignature");

  if (agent->classlist==NULL)
    fillClassList(agent);
  
  if (agent->classlist==NULL)
  {
    debug_log("Unable to obtain classlist");
    ps_writeDword(agent->pipe,0);
    return;
  }

  typedef (*comparefunction)(const char *haystack, const char *needle);
  
  comparefunction comp;
  
  int caseSensitive=ps_readByte(agent->pipe);
  if (caseSensitive)
    comp=strstr;
  else
    comp=strcasestr;
  
  
  short l=ps_readWord(agent->pipe);
  char *searchsig=(char*)malloc(l+1);
  ps_read(agent->pipe, searchsig,l);
  searchsig[l]=0; 
  
  debug_log("scanning for %s",searchsig);
  
  int i;
  jint count;
  jfieldID *fields=NULL;  

  PMemoryStream ms=ms_create(16*1024);  
  
  for (i=0; i<agent->classcount; i++)
  {
   
    if (_jvmti->GetClassFields(agent->jvmti, agent->classlist[i], &count, &fields)==JVMTI_ERROR_NONE)
	  {
      int j;
      for (j=0; j<count; j++)
      {
        char *name;
        char *sig;
        char *gen;
        
        if ((fields[j]) && (_jvmti->GetFieldName(agent->jvmti, agent->classlist[i], fields[j], &name, &sig, &gen)==JVMTI_ERROR_NONE))
        {
          if (name)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);  
          
          if (sig)
          {
            if (comp(sig, searchsig))
            {
              ms_writeQword(ms, agent->classlist[i]);
              ms_writeQword(ms, fields[j]);              
            }
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
          }
          
          if (gen)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);
        }
      }
    } 

    _jvmti->Deallocate(agent->jvmti, (unsigned char *)fields);    
  }
  
  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms);  
  
  debug_log("scan done");
  
  free(searchsig);
}

void launchCEJVMTIServer(JNIEnv *env, jvmtiEnv *jvmti, void* soa) 
{
  char *xpipename=(char*)malloc(100);;
  int pid=getpid();
  
  CEJVMTIAgent *agent=(CEJVMTIAgent*)malloc(sizeof(CEJVMTIAgent));
  memset(agent, 0, sizeof(CEJVMTIAgent)); 

  agent->env=env;
  agent->jvmti=jvmti;
  agent->soa=soa;
  
  agent->nextSearchTag=0xce000000;
  
  //fill in agent->m_getOffset (Android has this method)
  jclass reflectFieldclass=(*env)->FindClass(env,"java/lang/reflect/Field");
  if (reflectFieldclass)
  {
    debug_log("found java/lang/reflect/Field");
    
    agent->m_Field_getOffset=(*env)->GetMethodID(env, reflectFieldclass, "getOffset","()I");
    debug_log("m_Field_getOffset=%p", agent->m_Field_getOffset);
    
    (*env)->DeleteLocalRef(env, reflectFieldclass);
  }
  
  jclass VMDebugClass=(*env)->FindClass(env,"dalvik/system/VMDebug");
  debug_log("VMDebugClass=%d",VMDebugClass);  

  
  
  snprintf(xpipename, 99, "cejavadc_pid%d", pid);
  
  debug_log("Creating pipe %s", xpipename);
  
  int terminated=0;
  //while (!terminated)
  {
    agent->pipe=CreatePipeServer(xpipename);
    

    debug_log("CreatePipeServer returned %p", agent->pipe);
    
    while (agent && (agent->pipe) && ps_isvalid(agent->pipe))
    {
      char command=ps_readByte(agent->pipe);
      
      if (ps_isvalid(agent->pipe))
      {
      
        debug_log("Received command %d", command);
        
        switch (command)
        {
          case JAVACMD_STARTCODECALLBACKS:
            debug_log("NYI: JAVACMD_STARTCODECALLBACKS");
            break;        
            
          case JAVACMD_STOPCODECALLBACKS:
            debug_log("NYI: JAVACMD_STOPCODECALLBACKS");
            break;
            
          case JAVACMD_GETLOADEDCLASSES:
            debug_log("JAVACMD_GETLOADEDCLASSES");
            js_getLoadedClasses(agent);
            break;
            
          case JAVACMD_DEREFERENCELOCALOBJECT:
            js_dereferenceLocalObject(agent);
            break;
            
          case JAVACMD_DEREFERENCEGLOBALOBJECTS:
            js_dereferenceGlobalObjects(agent);
            break;
            
          case JAVACMD_GETCLASSFIELDS:
            js_getClassFields(agent);
            break;
            
          case JAVACMD_GETCLASSMETHODS:
            js_getClassMethods(agent);
            break;
            
          case JAVACMD_RELEASECLASSLIST:
            js_releaseClassList(agent);
            break;
            
          case JAVACMD_FINDCLASSINSTANCES:
            js_findClassInstances(agent);
            break;
            
          case JAVACMD_GETCAPABILITIES:
            js_getCapabilities(agent);
            break;
            
          case JAVACMD_COMPILEMETHOD:
            js_compileMethod(agent);
            break;
            
          case JAVACMD_GETFIELDVALUES:
            debug_log("JAVACMD_GETFIELDVALUES");
            js_getFieldValues(agent);
            break;
            
          case JAVACMD_COLLECTGARBAGE:
            js_collectGarbage(agent);
            break;
            
          case JAVACMD_ANDROID_DECODEOBJECT:
            js_android_decodeObject(agent);
            break;
            
          case JAVACMD_FINDFIELDSWITHSPECIFICSIGNATURE:
            js_findFieldsWithSpecificSignature(agent);
            break;
            
          case JAVACMD_FINDFIELDSWITHSPECIFICNAME:
            js_findFieldsWithSpecificName(agent);
            break;
            
          case JAVAVMD_GETOBJECTCLASSNAME:
            js_getObjectClassName(agent);
            break;
            
            
          default: 
            debug_log("Invalid command. Terminating server");
            terminated=1;
            destroyAgent(agent);
            agent=NULL;
            break;    
        }
      } 
      else
      {
        debug_log("pipe was disconnected");       
      }
    }
  }
  
  if (agent)
  {
    destroyAgent(agent);
    agent=NULL;
  } 
  
  
  if (xpipename)
    free(xpipename);
  
  debug_log("CEJVMTI Agent says Goodbyte!");  
}

#endif //CEJVMTIAGENT_H