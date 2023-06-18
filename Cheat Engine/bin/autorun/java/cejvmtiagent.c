//android JVMTI agent

#ifndef CEJVMTIAGENT_H
#define CEJVMTIAGENT_H

#include <cepipelib.c>
#include <celog.h>
#include <jni.h>
#include <jvmti.h>  //get it yourself

#include <jvarscan.c>

typedef size_t UINT_PTR;

int getpid();

int sprintf(char *str, const char *format, ...);
int snprintf(char *str, int max, const char *format, ...);
void *memset(void *s, int c, size_t n);
char *strstr(const char *haystack, const char *needle);

#ifdef _WIN32
//strcasestr does not exist, but StrStrIA does
char *StrStrIA(const char *haystack, const char *needle);
#define strcasestr StrStrIA
#else
char *strcasestr(const char *haystack, const char *needle);
#endif

#ifdef ANDROID
void* jit_load(void* something);
void jit_unload(void* handle);
void* jit_compile_method(void* handle, void* method, void* self, int baseline, int osr);

jobjectArray _ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh(JNIEnv* env, jclass, jobjectArray javaClasses, jboolean includeAssignable);

void *_ZNK3art6Thread13DecodeJObjectEP8_jobject(void* thread, jobject o);

int _ZN3art9ArtMethod18HasAnyCompiledCodeEv(void* ArtMethod);

#define Thread_DecodeJObject _ZNK3art6Thread13DecodeJObjectEP8_jobject
#define VMDebug_getInstancesOfClasses _ZN3artL29VMDebug_getInstancesOfClassesEP7_JNIEnvP7_jclassP13_jobjectArrayh
#define ArtMethod_HasAnyCompiledCode _ZN3art9ArtMethod18HasAnyCompiledCodeEv 
#endif


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
#ifdef ANDROID
#define JAVACMD_ANDROID_DECODEOBJECT 38
#endif

#define JAVACMD_FINDFIELDS 39
#define JAVACMD_FINDMETHODS 40
#define JAVAVMD_GETOBJECTCLASSNAME 41
#define JAVACMD_SETFIELDVALUES 42
#define JAVACMD_GETOBJECTCLASSNAMES 43
#define JAVACMD_GETFIELDSIGNATUREBYOBJECT 44



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
#ifdef ANDROID
  jmethodID m_Field_getOffset;
#endif  
  jlong nextSearchTag; 
  
  PScanData currentScan;
  
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


void js_startscan(PCEJVMTIAgent agent)
{
  ScanData sd;
  uint64_t resultcount;
  
  debug_log("js_startscan. Reading scandata object. (%d bytes)", sizeof(sd));
  
  ps_read(agent->pipe, &sd, sizeof(sd));
  
  debug_log("received scandata. Doing a scan");  
  resultcount=jvarscan_StartScan(agent->jvmti, sd);
  
  debug_log("jvarscan_StartScan returned %d", resultcount);
  
  ps_writeQword(agent->pipe, resultcount);
  
}

void js_refinescanresults(PCEJVMTIAgent agent)
{
  ScanData sd;
  uint64_t resultcount;
  jvalue value;
  
  ps_read(agent->pipe, &sd, sizeof(sd));
  
  jvarscan_refineScanResults(agent->jvmti, agent->env, sd);//
  
  ps_writeQword(agent->pipe, resultcount);
}

void js_getscanresults(PCEJVMTIAgent agent)
{
  int maxcount;
  maxcount=ps_readDword(agent->pipe);
  if (maxcount>=ScanResultsPos)
    maxcount=ScanResultsPos;
  
  debug_log("Fetching %d of %d results", maxcount, ScanResultsPos);
 
  int i;
  
  
  
  PMemoryStream ms=ms_create(maxcount*(8+128));
  ms_writeDword(ms, maxcount);
 
  for (i=0; i<maxcount; i++)
  {
    if ((ScanResults[i].object==NULL) && (ScanResults[i].objectTag)) //get the object from the tag
      scanresult_initentry(agent->jvmti, agent->env, &ScanResults[i]); //sets the object and the fieldid
   
    debug_log("sending result %d with objectid %p", i, ScanResults[i].object);
    ms_writeQword(ms, (uint64_t)ScanResults[i].object);
    ms_writeQword(ms, (uint64_t)ScanResults[i].fieldID); 
    ms_writeDword(ms, (uint32_t)ScanResults[i].fieldindex); 
  }
  
  ps_writeMemStream(agent->pipe, ms);  
  ms_destroy(ms);
  
  debug_log("Sent the results to the client");
  
}

void js_getFieldSignature(PCEJVMTIAgent agent) 
{
  jclass c=(jclass)ps_readQword(agent->pipe);
  jfieldID fid=(jfieldID)ps_readQword(agent->pipe);
  
  PMemoryStream ms=ms_create(512);
  
  jint error;
  char *name=NULL, *sig=NULL, *gen=NULL;
  int len;  

  if (_jvmti->GetFieldName(agent->jvmti, c, fid, &name, &sig, &gen)==JVMTI_ERROR_NONE)
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
    
  }  
  else
  {
    ms_writeWord(ms, 0);
    ms_writeWord(ms, 0);
    ms_writeWord(ms, 0);
  }
  
  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms); 
}

void js_getFieldSignatureByObject(PCEJVMTIAgent agent) 
{
  debug_log("js_getFieldSignatureByObject");
  jobject object=(jobject)ps_readQword(agent->pipe);
  jfieldID fieldid=(jfieldID)ps_readQword(agent->pipe);
  
  debug_log("js_getFieldSignatureByObject");

  jclass klass=_env->GetObjectClass(agent->env, object);
  
  PMemoryStream ms=ms_create(512);
  
  if (klass)
  {  
    jint error;
    char *name=NULL, *sig=NULL, *gen=NULL;
    int len;  
  
    if (_jvmti->GetFieldName(agent->jvmti, klass, fieldid, &name, &sig, &gen)==JVMTI_ERROR_NONE)
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
      
    }  
    else
    {
      debug_log("js_getFieldSignatureByObject: GetFieldName failed");
      ms_writeWord(ms, 0);
      ms_writeWord(ms, 0);
      ms_writeWord(ms, 0);
    }
    
    _env->DeleteLocalRef(agent->env, klass);  
  } 
  else
  {
    debug_log("js_getFieldSignatureByObject: invalid object");
    ms_writeWord(ms, 0);
    ms_writeWord(ms, 0);
    ms_writeWord(ms, 0);    
  }

  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms); 
}



void js_getClassFields(PCEJVMTIAgent agent) 
{
  jclass klass=(jclass)ps_readQword(agent->pipe);
  jint error;
  
  if (klass==0)
  {
    debug_log("Error: js_getClassFields : klass==0");
    ps_writeDword(agent->pipe,0);
    return;         
  }
	jint count;
  
	jfieldID *fields=NULL;  
  error=_jvmti->GetClassFields(agent->jvmti, klass, &count, &fields);
  if (error==JVMTI_ERROR_NONE)
	{
    PMemoryStream ms=ms_create(8+count*8*32);
    ms_writeDword(ms, count);
    
    //debug_log("GetClassFields success. Count=%d", count);
    
        
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
#ifdef ANDROID        
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
#endif        
      }

      ms_writeDword(ms, modifiers);
      ms_writeDword(ms, offset);     
		}
		_jvmti->Deallocate(agent->jvmti, (unsigned char *)fields);
    
    if (ms->pos==0)
      debug_log("Sending an empty fieldlist");

    ps_writeDword(agent->pipe, ms->pos);
    ps_write(agent->pipe, ms->buf, ms->pos);    
    
    ms_destroy(ms);

	}
	else
  {
    
    if (error==JVMTI_ERROR_CLASS_NOT_PREPARED)    
      debug_log("Class not prepared yet");    
    else
      debug_log("GetClassFields failed. Errorcode=%d",error);
    
		ps_writeDword(agent->pipe,0); //0 byte stream
  }
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

void js_getObjectClass(PCEJVMTIAgent agent) 
{
  jobject o=(jobject)ps_readQword(agent->pipe);
  jclass oc=_env->GetObjectClass(agent->env, o);
  
  jclass goc=_env->NewGlobalRef(agent->env, oc);
  _env->DeleteLocalRef(agent->env, oc);    
  
  ps_writeQword(agent->pipe, goc);  
}

void js_getObjectClassNames(PCEJVMTIAgent agent) 
{
  debug_log("js_getObjectClassNames");
  int count=ps_readDword(agent->pipe);
  int i;
  
  PMemoryStream ms=ms_create(count*128);
  ms_writeDword(ms, count); 
  for (i=0; i<count; i++)
  {
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
          ms_writeWord(ms, sl);
          ms_write(ms, sig, sl);
          
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);              
        }
        else
          ms_writeWord(ms,0);  
        
        if (gen)
          _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);      
    
      }
      else
        ms_writeWord(ms,0); //empty string
        
      _env->DeleteLocalRef(agent->env, oc);    
    }
    else
    {
      ms_writeWord(ms, 0); //empty string      
    }
  }

  ps_writeMemStream(agent->pipe,ms);    
  ms_destroy(ms);
  
}

void js_getObjectClassName(PCEJVMTIAgent agent) 
{
  debug_log("js_getObjectClassName");
  jobject o=(jobject)ps_readQword(agent->pipe);
  jclass oc;
  
  
  debug_log("o=%p",o);  
  
  if (o==NULL)
    oc=NULL;
  else
    oc=_env->GetObjectClass(agent->env, o);
  
  debug_log("oc=%p",oc);
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
    {
      debug_log("GetClassSignature failed");
      ps_writeWord(agent->pipe,0);
    }
      
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
#ifdef ANDROID
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
#else
  debug_log("Not yet implemented");
#endif
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
  char *sig=NULL;
  char *gen=NULL;
  if ((*(agent->jvmti))->GetClassSignature(agent->jvmti, klass, &sig, &gen)==JVMTI_ERROR_NONE)
  {
    if (sig)
    {
      debug_log("finding instances of class %s", sig);
      _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
      
      debug_log("Cleaned up sig");
    }
    
    if (gen)
    {
     _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);
     debug_log("Cleaned up gen");
    }
    
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
#ifndef ANDROID  
  if (lookupmethod==1)
#endif    
  {
    jint r;
    jvmtiHeapCallbacks callbacks;
    
    
    agent->nextSearchTag++;    
    debug_log("Tagging this class with tag %x", agent->nextSearchTag);    
    r=_jvmti->SetTag(agent->jvmti, klass, agent->nextSearchTag); 
    debug_log("SetTag returned %d", r);
        
    debug_log("initializing callbacks");
    memset(&callbacks,0,sizeof(callbacks));
    callbacks.heap_iteration_callback=(jvmtiHeapIterationCallback)ce_heap_iteration_callback;
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
          
          void *objptr=NULL;
#ifdef ANDROID
          objptr=Thread_DecodeJObject(agent->soa->thread, g_c);
#endif            
          debug_log("%d: %d -> %p", i, g_c, objptr);
        
                
          ms_writeQword(ms,(uint64_t)g_c);  //ce has to delete this global ref manually
          ms_writeQword(ms,(uint64_t)objptr);

          _env->DeleteLocalRef(agent->env, results[i]);
        }
              
        
        _jvmti->Deallocate(agent->jvmti, (unsigned char*)results);
      }
      
      if (tags)
        _jvmti->Deallocate(agent->jvmti, (unsigned char*)tags);
      
      ps_writeMemStream(agent->pipe, ms);
      ms_destroy(ms);      
    }
    else    
      ps_writeDword(agent->pipe,0);
     
  }
#ifdef ANDROID  
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
              
              ms_writeQword(ms,(uint64_t)g_c);  //ce has to delete this global ref manually
              ms_writeQword(ms,(uint64_t)objptr);

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
#endif  
}

void js_setFieldValues(PCEJVMTIAgent agent) 
/*
Sets a bunch of values to the provided objects and fields
*/
{
  debug_log("js_setFieldValues");
  int count=ps_readWord(agent->pipe);
  
  debug_log("Going to modify %d fields", count);
    
  int i;
  for (i=0; i<count; i++)
  {
    debug_log("%d:",i);    
    jobject objectorclass=(jobject)ps_readQword(agent->pipe);
    debug_log("   objectorclass=%d", objectorclass);
    jfieldID fieldid=(jfieldID)ps_readQword(agent->pipe);    
    debug_log("   fieldid=%d", fieldid);
    unsigned char fieldtype=ps_readByte(agent->pipe);    
    debug_log("   fieldtype=%d", fieldtype);
    unsigned char isstatic=ps_readByte(agent->pipe);
    debug_log("   static=%d", isstatic);
    
    jobject obj;
    jclass class;
    if (isstatic)
    {
      class=objectorclass;
      debug_log("   class=%d", class);
    }
    else
    {
      obj=objectorclass;
      debug_log("   obj=%d", obj);
    }
      
    
    switch (fieldtype)
    {
      case 0: //void
        break;
        
      case 1: //boolean
      {
        jboolean v=ps_readByte(agent->pipe);        
        if (!isstatic)
          _env->SetBooleanField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticBooleanField(agent->env, class, fieldid, v);
        
        break;
      }
      
      case 2: //byte
      {
        jbyte v=ps_readByte(agent->pipe);        
        if (!isstatic)
          _env->SetByteField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticByteField(agent->env, class, fieldid, v);
        
        break;
      }
      
      case 3: //char
      {
        jchar v=ps_readWord(agent->pipe);        
        if (!isstatic)
          _env->SetCharField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticCharField(agent->env, class, fieldid, v);
        
        break;
      }  

      case 4: //word
      {
        jshort v=ps_readWord(agent->pipe);        
        if (!isstatic)
          _env->SetShortField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticShortField(agent->env, class, fieldid, v);
        
        break;
      } 

      case 5: //dword
      {
        jint v=ps_readDword(agent->pipe);        
        if (!isstatic)
          _env->SetIntField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticIntField(agent->env, class, fieldid, v);
        
        break;
      }  
      
      case 6: //qword/long
      {
        jlong v=ps_readQword(agent->pipe);        
        if (!isstatic)
          _env->SetLongField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticLongField(agent->env, class, fieldid, v);
        
        break;
      }      

      case 7: //float
      {
        jfloat v;
        ps_read(agent->pipe, &v, 4);        
        if (!isstatic)
          _env->SetFloatField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticFloatField(agent->env, class, fieldid, v);
        
        break;
      }   
      
      case 8: //double
      {
        jdouble v;
        ps_read(agent->pipe, &v, 8);        
        if (!isstatic)
          _env->SetDoubleField(agent->env, obj, fieldid, v);
        else        
          _env->SetStaticDoubleField(agent->env, class, fieldid, v);
        
        break;
      }   

      
      case 9: //object 
      case 10:
      {
        uint64_t v=ps_readQword(agent->pipe);
        jobject o=(jobject)v;
        
        if (!isstatic)
          _env->SetObjectField(agent->env, obj, fieldid, o);
        else
          _env->SetStaticObjectField(agent->env, class, fieldid, o);
        
        break;
      }
      case 11:
      {
        debug_log("String change");   
        uint16_t strlen=ps_readWord(agent->pipe);
        char *str=(char*)malloc(strlen+1);
        
        ps_read(agent->pipe, str,strlen);
        str[strlen]=0;

        debug_log("new string=%s",str);
        
        jobject newstr=_env->NewStringUTF(agent->env, str);
        
        debug_log("newstr=%d");
        
        if (!isstatic)
        {
          debug_log("setting on object field");
          _env->SetObjectField(agent->env, obj, fieldid, newstr);
        }
        else
        {
          debug_log("setting on class static field");
          _env->SetStaticObjectField(agent->env, class, fieldid, newstr);
        }
        
        debug_log("Done. Cleaning up ref");
        if (newstr)
          _env->DeleteLocalRef(agent->env, newstr);
        
        if (str)
          free(str);
        break;
      }
    }
  }
}


void js_getFieldValues(PCEJVMTIAgent agent) 
/*
Retrieve a list of values for the given fieldid's and the object provided
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
    jfieldID field=(jfieldID)ps_readQword(agent->pipe);
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
          ms_writeQword(ms,(uint64_t)g_r);
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
          ms_writeQword(ms,(uint64_t)g_r);
          
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
          char *r=(char *)_env->GetStringUTFChars(agent->env, stringobj, NULL);
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

void js_invokeMethod(PCEJVMTIAgent agent) 
{
  typedef struct
  {
    char *str;
    jobject javastr;  
  } StringInfo, *PStringInfo;
  
  debug_log("js_invokeMethod");
  jobject obj=(jobject)ps_readQword(agent->pipe);
  jmethodID method=(jmethodID)ps_readQword(agent->pipe);
  unsigned char returntype=(unsigned char)ps_readByte(agent->pipe);
  unsigned char argumentcount=(unsigned char)ps_readByte(agent->pipe);
  jint modifiers=0;
  jclass methodclass=0;

  if (_jvmti->GetMethodModifiers(agent->jvmti, method, &modifiers)==0)
  {
    if (modifiers & ACC_STATIC)
    {      
      debug_log("this is a static method");
      if (_jvmti->GetMethodDeclaringClass(agent->jvmti, method, &methodclass))
      {
        debug_log("GetMethodDeclaringClass failed");
        ps_writeQword(agent->pipe, 0);
        return;        
      }
    }    
  }
  else
  {
    debug_log("GetMethodModifiers failed");
    ps_writeQword(agent->pipe, 0);
    return;
  }
  
  
  jvalue *parameters=malloc(sizeof(jvalue)*argumentcount);

  PStringInfo stringinfo=malloc(sizeof(StringInfo)*argumentcount);
  
  if ((!parameters) || (!stringinfo))
  {
    debug_log("parameter or stringinfo alloc failed");
    ps_writeQword(agent->pipe, 0);
    return;
  }
  
  
  memset(parameters,0,sizeof(jvalue)*argumentcount);  
  memset(stringinfo,0,sizeof(StringInfo)*argumentcount); 
  
  int i;
  for (i=0; i<argumentcount; i++)
  {
    unsigned char t=ps_readByte(agent->pipe);

    switch (t)
    {
      case 0: //void
        break;
        
      case 1: //bool
      case 2: //byte
        ps_read(agent->pipe, &parameters[i],1);
        break;
        
      case 3: //2 byte char
      case 4: //word
        ps_read(agent->pipe, &parameters[i],2);
        break;      
       
      case 5: //dword 
      case 7: //float
        ps_read(agent->pipe, &parameters[i],4);
        break;
        
      case 6: //qword
      case 8: //double
      case 9: //object (not string)
      case 10: //array
        ps_read(agent->pipe, &parameters[i],8);
        break;  

      case 11: //string
      {
        int strlen=ps_readWord(agent->pipe);
        stringinfo[i].str=malloc(strlen)+1;
        
        ps_read(agent->pipe, stringinfo[i].str,strlen);
        stringinfo[i].str[strlen]=0;
        
        stringinfo[i].javastr=_env->NewStringUTF(agent->env, stringinfo[i].str);        
        parameters[i].l=stringinfo[i].javastr;
        break;      
      }
    }
  }
  
  debug_log("all parameters have been read out. Going to call the method with %d parameters", argumentcount);
    
  //call
  jvalue result;
  result.j=0;
  
  switch (returntype)
  {
    case 0: //void
      if (methodclass)
        _env->CallStaticVoidMethodA(agent->env, methodclass, method, parameters);        
      else
        _env->CallVoidMethodA(agent->env, obj, method, parameters);
      break;  

    case 1: //boolean  
      if (methodclass)    
        result.z=_env->CallStaticBooleanMethodA(agent->env, methodclass, method, parameters);        
      else
        result.z=_env->CallBooleanMethodA(agent->env, obj, method, parameters);
      break;  

    case 2: //byte
      if (methodclass)
        result.b=_env->CallStaticByteMethodA(agent->env, methodclass, method, parameters);
      else
        result.b=_env->CallByteMethodA(agent->env, obj, method, parameters);
      break;
      
    case 3: //2 byte char
      if (methodclass)
        result.c=_env->CallStaticCharMethodA(agent->env, methodclass, method, parameters);
      else        
        result.c=_env->CallCharMethodA(agent->env, obj, method, parameters);
      
      break;    

    case 4: //2 byte
      if (methodclass)
        result.s=_env->CallStaticShortMethodA(agent->env, methodclass, method, parameters);        
      else
        result.s=_env->CallShortMethodA(agent->env, obj, method, parameters);
      break;  
      
    case 5: //4 byte
      if (methodclass)
        result.i=_env->CallStaticIntMethodA(agent->env, methodclass, method, parameters);
      else 
        result.i=_env->CallIntMethodA(agent->env, obj, method, parameters);
      break;     

    case 6: //8 byte
      if (methodclass)
        result.j=_env->CallStaticLongMethodA(agent->env, methodclass, method, parameters);
      else
        result.j=_env->CallLongMethodA(agent->env, obj, method, parameters);

      break;      

    case 7: //float
      if (methodclass)
        result.f=_env->CallStaticFloatMethodA(agent->env, methodclass, method, parameters);
      else
        result.f=_env->CallFloatMethodA(agent->env, obj, method, parameters);
      break; 

    case 8: //double
      if (methodclass)
        result.d=_env->CallStaticDoubleMethodA(agent->env, methodclass, method, parameters);
      else
        result.d=_env->CallDoubleMethodA(agent->env, obj, method, parameters);
      break; 
      
    case 9: //object
    case 10: //array object
    case 11: //string
      if (methodclass)    
        result.l=_env->CallStaticObjectMethodA(agent->env, obj, method, parameters);        
      else
        result.l=_env->CallObjectMethodA(agent->env, obj, method, parameters);
      
      if (result.l)
      {
        //upgrade to global ref
        jobject gr=_env->NewGlobalRef(agent->env, result.l);
        
        _env->DeleteLocalRef(agent->env, result.l);
        result.l=gr;        
      }
      break; 
  }
  
  _env->ExceptionClear(agent->env);
  
  if (returntype==11) //string
  {
    PMemoryStream ms=ms_create(256);
    debug_log("string type");
    if (result.l)
    {
      int sl;
      debug_log("valid string");
      ms_writeByte(ms, 1);  //valid
      char *s=_env->GetStringUTFChars(agent->env, result.l, NULL);
      
      debug_log("string=%s",s);
      
      sl=strlen(s);
      debug_log("sl=%d",sl);
      ms_writeWord(ms, sl);      
      ms_write(ms, s, sl);
      
      _env->ReleaseStringUTFChars(agent->env, result.l, s);
      _env->DeleteLocalRef(agent->env, result.l);
    }
    else
    {
      debug_log("nil string");
      ms_writeByte(ms, 0); //is nil      
    }
    
    ps_writeMemStream(agent->pipe, ms);
    ms_destroy(ms);
  }
  else  
    ps_write(agent->pipe, &result,8);
  
  //cleanup all allocated strings
  debug_log("after methodcall. Cleaning up");
  for (i=0; i<argumentcount; i++)
  {
    if (stringinfo[i].javastr)
    {
      _env->DeleteLocalRef(agent->env, stringinfo[i].javastr);
      stringinfo[i].javastr=0;
    }
    
    if (stringinfo[i].str)   
    {
      free(stringinfo[i].str);
      stringinfo[i].str=NULL;
    }   
  }    
  
  free(stringinfo);
  free(parameters);
  
  if (methodclass)
    _env->DeleteLocalRef(agent->env, methodclass);
  
  debug_log("invokeMethod end");
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
#ifdef ANDROID
void js_android_decodeObject(PCEJVMTIAgent agent) 
{
  jobject obj=(jobject)ps_readQword(agent->pipe);
  void *objptr;
  objptr=Thread_DecodeJObject(agent->soa->thread, obj);
  ps_writeQword(agent->pipe, (uint64_t)objptr);
}
#endif

typedef char * (*stringcomparefunction)(const char *haystack, const char *needle);
  
void js_findFields(PCEJVMTIAgent agent) 
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
  
  stringcomparefunction comp;
  
  int caseSensitive=ps_readByte(agent->pipe);
  if (caseSensitive)
  {
    comp=(stringcomparefunction)strstr;
    debug_log("case sensitive (comp=%p)",comp);
  }
  else
  {
    comp=(stringcomparefunction)strcasestr;
    debug_log("case insensitive (comp=%p)",comp);
  }
  

  short l;
  char *searchname=NULL;
  char *searchsig=NULL;
  
  l=ps_readWord(agent->pipe);
  if (l)
  {
    searchname=(char*)malloc(l+1);
    ps_read(agent->pipe, searchname,l);
    searchname[l]=0; 
  }
  
  l=ps_readWord(agent->pipe);
  if (l)
  {
    searchsig=(char*)malloc(l+1);
    ps_read(agent->pipe, searchsig,l);
    searchsig[l]=0; 
  }  
  
  if (searchname && searchsig)  
    debug_log("scanning for name:%s and sig:%s",searchname, searchsig);
  else
  if (searchname)
    debug_log("scanning for name:%s and sig:*", searchname);
  else
  if (searchsig)
    debug_log("scanning for name:* and sig:%s", searchsig);
  else
  {
    debug_log("scanning for nothing");
    ps_writeDword(agent->pipe,0);
    return;    
  }
  
  int i;
  jint count;
  jfieldID *fields=NULL;  

  PMemoryStream ms=ms_create(16*1024);
  
  debug_log("agent->classcount=%d",agent->classcount);
  
  for (i=0; i<agent->classcount; i++)
  {   
   // debug_log("%d",i);
    if (agent->classlist[i]==NULL)
    {
     // debug_log("classlist[%d]==NULL",i);
      continue;
    }
    
    jint status;
    
    if (_jvmti->GetClassStatus(agent->jvmti, agent->classlist[i], &status)==JVMTI_ERROR_NONE)
    {
     // debug_log("%d=%d", i, status);
      if (status!=7) 
      {
        //debug_log("skip");
        continue;
      }
    }
    else
    {
     // debug_log("%d GetClassStatus failed",i);
      continue;
    }
   
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
          int valid=0;
          if (name)
          {
            
            if (searchname && comp(name, searchname))
              valid=1;
            
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);  
          }
          
          if (sig)
          {
            if (searchsig && comp(sig, searchsig))
              valid=1;
            
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
          }
          
          if (gen)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);  
          
          if (valid)
          {
            debug_log("found a match");
            ms_writeQword(ms, (uint64_t)agent->classlist[i]);
            ms_writeQword(ms, (uint64_t)fields[j]);
          }
              
        }
      }
    } 
    _jvmti->Deallocate(agent->jvmti, (unsigned char *)fields);    
  }
  
  debug_log("ms->pos=%d", ms->pos);
  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms);
  
  debug_log("scan done");
  
  if (searchname)
    free(searchname);
  
  if (searchsig)
    free(searchsig);
}

void js_findMethods(PCEJVMTIAgent agent) 
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
  
  stringcomparefunction comp;
  
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
  

  short l;
  char *searchname=NULL;
  char *searchsig=NULL;
  
  l=ps_readWord(agent->pipe);
  if (l)
  {
    searchname=(char*)malloc(l+1);
    ps_read(agent->pipe, searchname,l);
    searchname[l]=0; 
  }
  
  l=ps_readWord(agent->pipe);
  if (l)
  {
    searchsig=(char*)malloc(l+1);
    ps_read(agent->pipe, searchsig,l);
    searchsig[l]=0; 
  }  
  
  if (searchname && searchsig)  
      debug_log("scanning for name:%s and sig:%s",searchname, searchsig);
  else
  if (searchname)
    debug_log("scanning for name:%s", searchname);
  else
  if (searchsig)
    debug_log("scanning for sig:%s", searchsig);
  else
  {
    debug_log("scanning for nothing");
    ps_writeDword(agent->pipe,0);
    return;    
  }
  
  int i;
  jint count;
  jmethodID *methods=NULL;  

  PMemoryStream ms=ms_create(16*1024);
  
  for (i=0; i<agent->classcount; i++)
  {   

    if (agent->classlist[i]==NULL)
    {
     // debug_log("classlist[%d]==NULL",i);
      continue;
    }
    
    jint status;
    
    if (_jvmti->GetClassStatus(agent->jvmti, agent->classlist[i], &status)==JVMTI_ERROR_NONE)
    {
     // debug_log("%d=%d", i, status);
      if (status!=7) 
      {
        //debug_log("skip");
        continue;
      }
    }
    else
    {
     // debug_log("%d GetClassStatus failed",i);
      continue;
    }
    
    if (_jvmti->GetClassMethods(agent->jvmti, agent->classlist[i], &count, &methods)==JVMTI_ERROR_NONE)
	  {
      int j;
      for (j=0; j<count; j++)
      {
        char *name;
        char *sig;
        char *gen;
        
        if ((methods[j]) && (_jvmti->GetMethodName(agent->jvmti, methods[j], &name, &sig, &gen)==JVMTI_ERROR_NONE))
        {
          int valid=0;
          if (name)
          {            
            if (searchname && comp(name, searchname))
              valid=1;
            
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)name);  
          }
          
          if (sig)
          {
            if (searchsig && comp(sig, searchsig))
              valid=1;
            
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)sig);
          }
          
          if (gen)
            _jvmti->Deallocate(agent->jvmti, (unsigned char *)gen);  
          
          if (valid)
          {
            ms_writeQword(ms, (uint64_t)agent->classlist[i]);
            ms_writeQword(ms, (uint64_t)methods[j]);
          }
              
        }
      }
    } 
    _jvmti->Deallocate(agent->jvmti, (unsigned char *)methods);    
  }
  
  debug_log("ms->pos=%d", ms->pos);
  ps_writeMemStream(agent->pipe, ms);
  ms_destroy(ms);
  
  debug_log("scan done");
  
  
  if (searchname)
    free(searchname);
  
  if (searchsig)
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
  
#ifdef ANDROID
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
#endif
  
#ifdef _WIN32
  snprintf(xpipename, 99, "\\\\.\\pipe\\cejavadc_pid%d", pid);
#else  
  snprintf(xpipename, 99, "cejavadc_pid%d", pid);
#endif
  
  debug_log("Creating pipe %s", xpipename);
  
  int terminated=0;
  //while (!terminated)
  {
    agent->pipe=CreatePipeServer(xpipename);
    

    debug_log("CreatePipeServer returned %p", agent->pipe);
    
    while (agent && (agent->pipe) && ps_isvalid(agent->pipe))
    {
      debug_log("waiting for command");
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
            
          case JAVACMD_SETFIELDVALUES:
            debug_log("JAVACMD_SETFIELDVALUES");
            js_setFieldValues(agent);
            break;
            
          case JAVACMD_COLLECTGARBAGE:
            js_collectGarbage(agent);
            break;
            
#ifdef ANDROID            
          case JAVACMD_ANDROID_DECODEOBJECT:
            js_android_decodeObject(agent);
            break;
#endif            
            
          case JAVACMD_FINDFIELDS:
            js_findFields(agent);
            break;
            
          case JAVACMD_FINDMETHODS:
            js_findMethods(agent);
            break;  

          case JAVACMD_GETOBJECTCLASS:
            js_getObjectClass(agent);
            break;
            
          case JAVAVMD_GETOBJECTCLASSNAME:
            js_getObjectClassName(agent);
            break;
            
          case JAVACMD_INVOKEMETHOD:
            js_invokeMethod(agent);
            break;   

          case JAVACMD_STARTSCAN:
            js_startscan(agent);
            break;
            
          case JAVACMD_REFINESCANRESULTS:
            js_refinescanresults(agent);
            break;  

          case JAVACMD_GETSCANRESULTS:    
            js_getscanresults(agent);
          
            break;
            
          case JAVACMD_GETOBJECTCLASSNAMES:
            js_getObjectClassNames(agent);
            break;
            
          case JAVACMD_GETFIELDSIGNATURE:
            js_getFieldSignature(agent);
            break;
            
          case JAVACMD_GETFIELDSIGNATUREBYOBJECT:
            js_getFieldSignatureByObject(agent);
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