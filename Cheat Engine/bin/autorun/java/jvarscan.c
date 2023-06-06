#ifndef jvarscan
#define jvarscan

#include <jni.h>
#include <cepipelib.c>

void *memset(void *s, int c, size_t n);

#define ST_UNKNOWN 0xffffffff
#define ST_EXACT 0
#define ST_INCREASED 1
#define ST_DECREASED 2
#define ST_CHANGED 3
#define ST_UNCHANGED 4
                

typedef struct
{
	//split seperately so it's faster (no double to int convertions during the scan)
	int scantype;    //0
	int booleanScan; //4, include boolean fields when scanning
	jboolean zValue; //8
	jbyte bValue;    //9
	jchar cValue;    //10
	jshort sValue;   //12
	jint iValue;     //16
	jlong jValue;    //24
	jfloat fMinValue, fMaxValue; //32,36
	jdouble dMinValue, dMaxValue; //40,48
} ScanData, *PScanData;


typedef struct
{	
	jlong objectTag;
	jint fieldindex;
	jfieldID fieldID;
	jvmtiPrimitiveType type;
	jvalue lastvalue;
	jvmtiHeapReferenceKind kind;
} ScanResult, *PScanResult;


PScanResult ScanResults;
int ScanResultsSize;
int ScanResultsPos=0;

jlong initialtag;
int tagcount=0;


jint JNICALL StartScan_FieldIteration(jvmtiHeapReferenceKind kind, const jvmtiHeapReferenceInfo* info, jlong object_class_tag, jlong* object_tag_ptr, jvalue value, jvmtiPrimitiveType value_type, void *user_data)
{
	
	if (kind==JVMTI_HEAP_REFERENCE_FIELD)
	{
		BOOL add=TRUE;
		PScanData sd=(PScanData)user_data;

		if (sd->scantype!=ST_UNKNOWN)
		{
			add=FALSE;
			switch (value_type)
			{
				
				case JVMTI_PRIMITIVE_TYPE_BOOLEAN:
					add=(sd->booleanScan) && (value.z==sd->zValue);
					break;

				case JVMTI_PRIMITIVE_TYPE_BYTE:
					add=value.b==sd->bValue;
					break;

				case JVMTI_PRIMITIVE_TYPE_CHAR:
					add=value.c==sd->cValue;
					break;

				case JVMTI_PRIMITIVE_TYPE_SHORT:
					add=value.s==sd->sValue;
					break;

				case JVMTI_PRIMITIVE_TYPE_INT:
					add=value.i==sd->iValue;
					break;

				case JVMTI_PRIMITIVE_TYPE_LONG:
					add=value.j==sd->jValue;
					break;

				case JVMTI_PRIMITIVE_TYPE_FLOAT:
					add=(value.f >= sd->fMinValue) && (value.f <=sd->fMaxValue);
					break;

				case JVMTI_PRIMITIVE_TYPE_DOUBLE:
					add=(value.d >= sd->dMinValue) && (value.d <=sd->dMaxValue);
					break;
			}
		}

		if (add)
		{
			ScanResult sr;
			if (*object_tag_ptr<=initialtag) //check if it's 0 or part of a previous tagging operation
				*object_tag_ptr=0xce000000+tagcount++;

			sr.objectTag=*object_tag_ptr;
			sr.fieldindex=info->field.index;
			sr.type=value_type;
			sr.lastvalue=value;
			sr.fieldID=0;

			sr.kind=kind;
      
      if (ScanResultsPos>=ScanResultsSize)
      {
        if (ScanResultsSize<80000) //near 4MB
          ScanResultsSize*=2;
        else
          ScanResultsSize+=80000;        
        
        ScanResults=(ScanResult*)realloc(ScanResults, sizeof(ScanResult)*ScanResultsSize);        
      }
      
      ScanResults[ScanResultsPos]=sr;
      ScanResultsPos++;
		}
	}

	return JVMTI_VISIT_OBJECTS;
}


void GetAllClassesAndInterfacesFromClass(jvmtiEnv *jvmti, JNIEnv *jni, 
  jclass c,
  jclass **classes, 
  int *classeslen,
  int *classespos,   
  jclass **interfaces,
  int *interfaceslen,
  int *interfacespos
  )
{
	jint icount;
	jboolean isInterface;
	jclass *implementedinterfaces=NULL;
	

	(*jvmti)->IsInterface(jvmti, c, &isInterface);
	if (isInterface)
	{
		//check if this class is already defined
		int i;
		for (i=0; i<(*interfacespos); i++)
		{
			if ((*jni)->IsSameObject(jni, (*interfaces)[i], c))
			{
				return; //already in the list			
			}
		}
		//still here
    (*interfaces)[*interfacespos]=c;
    (*interfacespos)++;		
    
    if ((*interfacespos) >= (*interfaceslen))
    {
      //realloc
      int newlen;
      if ((*interfaceslen)<1024)
        newlen=(*interfaceslen)*2;
      else
        newlen=(*interfaceslen)+4096;
      
      *interfaces=(jclass *)realloc((*interfaces),newlen*sizeof(jclass));      
    }
	}
	else
  {
    (*classes)[*classespos]=c;
    (*classespos)++;
   
    if ((*classespos) >= (*classeslen))
    {
      //realloc
      int newlen;
      if ((*classeslen)<1024)
        newlen=(*classeslen)*2;
      else
        newlen=(*classeslen)+4096;
      
      *classes=(jclass *)realloc((*classes),newlen*sizeof(jclass));        
    }

  }


	if ((*jvmti)->GetImplementedInterfaces(jvmti, c, &icount, &implementedinterfaces)==JVMTI_ERROR_NONE)
	{	
		for (int i=0; i<icount; i++)
			GetAllClassesAndInterfacesFromClass(jvmti, jni, implementedinterfaces[i], classes, classeslen, classespos, interfaces, interfaceslen, interfacespos);

		if (implementedinterfaces)
			(*jvmti)->Deallocate(jvmti, (unsigned char*)implementedinterfaces);
	}

	jclass currentclass=(*jni)->GetSuperclass(jni, c);
	if (currentclass)
		GetAllClassesAndInterfacesFromClass(jvmti, jni, currentclass, classes, classeslen, classespos, interfaces, interfaceslen, interfacespos);
}


void GetAllFieldsFromClass(jvmtiEnv *jvmti, JNIEnv *jni, jclass c, jfieldID **allfields, int *allfieldspos, int *allfieldslen)
{
  jclass *classes;
  int classeslen;
  int classespos;
  
  jclass *interfaces;
  int interfaceslen;
  int interfacespos;
  
  classes=(jclass*)malloc(32*sizeof(jclass));
  classeslen=32;
  classespos=0;
  
  interfaces=(jclass*)malloc(16*sizeof(jclass));
  interfaceslen=16;
  interfacespos=0;
  
  

	GetAllClassesAndInterfacesFromClass(jvmti, jni, c, &classes, &classeslen, &classespos, &interfaces, &interfaceslen, &interfacespos);

	

	//now that we have a list of all the interfaces and classes, get their fields
	
	//first the interfaces
  int i;
	

	for (i=interfacespos-1; i>=0; i--)
	{
		jint fcount;
		jfieldID *fields=NULL;
		if ((*jvmti)->GetClassFields(jvmti, interfaces[i], &fcount, &fields)==JVMTI_ERROR_NONE)
		{
			for (int j=0; j<fcount; j++)
      {
        (*allfields)[*allfieldspos]=fields[j];
        (*allfieldspos)++;
        if ((*allfieldspos)>=(*allfieldslen))
        {
          int newsize;
          if ((*allfieldslen)<1024)
            (*allfieldslen)=(*allfieldslen)*2;          
          else
            (*allfieldslen)=(*allfieldslen)+1024;

          *allfields=(jfieldID*)realloc(*allfields, (*allfieldslen)*sizeof(jfieldID));
        }
				
      }
			
			if (fields)
				(*jvmti)->Deallocate(jvmti, (unsigned char*)fields);
		}

		(*jni)->DeleteLocalRef(jni, interfaces[i]);
	}
	

	//and then the classes (reversed)
	for (int i=classespos-1; i>=0; i--)
	{
		jint fcount;
		jfieldID *fields=NULL;
		if ((*jvmti)->GetClassFields(jvmti, classes[i], &fcount, &fields)==JVMTI_ERROR_NONE)
		{
			for (int j=0; j<fcount; j++)
      {
        (*allfields)[*allfieldspos]=fields[j];
        (*allfieldspos)++;
        if ((*allfieldspos)>=(*allfieldslen))
        {
          int newsize;
          if ((*allfieldslen)<1024)
            (*allfieldslen)=(*allfieldslen)*2;          
          else
            (*allfieldslen)=(*allfieldslen)+1024;

          *allfields=(jfieldID*)realloc(*allfields, (*allfieldslen)*sizeof(jfieldID));
        }
				
      }
			
			if (fields)
				(*jvmti)->Deallocate(jvmti, (unsigned char*)fields);
		}

		if (i>0)
			(*jni)->DeleteLocalRef(jni, classes[i]);	


	}	
  
  free(classes);
  free(interfaces);

}

jfieldID getFieldIDFromFieldIndex(jvmtiEnv *jvmti, JNIEnv *jni, jobject o, int index)
{
	jboolean isinterface=1;
	jfieldID *fields;
  int fieldspos=0;
  int fieldslen=32;
  
  fields=(jfieldID*)malloc(32*sizeof(jfieldID));

	jclass currentclass=(*jni)->GetObjectClass(jni, o);

	(*jvmti)->IsInterface(jvmti, currentclass, &isinterface);
	if (isinterface)
	{
		OutputDebugStringA("Interface...");
		return 0;
	}


	GetAllFieldsFromClass(jvmti, jni, currentclass, &fields, &fieldspos, &fieldslen);

	(*jni)->DeleteLocalRef(jni, currentclass);

	if (fieldspos>(unsigned int)index)
		return fields[index];
	else
		return 0;
}


int jvarscan_refineScanResults(jvmtiEnv *jvmti, JNIEnv *jni, ScanData sd)
{
  int i;
  int newScanResultsPos=0; //ScanResultsPos can never increase
  int scantype=sd.scantype;
  
	//go through the results and check if they are usable or invalid now
  for (i=0; i<ScanResultsPos; i++)
	{
    ScanResult sr=ScanResults[i];
		BOOL valid=FALSE;
		jint count=0;
		jobject *objects;
    
		
		//for unknown initial value, it might be more efficient to just start with getting all the objects and enumerating the fields and then get the value instead of this
		(*jvmti)->GetObjectsWithTags(jvmti, 1, &sr.objectTag, &count, &objects, NULL); 
		if (count)
		{      
			jobject o=objects[0];

			if (sr.fieldID==0)			
				sr.fieldID=getFieldIDFromFieldIndex(jvmti, jni, o, sr.fieldindex);			

		
			if (sr.fieldID) //so not 0
			{
				//check the current value

				switch (sr.type)
				{					
					case JVMTI_PRIMITIVE_TYPE_BOOLEAN:
					{						
						jboolean newvalue=(*jni)->GetBooleanField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.zValue; //exact value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.z; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.z; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_BYTE:
					{
						jbyte newvalue=(*jni)->GetByteField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.bValue; //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.b; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.b; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.b; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.b; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_CHAR:
					{
						jchar newvalue=(*jni)->GetShortField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.cValue; //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.c; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.c; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.c; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.c; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_SHORT:
					{
						jshort newvalue=(*jni)->GetShortField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.sValue; //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.s; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.s; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.s; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.s; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_INT:
					{
						jint newvalue=(*jni)->GetIntField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.iValue; //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.i; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.i; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.i; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.i; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_LONG:
					{
						jlong newvalue=(*jni)->GetLongField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=newvalue==sd.jValue; //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.j; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.j; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.j; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.j; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_FLOAT:
					{

						jfloat newvalue=(*jni)->GetFloatField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=(newvalue>=sd.fMinValue) && (newvalue<=sd.fMaxValue); //exact value
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.f; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.f; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.f; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.f; //unchanged
						}

						break;
					}

					case JVMTI_PRIMITIVE_TYPE_DOUBLE:
					{
						jdouble newvalue=(*jni)->GetDoubleField(jni, o, sr.fieldID);
						switch(scantype)
						{
							case ST_EXACT:
								valid=(newvalue>=sd.dMinValue) && (newvalue<=sd.dMaxValue);
								break;

							case ST_INCREASED:
								valid=newvalue>sr.lastvalue.d; //increased value
								break;

							case ST_DECREASED:
								valid=newvalue<sr.lastvalue.d; //decreased value
								break;

							case ST_CHANGED:
								valid=newvalue!=sr.lastvalue.d; //changed
								break;

							case ST_UNCHANGED:
								valid=newvalue==sr.lastvalue.d; //unchanged
						}

						break;
					}

				} //switch


				
			}

			(*jni)->DeleteLocalRef(jni, o);

		}

		if (valid)
    {
      ScanResults[newScanResultsPos]=sr;
      newScanResultsPos++;
    }
	}
  
  ScanResultsPos=newScanResultsPos;
  
  return ScanResultsPos;
}


int jvarscan_StartScan(jvmtiEnv *jvmti, ScanData sd)
{
	//iterate over all objects and fields, and for each primitive field create a record with objectid (tagged object), fieldid and original value

	jvmtiHeapCallbacks callbacks;

	//ScanData
	ScanResultsPos=0;
	

  memset(&callbacks, 0, sizeof(callbacks)); //ZeroMemory(&callbacks, sizeof(callbacks));

	callbacks.primitive_field_callback=StartScan_FieldIteration;


	initialtag=0xce000000+tagcount;
	
	(*jvmti)->IterateThroughHeap(jvmti, 0, NULL, &callbacks, &sd);
  
  return ScanResultsPos;
}


#endif