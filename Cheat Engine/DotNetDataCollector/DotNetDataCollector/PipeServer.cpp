#include "StdAfx.h"
#include "PipeServer.h"


const IID IID_ICorDebugProcess={0x3d6f5f64, 0x7538, 0x11d3, 0x8d, 0x5b, 0x00, 0x10, 0x4b, 0x35, 0xe7, 0xef};  
const IID IID_ICorDebugProcess5={0x21e9d9c0, 0xfcb8, 0x11df, 0x8c, 0xff, 0x08, 0x00, 0x20, 0x0c ,0x9a, 0x66};
const IID IID_ICorDebugCode2={0x5F696509,0x452F,0x4436,0xA3,0xFE,0x4D,0x11,0xFE,0x7E,0x23,0x47}; //5F696509-452F-4436-A3FE-4D11FE7E2347


//const IID IID_ICorDebug={0x3d6f5f61, 0x7538, 0x11d3, 0x8d, 0x5b, 0x00, 0x10, 0x4b, 0x35 ,0xe7, 0xef}; //3d6f5f61-7538-11d3-8d5b-00104b35e7ef
//const IID CLSID_CorDebug={0x6fef44d0,0x39e7,0x4c77,0xbe,0x8e,0xc9,0xf8,0xcf,0x98,0x86,0x30}; //6fef44d0-39e7-4c77-be8e-c9f8cf988630

using namespace std;

CPipeServer::CPipeServer(TCHAR *name)
{
	processhandle=0;	
	CorDebugProcess5=NULL;
	CorDebugProcess=NULL;
	CLRDebugging=NULL;
	libprovider=NULL;	
	datacallback=NULL;
	

	StrCpy(pipename, L"\\\\.\\pipe\\");
	StrCat(pipename, name);

	pipe=CreateNamedPipeW(pipename, PIPE_ACCESS_DUPLEX, PIPE_TYPE_BYTE | PIPE_READMODE_BYTE | PIPE_WAIT, 255,256*1024, 16, INFINITE, NULL);

	if (StrCmp(name,L"BLA")==0)
	{
		//do some debug stuff
		processid=0x1814;
		OpenOrAttachToProcess();

		getAddressData(0x021ABF0C);
	}
	else
		ConnectNamedPipe(pipe, NULL);



}

CPipeServer::~CPipeServer(void)
{
	if (processhandle)
		CloseHandle(processhandle);


	
	if (CorDebugProcess5)
		CorDebugProcess5->Release();
	
	if (CorDebugProcess)
		CorDebugProcess->Release();

	if (CLRDebugging)
		CLRDebugging->Release();


	if (libprovider)
		delete libprovider;

	if (datacallback)
		delete datacallback;


}

BOOL CPipeServer::OpenOrAttachToProcess(void)
{
	ICLRMetaHost *pMetaHost;
	IEnumUnknown *RuntimeEnum;
	HANDLE ths;
	MODULEENTRY32 m;
	HRESULT r;
	BOOL result=FALSE;
	
	

	CLR_DEBUGGING_VERSION v;
	v.wStructVersion=0;
	v.wMajor=4;
	v.wMinor=0;
	v.wRevision=30319;
	v.wBuild=65535;


	CLRDebugging=NULL;
	CorDebugProcess=NULL;
	CorDebugProcess5=NULL;

	libprovider=NULL;
	datacallback=NULL;



	HMODULE hMscoree = LoadLibraryA("mscoree.dll");
	CLRCreateInstanceFnPtr CLRCreateInstance;

	if (hMscoree==NULL)
		return FALSE;

	CLRCreateInstance=(CLRCreateInstanceFnPtr)GetProcAddress(hMscoree, "CLRCreateInstance");
	if (CLRCreateInstance==NULL)
		return FALSE; //only 4.0 is supported for now

	processhandle=OpenProcess(PROCESS_ALL_ACCESS , FALSE, processid);
	if (processhandle==0)
		return FALSE;
	
	if (CLRCreateInstance(CLSID_CLRMetaHost, IID_ICLRMetaHost, (LPVOID*)&pMetaHost)!=S_OK)
		return FALSE;
		
	
	if (pMetaHost->EnumerateLoadedRuntimes(processhandle, &RuntimeEnum)==S_OK)
	{
		ICLRRuntimeInfo *info;
		ULONG count=0;

		RuntimeEnum->Next(1, (IUnknown **)&info, &count);
		if (count)
		{
			result=TRUE;
			libprovider=new CMyICLRDebuggingLibraryProvider(info);  //todo: scan for 4.0			
		}

		RuntimeEnum->Release();
	}
	pMetaHost->Release();

	if (!result)
		return FALSE; //no runtimes

	
	if (CLRCreateInstance(CLSID_CLRDebugging, IID_ICLRDebugging, (void **)&CLRDebugging)!=S_OK)
		return FALSE;

	

	
	ths=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE | TH32CS_SNAPMODULE32, processid);
	if (ths==INVALID_HANDLE_VALUE)
		return FALSE;

	

	ZeroMemory(&m, sizeof(m));
	m.dwSize=sizeof(m);
	if (Module32First(ths, &m))
	{
		datacallback=new CMyIcorDebugDataTarget(processhandle);

		do
		{
			CLR_DEBUGGING_PROCESS_FLAGS flags;

			r=CLRDebugging->OpenVirtualProcess((ULONG64)m.hModule, datacallback,  libprovider, &v, IID_ICorDebugProcess, (IUnknown **)&CorDebugProcess, &v, &flags);
			
			if (r==S_OK)
			{					
				CorDebugProcess->QueryInterface(IID_ICorDebugProcess5, (void **)&CorDebugProcess5);
				break;
			}

		}
		

		while (Module32Next(ths, &m));
	}


	CloseHandle(ths);


	if (r!=S_OK)
	{
#ifdef _DEBUG
		/*
		//todo for older (<4.0) c# versions:
		WCHAR Version[255];
		DWORD vlength;

		ICorDebug *CorDebug;
		processhandle=OpenProcess(PROCESS_ALL_ACCESS, FALSE, processid);

		r=GetVersionFromProcess(processhandle, Version, 255, &vlength);	

		r=CreateDebuggingInterfaceFromVersion(CorDebugVersion_2_0,  Version, (IUnknown **)&CorDebug);

		CorDebug->Initialize();
		//Cor

		CMyICorDebugManagedCallback *MyICorDebugManagedCallback;

		MyICorDebugManagedCallback=new CMyICorDebugManagedCallback();
		
		r=CorDebug->SetManagedHandler((ICorDebugManagedCallback *)MyICorDebugManagedCallback);

		
		r=CorDebug->DebugActiveProcess(processid, false, &CorDebugProcess);

		CorDebugProcess->QueryInterface(IID_ICorDebugProcess5, (void **)&CorDebugProcess5);

		CorDebug->Initialize();

		while (MyICorDebugManagedCallback->attached==FALSE) Sleep(100);

		//r=CorDebug->GetProcess(processid, &CorDebugProcess);

	
		CorDebugProcess->Stop(FALSE);
		CorDebugProcess->Detach();
		CorDebug->Release();
		*/

#endif
		if (r!=S_OK)
			return FALSE;
	}


	

	//still here
	return TRUE;
}

void CPipeServer::releaseObjectHandle(UINT64 hObject)
{
	IUnknown *o=(IUnknown *)hObject;

	o->Release();
}

void CPipeServer::enumDomains(void)
{
	if (CorDebugProcess)
	{
		ICorDebugAppDomainEnum *AppDomains;
		DWORD32 NumberOfDomains=0;
		DWORD bw;
		

		if (CorDebugProcess->EnumerateAppDomains(&AppDomains)==S_OK)
		{
			ULONG count;
			ICorDebugAppDomain **domains;
			ULONG32 namelength=0;
			TCHAR domainname[255];
			
			AppDomains->GetCount(&count);
			domains=(ICorDebugAppDomain **)malloc(count*sizeof(ICorDebugAppDomain *));
			AppDomains->Next(count, domains, &count);

			NumberOfDomains=count;
			WriteFile(pipe, &NumberOfDomains, sizeof(NumberOfDomains), &bw, NULL); //send the number of results to receive
			for (unsigned int i=0; i<count; i++)
			{
				UINT64 hDomain=(UINT64)domains[i];
				
				
				WriteFile(pipe, &hDomain, sizeof(hDomain), &bw, NULL); //send the unique ID

				namelength=0;
				domains[i]->GetName(255, &namelength, domainname);

				namelength=namelength*sizeof(TCHAR);

				WriteFile(pipe, &namelength, sizeof(namelength), &bw, NULL); //send the name
				WriteFile(pipe, domainname, namelength, &bw, NULL);
			}

			if (domains)
				free(domains);
			
			AppDomains->Release();
		}
		else
			WriteFile(pipe, &NumberOfDomains, sizeof(NumberOfDomains), &bw, NULL);
		
	}

}

void CPipeServer::enumModules(UINT64 hDomain)
/*
Enumerate the modules of the given ICorDebugAppDomain
*/
{
	ICorDebugAppDomain *domain=(ICorDebugAppDomain *)hDomain;
	ICorDebugAssemblyEnum *pAssemblies;	
	ICorDebugAssembly *assemblies[5];
	DWORD32 NumberOfModules=0;
	DWORD bw;
	unsigned int i,j;
	ULONG count;
	HRESULT r;

	std::vector<ICorDebugModule *> modulelist;

	if ((!domain) || (domain->EnumerateAssemblies(&pAssemblies)!=S_OK))
	{
		WriteFile(pipe, &NumberOfModules, sizeof(NumberOfModules), &bw, NULL);
		return;
	}

	modulelist.clear();

	do
	{
		r=pAssemblies->Next(5, assemblies, &count);
		for (i=0; i<count; i++)
		{
			ICorDebugModuleEnum *pModules;
			if (assemblies[i]->EnumerateModules(&pModules)==S_OK)
			{
				ULONG modulecount;
				ICorDebugModule *modules[5];
				HRESULT r2;
				do
				{
					r2=pModules->Next(5, modules, &modulecount);
					for (j=0; j<modulecount; j++)
					{
						modulelist.push_back(modules[j]);
					}
					
				} while (r2==S_OK);

				pModules->Release();
			}

			assemblies[i]->Release();
		}
	} while (r==S_OK);

	pAssemblies->Release();

	NumberOfModules=(DWORD32)modulelist.size();
	WriteFile(pipe, &NumberOfModules, sizeof(NumberOfModules), &bw, NULL); //count

	for (i=0; i<NumberOfModules; i++)
	{
		UINT64 hModule=(UINT64)modulelist[i];
		TCHAR modulename[255];
		ULONG32 modulenamelength;
		CORDB_ADDRESS baseaddress;

		WriteFile(pipe, &hModule, sizeof(hModule), &bw, NULL); //'handle'

		modulelist[i]->GetBaseAddress(&baseaddress);
		WriteFile(pipe, &baseaddress, sizeof(baseaddress), &bw, NULL); //baseaddress  (uint64) 


		if (modulelist[i]->GetName(255, &modulenamelength, modulename)==S_OK)
			modulenamelength=sizeof(TCHAR)*modulenamelength;
		else
			modulenamelength=0;

		WriteFile(pipe, &modulenamelength, sizeof(modulenamelength), &bw, NULL); //namelength in bytes
		if (modulenamelength)
			WriteFile(pipe, modulename, modulenamelength, &bw, NULL);
	
		
	}
}

IMetaDataImport *CPipeServer::getMetaData(ICorDebugModule *module)
{
	IMetaDataImport *MetaData=moduleMetaData[module];

	if (MetaData==NULL)
	{
		module->GetMetaDataInterface(IID_IMetaDataImport, (IUnknown **)&MetaData);
		moduleMetaData[module]=MetaData;
	}


	return MetaData;
}

void CPipeServer::enumTypeDefs(UINT64 hModule)
{
	ICorDebugModule *module=(ICorDebugModule *)hModule;
	DWORD32 NumberOfTypeDefs=0;
	DWORD bw;
	IMetaDataImport *MetaData=NULL;
	HCORENUM henum=0;
	HRESULT r;
	ULONG typedefcount, totalcount;

	mdTypeDef typedefs[256];
	unsigned int i;

	TCHAR typedefname[255];
	ULONG typedefnamesize;
	DWORD typedefflags;
	mdToken extends;

	if (module==NULL)
	{
		WriteFile(pipe, &NumberOfTypeDefs, sizeof(NumberOfTypeDefs), &bw, NULL);
		return;
	}

	MetaData=getMetaData(module);

	if (MetaData==NULL)
	{
		WriteFile(pipe, &NumberOfTypeDefs, sizeof(NumberOfTypeDefs), &bw, NULL);
		return;
	}

	MetaData->EnumTypeDefs(&henum, typedefs, 0, &typedefcount);
	MetaData->CountEnum(henum, &totalcount);

	WriteFile(pipe, &totalcount, sizeof(totalcount), &bw, NULL); 

	do
	{	 
		r=MetaData->EnumTypeDefs(&henum, typedefs, 256, &typedefcount);	

		for (i=0; i<typedefcount; i++)
		{
			
			WriteFile(pipe, &typedefs[i], sizeof(typedefs[i]), &bw, NULL); //tokenid

			typedefnamesize=0;
			MetaData->GetTypeDefProps(typedefs[i], typedefname, 255, &typedefnamesize, &typedefflags, &extends);

			typedefnamesize=sizeof(TCHAR)*typedefnamesize;
			
			WriteFile(pipe, &typedefnamesize, sizeof(typedefnamesize), &bw, NULL); //namesize
			WriteFile(pipe, typedefname, typedefnamesize, &bw, NULL); //namesize

			WriteFile(pipe, &typedefflags, sizeof(typedefflags), &bw, NULL); //flags
			WriteFile(pipe, &extends, sizeof(extends), &bw, NULL); //extends


		}

		

	} while (r==S_OK);

	MetaData->CloseEnum(henum);
}

void CPipeServer::enumTypeDefMethods(UINT64 hModule, mdTypeDef TypeDef)
{
	ICorDebugModule *module=(ICorDebugModule *)hModule;
	IMetaDataImport *MetaData=getMetaData(module);
	HRESULT r;
	DWORD bw;
	if (MetaData)
	{
		unsigned int i;
		HCORENUM henum=0;
		ULONG count=0;
		mdMethodDef methods[32];
		TCHAR methodname[255];
		ULONG methodnamesize;
		
		r=MetaData->EnumMethods(&henum, TypeDef, methods, 0, &count);
		MetaData->CountEnum(henum, &count);

		WriteFile(pipe, &count, sizeof(count), &bw, NULL); //number of methods

		do
		{
			r=MetaData->EnumMethods(&henum, TypeDef, methods, 32, &count);
			for (i=0; i<count; i++)
			{
				mdTypeDef classdef;
				unsigned int j;
				DWORD dwAttr;
				PCCOR_SIGNATURE sig;
				ULONG sigsize;
				ULONG CodeRVA;
				DWORD dwImplFlags;
				ICorDebugFunction *df=NULL;

				CORDB_ADDRESS ILCode=0;
				CORDB_ADDRESS NativeCode=0;
				ULONG32 NativeCodeSize=1;
				CodeChunkInfo codechunks[8];

				ULONG32 SecondaryCodeBlocks=0;

				WriteFile(pipe, &methods[i], sizeof(methods[i]), &bw, NULL); //tokenid


				methodnamesize=0;
				MetaData->GetMethodProps(methods[i], &classdef, methodname, 255, &methodnamesize, &dwAttr, &sig, &sigsize,  &CodeRVA, &dwImplFlags);

				methodnamesize=sizeof(TCHAR)*methodnamesize;
				
				WriteFile(pipe, &methodnamesize, sizeof(methodnamesize), &bw, NULL); //namesize
				WriteFile(pipe, methodname, methodnamesize, &bw, NULL); //name

				WriteFile(pipe, &dwAttr, sizeof(dwAttr), &bw, NULL);
				WriteFile(pipe, &dwImplFlags, sizeof(dwImplFlags), &bw, NULL);

							
				if (module->GetFunctionFromToken(methods[i], &df)==S_OK)
				{
					ICorDebugCode *Code;
					if (df->GetILCode(&Code)==S_OK)
					{
						Code->GetAddress(&ILCode);
						Code->Release();
					}

					if (df->GetNativeCode(&Code)==S_OK)
					{
						ICorDebugCode2 *Code2;
						Code->GetAddress(&NativeCode);

						if (Code->QueryInterface(IID_ICorDebugCode2, (void **)&Code2)==S_OK)
						{
							ULONG32 count;
							ZeroMemory(codechunks, 8*sizeof(CodeChunkInfo));
							Code2->GetCodeChunks(8, &count, codechunks);  //count is useless
							Code2->Release();

							SecondaryCodeBlocks=0;
							for (j=0; j<count; j++)
								if (codechunks[j].startAddr)
									SecondaryCodeBlocks++;															
						}

						Code->Release();
					}

				

				}

				WriteFile(pipe, &ILCode, sizeof(ILCode), &bw, NULL);
				WriteFile(pipe, &NativeCode, sizeof(NativeCode), &bw, NULL);
	
				WriteFile(pipe, &SecondaryCodeBlocks, sizeof(SecondaryCodeBlocks), &bw, NULL);
				for (j=0; j<SecondaryCodeBlocks; j++)
					WriteFile(pipe, &codechunks[j], sizeof(CodeChunkInfo), &bw, NULL);

			}
		} while (r==S_OK);

		if (henum)
			MetaData->CloseEnum(henum);

	}
}

int CPipeServer::getAllFields(COR_TYPEID cortypeid, COR_TYPE_LAYOUT layout, std::vector<COR_FIELD> *fieldlist)
{
	COR_FIELD *fields = NULL;
	ULONG32 fieldcount;

	if ((layout.parentID.token1) || (layout.parentID.token2))
	{
		//it has a parent
		COR_TYPE_LAYOUT layoutparent;
		if (CorDebugProcess5->GetTypeLayout(layout.parentID, &layoutparent) == S_OK)
			getAllFields(layout.parentID, layoutparent, fieldlist);

	}

	if (layout.numFields)
	{

		fields = (COR_FIELD *)malloc(sizeof(COR_FIELD)*layout.numFields);

		if (CorDebugProcess5->GetTypeFields(cortypeid, layout.numFields, fields, &fieldcount) == S_OK)
		{
			unsigned int i;
			for (i = 0; i < fieldcount; i++)
				fieldlist->push_back(fields[i]);
		}

		if (fields)
			free(fields);
	}

	return (int)fieldlist->size();
}


void CPipeServer::getAddressData(UINT64 Address)
{
	//Enumerate the heap, check if the address is in the heap
	UINT64 StartAddress=0;
	ICorDebugHeapEnum *pObjects;
	DWORD bw;
	BOOL found=FALSE;

	
	if ((CorDebugProcess5) && (CorDebugProcess5->EnumerateHeap(&pObjects)==S_OK))
	{
		COR_HEAPOBJECT objects[16];
		ULONG count,i;
		HRESULT r;
		
		
		
		do
		{
			r=pObjects->Next(16, objects, &count);
			for (i=0; i<count; i++)
			{
				if ((Address>=objects[i].address) && (Address<objects[i].address+objects[i].size))
				{
					//found the address
					COR_TYPE_LAYOUT layout;
					DWORD type;
	
					found=TRUE;
					StartAddress=objects[i].address;
					WriteFile(pipe, &StartAddress, sizeof(StartAddress), &bw, NULL);  //real start
				
					if (CorDebugProcess5->GetTypeLayout(objects[i].type, &layout)==S_OK)
					{
						type=(DWORD)layout.type;	
						WriteFile(pipe, &type, sizeof(type), &bw, NULL);  //objecttype							
						
						

						if(layout.type == ELEMENT_TYPE_ARRAY || layout.type == ELEMENT_TYPE_SZARRAY)
						{
							//get the array shape
							COR_ARRAY_LAYOUT array_layout;
							if(S_OK == CorDebugProcess5->GetArrayLayout(objects[i].type, &array_layout))
							{
								WriteFile(pipe, &array_layout.componentType, sizeof(array_layout.componentType), &bw, NULL);  //layout.type of elements within
								WriteFile(pipe, &array_layout.countOffset, sizeof(array_layout.countOffset), &bw, NULL);  //offset of "num_elements"
								WriteFile(pipe, &array_layout.elementSize, sizeof(array_layout.elementSize), &bw, NULL);  //size of each element
								WriteFile(pipe, &array_layout.firstElementOffset, sizeof(array_layout.firstElementOffset), &bw, NULL);  //offset of first element
							}
							else //error getting array layout
							{						
								DWORD sigil_size = sizeof(array_layout.componentType) + sizeof(array_layout.countOffset) + sizeof(array_layout.elementSize) + sizeof(array_layout.firstElementOffset);
								void * error_sigil = malloc(sigil_size);
								memset(error_sigil, 0xff, sigil_size);
								WriteFile(pipe, error_sigil, sigil_size, &bw, NULL); //write error codes in place of the array layout we failed to read
								free(error_sigil);
							}
						}
						else // this is something other than an array //(1) //if (layout.type==ELEMENT_TYPE_CLASS) //there is more data besides just address and type
						{
							//get the field data
							ICorDebugType *type;
							ICorDebugClass *c;
							ICorDebugModule *m;
							IMetaDataImport *metadata=NULL;
							ULONG32 fieldcount;
							mdTypeDef classtoken=0;
							WCHAR classname[255];
							ULONG classnamelength;
							mdToken extends;
							DWORD flags;

							vector<COR_FIELD> fields;
							

							


							//get the metadata for the module that owns this class
							if (CorDebugProcess5->GetTypeForTypeID(objects[i].type, &type)==S_OK)
							{
								if (type->GetClass(&c)==S_OK)
								{
									c->GetToken(&classtoken);

									if (c->GetModule(&m)==S_OK)
									{
										metadata=getMetaData(m);										
										m->Release();
									}									
									c->Release();
								}								
								type->Release();
							}

							classnamelength=0;
							if ((metadata) && (classtoken))							
								metadata->GetTypeDefProps(classtoken, classname, 255, &classnamelength, &flags, &extends);

							//send the name (if there is one)
							classnamelength=sizeof(WCHAR)*classnamelength;

							WriteFile(pipe, &classnamelength, sizeof(classnamelength), &bw, NULL);
							if (classnamelength)
								WriteFile(pipe, classname, classnamelength, &bw, NULL);
							
							fields.clear();
							fieldcount = getAllFields(objects[i].type, layout, &fields);

							//send the fields
							//if (CorDebugProcess5->GetTypeFields(objects[i].type, layout.numFields, fields, &fieldcount)==S_OK)
							{
								unsigned int j;
								mdTypeDef classtype;
								WCHAR fieldname[255];
								ULONG fieldnamelength;
								DWORD attr;
								PCCOR_SIGNATURE sigBlob;
								ULONG sigbloblength;
								DWORD CPlusTypeFlag;
								DWORD valuelength;
								UVCP_CONSTANT value;

								WriteFile(pipe, &fieldcount, sizeof(fieldcount), &bw, NULL); 
								
								for (j=0; j<fieldcount; j++)
								{
									DWORD fieldtype=(DWORD)fields[j].fieldType;	

									WriteFile(pipe, &fields[j].offset, sizeof(fields[j].offset), &bw, NULL);
									WriteFile(pipe, &fieldtype, sizeof(fieldtype), &bw, NULL);


									//optional name:
									fieldnamelength=0;
									if (metadata)
										metadata->GetFieldProps(fields[j].token, &classtype, fieldname, 255, &fieldnamelength, &attr,  &sigBlob, &sigbloblength, &CPlusTypeFlag, &value, &valuelength);

									fieldnamelength=sizeof(WCHAR)*fieldnamelength;

									WriteFile(pipe, &fieldnamelength, sizeof(fieldnamelength), &bw, NULL);
									if (fieldnamelength)
										WriteFile(pipe, fieldname, fieldnamelength, &bw, NULL);

								}

							}
							
						}
						
					}
					else
					{
						//error
						type=0xffffffff;						
						WriteFile(pipe, &type, sizeof(type), &bw, NULL);												
					}										
					break;
				}
			}
		} while ((r==S_OK) && (!found));

		

		pObjects->Release();
	}
	

	if (!found)
		WriteFile(pipe, &StartAddress, sizeof(StartAddress), &bw, NULL); //startaddress 0 means there is no data or no CorDebugProcess5
}

volatile int count;

void CPipeServer::enumAllObjects(void)
{
	//Enumerate all objects in the heap and return their basic info
	ICorDebugHeapEnum *pObjects;
	DWORD bw;
	BOOL found = FALSE;
	UINT64 address;
	DWORD size;
	WCHAR classname[255];
	ULONG classnamelength;


	if ((CorDebugProcess5) && (CorDebugProcess5->EnumerateHeap(&pObjects) == S_OK))
	{
		COR_HEAPOBJECT objects[16];
		ULONG count, i;
		HRESULT r;

		do
		{
			r = pObjects->Next(16, objects, &count);
			for (i = 0; i<count; i++)
			{
				ICorDebugType *type;


				
				if (CorDebugProcess5->GetTypeForTypeID(objects[i].type, &type) == S_OK)
				{
					ICorDebugClass *c;
					mdTypeDef classtoken = 0;
					IMetaDataImport *metadata = NULL;
					
					

					if (type->GetClass(&c) == S_OK)
					{		
						
						if (c->GetToken(&classtoken) == S_OK)
						{
							ICorDebugModule *m = NULL;
							if (c->GetModule(&m) == S_OK)
							{
								metadata=getMetaData(m);
								m->Release();
							}
						}
						
						c->Release();
					}

					if ((metadata) && (classtoken))
					{
						mdToken extends;
						DWORD flags;

						if (metadata->GetTypeDefProps(classtoken, classname, 255, &classnamelength, &flags, &extends) == S_OK)
						{
							//everything ok, send it to CE
							address = objects[i].address;
							size = (DWORD)objects[i].size;

							WriteFile(pipe, &address, sizeof(address), &bw, NULL);
							WriteFile(pipe, &size, sizeof(size), &bw, NULL);
							WriteFile(pipe, &objects[i].type, sizeof(objects[i].type), &bw, NULL);

							classnamelength = sizeof(WCHAR)*classnamelength;

							WriteFile(pipe, &classnamelength, sizeof(classnamelength), &bw, NULL);
							if (classnamelength)
								WriteFile(pipe, classname, classnamelength, &bw, NULL);							
						}
					}

					type->Release();
				}
			}
		} while (r == S_OK);

		pObjects->Release();
	}

	//write the end of list entry
	{
		COR_TYPEID type;
		DWORD size = 0;
		address = 0;
		size = 0;
		classnamelength = 0;
		type.token1 = 0;
		type.token2 = 0;

		WriteFile(pipe, &address, sizeof(address), &bw, NULL);
		WriteFile(pipe, &size, sizeof(size), &bw, NULL);
		WriteFile(pipe, &type, sizeof(type), &bw, NULL);
		WriteFile(pipe, &classnamelength, sizeof(classnamelength), &bw, NULL);
	}

}


int CPipeServer::Start(void)
{
	if (pipe!=INVALID_HANDLE_VALUE)
	{
		unsigned char command;
		ULONG bytesread, byteswritten;

		while (ReadFile(pipe, &command, sizeof(command), &bytesread, NULL))
		{
			switch(command)
			{
				case CMD_TARGETPROCESS:			
					if (ReadFile(pipe, &processid, sizeof(processid), &bytesread, NULL))
					{	
						BOOL result;
						result=OpenOrAttachToProcess();	

						WriteFile(pipe, &result, sizeof(result), &byteswritten, NULL);	
						if (result)
						{
							result=(CorDebugProcess5!=NULL); //tell that it supports structure type lookups or not
							WriteFile(pipe, &result, sizeof(result), &byteswritten, NULL);	
						}

					}
					else
						return 1;

					break;
			

				case CMD_CLOSEPROCESSANDQUIT:				
					return 0;

				case CMD_RELEASEOBJECTHANDLE:
				{
					UINT64 hObject;
					if (ReadFile(pipe, &hObject, sizeof(hObject), &bytesread, NULL))
						releaseObjectHandle(hObject);
					else
						return 1;

					break;
				}

				case CMD_ENUMDOMAINS:
					enumDomains();
					break;

				case CMD_ENUMMODULELIST:
					{
						UINT64 hDomain;
						if (ReadFile(pipe, &hDomain, sizeof(hDomain), &bytesread, NULL))
							enumModules(hDomain);
						else
							return 1;

						break;
					}

				case CMD_ENUMTYPEDEFS:
					{
						UINT64 hModule;
						if (ReadFile(pipe, &hModule, sizeof(hModule), &bytesread, NULL))
							enumTypeDefs(hModule);
						else
							return 1;

						break;
					}

				case CMD_GETTYPEDEFMETHODS:
					{
						UINT64 hModule;
						mdTypeDef TypeDef;
						
						if (ReadFile(pipe, &hModule, sizeof(hModule), &bytesread, NULL))
						{
							if (ReadFile(pipe, &TypeDef, sizeof(TypeDef), &bytesread, NULL))
								enumTypeDefMethods(hModule, TypeDef);
							else
								return 1;
						}
						else
							return 1;
						
						break;
					}

				case CMD_GETADDRESSDATA:
					{
						UINT64 Address;
						if (ReadFile(pipe, &Address, sizeof(Address), &bytesread, NULL))
						{
							getAddressData(Address);
						}
						else
							return 1;

						break;
					}

				case CMD_GETALLOBJECTS:
					{
						enumAllObjects();
						break;
					}
			}


		}
	}	
	return 1;
}