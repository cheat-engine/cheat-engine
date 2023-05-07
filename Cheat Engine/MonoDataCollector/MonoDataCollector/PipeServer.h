#pragma once

#include <Pipe.h>
#ifndef _WINDOWS
#include "Metadata.h"
#endif


                                //yyyymmdd
#define MONO_DATACOLLECTORVERSION 20230409 

#define MONO_TYPE_NAME_FORMAT_IL  0
#define MONO_TYPE_NAME_FORMAT_REFLECTION  1
#define MONO_TYPE_NAME_FORMAT_FULL_NAME  2
#define MONO_TYPE_NAME_FORMAT_ASSEMBLY_QUALIFIED  3

#define MONOCMD_INITMONO 0
#define MONOCMD_OBJECT_GETCLASS 1
#define MONOCMD_ENUMDOMAINS 2
#define MONOCMD_SETCURRENTDOMAIN 3
#define MONOCMD_ENUMASSEMBLIES 4
#define MONOCMD_GETIMAGEFROMASSEMBLY 5
#define MONOCMD_GETIMAGENAME 6
#define MONOCMD_ENUMCLASSESINIMAGE 7
#define MONOCMD_ENUMFIELDSINCLASS 8
#define MONOCMD_ENUMMETHODSINCLASS 9
#define MONOCMD_COMPILEMETHOD 10

#define MONOCMD_GETMETHODHEADER 11
#define MONOCMD_GETMETHODHEADER_CODE 12
#define MONOCMD_LOOKUPRVA 13
#define MONOCMD_GETJITINFO 14
#define MONOCMD_FINDCLASS 15
#define MONOCMD_FINDMETHOD 16
#define MONOCMD_GETMETHODNAME 17
#define MONOCMD_GETMETHODCLASS 18
#define MONOCMD_GETCLASSNAME 19
#define MONOCMD_GETCLASSNAMESPACE 20
#define MONOCMD_FREEMETHOD 21
#define MONOCMD_TERMINATE 22
#define MONOCMD_DISASSEMBLE 23
#define MONOCMD_GETMETHODSIGNATURE 24
#define MONOCMD_GETPARENTCLASS 25
#define MONOCMD_GETSTATICFIELDADDRESSFROMCLASS 26
#define MONOCMD_GETTYPECLASS 27
#define MONOCMD_GETARRAYELEMENTCLASS 28
#define MONOCMD_FINDMETHODBYDESC 29
#define MONOCMD_INVOKEMETHOD 30
#define MONOCMD_LOADASSEMBLY 31
#define MONOCMD_GETFULLTYPENAME 32
#define MONOCMD_OBJECT_NEW 33
#define MONOCMD_OBJECT_INIT 34
#define MONOCMD_GETVTABLEFROMCLASS 35
#define MONOCMD_GETMETHODPARAMETERS 36
#define MONOCMD_ISCLASSGENERIC 37
#define MONOCMD_ISIL2CPP 38
#define MONOCMD_FILLOPTIONALFUNCTIONLIST 39
#define MONOCMD_GETSTATICFIELDVALUE 40
#define MONOCMD_SETSTATICFIELDVALUE 41
#define MONOCMD_GETCLASSIMAGE 42
#define MONOCMD_FREE 43
#define MONOCMD_GETIMAGEFILENAME 44
#define MONOCMD_GETCLASSNESTINGTYPE 45
#define MONOCMD_LIMITEDCONNECTION 46
#define MONOCMD_GETMONODATACOLLECTORVERSION 47
#define MONOCMD_NEWSTRING 48
#define MONOCMD_ENUMIMAGES 49
#define MONOCMD_ENUMCLASSESINIMAGEEX 50
#define MONOCMD_ISCLASSENUM 51
#define MONOCMD_ISCLASSVALUETYPE 52
#define MONOCMD_ISCLASSISSUBCLASSOF 53
#define MONOCMD_ARRAYELEMENTSIZE 54
#define MONOCMD_GETCLASSTYPE 55
#define MONOCMD_GETCLASSOFTYPE 56
#define MONOCMD_GETTYPEOFMONOTYPE 57
#define MONOCMD_GETREFLECTIONTYPEOFCLASSTYPE 58					 


typedef struct {} MonoType;
typedef struct {} MonoObject;
typedef struct {} MonoMethodSignature;
typedef void * gpointer;

typedef void (__cdecl *MonoDomainFunc) (void *domain, void *user_data);
typedef void (__cdecl *GFunc)          (void *data, void *user_data);

typedef void (__cdecl *G_FREE)(void *ptr);

typedef void* (__cdecl *MONO_GET_ROOT_DOMAIN)(void);
typedef void* (__cdecl *MONO_THREAD_ATTACH)(void *domain);
typedef void (__cdecl *MONO_THREAD_DETACH)(void *monothread);
typedef void (__cdecl *MONO_THREAD_CLEANUP)(void);
typedef void* (__cdecl *MONO_OBJECT_GET_CLASS)(void *object);

typedef void (__cdecl *MONO_DOMAIN_FOREACH)(MonoDomainFunc func, void *user_data);

typedef int (__cdecl *MONO_DOMAIN_SET)(void *domain, BOOL force);
typedef void* (__cdecl *MONO_DOMAIN_GET)();
typedef int (__cdecl *MONO_ASSEMBLY_FOREACH)(GFunc func, void *user_data);
typedef void* (__cdecl *MONO_ASSEMBLY_GET_IMAGE)(void *assembly);
typedef void* (__cdecl *MONO_ASSEMBLY_OPEN)(void *fname, int *status);
typedef void* (__cdecl *MONO_IMAGE_GET_ASSEMBLY)(void *image);
typedef char* (__cdecl *MONO_IMAGE_GET_NAME)(void *image);
typedef void* (__cdecl *MONO_IMAGE_OPEN)(const char *fname, int *status);
typedef char* (__cdecl *MONO_IMAGE_GET_FILENAME)(void *image);


typedef void* (__cdecl *MONO_IMAGE_GET_TABLE_INFO)(void *image, int table_id);
typedef int (__cdecl *MONO_TABLE_INFO_GET_ROWS)(void *tableinfo);
typedef int (__cdecl *MONO_METADATA_DECODE_ROW_COL)(void *tableinfo, int idx, unsigned int col);
typedef char* (__cdecl *MONO_METADATA_STRING_HEAP)(void *image, UINT32 index);

typedef void* (__cdecl *MONO_CLASS_FROM_NAME_CASE)(void *image, char *name_space, char *name);
typedef void* (__cdecl *MONO_CLASS_FROM_NAME)(void *image, char *name_space, char *name);
typedef char* (__cdecl *MONO_CLASS_GET_NAME)(void *klass);
typedef char* (__cdecl *MONO_CLASS_GET_NAMESPACE)(void *klass);
typedef void* (__cdecl *MONO_CLASS_GET)(void *image, UINT32 tokenindex);
typedef void* (__cdecl *MONO_CLASS_FROM_TYPEREF)(void *image, UINT32 type_token);
typedef char* (__cdecl *MONO_CLASS_NAME_FROM_TOKEN)(void *image, UINT32 token);


typedef void* (__cdecl *MONO_CLASS_GET_METHODS)(void *klass, void *iter);
typedef void* (__cdecl *MONO_CLASS_GET_METHOD_FROM_NAME)(void *klass, char *methodname, int paramcount);
typedef void* (__cdecl *MONO_CLASS_GET_FIELDS)(void *klass, void *iter);
typedef void* (__cdecl *MONO_CLASS_GET_PARENT)(void *klass);
typedef void* (__cdecl *MONO_CLASS_GET_IMAGE)(void *klass);
typedef void* (__cdecl *MONO_CLASS_VTABLE)(void *domain, void *klass);
typedef int (__cdecl *MONO_CLASS_INSTANCE_SIZE)(void *klass);
typedef void* (__cdecl *MONO_CLASS_FROM_MONO_TYPE)(void *type);
typedef void* (__cdecl *MONO_CLASS_GET_ELEMENT_CLASS)(void *klass);
typedef int (__cdecl *MONO_CLASS_IS_GENERIC)(void *klass);
typedef bool (__cdecl *MONO_CLASS_IS_ENUM)(void *klass);
typedef bool (__cdecl *MONO_CLASS_IS_VALUETYPE)(void *klass);
typedef bool (__cdecl *MONO_CLASS_IS_SUBCLASS_OF)(void *klass, void* parentKlass, bool check_interface);

typedef int (__cdecl *MONO_CLASS_NUM_FIELDS)(void *klass);
typedef int (__cdecl *MONO_CLASS_NUM_METHODS)(void *klass);

typedef char* (__cdecl *MONO_FIELD_GET_NAME)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_TYPE)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_PARENT)(void *field);
typedef int (__cdecl *MONO_FIELD_GET_OFFSET)(void *field);

typedef char* (__cdecl *MONO_TYPE_GET_NAME)(void *type);
typedef void* (__cdecl* MONO_TYPE_GET_CLASS)(void* type);
typedef int (__cdecl *MONO_TYPE_GET_TYPE)(void *type);
typedef void* (__cdecl *MONO_TYPE_GET_OBJECT)(void *domain, void *type);
typedef void* (__cdecl *IL2CPP_TYPE_GET_OBJECT)(void *type);


typedef char* (__cdecl *MONO_TYPE_GET_NAME_FULL)(void *type, int format);
typedef bool(__cdecl* MONO_TYPE_IS_STRUCT)(void* type);

typedef int (__cdecl *MONO_FIELD_GET_FLAGS)(void *type);
typedef void* (__cdecl * MONO_FIELD_GET_VALUE_OBJECT)(void *domain, void* field, void* object);

typedef int (__cdecl *MONO_FIELD_GET_FLAGS)(void *type);
typedef void* (__cdecl * MONO_FIELD_GET_VALUE_OBJECT)(void *domain, void* field, void* object);


typedef char* (__cdecl *MONO_METHOD_GET_NAME)(void *method);
typedef void* (__cdecl *MONO_COMPILE_METHOD)(void *method);
typedef void (__cdecl *MONO_FREE_METHOD)(void *method);

typedef void* (__cdecl *MONO_JIT_INFO_TABLE_FIND)(void *domain, void *addr);

typedef void* (__cdecl *MONO_JIT_INFO_GET_METHOD)(void *jitinfo);
typedef void* (__cdecl *MONO_JIT_INFO_GET_CODE_START)(void *jitinfo);
typedef int (__cdecl *MONO_JIT_INFO_GET_CODE_SIZE)(void *jitinfo);

typedef int (__cdecl *MONO_JIT_EXEC)(void *domain, void *assembly, int argc, char *argv[]);
	

typedef uint32_t (__cdecl *MONO_METHOD_GET_FLAGS)(void *method, uint32_t *iflags);
typedef void* (__cdecl *MONO_METHOD_GET_HEADER)(void *method);
typedef void* (__cdecl *MONO_METHOD_GET_CLASS)(void *method);
typedef void* (__cdecl *MONO_METHOD_SIG)(void *method);
typedef void* (__cdecl *MONO_METHOD_GET_PARAM_NAMES)(void *method, const char **names);

typedef void* (__cdecl *MONO_METHOD_HEADER_GET_CODE)(void *methodheader, UINT32 *code_size, UINT32 *max_stack);
typedef char* (__cdecl *MONO_DISASM_CODE)(void *dishelper, void *method, void *ip, void *end);

typedef char* (__cdecl *MONO_SIGNATURE_GET_DESC)(void *signature, int include_namespace);
typedef MonoType* (__cdecl *MONO_SIGNATURE_GET_PARAMS)(MonoMethodSignature *sig, gpointer *iter);
typedef int (__cdecl *MONO_SIGNATURE_GET_PARAM_COUNT)(void *signature);
typedef MonoType* (__cdecl *MONO_SIGNATURE_GET_RETURN_TYPE)(void *signature);


typedef void* (__cdecl *MONO_IMAGE_RVA_MAP)(void *image, UINT32 addr);
typedef void* (__cdecl *MONO_VTABLE_GET_STATIC_FIELD_DATA)(void *vtable);


typedef void* (__cdecl *MONO_METHOD_DESC_NEW)(const char *name, int include_namespace);
typedef void* (__cdecl *MONO_METHOD_DESC_FROM_METHOD)(void *method);
typedef void  (__cdecl *MONO_METHOD_DESC_FREE)(void *desc);

typedef void* (__cdecl *MONO_ASSEMBLY_NAME_NEW)(const char *name);
typedef void* (__cdecl *MONO_ASSEMBLY_LOADED)(void *aname);
typedef void* (__cdecl *MONO_IMAGE_LOADED)(void *aname);

typedef void* (__cdecl *MONO_STRING_NEW)(void *domain, const char *text);
typedef char* (__cdecl *MONO_STRING_TO_UTF8)(void*);
typedef void* (__cdecl *MONO_ARRAY_NEW)(void *domain, void *eclass, uintptr_t n);
typedef int (__cdecl *MONO_ARRAY_ELEMENT_SIZE)(void * klass);
typedef void* (__cdecl *MONO_OBJECT_TO_STRING)(void *object, void **exc);
typedef void* (__cdecl *MONO_OBJECT_NEW)(void *domain, void *klass);


typedef void  (__cdecl *MONO_FREE)(void*);

typedef void* (__cdecl *MONO_METHOD_DESC_SEARCH_IN_IMAGE)(void *desc, void *image);
typedef void* (__cdecl *MONO_RUNTIME_INVOKE)(void *method, void *obj, void **params, MonoObject **exc);
typedef void* (__cdecl *MONO_RUNTIME_INVOKE_ARRAY)(void *method, void *obj, void *params, void **exc);
typedef void* (__cdecl *MONO_RUNTIME_OBJECT_INIT)(void *object);

typedef void* (__cdecl *MONO_FIELD_STATIC_GET_VALUE)(void *vtable, void* field, void* output);
typedef void* (__cdecl *MONO_FIELD_STATIC_SET_VALUE)(void *vtable, void* field, void* input);

typedef void* (__cdecl *IL2CPP_FIELD_STATIC_GET_VALUE)(void* field, void* output);
typedef void* (__cdecl *IL2CPP_FIELD_STATIC_SET_VALUE)(void* field, void* input);

typedef void* (__cdecl *MONO_VALUE_BOX)(void *domain, void *klass, void* val);
typedef void* (__cdecl *MONO_OBJECT_UNBOX)(void *obj);
typedef void* (__cdecl *MONO_OBJECT_ISINST)(void *obj, void* kls);
typedef void* (__cdecl *MONO_GET_ENUM_CLASS)(void);
typedef void* (__cdecl *MONO_CLASS_GET_TYPE)(void *klass);
typedef void* (__cdecl *MONO_CLASS_GET_NESTING_TYPE)(void *klass);

typedef int (__cdecl *MONO_RUNTIME_IS_SHUTTING_DOWN)(void);



//il2cpp:
typedef UINT_PTR* (__cdecl *IL2CPP_DOMAIN_GET_ASSEMBLIES)(void * domain, SIZE_T *size);

typedef int(__cdecl *IL2CPP_IMAGE_GET_CLASS_COUNT)(void* image);
typedef void*(__cdecl *IL2CPP_IMAGE_GET_CLASS)(void *image, int index);

typedef char*(__cdecl *IL2CPP_TYPE_GET_NAME)(void* ptype);
typedef char*(__cdecl *IL2CPP_TYPE_GET_ASSEMBLY_QUALIFIED_NAME)(void* ptype);

typedef int(__cdecl *IL2CPP_METHOD_GET_PARAM_COUNT)(void* method);
typedef char*(__cdecl *IL2CPP_METHOD_GET_PARAM_NAME)(void *method, int index);
typedef void*(__cdecl *IL2CPP_METHOD_GET_PARAM)(void *method, int index);
typedef void*(__cdecl *IL2CPP_METHOD_GET_RETURN_TYPE)(void *method);
typedef void*(__cdecl *IL2CPP_CLASS_FROM_TYPE)(void *type);
typedef wchar_t*(__cdecl *IL2CPP_STRING_CHARS)(void *stringobject);


class CPipeServer : Pipe
{
private:
    #ifdef _WINDOWS
	wchar_t datapipename[256];
	wchar_t eventpipename[256];
    #else
    char* datapipename[256];
    char* eventpipename[256];
    #endif

	void *mono_selfthread;

	G_FREE g_free;
	MONO_GET_ROOT_DOMAIN mono_get_root_domain;
	MONO_THREAD_ATTACH mono_thread_attach;
	MONO_THREAD_DETACH mono_thread_detach;
    MONO_THREAD_CLEANUP mono_thread_cleanup;
	MONO_OBJECT_GET_CLASS mono_object_get_class;
	MONO_CLASS_GET_NAME mono_class_get_name;
	MONO_CLASS_GET_NAMESPACE mono_class_get_namespace;
	MONO_CLASS_GET_PARENT mono_class_get_parent;
	MONO_CLASS_GET_IMAGE mono_class_get_image;	
	MONO_CLASS_VTABLE mono_class_vtable;
	MONO_CLASS_INSTANCE_SIZE mono_class_instance_size;
	MONO_CLASS_FROM_MONO_TYPE mono_class_from_mono_type;
	MONO_CLASS_IS_GENERIC mono_class_is_generic;
	MONO_CLASS_IS_ENUM mono_class_is_enum;
	MONO_CLASS_IS_VALUETYPE mono_class_is_valuetype;
	MONO_CLASS_IS_SUBCLASS_OF mono_class_is_subclass_of;
	MONO_DOMAIN_FOREACH mono_domain_foreach;
	MONO_DOMAIN_SET mono_domain_set;
	MONO_DOMAIN_GET mono_domain_get;
	MONO_ASSEMBLY_FOREACH mono_assembly_foreach;	
	MONO_ASSEMBLY_GET_IMAGE mono_assembly_get_image;
	MONO_IMAGE_GET_ASSEMBLY mono_image_get_assembly;
	MONO_ASSEMBLY_OPEN mono_assembly_open;
		
	MONO_IMAGE_GET_NAME mono_image_get_name;
	MONO_IMAGE_GET_TABLE_INFO mono_image_get_table_info;
	MONO_IMAGE_GET_FILENAME mono_image_get_filename;
    MONO_IMAGE_RVA_MAP mono_image_rva_map;
	MONO_IMAGE_OPEN mono_image_open;
	MONO_IMAGE_LOADED mono_image_loaded;

	MONO_TABLE_INFO_GET_ROWS mono_table_info_get_rows;
	MONO_METADATA_DECODE_ROW_COL mono_metadata_decode_row_col;
	MONO_METADATA_STRING_HEAP mono_metadata_string_heap;
	MONO_CLASS_GET mono_class_get;
	MONO_CLASS_FROM_TYPEREF mono_class_from_typeref;
	MONO_CLASS_NAME_FROM_TOKEN mono_class_name_from_token;

	MONO_CLASS_FROM_NAME_CASE mono_class_from_name_case;
	MONO_CLASS_FROM_NAME mono_class_from_name;

	MONO_CLASS_NUM_FIELDS mono_class_num_fields;
	MONO_CLASS_GET_FIELDS mono_class_get_fields;

	MONO_CLASS_NUM_METHODS mono_class_num_methods;
	MONO_CLASS_GET_METHODS mono_class_get_methods;

	MONO_CLASS_GET_METHOD_FROM_NAME mono_class_get_method_from_name;
	MONO_CLASS_GET_ELEMENT_CLASS mono_class_get_element_class;


	MONO_FIELD_GET_NAME mono_field_get_name;
	MONO_FIELD_GET_TYPE mono_field_get_type;
	MONO_FIELD_GET_PARENT mono_field_get_parent;
	MONO_FIELD_GET_OFFSET mono_field_get_offset;

	MONO_TYPE_GET_NAME mono_type_get_name;
	MONO_TYPE_GET_TYPE mono_type_get_type;
	MONO_TYPE_GET_OBJECT mono_type_get_object; //return a ReflectionType* object
	IL2CPP_TYPE_GET_OBJECT il2cpp_type_get_object;
	MONO_TYPE_IS_STRUCT mono_type_is_struct;
	MONO_TYPE_GET_CLASS mono_type_get_class;													  
	MONO_TYPE_GET_NAME_FULL mono_type_get_name_full;
	MONO_FIELD_GET_FLAGS mono_field_get_flags;
	MONO_FIELD_GET_VALUE_OBJECT mono_field_get_value_object;

	MONO_METHOD_GET_FLAGS mono_method_get_flags;
	MONO_METHOD_GET_NAME mono_method_get_name;
	MONO_METHOD_GET_HEADER mono_method_get_header;
	MONO_METHOD_GET_CLASS mono_method_get_class;
	MONO_METHOD_SIG mono_method_signature;
	MONO_METHOD_GET_PARAM_NAMES mono_method_get_param_names;

	MONO_SIGNATURE_GET_DESC mono_signature_get_desc;
	MONO_SIGNATURE_GET_PARAMS mono_signature_get_params;
	MONO_SIGNATURE_GET_PARAM_COUNT mono_signature_get_param_count;
	MONO_SIGNATURE_GET_RETURN_TYPE mono_signature_get_return_type;


	MONO_COMPILE_METHOD mono_compile_method;
	MONO_FREE_METHOD mono_free_method;

	MONO_JIT_INFO_TABLE_FIND mono_jit_info_table_find;
	MONO_JIT_INFO_GET_METHOD mono_jit_info_get_method;
	MONO_JIT_INFO_GET_CODE_START mono_jit_info_get_code_start;
	MONO_JIT_INFO_GET_CODE_SIZE mono_jit_info_get_code_size;
	MONO_JIT_EXEC mono_jit_exec;
	
	MONO_METHOD_HEADER_GET_CODE mono_method_header_get_code;
	MONO_DISASM_CODE mono_disasm_code;

	MONO_VTABLE_GET_STATIC_FIELD_DATA mono_vtable_get_static_field_data;

	MONO_METHOD_DESC_NEW mono_method_desc_new;
	MONO_METHOD_DESC_FROM_METHOD mono_method_desc_from_method;
	MONO_METHOD_DESC_FREE mono_method_desc_free;
	MONO_ASSEMBLY_NAME_NEW mono_assembly_name_new;
	MONO_ASSEMBLY_LOADED mono_assembly_loaded;

	MONO_STRING_NEW mono_string_new;
	MONO_STRING_TO_UTF8 mono_string_to_utf8;
	MONO_ARRAY_NEW mono_array_new;
	MONO_ARRAY_ELEMENT_SIZE mono_array_element_size;
	MONO_OBJECT_TO_STRING mono_object_to_string;
	MONO_OBJECT_NEW mono_object_new;
	MONO_FREE mono_free;
	MONO_VALUE_BOX mono_value_box;
	MONO_OBJECT_UNBOX mono_object_unbox;
	MONO_OBJECT_ISINST mono_object_isinst;
	MONO_GET_ENUM_CLASS mono_get_enum_class;

	MONO_CLASS_GET_TYPE mono_class_get_type;
	MONO_CLASS_GET_NESTING_TYPE mono_class_get_nesting_type;


	MONO_METHOD_DESC_SEARCH_IN_IMAGE mono_method_desc_search_in_image;
	MONO_RUNTIME_INVOKE mono_runtime_invoke;
	MONO_RUNTIME_OBJECT_INIT mono_runtime_object_init;

	MONO_FIELD_STATIC_GET_VALUE mono_field_static_get_value;
	MONO_FIELD_STATIC_SET_VALUE mono_field_static_set_value;

	MONO_RUNTIME_IS_SHUTTING_DOWN mono_runtime_is_shutting_down;


	//il2cpp
	IL2CPP_FIELD_STATIC_GET_VALUE il2cpp_field_static_get_value;
	IL2CPP_FIELD_STATIC_SET_VALUE il2cpp_field_static_set_value;

	IL2CPP_DOMAIN_GET_ASSEMBLIES il2cpp_domain_get_assemblies;

	IL2CPP_IMAGE_GET_CLASS_COUNT il2cpp_image_get_class_count;
	IL2CPP_IMAGE_GET_CLASS il2cpp_image_get_class;

	IL2CPP_TYPE_GET_NAME il2cpp_type_get_name;
	IL2CPP_TYPE_GET_ASSEMBLY_QUALIFIED_NAME il2cpp_type_get_assembly_qualified_name;

	IL2CPP_METHOD_GET_PARAM_COUNT il2cpp_method_get_param_count;
	IL2CPP_METHOD_GET_PARAM_NAME il2cpp_method_get_param_name;
	IL2CPP_METHOD_GET_PARAM il2cpp_method_get_param;
	IL2CPP_METHOD_GET_RETURN_TYPE il2cpp_method_get_return_type;
	IL2CPP_CLASS_FROM_TYPE il2cpp_class_from_type;
	IL2CPP_STRING_CHARS il2cpp_string_chars;


	BOOL limitedConnection;
	BOOL attached;
	BOOL il2cpp;

	BOOL UWPMode;
	void* domain;

	void CreatePipeandWaitForconnect(void);

	void InitMono();
	void ConnectThreadToMonoRuntime();

	void Object_GetClass();
	void EnumDomains();
	void SetCurrentDomain();
	void EnumAssemblies();
	void GetImageFromAssembly();
	void GetImageName();
	void GetImageFileName();
	void EnumImages();
	void EnumClassesInImage();
	void EnumClassesInImageEx();
	void EnumFieldsInClass();
	void EnumMethodsInClass();
	void CompileMethod();
	void GetMethodHeader();
	void GetILCode();
	void RvaMap();
	void GetJitInfo();
	void FindClass();
	void FindMethod();
	void GetMethodName();
	void GetMethodClass();
	void GetKlassName();
	void GetClassNamespace();
	void FreeMethod();
	void FreeObject();
	void DisassembleMethod();
	void GetMethodSignature();
	void GetMethodParameters();
	void GetParentClass();
	void GetClassNestingType();
	void GetClassImage();
	void GetClassType();
	void GetClassOfType();
	void GetTypeOfMonoType();
	void GetReflectionTypeOfClassType();
	void GetVTableFromClass();
	void GetStaticFieldAddressFromClass();
	void GetTypeClass();
	void GetArrayElementClass();
	void FindMethodByDesc();
	void InvokeMethod();
	void LoadAssemblyFromFile();
	void GetFullTypeName();
	std::string GetFullTypeNameStr(void* klass, char isKlass, int nameformat);
	void Object_New();
	void Object_Init();
	void IsGenericClass();
	void IsEnumClass();
	void IsValueTypeClass();
	void IsSubClassOf();
	void GetArrayElementSize();
	void IsIL2CPP();
	void FillOptionalFunctionList(); //mainly for unixbased systems
	void GetStaticFieldValue();
	void SetStaticFieldValue();
	void GetMonoDataCollectorVersion();
	void NewString();

public:
	CPipeServer(void);
	~CPipeServer(void);

	void Start(void);

	char* ReadString(void);
	void WriteString(const char*);
	void WriteString1(const char*);
	void FreeString(char*);

	//void *ReadObjectArray(void* domain);
	//void FreeObjectArray(void *arr);
	//int GetObjectArraySize(void *arr);
	//void **GetObjectArrayArgs(void *arr);

	//int GetObjectSize(int);
	//void* ReadObject(void* domain, MonoTypeEnum type, void* addr);
	//void WriteObject(void*);
	//void WriteEmptyObject();
};
