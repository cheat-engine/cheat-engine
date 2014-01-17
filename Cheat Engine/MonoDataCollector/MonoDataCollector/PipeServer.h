#pragma once

#include <Pipe.h>

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

typedef void (__cdecl *MonoDomainFunc) (void *domain, void *user_data);
typedef void (__cdecl *GFunc)          (void *data, void *user_data);

typedef void (__cdecl *G_FREE)(void *ptr);

typedef void* (__cdecl *MONO_GET_ROOT_DOMAIN)(void);
typedef void* (__cdecl *MONO_THREAD_ATTACH)(void *domain);
typedef void (__cdecl *MONO_THREAD_DETACH)(void *monothread);
typedef void* (__cdecl *MONO_OBJECT_GET_CLASS)(void *object);

typedef void (__cdecl *MONO_DOMAIN_FOREACH)(MonoDomainFunc func, void *user_data);

typedef int (__cdecl *MONO_DOMAIN_SET)(void *domain, BOOL force);
typedef int (__cdecl *MONO_ASSEMBLY_FOREACH)(GFunc func, void *user_data);
typedef void* (__cdecl *MONO_ASSEMBLY_GET_IMAGE)(void *assembly);
typedef char* (__cdecl *MONO_IMAGE_GET_NAME)(void *image);

typedef void* (__cdecl *MONO_IMAGE_GET_TABLE_INFO)(void *image, int table_id);
typedef int (__cdecl *MONO_TABLE_INFO_GET_ROWS)(void *tableinfo);
typedef int (__cdecl *MONO_METADATA_DECODE_ROW_COL)(void *tableinfo, int idx, unsigned int col);
typedef char* (__cdecl *MONO_METADATA_STRING_HEAP)(void *image, UINT32 index);

typedef void* (__cdecl *MONO_CLASS_FROM_NAME_CASE)(void *image, char *name_space, char *name);
typedef char* (__cdecl *MONO_CLASS_GET_NAME)(void *klass);
typedef char* (__cdecl *MONO_CLASS_GET_NAMESPACE)(void *klass);
typedef void* (__cdecl *MONO_CLASS_GET)(void *image, UINT32 tokenindex);
typedef void* (__cdecl *MONO_CLASS_GET_METHODS)(void *klass, void *iter);
typedef void* (__cdecl *MONO_CLASS_GET_METHOD_FROM_NAME)(void *klass, char *methodname, int paramcount);
typedef void* (__cdecl *MONO_CLASS_GET_FIELDS)(void *klass, void *iter);
typedef int (__cdecl *MONO_CLASS_NUM_FIELDS)(void *klass);
typedef int (__cdecl *MONO_CLASS_NUM_METHODS)(void *klass);

typedef char* (__cdecl *MONO_FIELD_GET_NAME)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_TYPE)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_PARENT)(void *field);
typedef int (__cdecl *MONO_FIELD_GET_OFFSET)(void *field);

typedef char* (__cdecl *MONO_TYPE_GET_NAME)(void *type);
typedef int (__cdecl *MONO_TYPE_GET_TYPE)(void *type);


typedef char* (__cdecl *MONO_METHOD_GET_NAME)(void *method);
typedef void* (__cdecl *MONO_COMPILE_METHOD)(void *method);
typedef void (__cdecl *MONO_FREE_METHOD)(void *method);

typedef void* (__cdecl *MONO_JIT_INFO_TABLE_FIND)(void *domain, void *addr);

typedef void* (__cdecl *MONO_JIT_INFO_GET_METHOD)(void *jitinfo);
typedef void* (__cdecl *MONO_JIT_INFO_GET_CODE_START)(void *jitinfo);
typedef int (__cdecl *MONO_JIT_INFO_GET_CODE_SIZE)(void *jitinfo);
	


typedef void* (__cdecl *MONO_METHOD_GET_HEADER)(void *method);
typedef void* (__cdecl *MONO_METHOD_GET_CLASS)(void *method);

typedef void* (__cdecl *MONO_METHOD_HEADER_GET_CODE)(void *methodheader, UINT32 *code_size, UINT32 *max_stack);
typedef char* (__cdecl *MONO_DISASM_CODE)(void *dishelper, void *method, void *ip, void *end);



typedef void* (__cdecl *MONO_IMAGE_RVA_MAP)(void *image, UINT32 addr);

class CPipeServer : Pipe
{
private:	
	wchar_t datapipename[256];
	wchar_t eventpipename[256];

	void *mono_selfthread;

	G_FREE g_free;
	MONO_GET_ROOT_DOMAIN mono_get_root_domain;
	MONO_THREAD_ATTACH mono_thread_attach;
	MONO_THREAD_DETACH mono_thread_detach;
	MONO_OBJECT_GET_CLASS mono_object_get_class;
	MONO_CLASS_GET_NAME mono_class_get_name;
	MONO_CLASS_GET_NAMESPACE mono_class_get_namespace;
	MONO_DOMAIN_FOREACH mono_domain_foreach;
	MONO_DOMAIN_SET mono_domain_set;
	MONO_ASSEMBLY_FOREACH mono_assembly_foreach;	
	MONO_ASSEMBLY_GET_IMAGE mono_assembly_get_image;
	
	MONO_IMAGE_GET_NAME mono_image_get_name;
	MONO_IMAGE_GET_TABLE_INFO mono_image_get_table_info;
    MONO_IMAGE_RVA_MAP mono_image_rva_map;

	MONO_TABLE_INFO_GET_ROWS mono_table_info_get_rows;
	MONO_METADATA_DECODE_ROW_COL mono_metadata_decode_row_col;
	MONO_METADATA_STRING_HEAP mono_metadata_string_heap;
	MONO_CLASS_GET mono_class_get;
	MONO_CLASS_FROM_NAME_CASE mono_class_from_name_case;

	MONO_CLASS_NUM_FIELDS mono_class_num_fields;
	MONO_CLASS_GET_FIELDS mono_class_get_fields;

	MONO_CLASS_NUM_METHODS mono_class_num_methods;
	MONO_CLASS_GET_METHODS mono_class_get_methods;

	MONO_CLASS_GET_METHOD_FROM_NAME mono_class_get_method_from_name;


	MONO_FIELD_GET_NAME mono_field_get_name;
	MONO_FIELD_GET_TYPE mono_field_get_type;
	MONO_FIELD_GET_PARENT mono_field_get_parent;
	MONO_FIELD_GET_OFFSET mono_field_get_offset;

	MONO_TYPE_GET_NAME mono_type_get_name;
	MONO_TYPE_GET_TYPE mono_type_get_type;


	MONO_METHOD_GET_NAME mono_method_get_name;
	MONO_METHOD_GET_HEADER mono_method_get_header;
	MONO_METHOD_GET_CLASS mono_method_get_class;


	MONO_COMPILE_METHOD mono_compile_method;
	MONO_FREE_METHOD mono_free_method;

	MONO_JIT_INFO_TABLE_FIND mono_jit_info_table_find;
	MONO_JIT_INFO_GET_METHOD mono_jit_info_get_method;
	MONO_JIT_INFO_GET_CODE_START mono_jit_info_get_code_start;
	MONO_JIT_INFO_GET_CODE_SIZE mono_jit_info_get_code_size;
	
	MONO_METHOD_HEADER_GET_CODE mono_method_header_get_code;
	MONO_DISASM_CODE mono_disasm_code;

	BOOL attached;

	void CreatePipeandWaitForconnect(void);

	void InitMono();
	void Object_GetClass();
	void EnumDomains();
	void SetCurrentDomain();
	void EnumAssemblies();
	void GetImageFromAssembly();
	void GetImageName();
	void EnumClassesInImage();
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
	void GetClassName();
	void GetClassNamespace();
	void FreeMethod();

	void DisassembleMethod();

public:
	CPipeServer(void);
	~CPipeServer(void);

	void Start(void);
};
