#pragma once

#include "Pipe.h"

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



typedef void (__cdecl *MonoDomainFunc) (void *domain, void *user_data);
typedef void (__cdecl *GFunc)          (void *data, void *user_data);


typedef void* (__cdecl *MONO_GET_ROOT_DOMAIN)(void);
typedef void* (__cdecl *MONO_THREAD_ATTACH)(void *domain);
typedef void* (__cdecl *MONO_OBJECT_GET_CLASS)(void *object);
typedef char* (__cdecl *MONO_CLASS_GET_NAME)(void *klass);
typedef void (__cdecl *MONO_DOMAIN_FOREACH)(MonoDomainFunc func, void *user_data);
typedef int (__cdecl *MONO_DOMAIN_SET)(void *domain, BOOL force);
typedef int (__cdecl *MONO_ASSEMBLY_FOREACH)(GFunc func, void *user_data);
typedef void* (__cdecl *MONO_ASSEMBLY_GET_IMAGE)(void *assembly);
typedef char* (__cdecl *MONO_IMAGE_GET_NAME)(void *image);

typedef void* (__cdecl *MONO_IMAGE_GET_TABLE_INFO)(void *image, int table_id);
typedef int (__cdecl *MONO_TABLE_INFO_GET_ROWS)(void *tableinfo);
typedef int (__cdecl *MONO_METADATA_DECODE_ROW_COL)(void *tableinfo, int idx, unsigned int col);
typedef char* (__cdecl *MONO_METADATA_STRING_HEAP)(void *image, UINT32 index);

typedef void* (__cdecl *MONO_CLASS_GET)(void *image, UINT32 tokenindex);
typedef void* (__cdecl *MONO_CLASS_GET_METHODS)(void *klass, void *iter);
typedef void* (__cdecl *MONO_CLASS_GET_FIELDS)(void *klass, void *iter);
typedef int (__cdecl *MONO_CLASS_NUM_FIELDS)(void *klass);
typedef int (__cdecl *MONO_CLASS_NUM_METHODS)(void *klass);

typedef char* (__cdecl *MONO_FIELD_GET_NAME)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_TYPE)(void *field);
typedef void* (__cdecl *MONO_FIELD_GET_PARENT)(void *field);
typedef int (__cdecl *MONO_FIELD_GET_OFFSET)(void *field);

typedef char* (__cdecl *MONO_METHOD_GET_NAME)(void *method);


class CPipeServer : Pipe
{
private:	
	wchar_t datapipename[256];
	wchar_t eventpipename[256];

	MONO_GET_ROOT_DOMAIN mono_get_root_domain;
	MONO_THREAD_ATTACH mono_thread_attach;
	MONO_OBJECT_GET_CLASS mono_object_get_class;
	MONO_CLASS_GET_NAME mono_class_get_name;
	MONO_DOMAIN_FOREACH mono_domain_foreach;
	MONO_DOMAIN_SET mono_domain_set;
	MONO_ASSEMBLY_FOREACH mono_assembly_foreach;	
	MONO_ASSEMBLY_GET_IMAGE mono_assembly_get_image;
	MONO_IMAGE_GET_NAME mono_image_get_name;

	MONO_IMAGE_GET_TABLE_INFO mono_image_get_table_info;
	MONO_TABLE_INFO_GET_ROWS mono_table_info_get_rows;
	MONO_METADATA_DECODE_ROW_COL mono_metadata_decode_row_col;
	MONO_METADATA_STRING_HEAP mono_metadata_string_heap;
	MONO_CLASS_GET mono_class_get;

	MONO_CLASS_NUM_FIELDS mono_class_num_fields;
	MONO_CLASS_GET_FIELDS mono_class_get_fields;

	MONO_CLASS_NUM_METHODS mono_class_num_methods;
	MONO_CLASS_GET_METHODS mono_class_get_methods;


	MONO_FIELD_GET_NAME mono_field_get_name;
	MONO_FIELD_GET_TYPE mono_field_get_type;
	MONO_FIELD_GET_PARENT mono_field_get_parent;
	MONO_FIELD_GET_OFFSET mono_field_get_offset;

	MONO_METHOD_GET_NAME mono_method_get_name;

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

public:
	CPipeServer(void);
	~CPipeServer(void);

	void Start(void);
};
