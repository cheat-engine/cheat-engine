#ifndef LIBTCC_H
#define LIBTCC_H

#ifndef LIBTCCAPI
# define LIBTCCAPI
#endif

#ifdef __cplusplus
extern "C" {
#endif

struct TCCState;

typedef struct TCCState TCCState;

typedef void (*TCCErrorFunc)(void *opaque, const char *msg);

/* create a new TCC compilation context */
LIBTCCAPI TCCState *tcc_new(void);

/* free a TCC compilation context */
LIBTCCAPI void tcc_delete(TCCState *s);

/* set CONFIG_TCCDIR at runtime */
LIBTCCAPI void tcc_set_lib_path(TCCState *s, const char *path);

/* set error/warning display callback */
LIBTCCAPI void tcc_set_error_func(TCCState *s, void *error_opaque, TCCErrorFunc error_func);

/* return error/warning callback */
LIBTCCAPI TCCErrorFunc tcc_get_error_func(TCCState *s);

/* return error/warning callback opaque pointer */
LIBTCCAPI void *tcc_get_error_opaque(TCCState *s);

/* set options as from command line (multiple supported) */
LIBTCCAPI void tcc_set_options(TCCState *s, const char *str);

/*****************************/
/* preprocessor */

/* add include path */
LIBTCCAPI int tcc_add_include_path(TCCState *s, const char *pathname);

/* add in system include path */
LIBTCCAPI int tcc_add_sysinclude_path(TCCState *s, const char *pathname);

/* define preprocessor symbol 'sym'. value can be NULL, sym can be "sym=val" */
LIBTCCAPI void tcc_define_symbol(TCCState *s, const char *sym, const char *value);

/* undefine preprocess symbol 'sym' */
LIBTCCAPI void tcc_undefine_symbol(TCCState *s, const char *sym);

/*****************************/
/* compiling */

/* add a file (C file, dll, object, library, ld script). Return -1 if error. */
LIBTCCAPI int tcc_add_file(TCCState *s, const char *filename);

/* compile a string containing a C source. Return -1 if error. */
LIBTCCAPI int tcc_compile_string(TCCState *s, const char *buf);

/*****************************/
/* linking commands */

/* set output type. MUST BE CALLED before any compilation */
LIBTCCAPI int tcc_set_output_type(TCCState *s, int output_type);
#define TCC_OUTPUT_MEMORY   1 /* output will be run in memory (default) */
#define TCC_OUTPUT_EXE      2 /* executable file */
#define TCC_OUTPUT_DLL      3 /* dynamic library */
#define TCC_OUTPUT_OBJ      4 /* object file */
#define TCC_OUTPUT_PREPROCESS 5 /* only preprocess (used internally) */

/* equivalent to -Lpath option */
LIBTCCAPI int tcc_add_library_path(TCCState *s, const char *pathname);

/* the library name is the same as the argument of the '-l' option */
LIBTCCAPI int tcc_add_library(TCCState *s, const char *libraryname);

/* add a symbol to the compiled program */
LIBTCCAPI int tcc_add_symbol(TCCState *s, const char *name, const void *val);

/* output an executable, library or object file. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI int tcc_output_file(TCCState *s, const char *filename);

/* link and run main() function and return its value. DO NOT call
   tcc_relocate() before. */
LIBTCCAPI int tcc_run(TCCState *s, int argc, char **argv);

/* do all relocations (needed before using tcc_get_symbol()) */
LIBTCCAPI int tcc_relocate(TCCState *s1, void *ptr);
/* possible values for 'ptr':
   - TCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. */
#define TCC_RELOCATE_AUTO (void*)1

/* return symbol value or NULL if not found */
LIBTCCAPI void *tcc_get_symbol(TCCState *s, const char *name);

/* return symbol value or NULL if not found */
LIBTCCAPI void tcc_list_symbols(TCCState *s, void *ctx,
    void (*symbol_cb)(void *ctx, const char *name, const void *val));

//Cheat Engine
#ifndef _TCCINCLUDECTX_TYPEDEF_
#define _TCCINCLUDECTX_TYPEDEF_
struct TCCIncludeCtx;
typedef struct TCCIncludeCtx TCCIncludeCtx;
#endif

struct TCCIncludeCtx {
    // Get the originating compiler state
    TCCState* (*get_state)(TCCIncludeCtx *c);

    // #include "filename"` will result in 1
    // #include <filename> will result in 0
    int(*is_quoted)(TCCIncludeCtx* c);

    // Get the filename that was the source of this #include
    const char* (*get_source)(TCCIncludeCtx *c);

    // resolve the include to an absolute filepath. when TCCIncludeFunc returns, filepath
    // is searched in the include cache before any attempt is made to open it. (NULL filepath
    // is ignored)
    void (*resolve)(TCCIncludeCtx *c, const char* path);

    // The following openers bypass the include cache and return -1 on failures.
    int (*open_file)(TCCIncludeCtx *c, const char *filename);
    int (*open_string)(TCCIncludeCtx *c, const char* str);
    int (*open_named_string)(TCCIncludeCtx *c, const char* str, const char* strname);
};

// Callback for handling #include statements. Should return -1 if it cant resolve the include.
// Note: IncludeCtx is only valid for the duration of the TCCIncludeFunc call.
typedef int (*TCCIncludeFunc)(void* opaque, TCCIncludeCtx* c, char* filename);

// Callback for enumerating paths in tcc_list_include_paths, etc.
// Note: Operates on copies of the stored paths (cannot be used to modify)
typedef int(*TCCPathCallback)(void* ctx, const char* path);

LIBTCCAPI void tcc_set_include_func(TCCState *s, void *include_opaque, TCCIncludeFunc include_func);
LIBTCCAPI TCCIncludeFunc tcc_get_include_func(TCCState *s);
LIBTCCAPI void* tcc_get_include_opaque(TCCState *s);
LIBTCCAPI int tcc_count_include_paths(TCCState *s);
LIBTCCAPI int tcc_count_sysinclude_paths(TCCState *s);
LIBTCCAPI int tcc_count_library_paths(TCCState *s);
LIBTCCAPI void tcc_list_include_paths(TCCState *s, void* ctx, TCCPathCallback path_cb);
LIBTCCAPI void tcc_list_sysinclude_paths(TCCState *s, void* ctx, TCCPathCallback path_cb);
LIBTCCAPI void tcc_list_library_paths(TCCState *s, void* ctx, TCCPathCallback path_cb);
//Cheat Engine Stop

#ifdef __cplusplus
}
#endif

#endif
