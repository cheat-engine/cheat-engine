// ucdl.h
// External interface to the UnderC extension DLL
#ifndef __UCDL_H
#define __UCDL_H

#ifdef __UNDERC__
# ifndef __linux__
#   define XAPI __stdcall
# else
#   define XAPI
# endif
# define STD
#else
# define XAPI
# ifndef __linux__
#   define STD __stdcall
# else
#   define STD
# endif
#endif
typedef void *Handle;

#ifdef __cplusplus
 extern "C" {
#endif

XAPI int    STD uc_init(char *defs_file, int use_defs);
XAPI int    STD uc_main(int argc, char **argv);  
XAPI void   STD uc_finis();
XAPI int    STD uc_include(char *path);
XAPI void   STD uc_interactive_loop();
XAPI void*  STD uc_main_window();
XAPI void   STD uc_load(char *path);
XAPI int    STD uc_run();
XAPI int    STD uc_exec(char* buffer);
XAPI void   STD uc_result(char *buffer, int sz);
XAPI void   STD uc_error(char *buffer, int sz);
XAPI int    STD uc_error_pos(char *filename);
XAPI int    STD uc_eval(char *expr, char *res, int sz);
XAPI void   STD uc_set_quote(char *var, char *val);
XAPI void   STD uc_init_ref(char *type, char *var, void *addr);
XAPI Handle STD uc_compile(const char *args, const char *expr);
int             uc_eval_args(void *sc, void *res, ...);
int             uc_eval_method_args(void *sc, void *obj, void *result, ...);
XAPI int    STD ucc_eval_exp(void *sc, void *args, void *res);
XAPI Handle STD uc_compile_fn(char *parm, char *expr);
XAPI int    STD uc_import(char *dcl, void *fn);

#ifdef __cplusplus
 }

class UCDll {
public:
  UCDll(bool do_load_default = true)
  { uc_init(NULL,do_load_default); }

  ~UCDll()
  { uc_finis(); }

};

#endif

#endif
