#include "libtcc.h"

#ifdef _MSC_VER
# define NOINLINE __declspec(noinline)
#else
# define NOINLINE __attribute__((noinline))
#endif

#if ONE_SOURCE
#define ST_NINL static NOINLINE
#else
#define ST_NINL
#endif

int tcc_open_string(TCCState* s, const char* strname, const char* str)
{
    int len = str == NULL ? 0 : (int)strlen(str);
    if(len == 0) {
        return -1;
    } else if (strname != NULL) {
        tcc_open_bf(s, strname, len);
    } else {
        tcc_open_bf(s, "<string>", len);
    }
    memcpy(file->buffer, str, len);
    return 0;
}

struct PrivateIncludeCtx {
    TCCIncludeCtx lib_ctx;
    int quoted;
    int status;
    TCCState* s1;
    char* resolved_ptr;
    const char* previous_ptr;
};

typedef struct PrivateIncludeCtx PrivateIncludeCtx;

#define GET_PRIVATE_CTX(CTX) ((PrivateIncludeCtx*)( \
        ((unsigned char*)CTX) - offsetof(PrivateIncludeCtx, lib_ctx) \
    ))

ST_NINL TCCState* incctx_get_state(TCCIncludeCtx* c)
{
    return GET_PRIVATE_CTX(c)->s1;
}

ST_NINL int inctx_is_quoted(TCCIncludeCtx* c)
{
    return GET_PRIVATE_CTX(c)->quoted;
}

ST_NINL const char* inctx_get_source(TCCIncludeCtx* c)
{
    return GET_PRIVATE_CTX(c)->previous_ptr;
}

ST_NINL void inctx_resolve(TCCIncludeCtx* c, const char* path)
{
    PrivateIncludeCtx* p = GET_PRIVATE_CTX(c);
    if(path == NULL) {
        return;
    }
    pstrcpy(p->resolved_ptr, sizeof(file->filename), path);
    p->status = TCC_LIBINC_RESOLVE;
}

ST_NINL int inctx_open_file(TCCIncludeCtx* c, const char* filename)
{
    PrivateIncludeCtx* p = GET_PRIVATE_CTX(c);
    int ret = tcc_open(p->s1, filename);
    if(ret > -1) {
        p->status = TCC_LIBINC_OPENED;
    }
    return 0;
}

ST_NINL int inctx_open_named_string(TCCIncludeCtx* c, const char* str, const char* strname)
{
    PrivateIncludeCtx* p = GET_PRIVATE_CTX(c);
    int ret = tcc_open_string(p->s1, strname, str);
    if(ret > -1) {
        p->status = TCC_LIBINC_OPENED;
    }
    return ret;
}

ST_NINL int inctx_open_string(TCCIncludeCtx* c, const char* str)
{
    return inctx_open_named_string(c, str, NULL);
}

ST_INLN TCCIncludeCtx* inctx_open(PrivateIncludeCtx* p, TCCState* s, char* buf, int quoted)
{
    TCCIncludeCtx* c = &p->lib_ctx;
    p->s1 = s;
    p->quoted = quoted;
    p->status = TCC_LIBINC_FAILED;
    p->resolved_ptr = buf;
    p->previous_ptr = file->true_filename;
    c->get_state = incctx_get_state;
    c->is_quoted = inctx_is_quoted;
    c->get_source = inctx_get_source;
    c->resolve = inctx_resolve;
    c->open_file = inctx_open_file;
    c->open_string = inctx_open_string;
    c->open_named_string = inctx_open_named_string;
    return c;
}

ST_FUNC int try_user_include_func(TCCState* s, char* buf, char* filename, int quoted)
{
    int ret = TCC_LIBINC_FAILED;
    if(s->include_func != NULL) {
        PrivateIncludeCtx p;
        TCCIncludeCtx* c = inctx_open(&p, s, buf, quoted);
        if(s->include_func(s->include_opaque, c, filename) != TCC_LIBINC_FAILED) {
            ret = p.status;
        }
    }
    return ret;
}

#define DEFINE_FIELD_COUNTER(field) \
    LIBTCCAPI int tcc_count_##field##(TCCState *s) \
    { \
        return s->nb_##field; \
    }

ST_FUNC void enum_path_array(char** paths, int count, void* ctx, TCCPathCallback path_cb)
{
    int i = 0;
    char buf1[sizeof file->filename];
    for(; i < count; i++) {
        const char* path = (const char*)paths[i];
        if(*path == '\0')
            continue;
        pstrcpy(buf1, sizeof(buf1), path);
        if(path_cb(ctx, (const char*)buf1) < 0)
            return;
    }
}

#define DEFINE_FIELD_LISTER(field) \
    DEFINE_FIELD_COUNTER(field) \
     \
    LIBTCCAPI void tcc_list_##field (TCCState *s, void* ctx, TCCPathCallback path_cb) \
    { \
        enum_path_array(s -> field, s->nb_##field, ctx, path_cb); \
    }

DEFINE_FIELD_LISTER(include_paths)

DEFINE_FIELD_LISTER(sysinclude_paths)

DEFINE_FIELD_LISTER(library_paths)

LIBTCCAPI void tcc_set_include_func(TCCState *s, void *include_opaque, TCCIncludeFunc include_func)
{
    s->include_opaque = include_opaque;
    s->include_func = include_func;
}

LIBTCCAPI TCCIncludeFunc tcc_get_include_func(TCCState *s)
{
    return s->include_func;
}

LIBTCCAPI void* tcc_get_include_opaque(TCCState *s)
{
    return s->include_opaque;
}

LIBTCCAPI int tcc_get_active_filename(TCCState *s, char buffer[], size_t* bufferSize)
{
    int ret = 0;
    size_t required = (size_t)strlen(file->filename) + 1;
    if(required > (*bufferSize)) {
        *bufferSize = required;
    } else if(required > 0) {
        ret = 1;
        memset((void*)buffer, required, 0);
        strncpy(buffer, file->filename, (size_t)required - 1);
    }
    return ret;
}
