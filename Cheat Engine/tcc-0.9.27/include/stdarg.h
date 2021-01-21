#ifndef _STDARG_H
#define _STDARG_H

#ifdef __x86_64__
#ifndef _WIN64

//This should be in sync with the declaration on our lib/libtcc1.c
/* GCC compatible definition of va_list. */
typedef struct {
    unsigned int gp_offset;
    unsigned int fp_offset;
    union {
        unsigned int overflow_offset;
        char *overflow_arg_area;
    };
    char *reg_save_area;
} __va_list_struct;

typedef __va_list_struct va_list[1];

void __va_start(__va_list_struct *ap, void *fp);
void *__va_arg(__va_list_struct *ap, int arg_type, int size, int align);

#define va_start(ap, last) __va_start(ap, __builtin_frame_address(0))
#define va_arg(ap, type)                                                \
    (*(type *)(__va_arg(ap, __builtin_va_arg_types(type), sizeof(type), __alignof__(type))))
#define va_copy(dest, src) (*(dest) = *(src))
#define va_end(ap)

/* avoid conflicting definition for va_list on Macs. */
#define _VA_LIST_T

#else /* _WIN64 */
typedef char *va_list;
#define va_start(ap,last) __builtin_va_start(ap,last)
#define va_arg(ap, t) ((sizeof(t) > 8 || (sizeof(t) & (sizeof(t) - 1))) \
	? **(t **)((ap += 8) - 8) : *(t  *)((ap += 8) - 8))
#define va_copy(dest, src) ((dest) = (src))
#define va_end(ap)
#endif

#elif __arm__
typedef char *va_list;
#define _tcc_alignof(type) ((int)&((struct {char c;type x;} *)0)->x)
#define _tcc_align(addr,type) (((unsigned)addr + _tcc_alignof(type) - 1) \
                               & ~(_tcc_alignof(type) - 1))
#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap = (void *) ((_tcc_align(ap,type)+sizeof(type)+3) \
                        &~3), *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_copy(dest, src) (dest) = (src)
#define va_end(ap)

#elif defined(__aarch64__)
typedef struct {
    void *__stack;
    void *__gr_top;
    void *__vr_top;
    int   __gr_offs;
    int   __vr_offs;
} va_list;
#define va_start(ap, last) __va_start(ap, last)
#define va_arg(ap, type) __va_arg(ap, type)
#define va_end(ap)
#define va_copy(dest, src) ((dest) = (src))

#else /* __i386__ */
typedef char *va_list;
/* only correct for i386 */
#define va_start(ap,last) ap = ((char *)&(last)) + ((sizeof(last)+3)&~3)
#define va_arg(ap,type) (ap += (sizeof(type)+3)&~3, *(type *)(ap - ((sizeof(type)+3)&~3)))
#define va_copy(dest, src) (dest) = (src)
#define va_end(ap)
#endif

/* fix a buggy dependency on GCC in libio.h */
typedef va_list __gnuc_va_list;
#define _VA_LIST_DEFINED

#endif /* _STDARG_H */
