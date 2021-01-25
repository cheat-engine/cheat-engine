/* accept 'defined' as result of substitution */

----- 1 ------
#define AAA 2
#define BBB
#define CCC (defined ( AAA ) && AAA > 1 && !defined BBB)
#if !CCC
OK
#else
NOT OK
#endif

----- 2 ------
#undef BBB
#if CCC
OK
#else
NOT OK
#endif

----- 3 ------
#define DEFINED defined
#define DDD (DEFINED ( AAA ) && AAA > 1 && !DEFINED BBB)
#if (DDD)
OK
#else
NOT OK
#endif

----- 4 ------
#undef AAA
#if !(DDD)
OK
#else
NOT OK
#endif
