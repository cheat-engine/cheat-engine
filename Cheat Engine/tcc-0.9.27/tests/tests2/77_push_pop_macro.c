#include <stdio.h>

int main()
{
    /* must not affect how #pragma ppop_macro works */
    #define pop_macro foobar1

    /* must not affect how #pragma push_macro works */
    #define push_macro foobar2

    #undef abort
    #define abort "111"
    printf("abort = %s\n", abort);

    #pragma push_macro("abort")
    #undef abort
    #define abort "222"
    printf("abort = %s\n", abort);

    #pragma push_macro("abort")
    #undef abort
    #define abort "333"
    printf("abort = %s\n", abort);

    #pragma pop_macro("abort")
    printf("abort = %s\n", abort);

    #pragma pop_macro("abort")
    printf("abort = %s\n", abort);
}
