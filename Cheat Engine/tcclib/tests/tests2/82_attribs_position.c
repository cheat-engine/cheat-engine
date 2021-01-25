typedef unsigned short uint16_t;
typedef unsigned char uint8_t;

typedef union Unaligned16a {
  uint16_t u;
  uint8_t b[2];
} __attribute__((packed)) Unaligned16a;

typedef union __attribute__((packed)) Unaligned16b {
  uint16_t u;
  uint8_t b[2];
} Unaligned16b;

extern void foo (void) __attribute__((stdcall));
void __attribute__((stdcall)) foo (void)
{
}

#define __stdcall __attribute__((stdcall))
extern int some_stdcall_func (int, int, int) __stdcall;
__stdcall int __stdcall some_stdcall_func(int foo, int bar, int baz) {
	//printf("Hello from stdcall: %i %i %i\n", foo, bar, baz);
	return 43;
}

/* The actual attribute isn't important, must just be
   parsable.  */
#define ATTR __attribute__((__noinline__))
int ATTR actual_function() {
  return 42;
}

extern int printf (const char *, ...);
static int globalvar;
int main()
{
    void *function_pointer = &actual_function;
    int localvar = 42, i;

    int a = ((ATTR int(*) (void)) function_pointer)();
    printf("%i\n", a);

    /* In the following we once misparsed 'ATTR *' is a btype
       and hence the whole type was garbled.  */
    int b = ( (int(ATTR *)(void))  function_pointer)();
    printf("%i\n", b);

    /* All these should work and leave the stack pointer in its original
       position.  */
    some_stdcall_func(1, 10, 100);
    ((int __stdcall (*)(int, int, int))some_stdcall_func) (2, 20, 200);
    ((int(*__stdcall)(int, int, int))some_stdcall_func) (3, 30, 300);
    for (i = 0; i < 1024; i++) {
	globalvar = i;
	/* This was once misparsed at <= gitrev 325241c0, forgetting
	   the stdcall attribute on the function pointer leading to
	   stack increment being done twice (in callee and caller).
	   This will clobber 'i' and 'localvar' which is how we detect
	   this.  */
	((int(__stdcall*)(int, int, int))some_stdcall_func) (4, 40, 400);
	if (localvar != 42 || globalvar != i)
	  printf("error, localvar=%d i=%d globalvar=%d\n", localvar, i, globalvar);
    }
    return 0;
}
