#include <stdio.h>

const int a = 0;

struct a {
	int a;
};

struct b {
	int a;
};

int a_f()
{
	return 20;
}

int b_f()
{
	return 10;
}

typedef int (*fptr)(int);
typedef void (*vfptr)(int);
int foo(int i)
{
  return i;
}
void void_foo(int i) {}

typedef int int_type1;

#define gen_sw(a) _Generic(a, const char *: 1, default: 8, int: 123);

int main()
{
	int i = 0;
	signed long int l = 2;
	struct b titi;
	const int * const ptr;
	const char *ti;
	int_type1 i2;

	i = _Generic(a, int: a_f, const int: b_f)();
	printf("%d\n", i);
	i = _Generic(a, int: a_f() / 2, const int: b_f() / 2);
	printf("%d\n", i);
	i = _Generic(ptr, int *:1, int * const:2, default:20);
	printf("%d\n", i);
	i = gen_sw(a);
	printf("%d\n", i);
	i = _Generic(titi, struct a:1, struct b:2, default:20);
	printf("%d\n", i);
	i = _Generic(i2, char: 1, int : 0);
	printf("%d\n", i);
	i = _Generic(a, char:1, int[4]:2, default:5);
	printf("%d\n", i);
	i = _Generic(17, int :1, int **:2);
	printf("%d\n", i);
	i = _Generic(17L, int :1, long :2, long long : 3);
	printf("%d\n", i);
	i = _Generic("17, io", char *: 3, const char *: 1);
	printf("%d\n", i);
	i = _Generic(ti, const unsigned char *:1, const char *:4, char *:3,
		     const signed char *:2);
	printf("%d\n", i);
	printf("%s\n", _Generic(i + 2L, long: "long", int: "int",
				long long: "long long"));
	i = _Generic(l, long: 1, int: 2);
	printf("%d\n", i);
	i = _Generic(foo, fptr: 3, int: 4, vfptr: 5);
	printf("%d\n", i);
	i = _Generic(void_foo, fptr: 3, int: 4, vfptr: 5);
	printf("%d\n", i);

	(void)_Generic((int(*)[2]){0}, int(*)[2]:0, int(*)[4]:0); //shouldn't match twice

	//should accept ({ }) in the controlling expr of _Generic even in const_wanted contexts
	struct { _Bool x_0: _Generic(({0;}),default:1); } my_x;

	_Generic((__typeof((float const)((float const){42}))*){0}, float*: 0); //casts lose top-level qualifiers
	int const x = 42; __typeof((__typeof(x))x) *xp = 0; (void)_Generic(xp, int*: 0); //casts lose top-level qualifiers

	//TEST TERNARY:
	//Same type
	_Generic( 0?(long*)0:(long*)0,  long*: (void)0);
	//combining of qualifiers
	_Generic( 0?(long volatile*)0:(long const*)0,  long const volatile*: (void)0);
	//nul-ptr constant selects other type
	_Generic( 0?(long*)0:0,			long*: (void)0);
	_Generic( 0?(long*)0:(void*)0, long*: (void)0);

	//void ptrs get chosen preferentially; qualifs still combine
	_Generic( 0?(int volatile*)0: (void const*)1, void volatile const*: (void)0);
	//like gcc but not clang, don't treat (void* const as the null-ptr constant)
	_Generic( 0?(int volatile*)0: (void const*)0, void volatile const*: (void)0);

	//ptrs to incomplete types get completed
	(void)(sizeof(struct { int x:_Generic( 0?(int (*)[4])0 : (int (*)[])0, int (*)[4]:+1, int (*)[5]:(void)0); }));
	(void)(sizeof(struct { int x:_Generic( 0?(int (*)[])0 : (int (*)[4])0, int (*)[4]:+1, int (*)[5]:(void)0); }));

	{
	  /* completion shouldn't affect the type of decl */
		char **argv;
		_Generic(argv, char**: (void)0);
		_Generic(0?(char const*)0:argv[0], char const*: (void)0);
		_Generic(argv, char**: (void)0);
	}
	{
	  extern int (*ar)[];
	  (void)(sizeof(struct { int x:_Generic( 0?(int (*)[4])0 : (int (*)[])0, int (*)[4]:+1, int (*)[5]:(void)0); }));
	  (void)(sizeof(struct { int x:_Generic( 0?(int (*)[])0 : (int (*)[4])0, int (*)[4]:+1, int (*)[5]:(void)0); }));
	  (void)(sizeof(struct { int x:_Generic( 0?ar : (int (*)[4])0, int (*)[4]:+1, int (*)[5]:(void)0); }));
	  (void)(sizeof(struct { int x:_Generic( 0?(int (*)[4])0 : ar, int (*)[4]:+1, int (*)[5]:(void)0); }));
	  (void)(sizeof(struct { int x:_Generic( 0?(int (*)[5])0 : ar, int (*)[5]:+1, int (*)[4]:(void)0); }));
	}

	return 0;
}
