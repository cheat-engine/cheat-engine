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
	return 0;
}
