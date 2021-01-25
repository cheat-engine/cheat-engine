#if defined test_56_btype_excess_1
struct A {} int i;

#elif defined test_57_btype_excess_2
char int i;

#elif defined test_58_function_redefinition
int f(void) { return 0; }
int f(void) { return 1; }

#elif defined test_global_redefinition
int xxx = 1;
int xxx;
int xxx = 2;

#elif defined test_59_function_array
int (*fct)[42](int x);

#elif defined test_60_enum_redefinition
enum color { RED, GREEN, BLUE };
enum color { R, G, B };
enum color c;

#elif defined test_62_enumerator_redefinition
enum color { RED, GREEN, BLUE };
enum rgb { RED, G, B};
enum color c = RED;

#elif defined test_63_local_enumerator_redefinition
enum {
    FOO,
    BAR
};

int main(void)
{
    enum {
        FOO = 2,
        BAR
    };

    return BAR - FOO;
}

#elif defined test_61_undefined_enum
enum rgb3 c = 42;

#elif defined test_74_non_const_init
int i = i++;

#elif defined test_pointer_assignment

void (*f1)(void);
void f2(void) {}

struct s1 *ps1;
struct s2 *ps2;

void *v1, **v2, ***v3;

enum e1 { a = 4 } e10, *e11, *e12;
enum e2 { b = -4 } e20, *e21;
enum e3 { c = 5000000000LL } e30;

int *ip;
unsigned int *up;
long *lp;
long long *llp;

char **c1;
char const **c2;
unsigned char **u1;

int no_main ()
{
    // function
    f1 = f2;
    // struct
    ps1 = ps2;
    // void*
    v1 = v3;
    v2 = v3;

    // enum
    e11 = e12;
    e11 = e21;
    e11 = &e10;
    ip = &e10;
    ip = &e20;
    up = &e10;
    up = &e20;
    up = &e30;

    lp = ip;
    lp = llp;

    // constness
    c1 = c2;
    *c1 = *c2;
    **c1 = **c2;

    // unsigned = signed
    u1 = c2;
    *u1 = *c2;
    **u1 = **c2;

    c2 = c1;
    *c2 = *c1;
    **c2 = **c1;

    return 0;
}


#elif defined test_enum_compat
enum e4;
enum e5;
void f3(enum e4 e);
void f3(enum e5 e);

#elif defined test_enum_compat_2
enum e6 { E1 = -1, E0 };
void f3(enum e6);
void f3(int);        // should work as int and e6 are compatible
void f4(enum e6 e);
void f4(unsigned e); // should error as unsigned and e6 are incompatible

#elif defined test_ptr_to_str
void f() { _Generic((int const *[]){0}, int:0); }
#elif defined test_fnptr_to_str
void f() { _Generic((int (*(*)(float,char))(double,int)){0}, int:0); }
#elif defined test_array_to_str
void f() { _Generic((int(*)[3]){0}, int:0); }
#elif defined test_duplicate_def_1
static enum myenum { L = -1 } L;                                                
#elif defined test_duplicate_def_2
void foo(void) {
static enum myenum { L = -1 } L;                                                
}
#elif defined test_abstract_decls
int bar(const char *());     // abstract declarator here is okay
int bar (const char *(*g)()) // should match this 'g' argument
{
  g();
  return 42;
}
int foo(int ())              // abstract decl is wrong in definitions
{
  return 0;
#elif defined test_invalid_1
void f(char*);
void g(void) {
  f((char[]){1, ,});
}
#elif defined test_invalid_2
int ga = 0.42 { 2 };
#elif defined test_invalid_3
struct S { int a, b; };
struct T { struct S x; };
struct T gt = { 42 a: 1, 43 };
#elif defined test_invalid_4
enum E {
    x = 1 / 0
};
#elif defined test_conflicting_types
int i;
void foo(void) {
    int i;
      {
        extern double i;
        i = 42.2;
      }
}
#elif defined test_nested_types
union u {
    union u {
        int i;
    } m;
};
#elif defined test_vla_1
int X=1;

int main(void) {
  int t[][][X];
}
#elif defined test_invalid_alignas
/* _Alignas is no type qualifier */
void * _Alignas(16) p1;

#elif defined test_static_assert

#define ONE 0
 _Static_assert(ONE == 0, "don't show me this");
 _Static_assert(ONE == 1, "ONE is not 1");

#elif defined test_static_assert_2
 _Static_assert(1, "1"" is 1");
 _Static_assert(0, "0"" is 0");

#elif defined test_static_assert_c2x
 _Static_assert(1);
 _Static_assert(0);

#elif defined test_void_array
 void t[3];

#elif defined test_incomplete_enum_array
  enum e t[3];

#elif defined test_incomplete_struct_array
  struct s t[3];

#elif defined test_const_fun_array
  typedef void f(void);
  const f t[3];

#elif defined test_incomplete_array_array
  int t[][3]; // gr: not an error, see below

/******************************************************************/
#elif defined test_extern_array
int iii[] = { 1,2,3 };
extern int iii[];
int x[];
int x[2];
int x[];
int x[2];
int x[];
extern int x[2];
extern int x[];
int x[3];

/******************************************************************/
#elif defined test_func_1 \
 || defined test_func_2 \
 || defined test_func_3 \
 || defined test_func_4 \
 || defined test_func_5 \
 || defined test_func_6
#if defined test_func_1
int hello(int);
#elif defined test_func_4
static int hello(int);
#endif
int main () {
#if defined test_func_6
    static
#endif
    int hello(int);
    hello(123);
    return 0;
}
int printf(const char*, ...);
#if defined test_func_3
static int hello(int a)
#elif defined test_func_5
int hello(int a, int b)
#else
int hello(int a)
#endif
{ printf("%s: a = %d\n", __FUNCTION__, a); return 0; }

/******************************************************************/
#elif defined test_var_1 \
   || defined test_var_2 \
   || defined test_var_3
#define P(n,v) printf("%-5s: %d ; %d\n", __FUNCTION__, n, v)
#if defined test_var_1
int xxx[];
#endif
int bar();
int printf(const char*, ...);
int main ()
{
#if !defined test_var_3
    int xxx = 2;
#endif
    {
        extern int xxx[
#if defined test_var_3
        2
#endif
        ];
        P(1, xxx[0]);
        xxx[0] += 2;
    }
#if !defined test_var_3
    P(2, xxx);
#endif
    bar(123);
    return 0;
}
int xxx[1] = {1};
int bar() { P(3, xxx[0]); return 0; }

#elif defined test_var_4
struct yyy { int y; };
struct zzz;
void f1() {
    extern char *x;
    extern char **xx;
    extern struct yyy y;
    extern struct yyy *yy;
    extern struct zzz z;
    extern struct zzz *zz;
}
void f2() {
    extern char *x;
    extern char **xx;
    extern struct yyy y;
    extern struct yyy *yy;
    extern struct zzz z;
    extern struct zzz *zz;
}
struct yyy y, *yy;
struct zzz { int z; } z, *zz;

/******************************************************************/
#elif defined test_long_double_type_for_win32

int main()
{
    double *a = 0;
    long double *b = a;
    int n = _Generic(*a, double:0, long double:1);
}

#elif defined test_stray_backslash
#define x \a
x

#elif defined test_stray_backslash2
int printf(const char*, ...);
int main()
{
#define _S(x) #x
#define S(x) _S(x)
    printf("%sn\n", S(\\));
}

/******************************************************************/
#elif defined test_var_array

static struct var_len { int i; const char str[]; } var_array[] =
{ { 1, "abcdefghijklmnopqrstuvwxyz" },
  { 2, "longlonglonglonglong" },
  { 3, "tst3" } };

#elif defined test_var_array2

struct c1 { int a; int b[]; };
struct c1 c1 = { 1, { 2, 3, 4 } };

struct c2 { int c; struct c1 c1; };
struct c2 c2 = { 1, { 2, { 3, 4, 5 }}};

/******************************************************************/
#elif defined test_default_int_type
n; // warn
f(); // don't warn

#elif defined test_invalid_global_stmtexpr
n[sizeof({3;})]; // crashed in block() due to missing local scope

#elif defined test_invalid_tokckill
f(){"12"3;} // second const token killed the value of the first

/******************************************************************/
#elif defined test_duplicate_member
struct S {
  int a, a;
};
#elif defined test_duplicate_member_anon
struct S1 {
  int b;
  struct {
    int b;
  } c;
};
struct S2 {
  int d;
  struct {
    int d;
  };
};
#endif
