#include <assert.h>
extern int printf(const char*, ...);

char arr[1];
static void f (void){}
void (*fp)(void) = f;
void call_fp()
{
    (fp?f:f)();
    (fp?fp:fp)();
    (fp?fp:&f)();
    (fp?&f:fp)();
    (fp?&f:&f)();
    _Generic(0?arr:arr, char*: (void)0);
    _Generic(0?&arr[0]:arr, char*: (void)0);
    _Generic(0?arr:&arr[0], char*: (void)0);
    _Generic(1?arr:arr, char*: (void)0);
    _Generic(1?&arr[0]:arr, char*: (void)0);
    _Generic(1?arr:&arr[0], char*: (void)0);
    _Generic((__typeof(1?f:f)*){0}, void (**)(void): (void)0);
    (fp?&f:f)();
    (fp?f:&f)();
    _Generic((__typeof(fp?0L:(void)0)*){0}, void*: (void)0);

    /* The following line causes a warning */
    void *xx = fp?f:1;
}

struct condstruct {
    int i;
};

static int getme(struct condstruct* s, int i)
{
    int i1 = (i != 0 ? 0 : s)->i;
    int i2 = (i == 0 ? s : 0)->i;
    int i3 = (i != 0 ? (void*)0 : s)->i;
    int i4 = (i == 0 ? s : (void*)0)->i;
    return i1 + i2 + i3 + i4;
}

int main()
{
   int Count;

   for (Count = 0; Count < 10; Count++)
   {
      printf("%d\n", (Count < 5) ? (Count*Count) : (Count * 3));
   }

   {
    int c = 0;
    #define ASSERT(X) assert(X)
    static struct stru { int x; } a={'A'},b={'B'};
    static const struct stru2 { int x; } d = { 'D' };
    ASSERT('A'==(*(1?&a:&b)).x);
    ASSERT('A'==(1?a:b).x);
    ASSERT('A'==(c?b:a).x);
    ASSERT('A'==(0?b:a).x);
    c=1;
    ASSERT('A'==(c?a:b).x);
    ASSERT(sizeof(int)    == sizeof(0 ? 'a' : c));
    ASSERT(sizeof(double) == sizeof(0 ? 'a' : 1.0));
    ASSERT(sizeof(double) == sizeof(0 ? 0.0 : 'a'));
    ASSERT(sizeof(float)  == sizeof(0 ? 'a' : 1.0f));
    ASSERT(sizeof(double) == sizeof(0 ? 0.0 : 1.0f));
    struct condstruct cs = { 38 };
    printf("%d\n", getme(&cs, 0));

    // the following lines contain type mismatch errors in every ternary expression
    //printf("comparing double with pointer : size = %d\n", sizeof(0 ? &c : 0.0));
    //printf("'%c' <> '%c'\n", (0 ? a : d).x, (1 ? a : d).x);
    //0 ? a : 0.0;
   }


   return 0;
}

/* vim: set expandtab ts=4 sw=3 sts=3 tw=80 :*/
