/* integer promotion */

int printf(const char*, ...);
#define promote(s) printf(" %ssigned : %s\n", (s) - 100 < 0 ? "  " : "un", #s);

int main (void)
{
    struct {
        unsigned ub:3;
        unsigned u:32;
        unsigned long long ullb:35;
        unsigned long long ull:64;
        unsigned char c;
    } s = { 1, 1, 1 };

    promote(s.ub);
    promote(s.u);
    promote(s.ullb);
    promote(s.ull);
    promote(s.c);
    printf("\n");

    promote((1 ? s.ub : 1));
    promote((1 ? s.u : 1));
    promote((1 ? s.ullb : 1));
    promote((1 ? s.ull : 1));
    promote((1 ? s.c : 1));
    printf("\n");

    promote(s.ub << 1);
    promote(s.u << 1);
    promote(s.ullb << 1);
    promote(s.ull << 1);
    promote(s.c << 1);
    printf("\n");

    promote(+s.ub);
    promote(+s.u);
    promote(+s.ullb);
    promote(+s.ull);
    promote(+s.c);
    printf("\n");

    promote(-s.ub);
    promote(-s.u);
    promote(-s.ullb);
    promote(-s.ull);
    promote(-s.c);
    printf("\n");

    promote(~s.ub);
    promote(~s.u);
    promote(~s.ullb);
    promote(~s.ull);
    promote(~s.c);
    printf("\n");

    promote(!s.ub);
    promote(!s.u);
    promote(!s.ullb);
    promote(!s.ull);
    promote(!s.c);
    printf("\n");

    promote(+(unsigned)s.ub);
    promote(-(unsigned)s.ub);
    promote(~(unsigned)s.ub);
    promote(!(unsigned)s.ub);

    return 0;
}
