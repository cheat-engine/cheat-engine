#ifdef __leading_underscore
# define _ "_"
#else
# define _
#endif

extern int printf (const char *, ...);
extern void vide(void);
__asm__(_"vide: ret");

int main() {
    vide();
    printf ("okay\n");
    return 0;
}
