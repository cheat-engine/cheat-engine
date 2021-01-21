extern int printf (const char *, ...);
extern void vide(void);
__asm__("vide: ret");

int main() {
    vide();
    printf ("okay\n");
    return 0;
}
