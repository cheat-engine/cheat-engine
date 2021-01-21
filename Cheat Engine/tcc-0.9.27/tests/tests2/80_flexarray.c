#include <stdio.h>
struct wchar {
    char *data; char mem[];
};
struct wint {
    char *data; int mem[];
};
int f1char (void) {
    char s[9]="nonono";
    struct wchar q = {"bugs"};
    return !s[0];
}
int f1int (void) {
    char s[9]="nonono";
    struct wint q = {"bugs"};
    return !s[0];
}
int main (void) {
   char s[9]="nonono";
   static struct wchar q = {"bugs", {'c'}};
   //printf ("tcc has %s %s\n", s, q.data);
   if (f1char() || f1int())
     printf ("bla\n");
   return !s[0];
}
