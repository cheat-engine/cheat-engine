/* The following are all valid decls, even though some subtypes
   are incomplete.  */
enum E *e;
const enum E *e1;
enum E const *e2;
struct S *s;
const struct S *s1;
struct S const *s2;

/* Various strangely looking declarators, which are all valid
   and have to map to the same numbered typedefs. */
typedef int (*fptr1)();
int f1 (int (), int);
typedef int (*fptr2)(int x);
int f2 (int (int x), int);
typedef int (*fptr3)(int);
int f3 (int (int), int);
typedef int (*fptr4[4])(int);
int f4 (int (*[4])(int), int);
typedef int (*fptr5)(fptr1);
int f5 (int (int()), fptr1);
int f1 (fptr1 fp, int i)
{
  return (*fp)(i);
}
int f2 (fptr2 fp, int i)
{
  return (*fp)(i);
}
int f3 (fptr3 fp, int i)
{
  return (*fp)(i);
}
int f4 (fptr4 fp, int i)
{
  return (*fp[i])(i);
}
int f5 (fptr5 fp, fptr1 i)
{
  return fp(i);
}
int f8 (int ([4]), int);
int main () { return 0; }
