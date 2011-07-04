#include <stdarg.h>

 void args(int i,...)
 {
 va_list ap;
 va_start(ap,i);
 cout << *ap-- << endl;
 cout << *ap-- << endl;
 cout << *ap-- << endl;
 cout << *ap-- << endl;
 }

int sum(int i,...) {
  va_list ap;
  va_start(ap,i);
  int sumi = i;
  int val;
  while ((val = va_arg(ap,int)) != 0) 
    sumi += val;
  return sumi;
}

int choose(char *target,...) {
  va_list ap;
  va_start(ap,target);
  char *name = va_arg(ap,char *);
  while (name != NULL) {
      int id = va_arg(ap,int);
      if (strcmp(target,name) == 0) return id;
      name = va_arg(ap,char *);
  }
  return 0; // no hit!
}

int main() {
   return choose("fred","frik",1,"fred",2,0);
}




