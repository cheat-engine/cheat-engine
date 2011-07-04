// usedl8.c

// exports1.cpp
#include "ucdl.h"

double sqr(double x) { return x*x; }

int main() {
  uc_init(NULL,1);
  uc_load("script2.uc"); 
  uc_finis();
}

