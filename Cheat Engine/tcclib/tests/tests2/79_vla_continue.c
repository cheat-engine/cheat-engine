#include <stdio.h>

int f(void)
{
  return 5;
}

void test1()
{
  int count = 10;
  void *addr[10];
  for(;count--;) {
    int a[f()];

    addr[count] = a;

    continue;
  }

  if(addr[9] == addr[0]) {
    printf("OK\n");
  } else {
    printf("NOT OK\n");
  }
}

void test2()
{
  int count = 10;
  void *addr[count];
  for(;count--;) {
    int a[f()];

    addr[count] = a;

    continue;
  }

  if(addr[9] == addr[0]) {
    printf("OK\n");
  } else {
    printf("NOT OK\n");
  }
}

void test3()
{
  int count = 10;
  void *addr[count];
  while(count--) {
    int a[f()];

    addr[count] = a;

    continue;
  }

  if(addr[9] == addr[0]) {
    printf("OK\n");
  } else {
    printf("NOT OK\n");
  }
}

void test4()
{
  int count = 10;
  void *addr[count];
  do {
    int a[f()];

    addr[--count] = a;

    continue;
  } while (count);

  if(addr[9] == addr[0]) {
    printf("OK\n");
  } else {
    printf("NOT OK\n");
  }
}

void test5()
{
  int count = 10;
  int a[f()];
  int c[f()];

  c[0] = 42;

  for(;count--;) {
    int b[f()];
    int i;
    for (i=0; i<f(); i++) {
      b[i] = count;
    }
  }

  if (c[0] == 42) {
    printf("OK\n");
  } else {
    printf("NOT OK\n");
  }
}

int main(void)
{
  test1();
  test2();
  test3();
  test4();
  test5();

  return 0;
}
