/*
 * Test that allocating a variable length array in a loop
 * does not use up a linear amount of memory
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define LOOP_COUNT 1000
#define ARRAY_SIZE 100

/* Overwrite a VLA. This will overwrite the return address if SP is incorrect */
void smash(char *p, int n) {
  memset(p, 0, n);
}

int test1(int n) {
  int i;
  char *array_ptrs[LOOP_COUNT];
  
  for (i = 0; i < LOOP_COUNT; ++i) {
    char test[n];
    smash(test, n);
    array_ptrs[i] = test;
  }
  
  return (array_ptrs[0]-array_ptrs[LOOP_COUNT-1] < n) ? 0 : 1;
}

/* ensure goto does not circumvent array free */
int test2(int n) {
  char *array_ptrs[LOOP_COUNT];

  int i = 0;
loop:;
  char test[n];
  smash(test, n);
  if (i >= LOOP_COUNT)
    goto end;
  array_ptrs[i] = test;
  ++i;
  goto loop;

end:
  smash(test, n);
  char test2[n];
  smash(test2, n);
  return (array_ptrs[0] - array_ptrs[LOOP_COUNT-1] < n) ? 0 : 1;
}

int test3(int n) {
  char test[n];
  smash(test, n);
  goto label;
label:
  smash(test, n);
  char test2[n];
  smash(test2, n);
  return (test-test2 >= n) ? 0 : 1;
}

#define RUN_TEST(t) \
  if (!testname || (strcmp(#t, testname) == 0)) { \
    fputs(#t "... ", stdout); \
    fflush(stdout); \
    if (t(ARRAY_SIZE) == 0) { \
      fputs("success\n", stdout); \
    } else { \
      fputs("failure\n", stdout); \
      retval = EXIT_FAILURE; \
    } \
  }

int main(int argc, char **argv) {
  const char *testname = NULL;
  int retval = EXIT_SUCCESS;
  if (argc > 1)
    testname = argv[1];
  RUN_TEST(test1)
  RUN_TEST(test2)
  RUN_TEST(test3)
  return retval;
}
