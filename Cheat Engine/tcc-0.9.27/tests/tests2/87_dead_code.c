/* This checks various ways of dead code inside if statements
   where there are non-obvious ways of how the code is actually
   not dead due to reachable by labels.  */
extern int printf (const char *, ...);
static void kb_wait_1(void)
{
  unsigned long timeout = 2;
  do {
      /* Here the else arm is a statement expression that's supposed
         to be suppressed.  The label inside the while would unsuppress
	 code generation again if not handled correctly.  And that
	 would wreak havoc to the cond-expression because there's no
	 jump-around emitted, the whole statement expression really
	 needs to not generate code (perhaps except useless forward jumps).  */
      (1 ? 
       printf("timeout=%ld\n", timeout) :
       ({
	int i = 1;
	while (1)
	  while (i--)
	    some_label:
	      printf("error\n");
	goto some_label;
	})
      );
      timeout--;
  } while (timeout);
}
int main (void)
{
  int i = 1;
  kb_wait_1();

  /* Simple test of dead code at first sight which isn't actually dead. */
  if (0) {
yeah:
      printf ("yeah\n");
  } else {
      printf ("boo\n");
  }
  if (i--)
    goto yeah;

  /* Some more non-obvious uses where the problems are loops, so that even
     the first loop statements aren't actually dead.  */
  i = 1;
  if (0) {
      while (i--) {
	  printf ("once\n");
enterloop:
	  printf ("twice\n");
      }
  }
  if (i >= 0)
    goto enterloop;

  /* The same with statement expressions.  One might be tempted to
     handle them specially by counting if inside statement exprs and
     not unsuppressing code at loops at all then.
     See kb_wait_1 for the other side of the medal where that wouldn't work.  */
  i = ({
      int j = 1;
      if (0) {
	  while (j--) {
	      printf ("SEonce\n");
    enterexprloop:
	      printf ("SEtwice\n");
	  }
      }
      if (j >= 0)
	goto enterexprloop;
      j; });

  /* The other two loop forms: */
  i = 1;
  if (0) {
      for (i = 1; i--;) {
	  printf ("once2\n");
enterloop2:
	  printf ("twice2\n");
      }
  }
  if (i > 0)
    goto enterloop2;

  i = 1;
  if (0) {
      do {
	  printf ("once3\n");
enterloop3:
	  printf ("twice3\n");
      } while (i--);
  }
  if (i > 0)
    goto enterloop3;

  /* And check that case and default labels have the same effect
     of disabling code suppression.  */
  i = 41;
  switch (i) {
      if (0) {
	  printf ("error\n");
      case 42:
	  printf ("error2\n");
      case 41:
	  printf ("caseok\n");
      }
  }

  i = 41;
  switch (i) {
      if (0) {
	  printf ("error3\n");
      default:
	  printf ("caseok2\n");
	  break;
      case 42:
	  printf ("error4\n");
      }
  }
  return 0;
}
