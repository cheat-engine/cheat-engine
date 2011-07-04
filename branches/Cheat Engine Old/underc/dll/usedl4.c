/* A useful function is uc_init_ref(), which can bind the address
 * of a variable in your program with a UC variable.
 * An expression can be compiled to a UC function, and evaluated
 * using uc_eval_args() or uc_eval_exp().
 */
#include <stdio.h>
#include "ucdl.h"

double alpha = 1.0, beta = 2.0, gamma = 3.0;

int main(int argc, char **argv)
{
 void *exp1, *exp2;
 double result;
 double args[10];
 uc_init(NULL,0);
// *** Bind the global variables to corresp UC ref variables
 uc_init_ref("double","alpha",&alpha);
 uc_init_ref("double","beta",&beta);
 uc_init_ref("double","gamma",&gamma);

// *** We can now initialize this program's variables using
// *** a script file, consisting of statements like 'alpha = 2.3;'
// *** (see the example init-file)
 if (argc > 1) {
    uc_include(argv[1]);
    printf("alpha = %lf beta = %lf gamma = %lf\n",
      alpha, beta, gamma);
 }

// *** uc_compile() works like uc_compile_fn(), except that it
// *** generates a UC function without a wrapper
 exp1 = uc_compile("double x","alpha*sin(x)");
 exp2 = uc_compile("","beta*cos(gamma)");

// *** these functions need to be explicitly executed; 
// *** uc_eval_args() allows an arbitrary number of parameters to
// *** be passed (only in C of course!)
 uc_eval_args(exp1,&result,2.3);
 printf("exp1(2.3) result %lf\n",result);
 
// *** the alternative function is uc_eval_exp(), where the args
// *** are specified as an array. Less convenient, but it will
// *** work with any language.
 args[0] = 1.2;
 uc_eval_exp(exp1,args,&result);
 printf("exp1(1.2) result %lf\n",result);

// *** you can of course rely on the connection between program
// *** variables and UC variables
 beta = 2.4;
 gamma = 6.7;
 uc_eval_exp(exp2,args,&result);  // args is ignored
 printf("exp2() result %lf\n",result);

 uc_finis();
}
