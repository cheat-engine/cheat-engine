#include <stdlib.h>
#include <stdio.h>

#define N 20

int nb_num;
int tab[N];
int stack_ptr;
int stack_op[N];
int stack_res[60];
int result;

int find(int n, int i1, int a, int b, int op)
{
    int i, j;
    int c;

    if (stack_ptr >= 0) {
        stack_res[3*stack_ptr] = a;
        stack_op[stack_ptr] = op;
        stack_res[3*stack_ptr+1] = b;
        stack_res[3*stack_ptr+2] = n;
        if (n == result)
            return 1;
        tab[i1] = n;
    }

    for(i=0;i<nb_num;i++) {
        for(j=i+1;j<nb_num;j++) {
            a = tab[i];
            b = tab[j];
            if (a != 0 && b != 0) {

                tab[j] = 0;
                stack_ptr++;

                if (find(a + b, i, a, b, '+'))
                    return 1;
                if (find(a - b, i, a, b, '-'))
                    return 1;
                if (find(b - a, i, b, a, '-'))
                    return 1;
                if (find(a * b, i, a, b, '*'))
                    return 1;
                if (b != 0) {
                    c = a / b;
                    if (find(c, i, a, b, '/'))
                        return 1;
                }

                if (a != 0) {
                    c = b / a;
                    if (find(c, i, b, a, '/'))
                        return 1;
                }

                stack_ptr--;
                tab[i] = a;
                tab[j] = b;
            }
        }
    }

    return 0;
}

int main(int argc, char **argv)
{
    int i, res, p;

    if (argc < 3) {
        printf("usage: %s: result numbers...\n"
               "Try to find result from numbers with the 4 basic operations.\n", argv[0]);
        exit(1);
    }

    p = 1;
    result = atoi(argv[p]);
    printf("result=%d\n", result);
    nb_num = 0;
    for(i=p+1;i<argc;i++) {
        tab[nb_num++] = atoi(argv[i]);
    }

    stack_ptr = -1;
    res = find(0, 0, 0, 0, ' ');
    if (res) {
        for(i=0;i<=stack_ptr;i++) {
            printf("%d %c %d = %d\n",
                   stack_res[3*i], stack_op[i],
                   stack_res[3*i+1], stack_res[3*i+2]);
        }
        return 0;
    } else {
        printf("Impossible\n");
        return 1;
    }
}
