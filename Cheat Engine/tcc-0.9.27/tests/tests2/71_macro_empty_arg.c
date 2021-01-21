#include <stdio.h>

#define T(a,b,c) a b c

int main(void)
{
    printf("%d", T(1,+,2) T(+,,) T(,2,*) T(,7,) T(,,));
    return 0;
}
