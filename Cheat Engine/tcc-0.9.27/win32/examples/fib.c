#include <stdio.h>
#include <stdlib.h>	// atoi()

int fib(n)
{
	if (n <= 2)
		return 1;
	else
		return fib(n-1) + fib(n-2);
}

int main(int argc, char **argv) 
{
	int n;
	if (argc < 2) {
		printf("usage: fib n\n"
			   "Compute nth Fibonacci number\n");
		return 1;
	}
		
	n = atoi(argv[1]);
	printf("fib(%d) = %d\n", n, fib(n));
	return 0;
}
