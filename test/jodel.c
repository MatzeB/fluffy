#include <stdio.h>

void print_int(int param)
{
	printf("Int: %d\n", param);
}

int get_int()
{
	printf("Int: ");
	fflush(stdout);
	int res;
	scanf("%d", &res);
	return res;
}

void jodel()
{
	switch(rand() % 3) {
	case 0: printf("JHoladaittijo\n"); break;
	case 1: printf("Hodaro\n"); break;
	case 2: printf("Iohodraeho\n"); break;
	}
}
