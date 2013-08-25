/*
 * server.c
 *
 *  Created on: Aug 25, 2013
 *      Author: eric
 */

#include <stdio.h>

__attribute__((constructor)) void bla(void)
{
  printf("weee\n");
}

