/*
 * extentionloader.h
 *
 *  Created on: Aug 19, 2013
 *      Author: eric
 */

#ifndef EXTENTIONLOADER_H_
#define EXTENTIONLOADER_H_

#include <stdlib.h>
#include "porthelp.h"

int loadCEServerExtension(HANDLE hProcess);
uint64_t allocWithoutExtension(HANDLE hProcess, void *addr, size_t length, int prot);

#endif /* EXTENTIONLOADER_H_ */
