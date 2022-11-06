/*
 * symbols.h
 *
 *  Created on: Aug 7, 2013
 *      Author: eric
 */

#ifndef SYMBOLS_H_
#define SYMBOLS_H_

#include "api.h"

typedef void (*symcallback)(uintptr_t address, char* symbolname, void *context);

int GetSymbolListFromFile(char *filename, unsigned char **output);
unsigned long long GetModuleSize(char *filename, unsigned long long defaultsize);

int FindSymbol(HANDLE hProcess, char *symbolname, symcallback cb, void* context);

#endif /* SYMBOLS_H_ */
