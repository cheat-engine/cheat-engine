/*
 * symbols.h
 *
 *  Created on: Aug 7, 2013
 *      Author: eric
 */

#ifndef SYMBOLS_H_
#define SYMBOLS_H_

int GetSymbolListFromFile(char *filename, unsigned char **output);
unsigned long long GetModuleSize(char *filename, unsigned long long defaultsize);


#endif /* SYMBOLS_H_ */
