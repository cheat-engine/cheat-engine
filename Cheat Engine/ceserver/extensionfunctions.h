/*
 * extensionfunctions.h
 *
 *  Created on: Aug 25, 2013
 *      Author: eric
 */

#ifndef EXTENSIONFUNCTIONS_H_
#define EXTENSIONFUNCTIONS_H_

#include <stdint.h>
#include "ceserver.h"
#include "porthelp.h"

uint64_t ext_alloc(HANDLE hProcess, uint64_t preferedBase, int size);

#endif /* EXTENSIONFUNCTIONS_H_ */
