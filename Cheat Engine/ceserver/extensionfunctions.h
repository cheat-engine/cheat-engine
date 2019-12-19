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

#define EXTCMD_ALLOC                    0
#define EXTCMD_FREE                     1
#define EXTCMD_CREATETHREAD             2
#define EXTCMD_LOADMODULE               3
#define EXTCMD_SPEEDHACK_SETSPEED       4
//#define EXTCMD_CHANGEMEMORYPROTECTION   x


uint64_t ext_alloc(HANDLE hProcess, uint64_t preferedBase, int size);
int ext_free(HANDLE hProcess, uint64_t address, int size);
uint64_t ext_createThread(HANDLE hProcess, uint64_t startaddress, uint64_t parameter);
int ext_speedhack_setSpeed(HANDLE hProcess, float speed);

#endif /* EXTENSIONFUNCTIONS_H_ */
