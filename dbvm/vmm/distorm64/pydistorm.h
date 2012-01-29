/*
pydistorm.h

Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#ifndef PYDISTORM_H
#define PYDISTORM_H

#ifdef SUPPORT_64BIT_OFFSET
/*
 * PyArg_ParseTuple/Py_BuildValue uses a format string in order to parse/build the offset.
 * type: int 64
 */
	#define _PY_OFF_INT_SIZE_ "K"
#else
	#define _PY_OFF_INT_SIZE_ "k"
#endif

#include "decoder.h"

#ifdef __GNUC__
 //#include <python2.5/Python.h>
#elif _MSC_VER
 //#include <python.h>
#endif

PyObject* distorm_Decode(PyObject* pSelf, PyObject* pArgs);

char distorm_Decode_DOCSTR[] =
"Disassemble a given buffer.\r\n"
#ifdef SUPPORT_64BIT_OFFSET
	"Decode(INT64 offset, string code, int type)\r\n"
#else
	"Decode(unsigned long offset, string code, int type)\r\n"
#endif
"type:\r\n"
"	Decode16Bits - 16 bits decoding.\r\n"
"	Decode32Bits - 32 bits decoding.\r\n"
"	Decode64Bits - AMD64 decoding.\r\n"
"Returns a list of tuples of offset, size, mnemonic and hex string.\r\n";

static PyMethodDef distormModulebMethods[] = {
    {"Decode", distorm_Decode, METH_VARARGS, distorm_Decode_DOCSTR},
    {NULL, NULL, 0, NULL}
};

#endif /* PYDISTORM_H */

