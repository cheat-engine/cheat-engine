/*
pydistorm.c

:[diStorm64}: Python Module Extension
The ultimate disassembler library (80x86, AMD64)
Copyright (C) 2003-2008 Gil Dabah, http://ragestorm.net/distorm/
This library is licensed under the BSD license. See the file COPYING.
*/


#include "decoder.h"
#include "textdefs.h"
#include "wstring.h"

#include "pydistorm.h"

/* PYTHON MODULE EXPORTS */
_DLLEXPORT_ void initdistorm()
{
	PyObject* distormModule = Py_InitModule3("distorm", distormModulebMethods, ":[diStorm64}:");
	PyModule_AddIntConstant(distormModule, "Decode16Bits", Decode16Bits);
	PyModule_AddIntConstant(distormModule, "Decode32Bits", Decode32Bits);
	PyModule_AddIntConstant(distormModule, "Decode64Bits", Decode64Bits);
	PyModule_AddIntConstant(distormModule, "OffsetTypeSize", sizeof(_OffsetType) * 8);
	PyModule_AddStringConstant(distormModule, "info", ":[diStorm64 1.7.30}:\r\nCopyright RageStorm (C) 2008, Gil Dabah \r\n\r\ndiStorm is licensed under the BSD license.\r\nhttp://ragestorm.net/distorm/\r\n");
}

#define MAX_INSTRUCTIONS 1000
PyObject* distorm_Decode(PyObject* pSelf, PyObject* pArgs)
{
	_DecodeType dt;
	uint8_t* code;
	int codeLen;
	_OffsetType codeOffset;
	_DecodeResult res = DECRES_NONE;

	_DecodedInst decodedInstructions[MAX_INSTRUCTIONS];
	unsigned int decodedInstructionsCount = 0, i = 0, next = 0;

	uint8_t instructionText[MAX_TEXT_SIZE*2];

	PyObject *ret = NULL, *pyObj = NULL, *dtObj = NULL;

	pSelf = pSelf; /* UNREFERENCED_PARAMETER */

	/* Decode(int32/64 offset, string code, int type=Decode32Bits) */
	if (!PyArg_ParseTuple(pArgs, _PY_OFF_INT_SIZE_ "s#|O", &codeOffset, &code, &codeLen, &dtObj)) return NULL;

	if (code == NULL) {
		PyErr_SetString(PyExc_IOError, "Error while reading code buffer.");
		return NULL;
	}

	if (codeLen < 0) {
		PyErr_SetString(PyExc_OverflowError, "Code buffer is too long.");
		return NULL;
	}

	/* Default parameter. */
	if (dtObj == NULL) dt = Decode32Bits;
	else if (!PyInt_Check(dtObj)) {
		PyErr_SetString(PyExc_IndexError, "Third parameter must be either Decode16Bits, Decode32Bits or Decode64Bits (integer type).");
		return NULL;
	} else dt = (_DecodeType)PyInt_AsUnsignedLongMask(dtObj);

	if ((dt != Decode16Bits) && (dt != Decode32Bits) && (dt != Decode64Bits)) {
		PyErr_SetString(PyExc_IndexError, "Decoding-type must be either Decode16Bits, Decode32Bits or Decode64Bits.");
		return NULL;
	}

	/* Construct an empty list, which later will be filled with tuples of (offset, size, mnemonic, hex). */
	ret = PyList_New(0);
	if (ret == NULL) {
		PyErr_SetString(PyExc_MemoryError, "Not enough memory to initialize a list.");
		return NULL;
	}

	while (res != DECRES_SUCCESS) {
		res = internal_decode(codeOffset, code, codeLen, dt, decodedInstructions, MAX_INSTRUCTIONS, &decodedInstructionsCount);

		if ((res == DECRES_MEMORYERR) && (decodedInstructionsCount == 0)) break;

		for (i = 0; i < decodedInstructionsCount; i++) {
			if (decodedInstructions[i].mnemonic.pos > 0) {
				memcpy(instructionText, decodedInstructions[i].mnemonic.p, decodedInstructions[i].mnemonic.pos + 1); /* Include \0. */
				if (decodedInstructions[i].operands.pos > 0)
					instructionText[decodedInstructions[i].mnemonic.pos] = SP_CHR;
				memcpy(&instructionText[decodedInstructions[i].mnemonic.pos+1], decodedInstructions[i].operands.p, decodedInstructions[i].operands.pos + 1);
			} else instructionText[0] = '\0';

			pyObj = Py_BuildValue("(" _PY_OFF_INT_SIZE_ "bss)", decodedInstructions[i].offset, decodedInstructions[i].size, instructionText, decodedInstructions[i].instructionHex.p);
			if (pyObj == NULL) {
				Py_DECREF(ret);
				PyErr_SetString(PyExc_MemoryError, "Not enough memory to append an item into the list.");
				return NULL;
			}
			if (PyList_Append(ret, pyObj) == -1) {
				Py_DECREF(pyObj);
				Py_DECREF(ret);
				PyErr_SetString(PyExc_MemoryError, "Not enough memory to append an item into the list.");
				return NULL;
			}
			// V 1.7.25 - Memleak fixed, it is necessary to DECREF the object, because PyList_Append INCREFs it on its own.
			Py_DECREF(pyObj);
		}

		/* Get offset difference. */
		next = (unsigned int)(decodedInstructions[decodedInstructionsCount-1].offset - codeOffset);
		next += decodedInstructions[decodedInstructionsCount-1].size;

		/* Advance ptr and recalc offset. */
		code += next;
		codeLen -= next;
		codeOffset += next;
	}

	return ret;
}

