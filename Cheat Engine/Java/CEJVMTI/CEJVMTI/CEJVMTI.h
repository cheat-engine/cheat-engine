// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the CEJVMTI_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// CEJVMTI_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef CEJVMTI_EXPORTS
#define CEJVMTI_API __declspec(dllexport)
#else
#define CEJVMTI_API __declspec(dllimport)
#endif

// This class is exported from the CEJVMTI.dll
class CEJVMTI_API CCEJVMTI {
public:
	CCEJVMTI(void);
	// TODO: add your methods here.
};

extern CEJVMTI_API int nCEJVMTI;

CEJVMTI_API int fnCEJVMTI(void);
