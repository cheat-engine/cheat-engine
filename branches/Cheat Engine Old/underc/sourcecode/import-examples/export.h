#ifdef __UNDERC__  
#define EXPORT
#else
#define EXPORT __declspec(dllexport)
#endif

