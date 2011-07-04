//*SJD* For the UCW import, we have to override dynamic allocation
// to guarantee the existance of four bytes _before the object_ for
// the UCW VMT.
#include <malloc.h>
#define EXPORT __declspec(dllexport)

typedef void * (*ALLOCATOR)(size_t);
typedef void (*DEALLOCATOR)(void *,size_t);
typedef void *(*VECTOR_ALLOCATOR)(size_t,size_t);

ALLOCATOR mAllocFn;
DEALLOCATOR mDeallocFn;
VECTOR_ALLOCATOR mVectAllocFn;

extern "C" EXPORT
 void _ucdl_set_allocator(ALLOCATOR a, DEALLOCATOR d, VECTOR_ALLOCATOR v)
 {
   mAllocFn = a;
   mDeallocFn = d;
   mVectAllocFn = v;
 }

void *_uc_alloc(int sz)
{
  if (mAllocFn) return mAllocFn(sz);
  else return malloc(sz);
}

void  _uc_free(void *ptr)
{
	if (ptr != 0) {
		if (mDeallocFn) mDeallocFn(ptr,0);
		else free(ptr);
    }
}

void *operator new(size_t sz)
{
 return _uc_alloc(sz);
}

void operator delete(void *ptr)
{
  _uc_free(ptr);
}

void *operator new[] (size_t sz)
{
   return _uc_alloc(sz);
}

void operator delete[] (void *ptr, size_t)
{
   _uc_free(ptr);
}

