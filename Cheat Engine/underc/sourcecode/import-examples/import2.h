// import2.h
// testing the import of classes w/ virtual methods

#ifndef __UNDERC__
#define EXPORT __declspec(dllexport)
#else
#define EXPORT
#lib import2.dll
#endif

class EXPORT Base {
protected:
  int a;
public:
  Base(int _a = 0x666);
  ~Base();
  virtual void method1();
  virtual int val();
  virtual void set(int _a);
  virtual int sum(int x);
};

class EXPORT Derived: public Base {
public:
  Derived();
  ~Derived();
  virtual int sum(int);
};

EXPORT Base *return_base();
EXPORT Base *return_derived();

#ifdef __UNDERC__  
#lib 
#endif

