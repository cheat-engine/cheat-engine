#include "import5.h"

class EXPORT B: public A {
public:
  virtual int m1() { return 10; }
};

class EXPORT C: public A {
public:
  virtual int m2() { return 20; }
};

class EXPORT D: public A {
public:
  virtual int m1() { return 11;}
  virtual int m2() { return 22;}
  virtual int m3() { return 33;}
};

EXPORT A *make(int type)
{
 switch(type) {
 case 1: return new A(); break;
 case 2: return new B(); break;
 case 3: return new C(); break;
 case 4: return new D(); break;
 }
}
