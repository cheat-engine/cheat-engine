#include "export.h"

class EXPORT A {
public:
 // A() { }
  virtual int m1() { return 1; }
  virtual int m2() { return 2; }
  virtual int m3() { return 3; }
};

EXPORT A *make(int type);
