#include "export.h"

class EXPORT Simple {
  int m_a;
  double m_b;
public:

  void set(int a, double b) {
    m_a = a; m_b = b;
  }

  Simple(int a=3, double b=6.3) : m_a(a), m_b(b) {} 

  int  a()
 { return m_a; }

  double  b()
  {
   return m_b;
  }
};
