#include "import2.h"

Base::Base(int _a)
{ a = _a; }

Base::~Base()
{ }

void Base::method1()
{ 
  a = sum(1);
}

int Base::val()
{ return a; }

void Base::set(int _a)
{ a = _a; }

int Base::sum(int x)
{ return a + x; }

Derived::Derived()
{ }

Derived::~Derived()
{ }

int Derived::sum(int sz)
{
  return Base::sum(2*sz);
}

EXPORT Base *return_base()
{ return new Base(); }

EXPORT Base *return_derived()
{ return new Derived(); }
