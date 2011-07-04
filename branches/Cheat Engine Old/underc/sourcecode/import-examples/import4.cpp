#include "export.h"
#ifndef __UNDERC__
#include "\ucw\include\list"
#endif

struct EXPORT SS {
   int a;
   int b;
};

class EXPORT IntStack {
private:
   std::list<int> m_ls;
public:
   IntStack() { }
   ~IntStack() { }

   void push(int i) { m_ls.push_back(i); }

   int pop() 
   {
    int val = m_ls.back();
    m_ls.pop_back();
    return val;
   }

   void push(std::list<int>& ls)
   {
     std::list<int>::iterator ili;
     for(ili = ls.begin(); ili != ls.end(); ++ili) {
        push(*ili);
     }
   }

};

