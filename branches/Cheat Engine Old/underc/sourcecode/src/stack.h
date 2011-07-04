/* STACK.H
 * a simple parametrized stack class
 * Note: we roll our own, rather than use the std implementation, because:
 *   (a) pop() should return a value; _drop()_ doesn't.  
 *   (b) the execution stack needs to operate in quadwords as well as doublewords.
 */

#ifndef __STACK_H
#define __STACK_H
template <class T, int STACKSIZE>
class Stack {
  int  m_cap;
  T m_arr[STACKSIZE];
  int  m_p;  
 public:
  Stack(T t)   { clear(); push(t); m_cap = STACKSIZE; }
  Stack()      { clear(); m_cap = STACKSIZE; }
  void clear() { m_p = -1; }

  Stack& operator = (const Stack& st)
  {
     m_p = st.m_p;
     for(int i = 0; i <= m_p; i++) m_arr[i] = st.m_arr[i];
     return *this;
  }

  bool empty()
  { return m_p == -1; }

  int depth()
  { return m_p+1; }

  int capacity()
  { return m_cap; }

  void push(T v)
    { m_arr[++m_p] = v; }

  void drop()
  { m_p--; }

  T pop()
  { return m_arr[m_p--]; }

  T& TOS()
  { return m_arr[m_p]; }

  T *ref_to_TOS(int depth)
  { return &m_arr[m_p + depth]; }

  void drop(int i)
  { m_p -= i; } 
};
#endif


