// UnderC Development project
#ifndef __FOR_EACH_H
#define __FOR_EACH_H

template <class C, class T>
   struct _ForEach {
     typename C::iterator m_it,m_end;
     T& m_var;
    _ForEach(C& c, T& t) : m_var(t)
     { m_it = c.begin(); m_end = c.end(); }

     bool get() { 
       bool res = m_it != m_end;
       if (res) m_var = *m_it;
       return res;
     }

     void next() { ++m_it; }
   };

#define FOR_EACH(v,c) for(_ForEach<typeof(c),typeof(v)> _fe(c,v); \
                          _fe.get();  _fe.next())

#endif
