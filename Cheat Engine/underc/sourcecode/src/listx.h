/* 
 * UnderC Development Project
 * A simplified implementation of a list container class
 * S.J. Donovan, December 1999.
 * 2nd vs, April 2002.  _Binary_ compatibility with std::list
 */
#ifndef _LISTXXX_H
#define _LISTXXX_H
#ifndef NULL
#define NULL 0
#endif

template <class T>
   class listx
  {
   struct ListEntry {
	   ListEntry *m_last,*m_next;
	   T m_val;
#ifndef __UNDERC__
	   void *operator new(size_t sz)
	   {  return _new_ex(sz); }

       // *fix 1.2.3 prototype for class operator delete was wrong...
       void operator delete(void *ptr/*, size_t sz*/)
	   { _delete_ex((char *)ptr,0/*sz*/); }
#endif
   };
   typedef ListEntry *_PEntry;
   _PEntry m_front, m_back;
   int m_size;
  public:
     class iterator {
	  _PEntry m_cursor;
	public:
	//  typedef T value_type;
	  iterator(_PEntry pe) { m_cursor = pe; }
	  iterator() { m_cursor = NULL; }
	  iterator(const iterator& it) { m_cursor = it.m_cursor; }
      iterator& operator=(const iterator& it)
        { m_cursor = it.m_cursor; return *this; }
	  bool operator == (const iterator& it) { return m_cursor == it.m_cursor; }
	  bool operator != (const iterator& it) { return m_cursor != it.m_cursor; }
	  void cursor(_PEntry pe) { m_cursor = pe; }
	  _PEntry cursor() { return m_cursor; }
	  void next() { m_cursor = m_cursor->m_next; }
	  void prior(){
              m_cursor = m_cursor->m_last;
          }
	  T& operator*()  { return m_cursor->m_val; }
	  T *operator->() { return &m_cursor->m_val; }
	  iterator& operator++()
	   { next(); return *this; }
	  iterator& operator--()
	   { prior(); return *this; }
	   iterator& operator++(int)
	   { next(); return *this; }
	  iterator& operator--(int)
	   { prior(); return *this; }
	 };

     typedef T value_type;
	 
     int size() const { return m_size; }

	 iterator begin() const 
	  { 
	    _PEntry pe = m_front;
		return iterator(pe);
	  }
	 iterator end()    const { return iterator(NULL);    }
	 iterator rbegin() const { return iterator(m_back);  }
	 iterator rend()   const { return iterator(NULL);    }

	 iterator iter(_PEntry pe) { return iterator(pe); }

     private:


	 _PEntry _alloc(const T& val) {
	   _PEntry pe = new ListEntry;
	   pe->m_val = val;
	   pe->m_next = pe->m_last = NULL;
	   return pe;
     }
 	 void _init()  { m_front = m_back = NULL; m_size = 0; }

	 void _insert(_PEntry p1, _PEntry p2, _PEntry p, int sz) { // low-level insert
	   if (! p2) p2 = p1;
   	   m_size += sz;
	   if (!p) { // adding at end....
             if (!m_front) { m_front = p1; m_back = p2; }
	     else {
	        m_back->m_next = p1;  p1->m_last = m_back;
		m_back = p2;
 	     }
	   } else { // insert before cursor!
	     _PEntry last = p->m_last;
	     p2->m_next = p;
             p1->m_last = last; // cd be NULL...
             p->m_last = p2;
	     if (last) last->m_next = p1; else m_front = p1;
	   }
	 }  

  	 void _erase(_PEntry cursor) {
  	     if (cursor->m_last) cursor->m_last->m_next = cursor->m_next;
		               else  m_front = cursor->m_next;
		 if (cursor->m_next) cursor->m_next->m_last = cursor->m_last;
		               else  m_back  = cursor->m_last;
             // if (m_front == m_back) m_back = m_front = NULL;  // empty the list!
             m_size--;
      }	    

     void _swap(_PEntry p1, _PEntry p2) {
	  _erase(p1);  _insert(p1,NULL,p2,1);
     }

    public:

	 void swap(listx& ls) {
          if (ls.size() != 0) 
            puts("yeah");

	  // swap(ls.m_front,m_front);  swap(ls.m_back,m_front); swap(ls.m_size,m_size);
	 }

	 void reverse() {
	  // for(_PEntry le = m_first; le != NULL; le=le->m_next) swap(le->m_last,le->m_next);
	  // swap(m_front,m_back);
         } 
	 
	 void insert(iterator _at, const T& _val) {
	   _PEntry np = _alloc(_val);
	   _insert(np,NULL,_at.cursor(),1);
	 }

	 void splice(iterator _at, listx& _ls) {
	   if (_ls.size() != 0) { 
	    _insert(_ls.m_front, _ls.m_back, _at.cursor(), _ls.size());
	    _ls._init();
           }
         }

	 void splice(iterator _at, listx& _ls, iterator _p) {
	   _PEntry cp = _p.cursor();
	   _insert(cp,cp,_at.cursor(),1);  //*NOT sure about this '1'!!
	   _ls._erase(cp);
     }

	 void erase(iterator _at) {
	     if (_at != end()) { _erase(_at.cursor()); delete _at.cursor(); }
	 }


     // _find is non-standard!
	iterator _find(iterator _is, iterator _ie, const T& _val)
	{
	   for(; _is != _ie; ++_is)
	     if (*_is == _val) return _is;
	   return end();
     	}
 
        iterator _find(const T& val)
        {
          iterator res = _find(begin(),end(),val);
          return res;
         } 

	 void remove(const T& val) { // *fix 1.0.0 used to access cursor after it was destroyed!
	    _PEntry _P,_Q = m_front;
  	    while (_Q) {
               _P = _Q->m_next;
               if (_Q->m_val == val) {
                   _erase(_Q);
                   delete _Q;
               } 
               _Q = _P;
            }
	}

	 void unique() { // removes consecutive _repeated values_
	   if (size() < 2) return;
	   iterator p = begin(), q = begin();
	   ++q;
	   while (q != end()) {
	     if (*p == *q) { erase(q); q = p; }
		 p = q;
		 ++q;
       }
     }



	 void push_back(const T& val) 	 { insert(end(),val); }
	 void push_front(const T& val) 	 { insert(begin(),val); }
	 void pop_back()                 { erase(rbegin()); }  //????
	 void pop_front()                { erase(begin()); }
	 T& front()                      { return m_front->m_val; }
	 T& back()                       { return m_back->m_val; }

         void clear() {
	    _PEntry _P,_Q = m_front;
  	    while (_Q) { _P = _Q->m_next; delete _Q; _Q = _P; }
            _init();
         }

	 void _copy(_PEntry start, _PEntry finis) {
	   for(; start != NULL; start = start->m_next)
	      push_back(start->m_val);
        }

	 listx()             { _init(); }
	 ~listx()            { clear();  }

	 listx(const listx& ls) { _init(); _copy(ls.m_front,ls.m_back); }

     // *fix 1.2.4 Forgot to clear destination list before copying!
	 listx& operator = (const listx& ls) {
        clear();
	   _copy(ls.m_front,ls.m_back);
	    return *this;
      }

	 bool operator == (const listx& ls) const {
	   _PEntry p1 = m_front, p2 = ls.m_front;
	   for(; p1 != NULL && p1->m_val == p2->m_val; p1=p1->m_next) {  
		p2=p2->m_next;
           }
           return p1 == NULL && p2 == NULL;
        }

     bool operator != (const listx& ls) const
	  { return ! (*this == ls); }


  };


#endif
