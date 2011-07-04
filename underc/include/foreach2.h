// foreach2.h: Implementing FOR_EACH without using typeof

class IterBase {
 public:
   virtual bool next()=0;
};

template <class V, class C>
 class Iter: public IterBase {
 protected:
  V& _ref;
  C::iterator _ci, _ci_end;

 public:
  Iter(V& ref, C& con)
   : _ref(ref), _ci_end(con.end()), _ci(con.begin())
  {  }

  bool next ()
  { 
    if (_ci == _ci_end) return false;
    _ref = *_ci;
    _ci++; 
    return true;
  }

};

template <class C>
 IterBase *_make_iter(C::value_type& ref, C& con)
 {
   return new Iter<C::value_type,C> (ref,con);
 } 

#define FOR_EACH(i,li) \
  for(std::auto_ptr<IterBase> it(_make_iter(i,(li))); \
     it->next();) 
