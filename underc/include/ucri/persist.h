/* UnderC object persistance using UCRI
 This of course makes some assumptions about the classes!
 -They must have a default ctor; I'm using XClass::create()
  to generate new instances dynamically.
 -When I see a pointer, I assume it points to a single instance,
  not a whole array. Arrays are quite all right because they have
  a distinct size. 
 -Only pointers to objects and to char make sense currently. 
 This version will stream out pointers to char in full, irrespective
 of whether they really are distinct pointers. Shouldn't be difficult
 to fix this.
*/

typedef map<void *,void *> PtrMap;

class Streamer {
protected:
  void *m_base;
  PtrMap m_ptr_map;
public:
  void set_base(void *base)
  { m_base = base; }

  Streamer()
   : m_base(NULL) {}

  virtual void stream(XEntry* xe) = 0;
  static void init();
  static void stream_in(char *fname, char* var);
  static void out_stream(ostream& out, XEntry* xe);
  static void stream_out(char *fname, char* var);
  void stream_array(XEntry* xe);
  void stream_object(XEntry* xe);
};

class InStreamer: public Streamer {
private:
   istream& m_in;
public:
   InStreamer(istream& in)
     : m_in(in)
   {}
  // override!
   void stream(XEntry* xe);
   void stream_in_container(XType* xt, void* ptr);

};

class OutStreamer: public Streamer {
private:
   ostream& m_out;
public:
   OutStreamer(ostream& out)
     : m_out(out)
   {}
  void stream_out_container(XType* xt, void* ptr);

  // override!
   void stream(XEntry* xe);
};

const int BUFFSIZE = 1024;
char ibuff[BUFFSIZE];

typedef void (*STREAMFN)(void *, Streamer&, XEntry*);
typedef void (*STREAMIN)(void *, void *, Streamer&, XEntry*, int);
typedef int  (*SIZEFN)(void *);

// a number of helper template functions.
// These will allow us to access containers through
// dynamic instantiation.

template <class C>
  int size_of(const C& ls)
  {
    return ls.size();
  }

template <class C>
  void stream_elements(const C& ls, Streamer& out, XEntry* xe)
  {
     C::iterator it = ls.begin(), iend = ls.end();
     for(; it != iend; ++it) {
        xe->set_ptr(& *it);    // entry now points to element
        out.stream(xe);        // which can be streamed out/in
     }
  }

template <class C, class V>
  void stream_elem_in(C& ls, V& val, Streamer& in, XEntry* pe, int n)
  {
   ls.clear();
   for(int i = 0; i < n; i++) {
     in.stream(pe);
     ls.push_back(*(V *)pe->ptr());
   }
  }

XTemplateFun* s_stream_in, *s_stream_elements, *s_size_of;

void Streamer::stream_array(XEntry* xe)
{
  XEntry* be = xe->base_entry(); // bogus entry for any element
  int sz = be->type()->size();   // size of element type
  int n  = xe->size();           // number of elements
  be->set_data(xe->data());
  for(int i = 0; i < n; i++) {
    stream(be);                     // stream out element
    be->set_data(be->data() + sz);  // and move to the next
  }
}

void Streamer::stream_object(XClass* pc, void *ptr)
{
  XEntry* ce;
  void *old_base = m_base;
  XEntries xlist;
  XEntries::iterator xli;
  set_base(ptr);
  pc->get_variables(xlist,NON_STATIC | FIELDS);
  for(xli = xlist.begin(); xli != xlist.end(); ++xli) {
       ce = *xli;
       stream(ce);
  }
  set_base(old_base);
}

// two useful functions.

Type* elem_type_of(XType* xt)
{
  return xt->as_class()->template_parm(0);
}

void *instantiate(XTemplateFun* templ, XType* t1, XType* t2 = NULL)
{
 XTList tl;
 tl.push_back(t1);
 if (t2) tl.push_back(t2);
 return templ->instantiate(tl);
}

void OutStreamer::stream_out_container(XType* xt, void* ptr)
{
// for the container types, the element type is the first
// template type parameter. We instantiate the above template
// function stream_elements(), which does the actual streaming.
//
// we need an entry to represent each element
  XEntry* fe = uc_global()->create("",elem_type_of(xt));

// instantiate stream_elements() using the container type,
// if it isn't already instantiated.
  STREAMFN stream_out = instantiate(s_stream_elements,xt);

// we'll also need the size of this container, which also
// has to be done indirectly using a simple template function.
// This size is then streamed out.
  SIZEFN size_fn = instantiate(s_size_of,xt);
  m_out << size_fn(ptr) << endl;

// Can now call stream_elements() with this container
  void *old_base = m_base;
  set_base(0);
  stream_out(ptr,*this,fe);
  set_base(old_base);
}

void InStreamer::stream_in_container(XType* xt, void* ptr)
{
// a similar trick to streaming out, except that the template function
// has two type parameters - the container and the element type. I did
// this to avoid having to specialize for lists, vectors, etc. (Altho
// not such a bad idea, that)
 XType* t_elem = elem_type_of(xt);
 XEntry* fe = uc_global()->create("",t_elem);
 STREAMIN stream_in = instantiate(s_stream_in,xt,t_elem);

 int num_elem;
 m_in >> num_elem;
 m_in.getline(ibuff,BUFFSIZE);
 void *old_base = m_base;
 set_base(0);
 stream_in(ptr,0,*this,fe,num_elem);
 set_base(old_base);
}

// write out an ASCII representation of the entry 'xe'
void OutStreamer::stream(XEntry* xe)
{
  string str;
  void *ptr = xe->ptr(m_base);
  XType *t  = xe->type();
  // char* and char[] are handled slightly differently, since char* is an indirect ref.
  // Note that char[] is not handled by the default array case.
  // We also do char separately because val_as_str() will put the value in single quotes
  if (t->is_char()) {
    if (t->pointer_depth()==1) {
      if (t->is_array()) m_out << (char *)ptr << endl;
      else m_out << *(char **)ptr << endl;
    } else m_out << *(char *)ptr << endl;
  } else
  if (t->is_array()) stream_array(xe);
  else if (! t->is_class()) {
     xe->val_as_str(str,m_base);
     m_out << str << endl;
  } else {
    XClass* pc = t->as_class();
    if (t->is_pointer() || t->is_reference()) {
       ptr = *(void **)ptr;        // deference to get actual ptr!
       if (ptr && pc->has_VMT())
         pc = XClass::get_class_of(ptr);  // _actual_ class of object!
       m_out << ptr << ' ' << pc->name() << endl;  // write out pointer value to stream
       if (m_ptr_map[ptr]) return; // already streamed this one out...
       m_ptr_map[ptr] = ptr;       // flag this as streamed out...
    }
    if (pc->get_template()) {
      // at the moment, we're just streaming out list<> and vector<> containers!
       string name = pc->get_template()->name();
       if (name == "list" || name == "vector") {
           stream_out_container(t,ptr);
       }
    } else
    if (ptr) stream_object(pc,ptr);
  }
}

int array_size(XEntry* xe)
{ return xe->size()/xe->type()->size(); }

void InStreamer::stream(XEntry* xe)
{
 static string s;
 XType *t = xe->type();
 if (t->is_char() && t->pointer_depth()==1) {
     void *ptr = xe->ptr(m_base);
     if (t->is_array())
       m_in.getline((char *)ptr, array_size(xe));
     else {
       m_in.getline(ibuff,BUFFSIZE);
       *(char **)ptr = strdup(ibuff);
     }
     //cout << "char " << ibuff << endl;
 } else
 if (t->is_array()) stream_array(xe);
  else if (! t->is_class()) {
  // read in an ASCII representation
  // (need to do this on a per-line business because
  // char * needs to come in as a line)
     m_in.getline(ibuff,BUFFSIZE);
     xe->str_to_val(ibuff,m_base);
     //cout << "val " << ibuff << endl;
 } else {
    XClass* pc = t->as_class();
    void *ptr = xe->ptr(m_base);
    if (t->is_pointer() || t->is_reference()) {
      void *id,*mptr,*actual_ptr;
      char name[80];
      m_in >> id >> name;
      m_in.getline(ibuff,BUFFSIZE);
      if (id == 0) {
      	*(void **)ptr = 0;
      	return;
      }
      mptr = m_ptr_map[id];
      if (! mptr) {
      // create a new object of this class and note it.
      // we must use the _actual_ class name
        pc = uc_global()->lookup_class(name);
        actual_ptr = pc->create();
        m_ptr_map[id] = actual_ptr;
      } else actual_ptr = mptr;
     // ptr is a reference to a pointer value...
       *(void **)ptr = actual_ptr;
     // if we've already read this object in, get out
       if (mptr) return;
     // ptr becomes the base for the ptr object
       ptr = actual_ptr;
    }
    if (pc->get_template()) {
       string name = pc->get_template()->name();
       if (name == "list" || name == "vector") {
           stream_in_container(t,ptr);
           return;
       }
    }
    stream_object(pc,ptr);
  }
}

void Streamer::init()
{
 XNTable* pglob = uc_global();
 s_size_of = pglob->lookup_template("size_of");
 s_stream_elements = pglob->lookup_template("stream_elements");
 s_stream_in = pglob->lookup_template("stream_elem_in");
}

void Streamer::stream_in(char *fname, char* var)
{
  XEntry *xe = pglob->lookup(var);
  ifstream inf(fname);
  InStreamer ii(inf);
  ii.stream(xe);
 }

void Streamer::out_stream(ostream& out, XEntry* xe)
{
 OutStreamer oo(out);
 oo.stream(xe);
}

void Streamer::stream_out(char *fname, char* var)
{
  let xe = pglob->lookup(var);
  if (fname) out_stream(ofstream(fname),xe);
  else out_stream(cout,xe);
 }

Streamer::init();






