#include <io.h>

class DirInfo {
   _finddata_t* m_pfd;
public:
  DirInfo(_finddata_t* pfd = NULL)
  : m_pfd(pfd) { }

  char* name()
  { return m_pfd->name; }

  long size()
  { return m_pfd->size; }

  time_t created()
  { return m_pfd->time_create; }

  time_t accessed()
  { return m_pfd->time_access; }

  time_t written()
  { return m_pfd->time_write; }

  bool is_read_only()
  { return m_pfd->attrib & _A_NORMAL; }

  bool is_directory()
  { return m_pfd->attrib & _A_SUBDIR; }

  bool is_archive()
  { return m_pfd->attrib & _A_ARCH; }

   operator char*() { return m_pfd->name; }
 };

class DirPaths {
  _finddata_t m_fd;
  long m_hand;
  bool m_ok; 
  char* m_path;
public:
  void close()
  {  _findclose(m_hand); }

  bool open(char* file = NULL)
  { 
   if (file) m_path = file;
   if (m_hand != 0) close();
   m_hand = _findfirst(m_path,&m_fd);
   m_ok = m_hand != 0;
   return m_ok;
  }

  DirPaths(char* file = NULL)
  {
   m_hand = 0;
   if (file) open(file);
  }

  ~DirPaths()
   { close(); }

  DirInfo info()
  {
    return DirInfo(&m_fd);
  }

  void next()
  {
    m_ok = _findnext(m_hand,&m_fd) == 0;
  } 

  bool ok()
  { return m_ok; }

  class iterator {
    DirPaths* m_dir;
  public:
    void next() {
      m_dir->next();
      if (! m_dir->ok()) m_dir = NULL;
    }
    iterator(DirPaths* dir=0)
     : m_dir(dir) {}
    bool operator!=(const iterator& it)
    { return m_dir != it.m_dir; }
    
    void operator++()      { next(); }
    void operator++(int)   { next(); }
    //char* operator*()    { return m_dir->name(); }
    DirInfo operator*()    { return m_dir->info(); }
  };

  iterator begin()  { /*open();*/ return iterator(this); }
  iterator end()    { return iterator(0);    }

};