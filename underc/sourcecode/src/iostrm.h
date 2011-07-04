// IOSTRM.H

#ifndef __IOSTRM_H
# define __IOSTRM_H

typedef char *PChar;
typedef void *handle;
// compatibility...

#ifndef NULL
#define NULL 0
#endif

#define IO_BUFFSIZE 4096

class strbuff {
//--- the base class merely manages the buffer
 protected:
    char *m_buff;
    int   m_buffsize;
    bool  m_own_buffer;
 public:
     strbuff(int size=IO_BUFFSIZE, char *buff = NULL);
     ~strbuff();
};

enum ISSFlags { ISS_BAD_FLOAT = 1, ISS_CANT_OPEN = 2,
                ISS_BAD_INT = 3 };

// dummy ios class for enumerations....
class ios {
  int dummy;
public:
  enum {app=1,ate=2,in=4,out=8,trunc=16,nocreate=32,binary=64};
  enum seek_dir { beg, cur, end };
};

class iss: public strbuff {
//--- base class for all input streams; 
//----redefine eof() and fetch_line() to create a useful class
   friend void spushback(iss&, char *);
 protected:
    char *m_s;
    char *m_delim;
   char *m_tok_ptr;
    bool m_finished;
 public:
  int  m_flags;  // shdn't be here, but...
   iss(char *buff=NULL);
  virtual ~iss() { }
  void  set_delim(char *ds=NULL); // wuz " \t"
  operator int () { return !m_flags; }
  char *  strtok(char *buff, char *delim);
  bool  grab();
  char* gets();
  virtual long  tellg();
  virtual void  seekg(long posn, int dir=ios::beg);
  virtual bool  eof();
  virtual bool  fetch_line(char *buff, int size);
  int  getline(char *buff, int size);
  iss&  read(char *buff, int size);
};

class ifs: public iss {
//---An input stream connected to a file handle ------
 protected:
    handle m_hand;
    long m_line;
 public:
   ifs(handle hand);
  int  lineno() { return m_line; }

 //---overrided iss methods
  bool  eof();
  bool  fetch_line(char *buff, int n);
  ifs&  read(char *buff, int size);

};

class ifile: public ifs {
//---a version of ifs which manages an actual input file----
 public:
    ifile(const char *f=NULL, int fmode=ios::in);
    ~ifile();
  bool  open(const char *f,  int fmode=ios::in);
  void  close();
  long  tellg();
  void  seekg(long posn, int dir=ios::beg);
  void  rewind();
 };

class istds: public ifs {
//---the standard input stream----
 public:
  istds();
};

class istrs: public iss {
//----a buffered string input stream----
    bool m_initial_pass;
    char *m_next;
 public:
   istrs(char *str);
  bool  fetch_line(char *buff, int n);
};

/////// Output classes //////////////////
class oss: public strbuff {
//--- base class for all output streams
//----redefine write() to create a useful derived class
 //protected:
 public:
    int m_digits, m_radix;
    int m_size;
   char m_outspace;
 public:
   oss(char *buff=NULL);
  virtual ~oss() {}
  void  puts(char *s);
  void  putch(char ch);
  void  flush();
  virtual long  tellp() { return 0; };
  virtual void  seekp(long posn, int dir=ios::beg) { };
  void  set_outspace(char ch=0)
     { m_outspace = ch; }
  virtual int  write(char *buff,int n);
};

class ostr: public oss {
  public:
  ostr(char *s, int sz);
  long  tellp();
  void  seekp(long posn, int dir=ios::beg);
  int write(char *buff, int sz);
};

class ofs: public oss {
  protected:
     handle m_hand;
  public:
    ofs(handle hand=NULL);
    int  write(char *buff,int n);
    void  flush();
};

class ofile: public ofs {
  public:
      ofile(const char *f=NULL, int fmode=ios::out);
      ~ofile();
     bool  open(const char *f,  int fmode=ios::out);
	 long  tellp();
     void  seekp(long posn, int dir=ios::beg);
     void  close();
};

class ostds: public ofs {
  public:
   ostds();
};

class ostderr: public ofs {
 public:
  ostderr();
};


class DbgStrm: public oss {
public:
     int write(char *, int);
};

extern DbgStrm dout;


//// IOSTRM 'Manipulators' ////////////////
//---the idea here is that these are distinct classes
//---which do nothing more than carry a value

class Endl { int dummy; };

struct Manipper {
    int val;
    Manipper(int v=0) { val = v; }
};

#define MANIPPER(name,def)         \
 struct name: public Manipper {    \
    name(int r=def) : Manipper(r) {} \
 }

MANIPPER(Radix,10);
MANIPPER(Prec,6);

////// Standard streams ////////

extern istds _in;
extern ostds _out;
extern ostderr _err;
extern Endl endl;
extern char ends;

/////// EXTRACTION AND INSERSION OPERATORS ON STREAMS ///////

iss& operator >> (iss& is, char *s);
iss& operator >> (iss& is, int& i);
iss& operator >> (iss& is, char& i);
iss& operator >> (iss& is, short& i);
iss& operator >> (iss& is, long& i);
iss& operator >> (iss& is, double& f);
iss& operator >> (iss& is, float& f);
iss& operator >> (iss& is, void*& f);
iss& operator >> (iss& is, unsigned int& i);

oss& operator << (oss& os, char *s);
oss& operator << (oss& os, void *p);
oss& operator << (oss& os, int i);
oss& operator << (oss& os, char ch);
oss& operator << (oss& os, short i);
oss& operator << (oss& os, long i);
oss& operator << (oss& os, double f);
oss& operator << (oss& os, float f);
oss& operator << (oss& os, unsigned long i); 
oss& operator << (oss& os, Endl&);
oss& operator << (oss& os, Radix& R);
oss& operator << (oss& os, Prec& P);

#ifndef NO_IOSTREAM_ALIAS
# define cin _in
# define cout _out
# define cerr _err
# define istream iss
# define ostream oss
# define ofstream ofile
# define ifstream ifile
# define istrstream istrs
# define ostrstream ostr
#endif


#endif

