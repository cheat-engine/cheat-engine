// iostrm.cpp
/* a simple (some would say 'toy') iostream library.
 * Its main redeeming feature (apart from being small) is
 * that it is very tolerant of input fields, and one can
 * set the delimiters used to extract these fields.
 * Also, by default, it leaves spaces between output
 * fields.
 *
 * 2nd vs.
  - ifile now supports tellg() and seekg().  There is a convenient rewind()
    method, and eof() does not depend on a sticky bit as in istream.
    (Besides, the BC4.5 implementation of fstream is BUGGY!!)
  - By means of a custom iss strtok() method, we can open more than one
    stream.
  - One can always ask for the line number.

 * 3rd vs. (21/11/98)
  - unless NO_IOSTRM_ALIAS is defined, it maps these class names onto the 
    more familiar 'true' iostream names.  Also, we default to just using
    whitespace as field delimiter, and no extra output space after fields.
    To be compatible with the iostream way of checking for errors, there's
    a conversion-to-int operator which gives the internal error flags.
*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "iostrm.h"

// *fix 1.2.4 Why was this turned on AT ALL?
//#define BIT16

// strbuff class - buffer management.
 strbuff::strbuff(int size, char *buff)
{
 if (buff) {
  m_buff = buff;
  m_own_buffer = false;
  m_buffsize = strlen(m_buff);
 } else {
  m_buff = new char[size];
  m_own_buffer = true;
  m_buffsize = size;
 }
}

 strbuff::~strbuff()
{
  //*TEMP*if (m_own_buffer) { delete m_buff; m_own_buffer = false; }
}

// iss class
// general input stream
// the strategy here is to use the string tokenizing
// functions to pull in delimited chunks.
 iss::iss(char *buff) : strbuff(IO_BUFFSIZE,buff)
{
  m_s = NULL;
  m_finished = false;
  m_flags = 0;
  set_delim(); // use default
}

void  iss::set_delim(char *ds)
{
  m_delim = ds;
}

bool  iss::eof()
// usually will be overriden, but istrs uses the
// m_finished flag.
 { return m_finished; }

bool  iss::fetch_line(char *, int)
 { return false; }

long  iss::tellg()
{ return 0; }

void  iss::seekg(long, int)
{ }

char *  iss::strtok(char *buff, char *delim)
// our private version of strtok() allows multiple (and thread-safe)
// use by many input streams.  Like the library function, it will stuff
// up the buffer as it goes along.
{

  if (buff) m_tok_ptr = buff; // first call

  char *P = m_tok_ptr, *Result;

  if (delim)
  {
  // skip characters in delim - we will return the first non-delim char
     while (*P && strchr(delim,*P)) P++;
     Result = P;
     if (! *P) return NULL; // ran out of string
    // skip until we hit characters in delim again (or end of string!)
     while (! strchr(delim,*P)) P++;
  }
  else
  {
    //  plain whitespace edition using the ctype.h macros
     while (isspace(*P)) P++;
     Result = P;
     if (! *P) return NULL;
     while (*P && ! isspace(*P)) P++;
  }
  if (*P) {
     *P = '\0';
     m_tok_ptr = P+1;
  }
  else m_tok_ptr = P;

  return Result;
}

int  iss::getline(char *buff, int size)
{
  if(!m_s) { 
    if (!fetch_line(m_buff,m_buffsize)) {
        *buff = '\0';
        return 0; // EOF!!
    }
    m_tok_ptr = m_buff;
  }
  strncpy(buff,m_tok_ptr,size);
  m_s = NULL;
  return strlen(buff);
}

bool  iss::grab()
{
top:
 if (fetch_line(m_buff,m_buffsize)) {
     m_s = strtok(m_buff,m_delim);
     if (!m_s) goto top;
     return true;
 } else {
    m_s = " "; // to prevent conversions choking..
    return false;
 }
}

char *  iss::gets()
{
 if (!m_s) { grab(); return m_s; }
 m_s = strtok(NULL,m_delim);
 if (!m_s) grab();
 return m_s;
}


void spushback(iss& is, char *)
{
  for(char *p = is.m_tok_ptr; p != is.m_buff; p--)
      if (*p == 0) *p = ' ';
  is.m_tok_ptr = is.m_buff;
}


//int iss:read(void *buff, int n)
//{
//}

// IFS class
// general file input stream
 ifs::ifs(handle hand) : iss()
{
 m_hand = hand;
 m_line = 0;
}

bool  ifs::eof()
{
  return feof((FILE *)m_hand) ? true: false;
}

bool  ifs::fetch_line(char *buff, int n)
{
  char *t = fgets(buff,n,(FILE *)m_hand);
  m_line++;
  int endi = strlen(buff)-1;
  if (buff[endi] == '\n') buff[endi] = '\0'; // lop off '\n'
  return t? true : false;
}

ifs&  ifs::read(char *buff, int size)
{
   fread(buff,1,size,(FILE *)m_hand);
   return *this;
}

/// Ifile class
// open an input file stream
 ifile::ifile(const char *f, int fmode) : ifs(NULL)
{
  if (f) open(f,fmode);
}

 ifile::~ifile()
{
  close();
}

bool  ifile::open(const char *f, int fmode)
{
 char ftype[4] = {'r',0,0,0};
 if (fmode == ios::binary) ftype[1] = 'b';
 if (m_hand) close();
 m_line = 0;
 m_hand = fopen(f,ftype);
 if (m_hand) { m_flags = 0; return true;  }
 else { m_flags = ISS_CANT_OPEN; return false;  }
}

void  ifile::close()
{
 if (m_hand) fclose((FILE *)m_hand);
}

long  ifile::tellg()
{
  return ftell((FILE *)m_hand);
}

void  ifile::seekg(long pos, int dir)
{
  fseek((FILE *)m_hand, pos, dir);
}

void  ifile::rewind()
{
  seekg(0,0);
  m_line = 0;
  m_flags = 0; // reset error flags!
}


// ISTRS class
// read from a char string
 istrs::istrs(char *buff): iss(buff)
{
 m_initial_pass = true;
}

bool  istrs::fetch_line(char *buff, int n)
{
 //if (m_initial_pass) m_initial_pass = false;
 //else m_finished = true;
 char *p;
 if (m_finished) return false;
 if (m_initial_pass) { p = m_buff; m_initial_pass = false; }
                 else { m_buff = m_next; p = m_buff;  }
 while (*p != 0 && *p != '\n') p++;
 if (*p != 0) { *p = 0;  m_next = p+1; } 
           else { m_next = p; m_finished = true; }
 return  *p == 0;
}


// ISTDS class
// standard i/o!!
 istds::istds(): ifs((void *)stdin)
{
}

// a general output stream....
 oss::oss(char *buff) : strbuff(IO_BUFFSIZE,buff)
{
 m_digits = 6;
 m_radix = 10;
 m_size = 0;
 m_outspace = 0;
}

void  oss::putch(char ch)
{
  char temp[2];
  temp[0] = ch;
  temp[1] = '\0';
  puts(temp);
}

int  oss::write(char *buff,int n)
{
 return 0;
}

void  oss::puts(char *s)
{
  write(s,strlen(s));
}

void  oss::flush()
{

}

//// ostrstream equivalent
// *We really ought to use the size!*
ostr::ostr(char *str, int)
 : oss(str)
 {
  m_buff[0] = '\0';
 }

int  ostr::write(char *s, int sz)
{
 m_size += sz;
 strcat(m_buff,s);
 if (m_outspace) putch(m_outspace);
 return sz;
}

long  ostr::tellp()
{
 return m_size;
}

void  ostr::seekp(long posn, int dir)
{
 m_size = posn;
 m_buff[posn] = '\0';
}

// class OFS
 ofs::ofs(handle hand): oss()
{
 m_hand = hand;
}

int  ofs::write(char *buff, int n)
{
  return fwrite(buff,1,n,(FILE *)m_hand);
}

void  ofs::flush()
{
  fflush((FILE *)m_hand);
}

// file output streaming...
 ofile::ofile (const char *f, int fmode) : ofs()
{
 if (f) open(f,fmode);
}

 ofile::~ofile()
{
 close();
}

 bool ofile::open(const char *f, int fmode)
{
 char ftype[4] = {'w',0,0,0};
 if (m_hand) close();
 if (fmode == ios::binary) ftype[1] = 'b';
 m_hand = fopen(f,ftype);
 return (m_hand)? true : false;
}

 void ofile::close()
{
 if (m_hand) fclose((FILE *)m_hand);
}

long  ofile::tellp()
{
  return ftell((FILE *)m_hand);
}

void  ofile::seekp(long posn, int dir)
{
 fseek((FILE *)m_hand, posn, dir);
}

// standard output
 ostds::ostds() : ofs((void *)stdout)
{
}

// standard error
 ostderr::ostderr() : ofs((void *)stderr)
{
}

istds _in;
ostds _out;
ostderr _err;
Endl endl;
char ends = '\0';

iss& operator >> (iss& is, char *s)
{
  strcpy(s,is.gets());
  return is;
}

// *fix 1.2.4 This operator was missing
iss& operator >> (iss& is, char& i)
{
  sscanf(is.gets(),"%c",&i);
  return is;
}

iss& operator >> (iss& is, int& i)
{
#ifdef BIT16
  short temp;  is >> temp; i = temp;
#else
  long temp;   is >> temp; i = temp;
#endif
  return is;
}

iss& operator >> (iss& is, short& i)
{
 // i = atoi(is.gets());
  if (!sscanf(is.gets(),"%d",&i)) is.m_flags = ISS_BAD_INT;
  return is;
}

iss& operator >> (iss& is, long& i)
{
//  i = atol(is.gets());
  if (!sscanf(is.gets(),"%ld",&i)) is.m_flags = ISS_BAD_INT;
  return is;
}

iss& operator >> (iss& is, double& f)
{
  char *endptr = "t"; // must not be NULL
  f = strtod(is.gets(),&endptr);
  if (*endptr) is.m_flags = ISS_BAD_FLOAT;
  return is;
}

iss& operator >> (iss& is, float& f)
{
  double temp;
  is >> temp;
  f = (float)temp;
  return is;
}

iss& operator >> (iss& is, void*& f)
{
  sscanf(is.gets(),"%x",&f);
  return is;
}

iss& operator >> (iss& is, unsigned int& i)
{
  sscanf(is.gets(),"%u",&i);
  return is;
}

// Insert into output stream....
oss& operator << (oss& os, char *s)
{
 os.puts(s);
 return os;
}
char temp[20];

oss& operator << (oss& os, int i)
{
 _itoa (i,temp,os.m_radix);
 os.puts(temp);
 return os;
}

oss& operator << (oss& os, short i)
{
 return os << (int)i;
}


oss& operator << (oss& os, char ch)
{
 os.putch(ch);
 return os;
}

oss& operator << (oss& os, long i)
{
 _ltoa (i,temp,os.m_radix);
 os.puts(temp);
 return os;
}

oss& operator << (oss& os, double f)
{
 os.puts(_gcvt(f,os.m_digits,temp));
 return os;
}

oss& operator << (oss& os, float f)
{
 os << (double)f;
 return os;
}

oss& operator << (oss& os, void *ptr)
{
 char buff[20];
 sprintf(buff,"%X",ptr);
 os.puts(buff);
 return os;
}

oss& operator << (oss& os, unsigned long i)
{
 ultoa (i,temp,os.m_radix);  // GCC hasn't got ultoa?
 os.puts(temp);
 return os;
}

oss& operator << (oss& os, Endl&)
{
 os.putch('\n');
 os.flush();
 return os;
}

oss& operator << (oss& os, Radix& R)
{
 os.m_radix = R.val;
 return os;
}

oss& operator << (oss& os, Prec& P)
{
 os.m_digits = P.val;
 return os;
}




