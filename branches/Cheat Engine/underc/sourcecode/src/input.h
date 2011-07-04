// A convenient thin interface to the input stream
#ifndef __INPUT_H
#define __INPUT_H
typedef int (*INTFN) ();
namespace Input {
  int lineno() ;
  string filename() ; 
  bool open(const char *fname) ;
  void clear() ;
  void insert_stream(istream *pos, const char *name, int start_line = 0) ;
  void insert_string(char* str);
  void grab_next_line(char *buff) ;
  char *next_token(bool first=true)  ;
  void set_open_restore(INTFN module_open, INTFN module_close) ;
  PEntry lookup_next_symbol() ;
}
#endif


