/* utils.h
 * Utilities for working with file path and changing directories
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */
#include "classlib.h"
#include <ctype.h>
#include <string.h>
#include <algorithm>
#ifdef _WIN32
#include <direct.h>
#include <io.h>
const char DIR_SEP = '\\';
const char ARG_QUOTE = '"';
#else
#include <unistd.h>
#include <stdio.h>
const char DIR_SEP = '/';
const char ARG_QUOTE = '\'';
#endif

#include "utils.h"

const int PATHSIZE = 256;
const int MAX_ARGS = 30;
typedef char *pchar;


#ifdef _WCON
// then it's already in TWL.CPP
char *_quote_strtok(char *str, char str_delim);
#else
char *_quote_strtok(char *str, char str_delim)
{
// a specialized version of strtok() which treats quoted strings specially 
// (used for handling command-line parms)
    static char *tok;
    if(str != NULL) tok = str;
          
    while (*tok && isspace(*tok)) tok++;
    if (*tok == '\0') return NULL;
    
    if (*tok == str_delim) {       
       tok++;            // skip "
       str = tok;
       while (*tok && *tok != str_delim) tok++;        
    } else {
       str = tok;
       while (*tok && ! isspace(*tok)) tok++;
    }
    if (*tok) *tok++ = '\0'; // *fix 1.2.0 watch out for the _last_ token!
    return str;
}
#endif

char *
Utils::quote_strtok(char *str)
{ return _quote_strtok(str,ARG_QUOTE); }


bool 
Utils::is_qualified_path(const char *path)
{
  return strchr(path,DIR_SEP) != NULL; 
}

static bool next_two(string& s, int idx, char c1, char c2)
{
 return s[idx] == c1 && s[idx+1] == c2;
}

bool Utils::is_absolute_path(string& path)
{
   return path[0] == DIR_SEP 
#ifdef _WIN32
                  || next_two(path,1,':',DIR_SEP) 
#endif
 ;
}


// this returns true if:
// (1) the path is just a filename (e.g test.c)
// (2) the path is relative (e.g. fred\test.c)
// Otherwise, it is a fullpath and it returns false.
// it will always extract the path bit, if present.
bool
Utils::extract_relative_path(string& path, string& dir)
{
// It's nice to be able to work with '/' under Win32...
#ifdef _WIN32
 std::replace(path.begin(),path.end(),'/','\\');
#endif
 bool has_dir_sep = is_qualified_path(path.c_str());
 bool full_path = is_absolute_path(path);

 // if NOT a plain filename, extract path
 if (has_dir_sep && !next_two(path,0,'.',DIR_SEP)) {
      int i;
      for(i = path.size()-1; i > 0 ; i--)
        if (path[i] == DIR_SEP) break;
      dir = path.substr(0,i+1);  
      return ! full_path;   
 }
 else {
    dir = "";
    return true;
 }
}

string 
Utils::full_path(string s)
{
 string file = s, pth;
 if (Utils::extract_relative_path(file,pth)) {
   string dir = Utils::get_curr_dir(); 
   Utils::check_path_end(dir);
   return dir + s;
 } else return s;
}

void 
Utils::strip_last(char *path)
{
  char *p = path + strlen(path);
  while (*p != DIR_SEP) p--;    
  *p = '\0';
}

void 
Utils::check_path_end(string& s)
{
  if (s[s.length()-1] != DIR_SEP) s += DIR_SEP;
}

string 
Utils::file_extension(string name)
{
  int k  = name.find(".");
  if (k != -1) return name.substr(k);
  else return "";
}


string
Utils::get_filepart(string s, bool strip_extension)
{
 int pos = s.rfind(DIR_SEP);
 if (pos != -1)   
   s = s.substr(pos+1);  //*fix 1.2.9a added '+1'
 if (strip_extension) {
     pos = s.rfind('.');
     s = s.substr(0,pos);
 }
 return s;
}

char *
Utils::get_curr_dir()
{
   static char buff[PATHSIZE];
   getcwd(buff,PATHSIZE);
   return buff;
}

void
Utils::change_dir(char *dir)
{
   chdir((const char *)dir);  // GCC requires the cast...
}

bool

#ifndef _WIN32
#define _access access
#endif

Utils::can_access(string path)
{
 return _access(path.c_str(),0) != -1;
}

// *change 1.2.2 An improved command-line arg parser

using namespace Utils;

Args::Args(int& argc, char** argv)
  : m_argc(argc), m_argv(argv),m_last_idx(1),m_stop_after_file(false)
  {}

 
 bool Args::delete_arg(int idx)
 {
   if (idx >= m_argc || m_argc < 2) return false;
   m_argc--;
   for(int i = idx; i < m_argc; i++)
     m_argv[i] = m_argv[i+1];
   return true;
 }

 bool Args::get_opt(char *opt)
 {
    if (m_last_idx == m_argc) return false;
    for(int idx = m_last_idx; idx < m_argc; idx++) {
       char* p = m_argv[idx];
       if (p && *p == '-') {
         *opt = p[1];
         m_optstr = p+2;
         delete_arg(idx);
         m_last_idx = idx;
         return true;
       } else if (m_stop_after_file) return false;
     }
     m_last_idx = m_argc;
     return false;
  }

 char* Args::get_opt_parameter()
 {
    if (*m_optstr == '\0') {  
      m_optstr = m_argv[m_last_idx];
      delete_arg(m_last_idx);
    }
    return m_optstr;
 }

 // *change 1.2.6 this method now _inserts_ the extra stuff in
 // the begining, so we can stop scanning for options
 // at the first non-option.
 char** Args::append_extra_args(char *arg_str)
 {
   char *xargs[MAX_ARGS];
   int i,k,nxargs = 0;
   char *tok = Utils::quote_strtok(arg_str);
   while (tok) {
	 xargs[nxargs++] = tok;
	 tok = Utils::quote_strtok(NULL);
   }
   int nnew = nxargs + m_argc;
   char **pnew_args = new pchar[nnew+1];
   pnew_args[0] = m_argv[0];           // copy program name to new argv
   for (i = 1; i <= nxargs; i++)
       pnew_args[i] = xargs[i-1];
   for (i = nxargs+1, k = 1; i < nnew; i++,k++)
       pnew_args[i] = m_argv[k];

   pnew_args[nnew] = NULL;
   m_argv = pnew_args;
   m_argc = nnew;
 return m_argv;
}

