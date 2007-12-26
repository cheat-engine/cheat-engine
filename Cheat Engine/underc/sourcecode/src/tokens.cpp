/* TOKENS.CPP 
 * The TokenStream class (a C/C++ preprocessor/tokenizer)      
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#ifdef _USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#pragma warning(disable:4786)

// disable 'forcing value to bool' (Microsoft specific)
#pragma warning(disable:4800)

#include "tokens.h"
#include "utils.h"
#include <map>

bool tok_dbg = false;  // *TEMPORARRY**

// from SUBST.CPP
char *copy_chars(char* q, char* p);
char *copy_token(char *p, char *tok);
char *massage_subst_string(char *buff, char **args, char *str); 
char *substitute(TokenStream& tok, char *buff, char **actual_args, char *subst);
char *copy_str(char *tok, char *start, char *end);
void insert(char *str, char *ins);
long convert_int(char *buff, int base);

static int mNoIncludePaths = 0;
static string mIncludeDir[MAX_INCLUDE_PATHS];

static char lbuff[LINESIZE];
static char sbuff[STRSIZE];
static char obuff[10];
static char tbuff[STRSIZE];
static char abuff[STRSIZE];

typedef TokenStream& TS;

char *last_line() { return lbuff; }

// note: cmsg is defined in classlib.h as being cout for plain ol console.
void warning(TS tok, string msg, bool is_error = false)
{
   tok.on_error(msg.c_str(),is_error);
}

void fatal_error(TS tok, string msg)
{
    warning(tok,msg,true);
}

//--------------Macro table stuff----------------------

 typedef std::map<string,PMEntry> MacroTable;

 static MacroTable mMacroTable;

 void macro_cleanup()
 {
   mMacroTable.clear();
 }

 static PMEntry macro_lookup(char *name)
 {
   static string nm = "-------------------------------------------";
   nm = name;
   MacroTable::iterator imt = mMacroTable.find(nm);
   return (imt != mMacroTable.end()) ? imt->second : NULL;
 }

 static PMEntry macro_new(const char *name)
 {
   PMEntry pme = new MEntry;
   mMacroTable[name] = pme;
   pme->is_alias = false;  // by default
   return pme;
 }

 bool TokenStream::macro_delete(const char *name)
 { 
   PMEntry pme = mMacroTable[name];
   if (!pme) return false; // wasn't there in the first place!
   delete pme;
   mMacroTable[name] = NULL;
   return true;
 }

 void TokenStream::macro_builtin(const char *name, int id)
 {
    PMEntry pme = macro_new(name); 
    pme->subst = NULL;
    pme->nargs = id;
 }

 void TokenStream::macro_subst(const char *name, const char *subst)
 {
   PMEntry pme = macro_new(name); 
   pme->subst = (char *)subst;
   pme->nargs = 0;
 }


 void TokenStream::quote_str(char *dest, const char *src)
 {
   strcpy(dest,"\"");
   strcat(dest,src); 
   strcat(dest,"\""); 
 }
//-----------------TokenStream class------------------------

TokenStream::TokenStream(const char *fname, UserCommand cmd)
{
    inf = NULL;
    filename = "DUD";
     m_C_str = true;
     m_skip = false;
    in_comment = false; 
    if(fname) open(fname);
    m_cmd = cmd;
    m_prompter = NULL;
    m_line_feed = false;
    m_cwd = "";
}

 static char mPromptBuffer[MAX_PROMPT_SIZE];
 
char* TokenStream::get_prompt_buffer()
{
   return mPromptBuffer;
}

void TokenStream::on_error(const char *msg, bool is_error)
{
  cerr << file() << '(' << lineno() << ") " << msg << endl;
  if (is_error) exit(-1);
}

void TokenStream::set_str(char *str)
{
 *str = 0;  // just in case...
  start = P = start_P = str;
}

TokenStream::~TokenStream()
{
    close();
}

#ifdef _WCON
static WConIstream win;
static istream *con_in = &win;
#else 
static istream *con_in = &cin;
#endif

// *add 1.2.6 exported as uc_include_path
int _uc_include_path(const char *fname, char* buff, int sz)
{
 int knt = 0;
 string path = fname;
 while(! Utils::can_access(path)) {
	 if (knt == mNoIncludePaths) return false;
	 path = mIncludeDir[knt++] + fname;
 }
 strncpy(buff,path.c_str(),sz);
 return true;
}

// *change 1.1.4
// Now supports multiple include paths!
// Normal include will now also check these paths...

bool TokenStream::open(const char *fname, bool system_include)
{
 //..try to allocate and open a file stream
 istream* is = NULL; 
 string path,new_cwd,dir;
 int knt = 0;
 bool true_system_include = system_include;

 while (is == NULL) 
 {
  if (system_include) {
	 path = mIncludeDir[knt++] + fname;
  } else 
     path = fname;

  bool is_relative = Utils::extract_relative_path(path,dir);
  // *change 1.0.0 open() keeps track of working directory
  if (! system_include) {
   if (is_relative && m_cwd.size() != 0) {
     path = m_cwd;
     path += fname; 
   } else
     path = fname;
  } 
  // *fix 1.0.0L 'CON' is of course not a dev name in Linux!
  // *fix 1.2.1   (Eric) Don't create the file if it can't be found!
  if (path == "CON") is = con_in; 
  else is = new ifstream(path.c_str(),IOS_IN_FLAGS); 
  if (!(*is)) {
     delete is; 
     is = NULL;
	 // if we have failed on our first try, keep going...
     system_include = true;	 
  	 // bail out if we have run out of include paths...
	 if (knt == mNoIncludePaths) break;
  }
  // *fix 1.2.8 only append the path part if it isn't absolute
  if (dir.size() != 0) {
	  if (Utils::is_absolute_path(dir)) new_cwd = dir;
	  else  new_cwd = m_cwd + dir;
  }  else new_cwd = "";
 }

 if (is == NULL)
	 fatal_error(*this,"Cannot open " + (true_system_include ? path : fname /*Utils::full_path(path)*/));
  
 return insert_stream(is,path.c_str() /*fname*/,0,new_cwd);
}

static Stack <int,MAX_FILE_DEPTH> mOldSkip;

bool TokenStream::close()
{
    if (inf) {
        if (inf != con_in) delete inf;
        inf = NULL;
    }
    //..pop the filestack!
    if (fstack.empty()) return false;
    FileStruct fs = fstack.pop();
    filename = fs.filename;
    line = fs.lineno;
    inf  = fs.fin;
    m_cwd = fs.cwd;
    if (fs.save_buff != NULL) {
      strcpy(buff,fs.save_buff);      //*LEAK* !!
      P = fs.save_P;
    } else  set_str(buff);

    // any operations that must occur when files are closed...
    // *add 1.2.4 Check that the skip-stack level is the same as it was at entry
    if (mOldSkip.depth() > 0 && mOldSkip.pop() != sstack.depth()) {
        warning(*this,"mismatched #if/#endif");
        set_skip(false);
    }
    on_restore();
    return inf != NULL;
}

void TokenStream::clear()
{
    mOldSkip.clear();
    sstack.clear();
    set_skip(false);
	if (fstack.depth() > 1) close();
	while (fstack.depth() > 1) {
        on_clear(filename.c_str(),line);		
		close();
    }
    set_str(buff); // and clear buffer!
}

bool TokenStream::insert_stream(istream *is, const char *name, int start_line, const string& new_cwd)
{
 if (! is || !(*is) || is->eof()) return false;
//..push our previous state onto the file stack
 FileStruct fs;
 fs.filename = filename;
 fs.fin = inf;
 fs.lineno = line;
 fs.cwd = m_cwd;
 if (start_line > 0) {
   fs.save_buff = strdup(buff);
   fs.save_P = P;
 } else
 fs.save_buff = NULL;
 fstack.push(fs);

 filename = name;
 inf = is;
 line = start_line;
 set_str(buff);
 if (new_cwd.size() > 0) m_cwd = new_cwd;

 // any operations on open - save the initial skip-stack level
 mOldSkip.push(sstack.depth());
 on_open();
 return true;
}

bool TokenStream::is_interactive()
{
  return inf == con_in;   // *change 1.0.0 Should be more efficient
}

int grab_macro_args(TokenStream& tok, char **args)
{
    int nargs = 0;
    char ch = tok.next();
    if (ch != '(') fatal_error(tok,"absurd!");
    ch = tok.next();
    while (ch != ')') {
        if (ch == T_TOKEN) { 
            *args++ = strdup(tok.get_token());
            nargs++;
        }
        else 
        if (ch == T_END) fatal_error(tok,"end of file in macro arguments"); else
        if (ch != ',' && ch != ')') fatal_error(tok,"illegal char in macro argument");
        ch = tok.next();
    }
    *args = NULL;
    return nargs;
}

int grab_actual_args(TokenStream& tok, char **args)
{
    // *fix 1.2.2b (Eric) 1: quoted macro arguments are grabbed verbatim; 2: code cleanup
    char ch, *q = tbuff;
    bool in_quote = false;
    int nargs = 0, level = 1;
    if (tok.next() != '(') return 0;
    while (ch = tok.getch()) {
        if (ch == '"') in_quote = !in_quote;
        if (in_quote) {
            if (ch == '\\' && tok.peek_ahead(0) != 0) {
                // it's escaped - grab two (one below)
                *q++ = ch;
                ch = tok.getch();
            }
        }
        else if (ch == '(') ++level;
        else if (ch == ',' || ch == ')') {
            if (level == 1) {
                // trim off surrounding whitespace
                while (isspace(q[-1])) q--;
                *q = 0;
                q = tbuff;
                while (isspace(q[ 0])) q++;

                // save off the arg and prepare for next time through
                args[nargs++] = strdup(q);
                q = tbuff;

                // either go get the next arg or bail out cause we're done
                if (ch == ',') continue;
                else break;
            }
            if (ch == ')') --level;
        }
        // accumulate the current arg
        *q++ = ch;
    }
    if (ch == 0) fatal_error(tok,"Unterminated macro arg list");
    args[nargs] = NULL;
    return nargs;
}



void expecting_macro_name(TS tok)
{ fatal_error(tok,"expecting macro name"); }

void expecting_string(TS tok)
{ fatal_error(tok,"expecting string"); }

// *add 1.1.0 ensure that the on_hash_cmd() method is always called, however we return!
struct Restore {
  TokenStream& tok;
  Restore(TokenStream& t) : tok(t) { }
 ~Restore() {
    tok.on_hash_cmd();
 }
};

const uchar NO_SKIP = 0,     // prepro is not skipping statements
            SKIP = 1,        // it is skipping normally
			BLOCK_SKIP = 2,  // finished with a #if/elsif/../endif
			NESTED_SKIP = 3; // we are within a block which is skipping

static uchar do_else(TokenStream& tok)
{
   if (tok.skip_stack_empty()) fatal_error(tok,"misplaced #else/#elif");
   uchar skipping = tok.get_skip();

   if (skipping == NO_SKIP || skipping == SKIP)
      tok.set_skip(skipping==SKIP ? NO_SKIP : BLOCK_SKIP);
   return tok.get_skip();
}

bool do_prepro_directive(TokenStream& tok)
{
    Restore after(tok);
    string ppd,path;
    // *add 1.2.6 null directive - also ignores shell script (e.g. #!/bin/ucc -f....)
    char ch = *tok.current();
    if (ch == '\0' || ch == '!') return true;
	// fetch the directive name
    int t = tok.next();    
    if (t != T_TOKEN) {
       fatal_error(tok,"expecting preprocessor directive");
       return true;
    }
    ppd = tok.get_token();
	uchar skipping = tok.get_skip();
    if (ppd == "include") {
		bool is_sys_include = false;
        if (skipping) return true;
        t = tok.next();
        if (t == '<') {
            char *str  = tok.get_upto('>',true);
            if (!str) fatal_error(tok,"expecting '>' in #include");
			is_sys_include = true;
			path = str;
        }
        else path = tok.get_str(tbuff);
        tok.open(path.c_str(),is_sys_include);
    } else 
    if (ppd == "define" || ppd == "alias") {
        char *subst, *sym;
        PMEntry pme;
        int nargs;
        char *args[MAX_MACRO_ARGS];

        if (skipping) return true;
        t = tok.next();
        if (t != T_TOKEN) expecting_macro_name(tok);
        sym = tok.get_str(tbuff);
        pme = macro_lookup(sym);         
        if (pme) warning(tok,"redefining " + string(sym));
        pme = macro_new(sym);
		pme->is_alias = ppd == "alias";
        tok.on_add_macro(sym,pme);
        if (tok.look_ahead() == '(') {
                nargs = grab_macro_args(tok,args);
        } else nargs = 0;
        pme->nargs = nargs;
        subst = tok.get_upto(0,false);
        if (nargs > 0) {
             massage_subst_string(abuff,args,subst);
             pme->subst = strdup(abuff);
        } else  pme->subst = strdup(subst);
    } else
    if (ppd == "ifdef" || ppd == "ifndef" || ppd == "if" || ppd == "elif") {
      if (ppd == "elif") skipping = do_else(tok); // *add 1.2.6 implement #elif
      else tok.push_skip();	  
	  //
	  if (ppd == "if" || ppd == "elif") { // *add 1.2.5 implement #if  
          char* line = tok.get_upto(0,false);
          if (! line) fatal_error(tok,"expecting #if expression");
          if (! skipping) {  // when we are not skipping, then we can change the skip state!
            tok.c_string_mode(true);
            tok.expecting_defined(true);
            int res = tok.eval_const_expr(line);
            tok.c_string_mode(false);
            tok.expecting_defined(false);
		    tok.set_skip(res ? NO_SKIP : SKIP);
          } else
		  if (ppd == "if") tok.set_skip(NESTED_SKIP);
	  } else { // #ifdef, #ifndef
        t = tok.next();
        if (t != T_TOKEN) expecting_macro_name(tok);
        if (! skipping) { 
          bool sym_exists = macro_lookup(tok.get_str(tbuff)) != NULL;
          if(ppd == "ifdef") { if(! sym_exists) tok.set_skip(SKIP); }
                      else   { if(sym_exists)   tok.set_skip(SKIP); }        
        } else tok.set_skip(NESTED_SKIP);
      }
   } else
   if (ppd == "endif") {
      if (tok.skip_stack_empty()) fatal_error(tok,"misplaced #endif");  // throws an exception
      tok.pop_skip();
   } else
   if (ppd == "else") {
       do_else(tok);
   } else
   if (ppd == "undef") {
       if (skipping) return true;
       t = tok.next();
       if (t != T_TOKEN) expecting_macro_name(tok);
       TokenStream::macro_delete(tok.get_str(tbuff));
   } else
       if (ppd == "warning" || ppd == "error") { // *add 1.2.5 #error, 1.2.6 #warning
       if (! skipping) { 
           char* line = tok.get_upto(0,false);
           warning(tok,line, ppd=="error");
       }
   } else { /// an interactive command!
    if (skipping) return true;
    if (!tok.user_cmd(ppd))  return false;  
   }
   return true;
}

void TokenStream::grab_next()
{
// *hack 1.1.4 a cheap & nasty way of looking ahead in the stream....
	inf->getline(lbuff,LINESIZE);
	strcat(buff,lbuff);
}

bool TokenStream::look_ahead_more(char ch)
{
 char *p = P;
 while (true) {
   while (*p && isspace(*p)) p++;
   if (*p) {
     return (*p == ch);
   }
   grab_next();
 }
}

bool TokenStream::fetch_line()
{
 int len;
 bool continuation = true;
 *buff = '\0';
 while (true) { // looking for non-blank, non-preprocessor lines
 set_str(buff);
 do {
     //...if we have run out of file, close and try to continue
     if (!inf || inf->eof()) {
         if(!close()){
            set_str(buff);
            return false;  // we have run out of open files
         // restored old file;  can continue
         }
     }
     do_prompt();   // really only need to call this when in interactive mode...
#ifndef _USE_READLINE
     inf->getline(lbuff,LINESIZE); 
#else
         if (inf != con_in) inf->getline(lbuff,LINESIZE);
         else {
            char *pl = readline(get_prompt_buffer());
            if (pl == NULL) return false; // *fix 1.2.2 (Dean) Stopping process with ctrl-D
            strcpy(lbuff,pl);
            free(pl);
            add_history(lbuff);
         }
#endif
// *fix 1.1.0 spaces after \ messes with the preprocessor's little mind!
     char *endp = lbuff + strlen(lbuff) - 1;
	 while (*endp && isspace(*endp)) --endp;
     continuation = (*endp == '\\'); 
//     if (continuation) *endp = '\0';  // lop off '\'
     if (continuation) { *(endp+1) = '\0';  *endp = '\n'; }
     strcat(buff,lbuff);  
	 line++;
 } while (continuation);

 start = P = start_P = buff; 
 while(isspace(*P)) P++;
 if (*P == 0) continue;  // can ignore empty lines!
 if (*P == '#' && ! in_comment) { 
     m_C_str = false;
     P++;
     try {
       if (!do_prepro_directive(*this)) {
          close(); 
          m_C_str = true;
          return false; // bail out!
       }
     } catch(string msg) {
 // *fix 1.2.7 fatal errors must stop parsing, unless in interactive mode.
 // *fix 1.2.8 true, but that isn't the same as 'inf != con_in'!  
 //  is_interactive_mode() is a virtual function overriden by UCTokenStream.
       if (! is_interactive_mode()) return false;
     }
     m_C_str = true;
 }
 else
 if (! m_skip) {
   // ensure that there's a final line feed!    
    if (m_line_feed) { 
      len = strlen(buff);
      buff[len]   = '$';
      buff[len+1] = '\0';
    }
    return true;
 } 
 } // while looking for non-blank, non-preprocessor lines
}

char *TokenStream::get_upto(char ch, bool discard_ch)
// Grab characters from the stream, upto (and optionally including) ch.
// if anything goes wrong we throw a wobbly.
// *this routine (for now) just works for the prepro*
// *fix 1.2.1 (Peter) Strip out C++ line comments if we're grabbing the whole line
{
  bool found_end;
  start_P = P;
  while(*P && *P != ch) P++;
  found_end = *P == ch;
  if (! discard_ch) P++;
  end_P = P;
  if (discard_ch) P++;
  if (found_end) {
      char *str = get_str(tbuff);
      if (ch=='\0') { // special case of grabbing the whole line
      // *fix 1.2.2 (Eric) strip out any C++ comments properly
      // *fix 1.2.4 Discard any \r found at end of line (DOS files in Linux)
          for (char *p = str; *p; ++p) {
              if ((p[0] == '/' && p[1] == '/') || p[0] == '\r') {
                  *p = '\0';
                  break;
              }
	
          }
      }
      return str;
  }
  else return NULL;
}

void TokenStream::discard_line()
{
 *P = 0; skip_whitespace();
}

// *fix 0.9.3 This was ignoring blank lines & generally messing
//  up the template line number diagnostics.  But it's still
//  giving grief - need to handle the case where there's something
//  still in the input buffer (as is usually the case)
void TokenStream::grab_line(char *buff)
{  
  skip_whitespace();
  start_P = P;
  P += strlen(P);
  end_P = P;
  get_str(buff);
  
  //char lbuff[LINESIZE];
  //inf->getline(lbuff,LINESIZE); 
  //strcpy(buff,lbuff);
 
}

void TokenStream::insert_string(char *str)
// stuff characters into the stream!
// for now, it effectively discards what was _in_ the buffer;
// this is fine for its current application, which is to execute
// commands..
{
 set_str(buff);
 strcpy(P,str);
}

bool no_new_line = false;

bool TokenStream::skip_whitespace()
{
 top:
   while(*P && isspace(*P)) P++;
   if (*P == 0) {
     if(no_new_line || !fetch_line()) return false; // EOF will pass through as T_END
     goto top;
   } else
   if (*P == '/') { //...filter out comments at this level!!
       if (*(P+1)=='/') { *P = '\0'; goto top; } 
       else
       if (*(P+1)=='*') { 
           P++; in_comment = true;
           while (true){ 
               if (*P++=='*' && *P=='/') { P++; in_comment = false; goto top; }
               if (*P == 0) if(!fetch_line()) {
                   fatal_error(*this,"unexpected end of file in comment");
                   return false;
               }
           }
       }
   }
   return true;
}

int grab_alias_args(char *ptr, char **args)
{
 int nargs = 0;
 char *tok = strtok(ptr," "); //Utils::quote_strtok(ptr);
 while (tok != NULL) {
	 *args++ = tok;
	 ++nargs;
	 tok = strtok(NULL," "); //Utils::quote_strtok(NULL);
 }
 return nargs;
}

void separate_alias_commands(TokenStream& tok)
{
// approved way to fetch the whole line
 char *line = strdup(tok.get_upto(0,true));  
 char *cmds[10], buff[80];
 int k = 0;
 // break up into individual @-commands (need to do this separately)
 char *cmd = strtok(line,"@");
 while (cmd) { 
   cmds[k++] = cmd;
   cmd = strtok(NULL,"@");
 }

 // insert the commands back into the stream 
 // shifting the current position is necessary to prevent
 // runaway substition of 'cd' etc.
 for(int i = 0; i < k; i++) {
   sprintf(buff,"# %s",cmds[i]);
   tok.insert_string(buff);
   tok.current(tok.current()+1);
   // *fix 1.2.0 NB to switch off C string mode when calling do_prepro_directive()
   tok.c_string_mode(false);
   try {
     do_prepro_directive(tok);   
   } catch(string msg) { }
   tok.c_string_mode(true);
 }
}

void TokenStream::skip_digits()
{
 while(isdigit(*P)) P++;
}

static bool first_token_in(char *buff, char *P)
{
	if (P == buff) return true;
	P--;
	while (P != buff && isspace(*P)) P--;
	return P == buff && isspace(*P);
}

// *change 1.2.2 I've separated out the macro substitution code from next()
// and broken it into the two cases, C macros and aliases, explicitly.

bool TokenStream::macro_attempt_process(char*& p, char *out, char *tok)
{
   PMEntry pme = macro_lookup(tok);
   *out = '\0';
   if (! pme || pme->is_alias) return false;
   else {
       char *old_P = current();
       current(p);
       macro_process(pme,out);
       p = current();
       current(old_P);
   }
   return true;
}

void TokenStream::macro_process(PMEntry pme, char *out)
{
    char *args[MAX_MACRO_ARGS];
    char temp_buff[TT_BUFFSIZE];
    int nargs;
    char *subst;
    if (pme->nargs > 0) {
       if (!pme->subst) { // Builtin macro
            handle_builtin_macro(tbuff,pme->nargs);
            subst = tbuff;
       } else {
		  nargs = grab_actual_args(*this,args);  // regular C-style macro
          if (nargs != pme->nargs) fatal_error(*this,"wrong no. of arguments for this macro");
          substitute(*this,temp_buff, args, pme->subst);
          subst = temp_buff;
       }    
    } else 
       subst = pme->subst;

    if (! out) insert(P,subst);
          else strcpy(out,subst);
}

void TokenStream::alias_process(PMEntry pme)
{
    char *args[MAX_MACRO_ARGS];
    char temp_buff[TT_BUFFSIZE];
    int nargs;
    if (pme->nargs > 0) {		  
		  nargs = grab_alias_args(P,args);
          set_str(buff); 
          if (nargs != pme->nargs) fatal_error(*this,"wrong no. of arguments for this macro");
          substitute(*this,temp_buff, args, pme->subst);
          insert(P,temp_buff);
    } else 
       insert(P,pme->subst);

	 skip_whitespace();
	 if(*P=='@') {
         ++P;
  	     separate_alias_commands(*this);
		 discard_line();
     }  
}

int TokenStream::next()
{
try {  // *fix 1.01 fatal_error() will throw a string!
do_it_again:
  if (! skip_whitespace()) return 0;  // means: finis, end of file, bail out.
  if (iscsymf(*P)) { //--------------------- TOKENS --------------
     start_P = P;
     while (iscsym(*P)) P++;
     end_P = P;
     copy_str(tbuff,start_P,end_P);
     if (m_C_str) { // ie. suppress macro lookup in preprocessor directives
         PMEntry pme;
         // *add 1.2.4 Support for defined(MACRO) in #if directives
         // *fix 1.2.5 'defined MACRO' is also acceptable
         if (m_expecting_defined && strcmp(tbuff,"defined")==0) {
           m_C_str = false;
           int t = next();
           char mname[MAX_IDEN_SIZE];
           bool ok = (t == '(' || t == T_TOKEN);
           if (ok) {  
             if (t == '(') ok = (next() == T_TOKEN);     // skip the '('
             if (ok) {
               get_str(mname);                             // pick up the macro name
               if (t == '(') next();                       // skip the ')'
               insert(P,(char*)(macro_lookup(mname) ? "1" : "0"));  
               m_C_str = true;
               goto do_it_again;
             }
           } 
           m_C_str = true;
           if (! ok) fatal_error(*this,"defined takes one macro argument");
         } else {
          pme = macro_lookup(tbuff);
          if (pme) {
             if (pme->is_alias) {
                 if (! first_token_in(buff,start_P)) return T_TOKEN;
                 else alias_process(pme);
             } else
             macro_process(pme,NULL);
             goto do_it_again;
          }
          // *fix 1.2.7 in #if expressions, all non-macros evaluate as 0
          else if (m_expecting_defined) {
             insert(P,"0");
             goto do_it_again;
          }
         }
     }
     return T_TOKEN;
  } else
  if (isdigit(*P)  ||  *P == '.' && isdigit(*(P+1))) { //------- NUMBERS ------------------
    int ntype = int_type = T_INT;   // until proved otherwise!
     start_P = P;
     if (*P != '.') {
      if (*P == '0') {
        // actual verification of hex or octal constants must happen in lexer
        if (*(P+1) == 'x') {       // hex constant
          while (isalnum(*P)) P++; // a preliminary check!
          ntype = int_type = T_HEX;
        } else 
  	 if (isdigit(*(P+1))) {      // octal constant
         skip_digits();
         ntype = int_type = T_OCT;
	}
	else skip_digits();         // plain zero!
      } else {
        P++;                        // skip first - might be '-'
        skip_digits();
      }
     }
     if (*P == '.') {               // (opt) fractional part
        P++;
        skip_digits();
        ntype = T_DOUBLE;
     }
     if (*P == 'e' || *P == 'E') { // (opt) exponent part
        P++;
        if (*P == '+' || *P == '-') P++;  // (opt) exp sign
        skip_digits();
        ntype = T_DOUBLE;
     }
     if (*P == 'f' || *P == 'F')      { P++; ntype = T_FLOAT; }
     // *fix 1.2.6 long integer constants ending with 'L' are now acceptable 
     else if (*P == 'l' || *P == 'L') { P++; ntype = T_INT;  }
     end_P = P;
     return ntype;
  } else
  if (*P == '\"' || *P == '\'') { //------------CHAR OR STRING CONSTANT-------
     char ch, endch = *P++;  char *p = sbuff;
     start_P = sbuff;
next_string:
     while (*P && *P != endch) {
         if (*P == '\\' && m_C_str) {
             P++;
             switch(*P) {
             case '\\': ch = '\\'; break;
             case 'n':  ch = '\n'; break;
             case 'r':  ch = '\r'; break;
             case 't':  ch = '\t'; break;
             case 'b':  ch = '\b'; break;
             case '\"': ch = '\"'; break;
             case '\'': ch = '\''; break;
             case '0':  { //..collecting OCTAL constant
                char *start_oct = P;
                skip_digits();  
                copy_str(obuff,start_oct,P);
                ch = (char)convert_int(obuff,8);
                P--;  // leave us on last digit
		} break;
            // *fix 1.1.2 We were not letting non-escape sequences through...
			 default: *p++ = '\\'; ch = *P; break; 
             } // switch
             *p++ = ch; P++;
         } else *p++ = *P++;
     } 
     if (! *P) fatal_error(*this,"Unterminated string constant");
     P++;  // skip the endch
     *p = '\0';
     end_P = p;
// *add 1.1.1 adjacent quoted text
// *fix 1.1.4 gave preprocessor directives the heebies
	 if (endch=='\"') {
	 if (m_C_str) {
           skip_whitespace();
           if (*P == '\"') { 
               P++;                         // will be concatenated!
	       goto next_string;            // so go back & keep grabbing...
	   }
         }
       return  T_STRING; 
     }
     else return T_CHAR;
  } else
  return *P++;
 } catch(...) {
   return ' ';
 }
}

int TokenStream::look_ahead(bool skip_wspace)
//*OPT* Can inline this!
{
  if(skip_wspace) skip_whitespace(); 
  return *P;
}

int TokenStream::peek_ahead(int count)
{
 return *(P+count);
}

char *TokenStream::get_string()
{
	return sbuff;
}

char *TokenStream::peek_next_token()
{
// This is a hack and only use it if you are slowly going mad with frustration.
// It will return an empty buffer if there's no more tokens on the current line.
  char *ptr = P;
  while (*ptr && !iscsym(*ptr)) ptr++;
  char *start_p = ptr;
  if (ptr) {
    while (iscsym(*ptr)) ptr++;
  } 
  copy_str(tbuff,start_p,ptr);
  return tbuff;
}

char *TokenStream::get_str(char *tok)
{
 if (tok==NULL) tok = tbuff; 
 copy_str(tok,start_P,end_P);
 return tok;
}

char *TokenStream::get_token()
{  return tbuff; }

double TokenStream::get_float()
{
 char buff[20];
 return atof(get_str(buff));
}

int TokenStream::get_int()
{
 char buff[20];
 return convert_int(get_str(buff),int_type == T_INT ? 10 : 16);
} 

double TokenStream::next_float()
{
 int t;
 do {
  t = next();
  if (t == T_NUMBER || t == T_END) return get_float();
 } while (t != T_END);
 return 0.0;
}

void TokenStream::set_include_dir(const char *s)
{
// *fix 1.2.2 Check for too many include paths
    if (mNoIncludePaths+1 >= MAX_INCLUDE_PATHS) {
        cerr << "Out of room for include paths!" << endl;
        return;
    }
    string path = s;
    Utils::check_path_end(path);
    mIncludeDir[mNoIncludePaths] = path;
    ++mNoIncludePaths;
}


