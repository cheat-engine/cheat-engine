// TOKENSTREAM.H
#ifndef __TOKENS_H
#define __TOKENS_H
#include "classlib.h"
#include "stack.h"

typedef bool (*UserCommand)(string cmd);

enum { T_END, T_TOKEN, T_INT, T_DOUBLE, T_FLOAT, T_HEX, T_CHAR, T_STRING, T_OCT,
       T_NUMBER = T_INT | T_DOUBLE | T_FLOAT | T_HEX | T_OCT,
       T_LAST
 };

// *fix 1.1.4 MAX_MACRO_ARGS was too small for the GDK headers
// *ch 1.2.9 patch Boosted max number of include paths for BeOS port
const int TT_BUFFSIZE = 4096, MAX_FILE_DEPTH = 20, MAX_NEST_IFDEF = 15;
const int LINESIZE = 512, STRSIZE = 1024, MAX_MACRO_ARGS = 16, MAX_INCLUDE_PATHS = 30;
const int MAX_IDEN_SIZE = 32;
const int MAX_PROMPT_SIZE = 80;

struct MEntry {
       int nargs;      // no of macro args - can be zero
	   bool is_alias;  // #alias or #define macro?
       char *subst;    // _massaged_ substitution (cd be NULL)
}; 
typedef MEntry *PMEntry;


struct FileStruct {
    string filename;
    istream *fin;
    int lineno;
    char *save_buff;
    char *save_P;
    string cwd;

    FileStruct () 
    {
        filename = "";
        fin = NULL;
        lineno = 0;
        save_buff = NULL;
    }

    FileStruct (const FileStruct& fs)
        : filename(fs.filename)
    {
      fin = fs.fin;
      lineno = fs.lineno;
      save_buff = fs.save_buff;
      save_P = fs.save_P;
      cwd = fs.cwd;
    }
};

class TokenStream;
typedef void (*PromptFn)(TokenStream&);
typedef unsigned char uchar;

class TokenStream {
private:
    char *start_P, *P, *start, *end_P;
    char buff[TT_BUFFSIZE];
    istream *inf;
    int line, int_type;
    string filename;
    string m_cwd;
    bool m_C_str, m_line_feed,m_expecting_defined;
	uchar m_skip;
    bool in_comment;
    Stack <FileStruct,MAX_FILE_DEPTH> fstack;
    Stack <uchar,MAX_NEST_IFDEF> sstack;
    UserCommand m_cmd;
    PromptFn m_prompter;
  public:
  // static member functions
    static void set_include_dir(const char *s);
    static void macro_builtin(const char *name, int id);
    static void quote_str(char *dest, const char *src);
    static bool macro_delete(const char *name);
	static void macro_subst(const char *name, const char *subst);
    static char* get_prompt_buffer();

   //construction and opening etc
    TokenStream(const char *str=0, UserCommand cmd=NULL);
    ~TokenStream();
    bool open(const char *fname, bool use_include_path=false);
    bool insert_stream(istream *is, const char *name, int start_line = 0, const string& new_cwd = "");
    bool close();
    void clear();

    bool is_interactive();
    void set_str(char *str);
    void insert_string(char *str);
    void discard_line();
    void grab_next();
    bool look_ahead_more(char ch);
    bool fetch_line();
    char *get_upto(char ch, bool discard_ch);
    void grab_line(char *buff);
    bool macro_attempt_process(char*& p, char *out, char *tok);
    void macro_process(PMEntry pme, char *out);
    void alias_process(PMEntry pme);
    bool skip_whitespace();
    void skip_digits();
    int next();
    int look_ahead(bool skipws=false);
    int peek_ahead(int count = 1);
	char *peek_next_token();
    char *get_str(char *tok=NULL);
	char *get_string();
    char *get_token();
    double get_float();
        int    get_int();
    double next_float();
    
    // overrideables
    virtual bool user_cmd(string ppd) { return true; }
    virtual void do_prompt()          {}
	virtual void on_hash_cmd()        {}
    virtual void handle_builtin_macro(char *tbuff,int id) { }
    virtual void on_open()      {}
    virtual void on_restore()   {}
    virtual void on_clear(const string& filename, int line) {}
    virtual void on_add_macro(char *name, PMEntry pme) { }
    virtual void on_error(const char *msg, bool is_error);
    virtual int  eval_const_expr(const char* msg) { return 0; }
	virtual	bool is_interactive_mode() { return false; }

    // inlines....
    
    void set_prompt(PromptFn prompt) { m_prompter = prompt; }
    bool is_in_comment() { return in_comment; }
    void need_line_feed(bool yes) { m_line_feed = yes; }
    void c_string_mode(bool yes) { m_C_str = yes; }
    // *add 1.2.5 Support for defined() within #if expressions
    void expecting_defined(bool yes) { m_expecting_defined = yes; }
 
    // used for bookmarking stream position...
    // be careful - only works w/in same line!
    char *current()        { return P; }
    void  current(char *p) { P = p; }


    char getch()
    { if (*P==0) if(!skip_whitespace()) return 0; 
      return *P++; }

    int lineno()
    { return line; }

    string& file()
    { return filename; }

    void set_skip(uchar do_skip)
    { m_skip = do_skip; }

    uchar get_skip()
    { return m_skip; }

    void pop_skip()
    { m_skip = sstack.pop(); }

    void push_skip()
    { sstack.push(m_skip); }

    bool skip_stack_empty()
    { return sstack.empty(); }
};

#endif

