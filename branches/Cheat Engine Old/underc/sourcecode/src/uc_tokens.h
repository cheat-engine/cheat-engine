// input.h

#ifndef __UC_TOKENS_H
#define __UC_TOKENS_H

#include "tokens.h"
typedef int (*RESTOREFN)();

class UCTokenStream: public TokenStream {
private:
    RESTOREFN m_restore, m_openfn; 
    string m_original_file;
public:
    UCTokenStream(char *file) : TokenStream(file), m_restore(NULL),m_openfn(NULL) {}
    static void init();
    bool next_two(char *ts, bool skip = true);
    void set_restore_op(RESTOREFN fn);
    void set_open_op(RESTOREFN fn);
// overrides!
    bool user_cmd(string ppd);
    void do_prompt();
    void on_hash_cmd();
    void handle_builtin_macro(char *tbuff, int id);
    void on_open();
    void on_restore();
    void on_clear(const string& filename, int line);
    void on_add_macro(char *name, PMEntry pme);
    void on_error(const char *msg, bool is_error);
	bool is_interactive_mode();
    int  eval_const_expr(const char* msg);
};
#endif

