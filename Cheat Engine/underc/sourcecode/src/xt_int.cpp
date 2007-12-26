/* XT_INT.CPP
 * A simple external interface for UCW
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/
#include "twl.h"
#include "classlib.h"
#include <string.h>
#include <stdlib.h>
#include "ide.h"
#include "errors.h"
#include "common.h"

// in main.cpp
int redirected_eval(char *s, bool do_semi, char *name=NULL, int lineno=0);
int ext_run_program(char *expr,int);

// from lexer.cpp
void uc_macro_subst(const char *str, char *buff, int sz);

// from tokens.cpp
int _uc_include_path(const char *fname, char* buff, int sz);

const int SYNCH_EXEC = 1981, SYNCH_EXEC_SC = 1982,
          GET_MSG = 1983,  GET_ERR = 1984, GET_FILEPOS = 1985,
          RUN_PROGRAM = 1986, RECONNECT = 1987,
          SINGLE_STEP_PROGRAM = 1988, STEP_OVER_PROGRAM = 1989, 
          EXPAND_MACRO = 1990, SET_FILEPOS = 1991, EXPAND_FILE = 1993;

const int Q2K_CONFIRM=4, Q2K_CONFIRM_RESULT=648, Q2K_MESSAGE=1,
          Q2K_ERROR=2, Q2K_RECONNECT=3, Q2K_BRING_TO_FRONT = 6;
const int MESSAGE_GENERAL = 0, MESSAGE_FINISHED = 2, MESSAGE_BREAKPOINT = 1,
          MESSAGE_ERROR = 3, MESSAGE_SHUT_DOWN = 4;

const int BUFFSIZE = 2048, IBUFFSIZE = 256;

static char buff[BUFFSIZE],ibuff[IBUFFSIZE];
static int n, ret;
static int curr_line = 0;
static char* curr_file = NULL;

class MessageWindow: public TEventWindow {
public:
    MessageWindow() : TEventWindow("yay",NULL) {}


    int handle_user(long id, long extra) 
    {
        switch(id) {
         case SYNCH_EXEC:
         case SYNCH_EXEC_SC:
             get_text(buff,BUFFSIZE);
             ret = redirected_eval(buff,id==SYNCH_EXEC_SC,curr_file,curr_line);             
             curr_file = NULL;
             return ret;
         case RUN_PROGRAM:
             get_text(buff,BUFFSIZE);
             return ext_run_program(buff,extra);
         case RECONNECT:
              IDE::reconnect();
              return Q2K_CONFIRM_RESULT;
         case GET_MSG:
         case GET_ERR:
             set_text(Errors::get_redirect_buffer(id==GET_MSG ? 1 : 2));
             return 1;
         case GET_FILEPOS:
             n = Errors::get_stop_position(buff);
             set_text(buff);
             return n; 
         case EXPAND_MACRO:
         case EXPAND_FILE:
             get_text(ibuff,IBUFFSIZE);
             ret = 1;
             if (id == EXPAND_MACRO)
                uc_macro_subst(ibuff,buff,BUFFSIZE);
             else 
                ret = _uc_include_path(ibuff,buff,BUFFSIZE);
             set_text(buff);
             return ret;
         case SET_FILEPOS:
             get_text(buff,BUFFSIZE);
             curr_file = strdup(buff);
             curr_line = extra;
             return 1;
        }
        return false;
    }
};


static MessageWindow *mw;
static TWin *qw;
static bool gNotBlocking = true;

long IDE::cmd(int cmd, long extra)
{
 if (qw != NULL && gNotBlocking)
   return qw->send_msg(WM_USER_PLUS,cmd,extra);
 else return 0;
}

bool IDE::reconnect()
{
   // *change 1.2.8 we have been asked to reconnect, but there
   // appears to be a perfectly good connection still going.
   // This ensures that UCW will only talk to the first instance
   // of USE that grabbed its attention.
    if (qw && cmd(Q2K_CONFIRM) == Q2K_CONFIRM_RESULT) return true;

    // grab current handle to USE and check whether it's responding
    long hand;
    ifstream ifs;
    if (!ifs.open("c:\\_q2k_.txt")) { qw = NULL; return false; }
    ifs >> hand;
    if (!qw) qw = new TWin();
    qw->set((Handle)hand);
    return cmd(Q2K_CONFIRM) == Q2K_CONFIRM_RESULT;
}

void IDE::initialize()
{
    mw = new MessageWindow();
    ofstream ofs("c:\\_ucws.txt"); 
    ofs << (int)mw->handle() << endl;
    ofs.close();

    if (! reconnect()) {
      cmsg << "Q2K is not available\n";
      qw = NULL;
    } else
      cmd(Q2K_RECONNECT);
}

void IDE::message()
{
 cmd(Q2K_MESSAGE,MESSAGE_GENERAL);
}

void IDE::ucw_shut_down()
{
 cmd(Q2K_MESSAGE,MESSAGE_SHUT_DOWN);
}

void IDE::finished()
{
 cmd(Q2K_MESSAGE,MESSAGE_FINISHED);
}

void IDE::breakpoint()
{
 cmd(Q2K_MESSAGE,MESSAGE_BREAKPOINT);
}

void IDE::error()
{
 if (Parser::debug.errors_as_break)
   cmd(Q2K_MESSAGE,MESSAGE_ERROR);
 else
   cmd(Q2K_ERROR);
}

void IDE::bring_to_front(void* handle)
{
 cmd(Q2K_BRING_TO_FRONT,(int)handle);
}

void IDE::set_blocking(bool yesno)
{ gNotBlocking = ! yesno; }


