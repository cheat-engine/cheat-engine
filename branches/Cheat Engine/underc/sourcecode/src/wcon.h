// WCON.H
#ifndef __WCON_H
#define __WCON_H
#include <stdio.h>
#include <stdarg.h>
#include "iostrm.h"
void wcon_prompt_char(char ch, int col);
char *wcon_gets(char *buff);
void wcon_puts(char *buff);
void wcon_puttext(char *buff);
int wcon_getch();
int wcon_vprintf(char *fmt,va_list args);
int wcon_printf(char *fmt,...);
int wcon_fprintf(FILE *out, char *fmt,...);
int wcon_scanf(char *fmt,...);
int wcon_fscanf(FILE *in,char *fmt,...);
char *wcon_fgets(char *buff, int sz, FILE *in);
int  wcon_eof();
void wcon_set_title(char *title, bool do_main=false);
void wcon_set_colour(float r, float g, float b);
void wcon_set_size(int x0, int y0, int w, int h); 
void wcon_copy_to_log(char *file);
void wcon_set_console(int idx);
int  wcon_create(int rows, int cols, bool defered = false);
void *wcon_window_handle();
void wcon_destroy(int id);
int  wcon_lines();
bool wcon_paused();
void wcon_pause();
void wcon_resume();
void wcon_clear(int mode); 
void wcon_bring_to_front();

class WConIstream: public iss {
public:
  bool fetch_line(char *buff, int sz); // override
};

class WConOstream: public oss {
private:
 int m_colour;
 oss *m_redirect;
 bool m_main_console;
public:
 WConOstream(float r, float g, float b);
 bool set_redirect(oss *pos, bool main_console = true);
 oss *get_redirect() const;
 int write(char *buff, int n); // override
};

bool wcon_redirect_output(WConOstream& out, oss *pos, bool main_console);
bool wcon_redirect_off(WConOstream& out);
bool wcon_is_redirected(const WConOstream& out);

extern WConIstream wcon_cin;
extern WConOstream wcon_cout, wcon_cerr, wcon_cmsg;
#undef cin
#undef cout
#undef cerr
#define cin wcon_cin
#define cout wcon_cout
#define cerr wcon_cerr
#define cmsg wcon_cmsg

#define printf wcon_printf
#define scanf wcon_scanf
#define gets wcon_gets
#define getch wcon_getch
#define puts wcon_puts
#endif


