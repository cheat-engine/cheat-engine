/* uc_graphics.cpp
 * simple (and temporary) graphics API for UCW
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
*/

#include "twl.h"
#include "uc_graphics.h"

class TUCWindow: public TFrameWindow {
public:
   TUCWindow(char *title) : TFrameWindow(title) {}

   void paint(TDC& dc) {
     get_dc()->set_colour(0,0,0);
   }
};

typedef TUCWindow *PWin;

UCWin ucw_create_window(char *title,int x0, int y0, int w, int h)
{
 TWin *old_w = TWin::get_active_window();
 PWin win = new TUCWindow(title);
 win->get_dc()->set_text_align(0,true);
 win->get_dc()->set_colour(0,0,0);
 win->resize(x0,y0,w,h);
 win->show();
 old_w->set_focus();
 return (UCWin)win;
}

void *wcon_window(); // in WCON.CPP

int ucw_title(UCWin win,char *title)
{
 if (! win) PWin(wcon_window())->set_text(title);
 else       PWin(win)->set_text(title);
 return 1;
}

int ucw_size_window(UCWin win,int x0, int y0, int w, int h)
{
 PWin(win)->resize(x0,y0,w,h);
 return 1;
}

//TDC *get_dc(UCWin win)
//{
// return 

int ucw_move_to(UCWin win,int x, int y)
{
 PWin(win)->get_dc()->move_to(x,y);
 return 1;
}

int ucw_line_to(UCWin win,int x, int y)
{
 PWin(win)->get_dc()->line_to(x,y);
 return 1;
}

int ucw_text_out(UCWin win,char *msg)
{
 PWin(win)->get_dc()->draw_text(msg);
 return 1;
}

int ucw_cmd(UCWin win, char *cmd)
{
  PWin pwin = (PWin)win;
  Rect rt;
  pwin->get_client_rect(rt);
  switch(cmd[0]) {
  case 'W': return pwin->width();
  case 'H': return pwin->height();
  case 'w': return rt.right; 
  case 'h': return rt.bottom;
  case 'X': pwin->get_dc()->xor_pen(); return 0;
  case 'F': pwin->show(); return 0;
  case 'T': {
    TWin *p = TWin::get_active_window(); // presumably the console!
    pwin->on_top();  // puts graphix window on top!
    p->set_focus();  // but restore focus to console
    return 0;
  }
  default: return 0;
  }
} 

static TFont *mFont = NULL;

int ucw_font(UCWin win, char *typeface, int size)
{
 //if (mFont) delete mFont;
 mFont = new TFont;
 mFont->set(typeface,size,0 /*flags*/);
 PWin(win)->get_dc()->select(*mFont);
 return 1;
}

unsigned long ucw_rgb(int r, int g, int b)
{
  return RGBI(r,g,b);
}

void ucw_set_colour(UCWin win, unsigned long clr, bool fg)
{
 TDC *pdc = PWin(win)->get_dc();
 if (fg) pdc->set_colour(clr);
    else pdc->set_back_colour(clr);
}

int ucw_fcolour(UCWin win, float r, float g, float b)
{
 PWin(win)->get_dc()->set_colour(r,g,b);
 return 1;
}

int ucw_bcolour(UCWin win, float r, float g, float b)
{
 PWin(win)->get_dc()->set_back_colour(r,g,b);
 return 1;
}

int ucw_rectangle(UCWin win, int x1, int y1, int x2, int y2)
{
 PWin(win)->get_dc()->rectangle(Rect(x1,y1,x2,y2));
 return 1;
}

int ucw_ellipse(UCWin win, int x1, int y1, int x2, int y2)
{
 PWin(win)->get_dc()->ellipse(Rect(x1,y1,x2,y2));
 return 1;
}

