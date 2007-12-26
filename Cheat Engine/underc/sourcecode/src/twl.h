/* TWL.H
 * Tiny Windows Library vs 1.0
 * Steve Donovan, Miningtek 1998
 * based on SWL, 1995.
*/
#ifndef __TWL_H
#define __TWL_H

//#include "twl-defs.h"
typedef char *pchar;
enum { NORMAL, BOLD=2, ITALIC=4 };


#ifndef _WINDOWS_
// some constants are fairly essential...
#define WM_COMMAND  0x0111
#define MB_OK
#define SW_SHOW 5
#define WS_CHILD    0x40000000L
#define WS_VISIBLE  0x10000000L
#define CHILD_VISIBLE (WS_CHILD | WS_VISIBLE)
#define NULL 0
struct POINT { int x,y; };
#else
//#define Rect RECT
//#define Point POINT
//#define dword DWORD
#endif

#define WM_USER_PLUS 0x9999

#define EXPORT __declspec(dllexport)
#define DLGFN EXPORT BOOL __stdcall
#define WNDFN EXPORT long __stdcall
typedef void *Handle;

struct Rect {
  int left,top,right,bottom;
  Rect() { }
  Rect(int x0, int y0, int x1, int y1) 
   { left = x0; top = y0; right = x1;  bottom = y1; }
};
struct Point {
  int x,y;
  Point() { }
  Point(int xp, int yp) { x = xp; y = yp; }
};
typedef unsigned long dword;

#ifndef _WIN32
#define bool int
#define true 1
#define false 0
#endif

#define BUFSIZE 120
#define ID_ABOUT    103

long RGBF(float r, float g, float b);
long RGBI(int r, int g, int b); 

extern char obuff[BUFSIZE];

const int TOPLEFT = 1, MSG_ERROR=2,MSG_WARNING=1;

class TWin {
// a basic wrapper for a HWND
public:
  TWin ()
    {set(NULL);}
  TWin (Handle hwnd)
    {set(hwnd);}
  void set(Handle hwnd) 
    { m_hwnd = hwnd; }
  Handle handle() 
   { return m_hwnd; }

  TWin(TWin *parent, pchar winclss, pchar text, int id, dword styleEx=0);
  TWin *create_child(pchar winclss, pchar text, int id, dword styleEx=0);

  virtual void update();
  void  invalidate(Rect *lprt=NULL);
  void  get_rect(Rect &rt);
  void  get_client_rect(Rect &rt);
  Point to_screen(Point pt);
  Point to_client(Point pt);
  int   width();
  int   height();
  void  set_text(pchar str);
  pchar get_text(pchar str=obuff,int sz=BUFSIZE);
  void  set_text(int id, pchar str);
  pchar get_text(int id, pchar str=obuff, int sz=BUFSIZE);
  void  set_int(int id, int val);
  int   get_int(int id);
  TWin *get_twin(int id);
  static TWin *get_active_window();
  int  get_id();
  void set_focus();
  void  resize(int x0, int y0, int w, int h);
  void  resize(int w, int h);
  void  move(int x0, int y0);
  void  on_top();  // *add 0.9.4
  void  show(int how = SW_SHOW);
  bool  is_visible();
  void  set_style(dword s);
  void  scroll(int dx, int dy);
  int   send_msg(int msg, int wparam=0, int lparam=0) const;
  int   post_msg(int msg, int wparam=0, int lparam=0) const;
  int   send_cmd(int id,  int lparam=0)
    { return send_msg(WM_COMMAND,id,lparam); }
  void  close();
  int message(char *msg, int type=0);
protected:
  Handle m_hwnd;
};

//int message_box(pchar txt, pchar title, int style=MB_OK);
int Message(char *format);
long current_time();

class TGDIObj {
  public:
     TGDIObj() { m_hand = NULL; };
     //~TGDIObj();
     void  set_handle(Handle hand)
      { m_hand = hand; }
     Handle handle()
      { return m_hand; }
    void destroy();
  protected:
    Handle m_hand;
};

class TFont: public TGDIObj {
  public:
   TFont();
   ~TFont();
   void create();
  // TFont& operator = (pchar spec);
   TFont& operator = (const TFont& f);
    void  set(pchar name, int sz, int ftype=NORMAL);
    void  set_name(pchar name);
    void  set_size(int pts);
    void  set_bold();
    void  set_italic();
protected:
   void *m_pfont;
};


///// Wrapping up the Windows Device Context
class TDC {
 public:
  TDC();
  ~TDC();
  void   set_hdc(Handle hdc)
  { m_hdc = hdc; }
  Handle get_hdc()
  { return m_hdc; }
  void set_twin(TWin *win)
   { m_twin = win; }

  
  void get(TWin *pw=NULL);
  void release(TWin *pw=NULL);
  void kill();
  Handle select(Handle obj);
  Handle select(TGDIObj& obj)
    { return select(obj.handle()); }   
  void select_stock(int val);

  void xor_pen();
  void to_logical(Point *pt, int sz);
  void to_device(Point *pt, int sz);
  void to_logical(Point& pt)
   { to_logical(&pt,1); }
  void to_device(Point& pt)
   { to_device(&pt,1); }
  void to_logical(Rect& rt)
   { to_logical((Point *)&rt,2); }
  void to_device(Rect& rt)
   { to_device((Point *)&rt,2); }

  // this changes both the _pen_ and the _text_ colour
  void set_colour(float r, float g, float b);
  // ditto, does brush and background colour
  void set_back_colour(float r, float g, float b);

  void set_colour(long colour);
  void set_back_colour(long colour);

  void set_text_colour(long colour);
  // wrappers around common graphics calls
  void set_text_align(int flags, bool update_cp = false);
  void get_text_extent(pchar text, int& w, int& h,TFont *font=NULL);
  void draw_text(pchar msg);
  void text_out(int x, int y, char *buff, int sz = -1);
  void move_to(int x, int y);
  void line_to(int x, int y);
  void rectangle(const Rect& rt);
  void ellipse(const Rect& rt);
  void invert_rect(const Rect& rt);
 protected:
    Handle m_hdc, m_pen, m_font, m_brush;
    int m_flags;
    TWin *m_twin;
 };

class TEventWindow; // forward
class TClientDC: public TDC {
public:
  TClientDC(TEventWindow *ew);
  ~TClientDC();
};

class AbstractMessageHandler;

class TScrollBar: public TWin {
private:
  int m_type;
public:
    // these corresp to SB_LINEUP, etc
     enum { LINEUP=0,LINEDOWN=1,PAGEUP=2,PAGEDOWN=3,POS=4,TRACK=5 };
     TScrollBar(Handle hwnd, int type);
     void set_range(int rmin, int rmax, bool repaint=false);
     void set_pos(int rpos, bool repaint=false);
     int  get_pos();
};

class TEventWindow: public TWin
{
private:
    friend WNDFN WndProc (Handle, unsigned int, unsigned int, long);
    bool m_do_resize, m_bail_out;
    POINT m_fixed_size;
    AbstractMessageHandler *m_dispatcher;
    TDC *m_dc;
    Handle m_bkgnd_brush, m_hmenu;
    long m_style, m_timer;
    long m_bk_color;
    Handle m_old_cursor;
    TScrollBar *m_sb_vert;

 public:

     enum { TM_CHAR_HEIGHT=1, TM_CHAR_WIDTH,
          TM_CAPTION_HEIGHT, TM_MENU_HEIGHT,TM_CLIENT_EXTRA,
          TM_SCREEN_WIDTH, TM_SCREEN_HEIGHT,
          TM_END,
          VERT_SCROLL = 0x00200000L
     };

     enum CursorType { RESTORE, HOURGLASS };

     POINT fixed_size();
     void enable_resize(bool do_resize, int w=0, int h=0);
     bool cant_resize();
     void client_resize(int cwidth, int cheight);
     TEventWindow() 
      { set_defaults(); }

     TEventWindow(TWin *parent, pchar winclss, pchar text, int id,
          dword styleEx=0)
      : TWin(parent,winclss,text,id,styleEx)
      { set_defaults(); }

     TEventWindow(pchar caption, TWin *parent=0, dword style=0);
     /*virtual*/ ~TEventWindow( );

     void set_defaults();
     void set_window();
     virtual void pre_create();
     void vert_scroll();
     void create_window(pchar caption, TWin *parent);
     void create_timer(int msec);
     void kill_timer();
     void set_menu(char *res);
     void set_menu(Handle menu);
     Handle get_menu()
       { return m_hmenu;      }
     TScrollBar *scroll_bar()
       { return m_sb_vert; }
     void check_menu(int id, bool check);
     void add_handler(AbstractMessageHandler *m_hand);
     AbstractMessageHandler *get_handler()
       { return m_dispatcher; }
     TDC *get_dc()
       { return m_dc; }
     void quit_loop();
     void cursor(CursorType curs);
     void update_data();
     void update_controls();
     int metrics(int ntype);     
     int run();

//-----------Event Handling-------------------
  virtual void size(int cx, int cy);
  virtual void on_move();
  virtual void paint(TDC&);
  virtual bool query_close() { return true; }

 // input
  virtual void keydown(int vkey);
  virtual void on_char(int vkey,int repeat);

 // mouse messages
  virtual void mouse_down(Point& pt);
  virtual void mouse_move(Point& pt);
  virtual void mouse_up  (Point& pt);
  virtual void right_mouse_down(Point& pt);
  virtual void mouse_double_click(Point& pt);
 // scrolling
  //virtual void hscroll(int code, int posn);
  virtual void vscroll(int code, int posn);

 // command
  virtual bool command(int id);
  virtual bool sys_command(int id);
  virtual int  handle_user(long lparam, long wparam);

 // other
  virtual void timer();
  virtual void focus(bool yes_no);
  virtual void destroy();
  void    show(int cmd_show=0);

  void set_background(float r, float g, float b); // how to spec. colour??

 };

 class AbstractMessageHandler {
 protected:
   TEventWindow *m_form;
 public:
   AbstractMessageHandler(TEventWindow *form)
    : m_form(form) { }
   virtual ~AbstractMessageHandler() { }
   virtual void dispatch(int id, int notify, Handle ctrl) = 0;
   virtual void add_handler(AbstractMessageHandler *) = 0;
   virtual void write()=0;
   virtual void read()=0;
 };

 class TFrameWindow: public TEventWindow {
  public:
     TFrameWindow(pchar caption="twl", TWin *cli=0, int style=0, TWin *parent=0);

     ~TFrameWindow();

    void destroy();
    void size();
    void set_status_text(int id, pchar txt);

     void set_client(TWin *cli);
     TWin *get_client()
      { return m_client; }

protected:
     TWin *m_client;
     Handle m_status;
 };

 typedef TEventWindow TEW;  //...common alias for EventWindow!

 //////// Wrappers around Windows Controls //////

 class TControl: public TEventWindow {
 public:
     void *m_wnd_proc;  //*TEMP*
     TControl(TWin *parent,pchar classname, pchar text,int id=-1, long style=0);
     TControl(pchar caption, TWin *parent=0, dword style=0)
      : TEventWindow(caption,parent,style) {}
     ~TControl();
     virtual char *type_name()                   { return "TControl"; }
     void calc_size();
     bool is_type(char *tname);
     void set_font(TFont *fnt);
     void set_colour(float r, float g, float b);
     long get_colour() const                     { return m_colour; }
     TEventWindow *parent() const                { return m_parent; }
     void parent(TEventWindow *ew)               { m_parent = ew; }
     void data(void *data)                       { m_data = data; }
     void *data()                                { return m_data; } 

      static TControl *user_data(Handle handle);      
 protected:
     long m_colour;
     TFont *m_font;
     TEventWindow *m_parent;
     void *m_data;
 };

 class TLabel: public TControl {
 public:
     TLabel(TWin *parent,pchar text, int id=-1);
     char *type_name() { return "TLabel"; }
 };

 class TEdit: public TControl {
 public:
     TEdit(TWin *parent, pchar text, int id, long style=0);
     char *type_name() { return "TEdit"; }
 };


 ////////// Dialog Box Class///////////////////////////////
class TDialog: public TWin
{
 public: 
  TDialog (pchar dlg_name,TWin *owner_win=NULL)
  {
    m_name = dlg_name;
    m_owner = owner_win;
    is_modeless(false);
  }
  ~TDialog ();

  void go (); 
  bool was_successful(); 
  bool modeless()
  { return m_modeless ? true : false; }
  void is_modeless(bool which=true)
   { m_modeless = which; }

  TWin *get_owner()
   { return m_owner; }

//...virtual event handlers......
  virtual bool init()       {return true;}
  virtual bool command(int) {return true;}
  virtual bool finis()      {return true;}

  TWin *field(int id);

 protected:
   void *m_lpfnBox;
   int m_modeless, m_ret;
    pchar m_name;
   TWin *m_owner;
};

 int exec(pchar s, int mode=SW_SHOW, bool do_wait = false);
 int message(char *format, bool was_error=false);
 bool get_key_state(int keycode); 
 #ifdef __MSTRING_H
  inline int message(string s) { return message(s.c_str()); }
 #endif

 int SMain(int argc, char **argv);
 int get_argc();
 char **get_argv();


#endif

