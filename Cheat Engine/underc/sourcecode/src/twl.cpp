// TWL.CPP
/*
 *  Tiny, Terminal or Toy Windows Library
 *  Steve Donovan, Miningtek 1998
 *  based on SWL, 1995.
*/

//#define WIN32_LEAN_AND_MEAN
#define NO_STRICT
#include <windows.h>
#ifdef _WIN32
//# include <commctrl.h>
//# define COMMCTRL
# define LOW_WORD LOWORD
#else
# define LOW_WORD WORD
# define GWL_USERDATA 0
#endif

#include <string.h>
#include <stdlib.h>
#include <ctype.h>

#include "twl.h"

#define MAX_CMD_LINE 120
#define N_CMD_LINE 20
// *hack 1.2.8 make the window class distinct from YAWL;
// in general, it should be unique per process.
#define EW_CLASSNAME "EVNTWNDCLSS0"
#define POP_WNDCLSS "WNDCLSS"

static HINSTANCE hInst;
static int CmdShow;
static HANDLE hAccel=0,hModeless=0;
char obuff[BUFSIZE];
static char buff[MAX_CMD_LINE];

typedef unsigned char byte;

// Miscelaneous functions!!
long RGBF(float r, float g, float b)
//-------------------------------------
{ return RGB(byte(255*r),byte(255*g),byte(255*b)); }

long RGBI(int r, int g, int b)
{ return RGB(r,g,b); }

int exec(pchar s, int mode, bool do_wait)
//--------------------------------------
{
 // return WinExec(s,mode);
 PROCESS_INFORMATION pi;
 STARTUPINFO si;
 TWin *lastw = TWin::get_active_window();
 ZeroMemory(&si,sizeof(STARTUPINFO));
 si.cb = sizeof(STARTUPINFO);
 si.dwFlags = STARTF_USESHOWWINDOW;
 si.wShowWindow = mode;
 BOOL bRet = CreateProcess(NULL, s, NULL,NULL,   // no security!
                           FALSE, // inherited handles
                           CREATE_NEW_CONSOLE,  // creation flags
                           NULL,NULL, 
                           &si, &pi);
 if (!bRet) return 0;
 if (do_wait) {
    WaitForSingleObject(pi.hProcess,1000);
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
 }
 //process_id = pi.dwProcessId;
 lastw->set_focus();
 delete lastw;
 return 1;
}

long current_time()
{ return GetTickCount(); }

bool get_key_state(int keycode)
{ return GetKeyState(keycode) < 0; }

int message(char *format,bool was_warning)
//----------------------------------------
{
  //char *Args;
  int ret;

 // Args = (char *) &format + sizeof format;
 // vsprintf(obuff,format,Args);
 // ret = MessageBox(NULL,obuff,"Message",MB_ICONEXCLAMATION | MB_OKCANCEL);

  ret = MessageBox(NULL,format,"Message",MB_ICONEXCLAMATION | MB_OKCANCEL);
  if (ret == IDCANCEL) return 0;  else return 1;
 }


/// TDC ///////////////

TDC::TDC()
{
    m_hdc = m_pen = m_font = m_brush = NULL;
    m_twin = NULL;
}

TDC::~TDC()
{  }

void TDC::get(TWin *pw)
//---------------------
{ 
    if(!pw) pw = m_twin;
    m_hdc = GetDC(pw->handle());
}

void TDC::release(TWin *pw) 
//--------------------------
{
    if(!pw) pw = m_twin;
    ReleaseDC(pw->handle(),m_hdc); 
}

void TDC::kill()
//--------------
{  DeleteDC(m_hdc); }

Handle TDC::select(Handle obj)
//---------------------------
{  return SelectObject(m_hdc,obj); }

void TDC::select_stock(int val)
//----------------------------
{ select(GetStockObject(val)); }

void TDC::xor_pen()
//-----------------
{  SetROP2(m_hdc, GetROP2(m_hdc)==R2_XORPEN ? R2_COPYPEN : R2_XORPEN); }

void TDC::to_logical(Point *pt, int sz)
{ DPtoLP(m_hdc,(LPPOINT)pt,sz); }

void TDC::to_device(Point *pt, int sz)
{ LPtoDP(m_hdc,(LPPOINT)pt,sz); }

void TDC::set_colour(long colour)
{
    COLORREF rgb = (COLORREF)colour;
    get();
    SetTextColor(m_hdc,rgb);
    if(m_pen) DeleteObject(m_pen);
    m_pen = CreatePen(PS_SOLID,0,rgb);
    select(m_pen);
    release();
}

void TDC::set_back_colour(long colour)
{
    COLORREF rgb = (COLORREF)colour;
    SetBkColor(m_hdc,rgb);
    if(m_brush) DeleteObject(m_brush);
    m_brush = CreateSolidBrush(rgb);
    select(m_brush);
}


// this changes both the _pen_ and the _text_ colour
void TDC::set_colour(float r, float g, float b)
//----------------------------
{
   set_colour(RGBF(r,g,b));
}

void TDC::set_back_colour(float r, float g, float b)
{
   set_back_colour(RGBF(r,g,b));
}

void TDC::set_text_colour(long colour)
{  SetTextColor(m_hdc,colour);      }
  

void TDC::set_text_align(int flags, bool update_cp)
//-------------------------------------------------
{
 get();
 SetTextAlign(m_hdc, flags | (update_cp ? TA_UPDATECP : 0));
 release();
}

void TDC::get_text_extent(pchar text, int& w, int& h, TFont *font)
//----------------------------------------------------------------
{
 SIZE sz;
 HFONT oldfont;
 get();
 if (font) oldfont = select(*font);
 #ifdef _WIN32
 GetTextExtentPoint32(m_hdc,text,lstrlen(text),&sz);
 #else
    DWORD res = GetTextExtent(m_hdc,text,lstrlen(text));
    sz.cx = LOWORD(res);
    sz.cy = HIWORD(res);
 #endif
 if (font) select(oldfont);
 release();
 w = sz.cx;
 h = sz.cy;
}

// wrappers around common graphics calls
void TDC::draw_text(pchar msg)
//---------------------------------------------------
{  TextOut(m_hdc,0,0,msg,lstrlen(msg)); }

void TDC::text_out(int x, int y, char *buff, int sz)
{
  TextOut(m_hdc,x,y,buff,sz);
}

void TDC::move_to(int x, int y)
//----------------------------
{  MoveToEx(m_hdc,x,y,NULL); }

void TDC::line_to(int x, int y)
//-----------------------------
{  LineTo(m_hdc,x,y/*,NULL*/); }

void TDC::rectangle(const Rect& rt)
//---------------------------------
{  Rectangle(m_hdc, rt.left,rt.top,rt.right,rt.bottom); }

void TDC::ellipse(const Rect& rt)
//---------------------------------
{  Ellipse(m_hdc, rt.left,rt.top,rt.right,rt.bottom); }

void TDC::invert_rect(const Rect& rt)
//-----------------------------------
{  InvertRect(m_hdc,(RECT *)&rt); }


TClientDC::TClientDC(TEventWindow *ew)
{
    set_hdc(ew->get_dc()->get_hdc());
    set_twin(ew);
    get();
}

TClientDC::~TClientDC()
{
    release();
}

//// TGDIObj

void TGDIObj::destroy()
//---------------
{ if (m_hand) DeleteObject(m_hand); m_hand = NULL; }
 
//// TFont ////////
#define PLF ((LOGFONT *)m_pfont)

TFont::TFont()
{
   m_pfont = (void *) new LOGFONT;
}

TFont::~TFont()
{
   delete (LOGFONT *) m_pfont;
}


void TFont::set(pchar spec, int sz, int ftype)
//-------------------------------------------
{
  LOGFONT&lf = *(LOGFONT *)m_pfont;  // define an alias...
  int wt = FW_NORMAL;
  lf.lfHeight = sz;
  if (ftype & BOLD)  wt = FW_BOLD;
  lf.lfWeight = wt;
  lf.lfItalic = (ftype & ITALIC) ? TRUE: FALSE;
  lf.lfOutPrecision = OUT_DEFAULT_PRECIS;
  lf.lfCharSet = ANSI_CHARSET;
  lf.lfQuality = PROOF_QUALITY;
  lf.lfEscapement = 0;
  lf.lfOrientation = 0;
  lf.lfUnderline = 0;
  lf.lfStrikeOut = 0;
  lstrcpy(lf.lfFaceName,spec);
  create();
}

/// copy constructor
TFont& TFont::operator = (const TFont& f)
//--------------------------------------
{ 
  memcpy(m_pfont,f.m_pfont,sizeof(LOGFONT));
  create();
  return *this;
}

void TFont::create()
//------------------
{
   destroy();
   m_hand = CreateFontIndirect((LOGFONT *)m_pfont);
}

void  TFont::set_name(pchar name)
//-------------------------------
{  lstrcpy(PLF->lfFaceName,name); create(); }


void  TFont::set_size(int pts)
//----------------------------
{  PLF->lfHeight = pts;      create(); }

void  TFont::set_bold()
//-----------------------
{  PLF->lfWeight = FW_BOLD;  create(); }

void  TFont::set_italic()
//-----------------------
{  PLF->lfItalic = TRUE;     create(); }

////// TWin ///////////

static void check_error()
{
 LPVOID lpMsgBuf;
 FormatMessage( 
    FORMAT_MESSAGE_ALLOCATE_BUFFER | 
    FORMAT_MESSAGE_FROM_SYSTEM | 
    FORMAT_MESSAGE_IGNORE_INSERTS,
    NULL,
    GetLastError(),
    MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
    (LPTSTR) &lpMsgBuf,
    0,
    NULL 
);
// Process any inserts in lpMsgBuf.
// ...(?)
// Display the string.
MessageBox( NULL, (LPCTSTR)lpMsgBuf, "Error", MB_OK | MB_ICONINFORMATION );
// Free the buffer.
LocalFree( lpMsgBuf );
}

TWin::TWin(TWin *parent, pchar winclss, pchar text, int id, dword styleEx)
//------------------------------------------------------------------------
{
   DWORD style = WS_CHILD | WS_VISIBLE | styleEx;
   HWND hwndChild = CreateWindowEx(WS_EX_LEFT,winclss,text,style,
           0,0,CW_USEDEFAULT,CW_USEDEFAULT,
           parent->m_hwnd, (HMENU)id, hInst,NULL);
   set(hwndChild);
   check_error();
}
 
void TWin::update()
//-----------------
{
    UpdateWindow(m_hwnd);
}

static inline bool as_bool(int val)
{ return val ? true : false; }

static inline bool as_bool(void *ptr)
{ return ptr ? true : false; }

bool TWin::is_visible()
{ 
  return as_bool(IsWindowVisible(m_hwnd));
}
 
void TWin::invalidate(Rect *lprt)
{ InvalidateRect(m_hwnd,(LPRECT)lprt,TRUE); }

void TWin::get_rect(Rect &rt)
//---------------------------
{
   GetWindowRect(m_hwnd,(LPRECT)&rt);
}

void TWin::get_client_rect(Rect &rt)
{
   GetClientRect(m_hwnd,(LPRECT)&rt);
}

Point TWin::to_screen(Point pt)
{ ClientToScreen(m_hwnd,(LPPOINT)&pt);  return pt; }

Point TWin::to_client(Point pt)
{ ScreenToClient(m_hwnd,(LPPOINT)&pt);  return pt; }


int TWin::width()
//---------------
{
   Rect rt; get_rect(rt); return rt.right - rt.left;
}

int TWin::height()
//----------------
{
   Rect rt; get_rect(rt); return rt.bottom - rt.top;
}

void TWin::set_text(pchar str)
//---------------------------
{ SetWindowText(m_hwnd, str);  }

pchar TWin::get_text(pchar str, int sz)
//-------------------------------------
{ GetWindowText(m_hwnd,str,sz);          return str; }

// These guys work with the specified _child_ of the window

void TWin::set_text(int id, pchar str)
//-------------------------------------
{ SetDlgItemText(m_hwnd,id,str); }

void TWin::set_int(int id, int val)
//-------------------------------------
{ SetDlgItemInt(m_hwnd,id,val,TRUE); }

pchar TWin::get_text(int id, pchar str, int sz)
//--------------------------------------------
{  GetDlgItemText(m_hwnd,id,str,sz);  return str; }

void TWin::scroll(int dx, int dy)
{  ScrollWindow(m_hwnd,dx,dy,NULL,NULL); }

int TWin::get_int(int id)
//-----------------------
{  BOOL success;
   return (int)GetDlgItemInt(m_hwnd,id,&success,TRUE);
}

TWin *
TWin::get_twin(int id)
//--------------------
{
    HWND hwnd = GetDlgItem(m_hwnd,id);
    if (hwnd) {
        // Extract the 'this' pointer, if it exists
        TWin *pwin = (TWin *)GetWindowLong(hwnd,0);
        // if not, then just wrap up the handle
        if (!pwin) pwin = new TWin(hwnd);
        return pwin;
    }
    else return NULL;
}

TWin *
TWin::get_active_window()
{
    return new TWin(GetActiveWindow());
}

void TWin::set_focus()
{
    SetFocus((HWND)m_hwnd);
}

int
TWin::get_id()
//------------
{ return GetWindowLong(m_hwnd,GWL_ID); }

void TWin::close()
{
  DestroyWindow(m_hwnd);
}

void TWin::resize(int x0, int y0, int w, int h)
//--------------------------------------------
{ 
 MoveWindow(m_hwnd,x0,y0,w,h,TRUE);
 //*SJD* This is a hack to ensure that WCON child console
 // windows do indeed appear on the top on Win95 systems.
 on_top();
}

void  TWin::on_top()  // *add 0.9.4
{
 SetWindowPos(handle(),HWND_TOP,0,0,0,0,SWP_NOMOVE | SWP_NOSIZE | SWP_SHOWWINDOW);
}

void TWin::resize(int w, int h)
//------------------------------
{ SetWindowPos(handle(),NULL,0,0,w,h,SWP_NOMOVE); }

void TWin::move(int x0, int y0)
//------------------------------
{ SetWindowPos(handle(),NULL,x0,y0,0,0,SWP_NOSIZE); }

void TWin::show(int how)
//--------------------------------
{ ShowWindow(m_hwnd,how); }

void TWin::set_style(dword s)
//---------------------------
{ SetWindowLong(m_hwnd,GWL_STYLE,s); }

int TWin::send_msg(int msg, int wparam, int lparam) const
//---------------------------------------------------
{ return SendMessage(m_hwnd, msg, wparam, lparam); }

int TWin::post_msg(int msg, int wparam, int lparam) const
{ return PostMessage(m_hwnd, msg, wparam, lparam); }
 
TWin *TWin::create_child(pchar winclss, pchar text, int id, dword styleEx)
//------------------------------------------------------------------------
{
    return new TWin(this,winclss,text,id,styleEx);
}

int TWin::message(char *msg, int type)
//-------------------------------------
{
 int flags;
 char *title;
 if (type == MSG_ERROR) { flags = MB_ICONERROR | MB_OK; title = "Error"; } else
 if (type == MSG_WARNING) { flags = MB_ICONEXCLAMATION | MB_OKCANCEL; title = "Warning"; }
 else { flags = MB_OK; title = "Message"; }
 return MessageBox(m_hwnd, msg, title,flags);
}

TScrollBar::TScrollBar(Handle hwnd, int type)
: TWin(hwnd),m_type(type)
{
}

void TScrollBar::set_range(int rmin, int rmax, bool repaint/*=false*/)
{
  SetScrollRange(m_hwnd,m_type,rmin,rmax,repaint);
}

void TScrollBar::set_pos(int rpos, bool repaint/*=false*/)
{
  SetScrollPos(m_hwnd,m_type,rpos,repaint);
}

int  TScrollBar::get_pos()
{
 return GetScrollPos(m_hwnd,m_type);
}

//----TEventWindow class member definitions---------------------
TEventWindow::TEventWindow(pchar caption, TWin *parent, dword style)
//-----------------------------------------------------------------
{
  set_defaults();
  if (style) m_style |= style;
  create_window(caption, parent);
  get_dc()->set_text_align(TA_TOP | TA_LEFT);
  enable_resize(true);
}

TEventWindow::~TEventWindow()
{
    if (m_timer) kill_timer();
    if (m_dc) delete m_dc;
    DestroyWindow((HWND)m_hwnd);
}

void TEventWindow::pre_create() {}

void TEventWindow::vert_scroll()
{
 m_sb_vert = (TScrollBar *)this; // just to flag!
}

void TEventWindow::create_window(pchar caption, TWin *parent)
//-------------------------------------------------------------------
{
 HWND hParent;
 void *CreatParms[2];
 pre_create();
 m_dc = new TDC;
 CreatParms[0] = (void *)this;
 if (parent) hParent = parent->handle(); else hParent = NULL;
 m_hwnd = CreateWindow (EW_CLASSNAME, caption, m_style,
            CW_USEDEFAULT,CW_USEDEFAULT,CW_USEDEFAULT, CW_USEDEFAULT,
            hParent,NULL, hInst, CreatParms);
 if(m_style & WS_VSCROLL) m_sb_vert = new TScrollBar(m_hwnd,SB_VERT);
 set_window();
}

void TEventWindow::set_defaults()
//-------------------------------
{
  m_style = WS_OVERLAPPEDWINDOW | CS_DBLCLKS;
  m_bkgnd_brush =  GetStockObject (WHITE_BRUSH /*LTGRAY_BRUSH*/);
  m_bk_color = RGB(192,192,192);
  m_hmenu = NULL;
  m_timer = 0;
  m_dispatcher = NULL;
  m_dc = NULL;
  m_bail_out = false;
  m_sb_vert = NULL;
}

void TEventWindow::add_handler(AbstractMessageHandler *m_hand)
{
 if (!m_dispatcher) m_dispatcher = m_hand;
 else m_dispatcher->add_handler(m_hand);
}

void TEventWindow::update_data()
{
    get_handler()->write();
}

void TEventWindow::update_controls()
{
    get_handler()->read(); 
}

bool TEventWindow::cant_resize()
{
 return !m_do_resize;
}

int TEventWindow::metrics(int ntype)
//----------------------------------
// Encapsulates what we need from GetSystemMetrics() and GetTextMetrics()
{
    if (ntype < TM_CAPTION_HEIGHT) { // text metrics
       TEXTMETRIC tm;
       GetTextMetrics(get_dc()->get_hdc(),&tm);
       if (ntype == TM_CHAR_HEIGHT) return tm.tmHeight; else
       if (ntype == TM_CHAR_WIDTH)  return tm.tmMaxCharWidth;
    } else {
        switch(ntype) { 
        case TM_CAPTION_HEIGHT: return GetSystemMetrics(SM_CYMINIMIZED);
        case TM_MENU_HEIGHT: return GetSystemMetrics(SM_CYMENU);
        case TM_CLIENT_EXTRA:
            return metrics(TM_CAPTION_HEIGHT) + (m_hmenu != NULL) ? metrics(TM_MENU_HEIGHT) : 0; 
        case TM_SCREEN_WIDTH:  return GetSystemMetrics(SM_CXMAXIMIZED); 
        case TM_SCREEN_HEIGHT: return GetSystemMetrics(SM_CYMAXIMIZED);
        default: return 0;
        }       
    }
    return 0;
}

void TEventWindow::client_resize(int cwidth, int cheight)
{
// *SJD* This allows for menus etc - really is a frame window method??
    resize(cwidth,cheight + metrics(TM_CLIENT_EXTRA));
}

void TEventWindow::enable_resize(bool do_resize, int w, int h)
{
 m_do_resize = do_resize;
 m_fixed_size.x = w;
 m_fixed_size.y = h;
}

POINT TEventWindow::fixed_size()
{
 return m_fixed_size;
}

void TEventWindow::cursor(CursorType curs)
{
  HCURSOR new_cursor;
  if (curs == RESTORE) new_cursor = m_old_cursor;
  else
  switch(curs) {
  case HOURGLASS: new_cursor = LoadCursor(NULL,IDC_WAIT); break;
  }
  SetCursor(new_cursor);
}


void TEventWindow::set_window()
//-----------------------------
{
  set(m_hwnd);

//  set_win(*this);   //...set up DC part.....??
}

void TEventWindow::set_background(float r, float g, float b)
//-----------------------------------------
{
    COLORREF rgb = RGBF(r,g,b);
    m_bkgnd_brush = CreateSolidBrush(rgb);
    get_dc()->get(this);
    SetBkColor(get_dc()->get_hdc(),rgb);
    get_dc()->select(m_bkgnd_brush);
    m_bk_color = rgb;
    get_dc()->release(this);
    invalidate();
}

void TEventWindow::set_menu(char *res)
//------------------------------------
{
     if (m_hmenu) DeleteObject(m_hmenu);
     set_menu(LoadMenu(hInst,res));
}

void TEventWindow::set_menu(Handle menu)
//------------------------------------
{
  m_hmenu = menu;
  SetMenu(m_hwnd,menu);
}

void TEventWindow::check_menu(int id, bool check)
{
    CheckMenuItem(m_hmenu,id, MF_BYCOMMAND | (check ? MF_CHECKED : MF_UNCHECKED));
}

void TEventWindow::show(int cmd_show)
//-----------------------------------
{
  // default:  use the 'nCmdShow' we were _originally_ passed
  if (! cmd_show) cmd_show = CmdShow;
  ShowWindow(m_hwnd,cmd_show);
}

void TEventWindow::create_timer(int msec)
//---------------------------------------
{
    if(m_timer) kill_timer();
    m_timer = SetTimer(m_hwnd,1,msec,NULL);
 }

void TEventWindow::kill_timer()
//-----------------------------
{  KillTimer(m_hwnd, m_timer); }

// *fix 1.2.2 (Eric) We were spending an excessive amount of time doing idle processing
#ifdef BUILD_WITH_GTK
void special_idle(); // in UCW_GTK
#else
void special_idle() { WaitMessage(); }
#endif

// Message Loop!
// *NB* this loop shd cover ALL cases, controlled by global
// variables like mdi_sysaccell,  accell, hModelessDlg, etc.
// 31/08/00 - there's a bail out flag - not perhaps the
// most elegant method, but it'll do.
int TEventWindow::run()
//---------------------
{
  MSG msg;
  while (TRUE) {
	if (PeekMessage (&msg, NULL, 0, 0,PM_REMOVE)) {
      if (msg.message == WM_QUIT) break;
      if (m_bail_out) { m_bail_out = false; return -1; }
      if (! hAccel || !TranslateAccelerator(m_hwnd,hAccel,&msg)) {
      if (! hModeless || !IsDialogMessage(hModeless,&msg)) {
        TranslateMessage (&msg) ;
        DispatchMessage (&msg) ;
	  }
	 }	
	}
	else special_idle();
 }
 return msg.wParam ;
}


static unsigned long mMainThread;

void set_main_thread(unsigned long id)
{
 mMainThread = id; // GetCurrentThreadId();
}

void TEventWindow::quit_loop()
{
  m_bail_out = true;
  //post_msg(WM_CHAR,' ',1); 
  ULONG id = GetWindowThreadProcessId((HWND)handle(),NULL);
  BOOL res = PostThreadMessage(id,WM_CHAR,' ',1);
}

// Place holder functions - no functionality offered at this level!
void TEventWindow::size(int,int) {}
void TEventWindow::on_move() {}

bool TEventWindow::command(int) { return true; }
bool TEventWindow::sys_command(int) { return false; }
void TEventWindow::paint(TDC&) {}
void TEventWindow::mouse_down(Point&) {}
void TEventWindow::mouse_up(Point&) { }
void TEventWindow::right_mouse_down(Point&) {}
void TEventWindow::mouse_move(Point&) { }
void TEventWindow::mouse_double_click(Point& pt) { }
void TEventWindow::focus(bool) {}
void TEventWindow::keydown(int) { }
void TEventWindow::on_char(int,int) { }
void TEventWindow::destroy() { }
void TEventWindow::timer() { }
void TEventWindow::vscroll(int code, int posn) { }
int  TEventWindow::handle_user(long lparam, long wparam) { return 0; }

////// Members of TFrameWindow

TFrameWindow::TFrameWindow(pchar caption, TWin *cli, int style, TWin *parent)
: TEventWindow(caption,parent,style)
{
 // int SBParts[2];
  set_client(cli);
 #ifdef COMMCTRL
  /*
  m_status = CreateStatusWindow(WS_CHILD | WS_BORDER | WS_VISIBLE,
                "", m_hwnd, 1);
  if(!m_status) set_text("no status");
  SBParts[0] = width()/2;
  SBParts[1] = -1;
  SendMessage(m_status,SB_SETPARTS,2,(LPARAM)SBParts);
  */
  m_status = NULL; // for now!!
 #else
  m_status = NULL;
 #endif
}

TFrameWindow::~TFrameWindow()
{
 delete m_client;
}

void TFrameWindow::set_status_text(int id, pchar txt)
{
#ifdef COMMCTRL
  SendMessage(m_status,SB_SETTEXT,id,(LPARAM)txt);
#endif
}

void TFrameWindow::destroy()
//....overrides the default w/ application-closing behaviour!!
{
  PostQuitMessage(0);
}

void TFrameWindow::set_client(TWin *cli)
{
  m_client = cli;
  if (m_client) {
      m_client->show();
  } 
}

void TFrameWindow::size()
{
  RECT rt;
  HDWP hdwp;
  int n = 0, bottom_extra = 0;

  if (m_status) n++;
  if (m_client) n++;
  if (n==0) return;

  GetClientRect(m_hwnd,&rt);
  hdwp = BeginDeferWindowPos(n);

  if (m_status) {
   DeferWindowPos(hdwp,m_status,NULL,0,0,       //???
                     rt.right - rt.left, 20, SWP_NOZORDER);
   bottom_extra = 20;
  } 

  if (m_client)
  DeferWindowPos(hdwp,m_client->handle(),NULL,0,0,
                     rt.right - rt.left, rt.bottom-rt.top - bottom_extra,
                      SWP_NOZORDER | SWP_NOMOVE);

  EndDeferWindowPos(hdwp);
}

/// Windows controls - TControl ////////////

TControl::TControl(TWin *parent, pchar classname, pchar text,int id, long style)
: TEventWindow(parent,classname,text,id,style)
{
   m_colour = RGB(0,0,0); //RGB(255,255,255);  // shd depend on background!
   m_font = NULL;
   SetWindowLong(m_hwnd,GWL_USERDATA,(long)this);
   m_parent = (TEventWindow *)parent;
   //calc_size();
}

TControl::~TControl()
{
}

TControl *
TControl::user_data(Handle handle)
{
 return (TControl *)GetWindowLong(handle,GWL_USERDATA);
}

void TControl::calc_size()
{
  int cx,cy;
  m_parent->get_dc()->get_text_extent(get_text(),cx,cy,m_font);
  resize(int(1.05*cx),int(1.05*cy));
}

bool TControl::is_type(char *tname)
{
    return strcmp(type_name(),tname)==0;
}

void TControl::set_font(TFont *fnt)
{
    m_font = fnt;
    calc_size();
    if (m_font) 
     send_msg(WM_SETFONT,(WPARAM)m_font->handle(),(LPARAM)TRUE);
}

void TControl::set_colour(float r, float g, float b)
{
    m_colour = (long)RGBF(r,g,b);
    update();
}


TLabel::TLabel(TWin *parent, pchar text, int id)
//--------------------------------------
: TControl(parent,"STATIC",text,id,0x0)
{ }

TEdit::TEdit(TWin *parent, pchar text, int id, long style)
//-----------------------------------------------
: TControl(parent,"EDIT",text,id,style | WS_BORDER)
{  }


//-----------------Dialog boxes--------------------------
TDialog::~TDialog ()
 { FreeProcInstance((DLGPROC)m_lpfnBox); }

bool TDialog::was_successful()
{
   if (modeless()) return as_bool(handle());
   else return as_bool(m_ret);
}

DLGFN DialogProc (HWND hdlg, UINT msg, UINT wParam,LONG lParam);

void TDialog::go ()
//-----------------
{
    HWND hdlg,hOwner;

    hOwner = (m_owner) ? m_owner->handle() : NULL; //GetDesktopWindow();
    m_lpfnBox = (void FAR *)MakeProcInstance((FARPROC)DialogProc,hInst);
     if (modeless()) {
      hdlg = CreateDialogParam(hInst,m_name,hOwner,(DLGPROC)m_lpfnBox,(long)this);
      hModeless = hdlg;
     } else {
       m_ret = DialogBoxParam(hInst,m_name,hOwner,(DLGPROC)m_lpfnBox,(long)this);
       m_hwnd = 0;  // thereafter, this object is not a valid window...
    }
     ShowWindow(hdlg,SW_SHOW);
}


TWin * TDialog::field(int id)
{
  return new TWin(GetDlgItem(m_hwnd,id));
}



//....Modeless Dialog Box procedure.........................
DLGFN DialogProc (HWND hdlg, UINT msg, UINT wParam,LONG lParam)
{
  int ret;
  TDialog *This = (TDialog *) GetWindowLong(hdlg,DWL_USER);

  switch (msg)
  {
     case WM_INITDIALOG:
        //..... 'This' pointer passed as param from CreateDialogParam()
         SetWindowLong(hdlg,DWL_USER,lParam);
         This = (TDialog *)lParam;
         if (This->modeless()) hModeless = hdlg;
         This->set(hdlg);
         return This->init();

     case WM_COMMAND:
        switch(LOW_WORD(wParam)) {
         case IDOK:
             ret = This->finis();
             if (! ret) return FALSE;  // no, we're not finished yet!
         case IDCANCEL:
             if (This->modeless()) SendMessage(hdlg,WM_CLOSE,0,0L);
                                  else EndDialog(hdlg,wParam==IDOK);
         break;
         default:
             This->command(wParam);
         break;
        }
        return TRUE;

     case WM_CLOSE:
         DestroyWindow(hdlg);
         hModeless = 0;
         break;
  }
  return FALSE;  // we did not process the message...
}


WNDFN WndProc (HWND hwnd, UINT msg, UINT wParam,LONG lParam);


//----------------Window Registration----------------------

void RegisterEventWindow(HANDLE hIcon=0, HANDLE hCurs=0)
//------------------------------------------------------
{
    WNDCLASS    wndclass;

    wndclass.style         = CS_HREDRAW | CS_VREDRAW |
                             CS_OWNDC | CS_DBLCLKS;
    wndclass.lpfnWndProc   = WndProc;
    wndclass.cbClsExtra    = 0;
    wndclass.cbWndExtra    = 4;
    wndclass.hInstance     = hInst;
    wndclass.hIcon         = hIcon ? hIcon : LoadIcon (NULL, IDI_APPLICATION) ;
    wndclass.hCursor       = hCurs ? hCurs : LoadCursor (NULL, IDC_ARROW) ;
    wndclass.hbrBackground = NULL; //GetStockObject(LTGRAY_BRUSH);
    wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = EW_CLASSNAME;

   RegisterClass (&wndclass);

 }

char *args[N_CMD_LINE];
int n_args;
int ParseCmdLine(LPSTR CmdLine, char **args);

int get_argc()
{ return n_args; }

char **get_argv()
{ return args; }


int PASCAL WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
            LPSTR lpszCmdLine, int nCmdShow)
//----------------------------------------------------------------
{
 
 #ifdef COMMCTRL
 InitCommonControls();
 #endif

 hInst = hInstance;
 CmdShow = nCmdShow;

 n_args = ParseCmdLine(lpszCmdLine, args);

 if (! hPrevInstance) RegisterEventWindow();

 return SMain(n_args,args);
}


//--------------Default Window Proc for EventWindow-----------------

typedef void *PVOID;

WNDFN WndProc (HWND hwnd, UINT msg, UINT wParam,LONG lParam)
//----------------------------------------------------------------------------
{
//  static SWin w;
  static BOOL dragging = FALSE;
  static long MouseTime = 0;
  static UINT size_flags;
  LPMINMAXINFO pSizeInfo;
  long ret;

  TEventWindow *This = (TEventWindow *) GetWindowLong(hwnd,0);

  switch (msg)
  {
     case WM_CREATE:
      {
         LPCREATESTRUCT lpCreat = (LPCREATESTRUCT) lParam;
         PVOID *lpUser;
         lpUser  = (PVOID *)lpCreat->lpCreateParams;

      //..... 'This' pointer passed as first word of creation parms
         SetWindowLong(hwnd,0,(long)lpUser[0]);
         This = (TEventWindow *)lpUser[0];
         This->get_dc()->set_twin(This);
      }
     return 0;

    case WM_SIZE:
      This->size(LOWORD(lParam),HIWORD(lParam));
      return 0;

    case WM_MOVE:
      This->on_move();
      return 0;

    case WM_GETMINMAXINFO:
        if (This && This->cant_resize()) {
          pSizeInfo = (LPMINMAXINFO) lParam;
          pSizeInfo->ptMaxTrackSize = This->fixed_size();
          pSizeInfo->ptMinTrackSize = This->fixed_size();
        }
        return 0;

  //
    case WM_COMMAND:
      if (This->m_dispatcher) { 
            This->m_dispatcher->dispatch(LOW_WORD(wParam),HIWORD(wParam),(Handle)lParam);
            return 0;
      }
      if (This->command(LOW_WORD(wParam))) return 0;
      else break;

   case WM_USER_PLUS:
      return This->handle_user(wParam,lParam);

    case WM_KEYDOWN:
        This->keydown(wParam);
        return 0;

    case WM_CHAR:
         This->on_char(wParam,LOWORD(lParam));  // 1
         return 0;

    case WM_HSCROLL:
       if (This->m_dispatcher) { 
            int id = GetWindowLong((HWND)lParam,GWL_ID);
            This->m_dispatcher->dispatch(id,LOWORD(wParam),(Handle)lParam);
       } 
       return 0;
    case WM_VSCROLL:
       This->vscroll(wParam,This->scroll_bar()->get_pos());
       return 0;

    case WM_PAINT:
     {
        PAINTSTRUCT ps;
        TDC *dc = This->get_dc();
        dc->set_hdc(BeginPaint(hwnd,&ps));
        This->paint(*dc);
        dc->set_hdc(NULL);
        EndPaint(hwnd,&ps);
     }
     return 0;

// Mouse messages....

  case WM_LBUTTONDOWN:
  case WM_LBUTTONUP:
  case WM_RBUTTONDOWN:
  case WM_LBUTTONDBLCLK:
    {
        Point pt(LOWORD(lParam),HIWORD(lParam));
        //pt.to_logical(*This);
     switch (msg) {
        case WM_LBUTTONDOWN:
            This->mouse_down(pt);
         break;
        case WM_LBUTTONUP:
         This->mouse_up(pt);
         break;
        case WM_RBUTTONDOWN:
            This->right_mouse_down(pt);
            break;
        case WM_LBUTTONDBLCLK:
            This->mouse_double_click(pt);
            break;
      }
    }
    return 0;


    case WM_MOUSEMOVE:  // needs different treatment??
     {
        Point pt(LOWORD(lParam),HIWORD(lParam));
          This->mouse_move(pt);
     }
     return 0;

  case WM_ERASEBKGND:
    {
      RECT rt;
      GetClientRect(hwnd,&rt);
      FillRect((HDC)wParam,(LPRECT)&rt, This->m_bkgnd_brush);
    }
    return 0;

  #ifdef _WIN32
  case WM_CTLCOLORSTATIC:
    {
     TControl *ctl = (TControl *)GetWindowLong((HWND)lParam,GWL_USERDATA);
     SetBkColor((HDC)wParam, (COLORREF)This->m_bk_color);
     SetTextColor((HDC)wParam, ctl->get_colour());
    }
    return (long)This->m_bkgnd_brush;
  #endif

  case WM_SETFOCUS:
     This->focus(true);
     return 0;

  case WM_KILLFOCUS:
     This->focus(false);
     return 0;

  case WM_SYSCOMMAND:
     if (This->sys_command(LOW_WORD(wParam))) return 0; //?
     else break;

  case WM_TIMER:
      This->timer();
      return 0;

  case WM_CLOSE:
    if (! This->query_close()) return 0;
    break;  // *NOTE* this was not here!!

  case WM_DESTROY:
    //*fix 1.2.8 This had been commented out; prevents closing via usual window operation. 
    This->destroy();
    if (This->m_hmenu) DeleteObject(This->m_hmenu);  // but why here?
    return 0 ;
 }

 ret = DefWindowProc (hwnd , msg, wParam, lParam) ;

 return ret;
}

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
    if (*tok) *tok++ = '\0';  
    return str;
}

int ParseCmdLine(LPSTR CmdLine, char **args)
//-----------------------------------------
{
  int i;
  char *arg;
  static char app_path[255];

  GetModuleFileName(NULL,app_path,255);

  lstrcpy(buff,CmdLine);
  args[0] = app_path; 
  args[1] = _quote_strtok(buff,'\"');
  if (! args[1]) return 1;  // at least one argument!
  i = 2;
  while (arg = _quote_strtok(NULL,'\"')) args[i++] = arg;
  return i;
}




