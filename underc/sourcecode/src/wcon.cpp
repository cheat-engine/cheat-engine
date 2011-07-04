/* C++ I/O using a GUI console window
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 *
 * WCON is rather like Petzold's winio library, as described in his _Undocumented Windows_.
 * The C++ interface is using the 'fake' iostreams library (iostrm.cpp) rather than
 * pukka iostreams. Currently this feature is only available in the Windows version, altho
 * this file does not depend directly on the Win32 API.  Rather, all platform access goes 
 * through YAWL (Yet Another Windows Library) which should be portable.
 *
 * Ideally, WCON should be made independent of UnderC, but this version depends on being
 * able to tell whether it's in a program thread, and to kill that program thread if there's
 * a buffer overrun. Also the 'paste into' feature which Greg Perry had on his wishlist 
 * means we need uc_eval() from the main program. 
 */

#include "twl.h"
#include "keys.h"
#include "gdi_stock.h"
#include "twl_misc.h"
//#include <stdio.h>
#include <string.h>
#include <ctype.h>
//#include <stdarg.h>
#include "classlib.h"  // iostreams & strings
#include "ex_vfscanf.h"
#include "wcon.h"

#include <list>

// (a fiddle! WCON shd not depend on the rest of the program!!)
#include "program.h"
// for in_main_thread()

#include "main.h"
// for finalization call...


namespace Engine {
  void kill(int retcode=0);  // in Engine
}

// from main.cpp
int uc_eval(char *expr, bool append_semicolon=true, bool synchronous=false, char *name=NULL, int lineno=0);

static TWin* sConsoleParent = NULL;

const int white = 0xFFFFFF;
const char CTRL_C = 3, CTRL_V = 22, CTRL_Z = 26, CTRL_X = 24;

// where are these buggers usually?
template <class T> 
 T min(T t1, T t2) { return t1 < t2 ? t1 : t2; }

template <class T> 
 T max(T t1, T t2) { return t1 > t2 ? t1 : t2; }

struct Position { 
  int x,y;
  void set(int _x, int _y)
  { x = _x;  y = _y; }

  Position(int _x, int _y)
  : x(_x),y(_y) {}

  Position()
  : x(0),y(0) {}
};

typedef char *pchar;
const int MAX_LINES = 5000,ROWS = 4000, COLUMNS = 100,
          CLIP_BUFF_SIZE = 4096;

char gPromptChar;
int  gPromptCol;

class Buffer {
protected:
  char *m_buff[MAX_LINES];
  long  m_line_colour[MAX_LINES];
  int m_col, m_row;
public:
  void clear_line(int r)
  {
    char *line = m_buff[r];
    for(int j = 0; j < m_col; j++) 
		line[j] = ' '; //'A' + i;
	line[m_col] = '\0'; //was line[j] = '\0';
	m_line_colour[r] = 0;
  }

  void alloc(int r1, int r2)
  {
    for(int i = r1; i <= r2; i++) { 
     m_buff[i] = new char[m_col+4];
     clear_line(i);
    }
  }

  void clear()
  {
    for(int i = 0; i < m_row; i++) 
       clear_line(i);
  }

  Buffer(int rows, int cols)
  : m_col(cols), m_row(rows)
  {
    alloc(0,rows-1);
  }

  ~Buffer()
  {
   for(int i = 0; i < m_row; i++) delete[] m_buff[i];
  }

  void reserve(int nrows)
  {
    if (nrows > m_row) { 
		  alloc(m_row,nrows-1);
	      m_row = nrows;
     }
  }

  void remove(Position& pos)
  {
    char *line = m_buff[pos.y];
	for(int i = pos.x;  i < m_col-1; i++)
	  line[i] = line[i+1];
    line[m_col-1] = ' ';
  } 

  void insert(Position& pos)
  {
    char *line = m_buff[pos.y];
	for(int i = m_col-2;  i >= pos.x; i--)
	  line[i+1] = line[i];
    //line[m_col-1] = ' ';
  }

  int cols() const { return m_col; }
  int rows() const { return m_row; }
  long colour(int r) { return m_line_colour[r]; }
  void colour(int r, long c) { m_line_colour[r] = c; }

  char& operator() (int i, int j)  // x,y!
  { return m_buff[j][i]; }

  void copy_buffer(char *out,int iy, int ix1, int ix2=-1, bool strip_space=false)
  {
    char *line = m_buff[iy], *p = out;
	if(ix2 == -1) ix2 = m_col-1;
	if(strip_space) while(ix2 > ix1 && isspace(line[ix2])) ix2--;
	for(int i = ix1; i <= ix2; i++) *p++ = line[i];
	*p++ = '\0';
  }

  void copy_out(ostream& os, int iy1, int iy2, bool strip_prompts=false)
  {
   char tbuff[255];
   for(int k = iy1; k <= iy2; k++) {
     copy_buffer(tbuff,k,0,-1,true);
	 char *ps = tbuff;
	 if (strip_prompts && ps[gPromptCol] == gPromptChar)
	    do { ps++; } while(!isspace(*ps));
	 os << ps << endl;
   }
  }
};

typedef std::list<string> StringList;

const int default_cursor_width = 2, MAX_HISTORY=20;
Point curr_pt;
int curr_ln, end_ln, old_end_ln;
long curr_tm;
bool is_dragging = false;
const int DRAG_WAIT = 150;
int kount = 0;

class ConsoleWin: public TFrameWindow {
protected:
  Position m_pos, m_start,m_end, m_char;
  Buffer m_buff;
  TCaret m_cursor;
  long m_colour, m_write_colour;
  bool m_insert_mode, m_immediate;
  int m_buff_begin, m_buff_end, m_last_end, m_last_line;
  int m_nchar;
  char m_delim;
  char *m_read_buff;

  // scroll management
  int m_pagesize, m_top;

  // history list
  StringList::iterator m_hlp;;
  StringList m_history;
public:
  void reset_pos()
  {
  	m_top = 0;
	m_pos.set(0,0);
	m_start = m_pos;
    m_end.set(m_buff.cols()-1,m_buff.rows()-1);
    scroll_bar()->set_range(0,100);
  }

  ConsoleWin(int rows, int cols) 
	  : m_buff(rows,cols),TFrameWindow("",NULL,VERT_SCROLL,sConsoleParent)
  {
    //set_background(1.0,0.8,0.8);

	m_colour = 0;
	m_write_colour = 0;
	m_insert_mode = false;
	m_immediate = true;
	m_buff_begin = 0;
	m_buff_end = 0;
	m_last_line = 0;
	m_read_buff = NULL;
	m_nchar = -1;
	m_delim = '\r';

	TClientDC dc(this);
	dc.select_stock(SYSTEM_FIXED_FONT);
	m_char.x = metrics(TM_CHAR_WIDTH);
	m_char.y = metrics(TM_CHAR_HEIGHT);
	m_cursor.set(this,default_cursor_width,m_char.y);

    reset_pos();
  }

  // *fix 1.1.1 Alex's problem w/ closing the UCW window
  void destroy() 
  {
	TFrameWindow::destroy();
	if (Program::in_main_thread())
 	  putchars("#q",true,true);
  }
 
  Buffer& buff() { return m_buff; }

  void copy_out(ostream& os, int iy1, int iy2=-1, bool strip_prompts=false)
  {
   if(iy2==-1) iy2 = m_pos.y-1; //?
   m_buff.copy_out(os,iy1,iy2,strip_prompts);
  }


  void copy_file_to_clip(char *file)
  {
  // this can properly become a TClipboard static method...
     TClipboard clp(this);
	 char *pstr = clp.buffer(CLIP_BUFF_SIZE); // we'll clean this up!
	  // pull _all_ of the file in - binary mode gets the \r\n!
	 ifstream(file,ios::binary).read(pstr,CLIP_BUFF_SIZE); 
	 clp.write();  // and write the buffer to the clipboard
  }

  void copy_to_clip(int iy1, int iy2)
  {
    copy_out(ofstream("tmp.tmp"),iy1,iy2,true);
	copy_file_to_clip("tmp.tmp");
  }

  void copy_selection()
  {
    copy_to_clip(min(curr_ln,end_ln),max(curr_ln,end_ln));
	invalidate();
  }

  void paste_into()
  {
  // *add 0.9.8 Can directly paste expressions into the console
    TClipboard clp(this);
    uc_eval(clp.read(),false,false);  // *fix 1.2.5 async eval is safer...
	putchars("",true,true);  // push an extra line...
  }

  void vscroll(int action, int pos)
  {
    TScrollBar *sb = scroll_bar();
	int old_pos = pos;
    switch(action) {
	case TScrollBar::LINEUP:   pos = max(0,pos-1);                  break;
	case TScrollBar::LINEDOWN: pos = min(m_pos.y,pos+1);            break;
	case TScrollBar::PAGEUP: 
        pos = max(0,pos - m_pagesize);
        break;  
   	case TScrollBar::PAGEDOWN:
        pos = min(m_last_line,pos + m_pagesize);
        break;
//	case TScrollBar::PAGEUP:   pos = max(0,pos - m_pagesize);       break;
//	case TScrollBar::PAGEDOWN: pos = min(m_pos.y,pos + m_pagesize); break;
	case TScrollBar::POS: break;
	}
	m_top = max(0,m_top + pos - old_pos);  // *fix 1.2.8 _must_ be +ve!
	scroll_bar()->set_pos(m_top);
	invalidate();
  }

  void set_read_buff(char *buff, int delim='\r', int nchar=-1)
  { m_read_buff = buff; m_delim = delim; m_nchar = nchar; }

  void size(int width, int height)
  {
   m_pagesize = height/m_char.y - 1;  //fiddle
   m_end.x = min(width/m_char.x - 1,COLUMNS);
  }
 

  void immediate_paint(bool yes)
  {
    if (yes != m_immediate) {
	  m_immediate = yes;
	  if (m_immediate) invalidate(); 
    }
  }

  void insert_mode(bool yes)
  {
    if (yes != m_insert_mode) {
	   m_insert_mode = yes;
	   m_cursor.set(this,
	       m_insert_mode ? m_char.x : default_cursor_width,m_char.y);
     }
  }

  void set_colour(long c)
  {
   m_write_colour = c; 
   m_buff.colour(m_pos.y, m_write_colour);
  }

  void clear_line()
  {
    m_buff.clear_line(m_pos.y);
	m_buff_begin = 0;
	m_pos.x = 0;
    m_last_end = 0;
	m_buff_end = 0;
    draw_buff(TClientDC(this),m_pos,Position(m_end.x,m_pos.y));
  }  

  // *fix 1.1.1 Command history list is now better behaved
  // *fix 1.2.8 Fails miserably if there's nothing in the history list.
  void history_list(bool up)
  {
    if (m_history.size()==0) return;
    if(!up){
	  ++m_hlp;
      if (m_hlp == m_history.end()) { --m_hlp; return; }
	}
	clear_line();
    putchars(m_hlp->c_str(),false,true);
	if (up) {
	  if (m_hlp == m_history.begin()) return;
	  --m_hlp;
    }
  }

  void add_to_history_list(char *line)
  {
    m_history.push_back(line);
	m_hlp = m_history.end();
	--m_hlp;
	if (m_history.size() > MAX_HISTORY) m_history.pop_front();
  }

  int xc2pix(int x) { return x*m_char.x; }
  int yc2pix(int y) { return (y-m_top)*m_char.y; } 
  int pix2xc(int x) { return x/m_char.x; }
  int pix2yc(int y) { return y/m_char.y + m_top; }
  int xpos()  { return xc2pix(m_pos.x);  }
  int ypos()  { return yc2pix(m_pos.y);  }

  void cursor_pos() 
  {
  	 if (m_immediate) m_cursor.set_pos(xpos(),ypos());
  }

  void focus(bool yes)
  {
    if (yes) {
	  m_cursor.create();
      cursor_pos();
	  m_cursor.show();
    } else {
      m_cursor.hide();
	  m_cursor.destroy();
	}
  }

  void draw_buff(TDC& dc, Position& s1, Position& s2, bool invert=false)
  {
    m_cursor.hide();
	if(invert) {
       dc.set_text_colour(white);
       dc.set_back_colour(0,0,0);
	}
	// *fix 1.1.1 limit width of _drawable_ buffer!
	int width = min(s2.x - s1.x, COLUMNS);  
    for(int iy = s1.y;  iy <= s2.y; iy++) {
	  int x = xc2pix(s1.x), y = yc2pix(iy);
	  char *str = &m_buff(s1.x,iy);
	  if (!invert && m_buff.colour(iy) != m_colour) {
	     m_colour = m_buff.colour(iy);
		 dc.set_text_colour(m_colour);
      }
	  dc.text_out(x,y,str,width);
    }
	if (invert) { // reset background
	    dc.set_back_colour(1,1,1);  // really we shd choose this...
        dc.set_text_colour(m_colour);
    }
    m_cursor.show();
  }

  void paint(TDC& dc)
  {
  // + 1 to account for end!
    draw_buff(dc,
		Position(0,m_top),
		Position(m_end.x,m_top + m_pagesize + 1)
	);//m_start,m_end);
  }

   // mouse management
  void mouse_down(Point& pt)
  {
	curr_pt = pt;
	curr_tm = current_time();
	curr_ln = pix2yc(pt.y);
	old_end_ln = curr_ln;
  }

  // *add 1.1.1 double-click on error line opens up Notepad!
  // *add 1.1.2 uses Metapad's '/g' option! Really shd make editor an option..
  void mouse_double_click(Point& pt)
  {
    char temp[COLUMNS], *file;
	int ln;
    m_buff.copy_buffer(temp,curr_ln,0);
    file = _strdup(strtok(temp," "));  
	
	ln = atoi(strtok(NULL,":"));
	sprintf(temp,"metapad /g %d:1 %s",ln,file); 
	exec(temp);
  }

  void mouse_move(Point& pt)
  {
    if (is_dragging) {
	   end_ln = pix2yc(pt.y);
	   if (end_ln != old_end_ln) { 
         draw_buff(TClientDC(this),
		              Position(0,      min(old_end_ln,end_ln)),
		              Position(m_end.x,max(old_end_ln,end_ln)),
					  end_ln > old_end_ln);
		 old_end_ln = end_ln;
       }
 	 } else
     if (get_key_state(VK_LBUTTON) && current_time() - curr_tm > DRAG_WAIT) {
	   is_dragging = true;       
     }
   }

  void mouse_up(Point& pt)
  {
    if (is_dragging) {
	  is_dragging = false; 
    }
  }

  void right_mouse_down(Point& pt)
  {
    // shd be a menu!
	copy_selection();
  }


  void keydown(int vkey)
  {
    switch(vkey) {
	case VK_HOME:   m_pos.x = m_buff_begin;            break;
	case VK_END:    m_pos.x = m_buff_end;              break;
	case VK_LEFT:   m_pos.x = max(m_pos.x-1,0);        break;
	case VK_RIGHT:  m_pos.x = min(m_pos.x+1,m_end.x);  break;
// *fix 1.2.8 page up/down better behaved - using actual m_top
	case VK_PRIOR:  vscroll(TScrollBar::PAGEUP,m_top);     break;
	case VK_NEXT:   vscroll(TScrollBar::PAGEDOWN,m_top);   break;
	case VK_UP:     history_list(true);                break;
	case VK_DOWN:   history_list(false);               break;
	case VK_INSERT:
	   insert_mode(! m_insert_mode);
	   break;
	case VK_DELETE: 
	   m_buff.remove(m_pos);
	   draw_buff(TClientDC(this),m_pos,Position(m_end.x,m_pos.y));
	   break;
	}
	cursor_pos();
  }

  void check_row()
  {
   if (m_pos.y > m_top + m_pagesize)  { // run out of window
	     m_top++;
		 scroll(0,-m_char.y);
   }
  }


  void process_char(int ch, bool from_input)
  {
    // <enter> is actually '\r', so we massage non-input chars
  	// probably better to do it via the keydown handler?
    if (!from_input && ch=='\n') ch = '\r';
	int last_line = m_pos.y;
    switch(ch) {
	case CTRL_C: // ASCII 3
      copy_selection();
	  break;
    case CTRL_V:
      paste_into();
	  break;
	case '\b': // backspace
	  if (m_pos.x > 0) {
	     m_pos.x--;
		 m_buff_end--;
		 m_last_end = m_buff_end;
		 keydown(VK_DELETE); 
      } break;
    case '\t': // *change 1.2.4 Tabs now 8 chars...
	  do {process_char(' ',from_input);} while(m_pos.x % 8 != 0);
	  break;
    case '\r':
	  m_pos.x = 0;  // and fall through
	  m_last_end = m_buff_end;
	  m_buff_end = 0;
    case '\n':
      m_pos.y++;
	  kount++;
	  m_buff.reserve(m_pos.y); // make sure buffer is big enough!
	  m_buff.colour(m_pos.y, m_write_colour);
	  check_row();
	  scroll_bar()->set_pos(m_pos.y);
      m_last_line = max(m_last_line,m_pos.y);
	  insert_mode(false);
	  if (from_input && m_nchar==1) { // special case of waiting for a single char...
	      *m_read_buff='\n';
		  quit_loop();
      }
	  break;
	default:
	  int xend;
	  if (m_insert_mode && m_pos.x < m_buff_end) {
	      m_buff.insert(m_pos);
		  xend = m_buff_end;
      } else xend = m_pos.x+1;
	  m_buff(m_pos.x, m_pos.y) = ch;
	  if (m_immediate) draw_buff(TClientDC(this),m_pos,Position(xend,m_pos.y));
	  m_pos.x ++;
	  m_last_end = m_buff_end;
	  m_buff_end ++;
	  if (m_pos.x > m_end.x) {
          m_pos.x = 0;	  
		  m_pos.y++;
		  check_row();
		  if (m_pos.y > m_end.y) m_pos.y = 0;
       }
       break;
    }
	if (from_input) {
	// two input scenarios: either we hit the delimiter, 
	// or we've read m_nchar chars in.
	 bool hit_delim = ch==m_delim;
	 bool got_nchar = m_nchar != -1 && m_pos.x - m_buff_begin==m_nchar;
	 if (m_nchar > 0 && ch == '\r') m_buff_begin = 0;  // we don't pick it up?
     if (m_read_buff && (hit_delim || got_nchar)) {
	    int last_pos = m_last_end;
	 	if(hit_delim) {
		  /*if(m_delim != '\r')*/ last_pos--; // don't copy delim!
        }
  	    m_buff.copy_buffer(m_read_buff,last_line,m_buff_begin,last_pos);
		m_buff_begin = m_pos.x;
		if (hit_delim) {
		  add_to_history_list(m_read_buff);
  		  m_last_end = 0;
        }
	    quit_loop();
      } 
    }
	cursor_pos();
  }	 

  void on_char(int ch, int rpt)
  {
    for(int i = 0; i < rpt; i++) {
	 process_char(ch,true);
    }
  }     

  void clear()
  {
     m_buff.clear();
     reset_pos();
	 scroll_bar()->set_pos(m_pos.y);	 
  }

  void putchars(char *str, bool endline, bool input)
  {
   try {
    if(!input) m_immediate = false;
    for(; *str != '\0'; str++) process_char(*str,input);
	if (endline) process_char('\r',input);
	if (!input) {
	  m_buff_begin = m_pos.x;
	  m_immediate = true;
	  invalidate();
	  cursor_pos();
    }
   } catch(...) {
     Engine::kill(-1);
	 message("Buffer overflow\nProgram failed to terminate?");
	 clear();
   } 
  }

  int last_row() { return m_pos.y; }


};

static ConsoleWin *conw[10];
static bool is_eof;
static int curr_id;
static bool mCreateDefered = false;

ConsoleWin *wcon()
{ 
 if (Program::in_main_thread()) return conw[0];
 else {
  if (mCreateDefered)  { conw[curr_id]->show(); mCreateDefered = false; }
  return conw[curr_id];
 }
}

void wcon_prompt_char(char ch, int col)
// setting ch to 0 will disable prompt stripping
// UCW uses (';',0);  DOS wd use (':',1).
{ gPromptChar = ch; gPromptCol = col; }

int wcon_lines()
{ return wcon()->last_row(); }

char *wcon_gets(char *buff)
{
  wcon()->set_read_buff(buff);
  int iret = wcon()->run();
  is_eof = iret !=-1; // i.e. a normal windows exit!
  return buff;
}

void wcon_puts(char *buff)
{
 wcon()->putchars(buff,true,false); 
}

void wcon_puttext(char *buff)
{
 wcon()->putchars(buff,false,false);
}

int wcon_getch()
{
   char chb[2];
   wcon()->set_read_buff(chb,'\0',1);    
   int iret = wcon()->run();
   is_eof = iret !=-1; // i.e. a normal windows exit!
   return is_eof ? -1 : chb[0];
}

char wcon_obuff[256];

int wcon_vprintf(char *fmt,va_list args)
{
 int ich = vsprintf(wcon_obuff,fmt,args);
 wcon_puttext(wcon_obuff);
 return ich;
}


int wcon_printf(char *fmt,...)
{
  va_list ap;
  int ich;
  va_start(ap,fmt);
  ich = wcon_vprintf(fmt,ap);
  va_end(ap);
  return ich;
}

int wcon_fprintf(FILE *out, char *fmt,...)
{
 va_list ap;
 int ich;
 va_start(ap,fmt);
 if (out==stdout || out==stderr) ich = wcon_vprintf(fmt,ap);
 else if (out==_str_out) ich = str_vprintf(fmt,ap); // NOTE(10)
 else ich = vfprintf(out,fmt,ap);
 va_end(ap);
 return ich;
}

static char *ptr = "";

char *wcon_getter(char *p)
{
  static char ibuff[150];
  if (p != NULL) ptr = p;
  else {
    ptr = skip_ws(ptr);
    if (*ptr == '\0') {
     wcon_gets(ibuff);
	 ptr = ibuff;
    }
  }
  return ptr;
}

int wcon_scanf(char *fmt,...)
{
  va_list ap;
  int ret;
  va_start(ap,fmt);
  ret = ex_vfscanf(wcon_getter,fmt,ap);
  va_end(ap);
  return ret;
}

int wcon_fscanf(FILE *in, char *fmt,...)
{
  va_list ap;
  int ret;
  va_start(ap,fmt);
  ret = ex_vfscanf(in == _str_in ? str_getter : wcon_getter,fmt,ap);
  va_end(ap);
  return ret;
}

char *wcon_fgets(char *buff, int sz, FILE *in)
{
 if(in==stdin) wcon_gets(buff); 
 else fgets(buff,sz,in);
 return buff;
}

int wcon_eof()
{ return is_eof; }

ConsoleWin *curr_cw() { return conw[curr_id]; }

void *wcon_window() { return curr_cw(); }

void wcon_copy_to_log(char *file)
{
  curr_cw()->copy_out(ofstream(file),0);
}

void *wcon_window_handle()
{
  return curr_cw()->handle();
}

void wcon_set_title(char *title, bool do_main)
{ 
  if (do_main) conw[0]->set_text(title);
  else curr_cw()->set_text(title);
}

void wcon_set_colour(float r, float g, float b)
{ curr_cw()->set_colour(RGBF(r,g,b)); }


void wcon_set_console(int idx)
{
 curr_id = idx;
}

void wcon_clear(int mode)
{
 if (mode==0) {// clear input
    ptr = "";
   // wcon_getter(NULL);	
 } else
    curr_cw()->clear();
}

void wcon_bring_to_front()
{
  wcon()->on_top();
}

void* wcon_handle()
{
  return wcon()->handle();
}

// *fix 0.9.4 Console window is _not_ made visible automatically; must use wcon_set_size().
// *add 0.9.7 'Defered' create - this is a hack until we can find a more _elegant_ solution...
int wcon_create(int rows, int cols, bool defered)
{
 static int last_id = 0;
 mCreateDefered = defered;
 conw[last_id] = new ConsoleWin(rows,cols);
 wcon_set_console(last_id);
 return last_id++;
}

void wcon_set_size(int x0, int y0, int w, int h)
{
 curr_cw()->resize(x0,y0,w,h);
 if (! mCreateDefered) curr_cw()->show();
}

void wcon_destroy(int id)
{
 conw[id]->close();
//] delete conw[id];
 conw[id] = NULL;
 if (curr_id == id) wcon_set_console(0); // main console always available
}

static bool mPaused = false;

bool wcon_paused()
{
 return mPaused;
}

void wcon_pause()
{
 mPaused = true;
 conw[curr_id]->run();
}

void wcon_resume()
{
 mPaused = false;
 conw[curr_id]->quit_loop();
}

void wcon_set_parent(void *parent)
{
  sConsoleParent = new TWin(parent);
}

bool WConIstream::fetch_line(char *buff, int sz)
{
 wcon()->set_colour(0);  // force input always in black, for now
 wcon_gets(buff);
 return true;
}

WConOstream::WConOstream(float r, float g, float b)
{
 m_colour = RGBF(r,g,b);
 m_redirect = NULL;
}

int WConOstream::write(char *buff, int n)
{
 if (m_redirect != NULL && Program::in_main_thread() == m_main_console) {
   m_redirect->write(buff,n);
 } else {
   wcon()->set_colour(m_colour);
   wcon_puttext(buff);
 }
 return 1;
}

bool WConOstream::set_redirect(ostream* pos, bool main_console)
{
 bool res = m_redirect != pos; 
 m_redirect = pos; m_main_console = main_console;
 return res;
}

ostream* WConOstream::get_redirect() const { return m_redirect; }

bool wcon_redirect_output(WConOstream& out, ostream* pos, bool main_console)
{
   return out.set_redirect(pos,main_console);
}

bool wcon_redirect_off(WConOstream& out)
{
   delete out.get_redirect();
   return out.set_redirect(NULL);
}

bool wcon_is_redirected(const WConOstream& out)
{ 
   return out.get_redirect() != NULL;
}

WConIstream wcon_cin;
WConOstream wcon_cout(0,0,0),     //black
            wcon_cerr(1,0,0),     //red
			wcon_cmsg(0,0.5,0);   //dark green

extern int main(int,char**);

int SMain(int argc,char **argv) // YAWL main program (see TWL.cpp)
{
 int id = wcon_create(ROWS,COLUMNS);
 int iret = 0;
#ifndef _USRDLL 
 iret = main(argc,argv);
 wcon_destroy(id);
#endif
 return iret;
}
