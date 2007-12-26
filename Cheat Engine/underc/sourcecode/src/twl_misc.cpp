// TWL_MISC.CPP
#include <windows.h>
#include "twl.h"
#include "twl_misc.h"

// The Caret (text cursor)

TCaret::TCaret()
: m_created(false)
{
}

void TCaret::set(TWin *win,int width, int height)
{
  m_win = win; m_width = width; m_height = height;
  if (m_created) {
     hide();
     destroy();
     create();
     show();
  }
}

void TCaret::create()
{
  CreateCaret((HWND)m_win->handle(),NULL,m_width,m_height);
  m_created = true;
}

void TCaret::destroy()
{
  DestroyCaret();
  m_created = false;
}

void TCaret::set_pos(int ix, int iy)
{
  SetCaretPos(ix,iy);
}

void TCaret::show()
{
  ShowCaret((HWND)m_win->handle()); 
}

void TCaret::hide()
{
  HideCaret((HWND)m_win->handle());
}

//// Clipboarding...
TClipboard::TClipboard(TWin *win)
 : m_owner(win),m_clipboard_open(false)
{
}

TClipboard::~TClipboard()
{
 close();
}

void TClipboard::close()
{
 if(m_clipboard_open) CloseClipboard();
 m_clipboard_open = false;
}

void TClipboard::open()
{
 OpenClipboard((HWND)m_owner->handle());
 m_clipboard_open = true;
}

char *TClipboard::buffer(int sz)
{
  m_buff_hand = GlobalAlloc(GHND,sz+1);
  return (char *)GlobalLock(m_buff_hand);
}

void TClipboard::write(char *buff/*=NULL*/)
{
 if(buff != NULL) {
   char *clip_ptr = buffer(lstrlen(buff));
   lstrcpy(clip_ptr,buff);
 }
 GlobalUnlock(m_buff_hand);
 open();
 EmptyClipboard();
 SetClipboardData(CF_TEXT,m_buff_hand);
 close();
}

char *TClipboard::read()
{
  open();
  m_buff_hand = GetClipboardData(CF_TEXT);
  if(m_buff_hand == NULL) return NULL;  // nothing to read!
  return (char *)GlobalLock(m_buff_hand);
}

