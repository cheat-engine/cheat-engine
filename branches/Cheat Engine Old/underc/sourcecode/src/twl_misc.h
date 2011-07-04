// twl_misc.h
// Things for which we need to find a home!
#ifndef __TWL_MISC
#define __TWL_MISC

class TCaret {
protected:
  TWin *m_win;
  int m_width, m_height;
  bool m_created;
public:
  TCaret();
  void set(TWin *win, int width, int height);
  void create();
  void destroy();
  void set_pos(int ix, int iy);
  void show();
  void hide();
};

class TClipboard {
protected:
  TWin *m_owner;
  bool m_clipboard_open;
  Handle m_buff_hand;
public:  
    TClipboard(TWin *win);
    ~TClipboard();
    void close();
    void open();
    char *buffer(int sz);
    void write(char *buff=NULL);
    char *read();
};
#endif

