// uc_graphics.h
// Simple built-in graphics library

#ifndef __UC_GRAPHICS_H
#define __UC_GRAPHICS_H

typedef int *UCWin;
UCWin ucw_create_window(char *title,int x0, int y0, int w, int h);
int ucw_title(UCWin win,char *title);
int ucw_size_window(UCWin win,int x0, int y0, int w, int h);
int ucw_move_to(UCWin win,int x, int y);
int ucw_line_to(UCWin win,int x, int y);
int ucw_text_out(UCWin win,char *msg);  // depends on CP!
int ucw_font(UCWin win, char *typeface, int size);
unsigned long ucw_rgb(int r, int g, int b);
void ucw_set_colour(UCWin win, unsigned long clr, bool fg);
int ucw_fcolour(UCWin win, float r, float g, float b);
int ucw_bcolour(UCWin win, float r, float g, float b);
int ucw_rectangle(UCWin win, int x1, int y1, int x2, int y2);
int ucw_ellipse(UCWin win, int x1, int y1, int x2, int y2);
int ucw_cmd(UCWin win, char *cmd);
#endif
