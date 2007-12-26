// turtle.h

#ifndef _TURTLE_H
#define _TURTLE_H

#include <string>

class TG {
private:
int win;
int W, H;
int gPx,gPy,gPw,gPh;
double gX1,gX2,gY1,gY2;
double gSx, gSy, gCx, gCy;
bool gPen, gShow;
double gSin, gCos, gX, gY,gOldX,gOldY,gOldSin,gOldCos;
double gOldAngle, gAngle;
double gCursorSize;
/*static const*/ double RADCON;
int gIdx;
double gPoints[40];

public:

void rescale()
{
 W = ucw_cmd(win,"w");
 H = ucw_cmd(win,"h");
 gSx = W/(gX2 - gX1);
 gSy = H/(gY2 - gY1);
 gCx = W - gSx*gX2;
 gCy = H - gSy*gY2;
 gX = 0;
 gY = 0;
 gCursorSize = (gX2 - gX1)/30;
}

void scale(double x1, double x2, double y1, double y2)
{
 gX1 = x1;  gX2 = x2;  gY1 = y1;  gY2 = y2;
 rescale();
}

int scalex(double x)
{
  return gCx + gSx*x + 0.5;
}

int scaley(double y)
{
  return H - (gCy + gSy*y + 0.5);
}

void pen(bool up = true)
{
  gPen = up;
}

void penup() { pen(false); }

void plot(double x, double y)
{
 int ix = scalex(x), iy = scaley(y);
 gX = x;  gY = y;
 if (gPen) ucw_line_to(win,ix,iy);
 else { 
  ucw_move_to(win,ix,iy);
  pen();
 }
}

void fcolor(double r, double g, double b)
{
  ucw_fcolour(win,r,g,b);
}

void bcolor(double r, double g, double b)
{
  ucw_bcolour(win,r,g,b);
}

static unsigned long rgb(int ri, int rg, int rb)
{
  return (unsigned long) ucw_rgb(ri,rg,rb);
}

void set_color(unsigned long clr, bool fground)
{
  ucw_set_colour(win,(long)clr,(int)fground);
}

void rectangle(double x1, double y1, double x2, double y2)
{
  ucw_rectangle(win,scalex(x1),scaley(y1),scalex(x2),scaley(y2));	 	 
}

void ellipse(double x1, double y1, double x2, double y2)
{
  ucw_ellipse(win,scalex(x1),scaley(y1),scalex(x2),scaley(y2));
}

void start_poly() {
  penup();
  gIdx = 0;
}

void add(double x, double y) {
  gPoints[gIdx++] = x;
  gPoints[gIdx++] = y;
}

void do_poly() {
 int i;
 add(gPoints[0],gPoints[1]);
 double x0 = gX, y0 = gY;
 for(i = 0; i < gIdx; i+=2) {
   double x = gPoints[i] - x0, y = gPoints[i+1] - y0;
   double sx = gCos*x - gSin*y + x0;
   double sy = gSin*x + gCos*y + y0;
   plot(sx,sy); 
 }

}

void draw_cursor(double sz, double x, double y) {
  start_poly();
  add(x-sz,y+sz);
  add(x+sz,y);
  add(x-sz,y-sz);
  do_poly();
  penup();
  plot(x,y);
}

void update_cursor() {
  ucw_cmd(win,"X");
  ucw_fcolour(win,1,1,1);
  draw_cursor(gCursorSize,gX,gY);
  ucw_cmd(win,"X");
  ucw_fcolour(win,0,0,0);
}

void turn(double angle)
{
 if(gShow) update_cursor();
 gAngle += angle;  // note the -ve sign! ??
 double rads = gAngle*RADCON;
 gSin = sin(rads);
 gCos = cos(rads);
 if(gShow) update_cursor();
}

void left() { turn(90); }
void right() { turn(-90); }

void draw(double len)
{
  if(gShow) update_cursor();
  gX += gCos*len;
  gY += gSin*len;
  plot(gX,gY);
  if(gShow) update_cursor();
 }

void move(double len)
{
 penup();
 draw(len);
}

void show(bool yes=true)
{
  if (yes != gShow) {
    update_cursor();
    gShow = yes;      
  }
}

void text(char *msg) {
 double old_x = gX, old_y = gY;
 ucw_text_out(win,msg);
 penup();  plot(old_x,old_y);
}

//using namespace std;
using std::string;
void text(const string& s) {
  text(s.c_str());
}

void resize(int x0=-1, int y0=-1, int w=0, int h=0, char *title=NULL)
{
 if (x0 > 0) {
  gPx = x0;  gPy = y0;
  if (w > 0) { gPw = w; gPh = h; }
 }
 if (title)
   win = ucw_create_window(title,gPx,gPy,gPw,gPh);
 else	  
   ucw_size_window(win,gPx,gPy,gPw,gPh);
} 

void on_top() {
  ucw_cmd(win,"T");
}

void go_to(double x, double y) {
  if(gShow) update_cursor();
  penup();
	plot(x,y);
  if(gShow) update_cursor();
}

void reset_angle() {
 gAngle = 0.0;
 gSin = 0.0;  gCos = 1.0;
}

int width()  { return ucw_cmd(win,"W"); }
int height() { return ucw_cmd(win,"H"); } 

void setup(double vmin=100,double vmax=100)
{
 gShow = false;
 ucw_rectangle(win,0,0,width(),height());
 reset_angle();
 scale(0,vmin,0,vmax);
}

TG(char *_name,int _W = 400, int _H = 400)
{
  resize(450,200,_W,_H,_name);
  RADCON = 0.01745;
  setup();
}

void init()
{
  on_top();
  rescale();
  ucw_fcolour(win,0,0,0);
  go_to((gX2+gX1)/2,(gY2+gY1)/2);
  penup();
  left();
  pen(true);
}

};

struct TG_State {
  double mOldX,mOldY,mOldAngle,mOldSin,mOldCos;
  TG& mTg;

  TG_State(TG& tg) : mTg(tg) {}

  void save()
  {
  mOldX = mTg.gX;  mOldY = mTg.gY;
  mOldSin = mTg.gSin;  mOldCos = mTg.gCos;
  mOldAngle = mTg.gAngle;
  }

  void restore()
 {
  mTg.gSin = mOldSin;  mTg.gCos = mOldCos;
  mTg.gAngle = mOldAngle;
  mTg.go_to(mOldX,mOldY);  // move back to original pos....
 }
}; 

#endif

  






