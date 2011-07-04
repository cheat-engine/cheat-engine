#ifndef __IDE_H
#define __IDE_H
namespace IDE  {
long cmd(int cmd, long extra=0);
bool reconnect();
void initialize();
void message();
void ucw_shut_down();
void finished();
void breakpoint();
void error();
void bring_to_front(void* handle);
void set_blocking(bool yesno);
}
#endif

