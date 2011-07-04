#include <iostream>
#include <windows.h>

class RestoreUCWin {

  RestoreUCWin()
  {
   HWND h = GetActiveWindow();
   if (! h) return;
   RECT rt;
   std::ifstream in("/_pos.txt");
   if (in) {
     in >> rt.left >> rt.top >> rt.right >> rt.bottom;
     MoveWindow(h,rt.left,rt.top,rt.right-rt.left,rt.bottom-rt.top,1);
   }
  }

  ~RestoreUCWin()
  {
    HWND h = GetActiveWindow();
    if (! h) return;
    RECT rt;
    GetWindowRect(h,&rt);
    std::ofstream out("/_pos.txt");
    out << rt.left << ' ' << rt.top << ' ' << rt.right << ' ' << rt.bottom << std::endl;
 }

};

RestoreUCWin __uc__;





