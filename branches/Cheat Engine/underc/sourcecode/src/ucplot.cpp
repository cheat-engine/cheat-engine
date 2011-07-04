// UCPLOT.CPP
#include "twl.h"
#include "threads.h"
#define CEXPORT extern "C" __declspec(dllexport)

class PlotThread: public Thread {
protected:
    TFrameWindow *m_fw;
public:
    PlotThread(char *caption)
    {
      m_fw = new TFrameWindow(caption);
    }

    TFrameWindow *window() { return m_fw; }
    TDC* dc() { return m_fw; }

    int execute()
    {   
     m_fw->resize(1,1,400,400); 
     m_fw->show();
     m_fw->run();
     delete m_fw;
     return 0;
    }
};

static PlotThread *plt = NULL;

void force_entry(); // in twl.cpp

CEXPORT int uc_plot_start(char *title) 
{
force_entry();
plt = new PlotThread(title);
plt->resume(); 
 return (int)plt;
}

CEXPORT int uc_plot_end() 
{
 plt->window()->close();
 delete plt;
 plt = NULL;
 return 0;
}


CEXPORT int uc_resize(int x, int y, int w, int h)
{
 plt->window()->resize(x,y,w,h); 
 return 0;
}

CEXPORT int uc_move_to(int x, int y)
{
  plt->dc()->get();
  plt->dc()->move_to(x,y);
  plt->dc()->release();
  return 0;
}

CEXPORT int uc_line_to(int x, int y)
{
  plt->dc()->get();  
  plt->dc()->line_to(x,y);
  plt->dc()->release();
  return 0;
}

