// program.h
namespace Program {
  enum RunType { new_thread, new_window, same_window };
  void stop_main(bool is_error);
  int  call_main(int argc, char **argv,RunType how=new_thread);
  bool run(char *cmdline,bool same_thread=false);
  bool in_main_thread();
  int run_int_function(int flags, int& retval);
  void compile_function(void *e);
  void pause_thread();
  bool thread_is_paused();
  void release_thread();
}


