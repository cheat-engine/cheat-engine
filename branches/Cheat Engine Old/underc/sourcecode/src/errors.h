#ifndef __ERRORS_H
#define __ERRORS_H
namespace Errors {
  char *get_redirect_buffer(int which);
  void redirect_output(bool to_buff, bool main_console = true);
  bool output_is_redirected();
  int  get_stop_position(char *filename);
  void set_halt_state(string msg, string fname, string file, int lineno, bool was_error);
  void reset_error_state();
  int check_output();
}
#endif

