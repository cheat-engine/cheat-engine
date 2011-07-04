// utils.h

#ifndef __UTILS_H
#define __UTILS_H

namespace Utils {
 char *quote_strtok(char *str);
 void strip_last(char *path);  
 bool is_qualified_path(const char *path);
 char *get_curr_dir();
 void change_dir(char *dir);
 void check_path_end(string& s);
 bool extract_relative_path(string& path, string& dir);
 string full_path(string s);
 string file_extension(string name);
 string get_filepart(string s, bool strip_extension);
 bool can_access(string path);
 bool is_absolute_path(string& path);

 class Args {
private:
  int& m_argc;
  char** m_argv;
  int m_last_idx;
  char* m_optstr;
  bool m_stop_after_file;
public:
  Args(int& argc, char** argv);
  void   set_stop_after_file() { m_stop_after_file = true; }
  bool   delete_arg(int idx);
  bool   get_opt(char *opt);
  char*  get_opt_parameter();
  char** append_extra_args(char *arg_str);
};

}


#endif

