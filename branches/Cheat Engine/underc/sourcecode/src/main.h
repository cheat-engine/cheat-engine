/* main.h
 * Main Program, initialization, and # command implementation
 * UnderC C++ interpreter
 * Steve Donovan, 2001
 * This is GPL'd software, and the usual disclaimers apply.
 * See LICENCE
 */

namespace Main {
 void initialize();
 bool process_command_line(int& argc, char**& argv);
 void banner();
 void finalize();
 int interactive_loop();
 char *uc_exec_name();
 string uc_lib_dir();
};

