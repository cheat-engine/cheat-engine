#ifndef __LOADED_MODULE_LIST_H
#define __LOADED_MODULE_LIST_H
namespace LoadedModuleList {
  void init();
  void init_module(int idx);
  void finish_module(Instruction* pi);
  void ODL_add(void* ptr, Function* dtor);
  void run_program_initialization();
  void run_program_finalization();
  void run_global_finalization();
};
#endif


