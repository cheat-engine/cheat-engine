// ENTRY.H

#ifndef __ENTRY_H
#define __ENTRY_H
#include "types.h"

struct Entry {
 string name;    // ref. to the actual name
 Type type;     // actual type
 RMode rmode;   // address mode
 int  data;     // usually an offset
 int  size;     // total size of object (useful for arrays)
 Table *context; // used to store a ptr to the owner of this entry!

 bool m_typename;
 int  m_access;

 bool is_typename()         { return m_typename; }
 bool is_constant()         { return name=="";   }
 bool is_class()            { return m_typename && size == 0; }
 bool is_typedef()          { return m_typename && size != 0; }
 bool is_namespace()        { return is_class() && data != 0; }
 void set_class()           { size = 0; }
 int  access()              { return m_access; }
 void set_access(int a)     { m_access = a; }
 void *global_ptr();
 char* object_ptr(char *obj);
 bool is_stack_relative()   { return rmode == SREL;}
 bool is_direct()           { return rmode==DIRECT; }
 bool is_object_relative()  { return rmode >= OREL; }
 bool is_bitfield()         { return rmode == OREL_F; }
 void set_bit_field(int offs, int bit_offs, int bit_size);
 void get_bit_field(int& offs, int& bit_offs, int& bit_size);

};
#endif

