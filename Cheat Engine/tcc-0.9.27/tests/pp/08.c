// test macro expansion in arguments
#define s_pos              s_s.s_pos
#define foo(x) (x)
foo(hej.s_pos)
