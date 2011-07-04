# Makefile for console-build UnderC C++ interpreter
# vs 1.2.4 Can also build as a shared library, if SO is set
# vs 1.2.9 Builds with Mingw 2 (GCC 3.2)

OBJ = tparser.o function_match.o class.o code.o common.o \
      directcall.o dissem.o engine.o expressions.o \
      function.o breakpoints.o tokens.o keywords.o \
      mangle.o mstring.o operators.o os.o subst.o \
      table.o templates.o hard_except.o  \
      types.o program.o uc_tokens.o lexer.o main.o \
      errors.o utils.o imports.o ex_vfscanf.o ucri.o

NAME = ucc
DEFINES = -D_CONSOLE
NO_READLINE=1

ifdef DEBUG
CFLAGS = -g -Wno-deprecated -D__GTHREAD_HIDE_WIN32API
else
CFLAGS = -O1 -Wno-deprecated -D__GTHREAD_HIDE_WIN32API
ifdef DYNAMIC
LFLAGS = -Wl,-E
endif
endif

ifndef NO_READLINE
DEFINES := $(DEFINES) -D_USE_READLINE
LFLAGS := $(LFLAGS) -lreadline -lncurses
ifndef BSD
LFLAGS := $(LFLAGS) -ldl
endif
endif

ifdef STATIC
LFLAGS := $(LFLAGS) -static
endif

ifdef SO
LFLAGS := $(LFLAGS) -shared
OBJ := $(OBJ) dll_entry.o
# DEFINES = $(DEFINES) -D_USRDLL
DEFINES = -D_USRDLL -D_USE_READLINE
NAME = ucc124.so
endif

ifdef GTK
LFLAGS := $(LFLAGS) `gtk-config --libs`
DEFINES = -D_CONSOLE -D_USE_READLINE  -D_LINK_GTK
endif

ucc: $(OBJ)
	c++ -o $(NAME) $(OBJ) $(LFLAGS)

%.o: %.cpp
	c++ -c $(CFLAGS) $(DEFINES) $<

tparser.cpp: parser.y
	bison -y -d parser.y; cp y_tab.c tparser.cpp; cp y_tab.h tparser.h

tparser.o: tparser.cpp

clean: 
	rm *.o

lib:
	g++ -o ../lib/string_imp.dll -shared -Wl,-export-dynamic ../lib/string_imp.cpp

function_match.o: function_match.cpp function_match.h \
       common.h templates.h

class.o: class.cpp class.h common.h \
   directcall.h operators.h opcodes.h tparser.h std_utils.h

code.o: code.cpp code.h common.h opcodes.h tparser.h

common.o: common.h function_match.h tparser.h code.h \
	opcodes.h engine.h

directcall.o: directcall.cpp directcall.h common.h \
        opcodes.h directcall.h mangle.h hard_except.h os.h

dissem.o: dissem.cpp common.h \
        opcodes.h breakpoints.h	

engine.o: engine.cpp engine.h common.h errors.h \
        opcodes.h directcall.h breakpoints.h hard_except.h

expressions.o: expressions.cpp expressions.h common.h \
         function_match.h tparser.h operators.h directcall.h

function.o: function.cpp function.h common.h opcodes.h \
         module.h
  
breakpoints.o: breakpoints.cpp breakpoints.h common.h \
         module.h opcodes.h tokens.h std_utils.h os.h

tokens.o: tokens.cpp tokens.h utils.h

keywords.o: keywords.cpp common.h \
       tparser.h

mangle.o: mangle.cpp mangle.h common.h

operators.o: operators.cpp operators.h common.h \
        tparser.h

os.o: os.cpp os.h

subst.o: subst.cpp

table.o: table.cpp table.h

templates.o: templates.cpp templates.h common.h \
         tparser.h std_utils.h

hard_except.o: hard_except.cpp hard_except.h

types.o: types.cpp types.h common.h \
       templates.h

program.o: program.cpp program.h common.h \
       module.h errors.h utils.h

uc_tokens.o: uc_tokens.cpp uc_tokens.h common.h \
       module.h version.h errors.h

lexer.o: lexer.cpp common.h \
      tparser.h keywords.h operators.h input.h uc_tokens.h

main.o:  main.cpp common.h \
      breakpoints.h module.h uc_tokens.h engine.h mangle.h \
      directcall.h keywords.h operators.h program.h utils.h \
      input.h version.h errors.h

errors.o: errors.cpp errors.h common.h \
      input.h

imports.o: imports.cpp imports.h common.h \
      mangle.h os.h

ucri.o: ucri.cpp module.h directcall.h program.h imports.h

dll_entry.o: dll_entry.cpp module.h engine.h main.h errors.h directcall.h input.h

ex_vfscanf.o: ex_vfscanf.h

utils.o: utils.cpp utils.h classlib.h

mstring.o: mstring.cpp mstring.h iostrm.h





 










