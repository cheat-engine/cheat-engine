# Common Tecmake definitions for Lua 5.1

SFX = 5.1
PROJNAME = lua$(SFX)
OPT = Yes           # Turn optimization on
USE_NODEPEND = Yes  # Disable dependencies
NO_SCRIPTS = Yes    # Disable scripts and batch files creation
NO_DYNAMIC = no     # Build also dynamic libraries with gcc in Windows

DEFINES = 
                          
# All non-Windows (posix)                         
ifeq ($(findstring Win, $(TEC_SYSNAME)), )
  DEFINES += LUA_USE_POSIX LUA_USE_DLOPEN
  
  # Linux Only
  ifneq ($(findstring Linux, $(TEC_UNAME)), )
    DEFINES += LUA_USE_READLINE
  endif
  
  # BSD Only
  ifneq ($(findstring BSD, $(TEC_UNAME)), )
    DEFINES += LUA_USE_READLINE
  endif
  
  # MacOS X
  ifneq ($(findstring Darwin, $(TEC_UNAME)), )
    DEFINES += LUA_USE_READLINE
  endif
  
  # Cygwin Only (POSIX build)
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    DEFINES += LUA_USE_READLINE
  endif
endif


# For Executables only
ifdef APPNAME
  # Disable strip
  STRIP = 
  
  APPTYPE = console

  ifneq ($(findstring Win, $(TEC_SYSNAME)), )
    SLIB += setargv.obj
    
    # In Windows, use the Dynamic Libraries
    USE_DLL = Yes
    LIBS += lua$(SFX)
    ifeq "$(TEC_SYSNAME)" "Win32"
      LDIR += ../lib/dll8
    else
      LDIR += ../lib/dll8_64
    endif
  else
    ifneq ($(findstring cygw, $(TEC_UNAME)), )
      LIBS += lua$(SFX)
      LDIR += ../lib/$(TEC_UNAME)   
      LDFLAGS = -s
    else
      #UNIX Use Static Libraries
      SLIB += ../lib/$(TEC_UNAME)/liblua$(SFX).a
    endif
  endif
  
  ifneq ($(findstring Linux, $(TEC_UNAME)), )
    LIBS += dl 
    LFLAGS = -Wl,-E
    LIBS += readline history curses ncurses
  endif
  
  ifneq ($(findstring BSD, $(TEC_UNAME)), )
    LFLAGS = -Wl,-E
    LIBS += readline history curses ncurses
  endif
  
  ifneq ($(findstring Darwin, $(TEC_UNAME)), )
    LIBS += readline
  endif
  
  ifneq ($(findstring SunOS, $(TEC_UNAME)), )
    LIBS += dl 
  endif
  
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    LIBS += lua$(SFX) readline history
    SLIB =
    LDIR += ../lib/$(TEC_UNAME)   
  endif
endif
