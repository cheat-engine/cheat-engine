SFX = 5.1
PROJNAME = lua$(SFX)
APPNAME = bin2c$(SFX)
APPTYPE = console
OPT = Yes
USE_NODEPEND = Yes    
NO_SCRIPTS = Yes

SRC = ../etc/bin2c.c

ifneq ($(findstring Win, $(TEC_SYSNAME)), )
  SRC += lua_simple.rc 
else
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    SRC += lua_simple.rc
  endif
endif
