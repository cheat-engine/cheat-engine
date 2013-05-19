SFX = 5.1
APPNAME = luac$(SFX)

SRC = luac.c print.c 

include lua_conf.inc

ifneq ($(findstring Win, $(TEC_SYSNAME)), )
  # In Windows, use the Static Libraries, overwrite the definitions in lua_conf.inc
  USE_DLL =
  LDIR = ../lib/$(TEC_UNAME)
  SRC += lua_simple.rc 
else
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    SRC += lua_simple.rc
  endif
endif
