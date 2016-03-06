SFX = 53
APPNAME = luac$(SFX)

SRC = luac.c

include lua_conf.mak

ifneq ($(findstring Win, $(TEC_SYSNAME)), )
  # In Windows, use the Static Libraries, overwrite the definitions in lua_conf.mak
  USE_DLL =
  LDIR = ../lib/$(TEC_UNAME)
  SRC += lua_simple.rc 
endif
