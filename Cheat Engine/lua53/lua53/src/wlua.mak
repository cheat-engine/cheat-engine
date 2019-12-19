SFX = 53
APPNAME = wlua$(SFX)

SRC = lua.c wmain.c

include lua_conf.mak

APPTYPE = windows

ifneq ($(findstring Win, $(TEC_SYSNAME)), )
  SRC += wlua.rc 
  GEN_MANIFEST = No
endif
