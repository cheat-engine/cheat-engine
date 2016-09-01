SFX = 53
APPNAME = lua$(SFX)

SRC = lua.c

include lua_conf.mak

ifneq ($(findstring Win, $(TEC_SYSNAME)), )
  SRC += lua.rc 
  GEN_MANIFEST = Yes
endif
