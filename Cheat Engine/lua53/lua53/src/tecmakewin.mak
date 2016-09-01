#-------------------------------------------------------------------------#
#- Tecmake  (Windows Version)                                            -#
#- Generic Makefile to build applications and libraries at TeCGraf       -#
#- The user makefile usually has the name "config.mak".                  -#
#-------------------------------------------------------------------------#

#---------------------------------#
# Tecmake Version
VERSION = 4.13


#---------------------------------#
# First target
.PHONY: build
build: tecmake


#---------------------------------#
# Location of this file
TECMAKE  = $(TECMAKE_HOME)/tecmakewin.mak


#---------------------------------#
# System Variables Definitions

# If tecmake.bat is not used,
# then at least define main system variables.

WIN32UNAMES = vc12 vc11 vc10 vc9 vc8 vc7 vc6 owc1 bc55 bc56 bc6 gcc3 gcc4 mingw3 mingw4 dllw4 dllg4 dll dll7 dll8 dll9 dll10 dll11 dll12
WIN64UNAMES = vc12_64 vc11_64 vc10_64 vc9_64 vc8_64 dll8_64 dll9_64 dll10_64 dll11_64 dll12_64 gcc4_64 mingw4_64 dllw4_64 dllg4_64

ifdef TEC_UNAME
  ifneq ($(findstring $(TEC_UNAME), $(WIN32UNAMES)), )
    TEC_WIN32 = Yes
  else
    ifneq ($(findstring $(TEC_UNAME), $(WIN64UNAMES)), )
      TEC_WIN64 = Yes
    endif
  endif
endif

ifdef TEC_WIN64
  TEC_SYSNAME=Win64
  TEC_SYSARCH=x64
  # This is not working, because make from Cygwin returns x86 even when running in AMD64.
  ifeq ($(PROCESSOR_ARCHITECTURE), x86)
    # Define this if compiling for 64-bits in a 32bits environment
    #USE_X86_CL64=XXX
  endif
else
  TEC_SYSNAME=Win32
  TEC_SYSARCH=x86
endif


#---------------------------------#
# System Info
.PHONY: sysinfo
sysinfo:
	@echo ''; echo 'Tecmake: System Info'
	@echo 'TEC_SYSNAME = $(TEC_SYSNAME)'
	@echo 'TEC_SYSARCH = $(TEC_SYSARCH)'
	@echo 'TEC_UNAME = $(TEC_UNAME)'
	@echo 'TEC_CC = $(TEC_CC)'; echo ''


#---------------------------------#
# Known platforms

UNAMES = $(WIN32UNAMES) $(WIN64UNAMES)


#---------------------------------#
# Directories Definitions

PROJDIR = ..
SRCDIR  = .
OBJROOT = $(PROJDIR)/obj


# ---------------------------------------------------------
# Byte Order and Word Size

ifneq ($(findstring x86, $(TEC_SYSARCH)), )
   TEC_BYTEORDER = TEC_LITTLEENDIAN
else
ifeq ($(TEC_SYSARCH), arm)
   TEC_BYTEORDER = TEC_LITTLEENDIAN
else
   TEC_BYTEORDER = TEC_BIGENDIAN
endif
endif

ifeq ($(TEC_SYSARCH), x64)
  TEC_BYTEORDER = TEC_LITTLEENDIAN
  TEC_WORDSIZE = TEC_64
else
  TEC_WORDSIZE = TEC_32
endif

# Itanium Exception
ifeq ($(TEC_SYSARCH), ia64)
  TEC_BYTEORDER = TEC_LITTLEENDIAN
  TEC_WORDSIZE = TEC_64
endif


#---------------------------------#
# Tools

SHELL      = bash
SLASH      = slash_parser

# Packed Lua script
LUAPRE = "$(TECMAKE_PATH)"/luapre.lua


#---------------------------------#
# Defaults
APPTYPE = windows
INCLUDES =
LIBS =
LIB =


#---------------------------------#
# User Configuration File

MAKENAME = config.mak

ifdef MF
  MAKENAME = $(MF).mak
endif

###################
include $(MAKENAME)
###################


#---------------------------------#
# Definitions of public variables

ifdef LIBNAME
  TARGETNAME = $(LIBNAME)
  MAKETYPE = LIB
else
  TARGETNAME = $(APPNAME)
  MAKETYPE = APP
endif

ifndef TARGETNAME
  $(error LIBNAME nor APPNAME defined in $(MAKENAME))
endif

PROJNAME ?= $(TARGETNAME)

ifneq ($(PROJNAME), $(TARGETNAME))
  OBJROOT := $(OBJROOT)/$(TARGETNAME)
endif

ifneq ($(findstring dll, $(TEC_UNAME)), )
  ifneq ($(MAKETYPE), APP)
    MAKETYPE = DLL
    DEF_FILE ?= $(TARGETNAME).def
    DEF_FILE := $(SRCDIR)/$(DEF_FILE)
  endif
endif

DEPEND := $(TARGETNAME).wdep

ifdef DEPENDDIR
  DEPEND := $(DEPENDDIR)/$(TARGETNAME).dep.$(TEC_UNAME)
endif


# ---------------------------------------------------------
# LO, LOH and LH folders

SRCLUADIR ?= $(SRCDIR)
ifdef NO_LUAOBJECT
  LHDIR  ?= $(SRCLUADIR)
else
  LOHDIR ?= $(SRCLUADIR)
endif

ifdef USE_LOH_SUBDIR
  ifeq ($(TEC_BYTEORDER), TEC_BIGENDIAN)
    ifeq ($(TEC_WORDSIZE), TEC_64)
      LOH_SUBDIR ?= be64
    else
      LOH_SUBDIR ?= be32
    endif
  else
    ifeq ($(TEC_WORDSIZE), TEC_64)
      # longs in 64-bits Windows are 32 bits!!!
      LOH_SUBDIR ?= le64w
    else
      LOH_SUBDIR ?= le32
    endif
  endif
  
  LOHDIR := $(LOHDIR)/$(LOH_SUBDIR)
  INCLUDES += $(LOHDIR)
else
  ifeq ($(TEC_BYTEORDER), TEC_BIGENDIAN)
    ifeq ($(TEC_WORDSIZE), TEC_64)
      LO_SUFFIX ?= _be64
    else
      LO_SUFFIX ?= _be32
    endif
  else
    ifeq ($(TEC_WORDSIZE), TEC_64)
      # longs in 64-bits Windows are 32 bits!!!
      LO_SUFFIX ?= _le64w
    else
      LO_SUFFIX ?=
    endif
  endif
endif

ifdef USE_LH_SUBDIR
  INCLUDES += $(LHDIR)
endif


#---------------------------------#
# Main Rule - Build Everything that it is necessary

.PHONY: tecmake
ifeq ($(MAKETYPE), APP)
  tecmake: print-start system-check directories application scripts
else
  ifeq ($(MAKETYPE), DLL)
    tecmake: print-start system-check directories dynamic-lib
  else
    tecmake: print-start system-check directories static-lib
  endif
endif

.PHONY: print-start
print-start:
	@echo ''; echo 'Tecmake: Starting [ $(TARGETNAME):$(TEC_UNAME) ]'


#---------------------------------#
# Definitions of public variables

ifeq ($(MAKETYPE), APP)
  TARGETROOT ?= $(PROJDIR)/bin
else
  TARGETROOT ?= $(PROJDIR)/lib
endif

ifeq ($(MAKETYPE), APP)
  TEC_UNAME_DIR ?= $(TEC_SYSNAME)
else
  TEC_UNAME_DIR ?= $(TEC_UNAME)
endif

ifdef DBG
  OPT:=
  ifdef DBG_DIR
    TEC_UNAME_DIR := $(TEC_UNAME_DIR)d
  endif
endif

ifdef LUAMOD_DIR
  ifdef USE_LUA53
    LUAMODSFX = 53
  endif
  ifdef USE_LUA52
    LUAMODSFX = 52
  endif
  ifdef USE_LUA51
    LUAMODSFX = 51
  endif
  TEC_UNAME_DIR := $(TEC_UNAME_DIR)/Lua$(LUAMODSFX)
endif

OBJDIR := $(OBJROOT)/$(TEC_UNAME_DIR)
TARGETDIR := $(TARGETROOT)/$(TEC_UNAME_DIR)

TARGETEXE := $(TARGETDIR)/$(TARGETNAME).exe
TARGETDLL := $(TARGETDIR)/$(TARGETNAME).dll
TARGETLIB := $(TARGETDIR)/$(TARGETNAME).lib
ifneq ($(findstring gcc, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif
ifneq ($(findstring mingw, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif
ifneq ($(findstring dllg, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).dll.a
endif
ifneq ($(findstring dllw, $(TEC_UNAME)), )
  TARGETLIB := $(TARGETDIR)/lib$(TARGETNAME).a
endif

ifdef NO_ECHO
  ECHO:=@
endif

#---------------------------------#
# Platform/Compiler dependend parameters

STDDEFS = -DTEC_UNAME=$(TEC_UNAME) -DTEC_SYSNAME=$(TEC_SYSNAME) -D$(TEC_BYTEORDER) -D$(TEC_WORDSIZE) -DWIN32
STDLIB  = kernel32 user32 gdi32 winspool comdlg32 advapi32 shell32 uuid oleaut32 ole32 comctl32

#Compilers
VC6 ?= x:/lng/vc6
VC7 ?= x:/lng/vc7
VC8 ?= x:/lng/vc8
VC9 ?= x:/lng/vc9
VC10 ?= x:/lng/vc10
VC11 ?= x:/lng/vc11
VC12 ?= x:/lng/vc12
OWC1 ?= x:/lng/owc1
BC55 ?= x:/lng/bc55
BC56 ?= x:/lng/cbuilderx
BC6  ?= x:/lng/bc6
MINGW3 ?= x:/lng/mingw3
MINGW4 ?= x:/lng/mingw4
MINGW4_64 ?= x:/lng/mingw4_64
# The default location is in the PATH
#GCC3 ?= x:/lng/gcc3
#GCC4 ?= x:/lng/gcc4

#Tools
QT ?= x:/lng/qt
ifdef USE_GTK3
  GTKSFX:=3
  GTK3 ?= x:/lng/gtk3
  GTK := $(GTK3)
else
  GTKSFX:=2
  GTK ?= x:/lng/gtk
endif

GLUT ?= x:/lng/glut
GLUT_LIB ?= $(GLUT)/lib
GLUT_INC ?= $(GLUT)/include

OBJEXT = obj
LIBEXT = lib

ifneq ($(findstring _64, $(TEC_UNAME)), )
  BUILD64 = Yes
endif

ifneq ($(findstring dll, $(TEC_UNAME)), )
  USE_DLL = Yes
endif

##########################################################################

ifeq "$(TEC_UNAME)" "vc6"
  COMPILER = $(VC6)
endif

ifeq "$(TEC_UNAME)" "vc7"
  COMPILER = $(VC7)
endif

ifeq "$(TEC_UNAME)" "vc8"
  COMPILER = $(VC8)
endif

ifeq "$(TEC_UNAME)" "vc8_64"
  COMPILER = $(VC8)
  SDKLIBBIN = /amd64
endif

ifneq ($(findstring vc9, $(TEC_UNAME)), )
  COMPILER = $(VC9)
endif

ifneq ($(findstring vc10, $(TEC_UNAME)), )
  COMPILER = $(VC10)
endif

ifneq ($(findstring vc11, $(TEC_UNAME)), )
  COMPILER = $(VC11)
endif

ifneq ($(findstring vc12, $(TEC_UNAME)), )
  COMPILER = $(VC12)
endif

ifeq "$(TEC_UNAME)" "dll"
  COMPILER = $(VC6)
endif

ifeq "$(TEC_UNAME)" "dll7"
  COMPILER = $(VC7)
endif

ifeq "$(TEC_UNAME)" "dll8"
  COMPILER = $(VC8)
  ifdef DBG
    #debug info not working for dll8 linker
    define DBG
    endef
  endif
endif

ifeq "$(TEC_UNAME)" "dll8_64"
  COMPILER = $(VC8)
  SDKLIBBIN = /amd64
endif

ifneq ($(findstring dll9, $(TEC_UNAME)), )
  COMPILER = $(VC9)
endif

ifneq ($(findstring dll10, $(TEC_UNAME)), )
  COMPILER = $(VC10)
endif

ifneq ($(findstring dll11, $(TEC_UNAME)), )
  COMPILER = $(VC11)
endif

ifneq ($(findstring dll12, $(TEC_UNAME)), )
  COMPILER = $(VC12)
endif

ifeq "$(COMPILER)" "$(VC6)"
  TEC_CC = vc
  # Use the VC7 Platform SDK, no harm if VC7 is not installed
  PLATSDK ?= $(VC7)/PlatformSDK
  OLD_OPENGL = Yes
endif

ifeq "$(COMPILER)" "$(VC7)"
  TEC_CC = vc
  PLATSDK ?= $(VC7)/PlatformSDK
  OLD_OPENGL = Yes
endif

ifeq "$(COMPILER)" "$(VC8)"
  NEW_VC_COMPILER = Yes
  TEC_CC = vc
  STDDEFS += -DMSVC8
  PLATSDK ?= $(VC8)/PlatformSDK
  OLD_OPENGL = Yes
  ifdef USE_DLL
    GEN_MANIFEST ?= Yes
  else
    #there is no single thread RTL in VC8
    USE_MT = Yes
  endif
endif

ifeq "$(COMPILER)" "$(VC9)"
  NEW_VC_COMPILER = Yes
  TEC_CC = vc
  STDDEFS += -DMSVC9
  ifdef USE_DLL
    GEN_MANIFEST ?= Yes
  else
    #there is no single thread RTL in VC9
    USE_MT = Yes
  endif
  ifdef VC9SDK
    PLATSDK ?= $(VC9SDK)
  else
    # Not the real folder, we copied from "C:\Program Files\Microsoft SDKs\Windows\v6.0A"
    PLATSDK ?= $(VC9)/WinSDK
  endif
  RESBIN := $(PLATSDK)/bin
  ifdef BUILD64
    RESBIN := $(RESBIN)/x64
  endif
endif

ifeq "$(COMPILER)" "$(VC10)"
  NEW_VC_COMPILER = Yes
  TEC_CC = vc
  STDDEFS += -DMSVC10
  ifdef USE_DLL
#    GEN_MANIFEST ?= Yes
  else
    #there is no single thread RTL in VC10
    USE_MT = Yes
  endif
  ifdef VC10SDK
    PLATSDK ?= $(VC10SDK)
  else
    # Not the real folder, we copied from "C:\Program Files\Microsoft SDKs\Windows\v7.1"
    PLATSDK ?= $(VC10)/WinSDK
  endif
  RESBIN := $(PLATSDK)/bin
  ifdef BUILD64
    RESBIN := $(RESBIN)/x64
  endif
endif

ifeq "$(COMPILER)" "$(VC11)"
  NEW_VC_COMPILER = Yes
  NEW_SDK_UM = Yes
  UMDIR := /win8/um
  TEC_CC = vc
  STDDEFS += -DMSVC11
  ifdef USE_DLL
#    GEN_MANIFEST ?= Yes
  else
    #there is no single thread RTL in VC11
    USE_MT = Yes
  endif
  ifdef VC11SDK
    PLATSDK ?= $(VC11SDK)
  else
    # Not the real folder, we copied from "C:\Program Files (x86)\Windows Kits\8.0"
    PLATSDK ?= $(VC11)/WinSDK
  endif
  ifdef BUILD64
    RESBIN := $(PLATSDK)/bin/x64
  else
    RESBIN := $(PLATSDK)/bin/x86
  endif
endif

ifeq "$(COMPILER)" "$(VC12)"
  NEW_VC_COMPILER = Yes
  NEW_SDK_UM = Yes
  UMDIR := /winv6.3/um
  TEC_CC = vc
  STDDEFS += -DMSVC12
  ifdef USE_DLL
#    GEN_MANIFEST ?= Yes
  else
    #there is no single thread RTL in VC12
    USE_MT = Yes
  endif
  ifdef VC12SDK
    PLATSDK ?= $(VC12SDK)
  else
    # Not the real folder, we copied from "C:\Program Files (x86)\Windows Kits\8.1"
    PLATSDK ?= $(VC12)/WinSDK
  endif
  ifdef BUILD64
    RESBIN := $(PLATSDK)/bin/x64
  else
    RESBIN := $(PLATSDK)/bin/x86
  endif
endif

ifeq "$(TEC_CC)" "vc"
  ifdef BUILD64
    STDDEFS += -DWIN64
    MACHINE = X64
    GTK := $(GTK)_x64
    VCLIBBIN = /amd64
    ifdef NEW_SDK_UM
      SDKLIBBIN := $(UMDIR)/x64
    else
      SDKLIBBIN ?= /x64
    endif
    ifdef USE_X86_CL64
      BIN = $(COMPILER)/bin/x86_amd64
    else
      BIN = $(COMPILER)/bin/amd64
    endif
  else
    ifdef NEW_SDK_UM
      SDKLIBBIN := $(UMDIR)/x86
    else
      VCLIBBIN =
    endif
    MACHINE = X86
    BIN = $(COMPILER)/bin
  endif
  RESBIN ?= $(COMPILER)/bin
  CC        = $(BIN)/cl -nologo
  CPPC      = $(BIN)/cl -nologo
  LIBC      = $(BIN)/link -lib -nologo
  LINKER    = $(BIN)/link -nologo
  MT        = $(RESBIN)/mt -nologo
  RCC       = $(RESBIN)/rc -fo
  ifdef NEW_SDK_UM
    STDINCS = $(PLATSDK)/include/shared $(PLATSDK)/include/um $(COMPILER)/include
  else
    STDINCS = $(PLATSDK)/include $(COMPILER)/include
  endif
  STDFLAGS  = -c -Fo$(OBJDIR)/ -W3
  STDLFLAGS =
  DEPDEFS   = -D_WIN32 -D_M_IX86 -D_STDCALL_SUPPORTED
  STDLIBDIR = -LIBPATH:$(COMPILER)/lib$(VCLIBBIN) -LIBPATH:$(PLATSDK)/lib$(SDKLIBBIN)
  OPTFLAGS := -O2
  DEBUGFLAGS := -Z7 -Od -GZ
  ifdef USE_ATL
    STDINCS += $(COMPILER)/atlmfc/include
    STDLIBDIR += -LIBPATH:$(COMPILER)/atlmfc/lib$(VCLIBBIN)
  endif
  ifdef NEW_VC_COMPILER
    DEBUGFLAGS := -Z7 -Od -RTC1
    STDDEFS += -D_CRT_SECURE_NO_DEPRECATE
    ifndef CPP_NARROW_INLINES
      STDDEFS += -D_CPP_NARROW_INLINES_DEFINED
    endif
    ifdef USE_CLR
      STDFLAGS += -clr
    else
      STDFLAGS += -EHsc
    endif
    ifdef USE_OPENMP
      STDFLAGS += -openmp
      LIBS += vcomp
    endif
  else                  # Exception Handling Model
    STDFLAGS += -GX
  endif
  ifneq ($(MAKETYPE), LIB)
    ifeq "$(COMPILER)" "$(VC6)"
      STDLFLAGS += -pdb:none -incremental:no -machine:$(MACHINE)
    else
      STDLFLAGS += -incremental:no -machine:$(MACHINE)
    endif
    ifdef DBG
      STDLFLAGS += -debug
    endif
    ifdef NEW_VC_COMPILER
      ifndef GEN_MANIFEST
        STDLFLAGS += -MANIFEST:NO
      else
        ifeq ($(GEN_MANIFEST), No)
          STDLFLAGS += -MANIFEST:NO
        else
          STDLFLAGS += -MANIFEST
        endif
      endif
    endif
  endif
  ifeq ($(MAKETYPE), APP)
    ifeq "$(COMPILER)" "$(VC6)"
      STDFLAGS += -GA
    else
      OPTFLAGS += -GL
      ifdef OPT
        STDLFLAGS += -LTCG
      endif
    endif
    STDLFLAGS += -subsystem:$(APPTYPE) -out:$(TARGETEXE)
  else
    ifeq ($(MAKETYPE), DLL)
      ifeq "$(COMPILER)" "$(VC6)"
        STDFLAGS += -GD
      else
        OPTFLAGS += -GL
        ifdef OPT
          STDLFLAGS += -LTCG
        endif
      endif
      STDLFLAGS += -dll -subsystem:$(APPTYPE) -out:$(TARGETDLL) -implib:$(TARGETLIB) -def:$(DEF_FILE)
    else
      STDLFLAGS += -out:$(TARGETLIB)
    endif
  endif
  ifdef USE_DLL
    ifdef DBG
      STDFLAGS += -MDd
    else
      STDFLAGS += -MD
    endif
  else
    ifdef USE_MT
      ifdef DBG
        STDFLAGS += -MTd
      else
        STDFLAGS += -MT
      endif
    else
      ifdef DBG
        STDFLAGS += -MLd
      else
        STDFLAGS += -ML
      endif
    endif
  endif
endif

##########################################################################

ifeq "$(TEC_UNAME)" "owc1"
  COMPILER = $(OWC1)
  TEC_CC  = wc
  STDLFLAGS =
endif

ifeq "$(TEC_CC)" "wc"
  WIN_OTHER = YES
  BIN     = $(COMPILER)/binnt
  CC      = $(SLASH) $(BIN)/wcc386
  CPPC    = $(SLASH) $(BIN)/wpp386
  LIBC    = $(SLASH) $(BIN)/wlib
  LINKER  = $(SLASH) $(BIN)/wlink
  RCC     = $(SLASH) $(BIN)/rc -fo
  STDINCS = $(COMPILER)/h $(COMPILER)/h/nt
  STDFLAGS += -od -w4 -5r -bt=nt -mf -e25 -zq -fo$(OBJDIR)/
  STDLIBDIR = LIBP $(COMPILER)/lib386 LIBP $(COMPILER)/lib386/nt
  DEBUGFLAGS := -d2
  OPTFLAGS := -ot
  ifeq ($(MAKETYPE), APP)
    STDLFLAGS = OP maxe=25 OP quiet FORM windows nt NAME $(TARGETEXE)
    ifeq ($(APPTYPE), console)
      STDLFLAGS += RU con
    endif
  else
    STDLFLAGS += -b -c -n -q -p=512 $(TARGETLIB)
  endif
  ifdef USE_DLL
    STDFLAGS += -bm -br
  endif
endif

##########################################################################

ifeq "$(TEC_UNAME)" "bc55"
  COMPILER = $(BC55)
  TEC_CC  = bc
  OLD_OPENGL = Yes
endif

ifeq "$(TEC_UNAME)" "bc56"
  COMPILER = $(BC56)
  TEC_CC  = bc
  OLD_OPENGL = Yes
endif

ifeq "$(TEC_UNAME)" "bc6"
  COMPILER = $(BC6)
  TEC_CC  = bc
  OLD_OPENGL = Yes
endif

ifeq "$(TEC_CC)" "bc"
  WIN_OTHER = YES
  TEC_CC   = bc
  BIN      = $(COMPILER)/bin
  CC       = $(BIN)/bcc32
  CPPC     = $(BIN)/bcc32
  LIBC     = $(BIN)/tlib /P32
  RCC      = $(BIN)/brc32 -r -fo
  LINKER   = $(SLASH) $(BIN)/ilink32
  STDINCS  = $(COMPILER)/include $(COMPILER)/include/dinkumware
  STDLIBDIR = -L$(COMPILER)/lib -L$(COMPILER)/lib/PSDK
  STDFLAGS  = -c -n$(OBJDIR)/
  STDLIB    := cw32 import32 $(STDLIB)
  ifeq ($(MAKETYPE), APP)
    STDLFLAGS = -Tpe #-x -c -Gn
    ifeq ($(APPTYPE), console)
      STARTUP = c0x32.obj
      STDLFLAGS += -ap
    else
      STARTUP = c0w32.obj
      STDLFLAGS += -aa
    endif
  else
    STDLFLAGS = $(TARGETLIB)
  endif
  OPTFLAGS := -O2
  DEBUGFLAGS := -v -x -xp
  ifdef USE_DLL
    STDFLAGS += -tWDMR
  endif
endif

##########################################################################

ifneq ($(findstring gcc, $(TEC_UNAME)), )
  TEC_CC = gcc
endif

ifneq ($(findstring mingw, $(TEC_UNAME)), )
  TEC_CC = gcc
endif

ifeq "$(TEC_UNAME)" "gcc3"
  COMPILER = $(GCC3)
  ifdef USE_OPENGL
    STDDEFS += -DUSE_OPENGL32
  endif
endif

ifeq "$(TEC_UNAME)" "mingw3"
  COMPILER = $(MINGW3)
  OLD_OPENGL = Yes
endif

ifneq ($(findstring gcc4, $(TEC_UNAME)), )
  COMPILER = $(GCC4)
endif

ifeq "$(TEC_UNAME)" "mingw4"
  COMPILER = $(MINGW4)
  OLD_OPENGL = Yes
endif

ifeq "$(TEC_UNAME)" "mingw4_64"
  COMPILER = $(MINGW4_64)
#  OLD_OPENGL = Yes
  BUILD64 = Yes
endif

ifneq ($(findstring dllg4, $(TEC_UNAME)), )
  COMPILER = $(GCC4)
  TEC_CC = gcc
endif

ifeq "$(TEC_UNAME)" "dllw4"
  COMPILER = $(MINGW4)
  TEC_CC = gcc
  OLD_OPENGL = Yes
endif

ifeq "$(TEC_UNAME)" "dllw4_64"
  COMPILER = $(MINGW4_64)
  TEC_CC = gcc
#  OLD_OPENGL = Yes
  BUILD64 = Yes
endif

ifeq "$(COMPILER)" "$(GCC4)"
  ifdef USE_OPENGL
    STDDEFS += -DUSE_OPENGL32
  endif
endif

ifeq "$(TEC_CC)" "gcc"
  WIN_OTHER = YES
  ifneq ($(findstring w4, $(TEC_UNAME)), )
    WIN_OTHER :=
  endif
  ifdef BUILD64
    STDDEFS += -DWIN64
    GTK := $(GTK)_x64
  endif
  ifneq "$(findstring mingw, $(COMPILER))" ""
    BIN   = $(COMPILER)/bin/
  endif
  CC      = $(BIN)gcc
  CPPC    = $(BIN)g++
  LIBC    = $(BIN)ar
  RCC     = $(BIN)windres -O coff -o
  ifndef LINKER
    ifneq "$(findstring .cpp, $(SRC))" ""
      LINKER := $(CPPC)
    else
      LINKER := $(CC)
    endif
  endif
  RANLIB  = $(BIN)ranlib
  ifneq "$(findstring mingw, $(COMPILER))" ""
    STDINCS = $(COMPILER)/include
    STDLIBDIR = -L$(COMPILER)/lib
  endif
  STDFLAGS += -Wall
  DEBUGFLAGS := -g
  OPTFLAGS := -O2
  OBJEXT=o
  LIBEXT=a
  ifdef USE_OPENMP
    STDFLAGS += -fopenmp
    LIBS += gomp
  endif
  ifeq ($(MAKETYPE), APP)
    STDLFLAGS = -Wl,-subsystem,$(APPTYPE)
  else
    ifeq ($(MAKETYPE), DLL)
      ifneq ($(findstring w4, $(TEC_UNAME)), )
        STDLFLAGS = -static-libgcc
        ifneq "$(findstring .cpp, $(SRC))" ""
          STDLFLAGS += -static-libstdc++
        endif
      else
        STDLFLAGS =
      endif
    else
      STDLFLAGS = r
    endif
  endif
endif

##########################################################################

ifdef DBG
  STDFLAGS += $(DEBUGFLAGS)
  STDDEFS += -DDEBUG
else
  STDDEFS += -DNDEBUG
  ifdef OPT
    STDFLAGS += $(OPTFLAGS)
  endif
endif

# allows an extra configuration file.
ifdef EXTRA_CONFIG
include $(EXTRA_CONFIG)
endif

.PHONY: system-check
system-check:
  ifndef TEC_UNAME
			@echo ''; echo 'Tecmake: check failed, TEC_UNAME not defined.'; echo '';
			@exit 1;
  endif
  ifndef TEC_WIN32
    ifndef TEC_WIN64
			@echo ''; echo 'Tecmake: check failed, TEC_UNAME not recognized.'; echo '';
			@exit 1;
    endif
  endif
  ifdef CHECK_GDIPLUS
    ifdef WIN_OTHER
			@echo ''; echo 'Tecmake: check failed, GDI+ NOT available in this system.'; echo '';
			@exit 1;
    endif
  endif


#---------------------------------#
# Tecgraf Libraries Location
TECTOOLS_HOME ?= ../..

IUP   ?= $(TECTOOLS_HOME)/iup
CD    ?= $(TECTOOLS_HOME)/cd
IM    ?= $(TECTOOLS_HOME)/im
LUA   ?= $(TECTOOLS_HOME)/lua
LUA51 ?= $(TECTOOLS_HOME)/lua5.1
LUA52 ?= $(TECTOOLS_HOME)/lua52
LUA53 ?= $(TECTOOLS_HOME)/lua53


#---------------------------------#
#  Pre-defined libraries

# Library order:
#   user + iupcd + cd + iup + motif + X
# Library path order is reversed

ifdef USE_LUA
  LUA_SUFFIX ?=
  LIBLUASUFX := 3
endif

ifdef USE_LUA4
  LUA_SUFFIX ?= 4
  LIBLUASUFX := 4
  override USE_LUA = Yes
  LUA := $(LUA4)
endif

ifdef USE_LUA5
  LUA_SUFFIX ?= 5
  LIBLUASUFX := 5
  override USE_LUA = Yes
  LUA := $(LUA5)
endif

ifdef USE_LUA50
  LUA_SUFFIX ?= 50
  LIBLUASUFX := 5
  override USE_LUA = Yes
  LUA := $(LUA50)
  NO_LUALIB := Yes
endif

ifdef USE_LUA51
  LUA_SUFFIX ?= 5.1
  LIBLUASUFX := 51
  override USE_LUA = Yes
  LUA := $(LUA51)
  NO_LUALIB := Yes
endif

ifdef USE_LUA52
  LUA_SUFFIX ?= 52
  LIBLUASUFX := 52
  override USE_LUA = Yes
  LUA := $(LUA52)
  NO_LUALIB := Yes
endif

ifdef USE_LUA53
  LUA_SUFFIX ?= 53
  LIBLUASUFX := 53
  override USE_LUA = Yes
  LUA := $(LUA53)
  NO_LUALIB := Yes
endif

ifdef USE_IUP
  override USE_IUP3 = Yes
endif
ifdef USE_IUP3
  override USE_IUP = Yes
endif

ifdef USE_IUP2
  override USE_IUP = Yes
  IUP := $(IUP)2
endif

ifdef USE_IUPBETA
  IUP := $(IUP)/beta
endif

ifdef USE_CDBETA
  CD := $(CD)/beta
endif

ifdef USE_IMBETA
  IM := $(IM)/beta
endif

ifdef USE_GLUT
  override USE_OPENGL = Yes
  LIBS += glut32
  LDIR += $(GLUT_INC)
  STDINCS += $(GLUT_LIB)
endif

ifdef USE_GDK
  override USE_GTK = Yes
endif

ifdef USE_IUPCONTROLS
  override USE_CD = Yes
  override USE_IUP = Yes
  ifdef USE_IUPLUA
    LIBS += iupluacontrols$(LIBLUASUFX)
    override USE_CDLUA = Yes
  endif
  LIBS += iupcontrols
endif

ifdef USE_IUPGLCONTROLS
  override USE_OPENGL = Yes
  override USE_IUP = Yes
  ifdef USE_IUPLUA
    LIBS += iupluaglcontrols$(LIBLUASUFX)
  endif
  LIBS += iupglcontrols ftgl
endif

ifdef USE_IMLUA
  override USE_IM = Yes
  LIBS += imlua$(LIBLUASUFX)
endif

ifdef USE_CDLUA
  override USE_CD = Yes
  LIBS += cdlua$(LIBLUASUFX)
endif

ifdef USE_IUPLUA
  override USE_IUP = Yes
  ifdef USE_CD
    LIBS += iupluacd$(LIBLUASUFX)
  endif
  ifdef USE_OPENGL
    LIBS += iupluagl$(LIBLUASUFX)
  endif
  LIBS += iuplua$(LIBLUASUFX)
endif

ifdef USE_LUA
  ifndef NO_LUALIB
    LIBS += lualib$(LUA_SUFFIX)
  endif
  LIBS += lua$(LUA_SUFFIX)

  LUA_LIB ?= $(LUA)/lib/$(TEC_UNAME)
  LDIR += $(LUA_LIB)

  LUA_INC ?= $(LUA)/include
  INCLUDES += $(LUA_INC)

  LUA_BIN ?= $(LUA)/bin/$(TEC_SYSNAME)
  ifdef USE_BIN2C_LUA
    BIN2C := $(LUA_BIN)/lua$(LUA_SUFFIX) $(BIN2C_PATH)bin2c.lua
  else
    BIN2C := $(LUA_BIN)/bin2c$(LUA_SUFFIX)
  endif
  LUAC   := $(LUA_BIN)/luac$(LUA_SUFFIX)
  LUABIN := $(LUA_BIN)/lua$(LUA_SUFFIX)
endif

ifdef USE_IUP
  ifdef USE_CD
    LIBS += iupcd
  endif
  ifdef USE_GTK
    LIBS += iupgtk
  else
    LIBS += iup
  endif
  
  IUP_LIB ?= $(IUP)/lib/$(TEC_UNAME)
  LDIR += $(IUP_LIB)
  
  ifdef USE_OPENGL
    LIBS += iupgl
  endif
  
  ifdef USE_DLL
    ifeq ($(MAKETYPE), APP)
      LIBS += iupstub
    endif
  endif

  IUP_INC ?= $(IUP)/include
  INCLUDES += $(IUP_INC)
endif

ifdef USE_CD
  ifdef USE_GDIPLUS
    CHECK_GDIPLUS = Yes
    LIBS += cdcontextplus gdiplus
  endif
  
  ifdef USE_CAIRO
    # To use Cairo with Win32 base driver (NOT for GDK)
    # Can NOT be used together with GDI+
    LIBS += cdcairo pangocairo-1.0 cairo
  endif
  
  ifdef USE_GDK
    LIBS += cdgdk
  else
    LIBS += cd
  endif
  
  LINK_FREETYPE = Yes
  
  CD_LIB ?= $(CD)/lib/$(TEC_UNAME)
  LDIR += $(CD_LIB)

  CD_INC ?= $(CD)/include
  INCLUDES += $(CD_INC)
endif

ifdef LINK_FREETYPE
  # To be compatible with the existing DLLs of gnuwin32
  LIBS += freetype6
  
  ifndef NO_ZLIB
    LINK_ZLIB = Yes
  endif
endif

ifdef USE_IM
  LIBS += im
  
  ifndef NO_ZLIB
    LINK_ZLIB = Yes
  endif
  
  IM_LIB ?= $(IM)/lib/$(TEC_UNAME)
  LDIR += $(IM_LIB)

  IM_INC ?= $(IM)/include
  INCLUDES += $(IM_INC)
endif

ifdef LINK_ZLIB
  ifndef ZLIB
    ZLIB = zlib1
    
    ifneq ($(findstring gcc, $(TEC_UNAME)), )
      ZLIB = z
    endif
    ifneq ($(findstring mingw, $(TEC_UNAME)), )
      ZLIB = z
    endif
  endif

  LIBS += $(ZLIB)
endif

ifdef USE_OPENGL
  ifdef OLD_OPENGL
    LIBS += glaux glu32 opengl32
  else
    LIBS += glu32 opengl32
  endif
endif

ifdef USE_GTK
  STDINCS += $(GTK)/include/atk-1.0 $(GTK)/include/gtk-$(GTKSFX).0 $(GTK)/include/gdk-pixbuf-2.0 
  STDINCS += $(GTK)/include/cairo $(GTK)/include/pango-1.0 $(GTK)/include/glib-2.0 
  STDINCS += $(GTK)/lib/glib-2.0/include 
  ifndef USE_GTK3
    STDINCS += $(GTK)/lib/gtk-2.0/include
  endif
  ifeq "$(TEC_CC)" "gcc"
    STDFLAGS += -mms-bitfields
  endif
  LDIR += $(GTK)/lib
  ifdef USE_GTK3
    LIBS += gtk-3 gdk-3
  else
    LIBS += gtk-win32-2.0 gdk-win32-2.0 
  endif
  LIBS += gdk_pixbuf-2.0 pango-1.0 pangowin32-1.0 gobject-2.0 gmodule-2.0 glib-2.0
endif

ifdef USE_QT
	#STDFLAGS += -Zm200 -w34100 -w34189 -Zm200 -w34100 -w34189 -w34100 -w34189
  STDINCS += $(QT)/include $(QT)/include/QtCore $(QT)/include/QtGui $(QT)/include/ActiveQt $(QT)/mkspecs/win32-msvc2005
  STDDEFS += -DQT_LARGEFILE_SUPPORT -DQT_DLL -DQT_QT3SUPPORT_LIB -DQT3_SUPPORT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_THREAD_SUPPORT
  LDIR += $(QT)/lib
  LIBS += QtMain QtGui4 QtCore4
endif


#---------------------------------#
#  Building compilation flags that are sets

DEPINCS := $(INCLUDES) $(EXTRAINCS)

# INCLUDES for dependencies, remove references to "c:" and similars
DEPINCS := $(patsubst c:%, /cygdrive/c%, $(DEPINCS))
DEPINCS := $(patsubst d:%, /cygdrive/d%, $(DEPINCS))
DEPINCS := $(patsubst x:%, /cygdrive/x%, $(DEPINCS))
DEPINCS := $(patsubst t:%, /cygdrive/t%, $(DEPINCS))
DEPINCS := $(addprefix -I, $(DEPINCS))

INCLUDES := $(addprefix -I, $(INCLUDES))
STDINCS := $(addprefix -I, $(STDINCS))
EXTRAINCS := $(addprefix -I, $(EXTRAINCS))
DEFINES := $(addprefix -D, $(DEFINES))

# For aplications and DLLs
ifneq ($(MAKETYPE), LIB)
  LIBS += $(STDLIB)
  LIBS := $(addsuffix .$(LIBEXT), $(LIBS))

  ifeq ($(TEC_CC), vc)
    ifdef LDIR
      LDIR  := $(addprefix -LIBPATH:, $(LDIR))
    endif

    STDLFLAGS += $(LDIR) $(STDLIBDIR) $(LIBS)
  endif

  ifeq ($(TEC_CC), bc)
    ifdef LDIR
      LDIR  := $(addprefix -L, $(LDIR))
    endif
  endif

  ifeq ($(TEC_CC), wc)
    ifdef LDIR
      LDIR  := $(addprefix LIBP , $(LDIR))
    endif

    LIBS := $(addprefix LIB , $(LIBS))

    STDLFLAGS += $(LDIR) $(STDLIBDIR) $(LIBS)
  endif

  ifeq ($(TEC_CC), gcc)
    LIBS := $(addprefix -l, $(LIBS))
    LIBS := $(LIBS:.a=)
    ifdef LDIR
      LDIR  := $(addprefix -L, $(LDIR))
    endif

    STDLFLAGS += $(LDIR) $(STDLIBDIR) $(LIBS)
  endif

endif


#---------------------------------#
# Definitions of private variables

# C, C++ and RC compiler flags
CFLAGS   = $(FLAGS) $(STDFLAGS) $(INCLUDES) $(STDINCS) $(EXTRAINCS) $(DEFINES) $(STDDEFS)
CXXFLAGS = $(CPPFLAGS) $(STDFLAGS) $(INCLUDES) $(STDINCS) $(EXTRAINCS) $(DEFINES) $(STDDEFS)
RCFLAGS  = $(INCLUDES) $(STDINCS) $(EXTRAINCS) $(DEFINES) $(STDDEFS)

# Sources with relative path
SOURCES    = $(addprefix $(SRCDIR)/, $(SRC))

# Target for applications or libraries
ifeq ($(MAKETYPE), APP)
  TARGET := $(TARGETEXE)
else
  ifeq ($(MAKETYPE), DLL)
    TARGET := $(TARGETDLL) $(TARGETLIB) $(TARGETDIR)/$(TARGETNAME).exp
  else
    TARGET := $(TARGETLIB)
  endif
endif

# OBJ: list of .o, without path
# OBJS: list of .o with relative path
OBJ = $(notdir $(SRC))
OBJ := $(OBJ:.c=.$(OBJEXT))
OBJ := $(OBJ:.cpp=.$(OBJEXT))
OBJ := $(OBJ:.cxx=.$(OBJEXT))
OBJ := $(OBJ:.cc=.$(OBJEXT))
OBJ := $(OBJ:.rc=.res)
OBJS = $(addprefix $(OBJDIR)/, $(OBJ))

# Construct VPATH variable
P-SRC = $(dir $(SRC))
P-SRC += $(dir $(SRCLUA))
VPATH = .:$(foreach dir,$(P-SRC),$(if $(dir)="./",:$(dir)))

#---------------------------------#
ifdef LOHPACK
  # Pacote LOH unificado com todos scripts Lua precompilados
  LOHS := $(LOHDIR)/$(LOHPACK)
  LOHDIRS :=
else
  ifdef NO_LUAOBJECT
    LH = $(notdir $(SRCLUA))
    LH := $(LH:.lua=.lh)
    LHS = $(addprefix $(LHDIR)/, $(LH))
    LUAS := $(LHS)
  else
    # LOH: lista dos arquivos .loh, sem path
    # LOHS: lista dos arquivos .loh, com path relativo
    LO = $(notdir $(SRCLUA))
    LO := $(LO:.lua=$(LO_SUFFIX).lo)
    LOS = $(addprefix $(OBJROOT)/, $(LO))

    LOH = $(notdir $(SRCLUA))
    LOH := $(LOH:.lua=$(LO_SUFFIX).loh)
    LOHS = $(addprefix $(LOHDIR)/, $(LOH))
    LUAS := $(LOHS)
  endif
endif

#---------------------------------#
# Compiler depedent adjusts

# CFLAGS: parametros passados ao linker e librarian
LINKFLAGS := $(LFLAGS)  $(STDLFLAGS) $(OBJS) $(SLIB)
LIBFLAGS  := $(LCFLAGS) $(STDLFLAGS) $(OBJS) $(SLIB)

ifeq ($(TEC_CC), bc)
  ifeq ($(MAKETYPE), APP)
    LINKFLAGS = $(LFLAGS) $(STDLFLAGS) $(LDIR) $(STDLIBDIR) $(STARTUP) $(OBJS), $(TARGETEXE), , $(LIBS) $(SLIB),
  else
    LIBFLAGS  = $(LCFLAGS) $(subst /,\\, $(STDLFLAGS) $(addprefix +,$(OBJS) $(SLIB)))
  endif
endif

ifeq ($(TEC_CC), wc)
  ifeq ($(MAKETYPE), APP)
    LINKFLAGS = $(LFLAGS) $(STDLFLAGS) $(addprefix F , $(OBJS) $(SLIB))
  else
    #wlib adds files using "+" as an option
    LIBFLAGS  := $(LCFLAGS) $(STDLFLAGS) $(addprefix +, $(OBJS) $(SLIB))
  endif
endif

ifeq ($(TEC_CC), gcc)
  ifeq ($(MAKETYPE), APP)
    LINKFLAGS = -o $(TARGETEXE) $(OBJS) $(SLIB) $(LFLAGS) $(STDLFLAGS)
  endif
  ifeq ($(MAKETYPE), DLL)
    LINKFLAGS = -shared -o $(TARGETDLL) -Wl,--out-implib=$(TARGETLIB) $(OBJS) $(DEF_FILE) $(SLIB) $(LFLAGS) $(STDLFLAGS)
  endif
endif

#---------------------------------#
# Dynamic Library Build

.PHONY: dynamic-lib
dynamic-lib: $(TARGETDLL) addmanifest

$(TARGETDLL) : $(LUAS) $(OBJS) $(EXTRADEPS) $(DEF_FILE)
	@echo ''; echo Tecmake: linking $(@F) ...
	$(ECHO)$(LINKER) $(LINKFLAGS)
	@echo ''; echo 'Tecmake: Dynamic Library ($@) Done'; echo ''


#---------------------------------#
# Static Library Build

.PHONY: static-lib
static-lib: $(TARGETLIB)

$(TARGETDIR)/$(TARGETNAME).lib : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: librarian $(@F) ...
	$(ECHO)$(LIBC) $(LIBFLAGS)
	@echo ''; echo 'Tecmake: Static Library ($@) Done'; echo ''
	
$(TARGETDIR)/lib$(TARGETNAME).a : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: librarian $(@F) ...
	$(ECHO)$(LIBC) $(ARFLAGS) $@ $(OBJS) $(SLIB)
	@echo ''; echo Tecmake: updating lib TOC $(@F) ...
	$(ECHO)-$(RANLIB) $@
	@echo ''; echo 'Tecmake: Static Library ($@) Done'; echo ''


#---------------------------------#
# Application Build

.PHONY: application
application: $(TARGETEXE) addmanifest

$(TARGETEXE) : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: linking $(@F) ...
	$(ECHO)$(LINKER) $(LINKFLAGS)
	@echo ''; echo 'Tecmake: Application ($@) Done.'; echo ''


#---------------------------------#
#  Application Scripts

# Nomes dos scripts
SRELEASE = $(SRCDIR)/$(TARGETNAME).bat
EXEC := $(subst /,\,$(TARGETEXE))

.PHONY: scripts
ifdef NO_SCRIPTS
  scripts: ;
else
  scripts: $(SRELEASE) ;
endif

$(SRELEASE): $(TARGETEXE)
	@echo ''; echo 'Tecmake: generating script $(@F)'
	@echo '@echo off' > $@
	@echo 'REM Script generated automatically by tecmake v$(VERSION)' >> $@
	@echo '$(EXEC) %*' >> $@


#---------------------------------#
# Directories Creation

.PHONY: directories
directories: $(OBJDIR) $(TARGETDIR) $(EXTRADIR) $(LOHDIR) $(LHDIR)

$(OBJDIR) $(TARGETDIR):
	if [ ! -d $@ ] ; then mkdir -p $@ ; fi

ifdef EXTRADIR
  $(EXTRADIR):
	  if [ ! -d $@ ] ; then mkdir -p $@ ; fi
else
  $(EXTRADIR): ;
endif

ifdef LOHDIR
  $(LOHDIR):
	  if [ ! -d $@ ] ; then mkdir -p $@ ; fi
else
  $(LOHDIR): ;
endif

ifdef LHDIR
  $(LHDIR):
	  if [ ! -d $@ ] ; then mkdir -p $@ ; fi
else
  $(LHDIR): ;
endif


#---------------------------------#
# Compilation Rules

$(OBJDIR)/%.o:  $(SRCDIR)/%.c
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIR)/%.o:  $(SRCDIR)/%.cpp
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) -c $(CXXFLAGS) -o $@ $<

$(OBJDIR)/%.o:  $(SRCDIR)/%.cxx
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) -c $(CXXFLAGS) -o $@ $<

$(OBJDIR)/%.o:  $(SRCDIR)/%.cc
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) -c $(CXXFLAGS) -o $@ $<

$(OBJDIR)/%.obj:  $(SRCDIR)/%.c
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CC) $(CFLAGS) $<

$(OBJDIR)/%.obj:  $(SRCDIR)/%.cpp
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) $(CXXFLAGS) $<
	
$(OBJDIR)/%.obj:  $(SRCDIR)/%.cxx
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) $(CXXFLAGS) $<
	
$(OBJDIR)/%.obj:  $(SRCDIR)/%.cc
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(CPPC) $(CXXFLAGS) $<
	
$(OBJDIR)/%.res:  $(SRCDIR)/%.rc
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(RCC) $@ $(RCFLAGS) $<

$(LHDIR)/%.lh:  $(SRCLUADIR)/%.lua
	@echo ''; echo Tecmake: generating $(<F) ...
	$(ECHO)$(BIN2C) $< > $@

$(LOHDIR)/%.loh:  $(OBJROOT)/%.lo
	@echo ''; echo Tecmake: generating $(<F) ...
	$(ECHO)$(BIN2C) $< > $@

$(OBJROOT)/%$(LO_SUFFIX).lo: $(SRCLUADIR)/%.lua
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(LUAC) -o $@ $<

ifdef LOHPACK
$(LOHDIR)/$(LOHPACK):  $(SRCLUA)
	@echo ''; echo Tecmake: Generating $(<F) ...
	$(ECHO)$(LUABIN) $(LUAPRE) $(LUAPREFLAGS) -l $(SRCLUADIR) -o $@ $(SRCLUA)
endif


#---------------------------------#
# Rule to add a manifest file to the generated binary
.PHONY: addmanifest
addmanifest:
  ifdef NEW_VC_COMPILER
    ifeq ($(GEN_MANIFEST), Yes)
	    @echo ''; echo Tecmake: adding Manifest ...
      ifeq ($(MAKETYPE), DLL)
	      $(ECHO)$(MT) -manifest $(TARGETDLL).manifest "-outputresource:$(TARGETDLL);2"
      endif
      ifeq ($(MAKETYPE), APP)
	      $(ECHO)$(MT) -manifest $(TARGETEXE).manifest "-outputresource:$(TARGETEXE);1"
      endif
    endif
  endif


#---------------------------------#
# Dependencies

.PHONY: depend
depend: $(DEPEND)

$(DEPEND): $(MAKENAME)
  ifdef SRC
	  @echo "" > $(DEPEND)
	  @which gcc 2> /dev/null 1>&2 ;\
	  if [ $$? -eq 0 ]; then \
	    echo "Tecmake: Building Dependencies ... (can be slow)" ;\
	    g++ $(DEPINCS) $(DEFINES) $(STDDEFS) $(DEPDEFS) -MM $(SOURCES) | \
	    sed -e '1,$$s/^\([^ ]*\)\.o/$$(OBJDIR)\/\1.$(OBJEXT)/' | \
	    sed -e 's/\([ \t][ \t]*\)\([a-zA-Z]\):/\1\/cygdrive\/\2/g' > $(DEPEND) ;\
	  else \
	    echo "" ;\
	    echo "Tecmake: error, g++ not found. Dependencies can not be built." ;\
	    echo "Must set NO_DEPEND=Yes" ;\
	    echo "" ;\
	    exit 1 ;\
	  fi
  endif

ifdef USE_NODEPEND
  NO_DEPEND:=Yes
endif

###################
ifndef NO_DEPEND
include $(DEPEND)
endif
###################


#---------------------------------#
# Management Rules

.PHONY: clean-dir
clean-dir:
	rm -fr $(OBJROOT) $(TARGETROOT)

#   Remove extra files
.PHONY: clean-extra
clean-extra:
	rm -f $(DEPEND) $(SRELEASE)

#   Remove Lua object inclusion files
.PHONY: clean-lohs
clean-lohs:
	rm -f $(LOS) $(LOHS)
	
#   Remove Lua inclusion files
.PHONY: clean-lhs
clean-lhs:
	rm -f $(LHS)
	
#   Remove object files
.PHONY: clean-obj
clean-obj:
	rm -f $(OBJS)

#   Remove target
.PHONY: clean-target
clean-target:
	rm -f $(TARGET)

#   Remove target and object files
.PHONY: clean-all-obj
clean-all-obj:
	@for d in $(UNAMES); do \
	  (cd $(OBJROOT)/$$d; echo $(OBJ) | xargs rm -f) ;\
	done

#   Remove libraries and executables for all platforms
.PHONY: clean-all-target
clean-all-target:
	@for d in $(UNAMES); do \
	  (rm -f $(TARGETROOT)/$$d/$(TARGETNAME).exe $(TARGETROOT)/$$d/$(TARGETNAME).$(LIBEXT) $(TARGETROOT)/$$d/$(TARGETNAME).dll $(TARGETROOT)/$$d/$(TARGETNAME).exp) ;\
	done

.PHONY: clean
clean: clean-target clean-obj

.PHONY: clean-all
clean-all: clean-extra clean-lohs clean-lhs clean-all-target clean-all-obj

#   Rebuild target and object files
.PHONY: rebuild
rebuild: clean-obj clean-target tecmake

#   Rebuild target without rebuilding object files
.PHONY: relink
relink: clean-target tecmake


#---------------------------------#

.PHONY: version
version:
	@echo "Tecmake Windows Version $(VERSION)"

#---------------------------------#
