#-------------------------------------------------------------------------#
#- Tecmake  (POSIX Version)                                              -#
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
# System Variables Definitions

ifndef TEC_UNAME
  # Base Definitions
  TEC_SYSNAME:=$(shell uname -s)
  TEC_SYSVERSION:=$(shell uname -r|cut -f1 -d.)
  TEC_SYSMINOR:=$(shell uname -r|cut -f2 -d.)
  TEC_SYSARCH:=$(shell uname -m)

  # Fixes
  ifeq ($(TEC_SYSNAME), Haiku)
    TEC_SYSARCH:=$(shell uname -p)
  endif
  ifeq ($(TEC_SYSNAME), SunOS)
    TEC_SYSARCH:=$(shell uname -p)
  endif
  ifeq ($(TEC_SYSNAME), IRIX)
    TEC_SYSARCH:=$(shell uname -p)
  endif
  ifeq ($(TEC_SYSNAME), FreeBSD)
    TEC_SYSMINOR:=$(shell uname -r|cut -f2 -d.|cut -f1 -d-)
  endif
  ifeq ($(TEC_SYSNAME), GNU/kFreeBSD)
    TEC_SYSNAME:=kFreeBSD
    TEC_SYSMINOR:=$(shell uname -r|cut -f2 -d.|cut -f1 -d-)
  endif
  ifeq ($(TEC_SYSNAME), AIX)
    TEC_SYSVERSION:=$(shell uname -v)
    TEC_SYSMINOR:=$(shell uname -r)
    TEC_SYSARCH:=ppc
  endif
  ifeq ($(TEC_SYSNAME), Darwin)
    TEC_SYSNAME:=MacOS
    TEC_SYSVERSION:=$(shell sw_vers -productVersion|cut -f1 -d.)
    TEC_SYSMINOR:=$(shell sw_vers -productVersion|cut -f2 -d.)
    TEC_SYSARCH:=$(shell uname -p)
  endif

  ifeq ($(TEC_SYSARCH), i686)
    TEC_SYSARCH:=x86
  endif
  ifeq ($(TEC_SYSARCH), i386)
    TEC_SYSARCH:=x86
  endif
  ifeq ($(TEC_SYSARCH), powerpc)
    TEC_SYSARCH:=ppc
  endif
  ifeq ($(TEC_SYSARCH), x86_64)
    TEC_SYSARCH:=x64
  endif
  ifeq ($(TEC_SYSARCH), amd64)
    TEC_SYSARCH:=x64
  endif

  # Compose
  TEC_SYSRELEASE:=$(TEC_SYSVERSION).$(TEC_SYSMINOR)
  TEC_UNAME:=$(TEC_SYSNAME)$(TEC_SYSVERSION)$(TEC_SYSMINOR)

  # Cygwin
  ifneq ($(findstring CYGWIN, $(TEC_SYSNAME)), )
    TEC_SYSNAME:=CYGWIN
    TEC_UNAME:=cygw$(TEC_SYSVERSION)$(TEC_SYSMINOR)
  endif

  # Linux 2.4 and GCC 3.x
  ifeq ($(TEC_UNAME), Linux24)
    GCCVER:=$(shell gcc -dumpversion|cut -f1 -d.)
    ifeq ($(GCCVER), 3)
      TEC_UNAME:=$(TEC_UNAME)g3
    endif
  endif

  # Linux 2.6 and GCC 4.x
  ifeq ($(TEC_UNAME), Linux26)
    GCCVER:=$(shell gcc -dumpversion|cut -f1 -d.)
    ifeq ($(GCCVER), 4)
      TEC_UNAME:=$(TEC_UNAME)g4
    endif
  endif

  ifeq ($(TEC_SYSNAME), Linux)
      # Linux and PowerPC
    ifeq ($(TEC_SYSARCH), ppc)
      TEC_UNAME:=$(TEC_UNAME)ppc
    endif

    # 64-bits Linux
    ifeq ($(TEC_SYSARCH), x64)
      BUILD_64=Yes
      TEC_UNAME:=$(TEC_UNAME)_64
    endif

    # Itanium Linux
    ifeq ($(TEC_SYSARCH), ia64)
      BUILD_64=Yes
      TEC_UNAME:=$(TEC_UNAME)_ia64
    endif
    
    # Linux Distribution
    TEC_DISTNAME=$(shell lsb_release -is)
    TEC_DISTVERSION=$(shell lsb_release -rs|cut -f1 -d.)
    TEC_DIST:=$(TEC_DISTNAME)$(TEC_DISTVERSION)
  endif

  # 64-bits FreeBSD
  ifeq ($(TEC_SYSNAME), FreeBSD)
    ifeq ($(TEC_SYSARCH), x64)
      BUILD_64=Yes
      TEC_UNAME:=$(TEC_UNAME)_64
    endif
  endif

  # Solaris and Intel
  ifeq ($(TEC_SYSNAME), SunOS)
    ifeq ($(TEC_SYSARCH) , x86)
      TEC_UNAME:=$(TEC_UNAME)x86
    endif
  endif

  # MacOS and Intel
  ifeq ($(TEC_SYSNAME), MacOS)
    ifeq ($(TEC_SYSMINOR), 5)
      ifeq ($(TEC_SYSARCH), x86)
        TEC_UNAME:=$(TEC_UNAME)x86
      endif
    else
      ifeq ($(TEC_SYSMINOR), 4)
        ifeq ($(TEC_SYSARCH), x86)
          TEC_UNAME:=$(TEC_UNAME)x86
        endif
      else
        TEC_SYSARCH:=x64
      endif
    endif
  endif
endif


#---------------------------------#
# System Info
.PHONY: sysinfo
sysinfo:
	@echo ''; echo 'Tecmake: System Info'
	@echo 'TEC_SYSNAME = $(TEC_SYSNAME)'
	@echo 'TEC_SYSVERSION = $(TEC_SYSVERSION)'
	@echo 'TEC_SYSMINOR = $(TEC_SYSMINOR)'
	@echo 'TEC_SYSARCH = $(TEC_SYSARCH)'
	@echo 'TEC_UNAME = $(TEC_UNAME)'
	@echo 'GTK_BASE = $(GTK_BASE)'
	@echo 'X11_LIB = $(X11_LIB)'
	@echo 'X11_INC = $(X11_INC)'
	@echo 'MOTIF_LIB = $(MOTIF_LIB)'
	@echo 'MOTIF_INC = $(MOTIF_INC)'
	@echo 'GLUT_LIB = $(GLUT_LIB)'
	@echo 'GLUT_INC = $(GLUT_INC)'
	@echo 'OPENGL_LIB = $(OPENGL_LIB)'
	@echo 'OPENGL_INC = $(OPENGL_INC)'
	@echo ''


#---------------------------------#
# Known Platforms

UNAMES = Linux24 Linux24g3 Linux24g3_64 Linux26 Linux26_64 Linux26g4 Linux26g4_64 Linux26_ia64 FreeBSD54 SunOS57 SunOS58 SunOS510 SunOS510_x86 AIX43 IRIX65 IRIX6465


#---------------------------------#
# Directories Definitions

PROJDIR = ..
SRCDIR  = .
OBJROOT = $(PROJDIR)/obj


#---------------------------------#
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
ifdef BUILD_64
  TEC_WORDSIZE = TEC_64
else
  TEC_WORDSIZE = TEC_32
endif
endif

# Itanium Exception
ifeq ($(TEC_SYSARCH), ia64)
  TEC_BYTEORDER = TEC_LITTLEENDIAN
  TEC_WORDSIZE = TEC_64
endif


#---------------------------------#
# Compilation Flags
STDFLAGS := -Wall
STDDEFS  := -DTEC_UNAME=$(TEC_UNAME) -DTEC_SYSNAME=$(TEC_SYSNAME) -D$(TEC_SYSNAME)=$(TEC_SYSRELEASE) -D$(TEC_BYTEORDER) -D$(TEC_WORDSIZE) -DFUNCPROTO=15
STDINCS  :=
OPTFLAGS := -O2
STDLFLAGS  := r
DEBUGFLAGS := -g
STDLDFLAGS := -shared
DLIBEXT := so
DLIBPRE := lib
APPEXT :=

ifneq ($(findstring Linux24, $(TEC_UNAME)), )
  NO_GTK_DEFAULT = Yes
endif
ifeq ($(TEC_UNAME), Linux26)
  NO_GTK_DEFAULT = Yes
endif
ifeq ($(TEC_UNAME), Linux26_64)
  NO_GTK_DEFAULT = Yes
endif

ifndef NO_GTK_DEFAULT
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    GTK_DEFAULT = Yes
  endif
  ifneq ($(findstring CentOS, $(TEC_UNAME)), )
    GTK_DEFAULT = Yes
  endif
  ifneq ($(findstring Linux, $(TEC_UNAME)), )
    GTK_DEFAULT = Yes
  endif
  ifneq ($(findstring MacOS, $(TEC_UNAME)), )
    GTK_DEFAULT = Yes
  endif
  ifneq ($(findstring FreeBSD, $(TEC_UNAME)), )
    GTK_DEFAULT = Yes
  endif
  ifneq ($(findstring SunOS, $(TEC_UNAME)), )
    ifeq ($(TEC_SYSARCH), x86)
      GTK_DEFAULT = Yes
    endif
  endif
endif

ifdef GTK_DEFAULT
  ifneq ($(findstring Linux31, $(TEC_UNAME)), )
    USE_GTK3 = Yes
  endif
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    USE_GTK3 = Yes
  endif
endif

#---------------------------------#
# Tools

CC       := $(TEC_TOOLCHAIN)gcc
CPPC     := $(TEC_TOOLCHAIN)g++
FF       := $(TEC_TOOLCHAIN)g77
RANLIB   := $(TEC_TOOLCHAIN)ranlib
AR       := $(TEC_TOOLCHAIN)ar
DEBUGGER := $(TEC_TOOLCHAIN)gdb
RCC      := $(TEC_TOOLCHAIN)windres

# Remote build script
REMOTE  = $(TECMAKE_HOME)/remote

# Packed Lua script
LUAPRE = $(TECMAKE_HOME)/luapre.lua


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

DEPEXT := dep
ifneq ($(findstring cygw, $(TEC_UNAME)), )
  DEPEXT := cdep
endif

DEPEND := $(TARGETNAME).$(DEPEXT)
ifdef DEPENDDIR
  DEPEND := $(DEPENDDIR)/$(TARGETNAME).$(DEPEXT).$(TEC_UNAME)
endif

ifeq ($(MAKETYPE), APP)
  TARGETROOT ?= $(PROJDIR)/bin
else
  TARGETROOT ?= $(PROJDIR)/lib
endif

ifneq ($(PROJNAME), $(TARGETNAME))
  OBJROOT := $(OBJROOT)/$(TARGETNAME)
endif

ifdef DBG
  OPT:=
  STDFLAGS += $(DEBUGFLAGS)
  STDDEFS += -DDEBUG
else
  STDDEFS += -DNDEBUG
  ifdef OPT
    STDFLAGS += $(OPTFLAGS)
    STRIP ?= Yes
  endif
endif

ifdef BUILD_64
  ifneq ($(findstring SunOS, $(TEC_UNAME)), )
    USE_CC = Yes
    BUILD_64_DIR = Yes
  endif
  ifneq ($(findstring AIX, $(TEC_UNAME)), )
    USE_CC = Yes
    BUILD_64_DIR = Yes
  endif
  ifneq ($(findstring IRIX, $(TEC_UNAME)), )
    USE_CC = Yes
    BUILD_64_DIR = Yes
  endif
endif

ifdef USE_CC
  CC := cc
  CPPC := CC
  STDFLAGS =
  UNAMES := $(UNAMES_CC)
  ifdef USE_CC_DIR
    TEC_UNAME := $(TEC_UNAME)cc
  endif
endif

ifdef BUILD_64
  ifdef BUILD_64_DIR
    TEC_UNAME := $(TEC_UNAME)_64
  endif
endif

TEC_UNAME_DIR ?= $(TEC_UNAME)
TEC_UNAME_LIB_DIR ?= $(TEC_UNAME)

ifdef DBG
  ifdef DBG_LIB_DIR
    TEC_UNAME_LIB_DIR := $(TEC_UNAME_DIR)d
  endif
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
  ifdef LUAMOD_LIB_DIR
    TEC_UNAME_LIB_DIR := $(TEC_UNAME_LIB_DIR)/Lua$(LUAMODSFX)
  else
    TEC_UNAME_DIR := $(TEC_UNAME_DIR)/Lua$(LUAMODSFX)
  endif
endif

OBJDIR := $(OBJROOT)/$(TEC_UNAME_DIR)
TARGETDIR := $(TARGETROOT)/$(TEC_UNAME_DIR)

# Change linker if any C++ source
ifndef LINKER
  ifneq "$(findstring .cpp, $(SRC))" ""
    LINKER := $(CPPC)
    LD := $(CPPC)
  else
    LINKER := $(CC)
    LD := $(CC)
  endif
endif

ifndef USE_STATIC
  # When using dynamic libraries,
  # there is no need to include indirect dependencies
  NO_OVERRIDE = Yes
endif
ifneq ($(findstring AIX, $(TEC_UNAME)), )
  # No dynamic libraries in AIX, so must behave as USE_STATIC
  NO_OVERRIDE :=
endif

ifdef NO_ECHO
  ECHO:=@
endif

#---------------------------------#
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
      LOH_SUBDIR ?= le64
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
      LO_SUFFIX ?= _le64
    else
      LO_SUFFIX ?=
    endif
  endif
endif

ifdef USE_LH_SUBDIR
  INCLUDES += $(LHDIR)
endif


#---------------------------------#
#  Platform specific variables

# Definitions for X11
X11_LIBS := Xmu Xt Xext X11
#X11_LIB :=
#X11_INC :=                     #include <X11/X.h>

# Definitions for OpenGL
OPENGL_LIBS := GLU GL
#OPENGL_LIB :=
#OPENGL_INC :=                  #include <GL/gl.h>  and possibly
MOTIFGL_LIB := GLw              #include <GL/GLwMDrawA.h>

# Definitions for Motif
#MOTIF_LIB :=
#MOTIF_INC :=                   #include <Xm/Xm.h>

# Definitions for GLUT
#GLUT_LIB :=
#GLUT_INC :=

# Definitions for GTK
ifdef GTK_BASE
  GTK := $(GTK_BASE)
else
  ifneq ($(findstring MacOS, $(TEC_UNAME)), )
  # Prefer using GTK_BASE then changing this
  # Fink GTK port
    GTK = /sw
  # MacPorts GTK
  #  GTK = /opt/local
  # GTK-OSX Framework
  #   GTK := /gtk/inst
  else
    GTK = /usr
  endif
endif

ifeq "$(TEC_SYSNAME)" "Haiku"
  STDFLAGS += -Wno-multichar
  LIBS += be textencoding tracker
endif

ifneq ($(findstring Linux, $(TEC_UNAME)), )
  UNIX_LINUX = Yes
  ifdef BUILD_64
    ifeq ($(TEC_SYSARCH), ia64)
      STDFLAGS += -fPIC
      X11_LIB := /usr/X11R6/lib
    else
      STDFLAGS += -m64 -fPIC
      X11_LIB := /usr/X11R6/lib64
    endif
  else
    X11_LIB := /usr/X11R6/lib
  endif
  X11_INC := /usr/X11R6/include
  MOTIFGL_LIB :=
  ifdef USE_OPENMP
    STDFLAGS += -fopenmp
    LIBS += gomp
  endif
endif

ifneq ($(findstring CentOS, $(TEC_UNAME)), )
  UNIX_LINUX = Yes
  ifdef BUILD_64
    ifeq ($(TEC_SYSARCH), ia64)
      STDFLAGS += -fPIC
      X11_LIB := /usr/X11R6/lib
    else
      STDFLAGS += -m64 -fPIC
      X11_LIB := /usr/X11R6/lib64
    endif
  else
    X11_LIB := /usr/X11R6/lib
  endif
  X11_INC := /usr/X11R6/include
  MOTIFGL_LIB :=
  ifdef USE_OPENMP
    STDFLAGS += -fopenmp
    LIBS += gomp
  endif
endif

ifneq ($(findstring IRIX, $(TEC_UNAME)), )
  UNIX_POSIX = Yes
  ifndef NO_LOCAL_LD
    LD = ld
  endif
  STDLDFLAGS := -elf -shared -rdata_shared -soname lib$(TARGETNAME).so
  RANLIB := /bin/true
  X11_LIBS := Xmu Xt X11
  ifdef BUILD_64
    ifdef USE_CC
      STDFLAGS += -64 -KPIC
      STDLDFLAGS += -64
      LINKER += -64
    endif
    X11_LIB := /usr/lib64
    MOTIF_LIB := /usr/Motif-2.1/lib64
  else
    X11_LIB := /usr/lib32
    MOTIF_LIB := /usr/Motif-2.1/lib32
  endif
  MOTIF_INC = /usr/Motif-2.1/include
endif

ifneq ($(findstring AIX, $(TEC_UNAME)), )
  UNIX_POSIX = Yes
  NO_DYNAMIC ?= Yes
  ifdef BUILD_64
    ifdef USE_CC
      STDFLAGS += -q64 # to compilers C and C++
      STDLFLAGS := -X64 $(STDLFLAGS) # to librarian
      STDLDFLAGS += -64
      LINKER += -q64 # to linker
    endif
  endif
endif

ifneq ($(findstring HP-UX, $(TEC_UNAME)), )
  UNIX_POSIX = Yes
  NO_DYNAMIC ?= Yes
  MOTIF_INC := /usr/include/Motif2.1
  X11_LIBS := Xt Xext X11
  OPENGL_LIB := /opt/graphics/OpenGL/lib
  OPENGL_INC := /opt/graphics/OpenGL/include
  STDDEFS := -DTEC_UNAME=$(TEC_UNAME) -DTEC_SYSNAME=$(TEC_SYSNAME) -D$(TEC_BYTEORDER) -D$(TEC_WORDSIZE) -DFUNCPROTO=15
  CC := aCC
  CPPC := aCC
  LINKER := aCC
endif

ifneq ($(findstring SunOS, $(TEC_UNAME)), )
  ifneq ($(TEC_SYSARCH), x86)
    UNIX_POSIX = Yes
  endif
  ifndef NO_LOCAL_LD
    LD = ld
  endif
  STDLDFLAGS := -G
  X11_INC := /usr/openwin/share/include
  X11_LIB := /usr/openwin/lib
  MOTIF_INC := /usr/dt/share/include
  MOTIF_LIB := /usr/dt/lib
  OPENGL_INC := /usr/openwin/share/include/X11
  GLUT_LIB := /usr/local/glut-3.7/lib/glut
  GLUT_INC := /usr/local/glut-3.7/include
  ifeq ($(TEC_SYSARCH), x86)
    STDINCS += /usr/X11/include
    GTK := /usr
  endif
  ifdef BUILD_64
    ifdef USE_CC
      STDFLAGS += -xarch=v9 -KPIC
      # have to force these PATHs because of a conflict with standard PATHs
      STDLDFLAGS += -64 -L/usr/lib/64 -L/usr/ucblib/sparcv9
      LINKER += -xarch=v9
    endif
  endif
  ifdef USE_CC
    ifdef DBG
      STDFLAGS += -g
    else
      ifdef OPT
#        STDFLAGS +=  ????
      endif
    endif
  endif
endif

ifneq ($(findstring MacOS, $(TEC_UNAME)), )
  UNIX_BSD = Yes
  X11_LIBS := Xmu Xp Xt Xext X11
  X11_LIB := /usr/X11R6/lib
  X11_INC := /usr/X11R6/include
  MOTIF_INC := /usr/OpenMotif/include
  MOTIF_LIB := /usr/OpenMotif/lib
  ifdef BUILD_DYLIB
    STDLDFLAGS := -dynamiclib -Wl -fno-common -headerpad_max_install_names -undefined dynamic_lookup -install_name lib$(TARGETNAME).dylib
    DLIBEXT := dylib
    STDFLAGS += -fno-common
  else
    STDLDFLAGS := -bundle -undefined dynamic_lookup
  endif
  ifdef USE_OPENGL
    ifeq ($(TEC_SYSMINOR), 5)
      #Darwin9 Only - OpenGL bug fix for Fink, when the message bellow appears
      #   ld: cycle in dylib re-exports with /usr/X11R6/lib/libGL.dylib
      LFLAGS += -dylib_file /System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib:/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL.dylib
    endif
  endif
  ifdef USE_OPENMP
    STDFLAGS += -fopenmp
    LIBS += gomp
  endif
endif

ifneq ($(findstring FreeBSD, $(TEC_UNAME)), )
  BSD = Yes
  X11_LIB := /usr/X11R6/lib
  X11_INC := /usr/X11R6/include
  ifeq ($(TEC_SYSARCH), x64)
    STDFLAGS += -fPIC
  endif
endif

#---------------------------------#
# Allows an extra configuration file.
ifdef EXTRA_CONFIG
include $(EXTRA_CONFIG)
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
endif

ifdef USE_GDK
  override USE_GTK = Yes
endif

ifdef USE_IUPCONTROLS
  override USE_CD = Yes
  override USE_IUP = Yes
  IUP_LIB ?= $(IUP)/lib/$(TEC_UNAME_LIB_DIR)
  
  ifdef USE_IUPLUA
    ifdef USE_STATIC
      SLIB += $(IUP_LIB)/libiupluacontrols$(LIBLUASUFX).a
    else
      LIBS += iupluacontrols$(LIBLUASUFX)
    endif
    override USE_CDLUA = Yes
  endif
  
  ifdef USE_STATIC
    SLIB += $(IUP_LIB)/libiupcontrols.a
  else
    LIBS += iupcontrols
  endif
endif

ifdef USE_IUPGLCONTROLS
  override USE_OPENGL = Yes
  override USE_IUP = Yes
  IUP_LIB ?= $(IUP)/lib/$(TEC_UNAME_LIB_DIR)
  CD_LIB ?= $(CD)/lib/$(TEC_UNAME_LIB_DIR)
  
  ifdef USE_IUPLUA
    ifdef USE_STATIC
      SLIB += $(IUP_LIB)/libiupluaglcontrols$(LIBLUASUFX).a
    else
      LIBS += iupluaglcontrols$(LIBLUASUFX)
    endif
  endif
  
  ifdef USE_STATIC
    SLIB += $(IUP_LIB)/libiupglcontrols.a $(CD_LIB)/libftgl.a
  else
    LIBS += iupglcontrols ftgl
  endif
endif

ifdef USE_IMLUA
  override USE_IM = Yes
  IM_LIB ?= $(IM)/lib/$(TEC_UNAME_LIB_DIR)
  ifdef USE_STATIC
    SLIB += $(IM_LIB)/libimlua$(LIBLUASUFX).a
  else
    LIBS += imlua$(LIBLUASUFX)
  endif
endif

ifdef USE_CDLUA
  override USE_CD = Yes
  CD_LIB ?= $(CD)/lib/$(TEC_UNAME_LIB_DIR)
  ifdef USE_STATIC
    SLIB += $(CD_LIB)/libcdlua$(LIBLUASUFX).a
  else
    LIBS += cdlua$(LIBLUASUFX)
  endif
endif

ifdef USE_IUPLUA
  override USE_IUP = Yes
  IUP_LIB ?= $(IUP)/lib/$(TEC_UNAME_LIB_DIR)
  
  ifdef USE_STATIC
    ifdef USE_CD
      SLIB += $(IUP_LIB)/libiupluacd$(LIBLUASUFX).a
    endif
    ifdef USE_OPENGL
      SLIB += $(IUP_LIB)/libiupluagl$(LIBLUASUFX).a
    endif
    SLIB += $(IUP_LIB)/libiuplua$(LIBLUASUFX).a
  else
    ifdef USE_CD
      LIBS += iupluacd$(LIBLUASUFX)
    endif
    ifdef USE_OPENGL
      LIBS += iupluagl$(LIBLUASUFX)
    endif
    LIBS += iuplua$(LIBLUASUFX)
  endif
endif

ifdef USE_LUA
  LUA_LIB ?= $(LUA)/lib/$(TEC_UNAME_LIB_DIR)
  ifdef USE_STATIC
    ifndef NO_LUALIB
      SLIB += $(LUA_LIB)/liblualib$(LUA_SUFFIX).a
    endif
    SLIB += $(LUA_LIB)/liblua$(LUA_SUFFIX).a
  else
    ifndef NO_LUALIB
      LIBS += lualib$(LUA_SUFFIX)
    endif
    ifndef NO_LUALINK
        LIBS += lua$(LUA_SUFFIX)
        LDIR += $(LUA_LIB)
    else
      ifneq ($(findstring cygw, $(TEC_UNAME)), )
        LIBS += lua$(LUA_SUFFIX)
        LDIR += $(LUA_LIB)
      endif
    endif
  endif

  LUA_INC ?= $(LUA)/include
  INCLUDES += $(LUA_INC)

  LUA_BIN ?= $(LUA)/bin/$(TEC_UNAME)
  ifdef USE_BIN2C_LUA
    BIN2C := $(LUA_BIN)/lua$(LUA_SUFFIX) $(BIN2C_PATH)bin2c.lua
  else
    BIN2C := $(LUA_BIN)/bin2c$(LUA_SUFFIX)
  endif
  LUAC   := $(LUA_BIN)/luac$(LUA_SUFFIX)
  LUABIN := $(LUA_BIN)/lua$(LUA_SUFFIX)
endif

ifdef USE_IUP
  IUP_SUFFIX ?=
  ifdef USE_IUP3
    ifndef USE_HAIKU
      ifdef GTK_DEFAULT
        ifdef USE_MOTIF
          IUP_SUFFIX := mot
        else
          ifndef NO_OVERRIDE
            override USE_GTK = Yes
          endif
        endif
      else
        ifdef USE_GTK
          IUP_SUFFIX := gtk
        else
          ifndef NO_OVERRIDE
            override USE_MOTIF = Yes
          endif
        endif
      endif
    endif
  else
    ifndef NO_OVERRIDE
      override USE_MOTIF = Yes
    endif
  endif
  
  IUP_LIB ?= $(IUP)/lib/$(TEC_UNAME_LIB_DIR)

  ifdef USE_STATIC
    ifdef USE_CD
      SLIB += $(IUP_LIB)/libiupcd.a
    endif
    ifdef USE_OPENGL
      SLIB += $(IUP_LIB)/libiupgl.a
    endif
    SLIB += $(IUP_LIB)/libiup$(IUP_SUFFIX).a
  else
    ifdef USE_CD
      LIBS += iupcd
    endif
    ifdef USE_OPENGL
      LIBS += iupgl
    endif
    LIBS += iup$(IUP_SUFFIX)
    LDIR += $(IUP_LIB)
  endif

  IUP_INC ?= $(IUP)/include
  INCLUDES += $(IUP_INC)
endif

ifdef USE_CD
  CD_SUFFIX ?=
  ifdef GTK_DEFAULT
    LINK_CAIRO = Yes
    ifdef USE_MOTIF
      CD_SUFFIX := x11
      ifndef NO_OVERRIDE
        override USE_X11 = Yes
      endif
    endif
  else
    ifdef USE_GTK
      LINK_CAIRO = Yes
      CD_SUFFIX := gdk
    else
      ifndef NO_OVERRIDE
        override USE_X11 = Yes
      endif
    endif
  endif
  
  CD_LIB ?= $(CD)/lib/$(TEC_UNAME_LIB_DIR)
  
  ifdef USE_STATIC
    ifdef USE_XRENDER
      CHECK_XRENDER = Yes
      SLIB += $(CD_LIB)/libcdcontextplus.a
      LIBS += Xrender Xft
    endif
    
    ifdef USE_CAIRO
      # To use Cairo with X11 base driver (NOT for GDK)
      # Can NOT be used together with XRender
      SLIB += $(CD_LIB)/libcdcairo.a
      LINK_CAIRO = Yes
    endif
    
    SLIB += $(CD_LIB)/libcd$(CD_SUFFIX).a
  else
    ifdef USE_XRENDER
      CHECK_XRENDER = Yes
      LIBS += cdcontextplus
      LIBS += Xrender Xft
    endif
    
    ifdef USE_CAIRO
      # To use Cairo with X11 base driver (NOT for GDK)
      # Can NOT be used together with XRender
      LIBS += cdcairo
      LINK_CAIRO = Yes
    endif
    
    ifdef USE_HAIKU
	    LIBS += xml2
	  endif
    
    LIBS += cd$(CD_SUFFIX)
    LDIR += $(CD_LIB)
  endif
  
  ifneq ($(findstring Linux26g4, $(TEC_UNAME)), )
    LIBS += fontconfig
  endif
  ifneq ($(findstring Linux3, $(TEC_UNAME)), )
    LIBS += fontconfig
  endif
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    LIBS += fontconfig
  endif
    
  LINK_FREETYPE = Yes

  CD_INC ?= $(CD)/include
  INCLUDES += $(CD_INC)
endif

ifdef USE_IM
  IM_LIB ?= $(IM)/lib/$(TEC_UNAME_LIB_DIR)
  
  ifndef NO_ZLIB
    LINK_ZLIB = Yes
  endif
  
  ifdef USE_STATIC
    SLIB += $(IM_LIB)/libim.a
  else
    LIBS += im
    LDIR += $(IM_LIB)
  endif

  IM_INC ?= $(IM)/include
  INCLUDES += $(IM_INC)
endif

ifdef LINK_FREETYPE
  FREETYPE = freetype
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    # To be compatible with the existing DLLs of cygwin
    #FREETYPE = freetype-6
  endif
  
  ifndef NO_ZLIB
    LINK_ZLIB = Yes
  endif
  
  ifdef USE_STATIC
    ifndef GTK_DEFAULT
      FREETYPE_LIB = $(CD_LIB)
      SLIB += $(FREETYPE_LIB)/lib$(FREETYPE).a
    else
      # If GTK is the default, 
      # use freetype from the system even when static link
      LIBS += $(FREETYPE)
    endif
  else
    LIBS += $(FREETYPE)
  endif
endif

ifdef LINK_ZLIB
  ifndef ZLIB
    ZLIB = z
  endif
  
  ifdef USE_STATIC
    ifdef USE_IM
      ZLIB_LIB = $(IM_LIB)
    else
      ZLIB_LIB = $(CD_LIB)
    endif
    
    SLIB += $(ZLIB_LIB)/lib$(ZLIB).a
  else
    LIBS += $(ZLIB)
  endif
endif

ifdef USE_GLUT
  LIBS += glut
  LDIR += $(GLUT_LIB)
  STDINCS += $(GLUT_INC)
endif

ifdef USE_OPENGL
  ifndef NO_OVERRIDE
    override USE_X11 = Yes
  endif
  ifdef USE_MOTIF
    ifndef USE_IUP3
      LIBS += $(MOTIFGL_LIB)
    endif
  endif
  LIBS += $(OPENGL_LIBS)
  LDIR += $(OPENGL_LIB)
  STDINCS += $(OPENGL_INC)
endif

ifdef USE_MOTIF
  ifndef NO_OVERRIDE
    override USE_X11 = Yes
  endif
  LIBS += Xm
  LDIR += $(MOTIF_LIB)
  STDINCS += $(MOTIF_INC)
  ifneq ($(findstring Linux, $(TEC_UNAME)), )
    X11_LIBS := Xpm $(X11_LIBS)
  endif
  ifneq ($(findstring cygw, $(TEC_UNAME)), )
    X11_LIBS := Xpm $(X11_LIBS)
  endif
endif

ifdef USE_GTK
  ifdef USE_GTK3
    GTKSFX:=3
  else
    GTKSFX:=2
  endif
  
  ifdef USE_PKGCONFIG
    # get compile/link flags via pkg-config
    PKGINCS += $(shell pkg-config --cflags gtk+-$(GTKSFX).0 gdk-$(GTKSFX).0)
    PKGLIBS += $(shell pkg-config --libs gtk+-$(GTKSFX).0 gdk-$(GTKSFX).0)
    GTK_BASE := $(shell pkg-config --variable=prefix gtk+-$(GTKSFX).0)
    GTK := $(GTK_BASE)    
  else
    CHECK_GTK = Yes
    ifneq ($(findstring MacOS, $(TEC_UNAME)), )
  # Option 1 - Fink GTK port
      LDIR += $(GTK)/lib
      ifndef NO_OVERRIDE
        override USE_X11 = Yes
      endif
      ifdef GTK_MAC
        LIBS += gtk-quartz-$(GTKSFX).0 gdk-quartz-$(GTKSFX).0 pango???-1.0
      else
        LIBS += gtk-x11-$(GTKSFX).0 gdk-x11-$(GTKSFX).0 pangox-1.0
      endif
  # Option 2 - Imendio Framework
  #   STDINCS += /Library/Frameworks/Gtk.framework/Headers
  #   STDINCS += /Library/Frameworks/GLib.framework/Headers
  #   STDINCS += /Library/Frameworks/Cairo.framework/Headers
  #   LFLAGS += -framework Gtk
  # Option 3 - GTK-OSX Framework
  #   LDIR += $(GTK)/lib
  #   LFLAGS += -framework Carbon
  #   LIBS += gtk-quartz-$(GTKSFX).0 gdk-quartz-$(GTKSFX).0 pangoft2-1.0

      LIBS += freetype
    else
      # if not the default, then include it for linker
      # must be before the default
      ifdef GTK_BASE
        LDIR += $(GTK)/lib
      endif
      ifndef NO_OVERRIDE
        override USE_X11 = Yes
      endif
      ifdef USE_GTK3
        LIBS += gtk-3 gdk-3 
        LINK_CAIRO = Yes
      else
        LIBS += gtk-x11-2.0 gdk-x11-2.0 pangox-1.0
      endif
    endif
    
    ifdef LINK_CAIRO
      LIBS += pangocairo-1.0 cairo
    endif

    LIBS += gdk_pixbuf-2.0 pango-1.0 gobject-2.0 gmodule-2.0 glib-2.0
    
    STDINCS += $(GTK)/include/atk-1.0 $(GTK)/include/gtk-$(GTKSFX).0 $(GTK)/include/gdk-pixbuf-2.0 
    STDINCS += $(GTK)/include/cairo $(GTK)/include/pango-1.0 $(GTK)/include/glib-2.0

    ifeq ($(TEC_SYSARCH), x64)
      STDINCS += $(GTK)/lib64/glib-2.0/include 
      ifndef USE_GTK3
        STDINCS += $(GTK)/lib64/gtk-2.0/include
      endif
      
      # Add also these to avoid errors in systems that lib64 does not exists
      STDINCS += $(GTK)/lib/glib-2.0/include 
      ifndef USE_GTK3
        STDINCS += $(GTK)/lib/gtk-2.0/include
      endif
      
      # Add also support for newer instalations
      STDINCS += $(GTK)/lib/x86_64-linux-gnu/glib-2.0/include
      ifndef USE_GTK3
        STDINCS += $(GTK)/lib/x86_64-linux-gnu/gtk-2.0/include
      endif
    else 
      ifeq ($(TEC_SYSARCH), ia64)
        STDINCS += $(GTK)/lib64/glib-2.0/include 
        ifndef USE_GTK3
          STDINCS += $(GTK)/lib64/gtk-2.0/include
        endif
      else
        STDINCS += $(GTK)/lib/glib-2.0/include 
        ifndef USE_GTK3
          STDINCS += $(GTK)/lib/gtk-2.0/include
        endif
        
        # Add also support for newer instalations
        STDINCS += $(GTK)/lib/i386-linux-gnu/glib-2.0/include
        ifndef USE_GTK3
          STDINCS += $(GTK)/lib/i386-linux-gnu/gtk-2.0/include
        endif
      endif
    endif
    
    ifneq ($(findstring FreeBSD, $(TEC_UNAME)), )
      STDINCS += /lib/X11R6/include/gtk-2.0
    endif
  endif
endif

ifdef USE_QT
  ifndef NO_OVERRIDE
    override USE_X11 = Yes
  endif
  LIBS += QtGui QtCore
  QT_BASE_INC := /usr/include/qt4
  STDINCS += $(QT_BASE_INC) $(QT_BASE_INC)/QtCore $(QT_BASE_INC)/QtGui
  STDDEFS += -DQT_DLL -DQT_QT3SUPPORT_LIB -DQT3_SUPPORT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_THREAD_SUPPORT
endif

ifdef USE_X11
  LIBS += $(X11_LIBS)
  LDIR += $(X11_LIB)
  STDINCS += $(X11_INC)
endif

ifneq "$(TEC_SYSNAME)" "Haiku"
  LIBS += m
endif

ifneq ($(findstring cygw, $(TEC_UNAME)), )
  WIN_OTHER := Yes

  DEPINCS := $(INCLUDES) $(EXTRAINCS)
  
  # INCLUDES for dependencies, remove references to "c:" and similars
  DEPINCS := $(patsubst c:%, /cygdrive/c%, $(DEPINCS))
  DEPINCS := $(patsubst d:%, /cygdrive/d%, $(DEPINCS))
  DEPINCS := $(patsubst x:%, /cygdrive/x%, $(DEPINCS))
  DEPINCS := $(patsubst t:%, /cygdrive/t%, $(DEPINCS))

  DLIBEXT := dll
  APPEXT := .exe
  # Use the cyg prefix to indicate that it is a Cygwin Posix DLL
  DLIBPRE := cyg

  STDLDFLAGS += -Wl,--out-implib=$(TARGETDIR)/lib$(TARGETNAME).dll.a
endif

#---------------------------------#
#  Building compilation flags that are sets

DEPINCS ?= $(INCLUDES) $(EXTRAINCS)
DEPINCS := $(addprefix -I, $(DEPINCS))

INCLUDES := $(addprefix -I, $(INCLUDES))
STDINCS := $(addprefix -I, $(STDINCS))
EXTRAINCS := $(addprefix -I, $(EXTRAINCS))
DEFINES := $(addprefix -D, $(DEFINES))

LIBS := $(addprefix -l, $(LIBS))
ifdef LDIR
  LDIR := $(addprefix -L, $(LDIR))
endif


#---------------------------------#
# Definitions of private variables

# Library flags for application and dynamic library linker
LFLAGS += $(LDIR) $(LIBS) $(PKGLIBS)
# C compiler flags
CFLAGS   = $(FLAGS) $(STDFLAGS) $(INCLUDES) $(STDINCS) $(PKGINCS) $(EXTRAINCS) $(DEFINES) $(STDDEFS)
# C++ compiler flags
CXXFLAGS = $(CPPFLAGS) $(STDFLAGS) $(INCLUDES) $(STDINCS) $(PKGINCS) $(EXTRAINCS) $(DEFINES) $(STDDEFS)

# Sources with relative path
SOURCES := $(addprefix $(SRCDIR)/, $(SRC))

TARGETAPPNAME := $(TARGETNAME)$(APPEXT)
TARGETSLIBNAME := lib$(TARGETNAME).a
TARGETDLIBNAME := $(DLIBPRE)$(TARGETNAME).$(DLIBEXT)

# Target for applications or libraries
ifeq ($(MAKETYPE), APP)
  TARGET := $(TARGETDIR)/$(TARGETAPPNAME)
else
  ifeq ($(NO_DYNAMIC), Yes)
    TARGET := $(TARGETDIR)/$(TARGETSLIBNAME)
  else
  ifeq ($(NO_STATIC), Yes)
    TARGET := $(TARGETDIR)/$(TARGETDLIBNAME)
  else
    TARGET := $(TARGETDIR)/$(TARGETSLIBNAME) $(TARGETDIR)/$(TARGETDLIBNAME)
  endif
  endif
endif

# OBJ: list of .o, without path
# OBJS: list of .o with relative path
OBJ = $(notdir $(SRC))
OBJ := $(OBJ:.c=.o)
OBJ := $(OBJ:.cpp=.o)
OBJ := $(OBJ:.cxx=.o)
OBJ := $(OBJ:.cc=.o)
OBJ := $(OBJ:.f=.o)
OBJ := $(OBJ:.for=.o)
OBJ := $(OBJ:.rc=.ro)
OBJ := $(OBJ:.cu=.o)
OBJS = $(addprefix $(OBJDIR)/, $(OBJ))

ifdef LOHPACK
  # Package with all LOHs
  LOHS := $(LOHDIR)/$(LOHPACK)
  LOHDIRS :=
else
  ifdef NO_LUAOBJECT
    LH = $(notdir $(SRCLUA))
    LH := $(LH:.lua=.lh)
    LHS = $(addprefix $(LHDIR)/, $(LH))
    LUAS := $(LHS)
  else
    # LOH: list of .loh, without path
    # LOHS: list of .loh, with relative path
    LO = $(notdir $(SRCLUA))
    LO := $(LO:.lua=$(LO_SUFFIX).lo)
    LOS = $(addprefix $(OBJROOT)/, $(LO))

    LOH = $(notdir $(SRCLUA))
    LOH := $(LOH:.lua=$(LO_SUFFIX).loh)
    LOHS = $(addprefix $(LOHDIR)/, $(LOH))
    LUAS := $(LOHS)
  endif
endif

# Construct VPATH variable
P-SRC = $(dir $(SRC))
P-SRC += $(dir $(SRCLUA))
VPATH = .:$(foreach dir,$(P-SRC),$(if $(dir)="./",:$(dir)))


#---------------------------------#
# Main Rule - Build Everything that it is necessary

.PHONY: tecmake
ifeq ($(MAKETYPE), APP)
  tecmake: print-start system-check directories application scripts
else
  ifeq ($(NO_DYNAMIC), Yes)
    tecmake: print-start system-check directories static-lib
  else
  ifeq ($(NO_STATIC), Yes)
    tecmake: print-start system-check directories dynamic-lib
  else
    tecmake: print-start system-check directories static-lib dynamic-lib
  endif
  endif
endif

.PHONY: print-start
print-start:
	@echo ''; echo 'Tecmake: starting [ $(TARGETNAME):$(TEC_UNAME) ]'

.PHONY: system-check
system-check:
  ifdef CHECK_XRENDER
    ifdef UNIX_POSIX
			@echo ''; echo 'Tecmake: check failed, XRender NOT available in this system.'; echo ''; exit 1;
    endif
  endif
  ifdef CHECK_GTK
    ifndef GTK_BASE
      ifdef UNIX_POSIX
			@echo ''; echo 'Tecmake: check failed, GTK NOT available in this system.'; echo ''; exit 1;
      else
        ifneq ($(findstring Linux24, $(TEC_UNAME)), )
					@echo ''; echo 'Tecmake: check failed, GTK too OLD in this system.'; echo ''; exit 1;
        endif
      endif
    endif
  endif
  ifdef CHECK_GDIPLUS
    ifdef WIN_OTHER
			@echo ''; echo 'Tecmake: check failed, GDI+ NOT available in this system.'; echo ''; exit 1;
    endif
  endif

#---------------------------------#
# Dynamic Library Build

.PHONY: dynamic-lib
dynamic-lib: $(TARGETDIR)/$(TARGETDLIBNAME)

$(TARGETDIR)/$(TARGETDLIBNAME) : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: linking $(@F) ...
	$(ECHO)$(LD) $(STDLDFLAGS) -o $@ $(OBJS) $(SLIB) $(LFLAGS)
	@echo ''; echo 'Tecmake: Dynamic Library ($@) Done.'; echo ''


#---------------------------------#
# Static Library Build

.PHONY: static-lib
static-lib: $(TARGETDIR)/$(TARGETSLIBNAME)

$(TARGETDIR)/$(TARGETSLIBNAME) : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: librarian $(@F) ...
	$(ECHO)$(AR) $(STDLFLAGS) $@ $(OBJS) $(SLIB) $(LCFLAGS)
	@echo ''; echo Tecmake: updating lib TOC $(@F) ...
	$(ECHO)-$(RANLIB) $@
	@echo ''; echo 'Tecmake: Static Library ($@) Done.'; echo ''


#---------------------------------#
# Application Build

.PHONY: application
application: $(TARGETDIR)/$(TARGETAPPNAME)

$(TARGETDIR)/$(TARGETAPPNAME) : $(LUAS) $(OBJS) $(EXTRADEPS)
	@echo ''; echo Tecmake: linking $(@F) ...
	$(ECHO)$(LINKER) -o $@ $(OBJS) $(SLIB) $(LFLAGS)
	@if [ ! -z "$(STRIP)" ]; then \
	   echo ''; echo 'Tecmake: striping debug information' ;\
	   strip $@ ;\
	 fi
	@echo ''; echo 'Tecmake: Application ($@) Done.'; echo ''


#---------------------------------#
#  Application Scripts

# Script name
SRELEASE := $(SRCDIR)/$(TARGETNAME)

.PHONY: scripts
ifdef NO_SCRIPTS
  scripts: ;
else
  scripts: $(SRELEASE) ;
endif

$(SRELEASE): $(MAKENAME)
	@echo ''; echo 'Tecmake: building script $(@F)'
	@echo "#!/bin/csh" > $@
	@echo "# Script generated automatically by tecmake v$(VERSION)" >> $@
	@echo "# Remove the comment bellow to set the LD_LIBRARY_PATH if needed." >> $@
	@echo '#setenv LD_LIBRARY_PATH $(MYLIB1)/lib/$${TEC_UNAME}:$(MYLIB2)/lib/$${TEC_UNAME}:$$LD_LIBRARY_PATH' >> $@
	@echo 'if ( -r app.env ) source app.env' >> $@
	@echo 'exec $(TARGETROOT)/$$TEC_UNAME/$(TARGETNAME) $$*' >> $@
	@chmod a+x $@


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

$(OBJDIR)/%.o: $(SRCDIR)/%.f
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(FC) -c $(FFLAGS) -o $@ $<

$(OBJDIR)/%.o: $(SRCDIR)/%.for
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(FC) -c $(FFLAGS) -o $@ $<

$(OBJDIR)/%.ro:  $(SRCDIR)/%.rc
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(RCC) $(RCFLAGS) -O coff -o $@ $<

$(LHDIR)/%.lh:  $(SRCLUADIR)/%.lua
	@echo ''; echo Tecmake: generating $(<F) ...
	$(ECHO)$(BIN2C) $< > $@

$(LOHDIR)/%.loh:  $(OBJROOT)/%.lo
	@echo ''; echo Tecmake: generating $(<F) ...
	$(ECHO)$(BIN2C) $< > $@

$(OBJROOT)/%$(LO_SUFFIX).lo:  $(SRCLUADIR)/%.lua
	@echo ''; echo Tecmake: compiling $(<F) ...
	$(ECHO)$(LUAC) -o $@ $<

ifdef LOHPACK
$(LOHDIR)/$(LOHPACK):  $(SRCLUA)
	@echo ''; echo Tecmake: generating $(<F) ...
	$(ECHO)$(LUABIN) $(LUAPRE) $(LUAPREFLAGS) -l $(SRCLUADIR) -o $@ $(SRCLUA)
endif


#---------------------------------#
# Dependencies

#   Build dependencies
.PHONY: depend
depend: $(DEPEND)

$(DEPEND): $(MAKENAME)
  ifdef SRC
	  @echo "" > $(DEPEND)
	  @which $(CPPC) 2> /dev/null 1>&2 ;\
	  if [ $$? -eq 0 ]; then \
	    echo "Tecmake: Building Dependencies ... [ $(DEPEND) ] (can be slow)" ;\
	    $(CPPC) $(DEPINCS) $(DEFINES) $(STDDEFS) -MM $(SOURCES) | \
	    sed -e '1,$$s/^\([^ ]\)/$$(OBJDIR)\/\1/' > $(DEPEND) ;\
	  else \
	    echo "" ;\
	    echo "Tecmake: error, $(CPPC) not found. Dependencies can not be built." ;\
	    echo "Must set NO_DEPEND=Yes." ;\
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

#   Remove extra files
.PHONY: clean-extra
clean-extra:
	rm -f $(DEPEND) $(SRELEASE) so_locations

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

.PHONY: clean-dir
clean-dir:
	rm -fr $(OBJROOT) $(TARGETROOT)

#   Remove target and object files
.PHONY: clean
clean: clean-target clean-obj

#   Remove symbols from executables
.PHONY: strip
strip:
	test -r $(TARGETDIR)/$(TARGETAPPNAME) && strip $(TARGETDIR)/$(TARGETAPPNAME)

#   Rebuild target and object files
.PHONY: rebuild
rebuild: clean-obj clean-target tecmake

#   Rebuild target without rebuilding object files
.PHONY: relink
relink: clean-target tecmake

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
	  (rm -f $(TARGETROOT)/$$d/$(TARGETNAME) $(TARGETROOT)/$$d/$(TARGETSLIBNAME) $(TARGETROOT)/$$d/$(TARGETDLIBNAME)) ;\
	done

#---------------------------------#
# Remote build
# There must be aliases in DNS for the known UNAMES
.PHONY: $(UNAMES)
$(UNAMES):
	@cwd=`csh -c "\\pwd"` ; home=`csh -c "cd;\\pwd"` ;\
	 dir=`echo $$cwd | sed -e "s|$$home/||"` ;\
	 xterm -bg black -fg lightblue -T "Tecmake: $@ ($(TARGETNAME))" -e ssh $@ $(REMOTE) $$dir $(TECMAKEFLAGS) $(MAKEFLAGS) & 2> /dev/null


#---------------------------------#

.PHONY: version
version:
	@echo "Tecmake Posix Version $(VERSION)"

#---------------------------------#
