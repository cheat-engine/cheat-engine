#
# Tiny C Compiler Makefile for libtcc1.a
#

TOP = ..
include $(TOP)/Makefile
VPATH = $(TOPSRC)/lib $(TOPSRC)/win32/lib
T = $(or $(CROSS_TARGET),$(NATIVE_TARGET),unknown)
X = $(if $(CROSS_TARGET),$(CROSS_TARGET)-)
BIN = $(TOP)/$(X)libtcc1.a

XTCC ?= $(TOP)/$(X)tcc$(EXESUF)
XCC = $(XTCC)
XAR = $(XTCC) -ar
XFLAGS-unx = -B$(TOPSRC)
XFLAGS-win = -B$(TOPSRC)/win32 -I$(TOPSRC)/include
XFLAGS = $(XFLAGS$(XCFG))
XCFG = $(or $(findstring -win,$T),-unx)

# in order to use gcc, tyoe: make <target>-libtcc1-usegcc=yes
arm-libtcc1-usegcc ?= no

ifeq "$($(T)-libtcc1-usegcc)" "yes"
 XCC = $(CC)
 XAR = $(AR)
 XFLAGS = $(CFLAGS) -fPIC
endif

# only for native compiler
$(X)BCHECK_O = bcheck.o

ifeq ($(CONFIG_musl)$(CONFIG_uClibc),yes)
 BCHECK_O =
endif

ifdef CONFIG_OSX
 XFLAGS += -D_ANSI_SOURCE
endif

I386_O = libtcc1.o alloca86.o alloca86-bt.o
X86_64_O = libtcc1.o alloca86_64.o alloca86_64-bt.o
ARM_O = libtcc1.o armeabi.o alloca-arm.o armflush.o
ARM64_O = lib-arm64.o
WIN_O = crt1.o crt1w.o wincrt1.o wincrt1w.o dllcrt1.o dllmain.o

OBJ-i386 = $(I386_O) $(BCHECK_O)
OBJ-x86_64 = $(X86_64_O) va_list.o $(BCHECK_O)
OBJ-x86_64-osx = $(X86_64_O) va_list.o
OBJ-i386-win32 = $(I386_O) chkstk.o bcheck.o $(WIN_O)
OBJ-x86_64-win32 = $(X86_64_O) chkstk.o bcheck.o $(WIN_O)
OBJ-arm64 = $(ARM64_O)
OBJ-arm = $(ARM_O)
OBJ-arm-fpa = $(ARM_O)
OBJ-arm-fpa-ld = $(ARM_O)
OBJ-arm-vfp = $(ARM_O)
OBJ-arm-eabi = $(ARM_O)
OBJ-arm-eabihf = $(ARM_O)
OBJ-arm-wince = $(ARM_O) $(WIN_O)

$(BIN) : $(patsubst %.o,$(X)%.o,$(OBJ-$T))
	$(XAR) rcs $@ $^

$(X)%.o : %.c
	$(XCC) -c $< -o $@ $(XFLAGS)

$(X)%.o : %.S
	$(XCC) -c $< -o $@ $(XFLAGS)

$(X)crt1w.o : crt1.c
$(X)wincrt1w.o : wincrt1.c

clean :
	rm -f *.a *.o $(BIN)
