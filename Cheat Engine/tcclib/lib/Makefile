#
# Tiny C Compiler Makefile for libtcc1.a
#

TOP = ..
include $(TOP)/Makefile
VPATH = $(TOPSRC)/lib $(TOPSRC)/win32/lib
T = $(or $(CROSS_TARGET),$(NATIVE_TARGET),unknown)
X = $(if $(CROSS_TARGET),$(CROSS_TARGET)-)

XTCC ?= $(TOP)/$(X)tcc$(EXESUF)
XCC = $(XTCC)
XAR = $(XTCC) -ar
XFLAGS-unx = -B$(TOPSRC)
XFLAGS-win = -B$(TOPSRC)/win32 -I$(TOPSRC)/include
XFLAGS = $(XFLAGS$(XCFG)) -I$(TOP)
XCFG = $(or $(findstring -win,$T),-unx)
S = $(if $(findstring yes,$(SILENT)),@$(info * $@))

# in order to use gcc, type: make <target>-libtcc1-usegcc=yes
arm-libtcc1-usegcc ?= no

# This makes bounds checking 40%..60% faster.
#x86_64-libtcc1-usegcc=yes
#i386-libtcc1-usegcc=yes

ifeq "$($(T)-libtcc1-usegcc)" "yes"
 XCC = $(CC)
 XAR = $(AR)
 XFLAGS = $(CFLAGS) -fPIC -gstabs -fno-omit-frame-pointer -Wno-unused-function -Wno-unused-variable
endif

ifneq ($(CONFIG_backtrace),no)
# only for native compiler
ifneq ($(CONFIG_bcheck),no)
$(X)BCHECK_O = bcheck.o
endif
$(X)BT_O = bt-exe.o bt-log.o
$(X)B_O = $(BCHECK_O) bt-exe.o bt-log.o bt-dll.o
endif

DSO_O = dsohandle.o

I386_O = libtcc1.o alloca86.o alloca86-bt.o $(BT_O)
X86_64_O = libtcc1.o alloca86_64.o alloca86_64-bt.o $(BT_O)
ARM_O = libtcc1.o armeabi.o alloca-arm.o armflush.o fetch_and_add_arm.o $(BT_O)
ARM64_O = lib-arm64.o fetch_and_add_arm64.o $(BT_O)
RISCV64_O = lib-arm64.o fetch_and_add_riscv64.o $(BT_O)
WIN_O = crt1.o crt1w.o wincrt1.o wincrt1w.o dllcrt1.o dllmain.o

OBJ-i386 = $(I386_O) $(BCHECK_O) $(DSO_O)
OBJ-x86_64 = $(X86_64_O) va_list.o $(BCHECK_O) $(DSO_O)
OBJ-x86_64-osx = $(X86_64_O) va_list.o $(BCHECK_O)
OBJ-i386-win32 = $(I386_O) chkstk.o $(B_O) $(WIN_O)
OBJ-x86_64-win32 = $(X86_64_O) chkstk.o $(B_O) $(WIN_O)
OBJ-arm64 = $(ARM64_O) $(BCHECK_O) $(DSO_O)
OBJ-arm = $(ARM_O) $(BCHECK_O) $(DSO_O)
OBJ-arm-fpa = $(ARM_O) $(DSO_O)
OBJ-arm-fpa-ld = $(ARM_O) $(DSO_O)
OBJ-arm-vfp = $(ARM_O) $(DSO_O)
OBJ-arm-eabi = $(ARM_O) $(DSO_O)
OBJ-arm-eabihf = $(ARM_O) $(DSO_O)
OBJ-arm-wince = $(ARM_O) $(WIN_O)
OBJ-riscv64 = $(RISCV64_O) $(BCHECK_O) $(DSO_O)

OBJ-extra = $(filter $(B_O),$(OBJ-$T))
OBJ-libtcc1 = $(addprefix $(X),$(filter-out $(OBJ-extra),$(OBJ-$T)))

ALL = $(addprefix $(TOP)/,$(X)libtcc1.a $(OBJ-extra))

all: $(ALL)

$(TOP)/$(X)libtcc1.a : $(OBJ-libtcc1)
	$S$(XAR) rcs $@ $^

$(X)%.o : %.c
	$S$(XCC) -c $< -o $@ $(XFLAGS)

$(X)%.o : %.S
	$S$(XCC) -c $< -o $@ $(XFLAGS)

$(TOP)/%.o : %.c
	$S$(XCC) -c $< -o $@ $(XFLAGS)

$(TOP)/bcheck.o : XFLAGS += -g $(if $(CONFIG_musl),-DTCC_MUSL)
$(TOP)/bt-exe.o : $(TOP)/tccrun.c

$(X)crt1w.o : crt1.c
$(X)wincrt1w.o : wincrt1.c

clean :
	rm -f *.a *.o $(ALL)
