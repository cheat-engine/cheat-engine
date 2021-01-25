TOP = ../..
include $(TOP)/Makefile
SRC = $(TOPSRC)/tests/tests2
VPATH = $(SRC)

TESTS = $(patsubst %.c,%.test,\
    $(sort $(notdir $(wildcard $(SRC)/??_*.c)))\
    $(sort $(notdir $(wildcard $(SRC)/???_*.c))))

# some tests do not pass on all platforms, remove them for now
SKIP = 34_array_assignment.test # array assignment is not in C standard
ifeq ($(CONFIG_arm_eabi),yes) # not ARM soft-float
 SKIP += 22_floating_point.test
endif
ifdef CONFIG_OSX
 SKIP += 40_stdio.test 42_function_pointer.test
endif
ifeq ($(ARCH),x86_64)
 SKIP += 73_arm64.test
endif
ifeq (,$(filter i386,$(ARCH)))
 SKIP += 98_al_ax_extend.test 99_fastcall.test
endif
ifeq (,$(filter i386 x86_64,$(ARCH)))
 SKIP += 85_asm-outside-function.test # x86 asm
endif
ifeq ($(CONFIG_backtrace),no)
 SKIP += 112_backtrace.test
 SKIP += 113_btdll.test
 CONFIG_bcheck = no # no bcheck without backtrace
endif
ifeq ($(CONFIG_bcheck),no)
 SKIP += 114_bound_signal.test
 SKIP += 115_bound_setjmp.test
 SKIP += 116_bound_setjmp2.test
 SKIP += 117_builtins.test
endif
ifeq ($(CONFIG_dll),no)
 SKIP += 113_btdll.test # no shared lib support yet
endif
ifeq (-$(findstring gcc,$(CC))-,--)
 SKIP += $(patsubst %.expect,%.test,$(GEN-ALWAYS))
endif
ifeq (-$(CONFIG_WIN32)-$(CONFIG_i386)$(CONFIG_arm)-,--yes-)
 SKIP += 95_bitfields%.test # type_align is different on 32bit-non-windows
endif
ifeq (-$(CONFIG_WIN32)-,-yes-)
 SKIP += 106_versym.test # No pthread support
 SKIP += 114_bound_signal.test # No pthread support
endif
ifneq (,$(filter OpenBSD FreeBSD NetBSD,$(TARGETOS)))
 SKIP += 106_versym.test # no pthread_condattr_setpshared
 SKIP += 114_bound_signal.test # libc problem signal/fork
 SKIP += 116_bound_setjmp2.test # No TLS_FUNC/TLS_VAR in bcheck.c
endif

# Some tests might need arguments
ARGS =
31_args.test : ARGS = arg1 arg2 arg3 arg4 arg5
46_grep.test : ARGS = '[^* ]*[:a:d: ]+\:\*-/: $$' $(SRC)/46_grep.c

# And some tests don't test the right thing with -run
NORUN =
42_function_pointer.test : NORUN = true

# Some tests might need different flags
FLAGS =
76_dollars_in_identifiers.test : FLAGS += -fdollars-in-identifiers

# These tests run several snippets from the same file one by one
60_errors_and_warnings.test : FLAGS += -dt
96_nodata_wanted.test : FLAGS += -dt

# Always generate certain .expects (don't put these in the GIT),
GEN-ALWAYS =
# GEN-ALWAYS += 95_bitfields.expect # does not work

# using the ms compiler for the really ms-compatible bitfields
95_bitfields_ms.test : GEN = $(GEN-MSC)

# this test compiles/links two files:
104_inline.test : FLAGS += $(subst 104,104+,$1)
104_inline.test : GEN = $(GEN-TCC)

# this test needs two files, and we want to invoke the linker
120_alias.test : FLAGS += $(subst 120,120+,$1)
120_alias.test : GEN = $(GEN-TCC)
120_alias.test : NORUN = true

# this test needs pthread
106_versym.test: FLAGS += -pthread
106_versym.test: NORUN = true

# constructor/destructor
108_constructor.test: NORUN = true

112_backtrace.test: FLAGS += -dt -b
112_backtrace.test 113_btdll.test: FILTER += \
    -e 's;[0-9A-Fa-fx]\{5,\};........;g' \
    -e 's;0x[0-9A-Fa-f]\{1,\};0x?;g'

# this test creates two DLLs and an EXE
113_btdll.test: T1 = \
    $(TCC) -bt $1 -shared -D DLL=1 -o a1$(DLLSUF) && \
    $(TCC) -bt $1 -shared -D DLL=2 -o a2$(DLLSUF) && \
    $(TCC) -bt $1 a1$(DLLSUF) a2$(DLLSUF) -Wl,-rpath=. -o a.exe && \
    ./a.exe

114_bound_signal.test: FLAGS += -b
114_bound_signal.test: NORUN = true # tcc -run does not support fork and -b and SELINUX
115_bound_setjmp.test: FLAGS += -b
116_bound_setjmp2.test: FLAGS += -b
117_builtins.test: T1 = ( $(TCC) -run $1 && $(TCC) -b -run $1 )
ifneq ($(CONFIG_bcheck),no)
121_struct_return.test: FLAGS += -b
122_vla_reuse.test: FLAGS += -b
endif

# Filter source directory in warnings/errors (out-of-tree builds)
FILTER = 2>&1 | sed -e 's,$(SRC)/,,g'

all test tests2.all: $(filter-out $(SKIP),$(TESTS))
	@$(MAKE) clean --no-print-directory -s

%.test: %.c %.expect
	@echo Test: $*...
	@$(call T1,$<) $(T3)

T1 = $(TCC) $(FLAGS) $(T2) $(ARGS)
T2 = $(if $(NORUN),$1 -o a.exe && ./a.exe,-run $1)
T3 = $(FILTER) >$*.output 2>&1 || true \
     && diff -Nbu $(filter %.expect,$^) $*.output \
     && rm -f $*.output $(filter $*.expect,$(GEN-ALWAYS))

# run single test and update .expect file, e.g. "make tests2.37+"
tests2.%+:
	@$(MAKE) $(call F2,$(call F1,$*)) --no-print-directory

# just run tcc to see the output, e.g. "make tests2.37-"
tests2.%-:
	@$(MAKE) $(call F1,$*) T3= --no-print-directory

# run single test, e.g. "make tests2.37"
tests2.%:
	@$(MAKE) $(call F1,$*) --no-print-directory

F1 = $(or $(filter $1_%,$(TESTS)),$1_???.test)
F2 = $1 UPDATE="$(patsubst %.test,%.expect,$1)"

# automatically generate .expect files with gcc:
%.expect :
	@echo Generating: $@
	@$(call GEN,$(SRC)/$*.c) $(FILTER) >$@ 2>&1
	@rm -f *.exe *.obj *.pdb

# using TCC for .expect if -dt in FLAGS
GEN = $(if $(filter -dt -bt -b,$(FLAGS)),$(GEN-TCC),$(GEN-CC))
GEN-CC = $(CC) -w -std=gnu99 $(FLAGS) $1 -o a.exe && ./a.exe $(ARGS)
GEN-TCC = $(T1)
GEN-MSC = $(MS-CC) $1 && ./$(basename $@).exe
MS-CC = cl

# tell make not to delete
.PRECIOUS: %.expect

# force .expect generation for these files
$(sort $(GEN-ALWAYS) $(UPDATE)) : force
force:

clean :
	rm -f fred.txt *.output a.exe *.dll *.so *.def $(GEN-ALWAYS)
