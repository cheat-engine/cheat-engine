TOP = ../..
include $(TOP)/Makefile
SRC = $(TOPSRC)/tests/tests2
VPATH = $(SRC)

TESTS = $(patsubst %.c,%.test,$(sort $(notdir $(wildcard $(SRC)/*.c))))

# some tests do not pass on all platforms, remove them for now
SKIP = 34_array_assignment.test # array assignment is not in C standard
ifeq ($(CONFIG_arm_eabi),yes) # not ARM soft-float
 SKIP += 22_floating_point.test
endif
ifdef CONFIG_OSX
 SKIP += 40_stdio.test 42_function_pointer.test
 FLAGS += -w
endif
ifeq ($(ARCH),x86_64)
 SKIP += 73_arm64.test
endif
ifeq (,$(filter i386,$(ARCH)))
 SKIP += 98_al_ax_extend.test 99_fastcall.test
endif
ifeq (,$(filter i386 x86_64,$(ARCH)))
 SKIP += 85_asm-outside-function.test
endif
ifeq (-$(findstring gcc,$(CC))-,--)
 SKIP += $(patsubst %.expect,%.test,$(GEN-ALWAYS))
endif
ifeq (-$(CONFIG_WIN32)-$(CONFIG_i386)$(CONFIG_arm)-,--yes-)
 SKIP += 95_bitfields%.test # type_align is different on 32bit-non-windows
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

# Filter source directory in warnings/errors (out-of-tree builds)
FILTER = 2>&1 | sed 's,$(SRC)/,,g'
# Filter some always-warning
ifeq (-$(findstring arm,$(ARCH))-,-arm-)
FILTER += 2>&1 | grep -v 'warning: soft float ABI currently not supported'
endif

all test tests2.all: $(filter-out $(SKIP),$(TESTS)) ;

%.test: %.c %.expect
	@echo Test: $*...
	@$(if $(NORUN),$(T1),$(T2)) $(if $(NODIFF),,$(T3))

T1 = $(TCC) $(FLAGS) $< -o a.exe && ./a.exe $(ARGS)
T2 = $(TCC) $(FLAGS) -run $< $(ARGS)
T3 = $(FILTER) >$*.output 2>&1 || true \
     && diff -Nbu $(filter %.expect,$^) $*.output \
     && rm -f $*.output $(filter $*.expect,$(GEN-ALWAYS))

# run single test and update .expect file, e.g. "make tests2.37+"
tests2.%+:
	@$(MAKE) $(call F2,$(call F1,$*)) --no-print-directory

# just run tcc to see the output, e.g. "make tests2.37-"
tests2.%-:
	@$(MAKE) $(call F1,$*) NODIFF=true --no-print-directory

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
GEN = $(if $(filter -dt,$(FLAGS)),$(GEN-TCC),$(GEN-CC))
GEN-CC = $(CC) -w -std=gnu99 $(FLAGS) $1 -o a.exe && ./a.exe $(ARGS)
GEN-TCC = $(TCC) $(FLAGS) -run $1 $(ARGS)
GEN-MSC = $(MS-CC) $1 && ./$(basename $@).exe
MS-CC = cl

# tell make not to delete
.PRECIOUS: %.expect

# force .expect generation for these files
$(sort $(GEN-ALWAYS) $(UPDATE)) : force
force:

clean:
	rm -f fred.txt *.output a.exe $(GEN-ALWAYS)
