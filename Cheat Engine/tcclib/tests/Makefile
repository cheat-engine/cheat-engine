#
# Tiny C Compiler Makefile - tests
#

TOP = ..
include $(TOP)/Makefile
VPATH = $(TOPSRC)/tests $(TOPSRC) $(TOP)
CFLAGS := $(filter-out -W% -g% -O%,$(CFLAGS)) -I$(TOPSRC) $(LDFLAGS)

# what tests to run
TESTS = \
 hello-exe \
 hello-run \
 libtest \
 libtest_mt \
 test3 \
 memtest \
 dlltest \
 abitest \
 asm-c-connect-test \
 vla_test-run \
 cross-test \
 tests2-dir \
 pp-dir

# test4_static -- Not all relocation types are implemented yet.
# asmtest / asmtest2 -- minor differences with gcc

ifneq ($(CONFIG_bcheck),no)
 TESTS += btest test1b
endif
ifeq ($(CONFIG_dll),no)
 TESTS := $(filter-out dlltest, $(TESTS))
endif
ifeq (,$(filter arm64 i386 x86_64,$(ARCH)))
 TESTS := $(filter-out vla_test-run,$(TESTS))
endif
ifeq ($(CONFIG_arm_eabi),yes)
 TESTS := $(filter-out test3,$(TESTS))
endif
ifeq (,$(filter i386 x86_64,$(ARCH)))
 TESTS := $(filter-out asm-c-connect-test,$(TESTS))
endif
ifeq ($(OS),Windows_NT) # for libtcc_test to find libtcc.dll
 PATH := $(CURDIR)/$(TOP)$(if $(findstring ;,$(PATH)),;,:)$(PATH)
endif
ifdef CONFIG_OSX
LIBS += $(LINK_LIBTCC)
endif
ifeq ($(ARCH),arm)
# tcctest refers to the alignment of functions, and with thumb mode
# the low bit of code addresses selects the mode, so the "alignment"
# of functions via bit masking comes out as 1.  Just disable thumb.
test.ref: CFLAGS+=-marm
endif
ifeq ($(ARCH)$(CONFIG_WIN32),i386)
# tcctest.c:get_asm_string uses a construct that is checked too strictly
# by GCC in 32bit mode when PIC is enabled.
test.ref: CFLAGS+=-fno-PIC -fno-PIE -Wl,-z,notext
endif
ifeq ($(CC_NAME),msvc)
test.ref abitest : CC = gcc
endif
ifeq ($(TARGETOS),OpenBSD)
dlltest: CFLAGS+=-fno-stack-protector
endif
ifneq (,$(filter FreeBSD NetBSD,$(TARGETOS)))
  # test3 has dlsym problems
  TESTS := $(filter-out test3,$(TESTS))
  TESTS += test1
endif

RUN_TCC = $(NATIVE_DEFINES) -run $(TOPSRC)/tcc.c $(TCCFLAGS)
DISAS = objdump -d
ifdef CONFIG_OSX
DUMPTCC = (set -x; $(TOP)/tcc -vv; otool -L $(TOP)/tcc; exit 1)
else
DUMPTCC = (set -x; $(TOP)/tcc -vv; ldd $(TOP)/tcc; exit 1)
endif

all test :
	@$(MAKE) --no-print-directory -s clean
	@$(MAKE) --no-print-directory -s -r $(TESTS)

hello-exe: ../examples/ex1.c
	@echo ------------ $@ ------------
	$(TCC) $< -o hello$(EXESUF) && ./hello$(EXESUF) || $(DUMPTCC)

hello-run: ../examples/ex1.c
	@echo ------------ $@ ------------
	$(TCC) -run $< || $(DUMPTCC)

libtes%: libtcc_tes%$(EXESUF)
	@echo ------------ $@ ------------
	./libtcc_tes$*$(EXESUF) $(TOPSRC)/tcc.c $(TCCFLAGS) $(NATIVE_DEFINES)

libtcc_test$(EXESUF): libtcc_test.c $(LIBTCC)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

libtcc_test_mt$(EXESUF): libtcc_test_mt.c $(LIBTCC)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS)

%-dir:
	@echo ------------ $@ ------------
	$(MAKE) -k -C $*

# test.ref - generate using cc
test.ref: tcctest.c
	$(CC) -o tcctest.gcc $< $(NATIVE_DEFINES) $(CFLAGS) -w -O0 -std=gnu99 -fno-omit-frame-pointer
	./tcctest.gcc > $@

# auto test
test1 test1b: tcctest.c test.ref
	@echo ------------ $@ ------------
	$(TCC) $(RUN_TCC) -w -run $< > test.out1
	@diff -u test.ref test.out1 && echo "$(AUTO_TEST) OK"

# iterated test2 (compile tcc then compile tcctest.c !)
test2 test2b: tcctest.c test.ref
	@echo ------------ $@ ------------
	$(TCC) $(RUN_TCC) $(RUN_TCC) -w -run $< > test.out2
	@diff -u test.ref test.out2 && echo "$(AUTO_TEST)2 OK"

# iterated test3 (compile tcc then compile tcc then compile tcctest.c !)
test3 test3b: tcctest.c test.ref
	@echo ------------ $@ ------------
	$(TCC) $(RUN_TCC) $(RUN_TCC) $(RUN_TCC) -w -run $< > test.out3
	@diff -u test.ref test.out3 && echo "$(AUTO_TEST)3 OK"

AUTO_TEST = Auto Test
test%b : TCCFLAGS += -b -bt1
test%b : AUTO_TEST = Auto Bound-Test

# binary output test
test4: tcctest.c test.ref
	@echo ------------ $@ ------------
# object + link output
	$(TCC) -c -o tcctest3.o $<
	$(TCC) -o tcctest3 tcctest3.o
	./tcctest3 > test3.out
	@if diff -u test.ref test3.out ; then echo "Object $(AUTO_TEST) OK"; fi
# dynamic output
	$(TCC) -o tcctest1 $<
	./tcctest1 > test1.out
	@if diff -u test.ref test1.out ; then echo "Dynamic $(AUTO_TEST) OK"; fi
# dynamic output + bound check
	$(TCC) -b -o tcctest4 $<
	./tcctest4 > test4.out
	@if diff -u test.ref test4.out ; then echo "BCheck $(AUTO_TEST) OK"; fi

test4_static: tcctest.c test.ref
	@echo ------------ $@ ------------
# static output.
	$(TCC) -static -o tcctest2 $<
	./tcctest2 > test2.out
	@if diff -u test.ref test2.out ; then echo "Static $(AUTO_TEST) OK"; fi

# use tcc to create libtcc.so/.dll and the tcc(.exe) frontend and run them
dlltest:
	@echo ------------ $@ ------------
	$(TCC) $(NATIVE_DEFINES) -DLIBTCC_AS_DLL $(TOPSRC)/libtcc.c $(LIBS) -shared -o libtcc2$(DLLSUF)
	$(TCC) $(NATIVE_DEFINES) -DONE_SOURCE=0 $(TOPSRC)/tcc.c libtcc2$(DLLSUF) $(LIBS) -Wl,-rpath=. -o tcc2$(EXESUF)
	./tcc2$(EXESUF) $(TCCFLAGS) $(RUN_TCC) -run $(TOPSRC)/examples/ex1.c
ifndef CONFIG_WIN32
	@echo ------------ $@ with PIC ------------
	$(CC) $(CFLAGS) -fPIC $(NATIVE_DEFINES) -DLIBTCC_AS_DLL -c $(TOPSRC)/libtcc.c
	$(TCC) libtcc.o $(LIBS) -shared -o libtcc2$(DLLSUF)
	$(TCC) $(NATIVE_DEFINES) -DONE_SOURCE=0 $(TOPSRC)/tcc.c libtcc2$(DLLSUF) $(LIBS) -Wl,-rpath=. -o tcc2$(EXESUF)
	./tcc2$(EXESUF) $(TCCFLAGS) $(RUN_TCC) -run $(TOPSRC)/examples/ex1.c
endif
	@rm tcc2$(EXESUF) libtcc2$(DLLSUF)

memtest:
	@echo ------------ $@ ------------
	$(CC) $(CFLAGS) $(NATIVE_DEFINES) -DMEM_DEBUG=2 $(TOPSRC)/tcc.c $(LIBS) -o memtest-tcc$(EXESUF)
	./memtest-tcc$(EXESUF) $(TCCFLAGS) $(NATIVE_DEFINES) $(TOPSRC)/tcc.c $(LIBS)
	./memtest-tcc$(EXESUF) $(TCCFLAGS) $(NATIVE_DEFINES) -run $(TOPSRC)/tcc.c $(TCCFLAGS) -w $(TOPSRC)/tests/tcctest.c
	@echo OK

# memory and bound check auto test
BOUNDS_OK  = 1 4 8 10 14 16
BOUNDS_FAIL= 2 5 6 7 9 11 12 13 15 17 18

btest: boundtest.c
	@echo ------------ $@ ------------
	@for i in $(BOUNDS_OK); do \
	   if $(TCC) -b -run $< $$i >/dev/null 2>&1 ; then \
	       echo "Test $$i succeeded as expected" ; \
	   else\
	       echo "Failed positive test $$i" ; exit 1 ; \
	   fi ;\
	done ;\
	for i in $(BOUNDS_FAIL); do \
	   if $(TCC) -b -bt1 -run $< $$i >/dev/null 2>&1 ; then \
	       echo "Failed negative test $$i" ; exit 1 ;\
	   else\
	       echo "Test $$i failed as expected" ; \
	   fi ;\
	done ;\
	echo Bound test OK

# speed test
speedtest: ex2 ex3
	@echo ------------ $@ ------------
	time ./ex2 1238 2 3 4 10 13 4
	time $(TCC) -run $(TOPSRC)/examples/ex2.c 1238 2 3 4 10 13 4
	time ./ex3 35
	time $(TCC) -run $(TOPSRC)/examples/ex3.c 35

weaktest: tcctest.c test.ref
	@echo ------------ $@ ------------
	$(TCC) -c $< -o weaktest.tcc.o
	$(CC) -c $< -o weaktest.gcc.o $(NATIVE_DEFINES) $(CFLAGS) -w -O0 -std=gnu99 -fno-omit-frame-pointer
	objdump -t weaktest.tcc.o | grep ' w ' | sed -e 's/.* \([a-zA-Z0-9_]*\)$$/\1/' | LC_ALL=C sort > weaktest.tcc.o.txt
	objdump -t weaktest.gcc.o | grep ' w ' | sed -e 's/.* \([a-zA-Z0-9_]*\)$$/\1/' | LC_ALL=C sort > weaktest.gcc.o.txt
	diff weaktest.gcc.o.txt weaktest.tcc.o.txt && echo "Weak Auto Test OK"

ex%: $(TOPSRC)/examples/ex%.c
	$(CC) -o $@ $< $(CFLAGS)

# tiny assembler testing
asmtest.ref: asmtest.S
	$(CC) -Wa,-W -o asmtest.ref.o -c asmtest.S
	objdump -D asmtest.ref.o > asmtest.ref

ifeq ($(ARCH),arm)
asmtest asmtest2:
	TCC="${TCC}" ./arm-asm-testsuite.sh
else
asmtest asmtest2: asmtest.ref
	@echo ------------ $@ ------------
	$(TCC) $(MAYBE_RUN_TCC) -c asmtest.S
	objdump -D asmtest.o > asmtest.out
	@if diff -u --ignore-matching-lines="file format" asmtest.ref asmtest.out ; then echo "ASM Auto Test OK"; fi
endif

# test assembler with tcc compiled by itself
asmtest2: MAYBE_RUN_TCC = $(RUN_TCC)

# Check that code generated by libtcc is binary compatible with
# that generated by CC
abitest-cc.exe: abitest.c $(LIBTCC)
	$(CC) -o $@ $^ $(CFLAGS) $(LIBS) -w

abitest-tcc.exe: abitest.c libtcc.c
	$(TCC) -o $@ $^ $(NATIVE_DEFINES) $(LIBS)

abitest-% : abitest-%.exe
	@echo ------------ $@ ------------
	./$< $(TCCFLAGS)

abitest: abitest-cc
ifneq ($(CONFIG_arm_eabi),yes) # not ARM soft-float
abitest: abitest-tcc
endif

vla_test$(EXESUF): vla_test.c
	$(TCC) -o $@ $^

vla_test-run: vla_test$(EXESUF)
	@echo ------------ $@ ------------
	./vla_test$(EXESUF)

asm-c-connect$(EXESUF): asm-c-connect-1.c asm-c-connect-2.c
	$(TCC) -o $@ $^

asm-c-connect-%.o: asm-c-connect-%.c
	$(TCC) -c -o $@ $<

asm-c-connect-sep$(EXESUF): asm-c-connect-1.o asm-c-connect-2.o
	$(TCC) -o $@ $^

asm-c-connect-test: asm-c-connect$(EXESUF) asm-c-connect-sep$(EXESUF)
	@echo ------------ $@ ------------
	./asm-c-connect$(EXESUF) > asm-c-connect.out1 && cat asm-c-connect.out1
	./asm-c-connect-sep$(EXESUF) > asm-c-connect.out2 && cat asm-c-connect.out2
	@diff -u asm-c-connect.out1 asm-c-connect.out2 || (echo "error"; exit 1)

# quick sanity check for cross-compilers
cross-test : tcctest.c examples/ex3.c
	@echo ------------ $@ ------------
	$(foreach T,$(CROSS-TGTS),$(call CROSS-COMPILE,$T))

CROSS-TGTS = \
 i386 \
 i386-win32 \
 i386-OpenBSD \
 x86_64 \
 x86_64-win32 \
 x86_64-osx \
 x86_64-FreeBSD \
 x86_64-NetBSD \
 x86_64-OpenBSD \
 arm-fpa \
 arm-eabihf \
 arm-NetBSD \
 arm-wince \
 arm64 \
 arm64-FreeBSD \
 arm64-NetBSD \
 arm64-OpenBSD \
 riscv64 \
 c67

define CROSS-COMPILE
 @echo " . $(1)"
 $(TCC) $(DEF-$1) -DTCC_CROSS_TEST -run $(TOPSRC)/tcc.c \
    -c $(if $(findstring c67,$1),$(filter %/ex3.c,$^),$<) -w $(TCCFLAGS)

endef

# targets for development
%.bin: %.c tcc
	$(TCC) -g -o $@ $<
	$(DISAS) $@

instr: instr.o
	objdump -d instr.o

instr.o: instr.S
	$(CC) -o $@ -c $< -O2 -Wall -g

cache: tcc_g
	cachegrind ./tcc_g -o /tmp/linpack -lm bench/linpack.c
	vg_annotate tcc.c > /tmp/linpack.cache.log

# clean
clean:
	rm -f *~ *.o *.a *.bin *.i *.ref *.out *.out? *.out?b *.cc *.gcc
	rm -f *-cc *-gcc *-tcc *.exe hello libtcc_test vla_test tcctest[1234]
	rm -f asm-c-connect$(EXESUF) asm-c-connect-sep$(EXESUF)
	rm -f ex? tcc_g weaktest.*.txt *.def *.pdb *.obj libtcc_test_mt
	@$(MAKE) -C tests2 $@
	@$(MAKE) -C pp $@

