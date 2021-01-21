#
# credits: 01..13.c from the pcc cpp-tests suite
#

TOP = ../..
include $(TOP)/Makefile
SRC = $(TOPSRC)/tests/pp
VPATH = $(SRC)

files = $(patsubst %.$1,%.test,$(notdir $(wildcard $(SRC)/*.$1)))
TESTS = $(call files,c) $(call files,S)

all test : $(sort $(TESTS))

DIFF_OPTS = -Nu -b -B

# Filter source directory in warnings/errors (out-of-tree builds)
FILTER = 2>&1 | sed 's,$(SRC)/,,g'

%.test: %.c %.expect
	@echo PPTest $* ...
	-@$(TCC) -E -P $< $(FILTER) >$*.output 2>&1 ; \
	    diff $(DIFF_OPTS) $(SRC)/$*.expect $*.output \
	    && rm -f $*.output

%.test: %.S %.expect
	@echo PPTest $* ...
	-@$(TCC) -E -P $< $(FILTER) >$*.output 2>&1 ; \
	    diff $(DIFF_OPTS) $(SRC)/$*.expect $*.output \
	    && rm -f $*.output

# automatically generate .expect files with gcc:
%.expect: # %.c
	gcc -E -P $*.[cS] >$*.expect 2>&1

# tell make not to delete
.PRECIOUS: %.expect

clean:
	rm -f *.output

02.test : DIFF_OPTS += -w
# 15.test : DIFF_OPTS += -I"^XXX:"

# diff options:
# -b ighore space changes
# -w ighore all whitespace
# -B ignore blank lines
# -I <RE> ignore lines matching RE
