LD = ld
CC = gcc
CFLAGS = -O0 -DSUPPORT_64BIT_OFFSET -DDARKBYTESFUCKEDUPCROSSCOMPILER -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-strcpy -fno-builtin-malloc -fno-builtin-snprintf -fno-builtin-memcpy -fno-builtin-memset -fno-stack-protector -g
LDFLAGS = 

all:
	@${CC} ${CFLAGS} decoder.c -c -o decoder.o
	@${CC} ${CFLAGS} distorm.c -c -o distorm.o
	@${CC} ${CFLAGS} instructions.c -c -o instructions.o
	@${CC} ${CFLAGS} insts.c -c -o insts.o
	@${CC} ${CFLAGS} operands.c -c -o operands.o
	@${CC} ${CFLAGS} prefix.c -c -o prefix.o

	@${CC} ${CFLAGS} textdefs.c -c -o textdefs.o
	@${CC} ${CFLAGS} wstring.c -c -o wstring.o
	@${CC} ${CFLAGS} x86defs.c -c -o x86defs.o

	@echo "distorm done"
	
clean:
	@rm -f *.o
	@rm -f *.bin
	@rm -f *.map
	@rm -f *~
