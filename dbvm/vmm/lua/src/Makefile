ASM = yasm
LD = ld
CC = gcc
#CPATH = ./include
AFLAGS = -gDWARF2 -f elf64 -DSERIALPORT=${SERIALPORT} -DDISPLAYDEBUG=${DISPLAYDEBUG} #-DJTAG
CFLAGS = -O0 -W -Wall  -DSUPPORT_64BIT_OFFSET -DDARKBYTESFUCKEDUPCROSSCOMPILER -fno-builtin-floor -fno-builtin-ceil -fno-builtin-strspn -fno-builtin-memcpy -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-malloc -fno-builtin-snprintf -fno-builtin-memset -fno-stack-protector -Wimplicit-fallthrough=0 -g
LDFLAGS = -static -T lua.lnk -Map lua.map 
LDFLAGS2 = -Map lua.map
C2FLAGS = -Wconversion
#

all:
	@${CC} ${CFLAGS} lapi.c -c -o ../lapi.o
	@${CC} ${CFLAGS} lobject.c -c -o ../lobject.o
	@${CC} ${CFLAGS} ldo.c -c -o ../ldo.o
	@${CC} ${CFLAGS} lgc.c -c -o ../lgc.o
	@${CC} ${CFLAGS} lstate.c -c -o ../lstate.o
	@${CC} ${CFLAGS} ldebug.c -c -o ../ldebug.o
	@${CC} ${CFLAGS} ltm.c -c -o ../ltm.o
	@${CC} ${CFLAGS} lvm.c -c -o ../lvm.o
	@${CC} ${CFLAGS} ltable.c -c -o ../ltable.o
	@${CC} ${CFLAGS} lstring.c -c -o ../lstring.o	
	@${CC} ${CFLAGS} lfunc.c -c -o ../lfunc.o
	@${CC} ${CFLAGS} lzio.c -c -o ../lzio.o
	@${CC} ${CFLAGS} ldump.c -c -o ../ldump.o
	@${CC} ${CFLAGS} lmem.c -c -o ../lmem.o
	@${CC} ${CFLAGS} lundump.c -c -o ../lundump.o
	@${CC} ${CFLAGS} lparser.c -c -o ../lparser.o
	@${CC} ${CFLAGS} llex.c -c -o ../llex.o
	@${CC} ${CFLAGS} lopcodes.c -c -o ../lopcodes.o
	@${CC} ${CFLAGS} lcode.c -c -o ../lcode.o
	@${CC} ${CFLAGS} lctype.c -c -o ../lctype.o
	@${CC} ${CFLAGS} lauxlib.c -c -o ../lauxlib.o
	
	@${CC} ${CFLAGS} lstrlib.c -c -o ../lstrlib.o
	@${CC} ${CFLAGS} linit.c -c -o ../linit.o
	@${CC} ${CFLAGS} lbaselib.c -c -o ../lbaselib.o	
		
	@echo "lua done"
	
clean:
	@rm -f ../*.o
	@rm -f *~
