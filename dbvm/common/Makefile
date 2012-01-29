ASM = yasm
CC = gcc -m32
CFLAGS = -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-strcpy -fno-builtin-malloc -fno-builtin-snprintf
AFLAGS = -f elf32

all:
	echo ${SERIALPORT}
	@${ASM} ${AFLAGS} commona.asm -o commona.o -DSERIALPORT=${SERIALPORT}
	@${CC} common.c -c -o common.o -DSERIALPORT=${SERIALPORT}
	@echo "common created"

clean:
	@rm -f *.o
	@rm -f *~
	@echo "common cleaned"
