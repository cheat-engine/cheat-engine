ASM = yasm
CC = gcc -m32
CFLAGS = -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-strcpy -fno-builtin-malloc -fno-builtin-snprintf -fno-stack-protector -g
AFLAGS = -f elf32 -gDWARF2

all:
	echo ${SERIALPORT}
	@${ASM} ${AFLAGS} commona.asm -o commona.o -DSERIALPORT=${SERIALPORT}
	@${CC} common.c -fno-stack-protector -g -c -o common.o -DSERIALPORT=${SERIALPORT}
	@echo "common created"

clean:
	@rm -f *.o
	@rm -f *~
	@echo "common cleaned"
