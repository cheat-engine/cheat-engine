ASM = yasm
CC = gcc -m32 -fno-stack-protector -o0 -g
LD = ld -melf_i386
AFLAGS = -gDWARF2 -f elf32
LDFLAGS = -T kernel.lnk
EXTRA_INC = -I ../common

all:
	@${ASM} ${AFLAGS} vmloader.asm -o vmloader.o -DVMMSIZE=`stat -c%s '../vmm.bin'` -DSERIALPORT=${SERIALPORT}
	@${CC} ${EXTRA_INC} vmloaderc.c  -c -o vmloaderc.o -DSERIALPORT=${SERIALPORT} -DVMMSIZE=`stat -c%s '../vmm.bin'`
	@${LD} ${LDFLAGS} vmloader.o vmloaderc.o ../common/common.o ../common/commona.o -o ../vmloader.elf -Map ../vmloader.map
	@objcopy -O binary ../vmloader.elf ../vmloader.bin
	@echo "vmloader created"	
	
clean:
	@rm -f *.o
	@rm -f *.bin
	@rm -f *.map
	@rm -f *.*~
	@rm -f *~
	@echo "vmloader cleaned"	
