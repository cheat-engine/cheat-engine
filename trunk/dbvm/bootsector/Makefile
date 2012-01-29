ASM = nasm
LD = ld
AFLAGS = -f elf
LDFLAGS = -T bootloader.lnk -melf_i386


all:
	@${ASM} ${AFLAGS} bootloader.asm -o bootloader.o -Dsize=`stat -c%s '../vmloader.bin'`
	@${LD} ${LDFLAGS} bootloader.o -o ../bootloader.bin
	@echo "bootloader created"

clean:
	@rm -f *.o
	@rm -f *.*~
	@rm -f bootloader.bin
	@echo "bootloader cleaned"
