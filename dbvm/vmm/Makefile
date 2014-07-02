ASM = yasm
LD = ld
CC = gcc
AFLAGS = -f elf64 -DSERIALPORT=${SERIALPORT}
CFLAGS = -O0 -W -Wall -DSERIALPORT=${SERIALPORT} -DSUPPORT_64BIT_OFFSET -DDARKBYTESFUCKEDUPCROSSCOMPILER -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-strcpy -fno-builtin-malloc -fno-builtin-memcpy -fno-builtin-snprintf -fno-builtin-memset -fno-stack-protector -g
LDFLAGS = -T vmm.lnk -Map vmm.map
LDFLAGS2 = -Map vmm.map
C2FLAGS = -Wconversion

all:
	@${ASM} ${AFLAGS} vmma.asm -o vmma.o 
	@${ASM} ${AFLAGS} apica.asm -o apica.o
	@${ASM} ${AFLAGS} commona.asm -o commona.o
	@${ASM} ${AFLAGS} testa.asm -o testa.o
	@${CC} ${CFLAGS} main.c -c -o main.o
	@${CC} ${CFLAGS} neward.c -c -o neward.o
	@${CC} ${CFLAGS} vmmhelper.c -c -o vmmhelper.o
	@${CC} ${CFLAGS} multicore.c -c -o multicore.o
#	@${CC} ${CFLAGS} vmmemu.c -O3 -c -o vmmemu.o #obsolete
	@${CC} ${CFLAGS} mm.c -c -o mm.o
	@${CC} ${CFLAGS} vmpaging.c -c -o vmpaging.o
	@${CC} ${CFLAGS} vmcall.c -c -o vmcall.o	
	@${CC} ${CFLAGS} vmevent_invalidstate.c -c -o vmevent_invalidstate.o		
	@${CC} ${CFLAGS} vmeventhandler.c -c -o vmeventhandler.o	
	@${CC} ${CFLAGS} realmodeemu.c -c -o realmodeemu.o	
	@${CC} ${CFLAGS} pmodeemu.c -c -o pmodeemu.o	
	@${CC} ${CFLAGS} common.c -c -o common.o
	@${CC} ${CFLAGS} keyboard.c -c -o keyboard.o
	@${CC} ${CFLAGS} osspecific.c -c -o osspecific.o
	@${CC} ${CFLAGS} pci.c -c -o pci.o
	@${CC} ${CFLAGS} apic.c -c -o apic.o
	@${CC} ${CFLAGS} offloados.c -c -o offloados.o
	@${CC} ${CFLAGS} ultimap.c -c -o ultimap.o
	@${CC} ${CFLAGS} test.c -c -o test.o
	@${CC} ${CFLAGS} vmeventhandler_amd.c -c -o vmeventhandler_amd.o

	@make -C distorm64 all
	
	@${LD} ${LDFLAGS} vmma.o main.o neward.o common.o commona.o apica.o vmmhelper.o multicore.o mm.o vmpaging.o vmevent_invalidstate.o vmeventhandler.o realmodeemu.o pmodeemu.o vmcall.o keyboard.o osspecific.o offloados.o ultimap.o pci.o apic.o test.o testa.o vmeventhandler_amd.o ./distorm64/wstring.o ./distorm64/x86defs.o ./distorm64/textdefs.o ./distorm64/insts.o ./distorm64/prefix.o ./distorm64/instructions.o ./distorm64/decoder.o ./distorm64/operands.o ./distorm64/distorm.o -o ../vmm.elf -Map ../vmm.map
#	@${LD} ${LDFLAGS2} vmma.o main.o neward.o common.o commona.o apica.o vmmhelper.o multicore.o mm.o vmpaging.o vmevent_invalidstate.o vmeventhandler.o realmodeemu.o pmodeemu.o vmcall.o keyboard.o osspecific.o offloados.o ultimap.o pci.o apic.o test.o testa.o vmeventhandler_amd.o ./distorm64/wstring.o ./distorm64/x86defs.o ./distorm64/textdefs.o ./distorm64/insts.o ./distorm64/prefix.o ./distorm64/instructions.o ./distorm64/decoder.o ./distorm64/operands.o ./distorm64/distorm.o -o ../vmm.elf -Map ../vmm.map
	@objcopy -O binary ../vmm.elf ../vmm.bin
	@echo "vmm done"
	
clean:
	@make -C distorm64 clean
	@rm -f *.o
	@rm -f *.bin
	@rm -f *.map
	@rm -f *~
