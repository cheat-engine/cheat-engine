ASM = yasm
LD = ld
CC = gcc
AFLAGS = -gDWARF2 -f elf64 -DSERIALPORT=${SERIALPORT} -DDISPLAYDEBUG=${DISPLAYDEBUG} #-DJTAG
CFLAGS = -O0 -W -Wall -DSERIALPORT=${SERIALPORT} -DDISPLAYDEBUG=${DISPLAYDEBUG} -DSUPPORT_64BIT_OFFSET -DDARKBYTESFUCKEDUPCROSSCOMPILER -fno-builtin-floor -fno-builtin-ceil -fno-builtin-strspn -fno-builtin-memcpy -fno-builtin-strcpy -fno-builtin-strlen -fno-builtin-strcat -fno-builtin-malloc -fno-builtin-snprintf -fno-builtin-memset -fno-stack-protector  -g
LDFLAGS = -static -T vmm.lnk -Map vmm.map 
LDFLAGS2 = -Map vmm.map
C2FLAGS = -Wconversion
#

all:
	@${ASM} ${AFLAGS} vmma.asm -o vmma.o 
	@${ASM} ${AFLAGS} apica.asm -o apica.o
	@${ASM} ${AFLAGS} commona.asm -o commona.o
	@${ASM} ${AFLAGS} testa.asm -o testa.o
	@${ASM} ${AFLAGS} -w vbe3.asm -o vbe3a.o
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
	@${CC} ${CFLAGS} vmxemu.c -c -o vmxemu.o
	@${CC} ${CFLAGS} vmxsetup.c -c -o vmxsetup.o
	@${CC} ${CFLAGS} epthandler.c -c -o epthandler.o	
	@${CC} ${CFLAGS} -m32 psod32.c -c -o psod32.o.32
	objcopy "--input-target=elf32-i386" "--output-target=elf64-x86-64" psod32.o.32 psod32.o
	@${CC} ${CFLAGS} vbe3.c -c -o vbe3.o
	@${CC} ${CFLAGS} exports.c -c -o exports.o
	@${CC} ${CFLAGS} maps.c -c -o maps.o
	@${CC} ${CFLAGS} list.c -c -o list.o
	@${CC} ${CFLAGS} displaydebug.c -c -o displaydebug.o	
	
	@echo "part1"
ifneq ($(SERIALPORT), 0)	
	@${CC} ${CFLAGS} luahandler.c -c -o luahandler.o
endif

	@make -C distorm64 all

ifneq ($(SERIALPORT), 0)
	@make -C lua/src all
endif
	
ifeq ($(SERIALPORT),0)
	@${LD} ${LDFLAGS} vmma.o main.o neward.o common.o commona.o apica.o vmmhelper.o multicore.o mm.o vmpaging.o vmevent_invalidstate.o vmeventhandler.o realmodeemu.o pmodeemu.o vmcall.o keyboard.o osspecific.o offloados.o ultimap.o pci.o apic.o test.o testa.o vmeventhandler_amd.o vmxemu.o vmxsetup.o epthandler.o exports.o vbe3.o vbe3a.o psod32.o maps.o list.o displaydebug.o ./distorm64/wstring.o ./distorm64/x86defs.o ./distorm64/textdefs.o ./distorm64/insts.o ./distorm64/prefix.o ./distorm64/instructions.o ./distorm64/decoder.o ./distorm64/operands.o ./distorm64/distorm.o -o ../vmm.elf -Map ../vmm.map
else	
	@${LD} ${LDFLAGS} vmma.o main.o neward.o common.o commona.o apica.o vmmhelper.o multicore.o mm.o vmpaging.o vmevent_invalidstate.o vmeventhandler.o realmodeemu.o pmodeemu.o vmcall.o keyboard.o osspecific.o offloados.o ultimap.o pci.o apic.o test.o testa.o vmeventhandler_amd.o vmxemu.o vmxsetup.o epthandler.o exports.o luahandler.o vbe3.o vbe3a.o psod32.o maps.o list.o displaydebug.o ./distorm64/wstring.o ./distorm64/x86defs.o ./distorm64/textdefs.o ./distorm64/insts.o ./distorm64/prefix.o ./distorm64/instructions.o ./distorm64/decoder.o ./distorm64/operands.o ./distorm64/distorm.o ./lua/lapi.o ./lua/lobject.o ./lua/ldo.o ./lua/lgc.o ./lua/lstate.o ./lua/ldebug.o ./lua/ltm.o ./lua/lvm.o ./lua/ltable.o ./lua/lstring.o ./lua/lfunc.o ./lua/lzio.o ./lua/ldump.o ./lua/lmem.o ./lua/lundump.o ./lua/lparser.o ./lua/lopcodes.o ./lua/llex.o ./lua/lcode.o ./lua/lctype.o ./lua/lauxlib.o ./lua/lstrlib.o ./lua/linit.o ./lua/lbaselib.o -o ../vmm.elf -Map ../vmm.map
endif	
	@objcopy -O binary ../vmm.elf ../vmm.bin
	@echo "vmm done"
	
clean:
	@make -C distorm64 clean
	@rm -f *.o
	@rm -f *.bin
	@rm -f *.map
	@rm -f *~
