#SERIALPORT is the port to communicate with the debugger, usually 0x3f8, on db's system it's 0xef00
SERIALPORT=0 #release/no serialport build
#SERIALPORT=0x3f8 #bochs and jtagged gigabyte test system
#SERIALPORT=0xbf00 #intel
#SERIALPORT=0xec00 #amd
#SERIALPORT=0xd010 #16 core test system
#SERIALPORT=0xf010 #32 core test system
#SERIALPORT=0x2f8 #vmware test

 
#DISPLAYDEBUG if DISPLAYDEBUG==1 it means that all serial output will be sent to the screen instead (useful for cases where dbvm won't load AT ALL, not so much when it does)
DISPLAYDEBUG=0
#SERIALPORT=0xf010 #32 core test system
 
all:
#	make -C vmm all SERIALPORT=${SERIALPORT} DISPLAYDEBUG=${DISPLAYDEBUG}
	make -C vmm -f mkx all SERIALPORT=${SERIALPORT} DISPLAYDEBUG=${DISPLAYDEBUG}
	make -C common all SERIALPORT=${SERIALPORT}
	make -C parsevmmmap all SERIALPORT=${SERIALPORT}
	parsevmmmap/parsevmmmap	
	make -C vmloader all SERIALPORT=${SERIALPORT}
	make -C parsevmloadermap all SERIALPORT=${SERIALPORT}
	parsevmloadermap/parsevmloadermap
	make -C bootsector all SERIALPORT=${SERIALPORT}
	make -C imagemaker all SERIALPORT=${SERIALPORT}

install: all
	imagemaker/imagemaker
	chmod 777 vmdisk.img
	chmod 777 vmdisk144.img
	sync
	
displaydebug: DISPLAYDEBUG=1
displaydebug: clean install
	
usb: install
	@if [ -b /dev/sdb ] ;\
	then \
		dd if=vmdisk.img of=/dev/sdb ;\
		sync ;\
	else \
		echo "/dev/sde does not exist. Did you just run this without understanding makefiles? This could have really fucked you up" ;\
		exit 1 ;\
	fi
	
efiusb: install
	cp vmdisk.img /media/eric/EC9B-5AFC/
	sync

qemu: install
	cp vmdisk.img /mntx
	sync
	@echo "copied files to /mntx"
		
disk: install
	dd if=vmdisk.img of=/dev/fd0
	sync

net: install
	-rm -f /mnt/cebin/vmdisk.img
	cp vmdisk.img /mnt/cebin
	sync

cdimage: install
	-mkdir iso
	-mkdir iso/boot  
	cp vmdisk144.img iso/boot
	cd iso
	mkisofs -r -b boot/vmdisk144.img -c boot/boot.catalog -o vmcd.iso iso/
	

	

release: clean install cdimage
	-rm -rf release	
	mkdir release	
	cp vmcd.iso release
	cp vmdisk.img release
	cp docs/readme.txt release
	cp vmm/docs/vmcall.txt release
	rar a dbvm.rar release/*
	@echo "Your release file is ready"

clean:
	@make -C imagemaker clean
	@make -C parsevmloadermap clean
	@make -C parsevmmmap clean
	@make -C bootsector clean
	@make -C vmloader clean
	@make -C vmm clean
	@make -C common clean
	@-rm -f *.bin
	@-rm -f *.img
	@-rm -f *.map
	@-rm -f *~
	@-rm -rf iso
	@-rm -rf release
	@-rm -f vmcd.iso
	@-rm -f dbvm.rar
	@-rm -f *.elf
	