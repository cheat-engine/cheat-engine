Cheat Engine 5.6

Changes:
Pointerscanner speed has been increased a lot
New Icon (thanks to Phox from the forum)
The pointerscanner can now scan for values
The pointerscanner now lets you specify an offset list that it has to end with.
Removed the injected pointerscanner
The auto assembler now supports code outside of [enable] and [disable] sections so it affects both
Resultcount is now comma seperator (thanks to infinito)
New kernelmode debugger
Added the ability to offload the current OS to dbvm (if your cpu supports it)
The driver is now 64-bit compatible. (You will have to sign it yourself, or reboot with unsigned driver support)
Rewrote the disassemblerview
Deleting addresses from a scanresult is now a bit faster
Changed the hotkey handler to be more controllable
New heaplist that works based on dll injection instead of toolhelp32
Dissectcode now helps finding referenced strings
Added a new Auto assembler command "aobscan(varname, arrayofbytestring)"
Added a new Auto assembler command "assert(address, arrayofbyte)" which will make an script fail if the bytes aren't what they should be
Dissect data now works with offsets instead of sizes
Added the ability to follow pointers easily with Dissect Data
There's now a stacktrace visible during debugging
The registerview is gone as long as you're not debugging
CE now suppresses the "No disk" message when the searchpath is invalid
Added a common modulelist to the ce folder that you can edit. Include files that you do NOT want to go through when doing memory inspection
You can now open another pointerscanner window while another pointerscan is running and read the results.
Added a string reference windows
Improved the plugin system with some extra disassembler commands and a command to load modules into memory
Added (float)#, (double)# and (int)# support to the assembler, (double) is mainly usable in combination with the new DQ command though


Fixes:
Fix mov [reg],reg disassembly when a 16-bit prefix is used
Fixed some floating point assembler instructions
Taborder fix for "Value between scan" (infinito)
Fixed the Auto assembler code injection template with regard to the "Alt:" line
Jmp FAR instruction not shown properly in the disassembler
Fixed disassembler instructions that had a rep/repe prefix while they shouldn't
Fixed xorps instruction
Fixed assembler where segment registers are used
Fixed rm32,imm16 notations getting dowsized to rm32,imm8 while they should go rm32,imm32 
Fixed hang when setting the window on top and then doing a scan
Fixed FILD qword instruction
Fixed FNSTSW AX instruction
Fixed FCOM instruction
Fixed IMUL,0a instruction
Fixed broken alt-key when the disassemblerview is focused
Fixed the bug where removing a assigned hotkey to a cheat table didn't work
Fixed the floating point panel. It now actually shows the floating point values...
Fixed several bugs in the Dissect Data window
Several gui fixes for high dpi systems



How to use:
Theres a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
or
http://www.heijnen1.demon.nl/