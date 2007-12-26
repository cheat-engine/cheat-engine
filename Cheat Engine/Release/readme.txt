Cheat Engine 5.3

Changes:
Made multiple bytes selectable in the hexview 
Save userdefined symbols added by the user, to the table
Added ctrl+A key to select all text in the auto assembler 
Added recalculate base address in structure definer (only for level1) 
Added scroll support to main cheat list 
Added plugin dll's to several sections (for those that are too lame to send me a dll to add to the ce package) 
Added a special window for pasting cheat entries which includes easy modifying the offset and description 
Added information popup when a scan finds more than the max ammount of addresses the first time 
Added edit hotkey (ctrl+e) using the same window as paste 
Implemented use of kernel VirtualAllocEx 
Integrated pointer scan in main 
Rewrote pointerscan, it's faster and more accurate now
Increased speed of next scan routines
implemented a lower level openprocess instead of calling ZwOpenProcess
Added search for assembler option
Added symbol handling including kernel symbols
Added scan option: "Same as first scan" 
Added exact value hotkey scans 
Added a driverlist 
Added step 7 to the tutorial: code injection 
Added step 8 to the tutorial: multilevel pointers
Added quit comments on the tutorial
Added ability to set symbol searchpath 
Added 3 new functions to the auto assembler: define, loadbinary and include
Driver now uses a driver.dat file to configure itself (no problem if it's missing)


Fixes:
Fixed the known "array of byte" scan bugs
Fixed changed/unchanged+fastscan+4 byte+hyperscan not filtering right 
Fixed enable/disable cheat menu item enabling/disabling all 
Fixed "change element" in structure definer and bytesize of strings 
Fixed window title for the api hook template 
Fixed copy/paste bug 
Fixed newscan turning of the hex checkbox without converting 
Fixed the change record menu item 
Fixed next scan exact value for double scans
Fixed fs: not showing up for some disassembler instructions
Fixed paste bug in 2nd between textfield 
Fixed "je" (and probable other familiars as well) when the positive jump is bigger than 0x7f (should then do a long jump, not short jump)
Fixed hotkey set value not always working. 
Fixed bug where 2 byte entries don't support increase and decrease allowed freezing.
Fixed string freezing bug
Fixed freezing addresses showing up as normal when hexadecimal was set 


How to use:
Theres a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com
ICQ = 6330306


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
or
http://cheatengine.tk/
or
http://www.heijnen1.demon.nl/