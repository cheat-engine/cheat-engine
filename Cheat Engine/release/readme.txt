Cheat Engine 6.5

Fixes:
Fixed page exception breakpoints from not working  
Fixed the save as button in the lua script assigned to the table
Fixed the dotnetdatacollector from not fetching parent fields
Fixed disassembling of some instructions
Fixed assembling some instructions
Fixed assembling instructions that referenced address 80000000 to ffffffff in 64-bit targets
Fixed dealing with unexpected breakpoints
Fixed several issues with the network scanner. (symbols, scanspeed, threads, etc...)
Fixed "going to" 64-bit registers.
Fixed pointerstrings for 64-bit
Fixed the addressparser in memview's hexview not handing static 64-bit addresses 
Fixed r8 and r9 looking broken in the memoryview window
Fixed hotkeys that set a value as hexadecimal and the value is smaller than 0x10
Fixed multiline string editing for memory records
Fixed dragging cheat tables into CE
Fixed VEH debug for 'Modern' apps
Fixed several translation issues

lua:
 fixed getStructureCount, writeRegionToFile, readRegionFromFile, readInteger, ListColum.GetCount
 fixed memoryleak in MemoryStream


Several fixes to DBVM:
 added support for Windows 10
 support for more than 8 cpu's
 support for newer cpu's
 fixed issue where calling CPUID right after setting the TF flag wouldn't trigger a breakpoint after it



Additions and changes:
Array of Byte's can now deal with nibble's.  (e.g: 9* *0 90 is now a valid input- and scanstring)
The auto assembler can now deal with some mistakes like forgetting to declare a label
Added support to use binutils as assembler and disassembler, and a special scripting language for it
Added support for 64-bit mono, and script support for cases where mono.dll isn't called mono.dll
Added an option to get a list of all recently accessed memory regions. This is useful for the pointerscanner
The pointerscanner can now use multiple snapshots (pointermaps) to do a scan. This basically lets you do a rescan during the first scan, saving your harddisk 
Made the pointerscan network scanner a bit easier to use. You can now join and leave a pointerscan session
You can now stop pointerscans and resume them at a later time
Pointerscan files can get converted to and from sqlite database files
The pointerscan configuration window now has an advanced and basic mode display
The all type now has a setting that lets you define what under "all" falls
Custom types now also have access to the address they're being used on
Split up the "(de)activating this (de)activates children" into two seperate options (one for activate, one for deactivate)
Added some basic Thumb disassembling
The xmplayer has been replaced with mikmod which supports many different module types (in lua you still call it xmplayer)
Rightlicking on "your system supports dbvm" will let you manually load DBVM for each cpu. This is usefull if for some reason your system crashes when it's done too quickly
In "Find what addresses this instruction accesses" you can now open the structure dissect window of your choice in case there are others. It will also fill in the base address, so no need to recalculate yourself
AA command GlobalAlloc now has an optional 3th parameter that lets you specify the prefered region
Added an option to record and undo writes. (Off by default, can be enabled in settings.  Memview ctrl+z will undo the last edit)
Added aobscanregion(name,startaddress,stopaddress,aob)

lua:
  switched from Lua 5.1 to 5.3    
  debug_setBreakpoint can now take an OnBreakpoint parameter that lets you set a specific function just for that breakpoint
  added dbk_getPhysicalAddress(int)
  added dbk_writesIgnoreWriteProtection(bool)
  added getWindowList()
  And a bunch of other lua functions. (check out main.lua)


How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
