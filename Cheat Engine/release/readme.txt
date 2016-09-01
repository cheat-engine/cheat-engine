Cheat Engine 6.5.1

Fixes:
Fixed increased value by/decreased value by for float values
Fixed disassembling/assembling some instructions (64-bit)
Fixed the autoassembler tokenizing wrong words
Fixed several bugs related to the structure dissect window (mainly shown when autodestroy was on)
Fixed a small saving issue
Groupscans now deal with alignment issues better
Fixed java support for 32-bit



Additions and changes:
Signed with a sha256 signature as well (for OS'es that support it)
Changed Ultimap to use an official way to get the perfmon interrupt instead of IDT hooking (less BSOD on win10)
Individual hotkeys can now play sounds
Now compiled with fpc 3.0/lazarus 1.6 (Previously 2.7/1.1)
You can now search in the string list
PEInfo now has a copy to clipboard
Some places can now deal better with mistakes
Lazarus .LFM files can now be loaded and saved


lua:
Fixed several incompatibilities between lua that popped up in 6.5 (due to the lua 5.1 to 5.3 change)
Fixed the OnSelectionChange callback property in the memoryview object
MemoryRecords now have an Collapsed property
Added TCanResizeEvent to the splitter
Fixed setBreakpoint not setting a proper trigger if not provided
Fixed executeCode* parameter passing
Fixed several memory leaks where unregistering hooks/addons didn't free the internal call object
Some tableFile additions
Fixed registerAssemble assembler commands 
Added kernelmode alloc and (un)mapping functionality
Added an easy way to add auto assembler templates
Added window related functions including sendMessage
Added Xbox360 controller support functions
Added more thread functions




How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
