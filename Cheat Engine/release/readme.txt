Cheat Engine 6.3

Fixes:
Fixed dll injection for 64-bit targets (also fixes speedhack for 64-bit)
Fixed speedhack thread safety so changing speed in a program that constantly checks speed won't cause a crash/weird behaviour
Fixed Lua speedhack_setSpeed being limited to 2 digit accuracy
Customtypes can now deal with huge size types (4096 bytes and bigger)
Some table merging bugs
Fixed negative values in groupscans
Fixed a lot of assembler and disassembler instructions
Fixed GenericHotkey in lua
Fixed the table version of writeBytes in lua
Fixed the bug where if you opened the settings window and click ok you wouldn't be able to debug anymore
Fixed unlabed labels
Fixed crash when clicking stop when using the debugger to find something
Fixed where CE would select invisible entries when multiselecting and press space
Loading a table now deletes tables you might have previously defined
The autoassembler can now handle $luavar when it's an integer instead of string
Fixed break on entry when creating a process
Fixed the stackview in 64-bit ce when targeting a 32-bit program
Fixed unloading the driver when global debug was used before
Fixed the symbolpath not changing to what you wish, and add the game's exe to the symbol path search by default
Fixed dbvm stability
Fixed global debug not handling 64-bit mov dr* instructions properly

Additions and changes:
Redesigned the lua class system
Added mouse4 and mouse5 to the lua defines
Added the THREADSTACK# symbol which points to the stack start of the specific thread number (pointerscan can use it)
The pointerscan has several new features to decrease time and increase useful results
Added sorting the pointerscan by column (Tip: After a sort close the pointerscan and delete .ptr files you do not wish)
Changed hotkey handling internally
The different display types in the hexadecimal view of memoryview now support direct editing as well
The foundlist can now display using a different display type, on the condition that the type has a compatibly bytesize
The foundlist now shows a "previous value" column and marks differences red
The symbolhandler now has a better distinction between 32 and 64-bit modules. Non-compatible modules (64-bit in 32-bit programs) will get an underscore in front of their symbolnames
Groupscans can now let you choose which elements to add to the addresslist when doubleclicked
Added a graphical memory view
Added a new breakpoint type :Exceptions (not dependant on size and no debug registers, but extremely slow to unplayable)
The "Find out what *** this address" function now has the ability to show if the given opcode is used for other data as well
Added a luaserver to ce that you can use to let a different/target process execute lua commands and pass data
The userdefined comments can now show handle multiple lines
Dissect code now lets you jump to a referal if you click the line
Added a few new lua methods to the disassembler so you can render your own data in front and after a disassembler line
Assembler: Added override support to relative jumps
Auto Assembler: AA command ReadMem can now work on large sets of data without being too slow
Auto Assembler: Scripts with multiple AOBScan commands will go faster now (grouped into one)
Auto Assembler: Added a new "AOBSCANMODULE" auto assemble command . Usage: AOBSCANMODULE(modulename, aob)
Auto Assembler: GlobalAlloc now doesn't allocate 4KB (64KB in reality) for each symbol but now groups them
Auto Assembler: Registersymbol now works with aobscan results
Auto Assembler: Add support for inscript structure definitions
Tracer: You can now save and load a trace
Addresslist: Changing a records' value (lua setValue) now supports lua statements if the new value is enclosed by brackets [  ]  (Example: [12-2] becomes 10, and [readInteger(0x00400500)+10] returns the value at 00400500 with 10 added to it)
D3D: Added the ability to dissect a whole d3d scene and get the stack at the moment a specific object is being rendered
D3D: Also works on 64-bit targets now
Symbolhandler: It now interprets "structurename.variablename" and returns the offset of variablename in the structure. This includes auto assembler
Binary files inside cheat tables are now stored using ascii85 instead of hexadecimal
Added a more complex disassembler class that gives more information about what it disassembled
Dissect data now also shows the effect of a locked column on childnodes
Dissect data can now have custom name under each address besides a groupname
Dissect data elements can now have a custom background color
The structure spider can now also work with locked memory (shadow memory)
Changed the way the vertical scrollbars of the disassembler and hexview panels work
The "find what acceses this address" window no longr prints out ALL results in the info box when multiselecting.
Changed the stackview panel so when it's visible and you resize the window it's on, it resizes instead of the hexview
The assembly scan can now have a custom range
Added an option to the trainerscript generator to generate a D3DTrainer (if the game supports it)
DBVM now works on AMD systems. Some featues like Ultimap are still Intel only, but int hooks work



How to use:
Theres a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
or
http://members.upc.nl/cheatengine/