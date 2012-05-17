Cheat Engine 6.2

Fixes:
Fixed the first plugin from not getting activated at restart
Custom types can now do an unknown initial value scan
Fixed the auto assembler highlighter from hiding some text while typing
Fixed the auto assembler highlighter from not showing hexadecimal values as hex starting with A to F
Fixed global debug from crashing in 64-bit (with dbvm)
Fixed dbvm from not working on several systems (freeze)
Fixed notification when closing and you had some changes
Fixed the rescan of the structure spider not working at all
Fixed several disassembler and assembler instructions
Fixed several plugin-system related bugs
Fixed aobscan for 64-bit
Fixed displaying the registers in 64-bit when using "find what xxx this address"
Fixed the stackview when single stepping through the code
Fixed several lua function, including createhotkey
Fixed the assembler not dealing well with names with a - in it. (like the tutorial)




Additions and changes:
Added a profiler so you can find function entry points and see how often they are called
Deleting a plugin now calls FreeLibrary on it
Ctrl+enter in the luaengine form now automatically executes the command
Added direct 3d hook functions (can be used to show trainers and menu's inside games)
Plugins settings between the 32 and 64-bit executable are now seperated
Recalculate addresses with only one selection now only updates the siblings and children. Doesn't touch the parent node
Addresslist entries can have the notation +xxx and -xxx, which will calculate the address based on the parent address (If the parent address changes, these change automatically
The structure dissect has been rewritten from scratch, and the functions have been exposed to lua as well
Added a new step to the tutorial (step9) showing how to deal with shared code
Made the tutorial translatable as well
Added a new trainer type which generates trainer that are a lot smaller in size (tiny)
Added a groupscan type which you can use to scan for different things in the same block
If you boot up with dbvm you do not need to sign the driver in 64-bit to load it
Custom types now support handling as a floating point value
Added debug register states to the threadview and made the threadview window capable of changing registers
Added remote scanning with ce for different systems. The server just needs to be running, and a 1GBPs network connection or faster is recommended
Changed the add/change address window to be more compact and make dealing with offsets easier
Lots of new lua functions


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