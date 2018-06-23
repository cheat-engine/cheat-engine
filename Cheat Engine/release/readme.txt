Cheat Engine 6.8.1

Fixes:
Fixed several issues with the structure compare
Fixed the commonality scanner from picking up unrelated registers for comparison
Fixed speedhack hotkeys
Fixed ultimap 1
Fixed a bunch of random access violations
Fixed Lua dissectCode.getStringReferences now also returns the string
Fixed Lua breakpoints that specify a specific function
Fixed Lua toAddress when the 2nd parameter is an address
Fixed assembling xmm,m32
Fixed issue when disassembling AVX instructions
Fixed rightclicking r8-r9 in the registers window
Fixed the plugin system for DBVM
Fixed DBVM memory allocations when smaller than 4KB


Additions and changes:
Added translation strings for the all type settings
You can now drop files into the auto assembler
auto assembler commands allocnx (allocate no execute) and allocxo (allocate execute only)
The memoryview windows's hexadecimalview now shows the allocationbase as well, and can be doubleclicked to go there
Added support for mono dll's that do not export g_free
Improved DBVM speed slightly


lua:
added RemoteThread class object




How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
