Cheat Engine 6.4

Fixes:
Network: Network server can now handle multiple incomming connections at the same time
Gui: Fixed a crash when using multiple scan tabs
Assembler/Disassembler: Fixed several assembler/disassembler bugs
Debug: Fixed issues where deleting a breakpoint wouldn't actually remove it, causing a crash
Debug: Fixed a problem where deleting a breakpoint that was marked for deletion would never happen if the game was constantly triggering the debugger
Lua: Fixed the 6.2 and earlier version of opendialog_execute
Lua: Fixed memscan.waitTillDone() when using it on the gui memscan
Lua: Fixed speedhack_setSpeed() not taking more than 3 digits
D3D Hook: Direct3D9 objects now support transparency
D3D Hook: Fix detection of which directx version is actually used for rendering
Auto Assembler: Fixed some commands not highlighting properly
Ultimap: Fixed ultimap so it now works in windows 8
Ultimap: Fixed the hotkeys
Ultimap: Fixed the hint popup for pre-emptive flushing
Symbols: Fixed a problem where 32-bit modules where detected as 64-bit
Memory Scan: Fixed next scan causing a buffer overflow in some rare situations
Form Designer: Fixed a problem where deleting a non visible object failed
PE-Info: Fixed a possibility where a bad PE header could cause an read error
Memory view: Hexview: Fix 8-byte value editing



Additions and changes:
Address List: Added a group option that shows a +/- sign in front of group entries
Address List: Pressing enter on a single entry now goes into value edit mode
Address List: Added an option so entries in the address list show a groupbox the user can pick from
Auto Assembler: New auto assembler templates that focus on Array of Byte scans(thanks to jgoemat)
Auto Assembler: The auto assembler can now handle {$LUA} and {$ASM} preprocessors for multiline lua scripts
Break And Trace: Added a donottrace.txt file in the base directory which holds a list of modules that should not be traced but stepped over instead
Pointerscan: Improved performance of the pointer scanner
Pointerscan: The pointerscan now has the option to generate a lot smaller .PTR files
Pointerscan: Added the ability to do a distributed pointerscan and pointer rescan
ProcessList: You can now type in the processlist to filter for the specific process
Network: Added a basic ARM assembler/disasembler
Network: The linux/android network version can now use basic debugging (find what access/writes)
Network: Added speedhack to the network version
Network: The network version now compresses read/write process memory before sending to the client. The compression level can be changed at runtime
Network: Added module injection for linux/android
Symbols: Added better support for .PDB debug files so parameters and local variable references show when that data is available
Symbols: Added support for .Net
Symbols: Added support for Java (proof-of-concept showing off the extendabilty of CE)
Symbols: Added support for Mono (^)
Memory view: Hexview: Added decimal display modes for the other types
Stack View: Added a search option
D3D Hook: you can now reattach the D3D hook to a process that previously had been hooked
Lua engine window: Added a search and replace option to the editor
Lua engine window: Added the ability to set breakpoints, inspect variables and step over lines
Trainer Generator: Replaced the beepOnAction with playSoundOnAction and added 2 build in activate/deativate sounds. (You can override them)
Trainer Generator: The XM file field now has a play button
Lua/Trainer Generator/Designer: Added several new components , propertes and methods.
Lua: Added a dll search path to cedir\clibs32 or cedir\clibs64 depending on which cheat engine version is used. Use it for lua extentions
Lua: Made it more forgivable about method and property names
Lua: Added some threading helper functions
Lua: You can now override the disassembler/assembler
Lua: Lots of other new features. Check out main.lua



How to use:
Theres a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
