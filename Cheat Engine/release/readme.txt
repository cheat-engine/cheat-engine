Cheat Engine 6.8.2

Fixes:
Disassembler: Several disassembler instructions had a comma too many or too few ,fixed those
Disassembler: Fixed the description for ret #
Disassembler/Debug: Fixed the address that is being edited when a breakpoint hits while editing an instruction
Assembler: Fixed assembling reg*2/4/8+unquotedsymbol
Plugin: Fixed the SDK for C plugins that use the disassembler callback
Hotkeys: Fixed the attach to foreground hotkey
Memory Scan: Fixed the percentage scan
Memory Scan: Fixed a rare situation that could cause an error
Memory Scan: Simple values now works with groupscan
Memory Scan Lua: Scanfiles now also get deleted if the memory scan object is freed before the scan is fully done
Fill Memory: Now allows 64-bit addresses
Structure Dissect: Fixed the popupmenu "change type" so it now affects all selected entries instead of just the first
PointerOrPointee window: Fix the debug pointer or pointee window button text when using access instead of writes
GUI: Fixed and restored the DPI Aware option in setting
GUI: Some DPI fixes/adjustments here and there
Graphical Memory view: Fixed DPI issues
Symbolhandler: When the symbolhandler now waits till it's done, it won't wait for the structures to be parsed anymore

Additions and changes:
Lua Engine: Added autocomplete
DLL injection: On DLL injection failure CE tries to fall back on forced injection methods
Assembler: Added multibyte NOP
Plugins: Plugins can now have side dll's that are statically linked in their own folder (Windows 7 with updates and later)
Debugging: Improved the FPU window editing when single stepping, allowing you to change the FPU registers
Debugging: Threadview now updates when single stepping and cnanges made there will affect the currently debugged thread (before it didn't)
Debugging: Added Code Filter. This lets you filter out code based on if it has been executed or not (Uses software breakpoints)
Debugging: Added an option to chose if you wish to break on unexpected breakpoints, and if CE should break on unexpected breakpoints, or only on specified regions (like AA scripts)
Disassembler: The comments now show multiple parameters
Pointerscan: Add option to allow negative offset scanning
Pointerscan: Add extra types to the display
Advanced Options/CodeList: Now uses symbolnames
Tutorial Game: Added a levelskip option when you've solved a step
Tutorial Game: Added a secondary test
Compare memory: Added a limit to the number of address values shown per row (can be changed)
Address List: When the option to deactivate children is set, the children will get deactivated first

lua:
ExecuteCodeLocal (Let's you execute code in the target and pass parameters)
Added 2 new parameters to getNameFromAddress (ModuleNames and Symbols)
Added addModule and deleteModule to the symbollist class
Added the ModuleLoader class which can force load dll's
Fixed endUpdate for the listview



How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
