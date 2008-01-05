Cheat Engine 5.4

Changes:
Rewrote the memoryscan engine to make better use of multiple cpu cores
Added an All valuetype
Added a Custom scan option, which allows auto assembler scripts to be used as internal compare routines
New Auto Assembler functions: Added CreateThread(address) , loadlibrary(filename) and readmem(address,size)
Added /* */ comments to the auto assembler
Added codehighlighting for the auto assembler
The assembler now allows you to type in normal decimal values instead of hex. Add a # in front. e.g #100
Goto history for disassembler and hexview
Follow and back instruction for the disassembler-view
Added a simple C-scripting language.
The trainer maker now lets buttons execute cheat entries
The address interpreter now understands pointer notations like [[00412345+123]+123]
When opening a process for the first time the current disassembleraddress will go to the entrypoint of the process. The hexview will go to the start of data.
Added the DF flag to the debugger
Implemented a reverse pointer scan. Instead of starting at the base address andscanning all memory, it scans for the final address and continues from the results there.
Added multiline selection to the disassembler. (Hold shift)
Added Ctrl+C in disassembler to copy to clipboard.
Added ability to show multiple memoryview windows at a time. (multiple displays recommended)
Implemented a hypervisor that CE can make use of. Look up DBVM on the website. Intel VT only for now
Added an option to save the memory when a pointerscan has finished so it can be reused again


Fixes:
Several VISTA-32 bit fixes.
fixed some asembler/disassembler instructions
The trainer maker now supports scripts with alloc in them
hotkeys now work better and don't execute without pressing any key, or when pressing an unrelated key
Fixed a dpi bug for the trainer maker "Add cheat" window
Fixed missing confirmation screen when closing and a script was edited.
Fixed pressing C in the hexview would stop the editing.
Fixed alignment problem of the "at least %" string when increased or decreased scan is selected
Fixed example c-plugin where the define of changeregonbp was missing some padding
Fixed the processwatcher for vista 32-bit (don't even try vista 64, will not work)
Fixed ce detecting vista as an older version instead of newer.
Now when selecting a new process ticked assemblerscripts go to disabled state.
Type change window now closes when pressing escape.
Fixed cleanup problem when the red X was used causing group info to stay behind on new entries.
Fixed typing in values with the numlock keypad
Hexview editbox now closes when rightlicking on the hexview.
The autoassembler now handles spaces and tabs better
Added some extra checks to the processname doubleclick when kernelmode has been enabled to prevent a BSOD


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