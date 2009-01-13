Cheat Engine 5.5

Changes:
added 'short' and 'far' override to the jump instructions. (Mainly usefull for auto assembler scripts that by default pick the far one)
Copied the "Find out what addresses this code accesses" from advanced options to memory view
Made the above function display the current value so you don't have to add them first
And made it non-modal so you can do other stuff while it's working.
Improved custom scan so it can now also display the results
New speedhack implementation
New AutoAssembler window
Removed the question if you want a new scan. (You must now click on new scan yourself)
Added GlobalAlloc to the autoassembler
The dissect structure window can now be opened multiple times (for comparison)
Structures are now saved in the cheat table
The main ce window now has a menu (can be disabled in settings if you don't like it)
The processlist can now show process icons. (could be slow, so can be disabled in settings)
The settings window has been changed from tabs to a list
Added a tools menu so people can add quicklaunch apps. (e.g: calc)
Added the option to save and load tables as XML
Changed copy/pasting of entries to XML
Helpfile changed from .HLP to .CHM , also incorporated the plugin documentaion in the helpfile instead of a seperate .RTF
Added a few new functions to the plugin system and made the examples easier to understand (Mainly for helping with assembly scripts)
Added a packet editor example plugin
Assembler can now work with " and ' strings
Changed the reverse pointer scan to give more details about what is going on
The positions of the main window and memoryview can now be saved
The about window now tells you which version of dbvm is loaded if dbvm is running
If DBVM is running and you choose phsyical memory, it goes through dbvm's read physical memory instead of windows'
Added a floating point panel to several windows that use system context to display variables
Added the option to the memoryview hexview part to display 2 bytes, 4 byte, float or double instead of bytes
Improved the dissect data window to show addresses next to eachother


Fixes:
Fixed the multicore scan crash
Fixed the MEMORYFIRST.TMP file showing up in a weird location
Fixed several hotkey setting bugs
Fixed loading back the speedhack hotkey values
Fixed a crash when opening a process with a broken PE-header
Fixed several assembler and disassembler instructions
Fixed memoryviewers "copy to clipboard" option where it picks the wrong option
Fixed binary scan crash on nextscan
Fixed huge memory leak in pointerscanner
Fixed the "No error message" on error during a scan
Fixed bug with calling kernel_XXXX functions in assembler
Fixed a trainer bug regarding clicking on cheats
Fixed 32-bit icons in the trainer maker
Fixed the terminate scan option
Fixed a DPI bug on several windows
Fixed a crash when loading of the symbols failed





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