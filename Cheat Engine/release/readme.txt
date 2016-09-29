Cheat Engine 6.6

Fixes:
Fixed saving of hotkey sounds
Fixed the CF flag in the disassembler stepping mode
Fixed Kernelmode VirtualQueryEx for Windows 10 build 14393
Fixed DBVM for Windows 10 build 14393
Fixed the shortest assembler instruction picking for some instructions
Fixed a few bugs in the break and trace routine when you'd stop it while the thread still had a single step set
Fixed several ansi to UTF8 incompatbilities that poped up between 6.5 and 6.5.1
Fixed the stackview not properly setting the color, and giving an error when trying to change a color
Fixed the exe generator not adding both .sys files or the .sig files when using kernel functions
Fixed some places of the disassembler where it helps guessing if something is a float or not
When using the code finder, it won't show the previous instruction anymore if it's on a REP MOVS* instruction
Fixed an issue when editing memoryrecords with strings, where wordwrap would add newline characters
Fixed D3D alpha channel for textures and fontmaps
Fixed the helpfile not being searchable
The installer will now mark the CE destination folder as accessible by APPS. (fixes speedhack for some APPS)
Fixed the form designed crashing is resized 'wrong'


Additions and changes:
Ultimap 2 for Intel CPU's of generation 6 and later (no DBVM needed for those)
Language select if you have multiple language files for CE
Memoryrecord pointer offsets can use calculations, symbols and lua code now
While stepping in the debugger you can now easily change the EIP/RIP register by pressing ctrl+f4
changed the way CE is brought to front when a hotkey is pressed
Made the GUI more adaptive to different fontsizes and DPI
Several font and minor GUI changes
Added DPIAware and a font override to the settings window. (DPI aware is on by default, but can be turned of if experiencing issues)
Added option to enable pause by default
Disassembling mega jumps/calls now show the code in one line
The standalone auto assembler window will now give an option to go to the first allocated memory address
Changed the point where the settings are loaded in CE's startup sequence
The formdesigner now allows copy and paste of multiple objects, and uses text
Added scrollbox and radiogroup to the formdesigner
Added Middle, MD5 and MD5 as allowable hotkeys
Added controller keys as hotkeys
Single stepping now shows an indication if an condition jump will be taken
Added a watchlist to the debugger
Added the 'align' assembler pseudo command (allocates memory so the next line is aligned on a block of the required size)
Added the 'Not' option for scans, which causes all addresses that match the given entry as invalid
Changed the Unicode text to UTF-16. Text scans are now UTF8/UTF16 (no codepage)
Hexview can now show and edit values in 3 different textencodings. (Ascii, UTF-8 and UTF-16)
Rescan pointerscans on pointerscans that where done on a range can now change the offset


lua:
speak(): Text to speech
hookWndProc: a function that lets you hook the windows message handler of a window
registerEXETrainerFeature: Lets you add extra files to the exe trainer file packer
getFileVersion(): A function to get version information from a file
mouse_event() : Lets you send mouse events to windows. (move, click, etc...)
loadFontFromStream() : Lets you load a font from a memory stream. (Useful for trainers that use a custom font)
added several thread synchronization objects
control class: added bringToFront and sendToBack

lua changes:
dbk_writesIgnoreWriteProtection() now also disables virtualprotectex calls from CE
loadTable() can now also load from a Stream object. 
the addresslist has some Color properties published for better customization

the LUA server has had some new commands added so hooked code can do more efficient calls. (LUAClient dll has been updated to use them in a basic way)





How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
