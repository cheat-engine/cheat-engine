Cheat Engine 6.7

Fixes:
Fixed some DPI issues at some spots
Fixed the "Not" scan for ALL
"simple values" now also applies to the All type
Fixed not adding the 0-terminator to strings when the option was set to add it
Fixed ultimap hotkeys
Fixed ultimap2 filtering
Changing pointers in the change address dialog won't set/override global memrec and address anymore (local now)
Fixed show as signed not working for custom types
Fixed several issues with the structure spider
Fixed 64-bit registers in the tracer getting truncated on doubleclick, and fix r8 to r15
Fixed copy/paste in the scanvalue
Fixed kernelmode QueryMemoryRegions for windows build 1607
Fixed some disassembler errors
Fixed lua command fullAccess
Fixed text to speech if launched from a different thread
Fixed clicking on checkboxes when the dpi is different
Fixed the found code dialog count size
Fixed mono freezing Cheat Engine when it crashes/freezes


Additions and changes:
Changed the processlist and added an Applications view similar to the taskmanager
Small change to the tutorial first step wording
Structure Dissect: Added RLE compression (by mgr.inz.player) and other things to improve filesize
Structure Dissect: If setting a name, it will also be shown in the header
The symbolhandler can now deal with complex pointer notations
Added support for single-ToPA systems for ultimap2
Added some more spots where the history will be remebered in memoryview
Memoryrecords with auto assembler scripts can now execute their code asynchronous (rightclick and set "Execute asynchronous")
Kernelmode memory reading/writing is safer now
Added an option to filter out readable paths in the pointerscan rescan
Added "codePage" support
Added font/display options to several places in CE
Added a search/replace to the script editors
You can now delete addresses and reset the count from "Find what addresses this code accesses"
Added a statusbar to the hexview in memoryview
Pointerscan for value scans now add the results to the overflow queue
Opening a file and changing bytes do not change them to the file anymore (you need to explicitly save now)
Added an option to the processlist to filter out system processes
Added a system to let users sign their tables so you know you can trust their tables.
Memory record dropdown lists can now reference those of others. USe as entry text: (memoryrecorddescription)
Added an option to notify users of new versions of Cheat Engine


lua:
Custom Types can now be referenced from Lua
Auto assembler lua sections now have access to "memrec" which is the memory record they get executed from. Can be nil
stringToMD5String now support strings with a 0 byte in them
autoAssemble() now also returns a disableInfo object as 2nd parameter. You can use this to disable a script
added Action and Value properties to MemoryRecordHotkey objects
added screenToClient and clientToScreen for Control objects
added readSmallInteger and writeSmallInteger
added enableDRM()
added openFileAsProcess/saveOpenedFile
added saveCurrentStateAsDesign for CEForm objects
added disableWithoutExecute and disableAllWithoutExecute
added OnCustomDraw* events to the listview
added being/endUpdate for the Strings class
added SQL support
added color overrides to the disassembler text
added OnPaint to the CustomControl class
added autoAssembleCheck to syntax check an AA script
fixed the addresslist returning nil for PopupMenu (while popupMenu did work)
added an timeout option for pipes
added some graphical options
added some low level system functions


How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
