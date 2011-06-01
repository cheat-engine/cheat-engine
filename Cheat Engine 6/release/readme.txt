Cheat Engine 6.1

Fixes:
Fixed DBVM from not working
Several disassembler fixes
Scanning errors now show the error
Fixed a few 16-bit assembler instructions
Fixed doubleclicking the assembler scan going to 00000000
Fixed the assembler scan going from ffffffff back to 0 and starting over again
Fixed autoattach causing huge memory leak
Fixed clicking nextscan when having 0 results
Fixed 8 byte scans so they it can now scan negative values
Prevent a 32-bit plugin from showing up error messages when loaded in the 64-bit ce version (It won't work)
Fixed the VEH debugger from not handling int3 breakpoints properly
Fixed XMM registers in the veh debugger
Fixed the VEH debugger from causing a program to hang when Cheat Engine is closed normally



Changes:
Added a structure spider which may help in finding ways to distinguish between two objects
Value scanning can now take formulas
Added a form designer to create lua extensions
Added an automated trainer generator that will generate a trainer script for you
Added lots and lots of new functions to the lua engine. Check the helpfile or main.lua
Added the ability to save binary files into a cheat table
Added an xm-player
Added columns to the stackview window
Added an option to choose if the disassembler should show 32-bit or 64-bit code
Added support to translate cheat engine to any language you want (check the language folder for more info)
Some speed improvements at several tools
Added undo last edit (ctrl+z) when editing values in a cheat table
Added extra option to the pointer rescan so you can filter out paths more specifically
Added custom comments to the assembler window
Added the ability to use lua variables inside auto assembler ( $luavariable )
Added syntax highlighting to lua
Changed the lua dlls with versions that don't need the C++ runtime installed
Changed the lua library to support 64-bit dll's
The lua script has been moved from the comments window to it's own menu on top
In the hexadecimal view when selecting 4 bytes and then pressing space will make you go there. Backspace returns
Added the .cetrainer file extension so you can download very small files and have great trainers


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