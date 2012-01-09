Cheat Engine 6.1

Fixes:
Fixed the first plugin from not getting activated at restart
Custom types can now do an unknown initial value scan
Fixed the auto assembler highlighter from hiding some text while typing


Additions and changes:
Added a profiler so you can find function entry points and see how often they are called
Deleting a plugin now calls FreeLibrary on it
Ctrl+enter in the luaengine form now automatically executes the command
Added direct 3d hook functions (can be used to show trainers inside games)
Plugins settings between the 32 and 64-bit executable are now seperated
Recalculate addresses with only one selection now only updates the siblings and children. Doesn't touch the parent node
Addresslist entries can have the notation +xxx and -xxx, which will calculate the address based on the parent address (If the parent address changes, these change automatically
The structure dissect has been rewritten from scratch, and the functions have been exposed to lua as well
Added a new step to the tutorial (step9) showing how to deal with shared code
Made the tutorial translatable as well (You can even translate it to unbroken English)
Added a loop option to the pointerscanner rescan


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