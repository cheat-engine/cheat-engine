Cheat Engine 6.8

Fixes:
Fixed some more high dpi issues
Fixed issues with the dropdown list in memory records
Fixed offset symbols
Fixed registered binutils
Fixed graphical issues with the tablist
Fixed issue where memory blocks would get cut of before the page end
Fixed some memory leaks
Fixed some graphical issues in the addresslist
Fixed rightclick on r8 and r9 in memoryview
Fixed disassembling some instructions
Fixed DBVM so it works on windows 1709 and later (tested on 1803)
Fixed several DBVM offload crashes
Fixed freeze with allow increase/decrease for 8 byte long values
Fixed several issues where minimizing a window and then close it would hang CE



Additions and changes:
Text editor improvements
Added hundreds of new cpu instructions
Mono now has some new features like instancing of objects
Mono instances window is now a treeview where you can see the fields and values
"find what addresses this code accesses" can also be used on RET instructions now (useful to find callers)
The graphical memory view now has a lot more options to set it just the way you need
Codepage support in hexview
structure data from PDB files is now stored in a database for lookup later
dissect structures form can now show a list of known structures (pdb, mono, ...)
Added a "revert to saved scan" option  (lets you undo changes)
Added a "forgot scan" option (in case you forgot what you're doing)
Pointerscan limit nodes is default on in a new ce install (remembers your choice when you disable it)
Autoattach now happens using a thread instead of a gui blocking timer
Some colorscheme enhancements
Added a DBVM based "Find what writes/accesses" feature. (For pro users, enable kernelmode options for it to show)
Changed the dissect data setup from seperate yes/no/value dialogs to a single window
Added a bypass option for ultimap2 on windows 1709. When using ranges, do not use interrupts, or use DBVM
Added find what writes/access to the foundlist
autoassembler scriptblocks are now grouped when written to memory



lua:
added automatic garbage collection and settings to configure it
added new functions, like:
  reinitializeSelfSymbolhandler
  cpuid
  and more



How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
