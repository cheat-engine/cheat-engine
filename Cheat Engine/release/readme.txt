Additions and changes:
Added support for il2cpp
Added support for .NET dll plugins
Change register on breakpoint now also affects FP and XMM registers
Added CEShare, a way to share your tables with other people
Improved disassembling
copy bytes+addresses now only does bytes+addresses
call filter can now use the unwind data for functions to get a decent list of instructions
structure dissect shows the pointerpath at the bottom
Follow register while stepping (rightclick the register to show the option)
registersymbol and label now support multiple definitions in one line
improved the speed of the structure list when getting data from a pdb
hexview: doubleclicking a non-byte value now shows in the type you set
added sorting to the found code dialog
added filtering to the changed addresses window
the debugger settings won't lock from changing anymore, still needs you to reopen a process to have an affect
added always hide children groupoption
group headers can act as address now
AA command createthreadandwait now has a timeout parameter
Assembler scanning improvement
Added an AVX2 version of CE, which will speed up all those floating point operations CE does so much...
Improved structure lookup for PDB files
Symbolhandler can now have the following types in front of pointers : (BYTE), (WORD), (DWORD), (QWORD), (CHAR), (SHORT), (INT), (INT64) to typecast the pointer to a value of that type
Structure dissect can detect vc++ and object pascal classnames now
Dissect code now also detects references to strings
Sorting the addresslist now sorts faster and more properly with regards to groups (depends on the level your current selection is)
Rightclick the addresslist header to bring up a menu which allows you to disable sorting
Improved the processlist responsiveness
The chosen floating point rounding type is now saved in the registry
You can now use (addresslist description) as an address
Improved autocomplete

Changes:
DBVM doesn't activate the TSC hook by default. You can still activate it by using dbvm_enableTSCHook() 



Fixes:
Fixed memoryleak when opening a file for hexediting again
Fixed utf8 display of the dissect windows window
Clear the taskbar progress when using a custom scan
Hexview: Fixed changing the address when pressing a non char key
Hexview: Fixed changing the address when doubleclicking and then canceling
alloc with a prefered base is now more aggressive in getting the range you want
fixed mono symbol lookup while dlls are still being loaded
fixed the structure compare not giving a proper errormessage
fixed improper error messages in structure dissect
fixed opening process in XP
fixed potential deadlock with the symbolhander
fixed issue with using the process var as symbol
fixed default form size for some windows when using high DPI
fixed DBVM on systems that have the default MTRR set as 6 (e.g Asus systems)
fixed DBVM find what accesses with large datasets


lua:
  New functions:
    function onTableLoad(before)
    sendMessageTimeout
    createTimer(delay,function())
    createStructureFromName()
    createSynEdit()


  changes:
    fixed executeCodeLocalEx with certain parameter definitions
    fixed openFileAsProcess
    checkSynchroniuze has a timeout now
    OnGetDisplayValue now also works on AA records


How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
