Additions and changes:
Added support for il2cpp
Added support for .NET dll plugins
Change register on breakpoint now also affects FP and XMM registers
Added CEShare, a way to share your tables with other people
Improved disassembling
copy bytes+addresses now only does bytes+addresses
call filter can now use the unwind data for functions to get a decent list of instructions
structure dissect shows the pointerpath at the bottom
Follow register while stepping
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


Fixes:
Fixed memoryleak when opening a file for hexediting again
Fixed utf8 display of the dissect windows window
Clear the taskbar progress when using a custom scan
Hexview: Fixed changing the address when pressing a non char key
Hexview: Fixed changing the address when doubleclicking and then canceling
alloc with a prefered base is now more aggressive in getting the range you want
fixed mono symbol lookup while dlls are still being loaded
fixed the structure compare not giving a proper errormessage


lua:
  New functions:
    function onTableLoad(loaded)
    sendMessageTimeout
      


  changes:
    fixed executeCodeLocalEx with certain parameter definitions
    fixed openFileAsProcess
    checkSynchroniuze has a timeout now

How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
