Cheat Engine 7.0 Beta 1

Fixes:
speedhack now waits for proper dll injection
several disassembler instructions
improved the stability of dbvm find what * routines
fixed the hit counter in dbvm find what * routines
fixed kernelmode symbol loading



Additions and changes:
speedhack also hooks gettickcount64
implement VPID support in DBVM (performance increase)
kernelmode->driverlist now also shows driver exports
kernelmode symbols nopw also show in the enum dll's and exports
implemented DBVM based execute watch



lua:
  New functions:
    compareMemory
    executeCodeLocalEx
    waitforsymbols
    waitForExports
    waitForDotNet
    waitForPDB
    searchPDBWhileLoading


  changes:
    executeCodeEx can now also taker just parameters without typedefs
    added Data field to ListItem's 
    added description field to memrec.createHotkey method
    readStringEx() can now deal with partial memory reads
    executeCodeEx: Fixed more than 4 parameters





How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
