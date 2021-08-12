Additions and changes:
  Added dark mode support (restart CE when you channge the setting)
  All saved results are now shown in the foundlist (can be turned off)
  Groupscan now supports pointer wildcards. (only valid if the field is a proper pointer)
  Hotkeys can be repeated by releasing the key and repressing if the repeat timer hasn't finished yet
  Structure dissect add to addresslist uses the addressstring instead of number, so symbols will be preserved
  Structure dissect now has a option to save the previous state of a column and show changes easier
  Middle-mouse clicking now copies the value of a structure element to your clipboard
  Added {$LUACODE} blocks for inline Lua coding
  Added a c-compiler to CE
  Added {$C} blocks to the auto assembler. all {$C} blocks get combined into one script before execution
  Added {$CCODE} blocks for inline C coding (Check the forum, wiki, CE patreon discord or CE's youtube)
  Added a C# compiler (compilecs)
  Added routines to do .NET(and mono) method detouring.  .NET info has a new contextmenu where you can create a detour template for the autoassembler
  Added invoke method to the .NET Info window as well
  [Disable] sections can now reference labels, defines, AOBScan results, and allocs created in the [ENABLE] section
  Userdefined symbollist has a secondary list for CCode symbols
  The change address window now also supports relative offsets
  DBVM speed improvements
  DBVM has an extra security level, and added dbvm_setKeys to easily change the access codes
  DBVM has now some basic support for nested VM's (only so you can run them, not yet modify)
  New debugger interface: DBVM-level debugger
  Improved performance of "Find what access/writes this address"
  Dissect code now lets you specify custom ranges
  Addresslist value sort now sorts values by alphabet if the record is a string type
  The dropdown list of multiple entries can now be changed at the same time
  Standalone register window now shows flags values as well
  Value Between scans now autoswap the order if the first value is bigger than the 2nd
  

Fixes:
  fixed some games freezing CE when symbols where accesses
  Lua debug now shows for loop variables
  several windows now save their position, and won't get corrupted if you don't show them the first time running CE
  fixed createthreadandwait when using a timeout
  fixed disassembling vcvtsi2ss
  fixed compare to first scan if it's a large block, and made it more efficient
  ceshare: logout fixed
  fixed assembling movsq
  fixed ultimap ret filter
  fixed luapipe never calling OnError
  fixed vehdebug in 64-bit CE zeroing out the FPU registers in 32-bit targets
  fixed DBVM find what access/writes sometimes skipping entries on AMD
  fixed undo not working on memory records when using the single line editor
  fixed hide children group option when loading a table
  fixed some font issues in the break and trace window
  fixed pasting the other types in hexadecimal view
  fixed the symbolloader fully crashing on unknown pdb symboldata

lua:
  changes:
    saveTable won't ask to sign the table anymore
    messageDialog will work if you omit the buttonlist. (Defaults to mbOK then)
    added more customizabe button
    registerSymbol no longer errors out the whole script on failure.  It now overwrites existing symbols

  New functions:
     form.saveToStream 
     compile()
     compilecs()
     signExtend
     signTable
     symbollist.getModuleList
     symbollist.getSymbolList
     memscan.getSavedResultHandler
     memscan.getSavedResultList

     SavedResultHandler class


How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, go to this url:
http://www.cheatengine.org/
