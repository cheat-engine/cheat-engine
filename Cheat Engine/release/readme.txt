Additions and changes:
  AA templates now generate 14 byte jmp scripts when holding down ctrl
  Foundcode dialog: Replace now toggles between nop and original.  Also prevents duplicates
  improved keyboard control to the hexview in memoryview. You can now hold shift while using the cursors to move
  laststate isn't saved in tables anymore (unless ctrl is down)
  added some space for dbvm functions so it's less likely to click them  
  you can now manually delete saved results
  debugger attach timeout window will now show the status on some debugger interfaces
  modules for 64-bit in 32-bit targets are more clearly marked as such
  mono will not try to re-attach after a disconnect



Fixes:
  fixed loading back highligter config for auto assembler windows
  .netinfo: fix field searching
  fixed disassembler issues/memory corruption when closing a secondary memoryview window
  fixed brake and trace killing the debugger when skipping certain modules an failing in figuring out the return address
  fixed auto attach not stopping the process flash
  mono is less likely to disconnect when dissecting an invalid memory address
  fixed checkbox and radiobutton not sizing properly in dark mode
  foundlist: display type override also afffects the saved columns
  foundlist: new scan now alsdo clears the saved results
  processlist: Fixed the highlighted color process entries in light mode
  fixed compare to first scan hotkey
  fixed handling of broken/empty language folders
  fixed network modulesize lookup. (needs a new ceserver build as well)
  fixed position saving for the foundcode dialog
  fixed lua errors not giving a proper errormessage
  fixed {$c} and {$ccode} for the 32-bit CE build
  fixed logging of writes to ignore the addresslist freezing(Skyrimfus)
  fixed dealing with -0.0f in c/ccode blocks
  fixed memscan on the last block of readable memory
  fixed dealing with the proper way of namespace.classname:modulename formatting.  (Supports both formats)
  fixed error when using freeze by thread with a very small interval
  fixed {$ccode} and {$luacode} when not giving any parameters
  fixed some include files erroring out when used 

lua:
  fixed copyMemory mode 2
 


How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, go to this url:
http://www.cheatengine.org/
