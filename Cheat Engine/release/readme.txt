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
  lua: fixed copyMemory mode 2  

from 7.3.1-7.3.2:
  structure dissect watch for changes now also shows you when something has changed inbetween
  added hints to how the pointer wildcard works 
  created an alternate named CE version that runs without admin (not an UCE, do not think it is one, you will be disappointed)
  the replace button in foundcode dialog now supports multiselect
  You can now also change values of groupscan scan results directly in the foundlist
  lua's openProcess command now won't deactivate all entries when previously no process was selected
  you can now edit instructions with a breakpoint on them
  added linux ABI c-compiler dll's
  by default mono now releases the .net thread

from 7.3.2-7.4:
  added shortcut to add this address to addresslist in hexview (numPlus)
  goto address popup now centers on the memview window, instead of screen center
  you can now change the font of the tracer tree
  added isRep to the lua LastDisassemblerData field.  And stepover now steps over rep instructions
  break and trace: Added 'stay within module' option
  added custom alignment option to the hexviewer section of the memoryviewer




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

from 7.3.1-7.3.2:
  network ceserver/linux: Fixed wpm corrupting the memory
  fixed the elf symbol parser
  fixed speedhack on linux
  il2cpp now has a progressbar
  fixed handling some newer il2cpp games
  fixed vmin assembling  
  fixed the ceshare color  (bug introduced in 7.3.1)
  fixed freezing when entering the wrong ceserver details
  fixed deleting groupscan entries from the scan
  fixed pointerscan not loading results when in a path with non-ascii characters
  fixed the standalone trainer maker giving an error about duplicate entries

from 7.3.2-7.4:
  lua: fixed readByte signextending when it shouldn't
  fix changeregonbp where it only changed xmm0
  window position saving of "find what addresses this code accesses" should be more predictable
  fixed saving of some color preferences in hexview, and added the fadecolor
  fix AA createThreadAndWait not working in a standalone script
  improved stability of mono
  fixed break and trace ignore flag causing an stop instead of ignore on 64 bit targets
