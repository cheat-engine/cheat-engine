Additions and changes:
 

from 7.4-7.4.1:
  added .Visible property to treenode entries
  added .VisibleRowCount and .TopItem to listviews
  added arm64 disassembling and assembling
  added lua function "runCommand"
  added a radiobutton to select if the generated script will use 5 or 14 byte jmps.
  conditional jumps can now deal 2gb+ destinations (will get rewritten)
  dotnetinfo: Performance improvement
  memory record hotkeys now have a "Only while down" option
  Updated the dbghelp to a more recent version which can better handle nowadys pdb symbols
  different memory allocations now get placed within the initial allocation block. Protection is changed afterwards
  tracer can now step over rep instructions
  lua stringstream now inherits from memorystream, so you have access to the Memory field
  lua: Added a callback for whenever the structure list is modified
  added architecture distinguishing to ceserver
  pressing escape in the hotkey form will now close it
  added nested structure support
  added string based custom types
  ctrl+enter in the disassembler now shows relative addresses from that point
  the diffcount in "find out what accessess/writes" will now stay even when disabling the option to find the number of different addresses an instruction accesses

from 7.4-7.4.1:
Fixes:
  fixed the all type not finding 4 types when double was deselected
  fixed the "all" type when not using double
  fixed ccode esp access in 32-bit and "reg"f types
  fixed disassembling when using binutils for disassembly
  fixed the tablefiles menulist eating memory because they didn't get deleted properly
  fixed .net issues that use obfoscated modules and missing metadata
  fixed paring value starting with a - or +
  fixed assembling pmovmskb
  fixed disassembling vgather* vex256 instructions and allow usage of xmm/ymm registers as address (for instructions that allow it. Like this one)
  fixed the addresslist not giving a proper error when using multiple enable or disable section
  fixed error when using ctrl for speedhack hotkeys
  fixed the groupscan command parser from assigning wildcard to the wrong combobox
  fixed disassembling xchg eax/rax,xxx
  fixed lua custom type registering as float when using the non lua function method
  fixed small memoryscan issue for data at the end of a memoryblock
  ccode doesn't register useless symbols anymore