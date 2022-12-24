Additions and changes:
 
from 7.4.1-7.4.2:
  ipt: Added intel process trace feature provided by microsoft.
  ceserver: Improve the modulelist fetch speed, more stable
  ceserver: option to disconnect from closed ceservers
  ceserver: the discovery list is now also a history list
  ceserver: implement injection on arm64 as well
  ceserver: also gets the fpu registers now
  assembler x86_64: prefer mov rax,[rip+xxx] over mov rax,[imm64] 
  disasembler x86_64: switch from r#l to r#b because why not
  mono: the dll now has a versioncheck so that you don't accidentally mix monodatacollector dll's
  mono: deal with situations where there is no mainform
  mono/.net: the methodlist is now sorted by name
  better arm disassmbler and assembler
  better arm64 disassmbler and assembler  
  the scanregions can be saved/loaded upon close/start ce (seperate option in settings)
  added an option to skip loading .PDB files
  a lot more functions are exposed to newstate threads
  added ranges scans to groupscan
  freeze+allow increase/decrease now also looks if the value is signed
  trainers: Forms and controls now scale based on DPI
  changing record showassigned/showashex now also applies to other selected entries
  texttraces now don't save as .cetrace but as .txt now
  ccode: #include now searches table files for files there as well
  ccode: the internal symbolhandler can now deal with stdcalled function symbols
  lua: added ImageIndex property to TTreeNode
  lua: added OnValuechanged and OnValueChangedByUser callbacks to MemoryRecord objects
  lua: added getOpenedFileSize()
  lua: added onHelpEvent callback
  lua: added releaseDebugFiles()
  lua: added enumRegisteredSymbolLists() and enumRegisteredSymbols()
  lua: added getBitmap method to ImageList objects

  

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

Fixes:
from 7.4.1-7.4.2:
  Fixed the tracer search for instructions ending with a ]
  VEH debug: Fixed the potential of invalid handles being used
  Kernelmode debug anmd VEH debug: Fixed setting context on non suspended threads
  fixed the lua_pcallk delegate in the c# plugin example
  fixed speedhack on wine 7.0
  fixed high dpi issue of structure dissect on first view
  fixed high dpi issue on find what access/writes dialogs
  restored the anchor editor (was gone in 7.4.1)
  fixed .net info instance lookup issue
  fixed customtypes getting marked as string (bug introduced in 7.4.1)
  fixed runcommand
  fixed modalforms from losing their text color internally (bug introduced in 7.4.1)
  mac: fixed some progressbars not properly updating

from 7.4-7.4.1:

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
