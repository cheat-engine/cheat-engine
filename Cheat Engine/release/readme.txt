Additions and changes:
Icons
Added a way to dissect and show a function's working using a diagram
Auto repeat option for unchanged value scans
Added lua formula scans
Moved the compare to first/saved from scan type to a checkbox
Added a scanner that can detect and undo memory changes in the target process code sections (patch scanner under tools in memview)
The tutorial now has a link to online help for each step
Added a toolbar for stepping while debugging
When a memoryrecord fails to activate, you can rightclick it and see why it failed
AutoAssembler will now cut down on the nop's used
Better error reporting in autoassembler scripts
DBVM can now dynamically adjust the TSC. This can be disabled if it causes performance issues with dbvm_setTSCAdjust(disable)
speedhack now also hooks gettickcount64
added dw 'utf16string' support
implement VPID support in DBVM (performance increase)
kernelmode->driverlist now also shows driver exports
kernelmode symbols now also show in the enum dll's and exports
change the way slow symbol lookup is handled
several small updates to the codecave scanner
several small updates to the hexview find window
Use defered pdb loading instead of waiting for it
Several more windows have been made DPI Aware
Add option to show processid's as decimal in the processlist
When pressing shift+space instead of space to follow an instruction you will now open a new memview window
Added *:undefined support for dropdown lists
Added init.lua files for translations to use
Stackview now shows symbolnames as well
Some extra windows will now save their location
Exe trainer generator now lets you change the default files
The description of conditional jumps is better to understand now
Ultimap2 can now keep the tracer files and scan kernelmode memory as well if desired
Groups now get created afgter the selected item
Smartedit can now also 'smart'-edit children
You can now pick a breakpoint type without having to go to settings, and once picked it will be the default type for Toggle breakpoints until changes again
Floating point screen will now stay within the screen
Clicking execute in the lua engine will now change focus back to the editor field
Debugger interface settings will now only lock after a succesful attach
Some translation improvements here and there
DBVM changeregonbp's are now displayed in CE's interface as well
The foundlist colors can be customized now
Improved DBVM cloak performance when more than 20 regions are cloaked
Implemented DBVM based execute watch and "find what addresses this code accesses"
Implemented DBVM breakpoints (They always execute afterwards, including execute bp's)
Improved autocomplete so it now doesn't delete old code
Added hotkeys to ultimap2


Fixes:
fixed BSOD in recent windows updates
fixed VEH debug not handling breakpoints when a thread is created/destroyed right at the same time as a breakpoint
fixed the stacktrace in 64-bit targets
fixed rounded Extreme for float and double scans
speedhack now waits for proper dll injection
several disassembler instructions
some assembler instructions
improved the stability of dbvm find what * routines
fixed the hit counter in dbvm find what * routines
fixed kernelmode symbol loading
fixed DBVM memoryleak when disabling watches
fixed DBVM internal memory manager (more stable now)
fixed internal VirtualToPhysicalCR3 when dealing with 2MB+ pagesizes
fixed using megajmp's in cloaked regions
fixed speech when using async records
fixed executeCodeEx for more than 4 parameters
fixed static field script in mono by adding 64-bit codegeneration
fixed mono process crashing in some cases
fixed megacall following
fixed NO_CACHE memory being scanned anyhow even if it was disabled
fixed dealing with floating point values that are too big to be useful
fixed setting DR7 to a strange value when using global debug
fixed clearing DR6 when in the wrong process in kernel debug
fixed triggering kernelmode breakpoints in locations you can not properly break (no interrupts)
fixed loading dbvm inside dbvm inside dbvm inside dbvm
fixed autoassembler replacing words in quoted strings
fixed processwatcher not getting a proper PID
fixed hotkeys triggering changed/unganged for types they aren't supposed to
fixed showing dr2, dr3 and dr4 types in the threadlist

lua:
  New functions:
    compareMemory
    encodeFunctionEx
    enableKernelSymbols
    waitForExports
    waitForDotNet
    waitForPDB
    waitforsymbols
    searchPDBWhileLoading
    duplicateHandle
    getScreenDPI
    extendedToByteTable
    byteTableToExtended

    executeCodeLocalEx
    executeMethod

    allocateSharedMemoryLocal
    (un)registerCreateCallback
    (un)registerFirstShowCallback
    (un)registerCreateCallback

    dbk_usePhysicalMemoryAccess
    dbk_setSaferPhysicalMemoryScanning
    dbk_readPhysicalMemory
    dbk_writePhysicalMemory
    dbvm_addMemory
    dbvm_removechangeregonbp
    dbvm_speedhack_setSpeed
    dbvm_setTSCAdjust

    


  changes:
    getWindowList now returns the results just like ce's window list
    documented OnChangeBounds for the Control class (was already there, now documented as well)
    executeCodeEx can now also taker just parameters without typedefs
    added Data field to ListItem's 
    added description field to memrec.createHotkey method
    added clear method to the menuItem class
    readStringEx() can now deal with partial memory reads
    executeCodeEx: Fixed more than 4 parameters
    added Point type
    fixed editbox selstart/sellength types, and added CaretPos
    added HeaderSection class and document HeaderSections
    added roundRect, drawFocusRect and textRect to the Canvas. 
    added ansicode character support for textRect
    added loadFromStream and saveToStream to the RasterImage class  
    added readAnsiString and writeAnsiString to the Stream class
    Better document the mode field of createFileStream
    Expose the handle of the LuaPipeServer
    Publish frmTracer
    Publish frmUltimap2
    Publish frmCodeFilter
    Publish imagelist
    Added the DrawItemEvent general GUI property to Lua's callback system
    Added the MenuDrawItemEvent general GUI property to Lua's callback system
    Added the ContextPopupEvent general GUI property to Lua's callback system
    Created a new Diagram class group which can allow you to create graphs and diagrams
    Memoryrecord.DropDownValue and DropDownDescription work now (still RO)



How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
