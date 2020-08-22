Additions and changes:
  Big Endian custom types. You can enable them in settings if you like
  Commonality scanner now also compares the base address. (handy in case it's more than one register)
  translation support for ceshare
  smartedit now also deals with isPointer and isOffset memrecs
  referencedfunctions filter improvement
  PE section display/parsing for addresses
  D3D hook now asks if you're sure you wish to use it (in case of accidental click)
  Memoryview hexadecimal view:
    can now show custom types
    Changing memory protection depends on the selected byte(range)
  Break and trace window now supports searching the referencedAddress, referencedBytes and Instruction
  When changing a memoryrecord value, you can reference 'value' and apply math to it
  Added a "File->Load Recent..." menulist
  Added an option to autosave (in settings)
  Added .netcore support to the dotnet data collector
  Added a syntaxcheck menuoption to the CE lua script window
  Added tabs to the autoassembler and CE Lua script window. In case of the Lua script, the tabs get loaded from left to right whenn the table loads
  When syntax checking an AOBScan script in 64-bit that does an Alloc without prefered base, ask if the user understands that the jmp instruction will be 14 bytes long
  Some extra foundlist preferences
  Find out what access/writes now resolved the address to string (when it has time)

Fixes:
  Auto Assembler: Fixed getting weird numbers for newmem when using the templates to add new scripts
  Unknown initial value scan for 2GB+ regions failed
  Resolved issue where typecasts where replaced by addresses. (having a memoryrecord named float, would break AA scripts that'd use (float) )
  AMD support for DBVM
  Memoryrecord hotkeys showing up in the settings window as bring to front. Where clicking OK would then set it to that
  Copy paste bug in the form designer
  Hotkeys swapped comma and period on display
  Resolved some issues with the forced module loader, and if it fails, don't freeze CE forever
  AutoAssemble local would fail after opening a process
  Pointermap based rescan
  Assembler: (v)insertps , (v)comiss, (v)blendvp(s/d)

  Lua/Mono: Better support for utf8 strings
  Lua/Mono: Support targets that use mono, but not unity
  Lua/Mono: UWP targets work better

  

lua:
  changes:
    Fixed executeMethod for widestrings (type4)
    AddressList['description'] works now as well
    some scripts variables that used to be global are now local
    injectDLL has a new parameter to specify if CE should reload the symbols
    getNameFromAddress has a new parameter to specify if you wish section names (default=false)
    TfrmLuaEngine: document the mOutput and mScript properties
    loadModule now has an optional timeout value
    added an interface for the DotNetDataCollector

  New functions:    
    generateCodeInjectionScript
    generateAOBInjectgionScript
    generateFullInjectionScript
    getNextAllocNumber
    addSnapshotAsComment
    getUniqueAOB
    waitForSections
    getUserDocumentsPath
    getDotNetDataCollector

    TfrmLuaEngine:
      createLuaEngine     
  
    TfrmAutoInject:
      Properties:
        TabCount
        TabScript

      methods:
        addTab
        deleteTab

    Memoryrecord:
      properties:
        NumericanValue

      methods:
        beginEdit/endEdit

    AddressList:
       properties:
         OnAutoAssemblerEdit

       methods:
         rebuildDescriptionCache
 
    Settings:
      methods:
        getBinaryValue
        setBinaryValue




How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, goto this url:
http://www.cheatengine.org/
