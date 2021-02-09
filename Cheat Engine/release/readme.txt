Additions and changes:
  Added dark mode support (restart CE when you channge the setting)
  Hotkeys can be repeated by releasing the key anbd repressing if the repeat timer hasn't finished yet
  structure dissect add to addresslist uses the addressstring instead of number, so symbols will be preserved
  structure dissect now has a option to save the previous state of a column and show changes easier
  Added {$LUACODE} blocks for inline Lua coding
  Added a c-compiler to CE (compile)
  Added {$C} blocks to the auto assembler. all {$C} blocks get combined into one script before execution
  Added {$CCODE} blocks for inline C coding (Check the forum, wiki, CE patreon discord or CE's youtube)
  Added a C# compiler (compilecs)
  Added routines to do .NET(and mono) method detouring.  .NET info has a new contextmenu where you can create a detour template for the autoassembler
  [Disable] sections can now reference labels, defines, AOBScan results, and allocs created in the [ENABLE] section
  Userdefined symbollist has a secondary list for CCode symbols



  

Fixes:
  Lua debug now shows for loop variables
  several windows now save their position, and won't get corrupted if you don't show them the first time running CE
  fixed createthreadandwait when using a timeout
  fixed disassembling vcvtsi2ss
  fixed compare to first scan if it's a large block, and made it more efficient
  ceshare: logout fixed
  fixed assembling movsq
  fixed ultimap ret filter
  

lua:
  changes:
    saveTable won't ask to sign the table anymore
    messageDialog will work if you omit the buttonlist. (Defaults to mbOK then)

  New functions: 



How to use:
There's a tutorial program included with Cheat Engine,
it teaches how to use the basics of Cheat Engine, also the helpfile may be of use.



For comments or suggestions and such, contact me at:
e-mail(msn) = dark_byte@hotmail.com


For more information about Cheat Engine or tables for it 
and other things, go to this url:
http://www.cheatengine.org/
