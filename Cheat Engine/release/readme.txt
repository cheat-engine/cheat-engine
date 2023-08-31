Additions and changes:

from 7.5 to 7.5.1:
No more version numbering in the main caption. Only the about screen now has it
can run without admin and asks for admin if needed (you can set to run as admin in settings as well)
better error reporting (Especially if using the debugsymbol version)
symbol synchronization.  Symbols are shared between CE instances and remembered when reopening CE
Speedhack v3 now replaces the old speedhack in windows. No more speedhack dll's to inject (more dll's to follow in the future)
byteTableToxxx now support an start index
autoassembler command AOBSCANEX - Scan only executable memory
c compiler: added __stdcall define
c compiler: windows: auto assembler can deal with stdcall mangled symbols names, and c compiler can deal with unmangled symbols when stdcall is used
c compiler: header files are used as table files. And tablefiles can have any name now
added Java info similar to .net/mono info
improved the .net info classlist performance (especially noticable in ceserver)
ctrl+space on selected bytes in hexview will make the disassembler go there
bunch of mono info improvements
lua: treenode.Index is now writable
redesigned the internals of structure dissect. Expect many new bugs there, please report! (I already know a few of them, but just waiting to get a report on them before fixing ;-) )



Fixes:
from 7.5 to 7.5.1:
Loooots of fixes in ceserver from symbollookup to debugging, pipes, basic initialization going wrong, etc...
fixed symbol to address lookup picking an old version of a same named symbol instead of later (was an issue with symbollists, like ccode)
fixed an error with .net/mono info giving an error when looking at a string
fixed disassembling of some vector instructions
fixed VEX instructions not working in 32-bit targets
fixed the translation files not being up to date in the installer
dissectcode high dpi fix