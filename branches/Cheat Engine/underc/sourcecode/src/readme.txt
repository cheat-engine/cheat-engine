Building UnderC is easiest using Microsoft C++ 6.0. I've supplied
a workspace containing three projects; ucc is the text console
version, uccdll is the DLL, and ucw is the graphics-console
version with graphics.

To rebuild UCW, make ucw the current project, set to release build,
and go for it.  You can then copy .\ucw\src\wrelease\ucw.exe to
some convenient place, like .\ucw. I've supplied some verification
code in .\ucw\verify; you should be able to load tests.cpp and run
the program without any trouble. 

The supplied makefile is for the mingw port of GCC - just type 'make'
in the source directory.



