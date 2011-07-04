Welcome to UnderC version 1.2.9w

This package consists of the executable (UCW), a default script file,
this file, and the library files.  It is important that the header files
end up in a 'include' subdirectory of the directory where UCW is found.
If you unzip this file using its path information ('use folder names') this will
automatically happen. You can optionally specify the UnderC directory
with the environment variable UC_HOME; note that this points to the directory
containing ucw.exe.  If you do this, then you can copy the executable anywhere 
and it will still be able to find the header files.

defs.h is the default script that is included when UC loads, and
contains the basic standard library includes (it's of course possible
to define and use another defs.h by working from another directory). 
If it is not found in the current directory, then the version in the 
UCW directory will be used.

Purists will notice 'using namespace std;' in defs.h and may delete it if it
makes them feel better.  These headers make no pretense at implementing
the full standard library versions - the string and the iostream 
classes are not templates, and should be seen as 'pocket' versions
for demonstrating the full power of these classes.  The final
release versions will be more powerfull;  in particular, I haven't
implemented any formating control of the ostream output.

When UC loads you will be presented with the ;> prompt. You can now
type a valid C++ statement, a preprocessor directive like #include, or
one of several extra interactive commands which all begin with '#'. 
I have an IDE in development, which is an editor which communicates
with UCW and provides a menu-driven interface to these commands.

Command Summary

Quit Command: #q, #ql

Close session; #ql in addition writes a unique log file, with a
name based on the time and date.

Load Command: #l <file>
The most important of these is '#l file' which has the same effect
as '#include "file"', except that the system does some clean-ups if
that module has been previously loaded.  It will remove any macros
that were defined in that source file, clean out typedefs, and 
remove any injected namespaces.  So even if the std namespace is 
loaded in your interactive session (and this is the default) this
doesn't apply to any #l'd files.

*NB* You still have to say 'using namespace std' in any #l'd files,
if you want std injected into the global namespace.  I am thinking
of changing this requirement.

After the first load, '#l' on its own will reload the last file.

Run Program: #r <file> <command-line arguments>
After a file containing a main() function is loaded, and successfully
compiled, #r will let you run that program with the supplied
arguments.  The program is run in its own thread and console window,
so you can interactively evaluate variables etc while the program is
waiting for input.

Execute shell command: #x <cmd>

For example, #x dir /w

Load Library command: #lib <dll>

After this command, any prototype or class definition is assumed
to be a request for dynamically linking to the library.

There are also commands for setting breakpoints, etc, but these are
still experimental, and chiefly intended for use of the IDE.

Built-in Functions

The following library functions are already available:
sin, cos, exp, log, atof, atoi, rand
strcpy, strncpy, strcat, strcmp, strdup, strtok, strstr, strlen,memmove
puts,printf,sprintf,gets,fgets,fprintf,fscanf,fread,fwrite,feof,
fopen,fclose,fflush

You will find it necessary to put empty stdio.h, etc headers in the
include directory for compatibility with traditional systems.

Importing Functions and Classes

It is easy to import any extern "C" function from a DLL. For example,
all modern Linux systems have the runtime shared library, libc.so.6
To make isalpha() available, one can say:

;> #lib msvcrt40.dll
;> extern "C" int isalpha(char);
;> #lib
;>isalpha('*');
(int) 0

It is also possible to use '#pragma dlink' which has the same meaning as 
'#lib' but is C++ compatible.

It is possible to import classes which have been compiled with
GCC 2.96 and 3.2  This is compiler-specific because generally
C++ export names are 'decorated' or 'mangled'.

This is still work in progress and obviously it still gets
confused.  However, it is possible to inherit from an imported 
class and override a virtual method, which is more than older systems
like CINT can do.  I have managed to import VTK vs 1.0 (Visualization
Toolkit) which is a large class library for visualizing data in 
three dimensions.  This was about 200 classes, so the system can
definitely scale up.  Currently I am working on importing YAWL
(Yet Another Windows Library) which is the GUI framework 
which UC and the IDE use. 

Features

UC implements a generous subset of the ISO standard, including
exception handling, namespaces, and templates.  Obviously the more
advanced features are somewhat restricted, but will do fine for
educational purposes.  UC compiles source into an intermediate
p-code, so as interpreters go it is pretty fast, but speed has not
been a concern yet.  I think correctness, robustness and friendliness
are my main goals, and there has been good progress with the first two.
(The error messages are still a little non-obvious).

Limitations

Templates are implemented naively, as macro-like insertions of source,
rather than by building an abstract parse tree.  One implication of
this is that UCW instantiates _all methods_, including those which
obviously don't appy to all possible parameter types.  For example,
list<T>::remove() doesn't make sense unless objects of type T can be
compared.  I've hacked UC so that not finding operator== in a
simple struct is not a problem.

Instantiation errors are currently not reported on the correct line.

There are still a few rough edges with the parsing, which as always
with C++ has been distinctly non-trivial.

I intend to have a consolidated list of known problems available
soon.

Portability

Currently UnderC runs on Linux and Windows, where there is both a console
and a graphical version.  It should be easy to move across to any x86 
system; the machine dependencies are all contained in directcall.cpp and
consist of a hundred lines of inline assembler.

An Example Transcript

UnderC Development Project, Vs 0.9.0w
Steve Donovan, 2001
;> // demonstrating std::string
;> string s = "hello dolly";
;> s.substr(0,5);
(string) 'hello'
;> s.substr(s.find("dolly"),5);
(string) 'dolly'
;> s += " you're so swell";
(string&) 'hello dolly you're so swell'
;> s.length();
(int) 27
;> // creating a list of strings...
;> list<string> ls;
instantiated: list<string>
;> ls.push_back(s);
;> ls.push_back("way back when");
;> ls.front();
(string&) 'hello dolly you're so swell'
;> ls.back();
(string&) 'way back when'
;> ls.push_front("singing...");
;> typedef list<string> LS;
;> LS::iterator li;
;> for(li=ls.begin();li!=ls.end();++li)
;>   cout << *li << endl;
singing...
hello dolly you're so swell
way back when
;> // demonstrating vectors...
;> vector<int> vi;
instantiated: vector<int>
;> for(int i=0;i<10;i++)
;>   vi.push_back(i);
;> vi[9];
(int&) 9
;> // a useful shortcut!
;> #define FORALL(i,c) \
;>  for(i=c.begin();i!=c.end();++i)
;> vector<int>::iterator ii;
;> int sum = 0;
;> FORALL(ii,vi) sum += *ii;
;> sum;
(int) sum = 45
;> // typing in a function
;> int sqr(int i) { return i*i;}
;> sqr(10);
(int) 100
;> double sqr(double x) { return x*x; }
;> sqr(1.2);
(double) 1.44
;> // the function main is special
;> int main() {
;:1} int i,j;
;:1} cin >> i >> j;
;:1} cout << "i+j = " << i+j << endl;
;:1} }
;> #r                      


Steve Donovan,
 sdonovan@mweb.co.za
