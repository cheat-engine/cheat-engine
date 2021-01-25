Tiny C Compiler - C Scripting Everywhere - The Smallest ANSI C compiler
-----------------------------------------------------------------------

Features:
--------

- SMALL! You can compile and execute C code everywhere, for example on
  rescue disks.

- FAST! tcc generates optimized x86 code. No byte code
  overhead. Compile, assemble and link about 7 times faster than 'gcc
  -O0'.

- UNLIMITED! Any C dynamic library can be used directly. TCC is
  heading toward full ISOC99 compliance. TCC can of course compile
  itself.

- SAFE! tcc includes an optional memory and bound checker. Bound
  checked code can be mixed freely with standard code.

- Compile and execute C source directly. No linking or assembly
  necessary. Full C preprocessor included.

- C script supported : just add '#!/usr/local/bin/tcc -run' at the first
  line of your C source, and execute it directly from the command
  line.

Documentation:
-------------

1) Installation on a i386/x86_64/arm/aarch64/riscv64
   Linux/macOS/FreeBSD/NetBSD/OpenBSD hosts.

   ./configure
   make
   make test
   make install

   Notes: For FreeBSD, NetBSD and OpenBSD, gmake should be used instead of make.
   For Windows read tcc-win32.txt.

makeinfo must be installed to compile the doc.  By default, tcc is
installed in /usr/local/bin.  ./configure --help  shows configuration
options.


2) Introduction

We assume here that you know ANSI C. Look at the example ex1.c to know
what the programs look like.

The include file <tcclib.h> can be used if you want a small basic libc
include support (especially useful for floppy disks). Of course, you
can also use standard headers, although they are slower to compile.

You can begin your C script with '#!/usr/local/bin/tcc -run' on the first
line and set its execute bits (chmod a+x your_script). Then, you can
launch the C code as a shell or perl script :-) The command line
arguments are put in 'argc' and 'argv' of the main functions, as in
ANSI C.

3) Examples

ex1.c: simplest example (hello world). Can also be launched directly
as a script: './ex1.c'.

ex2.c: more complicated example: find a number with the four
operations given a list of numbers (benchmark).

ex3.c: compute fibonacci numbers (benchmark).

ex4.c: more complicated: X11 program. Very complicated test in fact
because standard headers are being used ! As for ex1.c, can also be launched
directly as a script: './ex4.c'.

ex5.c: 'hello world' with standard glibc headers.

tcc.c: TCC can of course compile itself. Used to check the code
generator.

tcctest.c: auto test for TCC which tests many subtle possible bugs. Used
when doing 'make test'.

4) Full Documentation

Please read tcc-doc.html to have all the features of TCC.

Additional information is available for the Windows port in tcc-win32.txt.

License:
-------

TCC is distributed under the GNU Lesser General Public License (see
COPYING file).

Fabrice Bellard.
