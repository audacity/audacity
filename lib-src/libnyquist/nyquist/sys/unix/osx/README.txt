sys/unix/osx/README.txt -- notes on OS X (Apple) version of  Nyquist

Roger B. Dannenberg
23 April 2011

HOW TO REBUILD STUBS TO CALL NYQUIST PRIMITIVES FROM XLISP

The file nyqsrc/sndfnint.c implements XLISP functions that correspond
to Nyquist primitives such as SND-OSC. This file and a number of
associated files (such as sndfnintdefs.h) are built automatically by
the intgen program. This process is automated in the Linux Makefile,
but here's how to do it under OS X.

0. COMPILE NYQUIST WITH XCODE
Before changing the sources, make sure you can compile your existing
installation of Nyquist using XCODE.

1. COMPILE INTGEN WITH XCODE
From XCODE, choose Active Configuration: Deployment, Active Target:
intgen, Active Executable: intgen.

Build (Command-B).

Result is in nyquist/macosxproject/build/Deployment/intgen. You might
want to make an alias to this or move it to your ~/bin directory or
change your PATH so you can execute this from the command line without
typing the full path. In the following, I assume simply typing
"intgen" will run the application.

After setting up intgen to run, the command "which intgen" should
indicate the path to intgen that the shell will use.

2. WORK IN A TERMINAL WINDOW AND CD TO THE MAIN NYQUIST DIRECTORY
Open a Terminal application.
cd nyquist [or navigate using cd to wherever your nyquist sources are]

3. GENERATE THE INTERFACE CODE
Note: if you are adding a new primitive, you will want to modify
sys/unix/osx/sndfnint.cmdline by inserting the name of the header (.h)
file for the new primitive. (For a nyquist release, the name should
also go into a list in nyquist/misc/transfiles.lsp and the Makefiles
should be rebuilt, but we'll stick to the Apple-specific tasks here.)

intgen @sys/unix/osx/sndfnint.cmdline

The files sndfnint.c, sndfnint.lsp, sndfnintdefs.h, and sndfnintptrs.h
will be generated and placed in the current (nyquist) directory.

4. INSTALL THE FILES
You might want to make a safe backup copy of the original
nyqsrc/sndfnint.c, nyqsrc/sndfnint.lsp, nyqsrc/sndfnintdefs.h, and
nyqsrc/sndfnintptrs.h for reference.

Move the new files to nyqsrc:

mv sndfnint.c sndfnint.lsp sndfnintdefs.h sndfnintptrs.h nyqsrc

5. RECOMPILE NYQUIST WITH XCODE


