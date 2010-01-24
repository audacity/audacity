README_LINUX.txt for PortMidi
Roger Dannenberg
29 Aug 2006

To make PortMidi and PortTime, go back up to the portmidi
directory and type 

make -f pm_linux/Makefile

(You can also copy pm_linux/Makefile to the portmidi
directory and just type "make".)

The Makefile will build all test programs and the portmidi
library. You may want to modify the Makefile to remove the
PM_CHECK_ERRORS definition. For experimental software,
especially programs running from the command line, we 
recommend using PM_CHECK_ERRORS -- it will terminate your
program and print a helpful message if any PortMidi 
function returns an error code.

If you do not compile with PM_CHECK_ERRORS, you should 
check for errors yourself.

This code has not been carefully tested; however, 
all test programs in pm_test seem to run properly.

A NOTE ABOUT AMD64:

When compiling portmidi under linux on an AMD64, I had to add the -fPIC
flag to the gcc flags.

Reason: when trying to build John Harrison's pyPortMidi gcc bailed out
with this error:

./linux/libportmidi.a(pmlinux.o): relocation R_X86_64_32 against `a local symbol' can not be used when making a shared object; recompile with -fPIC
./linux/libportmidi.a: could not read symbols: Bad value
collect2: ld returned 1 exit status
error: command 'gcc' failed with exit status 1

What they said:
http://www.gentoo.org/proj/en/base/amd64/howtos/index.xml?part=1&chap=3
On certain architectures (AMD64 amongst them), shared libraries *must* 
be "PIC-enabled".

CHANGELOG

29-aug-2006 Roger B. Dannenberg
   Fixed PortTime to join with time thread for clean exit.    

28-aug-2006 Roger B. Dannenberg
    Updated this documentation.
 
08-Jun-2004 Roger B. Dannenberg
      Updated code to use new system abstraction.

12-Apr-2003 Roger B. Dannenberg
      Fixed pm_test/test.c to filter clocks and active messages.
      Integrated changes from Clemens Ladisch:
          cleaned up pmlinuxalsa.c
          record timestamp on sysex input
          deallocate some resources previously left open
