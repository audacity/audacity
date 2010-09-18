README_LINUX.txt for PortMidi
Roger Dannenberg
14 Oct 2009

To make PortMidi, you need cmake and the Java SDK.
Go back up to the portmidi directory and type:

ccmake .

Type 'c' (configure) and then 'g' (generate). You may have
to manually set JAVA_INCLUDE_PATH and JAVA_JVM_LIBRARY
by typing 't' (toggle to advanced mode) and using the 
editor to change the fields. You can find possible values
for JAVA_INCLUDE_PATH by typing "locate jni.h", and for
JAVA_JVM_LIBRARY by typing locate libjvm".

You also need JAVA_INCLUDE_PATH2, but this will normally
be set automatically after you set JAVA_INCLUDE_PATH and
run "configure" (type "c" to ccmake). Normally,
JAVA_INCLUDE_PATH2 is the linux subdirectory within
JAVA_INCLUDE_PATH.

Notice that the CMAKE_BUILD_TYPE can be Debug or Release.
Stick with Release if you are not debugging.

After successfully generating make files with ccmake, you
can run make:

make

The Makefile will build all test programs and the portmidi
library. For experimental software,
especially programs running from the command line, we 
recommend using the Debug version -- it will terminate your
program and print a helpful message if any PortMidi 
function returns an error code. (Released software should
check for error codes and handle them, but for quick,
non-critical projects, the automatic "print and die" 
handling can save some work.)

THE pmdefaults PROGRAM

You should install pmdefaults. It provides a graphical interface
for selecting default MIDI IN and OUT devices so that you don't
have to build device selection interfaces into all your programs
and so users have a single place to set a preference.

Follow the instructions above to run ccmake, making sure that
CMAKE_BUILD_TYPE is Release. Run make as described above. Then:

sudo make install

This will install PortMidi libraries and the pmdefault program.
You must alos have the environment variable LD_LIBRARY_PATH set
to include /usr/local/lib (where libpmjni.so is installed).

Now, you can run pmdefault.


SETTING LD_LIBRARY_PATH

pmdefaults will not work unless LD_LIBRARY_PATH includes a 
directory (normally /usr/local/lib) containing libpmjni.so,
installed as described above.

To set LD_LIBRARY_PATH, you might want to add this to your
~/.profile (if you use the bash shell):

LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib
export LD_LIBRARY_PATH


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

22-jan-2010 Roger B. Dannenberg
   Updated instructions about Java paths

14-oct-2009 Roger B. Dannenberg
   Using CMake now for building and configuration

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
