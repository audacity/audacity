README_MAC.txt for PortMidi
Roger Dannenberg
17 jan 2007

To build PortMidi for Mac OS X:

==== USING MAKE ====

go back up to the portmidi
directory and type 

make -f pm_mac/Makefile.osx

(You can also copy pm_mac/Makefile.osx to Makfile in the 
portmidi directory and just type "make".)

The Makefile.osx will build all test programs and the portmidi
library. You may want to modify the Makefile.osx to remove the
PM_CHECK_ERRORS definition. For experimental software,
especially programs running from the command line, we 
recommend using PM_CHECK_ERRORS -- it will terminate your
program and print a helpful message if any PortMidi 
function returns an error code.

If you do not compile with PM_CHECK_ERRORS, you should 
check for errors yourself.

The make file will also build an OS X Universal (ppc and i386)
dynamic link library using xcode. For instructions about this
and other options, type

make -f pm_mac/Makefile.osx help

==== USING XCODE ====

Open portmidi/pm_mac/pm_mac.xcode with Xcode and 
build what you need: if you are just exploring, start with 
the lib+test suite.

[pm_mac.xcodeproj courtesy of Leigh Smith]

CHANGELOG

17-Jan-2007 Roger B. Dannenberg
    Explicit instructions for Xcode
15-Jan-2007 Roger B. Dannenberg
    Changed instructions because of changes to Makefile.osx
07-Oct-2006 Roger B. Dannenberg
    Added directions for xcodebuild
29-aug-2006 Roger B. Dannenberg
    Updated this documentation.
 
