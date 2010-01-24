README_CL.txt for PortMidi
Roger B. Dannenberg
17 Jan 2007

This is a Common Lisp interface to PortMidi.

On Mac OSX, you need to build PortMidi as a dynamic link library
before you can use PortMidi from Common Lisp.

You can build PortMidi as a dynamic link library by running this:

cd portmidi
make -F pm_mac/Makefile.osx install-with-xcode

This is just a shortcut for:

cd portmidi/pm_mac
sudo xcodebuild -project pm_mac.xcodeproj -configuration Deployment install DSTROOT=/

You can check the file and the architecture for which it is built using:
    file /usr/local/lib/libportmidi.dylib

If you've done this install of portmidi, then you should also have 
   /usr/local/include/portmidi.h
This will be necessary to successfully build the cffi interface below.

To test PortMidi with Common Lisp, I (RBD) am using SBCL, which I 
downloaded from http://prdownloads.sourceforge.net/sbcl. Currently, I use 
    sbcl-0.9.17-x86-darwin-binary.tar.bz2
To install this, I unpacked it by just double-clicking in the finder. Then, 
from a command window, I became root using "sudo sh", and then typed:
# INSTALL_ROOT=/usr/local
# sh install.sh
# exit

I also downloaded cffi-061012.tar.gz from 
    http://common-lisp.net/project/cffi/tarballs/?M=D

To compile cffi, use the following, where "/Lisp/cffi/" is replaced by 
the actual directory of cffi, e.g. 
    "/Users/rbd/sbcl-0.9.17-x86-darwin/cffi-061012":

% sbcl
* (require 'asdf)
* (push "/Lisp/cffi/" asdf:*central-registry*)
* (asdf:oos 'asdf:load-op :cffi)
* (quit)

Download Common Music's portmidi module from cvs and build the c side:
(Replace "/Lisp" with your lisp directory, e.g. 
"/Users/rbd/sbcl-0.9.17-x86-darwin". These cvs commands will create
a new directory, portmidi.)

% cd /Lisp
% export CVSROOT=:pserver:anonymous@commonmusic.cvs.sourceforge.net:/cvsroot/commonmusic
% cvs login   # press Return at password prompt
% cvs checkout portmidi
% cd portmidi
% ./configure
% make
% cd ..

Now compile/load the portmidi module just like cffi. Again, change
"/Lisp/cffi/" and "/Lisp/portmidi" to correspond to your local file system.
(Note that /Lisp becomes your lisp directory, and "cffi" becomes your
cffi folder name, e.g. "cffi-061012".

% sbcl
* (require 'asdf)
* (push "/Lisp/cffi/" asdf:*central-registry*)
* (asdf:oos 'asdf:load-op :cffi)
* (push "/Lisp/portmidi/" asdf:*central-registry*)
* (asdf:oos 'asdf:load-op :portmidi)

Look in the file /Lisp/portmidi/test.lisp for a test of the lisp interface to
portmidi. For example, while still running sbcl:

* (pm:portmidi)  ; initialize portmidi
* (pt:start) ; start time
* (pt:time) ; get time
* (pprint (pm:GetDeviceInfo)) ; get list of devices
((:ID 0 :NAME "IAC Driver Bus 1" :TYPE :INPUT :OPEN NIL)
 (:ID 1 :NAME "IAC Driver Bus 1" :TYPE :OUTPUT :OPEN NIL))

Notice that test.lisp assumes MIDI input devices are connected
and uses some hard-wired device numbers, so it may not run
as is without error.

Since test.lisp uses some Common Music calls, I (RBD) wrote a
simpler test, test-no-cm.lisp, which is in the same folder as
this (README_CL.txt) file. To use it, first check that the 
values for outid (4) and inid (1) actually match PortMidi device
id's for output and input devices, and make sure the input
device is a keyboard that can generate a middle-C -- otherwise
the program will hang waiting for input. Run sbcl from this
pm_cl folder, and type:

(load "test-no-cm.lisp")

The program pauses frequently by calling (READ), so you
should type t or something, then <RETURN> to continue.


(Thanks to Leigh Smith and Rick Taube)
