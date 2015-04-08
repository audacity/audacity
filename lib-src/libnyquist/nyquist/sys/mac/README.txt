README.txt -- information on Nyquist for Mac OS X 

Installation
------------
The simplest way to install and run Nyquist is to get the pre-compiled
NyquistIDE application, which includes executables, documentation, and
libraries all in one package.

When the pre-compiled package is uncompressed, it produces two directories:
     nyquist
     NyquistIDE.app
You should always keep these together. The nyquist directory initially 
contains documentation, but when NyquistIDE.app is run, the nyquist
folder is also populated with links to the lib and demos folders,
which contain example code and more documentation. The symbolic links
allow you to easily find these files.

You can copy both nyquist and NyquistIDE.app to the /Applications folder
if you wish, and you can add NyquistIDE.app to the Dock.

You will probably run Nyquist using the NyquistIDE application, but
you can also run nyquist from the command line. The executable is
located in

    NyquistIDE.app/Contents/Resources/Java/ny

To run from the command line, you will need to set the XLISPPATH
environment variable using this command line (if you use the C shell,
e.g. csh):

    setenv XLISPPATH `pwd`/runtime:`pwd`/lib

If you use the bash shell, use:

    export XLISPPATH=`pwd`/runtime:`pwd`/lib

Note that this sets XLISPPATH in the environment of the current
command line shell. If you exit the shell or switch to another shell,
the XLISPPATH variable will not be set. Your shell reads an
initialization file when it starts. You can add the XLISPPATH
initialization command to this file if you want the variable to be set
automatically in every instance of your command line shell.

On the topic of the XLISPPATH, note that this variable is set by
NyquistIDE when running with that application, overriding any other
value. You can extend the search path by creating the file xlisppath
in the same directory as the nyquist executable ny. The xlisppath file
should have colon-separated paths on a single line of text.

You can also build Nyquist from sources, as described below.


How To Build Nyquist on Mac OS X
--------------------------------
You need to install Xcode, Apple's free software development system
for OS X.

Xcode 4.3.2 and later
-------
The nyquist project for Xcode v4 is in nyquist/macosxproject/nyquist.xcodeproj

To build Nyquist or NyquistIDE:
 - Open nyquist.wxworkspace in Xcode
 - Set the active target to "Nyquist" or "NyquistIDE"
 - Click on "build active target"
 - ny or NyquistIDE will be produced in a temporary folder managed by Xcode.
   To find the files Right click on the Products/NyquistIDE.app of the project 
   navigator, and select "reveal in finder". 

