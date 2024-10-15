README.txt -- information on Nyquist for Mac OS X 

IMPORTANT
---------
You may get the message 
    "NyquistIDE is damaged and can't be opened. 
    You should move it to the Trash from the Finder."
This appears to be a deliberately incorrect statement from Apple.
Here is what to do:

1) Go to Preferences, Security & Privacy
2) Unlock (requires password)
3) Allow apps downloaded from: Anywhere
4) Control-click on the NyquistIDE icon and select open
5) Tell the Finder yes, you really want to open this application
6) Nyquist should run
7) Close Nyquist (maybe not necessary)
8) Now, you can go back to Security & Privacy and revert back 
    to the original policy, e.g. "Mac App Store and identified
    developers" if you wish (a reasonable precaution)
9) Lock the settings in Security & Privacy (click the lock icon)
10) Test: You should now be able to click to start Nyquist; it 
    should start without any false claims about file damage or 
    even warning messages.

Installation
------------
The simplest way to install and run Nyquist is to get the pre-compiled
NyquistIDE application, which includes executables, documentation, and
libraries all in one package and one directory:
     NyquistIDE.app, and
     nyquist
Typically, put NyquistIDE.app in /Applications and nyquist in your
home directory, but anywhere is fine. If nyquist is not copied to your
home directory, NyquistIDE.app will prompt you to find it when you 
run NyquistIDE.app for the first time.

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
value. 

You can also build Nyquist from sources, as described below.


How To Build Nyquist on Mac OS X
--------------------------------
You need to install Xcode, Apple's free software development system
for OS X, and CMake for OS X. Use CMake to build the xcode project.
Get the sources from SourceForge, and run CMake on
nyquist/CMakeLists.txt.
