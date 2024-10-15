README.txt -- Nyquist information for Unix systems

UNIX INSTALLATION
=================
For Unix systems, Nyquist is distributed as a compressed file of
sources named nyqsrc3<nn>.zip, where <nn> is the version number
(e.g. v3.01 was in nyqsrc301.zip).  To install Nyquist, copy
nyqsrc3<nn>.zip) to the directory on your machine where you would
like to install Nyquist.

Note 1: you will need the "normal tool chain" consisting of the Gnu
C/C++ compiler, linker, C/C++ runtime libraries, autoconf, libtool,
automake, etc. Most linux installations already have this, but some 
more recent trimmed-down installations for netbooks and
consumer-oriented computers do not have compilers installed by
default.

Note 2: There are two main unix versions of Nyquist: alsa and nonalsa.
The alsa version is probably what you want. This version uses ALSA,
the Linux audio system. This has also become standard, but your
machine might not have the ALSA development package (probably named
libasound2-dev), so you might have to install it. If you find you are
missing "asound", you are missing and need to install the ALSA
developmnent package. The nonalsa version is a special version for 
Debian linux. The ONLY difference is that it omits -lasound from the
link step, so it does not try to link with ALSA. I assume this works
because the PortAudio library which is included in the Nyquist sources
configures itself differently on Debian and doesn't need ALSA.

Note 3: You will also need Java and (maybe) ant

Note 4: Nyquist has recently switched from a
home-brew makefile system to CMake. You'll need to install cmake if
you do not have it. At present, the cmake files work for Windows and
OS X, but there are likely to be some problems with Linux that need
to be solved.

Unzip sources (e..g use the Archive Manager), creating a nyquist 
directory and some subdirectories, and use cd to change the current
directory:

    cd nyquist (or "cd <path to the nyquist directory>")

Build Nyquist with cmake and make:

    ccmake .
        change configuration to Release
        type c to configure, g to generate and exit
    make
    
Set the search path (in bash), which tells Nyquist where to search
for lisp files to be loaded when a file is not found in the current
directory. See SHELL STARTUP below for information about how to
automate this.

    export XLISPPATH=`pwd`/runtime:`pwd`/lib 

(Alternatively, tcsh users can type
    setenv XLISPPATH `pwd`/runtime:`pwd`/lib 
)

64-BIT UBUNTU
=============
if xlisp/extern.c fails to compile because of a missing bits/predefs.h,
    try using synaptic to update libc6 and libc6-i386 and libc6-dev-i386, or
    try sudo apt-get install libc6-dev-i386

you may need to install nasm
you may need to install build-essential and g++
you may need to install lib32asound2-dev
you may need to install lib32stdc++
you may need to install g++multilib
you may need to install libogg-dev
you may need to install libvorbis-dev
you may need to install Java (perhaps as follows):
    download jdk-7u2-linux-x64.tar.gz from
        http://www.oracle.com/technetwork/java/javase/downloads/jdk-7u2-download-1377129.html
    tar xfvz jdk-7u2-linux-x64.tar.gz
    sudo mv ./jdk1.7.0_02 /usr/lib/jvm/jdk1.7.0    
    sudo update-alternatives --install "/usr/bin/java" "java" "/usr/lib/jvm/jdk1.7.0/bin/java" 1
    sudo update-alternatives --install "/usr/bin/javac" "javac" "/usr/lib/jvm/jdk1.7.0/bin/javac" 1
    sudo update-alternatives --install "/usr/bin/javaws" "javaws" "/usr/lib/jvm/jdk1.7.0/bin/javaws" 1
    sudo update-alternatives --config java
    java -version [check that output is 1.7.0_02"]
    sudo update-alternatives --config javac
    sudo update-alternatives --config java



RUNNING NYQUIST FROM THE COMMAND LINE
=====================================
Assuming the make completes successfully, you can run Nyquist as follows:
    ./ny
When you get the prompt, you may begin typing expressions such as
the ones in the following "Examples" section in the Nyquist
manual. (See doc/nyquistman.pdf or doc/home.html).

RUNNING NYQUIST USING NyquistIDE
=====================================
One you establish that Nyquist (ny) is working from the command line,
you should try using NyquistIDE, the Java-based Nyquist development
environment. First, make jny executable (do this only once when you
install Nyquist):
    chmod +x jny
Then try running jNyqIDE by typing:
    ./jny

If the NyquistIDE window does not appear, make sure you have Java
installed (if not, you probably already encountered errors when you
ran the make command.) You can also try recompiling the Java
files. Note that jnyqide/SpecialMacHandler.java may NOT compile
under non-OS X systems. The Makefile renames this file to "hide" it
from the Java compiler, compiles all the remaining java files, and
then restores jnyqide/SpecialMacHandler.java:
    make jnyqide/jNyqIDE.jar

NYQUIST SEARCH PATH UNDER NyquistIDE
====================================
Note: With Linux and Mac OS X, jNyqIDE defines the environment passed
to Nyquist. If you set XLISPPATH as shown above, it will be passed
along to Nyquist under jNyqIDE. If not, a default XLISPPATH will have
the lib and runtime directories only. This does not apply to Windows
because even though the environment is there, the Windows version of
Nyquist reads the XLISPPATH from the Registry. 

MORE DETAILS
============
It is good to have USER in the environment with your user ID. This
string is used to construct some file names. NyquistIDE will look for it
in the environment. You can also specify your user ID using the file
nyquist/user, but if you have a shared installation of Nyquist,
this will not be very useful.

Note: Nyquist looks for the file init.lsp in the current directory.
If you look in the init.lsp in runtime, you will notice two things.
First, init.lsp loads nyquist.lsp from the Nyquist directory, and
second, init.lsp loads system.lsp which in turn defines the macro
play.  Normally, Nyquist plays audio through the PortAudio library,
which should work on any system. An alternative is to save audio to a
file and invoke a local non-Nyquist program to play the sound file.
You can modify system.lsp to accomplish this.

SHELL STARTUP
=============
The runtime directory should always be on your XLISPPATH when you
run Nyquist, so you may want to set XLISPPATH in your shell startup
file, e.g. .cshrc.

Which shell are you using?  echo $SHELL will tell you. If you use
/bin/bash, your startup file is probably ~/.profile. (Remember that
"~/" means your home directory, so the file will be something like
/home/rbd/.profile).  In this file, you can add a line such as:

export XLISPPATH="/home/rbd/nyquist/runtime:/home/rbd/nyquist/lib"

Do not use the shorthand `pwd`/runtime, because `pwd` returns the
current working directory, which is not going to be your Nyquist
directory when .profile is loaded.

If you use /bin/csh (the C Shell), your startup file is probably
~/.cshrc. (Remember that "~/" means your home directory, so the file
will be something like /home/rbd/.cshrc).  In this file, you can add
a line such as:

setenv XLISPPATH "/home/rbd/nyquist/runtime:/home/rbd/nyquist/lib"

