README.txt -- Nyquist information for Windows 

Installation
------------
The Win32 version of Nyquist is packaged as a compiled (runtime)
system in an executable installer. For most users, the runtime version
contain everything you need to run Nyquist, including the executable,
examples, and documentation, packaged as an executable installer
program. After executing the installer, just find Nyquist in your
Start menu to run it. You may begin typing expressions such as the
ones in the following "Examples" section of the Nyquist manual (in
doc/nyquistman.pdf or doc/home.html).

(See "The 'java is not recognized' Error" below if you get this error
message.)

A source version is also available (the same source download is for
Win32, Mac OS X, and Linux). The source version is intended for
developers who want to recompile Nyquist.  See Win32 Installation
in the reference manual for more instructions.

64-bit Windows
--------------
Nyquist runs on 64-bit Windows and is no longer tested on 32-bit
Windows.

Optional
--------
Nyquist needs to know where to find the standard runtime files. The
location of runtime files must be stored in the Registry. The
installers create a registry entry, but if you move Nyquist or deal
with different versions, you can edit the Registry manually as
follows:

    Run the Registry editor (e.g. type regedit into the Start Search
    box of the Start menu and type the Enter key).

    Find and highlight the SOFTWARE key under HKEY_LOCAL_MACHINE.

    If you are on 64-bit Windows using 32-bit Nyquist, open the
    Wow6432Node key under SOFTWARE.

    Open the CMU key (if it is not there, use the Edit:New:Key menu
    item to create a CMU key. CMU is case sensitive.)

    Highlight the new CMU key.

    Open the Nyquist key (if it is not there, use the Edit:New:Key menu
    item to create a Nyquist key. Nyquist is case sensitive.)

    Highlight the new Nyquist key.

    Find the XLISPPATH string (if it is not there, use the
    Edit:New:String menu item to create a new string and change the
    name by typing XLISPPATH).

    Select XLISPPATH and choose the Edit:Modify... menu item.
    In the String Edit box, type a list of paths you want Nyquist to
    search for lisp files. For example, if you installed Nyquist as
    C:\nyquist, then type:

        C:\nyquist\runtime,C:\nyquist\lib

    The paths should be separated by a comma or semicolon and no
    space. The runtime path is essential, and the lib path may become
    essential in a future release. You can also add paths to personal
    libraries of Lisp and Nyquist code. 

    Click the OK button of the string box and exit from the Registry
    Editor application.


What if Nyquist functions are undefined?
----------------------------------------
If you do not have administrative privileges for your machine, the
installer may fail to set up the Registry entry that Nyquist uses to
find initialization files. In this case, Nyquist will run a lisp
interpreter, but many Nyquist functions will not be defined. If you
can log in as administrator, do it and reinstall Nyquist. If you do
not have permission, you'll have to find an administrator to run
the installer.


SystemRoot
----------
(Ignore this paragraph if you are not planning to use Open Sound
Control under Windows.) 

If Nyquist prints an error message and quits when you enable Open
Sound Control (using osc-enable), check to see if you have an
environment variable SystemRoot, e.g. type set to a command prompt and
look for the value of SystemRoot. The normal value is C:\windows. If
the value is something else, you should put the environment entry, for
example: 

    SystemRoot="D:\windows"

into a file named systemroot (no extension). Put this file in your
nyquist directory. When you run jNyqIDE, it will look for this file
and pass the contents as an environment variable to Nyquist. The
Nyquist process needs this to open a UDP socket, which is needed for
Open Sound Control. 

The window vanishes, "java is not recognized", and other errors
---------------------------------------------------------------
Sometimes Nyquist pops up a window that closes instantly. This
indicates that Java was not found.

Sometimes, Nyquist will run directly from the installer, but then it
will not start from the Windows Start menu. You can try running the
nyquist/jnyqide.bat program from a Windows command prompt (cmd). 
If that fails, and you see an error similar to "java is not recognized 
as in internal or external command error", the problem may be that 
paths are not set up properly to allow the Windows shell to find java. 

Right click on "My Computer" on the Windows desktop and select
"Properties." Under the "Advanced" tap, press the "Environment
Variables" button, and look for PATH under "System Variables." Make
sure the Java bin directory is on the path. If it is not, you will
have to find your installation of Java and add the appropriate
directory to the PATH variable, e.g. "C:\Program
Files\Java\jdk1.7.0\bin."

Another possible problem is that your Java version is not compatible
with Nyquist. In that case, you should see an error message 
complaining about "Unsupported major.minor version...". The current
major.minor version is 51 and you need to have Java version 10
installed. (Use the command: java -version to find out your java
version number.)

You might have to reboot for these changes to take effect.

Compile Nyquist using Visual Studio
---------------------------------------
Nyquist can be compiled using Visual Studio.

To make a VS solution file, get CMake and run it on CMakeLists.txt in
the nyquist directory.

In Visual Studio,
    Set solution configuration to "Release" and solution platforms
    to "Win64".
        Build Solution
        Start Debugging

To build jnyqide, 
    Check if you have installed JDK and have the directory to
    javac.exe added to your system PATH. 
    Run comp-ide.bat under the project root. It will compile .java
    files under jnyqide.
    For debugging, you can run the IDE immediately. 
        First copy nyquist\WinRel\nyquist.exe to nyquist\ (only do
                this each time you recompile nyquist.)
        Then, run jnyqide.bat.
        To generate a release:
        Run releasenyqide.bat under the project root. It will copy
                all the necessary files to .\nyqrelide\. You can run
                jnyqide.bat from there. 
