File: PortMidi Win32 Readme
Author: Belinda Thom, June 16 2002
Revised by: Roger Dannenberg, June 2002, May 2004, June 2007, 
            Umpei Kurokawa, June 2007
            Roger Dannenberg Sep 2009

Contents:
        Using Portmidi
        To Install Portmidi
        To Compile Portmidi
        About Cmake
        Using other versions of Visual C++
        To Create Your Own Portmidi Client Application
        


=============================================================================
USING PORTMIDI:
=============================================================================

Using Microsoft Visual C++ project files (provided with PortMidi), there
are two configurations of the PortMidi library. The Debug version is 
intended for debugging, especially in a console application. The Debug
version enables some extra error checking and outputs some text as well
as a prompt to type ENTER so that you don't lose any debugging text when
the program exits. You can turn off this extra debugging info by taking
out the compile-time definition for DEBUG. (But leave _DEBUG, which I
think is important for compiling in Debug mode.) This debugging version also
defines PM_CHECK_ERRORS, which forces a check for error return codes from
every call to PortMidi. You can disable this checking (especially if you
want to handle error codes in your own way) by removing PM_CHECK_ERRORS
from the predefined symbols list in the Settings dialog box.

PortMidi is designed to run without a console and should work perfectly 
well within a graphical user interface application. The Release version
is both optimized and lacking the debugging printout code of the Debug
version.

Read the portmidi.h file for PortMidi API details on using the PortMidi API.
See <...>\pm_test\test.c and other files in pm_test for usage examples.

=============================================================================
TO INSTALL PORTMIDI:
=============================================================================
1)  get current source from the portmedia project at SourceForge.net

2)  copy source into directory: <...>\portmidi

=============================================================================
TO COMPILE PORTMIDI:
=============================================================================

3)  cd to or open the portmidi directory

4)  start or click on the portmidi.sln workspace (note, all Visual Studio
    files are built by CMake. If you need a different version or have
    problems with paths, try rebuilding the Visual Studio project files
    using CMake -- See "Using other versions of visual C++" below.)
	
5)  the following projects exist within this workspace:
    - portmidi-static, portmidi-dynamic (versions of the PortMidi library)
    - test (simple midi I/O testing)
    - midithread (an example illustrating low-latency MIDI processing
        using a dedicated low-latency thread)
    - sysex (simple sysex message I/O testing)
    - latency (uses porttime to measure system latency)
    - midithru (an example illustrating software MIDI THRU)
    - qtest (a test of the new multicore-safe queue implementation)
    - mm  (allows monitoring of midi messages)
    - pmjni (a dll to provide an interface to PortMidi for Java)

6)  set the Java SDK path using one of two methods:
    Method 1: open portmidi/CMakeLists.txt with CMake, configure, and 
        generate -- this should find the Java SDK path and update your
            solution and project files
    Method 2: (does not require CMake):
        - open the pmjni project properties
        - visit Configuration Properties, C/C++, General
        - find Additional Include Directories property and open the editor (...)
        - at the end of the list, you will find two paths mentioning Java
        - these are absolute paths to the Java SDK; you'll need to install the
          Java SDK (from Sun) and update these directories in order to build
          this project.
        - similarly, the Linker->Input->Additional Dependencies list has a
          path to the jvm.lib file, which needs to be correct(ed).

6)  use Build->Batch Build ... to build everything in the project. If a 
    build fails, try building again. There seem to be some missing 
    dependencies, so you may have to "ALL_BUILD" several times before
    everything builds successfully.
	
7)  The settings for these projects were distributed in the zip file, so
    compile should just work.

8)  run test project; use the menu that shows up from the command prompt to
    test that portMidi works on your system. tests include: 
		- verify midi output works
		- verify midi input works

9) run other projects if you wish: sysex, latency, midithread, mm, 
    qtest, midithru

10) compile the java code:
    - cd pm_java
    - make.bat
        + If there is a problem running javac, note that you must have
          a path to javac.exe on your PATH environment variable. Edit
          your path (in Vista) using Control Panel > User Accounts > 
          User Accounts > Change my environment variables; then select
          Path and click Edit... After changing, you will have to 
          restart the command window to see any effect.
        + In Vista, you may get a warning about running 
          UpdateRsrcJavaExe.exe. This is called by make.bat, and you
          should allow the program to run.
        + Note that make.bat does not build pmjni\jportmidi_JPortMidiApi.h
          because it is included in the distribution. You can rebuild it 
          from sources as follows:
              cd pm_java
              javah jportmidi.JPortMidiApi
              move jportmidi_JPortMidiApi pmjni\jportmidi_JPortMidiApi.h
       
11) you might wish to move pm_java/win32 to another location; run the
    pmdefaults.exe program from the (entire) win32 directory to use 
    PmDefaults. This program let's you select default input/output 
    midi devices for PortMidi applications.

============================================================================
ABOUT CMAKE
============================================================================

cmake was used to generate .vcproj files. cmake embeds absolute paths
into .vcproj files, which makes the files non-portable to other systems.
To work around this problem, pm_win\clean_up_vcproj.bat can be used to 
replace absolute paths with relative paths. To use it, you will need to
install gawk and set your search path to allow you to execute gawk, e.g. 
my path includes "C:\Program Files\GnuWin32\bin;". You will also need to
edit pm_win\clean_up_vcproj.awk, replacing C:\Users\rbd\portmidi with
whatever absolute path cmake uses in your vcproj files.

This is not a general or robust fix, but it seems to work with the 
vcproj files currently created by CMake.

============================================================================
USING OTHER VERSIONS OF VISUAL C++
============================================================================

You can use cmake to make Visual Studio solution and project files.
If you do not want to use the provided Version 9 project files, install
cmake, run it, set the "Where is the source code" box to your portmidi
directory, and click on Configure. A menu will allow you to choose the
Visual Studio project version you want. Click Configure once again, then
Generate, and you should be all set to open portmidi.sln.

============================================================================
TO CREATE YOUR OWN PORTMIDI CLIENT APPLICATION:
============================================================================

NOTE: this section needs to be reviewed and tested. My suggestion would
be to copy the test project file (test.dsp) and modify it. -RBD

The easiest way is to start a new project w/in the portMidi workspace:

1) To open new project: 
	- File->New->Projects
	- Location: <...>\portmidi\<yourProjectName>
	- check Add to current workspace
	- select Win32 Console Application (recommended for now)
	- do *NOT* select the "make dependency" box (you will explicitly do this
      in the next step)
	- Click OK
	- Select "An Empty Project" and click Finish
	
	In Visual C++ 2005 Express Edition, 
	- File->New->Projects
	- Location: <...>\portmidi\<yourProjectName>
	- select Add to solution
	- select CLR Empty project in CLR
	- select Win32 Console Application in Win32
	- select Empty project in General
	
2) Now this project will be the active project. Make it explicitly depend
   on PortMidi dll:
	- Project->Dependencies
	- Click pm_dll

3) add whatever files you wish to add to your new project, using portMidi
   calls as desired (see USING PORTMIDI at top of this readme)

4) when you include portMidi files, do so like this:
	- #include "..\pm_common\portmidi.h"
	- etc.

5) build and run your project

============================================================================
DESIGN NOTES
============================================================================

PortMidi for Win32 exists as a simple static library,
with Win32-specific code in pmwin.c and MM-specific code in pmwinmm.c.

Orderly cleanup after errors are encountered is based on a fixed order of
steps and state changes to reflect each step. Here's the order:

To open input:
    initialize return value to NULL
    - allocate the PmInternal strucure (representation of PortMidiStream)
    return value is (non-null) PmInternal structure
    - allocate midi buffer
    set buffer field of PmInternal structure
    - call system-dependent open code
        - allocate midiwinmm_type for winmm dependent data
        set descriptor field of PmInternal structure
        - open device
        set handle field of midiwinmm_type structure
        - allocate buffers
        - start device
        - return
    - return

SYSEX HANDLING -- the most complex, least exercised, and therefore most
      buggy part of PortMidi (but maybe bugs are finally gone)

There are three cases: simple output, stream output, input
Each must deal with:
 1. Buffer Initialization (creating buffers)
 2. Buffer Allocation (finding a free buffer)
 3. Buffer Fill (putting bytes in the buffer)
 4. Buffer Preparation (midiOutPrepare, etc.)
 5. Buffer Send (to Midi device)
 6. Buffer Receive (in callback)
 7. Buffer Empty (removing bytes from buffer)
 8. Buffer Free (returning to the buffer pool)
 9. Buffer Finalization (returning to heap)

Here's how simple output handles sysex:
 1. Buffer Initialization (creating buffers)
  allocated when code tries to write first byte to a buffer
  the test is "if (!m->sysex_buffers[0]) { ... }"
  this field is initialized to NULL when device is opened
  the size is SYSEX_BYTES_PER_BUFFER
  allocate_sysex_buffers() does the initialization
  note that the actual size of the allocation includes
      additional space for a MIDIEVENT (3 longs) which are
      not used in this case
 2. Buffer Allocation (finding a free buffer)
  see get_free_sysex_buffer()
  cycle through m->sysex_buffers[] using m->next_sysex_buffer
      to determine where to look next
  if nothing is found, wait by blocking on m->sysex_buffer_signal
  this is signaled by the callback every time a message is
      received
 3. Buffer Fill (putting bytes in the buffer)
  essentially a state machine approach
  hdr->dwBytesRecorded is a position in message pointed to by m->hdr
  keep appending bytes until dwBytesRecorded >= SYSEX_BYTES_PER_BUFFER
  then send the message, reseting the state to initial values
 4. Buffer Preparation (midiOutPrepare, etc.)
  just before sending in winmm_end_sysex()
 5. Buffer Send (to Midi device)
  message is padded with zero at end (since extra space was allocated
      this is ok) -- the zero works around a bug in (an old version of)
      MIDI YOKE drivers
  dwBufferLength gets dwBytesRecorded, and dwBytesRecorded gets 0
  uses midiOutLongMsg()
 6. Buffer Receive (in callback)
 7. Buffer Empty (removing bytes from buffer)
  not applicable for output
 8. Buffer Free (returning to the buffer pool)
  unprepare message to indicate that it is free
  SetEvent on m->buffer_signal in case client is waiting
 9. Buffer Finalization (returning to heap)
  when device is closed, winmm_out_delete frees all sysex buffers

Here's how stream output handles sysex:
 1. Buffer Initialization (creating buffers)
  same code as simple output (see above)
 2. Buffer Allocation (finding a free buffer)
  same code as simple output (see above)
 3. Buffer Fill (putting bytes in the buffer)
  essentially a state machine approach
  m->dwBytesRecorded is a position in message
  keep appending bytes until buffer is full (one byte to spare)
 4. Buffer Preparation (midiOutPrepare, etc.)
  done before sending message
  dwBytesRecorded and dwBufferLength are set in winmm_end_sysex
 5. Buffer Send (to Midi device)
  uses midiStreamOutMsg()
 6. Buffer Receive (in callback)
 7. Buffer Empty (removing bytes from buffer)
  not applicable for output
 8. Buffer Free (returning to the buffer pool)
  unprepare message to indicate that it is free
  SetEvent on m->buffer_signal in case client is waiting
 9. Buffer Finalization (returning to heap)
  when device is closed, winmm_out_delete frees all sysex buffers


Here's how input handles sysex:
 1. Buffer Initialization (creating buffers)
  two buffers are allocated in winmm_in_open
 2. Buffer Allocation (finding a free buffer)
  same code as simple output (see above)
 3. Buffer Fill (putting bytes in the buffer)
  not applicable for input
 4. Buffer Preparation (midiOutPrepare, etc.)
  done before sending message -- in winmm_in_open and in callback
 5. Buffer Send (to Midi device)
  uses midiInAddbuffer in allocate_sysex_input_buffer (called from
      winmm_in_open) and callback
 6. Buffer Receive (in callback)
 7. Buffer Empty (removing bytes from buffer)
      done without pause in loop in callback
 8. Buffer Free (returning to the buffer pool)
  done by midiInAddBuffer in callback, no pointer to buffers
      is retained except by device
 9. Buffer Finalization (returning to heap)
  when device is closed, empty buffers are delivered to callback,
      which frees them

IMPORTANT: In addition to the above, PortMidi now has
"shortcuts" to optimize the transfer of sysex data. To enable
the optimization for sysex output, the system-dependent code
sets fields in the pmInternal structure: fill_base, fill_offset_ptr,
and fill_length. When fill_base is non-null, the system-independent
part of PortMidi is allowed to directly copy sysex bytes to
"fill_base[*fill_offset_ptr++]" until *fill_offset_ptr reaches
fill_length. See the code for details.

-----------

Additional notes on using VS 2005 (maybe this is obsolete now?):

1) Make sure "Configuration: All Configurations" is selected in all of the following Properties modifications!

2) In my case the project defaulted to compiling all .c files with the C++ compiler, which was disastrous. I had to go to set Properties for each file, to wit: Expand Configuration Properties, Expand C/C++, Select Advanced, set the Compile As popup to Compile as C Code (/TC). (For better or worse, the project I inherited has a bunch of .c files that rely on C++ features, so I couldn't reliably set this the project properties level.)

3) While you're there, make sure that the C/C++ -> General -> "Compile with Common Language Runtime support" is set to "No Common Language Runtime support" (the C compiler *can't* support CLR, but VS won't do anything useful like automatically set the two options to match)-.

4) I never got VS precompiled header thing to work sensibly, so I took the path of least resistance and turned PCH's off for all my files. Properties -> Configuration Properties -> C/C++ -> Precompiled Headers -> Create/Use Precompiled Header popup set to "Not Using Precompiled Headers". The compiler is reasonably fast even if it has to parse all the header files, so unless someone wants to explain VS's PCHs to me, the hell with it, I say.

