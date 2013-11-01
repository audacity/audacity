README_QT.txt for PortMidi
Richard Starfield
20 Sep 2010

This is a QtCreator build file for PortMidi.

To build PortMidi on Windows with QtCreator:

Move portmidi/pm_qt/portmidi.pro to portmidi/portmidi.pro

Open portmidi.pro in QtCreator, change to the release build option and build all to compile the static library.

This has been tested in Windows. The project file does include Linux build switches but they haven't been tested yet.

To compile a DLL instead of a static library change line 11 from "CONFIG += staticlib" to "CONFIG += DLL"
