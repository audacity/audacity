These files, dot-cproject and dot-project, come from Philip Martin
(posted on Media_api mailing list on September 3, 2008).
He writes: "I do not have a makefile. I used Eclipse + CDT to build
PortMidi using MinGw. ... The two files .project and .cproject I 
believe must be in the root of the portmidi tree to work with Eclipse.
... I have only compiled the relevant sources into one .dll here."

The .project and .cproject files have been renamed to dot-project and
dot-cproject here to make them more visible. To use them, you will
need to rename them to .project and .cproject, and probably move them
to the portmidi tree root.

At this time, no one is actively maintaining Eclipse or MinGw versions
of PortMidi, so these files may be out-of-date or have other problems.
Feel free to submit updates or discuss the possibility of maintaining
these or some equivalent files for MinGw.

Update, 20 Sep 2010: CMake supports Code::Blocks in conjunction with
MinGW.

Roger Dannenberg
18-Oct-2008

