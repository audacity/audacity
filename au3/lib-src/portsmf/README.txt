portsmf README.txt
14 Jun 2008
Roger B. Dannenberg

Portsmf is "Port Standard MIDI File", a cross-platform, C++ library
for reading and writing Standard MIDI Files.

License information: free and open source, see license.txt for details

Features:

- input and output of Standard MIDI Files
- data structures, classes, etc. for representing music data in memory
    o sequence structure consisting of multiple tracks
    o track structure consisting of multiple events
    o events contain note and control data
    o extensible attribute-value property lists
    o tempo track and time signature representation
- input and output of a text-based representation: Allegro files
- extensive editing operations on sequences and tracks
- conversion to/from binary buffers for archiving, undo/redo, etc.

Portsmf is a relatively small number of about 9 files, so there is
currently no support for building/maintaining Portsmf as a separate
library. (Contributions are welcome.) For now, it is suggested that
you simply compile these files along with your application sources.

There is a test program in portsmf_test and makefiles to build it as
an example.

You might want to browse through portsmf_test/allegro_test.cpp 
for examples that use and exercise most of the portsmf functions.
