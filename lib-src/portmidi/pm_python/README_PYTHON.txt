PyPortMidi v0.03 03/15/05
Python wrappings for PortMidi
John Harrison
harrison@media.mit.edu

Modified by Roger B. Dannenberg, Nov 2009

PyPortMidi
----------

PyPortMidi is a Python wrapper for PortMidi. PortMidi is a cross-platform
C library for realtime MIDI control. Using PyPortMidi, you can send and
receive MIDI data in realtime from Python.

Besides using PyPortMidi to communicate to synthesizers and the
like, it is possible to use PyPortMidi as a way to send MIDI messages
between software packages on the same computer. For example, Using
PyPortMidi and MIDI-YOKE on a Windows machine, it is possible to send
realtime MIDI messages between programs on the same computer using
loopback virtual MIDI ports. (At this time, MIDI-YOKE does not appear
to run on Windows Vista.)

PyPortMidi is cross-platform, but it will require some small
changes in the setup.py file for it to install correctly on Linux
machines. The changes should be pretty straightforward, and I am
anxious to work with a Linux user on the port.

PyPortMidi works with Python 2.6 and Python 3.1, although the ports
are mostly separate because of various language incompatibilities.

Please see README26.txt for information about the Python 2.6 version.

See README31.txt for information about the Python 3.1 version.

