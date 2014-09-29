README for PortAudio Loopback Test

Copyright (c) 1999-2010 Phil Burk and Ross Bencina
See complete license at end of file.

This folder contains code for a single executable that does a standalone test of PortAudio.
It does not require a human to listen to the result. Instead it listens to itself using
a loopback cable connected between the audio output and the audio input. Special pop detectors
and phase analysers can detect errors in the audio stream.

This test can be run from a script as part of a nightly build and test.

--- How to Build the Loopback Test ---

The loopback test is not normally built by the makefile.
To build the loopback test, enter:

  ./configure && make loopback
  
This will build the "bin/paloopback" executable.
  
--- How To Run Test ---

Connect stereo cables from one or more output audio devices to audio input devices. 
The test will scan all the ports and find the cables.

Adjust the volume levels of the hardware so you get a decent signal that will not clip.

Run the test from the command line with the following options:

  -i# Input device ID. Will scan for loopback if not specified.
  -o# Output device ID. Will scan for loopback if not specified.
  -r# Sample Rate in Hz. Will use multiple common rates if not specified.
  -s# Size of callback buffer in frames, framesPerBuffer.
  -w  Save bad recordings in a WAV file.
  -dDir  Path for Directory for WAV files. Default is current directory.
  -m  Just test the DSP Math code and not the audio devices.

If the -w option is set then any tests that fail will save the recording of the broken
channel in a WAV file. The files will be numbered and shown in the report.

--- ToDo ---

* Add check for harmonic and enharmonic distortion.
* Measure min/max peak values.
* Detect DC bias.
* Test against matrix of devices/APIs and settings.
* Detect mono vs stereo loopback.
* More command line options
   --quick
   --latency
   --duration
* Automated build and test script with cron job.
* Test on Windows.


/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Copyright (c) 1999-2008 Phil Burk and Ross Bencina
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */
