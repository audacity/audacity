scorealign -- a program for audio-to-audio and audio-to-midi alignment

Last updated 10 May 2013 by RBD

Contributors include: 
             Ning Hu
             Roger B. Dannenberg
             Joshua Hailpern
             Umpei Kurokawa
             Greg Wakefield
             Mark Bartsch
 
scorealign works by computing chromagrams of the two sources. Midi chromagrams
are estimated directly from pitch data without synthesis. A similarity matrix
is constructed and dynamic programming finds the lowest-cost path through the
matrix.

The alignment can optionally skip the initial silence and final silence 
frames in both files. The "best" path matches from the beginning times
(with or without silence) to the end of either sequence but not
necessarily to the end of both. In other words, the match will match
all of the first file to an initial segment of the second, or it will
match all of the second to an initial segment of the first.

Output includes a map from one version to the other. If one file is MIDI, 
output also includes (1) an estimated transcript in ASCII format with time, 
pitch, MIDI channel, and duration of each notes in the audio file, (2) a
time-aligned midi file, and (3) a text file with beat times.

scorealign uses libsndfile (http://www.mega-nerd.com/libsndfile/). You must
install libsndfile to build scorealign.

For Macintosh OS X, use Xcode to open scorealign.xcodeproj
For Linux, use "make -f Makefile.linux"
For Windows, open scorealign-vc2010.sln (This is set up to use my locally built
copies of libsndfile, libogg, libvorbis, and libFLAC. These are such a pain on
Windows, that I actually used a different Visual C++ solution file for Nyquist
that includes projects to build all these libraries. You can find Nyquist on
SourceForge, or you can build the libraries some other way. Note that my
projects are set up to use 8-bit ASCII rather than Unicode or other.)

Command line parameters:

scorealign [-<flags> [<period> <windowsize> <path> <smooth> 
           <trans> <midi> <beatmap> <image>]] 
                 <file1> [<file2>]
   specifying only <file1> simply transcribes MIDI in <file1> to  
   transcription.txt. Otherwise, align <file1> and <file2>.
   Flags are all listed together, e.g. -hwrstm, followed by filenames
   and arguments corresponding to the flags in the order the flags are
   given. Do not try something like "-h 0.1 -w 0.25" Instead, use
   "-hw 0.1 0.25". The flags are:
   -h 0.25 indicates a frame period of 0.25 seconds
   -w 0.25 indicates a window size of 0.25 seconds. 
   -r indicates filename to write raw alignment path to (default path.data)
   -s is filename to write smoothed alignment path(default is smooth.data)
   -t is filename to write the time aligned transcription 
      (default is transcription.txt)
   -m is filename to write the time aligned midi file (default is midi.mid)
   -b is filename to write the time aligned beat times (default is beatmap.txt)
   -i is filename to write an image of the distance matrix 
         (default is distance.pnm)
   -o 2.0 indicates a smoothing window of 2.0s
   -p 3.0 means pre-smooth with a 3s window
   -x 6.0 indicates 6s line segment approximation
   
A bit more detail:

The -o flag (smoothing) controls a post-process on the path. Since the
path is discrete, it will have small jumps ahead or pauses whenever it
differs from the diagonal. A linear regression is performed at each frame
using a set of points whose size is determined by the -o parameter, and the
discrete time indicated by the path is replaced by a continuous time estimated
from neighboring points. This smooths out local irregularities in the time
map.

The -p flag (presmoothing) operates on the discrete path. It tries to fit a 
straight line segment (length is set by -p) to the path. If the path fits
well to the first half of the path and the second half of the path, the 
entire path is replaced with a straight line approximation. To "fit well",
half of the path points must fall very close to the straight line (currently,
within 1.5 frames). For example, if the line segment spans 40 frames, then 10
path points must be close to the first 20 frames and 10 path points must be 
close to the last 20 frames. The step is repeated on overlapping windows
through the whole piece. This presmoothing step is designed to detect
places where dynamic programming "wanders off" from the true path and then
realigns to the true path. The off-track points are replaced, so they do not
adversely affect the smoothing step. This approach does not seem to be 
robust, but sometimes works well.

The -x flag is another approach to deal with dynamic programming errors. It
divides the entire piece into segments whose lengths are about equal and about
the length specified by the -x parameter. The line segments are fit to the
path by linear regression, and their endpoints are joined by averaging their
linear regression values. Next, a hill-climbing search is performed to 
minimize the total distance along the path. This is like dynamic programming
except that each line spans many frames, so the resulting path is forced to 
be fairly straight. Linear interpolation is used to estimate chroma distance
since the lines do not always pass through integer frame locations. This 
approach is probably good when the audio is known to have a steady tempo or 
be performed with tempo changes that match those in the midi file.

Some notes on the software architecture of scorealign:

scorealign was originally implemented as a fairly monolithic program
in MatLab. It was ported to C++. To incorporate this code into Audacity,
the code was restructured so that audio input is obtained from
Audio_reader, an abstract class that calls on a subclass to implement
read(). The subclass just copies floats into the provided buffer. It is
responsible for sample format conversion, stereo-to-mono conversion, etc.
The Audio_reader returns possibly overlapping buffers of floats. The
Audio_file_reader subclass uses libsndfile to read in samples and convert
them to float. It does its own conversion to mono.

When scorealign is used in Audacity, a different subclass of Audio_reader
will call into Audacity using a Mixer object to retrieve samples from
selected tracks.

For use from the command line, scorealign has a module main.cpp that 
parses command line arguments. A lot of parameters and options that 
were formerly globals are now stored in a Scorealign object that is
passed around to many routines and methods. main.cpp creates a (global)
Scorealign object and uses code in the module alignfiles.cpp to do the
work. The purpose of alignfiles is to provide an API that does not 
depend upon a command line interface, but which assumes you are aligning
files. Finally, alignfiles.cpp uses an Audio_file_reader to offer
samples to the main score alignment algorithm.

To summarize:
   scorealign.cpp and gen_chroma.cpp do most of the pure alignment work
   audioreader.cpp abstracts the source of audio, whether it comes from
      a file or some other source
   alignfiles.cpp opens files and invokes the modules above
   main.cpp parses the command line and invokes alignfiles.

