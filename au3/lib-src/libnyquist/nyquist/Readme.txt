README file for Nyquist Version 3.16
13 Oct 2020
Roger B. Dannenberg

LICENSE: see license.txt
WEB SITE: http://www.cs.cmu.edu/~music/nyquist

INSTALLING NYQUIST
====================
You can download pre-compiled versions for Windows and OS X.
After downloading, please see
      +---------------------------------------+
      | http://www.cs.cmu.edu/~music/nyquist/ |
      +---------------------------------------+
for additional information, or look for one of
      - nyquist/doc/readme-win.txt
      - nyquist/doc/readme-mac.txt
      - nyquist/doc/readme-linux.txt

You can compile Nyquist from sources for Windows, OS X, linux,
and other versions of Unix. For details, see one of these files:
 - sys/win/README.txt
 - sys/mac/README.txt
 - sys/unix/README.txt


IMPLEMENTATION STATUS
=====================

Version 3.16 provides:
    Long sounds: sample counts are consistently 64-bit.
    Details in documentation on avoiding accumulating computed samples in memory
       some fixes in code to eliminate the same.
    Win32 32-bit implementation makes Nyquist FIXNUMs 64-bit.
Version 3.15 provides:
    Fix to retain selected nyquist directory
    Fix for NyquistIDE in paths with spaces
    Extension list file can be selected in Preferences for testing
Version 3.14 provides:
    New runtime configuration: a nyquist directory in user's home
        directory has all documentation, libraries, extensions,
        and only executables are "hidden" and read-only.
Version 3.13 provides:
    Extension Manager and many examples moved to extensions
Version 3.12 provides:
    Fix for Help:Manual and links to docs on OS X 12 (I hope)
    Fix for reverb function (3.11 broke it)    
    Better type checking for OSC table parameter
Version 3.11e provides:
    Works again as 64-bit executable on Windows 10
Version 3.11b provides:
    fix to FFT, apparently only affects only 64-bit Windows
    improved formatting of reference manual (PDF) and minor
        documentation changes
Version 3.11 provides:
    Significantly more parameter checking and error reporting for
        Nyquist primitives.
Version 3.10 provides:
    phasevocoder -- new phase vocoder function
    pv-pitch-time -- phase vocoder + resampling for independent pitch
        and speed control
    convolve -- new fast convolution
    interactive control of sounds with make-slider-panel,
        close-slider-panel, make-slider, make-button, slider,
        lpslider, get-slider-value, snd-slider, stop-on-zero, trigger,
        snd-stop-on-zero, and snd-trigger functions
    bug fix to snd-down (called by force-srate) that could crash Nyquist
    adjustable upper limit on memory allocated for sounds, see
        snd-set-max-audio-mem function
Version 3.09 provides:
    Various bug fixes
    OS X version is significantly faster
    Security features added to limit CPU, memory, file access (default
        is still unrestricted; this feature is to protect servers)
Version 3.08 provides:
    NyquistIDE opens documentation properly on default browser
    NyquistIDE forcefully terminates nyquist process on exit if needed
    bug fix for mixed sample rate signal handling, affects many primitives
    timed-seq and score-play avoid stack overflow on finely spaced events
    sampler() primitive does error checking to avoid infinite loop
Version 3.07 provides:
    Bug fixes in NyquistIDE Envelope Editor
    NyquistIDE installs symbolic links to lib and demos
      directories from nyquist directory (where documentation
      is kept) so these folders are not hidden in the app bundle.
    Bug fix in quantize and snd-quantize functions. 
Version 3.06 provides:
    64-bit architecture support
    Access to OGG and FLAC file formats
    Updates for MSVC++2010, VS2012, XCode 4.5
    This is a deadline-driven release. Expect an update soon.
Version 3.05 provides:
    New "UPIC Editor" window in NyquistIDE
    Fix to escape backslashes in default windows directory
    Fix to other problems with Preferences
    Arpeggiator example in nyquist/demos
Version 3.04 provides:
    Updates to libraries, including liblo and PortAudio
    Documentation uses both syntax SAL and Lisp syntax
    Some STK instruments have been added
    Build files modified to make 32-bit code even on 64-bit 
      architectures (Nyquist only runs in 32-bit mode)
Version 3.03 provides:
    Bug fix to Markov pattern generator (see make-markov).
    Update to current (24-feb-09) liblo library.
    Slight change to license.txt to comply wiht two LGPL
      library licenses: libsndfile and liblo.
    score-sort can sort very big scores now using iterative
      merge sort
Version 3.02 provides:
    Uses libsndfile and recent version of portaudio.
    Many bug fixes.
    Support & compatibility for Algorithmic Composition (to appear)
Version 3.01 provides:
    Feedback FM: see fmfb, snd-fmfb and snd-fmfbv
    fixed help functions and internal browser window
    Documentation mostly using SAL syntax now
Version 3.00 provides:
    First release supporting SAL syntax
    Major revision to documentation (but more to come)
    Bug fixes for sustain transformation
    Many new STK instruments ported by Pedro Morales
    Pedro's sdl music input language
Version 2.38 provides:
    improved PWL editor
    improved preferences dialog
    bug fixes in Equalizer editor
    additional documentation for demos/plight/drums.lsp
    option click or right click on completion list to get help
    manual can be displayed in an internal window in NyquistIDE
Version 2.37 provides:
    fix for byte order on Mac PPC that prevented pianosyn.lsp from loading
Version 2.36 provides:
    cross-platform browser launching in NyquistIDE
    fix search path set by NyquistIDE (OS X-related bug introduced in 2.35)
    fix bug in slider update code on OS X
Version 2.35 provides:
    fix for Open Sound Control under Windows/jNyqIDE
    other minor NyquistIDE fixes    
Version 2.34 provides:
    fix to ^U (send selection to Nyquist) in NyquistIDE
    default sound file path for Mac OS X is /tmp
    Nyquist exits when EOF detected -- try to make orphans abort
Version 2.33 provides:
    additional documentation for Open Sound Control
        interface and utility programs
Version 2.32 provides:
    envelope editor in NyquistIDE
    EQ editor in NyquistIDE
    score editor in NyquistIDE
    slider support in Nyquist
    OSC (Open Sound Control) interface
    OSC test program and serial-to-OSC program
    drum machine (as separate download)
    NyquistIDE has pop-up menus and per-file menu bars
Version 2.31 provides:
    new compositional algorithm support in xm.lsp
    many bug fixes
    MiniMoog emulator
    spatialization libraries
    sound reversal functions
    Dolby Surround encode/decode
Version 2.30 provides:
    many many changes, bug fixes, enhancements
    new Java-based IDE: jnyqide
    LPC analysis/synthesis
    uses PortAudio for audio I/O
    changes for Debian Linux compatibility
    new examples in demos
    new documentation and html files
Version 2.29 provides:
    new functions: snd-alpassvc, sndalpassvv, snd-eqbandvvv
    corresponding high-level functions in Nyquist
    new licenses for both Nyquist and XLISP
    new NyqIDE implementation
    fixed BUZZ function
    various bug and documentation fixes
Version 2.28 provides:
    include indx.html in doc folder (in files.txt)
    fixed compute-default-sound-file in nyquist.lsp to
    compute appropriate extension (.wav, .aif)
    more code to automate win32 releases
Version 2.27 provides:
    makefile.lsp now generates sndfn.wcl & sndfn.cl
    fix to include snd-pluck and some others omittted from 2.26
Version 2.26 provides:
    bug fix in sampler, negative frequency handling
    guard against out-of-order events in TIMED-SEQ
    added FMLFO, an lfo with frequency modulation
    added SND-SQRT, S-SQRT, SND-ABS, S-ABS functions
    new NyqIDE version with S-PLOT function (!)
    NyqIDE has better parsing for paren balancing
    NyqIDE upgrade to WindowsXP and Delphi 6
    NyqIDE increases input string length maximum
    NyqIDE prompts on save conflict
    added voice-synthesis demo from Eduardo Miranda
    corrected absolute path in demos/pmorales/e2.lsp
    minor documentation and indexing improvements
    pointer to demo docs goes on start menu now
Version 2.25 provides:
    new way to provide search path: set *SEARCH-PATH* to a string,
    e.g. (SETF *SEARCH-PATH* 
    "C:/program files/nyquist/runtime,c:/program files/nyquist/lib")
    allowing Nyquist to be run without setting registry.
Version 2.24 provides:
    text editing for command lines in Linux version
Version 2.23 provides:
    bug fix in (current-path) for Mac
    fixes to some Mac sources corrupted in 2.22
Version 2.22 provides:
    documentation (HTML) included in release now
    bug fix for Mac console output exceeding 32K limit
    protection from playing very high sample rates in Win32
        (crashes in Windows MME library!)
    change s-save to take :endian rather than :swap parameter
    pianosyn.lsp runs on the Mac now
    demos/examples.lsp generates audio with "normal" sample rates
Version 2.21 provides:
    s-plot uses gnu-plot in Linux
    separation from CVS -- I just couldn't keep beating my head
               against the wall
Version 2.20 provides:
    improved Macintosh support
Version 2.19 provides:
    integration of Macintosh code (from v2.12)
    addition of PLUCK and BUZZ synthesis functions
Version 2.18 provides:
    bug fix in midifile read routine under Linux
Version 2.17 provides:
    bug fix for long line input under linux and windows
    biquad filters
    hzosc osc-tri osc-saw osc-pulse -- new oscillator variants
    bug fix for reading in non-AIFF files with 'FORM' headings
    extension to s-read to support explicit byte-swap parameter
Version 2.16 provides:
    bug fix in tables (lookup oscillators and other functions)
    Windows GUI version of Nyquist
Version 2.15 provides:
    port to Linux
Version 2.5 provides:
    more signal processing functions
Version 2.2 provides:
        ports to more systems including Win32 (Win95 and NT)
        bug fixes
        more signal processing functions
        improved sound I/O subsystem
Version 2.1 provides:
        bug fixes
        documentation and code for user extensions
Version 2.0 provides:
        continuous time warps
        many more functions
        bug fixes

The distribution may contain sources for Nyquist. If not,
you got the runtime distribution, and there is a source version
available.
 
A number of "source" files are machine generated, including:
-  many .c and .h files that implement signal processing functions.
        These are generated by translation system that converts
        .alg files to .c and .h files.  .alg files give high-level
        descriptions of DSP algorithms.  
- Makefile.* is generated by "makefile.lsp".

The status is:

System                  Status

RS6K = RS6000, AIX      untested, but used to work
NEXT = NeXT 3.0 (Cube)  untested, but worked fine on a previous version
SGI = ???               untested, but used to work
PMAX = Mach 2.5 on Dec workstation
            untested, but worked in previous version
SPARC = Sun Sparc ???   untested - previous version of Nyquist DID work
LINUX = Linux		tested
Win32			tested
Mac			tested

If you have problems running Nyquist on a Unix machine, I'd be happy to 
help. I can give you advice or if you give me an account, I can log in
remotely and install Nyquist for you. If you make corrections yourself,
please let me have them so I can put them in the next release.

DIRECTORY STRUCTURE
===================
cmt - CMU MIDI Toolkit files, used by Nyquist for MIDI File I/O
demos - Nyquist demos go here
fft - some fft functions
lib - .lsp files offering extensions to Nyquist
misc - various files and programs used to implement Nyquist
nyqsrc - general Nyquist source code (mostly in C)
runtime - the Nyquist and XLisp runtime code (mostly in XLisp)
sys - system specific files
snd - the sound file, sound input, and sound output package
test - test code (this is not maintained and may not be in the release)
todo - list of things to do (this may not be in the release)
tran - descriptor (.alg) files for machine-translated Nyquist code
xlisp - sources for Xlisp (these are linked into Nyquist)

