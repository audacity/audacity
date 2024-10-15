/********************************************************************
*    swlogic.h - switch logic, loaded from various versions of switch.h
*
* Copyright 1989 Carnegie Mellon University
*
*********************************************************************/

/* 
 * When included, one of the following should be defined: 
 *         AZTEC (manx compiler, implies AMIGA)
 *         THINK_C (Think C compiler, implies Macintosh)
 *         __MWERKS__ (Metrowerks C compiler, implies Macintosh)
 *         LATTICE & DOS (Lattice compiler for IBM PC/AT/XT/CLONES)
 *         MICROSOFT & DOS (Microsoft compiler, implies IBM PC/AT/XT/CLONES)
 *         UNIX (emulator for UNIX)
 *         UNIX_ITC (ITC code for RS6000)
 *         UNIX_MACH (MACH ukernel system)
 */
 
/*------------------------------------------*/
/* Other switches that might be defined in switches.h are as follows: */
/* APPLICATION, SPACE_FOR_PLAY, MAX_CHANNELS */
/*------------------------------------------*/

/* We're moving toward the elimination of switches.h, so try to map
 * predefined constants into our standard constants shown above:
 */

/* CHANGE LOG
 * --------------------------------------------------------------------
 * 28Apr03  dm  new conditional compilation structure
 * 28Apr03  rbd remove macro redefinitions: MICROSOFT 
 */



/* Microsoft C compiler: */
#ifdef _MSC_VER
#endif
#ifdef _MSDOS
#define DOS
#endif

/* Quick C compiler: */
#ifndef DOS
#ifdef MICROSOFT
#define DOS
#endif
#endif

/* Borland C compiler: */
#ifdef __BORLANDC__
#define BORLAND
#define DOS
#endif

/* Borland Turbo C compiler: */
#ifdef __TURBOC__
#define BORLAND]
#define DOS
#endif

/* SGI systems */
#ifdef sgi
#ifndef UNIX
#define UNIX
#endif
#define UNIX_IRIX
#define MAX_CHANNELS 32
#endif

/* APPLICATION -- define APPLICATION if you want to disable
 * looking for command line switches in the midi interface.
 * I think this feature is here for the Piano Tutor project
 * and you should not define APPLICATION for CMU Midi Toolkit
 * projects (APPLICATION is a poor choice of terms):
 */


/* memory space management (system dependent):
 *      SPACE_FOR_PLAY must be enough space to allow
 *  seq to play a score.  This may include space for
 *  note-off events, I/O buffers, etc.
 */
#ifndef SPACE_FOR_PLAY
#define SPACE_FOR_PLAY 10000L
#endif


/* How many MIDI channels are there?  MACINTOSH can use 2 ports,
 * so it supports 32 channels.  Others have one port, 16 channels.
 * On the other hand, if you don't have all the MIDI ports plugged
 * into MIDI interfaces, CMT will just hang, so I'll compile with
 * just 16 channels.  The 32 channel option for the Mac is untested.
 */
#ifndef MAX_CHANNELS
#define MAX_CHANNELS 16
#endif


/*------------------------------------------*/
/* Now we get to the "logic": define things as a function of what
 * was defined in switches.h
 */

#ifdef THINK_C
#define MACINTOSH
#endif

#ifdef __MWERKS__
#define MACINTOSH
#endif

#ifdef MACINTOSH
#define MACINTOSH_OR_DOS
#define MACINTOSH_OR_UNIX
/* I don't know if THINK_C defines this and we need it for a few prototypes... */
#ifndef __STDC__
#define __STDC__
#endif
#ifndef TAB_WIDTH
#define TAB_WIDTH 4
#endif
#endif

#ifndef TAB_WIDTH
#define TAB_WIDTH 8
#endif

/*
 * If MIDIMGR is defined, compile for the Apple MIDI Manager
 * (Non MIDI manager code is no longer supported)
 */
#ifdef MACINTOSH
/* under Nyquist, the MidiMgr is not used, so you can't
 * receive or send Midi as in CMU MIDI Toolkit; however,
 * much of CMU MIDI Toolkit is used for Midi file IO
 */
#ifndef NYQUIST
#define MIDIMGR
#endif
#define HAVE_VSNPRINTF 1
#endif

#ifdef BORLAND
#define DOS
#endif

#ifdef LATTICE322
#define DOS
#define OLD_PROTOTYPES
#endif

#ifdef UNIX_ITC
#define UNIX
#define ITC
#endif

#ifdef UNIX_MACH
#define UNIX
#define ITC
#endif

/* HAVE_VSNPRINTF says vsnprintf() is defined */
#ifdef ITC
#define HAVE_VSNPRINTF 1
#endif
#ifdef AZTEC
#define HAVE_VSNPRINTF 1
#endif


/* DOTS_FOR_ARGS says ANSI "..." notation is recognized */
#ifdef __STDC__
#define DOTS_FOR_ARGS
#endif
#ifdef UNIX_ITC
#define DOTS_FOR_ARGS
#endif
#ifdef BORLAND
#define DOTS_FOR_ARGS
#endif
#ifdef MICROSOFT
#define DOTS_FOR_ARGS
#endif

#ifdef DOS
#define MACINTOSH_OR_DOS
#else
#define huge
#endif

#ifdef UNIX
#define MACINTOSH_OR_UNIX
#endif

#define SWITCHES
