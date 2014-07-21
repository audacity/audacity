/**********************************************************************

  Audacity: A Digital Audio Editor

  Experimental.h

  Dominic Mazzoni
  James Crook

  Used for #includes and #defines for experimental features.

  When the features become mainstream the #include files will
  move out of here and into the files which need them.  The
  #defines will then be retired.



  JKC: This file solves a problem of how to avoid forking the
  code base when working on new features e.g:
    - Additional displays in Audacity
    - Modular architecture.
  Add #defines in here for the new features, and make your code
  conditional on those #defines.

  All the #defines are positive, i.e., when defined,
  they enable the feature.

**********************************************************************/

#ifndef __EXPERIMENTAL__
#define __EXPERIMENTAL__

// ACH 08 Jan 2014
// EQ accelerated code
//#define EXPERIMENTAL_EQ_SSE_THREADED

// LLL, 09 Nov 2013:
// Allow all WASAPI devices, not just loopback
#define EXPERIMENTAL_FULL_WASAPI

// JKC (effect by Norm C, 02 Oct 2013)
//#define EXPERIMENTAL_SCIENCE_FILTERS

// LLL, 01 Oct 2013:
// new key assignment view for preferences
#define EXPERIMENTAL_KEY_VIEW

// feature to link audio tracks to a label track
#define EXPERIMENTAL_SYNC_LOCK

// experimental theming
// Work in progress, June-2008.
// This mostly sets up a weird color scheme currently.
//#define EXPERIMENTAL_THEMING

//August 2009 - Theming not locked down enough for a stable release.
// This turns on the Theme panel in Prefs dialog. It is independent of EXPERIMENTAL_THEMING.
//#define EXPERIMENTAL_THEME_PREFS

//Next line enables Mic monitoring at times when it was previously off.
//More work is needed as after recording or playing it results in an
//unwanted record-cursor on the wave track.
//#define EXPERIMENTAL_EXTRA_MONITORING

//#define EXPERIMENTAL_ROLL_UP_DIALOG
//#define RIGHT_ALIGNED_TEXTBOXES
//#define EXPERIMENTAL_VOICE_DETECTION

// Effect categorisation. Adds support for arranging effects in categories
// and displaying those categories as submenus in the Effect menu.
// This was a 2008 GSoC project that was making good progress at the half-way point
// but then the student didn't contribute after that.  It needs a bit of work to finish it off.
// As a minimum, if this is turned on for a release,
// it should have an easy mechanism to disable it at run-time, such as a menu item or a pref,
// preferrably disabled until other work is done.  Martyn 22/12/2008.
//#define EFFECT_CATEGORIES

// Andreas Micheler, 20.Nov 2007:
// A spectrumLogF-like view mode with notes quantization.
// Just select the "Find Notes" checkbox in the spectrum prefs
// to activate it instead of the Spectrum log(f) mode.
//#define EXPERIMENTAL_FIND_NOTES

// AM, 22.Nov 2007
// Skip Points support in the spectrum view mode.
//#define EXPERIMENTAL_FFT_SKIP_POINTS

// AM, 22.Nov 2007:
// A Frequency Grid for the Spectrum Log(f) & Find Notes modes
//#define EXPERIMENTAL_FFT_Y_GRID

// Andy Coder, 03.Mar 2009:
// Allow keyboard seeking before initial playback position
//#define EXPERIMENTAL_SEEK_BEHIND_CURSOR

// Michael Chinen, 08.Oct 2009
// Use on-demand importing for FLAC. Has issues with opening projects that
// have not been fully imported in builds without FLAC support, so disabled for
// 2.0 release
//#define EXPERIMENTAL_OD_FLAC
// similarly for FFmpeg:
// Won't build on Fedora 17 or Windows VC++, per http://bugzilla.audacityteam.org/show_bug.cgi?id=539.
//#define EXPERIMENTAL_OD_FFMPEG 1

// Philip Van Baren 01 July 2009
// Replace RealFFT() and PowerSpectrum function to use (faster) RealFFTf function
#define EXPERIMENTAL_USE_REALFFTF

// RBD, 1 Sep 2008
// Enables MIDI Output of NoteTrack (MIDI) data during playback
// USE_MIDI must be defined in order for EXPERIMENTAL_MIDI_OUT to work
#ifdef USE_MIDI
//#define EXPERIMENTAL_MIDI_OUT
#endif

// USE_MIDI must be defined in order for EXPERIMENTAL_SCOREALIGN to work
#ifdef USE_MIDI
//#define EXPERIMENTAL_SCOREALIGN
#endif

// experimental features
#define EXPERIMENTAL_FEATURES // ANSWER ME: EXPERIMENTAL within EXPERIMENTAL?
#ifdef EXPERIMENTAL_FEATURES
   // The first experimental feature is a notebook that adds
   // a tabbed divider to the project.
   //#define EXPERIMENTAL_NOTEBOOK
   // The notebook in turn can contain:
   // 1. The Nyquist Inspector, which is a browser for the objects in
   // Audacity.
   //#define EXPERIMENTAL_NYQUIST_INSPECTOR
   // 2. The Vocal Studio, a screen for working with vocal sounds
   // particularly vowel sounds.
   //#define EXPERIMENTAL_VOCAL_STUDIO
   // 3. The Audacity Tester is an extended version of the benchmarks
   // display.  The crucial idea is to be able to compare waveforms
   // where effects have been applied by audacity but using different
   // block-sizes.  This should give high confidence that we don't
   // suffer from end-effects on buffers, e.g. losing one sample on
   // each buffer.
   //#define EXPERIMENTAL_AUDACITY_TESTER

   // A long term plan is to use dso's and dlls for Audacity extensions
   // These are 'WX' plug ins that manage their own displays using
   // wxWidgets.
   //#define EXPERIMENTAL_WX_PLUG_INS
#endif

//If you want any of these files, ask JKC.  They are not
//yet checked in to Audacity SVN as of 12-Feb-2010
#ifdef EXPERIMENTAL_NOTEBOOK
   #include "widgets/GuiFactory.h"
   #include "widgets/APanel.h"
   extern void AddPages(   AudacityProject * pProj, GuiFactory & Factory,  wxNotebook  * pNotebook );
#endif

#ifdef EXPERIMENTAL_NYQUIST_INSPECTOR
   #include "NyquistAdapter.h"
#endif

#ifdef EXPERIMENTAL_AUDACITY_TESTER
#endif

#if USE_PORTMIXER
   //Automatically tries to find an acceptable input volume
   //#define AUTOMATED_INPUT_LEVEL_ADJUSTMENT
#endif

// John (Thales) work to make the display show the result of the pan and gain sliders, rather than their input.
// First committed by Martyn, 30th May 2013.
//#define EXPERIMENTAL_OUTPUT_DISPLAY

// Module prefs may be used to treat 'official' modules differently to 3rd party ones
//#define EXPERIMENTAL_MODULE_PREFS
#endif
