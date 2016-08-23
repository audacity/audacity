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
  code base when working on NEW features e.g:
    - Additional displays in Audacity
    - Modular architecture.
  Add #defines in here for the NEW features, and make your code
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
#define EXPERIMENTAL_SCIENCE_FILTERS

// LLL, 01 Oct 2013:
// NEW key assignment view for preferences
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
//#define EXPERIMENTAL_RIGHT_ALIGNED_TEXTBOXES
//#define EXPERIMENTAL_VOICE_DETECTION

// Effect categorisation. Adds support for arranging effects in categories
// and displaying those categories as submenus in the Effect menu.
// This was a 2008 GSoC project that was making good progress at the half-way point
// but then the student didn't contribute after that.  It needs a bit of work to finish it off.
// As a minimum, if this is turned on for a release,
// it should have an easy mechanism to disable it at run-time, such as a menu item or a pref,
// preferrably disabled until other work is done.  Martyn 22/12/2008.
// 

// JKC Apr 2015, Menu item to manage effects.
#define EXPERIMENTAL_EFFECT_MANAGEMENT

// Andreas Micheler, 20.Nov 2007:
// A spectrumLogF-like view mode with notes quantization.
// Just select the "Find Notes" checkbox in the spectrum prefs
// to activate it instead of the Spectrum log(f) mode.
//#define EXPERIMENTAL_FIND_NOTES

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

// Paul Licameli (PRL) 5 Oct 2014
#define EXPERIMENTAL_SPECTRAL_EDITING

// Paul Licameli (PRL) 29 Nov 2014
// #define EXPERIMENTAL_IMPROVED_SEEKING

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

#if USE_PORTMIXER
   //Automatically tries to find an acceptable input volume
   //#define EXPERIMENTAL_AUTOMATED_INPUT_LEVEL_ADJUSTMENT
#endif

// John (Thales) work to make the display show the result of the pan and gain sliders, rather than their input.
// First committed by Martyn, 30th May 2013.
//#define EXPERIMENTAL_OUTPUT_DISPLAY

// Module prefs provides a panel in prefs where users can choose which modules
// to enable.
#define EXPERIMENTAL_MODULE_PREFS

// Define to allow realtime processing in Audacity effects that have been converted.
#define EXPERIMENTAL_REALTIME_AUDACITY_EFFECTS

// Define to include the effects rack (such as it is).
//#define EXPERIMENTAL_EFFECTS_RACK

// Define to make the meters look like a row of LEDs
//#define EXPERIMENTAL_METER_LED_STYLE

// Define to enable the device change handler
//#define EXPERIMENTAL_DEVICE_CHANGE_HANDLER

// Define for NEW noise reduction effect from Paul Licameli.
#define EXPERIMENTAL_NOISE_REDUCTION

// Define to enable Nyquist audio clip boundary control (Steve Daulton Dec 2014)
#define EXPERIMENTAL_NYQUIST_SPLIT_CONTROL

// Paul Licameli (PRL) 16 Apr 2015
// Support for scrubbing in the AudioIO engine, without calls to it
#define EXPERIMENTAL_SCRUBBING_SUPPORT
#ifdef EXPERIMENTAL_SCRUBBING_SUPPORT
   // The following enable parts of the scrubbing user interface.
   #define EXPERIMENTAL_SCRUBBING_BASIC
   #ifdef EXPERIMENTAL_SCRUBBING_BASIC
      #define EXPERIMENTAL_SCRUBBING_SCROLL_WHEEL
   #endif
#endif

// Paul Licameli (PRL) 24 May 2015
// Allow scrolling up to one half of a screenful beyond either end of the project,
// if you turn on the appropriate Tracks preference.
// This allows smooth-scrolling scrub to work more reasonably at the ends.
#define EXPERIMENTAL_SCROLLING_LIMITS

// Paul Licameli (PRL) 28 May 2015
// Draw negative numbers on the time ruler in a different color, when
// scrolling past zero is enabled. Perhaps that lessens confusion.
#define EXPERIMENTAL_TWO_TONE_TIME_RULER

// Define to include crash reporting
#include <wx/defs.h>
#define EXPERIMENTAL_CRASH_REPORT
#if !defined(wxUSE_DEBUGREPORT) || !wxUSE_DEBUGREPORT
#undef EXPERIMENTAL_CRASH_REPORT
#endif

// Paul Licameli (PRL) 31 May 2015
// Zero-padding factor for spectrograms can smooth the display of spectrograms by
// interpolating in frequency domain.
#define EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS

#endif
