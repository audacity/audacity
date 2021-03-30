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

#ifndef __AUDACITY_H__
// Audacity.h is needed for the USE_* macros
#error Must include Audacity.h before Experimental.h
#endif

// ACH 08 Jan 2014
// EQ accelerated code
//#define EXPERIMENTAL_EQ_SSE_THREADED

// LLL, 09 Nov 2013:
// Allow all WASAPI devices, not just loopback
#define EXPERIMENTAL_FULL_WASAPI

// JKC (effect by Norm C, 02 Oct 2013)
#define EXPERIMENTAL_SCIENCE_FILTERS

// JKC an experiement to work around bug 2709
// disabled.
//define EXPERIMENTAL_CEE_NUMBERS_OPTION

// LLL, 01 Oct 2013:
// NEW key assignment view for preferences
#define EXPERIMENTAL_KEY_VIEW

// feature to link audio tracks to a label track
#define EXPERIMENTAL_SYNC_LOCK

// DA: Enables dark audacity theme and customisations.
//#define EXPERIMENTAL_DA

// These CFG macros allow easy distinction between Audacity and DA defaults.
#ifdef EXPERIMENTAL_DA
#define CFG_A( x ) 
#define CFG_DA( x ) x
#else
#define CFG_A( x ) x
#define CFG_DA( x ) 
#endif


// Define this so that sync-lock tiles shine through spectrogram.
// The spectrogram pastes a bitmap over the tiles.
// This makes it use alpha blending, most transparent where least intense.
#define EXPERIMENTAL_SPECTROGRAM_OVERLAY

// Define this so that sync-lock tiles shine through note/MIDI track.
// The note track then relies on the same code for drawing background as
// Wavetrack, and draws its notes and lines over the top.
#define EXPERIMENTAL_NOTETRACK_OVERLAY

// Define this, and the option to zoom to half wave is added in the VZoom menu.
// Also we go to half wave on collapse, full wave on restore.
#define EXPERIMENTAL_HALF_WAVE

// EXPERIMENTAL_THEMING is mostly mainstream now.
// the define is still present to mark out old code before theming, that we might
// conceivably need.
// TODO: Agree on and then tidy this code.
#define EXPERIMENTAL_THEMING

//August 2009 - Theming not locked down enough for a stable release.
// This turns on the Theme panel in Prefs dialog. It is independent of EXPERIMENTAL_THEMING.
//#define EXPERIMENTAL_THEME_PREFS

// This shows the zoom toggle button on the edit toolbar.
#define EXPERIMENTAL_ZOOM_TOGGLE_BUTTON

//#define EXPERIMENTAL_ROLL_UP_DIALOG
//#define EXPERIMENTAL_RIGHT_ALIGNED_TEXTBOXES
//#define EXPERIMENTAL_VOICE_DETECTION

// Effect categorisation. Adds support for arranging effects in categories
// and displaying those categories as submenus in the Effect menu.
// This was a 2008 GSoC project that was making good progress at the half-way point
// but then the student didn't contribute after that.  It needs a bit of work to finish it off.
// As a minimum, if this is turned on for a release,
// it should have an easy mechanism to disable it at run-time, such as a menu item or a pref,
// preferably disabled until other work is done.  Martyn 22/12/2008.
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

// Paul Licameli (PRL) 5 Oct 2014
#define EXPERIMENTAL_SPECTRAL_EDITING

// Paul Licameli (PRL) 29 Nov 2014
// #define EXPERIMENTAL_IMPROVED_SEEKING

//#define EXPERIMENTAL_MIDI_IN

// RBD, 1 Sep 2008
// Enables MIDI Output of NoteTrack (MIDI) data during playback
// USE_MIDI must be defined in order for EXPERIMENTAL_MIDI_OUT to work
#ifdef USE_MIDI
#define EXPERIMENTAL_MIDI_OUT
#endif
// JKC, 17 Aug 2017
// Enables the MIDI note stretching feature, which currently
// a) Is broken on Linux (Bug 1646)
// b) Crashes with Sync-Lock (Bug 1719)
// c) Needs UI design review.
//#define EXPERIMENTAL_MIDI_STRETCHING

// USE_MIDI must be defined in order for EXPERIMENTAL_SCOREALIGN to work
//#define EXPERIMENTAL_SCOREALIGN

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

#ifndef IN_RC
   // Define to include crash reporting
   #define EXPERIMENTAL_CRASH_REPORT
   #ifdef EXPERIMENTAL_CRASH_REPORT
      #include <wx/setup.h> // for wxUSE* macros
      #if !defined(wxUSE_DEBUGREPORT) || !wxUSE_DEBUGREPORT
         #undef EXPERIMENTAL_CRASH_REPORT
      #endif
   #endif
#endif

// Paul Licameli (PRL) 31 May 2015
// Zero-padding factor for spectrograms can smooth the display of spectrograms by
// interpolating in frequency domain.
#define EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS

// PRL 11 Jul 2017
// Highlight more things in TrackPanel when the mouse moves over them,
// using deliberately ugly pens and brushes until there is better cooperation
// with themes
//#define EXPERIMENTAL_TRACK_PANEL_HIGHLIGHTING

// Paul Licameli (PRL) 28 Dec 2017
// Easy drag-and-drop to add Nyquist, LADSPA, and VST plug-ins
// #define EXPERIMENTAL_DRAG_DROP_PLUG_INS

// PRL 5 Jan 2018
// Easy change of keystroke bindings for menu items
#define EXPERIMENTAL_EASY_CHANGE_KEY_BINDINGS

// PRL 1 Jun 2018
#define EXPERIMENTAL_PUNCH_AND_ROLL

// PRL 31 July 2018
#define EXPERIMENTAL_DRAGGABLE_PLAY_HEAD

// Jonatã Bolzan Loss 31 Dec 2019
#define EXPERIMENTAL_TIMER_TOOLBAR

#endif
