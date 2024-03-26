/**********************************************************************
 
 Audacity: A Digital Audio Editor
 
  Experimental.h

  Dominic Mazzoni
  James Crook

  Paul Licameli adapted from Experimental.cmake

  Constants that enable or disable experimental features with constexpr if,
  which still performs compilation checks of disabled branches.

  Use this when more than one translation unit really needs to see a common
  switch -- but otherwise, prefer to define a flag private to one source
  file.  Follow the convention of putting such flags in namespace Experimental
  too.

  When the features become mainstream the options can then be retired.

  JKC: This file solves a problem of how to avoid forking the
  code base when working on new features e.g:
    - Additional displays in Audacity
    - Modular architecture.
  Add options in here for the new features.

 **********************************************************************/
#ifndef __AUDACITY_EXPERIMENTAL__
#define __AUDACITY_EXPERIMENTAL__

namespace Experimental {

// JKC an experiment to work around bug 2709
constexpr bool CeeNumbersOption = false;

// August 2009 - Theming not locked down enough for a stable release.
// This turns on the Theme panel in Prefs dialog.
constexpr bool ThemePrefs = false;

/* Andreas Micheler, 20.Nov 2007:
 A spectrumLogF-like view mode with notes quantization.
 Just select the "Find Notes" checkbox in the spectrum prefs
 to activate it instead of the Spectrum log(f) mode.
 */
constexpr bool FindNotes = false;

/* AM, 22.Nov 2007:
 A Frequency Grid for the Spectrum Log(f) & Find Notes modes
 */
constexpr bool FftYGrid = false;

/*
 Edward Hui 1 Jul 2021
 */
constexpr bool SpectralBrushTool = false;
}

#endif
