/**********************************************************************

  Audacity: A Digital Audio Editor

  PitchName.h

  Copyright 2005-9, Vaughan Johnson and Dominic Mazzoni. 
  All rights reserved.

  Utilities for converting from frequency to pitch 
  and from pitch to absolute (e.g., C4 for middle C) 
  or nominal (A through G#) pitch name.

**********************************************************************/

#ifndef __AUDACITY_PITCHNAME__
#define __AUDACITY_PITCHNAME__

#include <wx/defs.h>

// FreqToMIDInoteNumber takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 69, middle C (C4) is 60, etc.
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
double FreqToMIDInoteNumber(double freq);

// PitchIndex returns the [0,11] index for a double pitchNum, 
// as per result from FreqToMIDInoteNumber, corresponding to modulo 12 
// of the integer part of (pitchNum + 0.5), so 0=C, 1=C#, etc.
unsigned int PitchIndex(double pitchNum);

// PitchName takes pitchNum (as per result from 
// FreqToMIDInoteNumber) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName(double pitchNum, bool bWantFlats = false);

// PitchName_Absolute does the same thing as PitchName, but appends 
// the register number, e.g., instead of "C" it will return "C4" 
// if the pitchNum corresonds to middle C.
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName_Absolute(double pitchNum, bool bWantFlats = false);

#endif	// __AUDACITY_PITCHNAME__

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 1cd2b819-63b6-4051-9991-37165f236037

