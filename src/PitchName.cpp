/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2.  See License.txt.

  PitchName.cpp
  Vaughan Johnson and Dominic Mazzoni. 

******************************************************************//**

\file PitchName.cpp
\brief   Utilities for converting from frequency to pitch  
  and from pitch to absolute (e.g., C4 for middle C) 
  or nominal (A through G#) pitch name.

*//*******************************************************************/


#include <math.h>
#include <stdio.h>

#include "PitchName.h"


// FreqToMIDInoteNumber takes a frequency in Hz (exponential scale relative to 
// alphabetic pitch names) and returns a pitch ID number (linear 
// scale), such that A440 (A4) is 69, middle C (C4) is 60, etc.
// Each register starts with C (e.g., for middle C and A440, 
// it's register 4).
double FreqToMIDInoteNumber(double freq)
{
   // Make the calculation relative to A440 (A4), note number 69. 
   return (69.0 + (12.0 * (log(freq / 440.0) / log(2.0))));
}

// PitchIndex returns the [0,11] index for a double pitchNum, 
// as per result from FreqToMIDInoteNumber, corresponding to modulo 12 
// of the integer part of (pitchNum + 0.5), so 0=C, 1=C#, etc.
unsigned int PitchIndex(double pitchNum)
{
   int nPitchIndex = ((int)(pitchNum + 0.5) % 12);
   // MIDI numbers (pitchNum) can be negative.
   // Because of the modulo, we know we're within 12 of positive. 
   if (nPitchIndex < 0)
      nPitchIndex += 12;
   return nPitchIndex;
}


wxChar gPitchName[10];
wxChar * pPitchName;

// PitchName takes pitchNum (as per result from 
// FreqToMIDInoteNumber) and returns a standard pitch/note name [C, C#, etc.). 
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName(double pitchNum, bool bWantFlats /* = false */)
{
   pPitchName = gPitchName;

   switch (PitchIndex(pitchNum)) {
   case 0:
      *pPitchName++ = wxT('C');
      break;
   case 1:
      if (bWantFlats) {
         *pPitchName++ = wxT('D');
         *pPitchName++ = wxT('b');
      } else {
         *pPitchName++ = wxT('C');
         *pPitchName++ = wxT('#');
      }
      break;
   case 2:
      *pPitchName++ = wxT('D');
      break;
   case 3:
      if (bWantFlats) {
         *pPitchName++ = wxT('E');
         *pPitchName++ = wxT('b');
      } else {
         *pPitchName++ = wxT('D');
         *pPitchName++ = wxT('#');
      }
      break;
   case 4:
      *pPitchName++ = wxT('E');
      break;
   case 5:
      *pPitchName++ = wxT('F');
      break;
   case 6:
      if (bWantFlats) {
         *pPitchName++ = wxT('G');
         *pPitchName++ = wxT('b');
      } else {
         *pPitchName++ = wxT('F');
         *pPitchName++ = wxT('#');
      }
      break;
   case 7:
      *pPitchName++ = wxT('G');
      break;
   case 8:
      if (bWantFlats) {
         *pPitchName++ = wxT('A');
         *pPitchName++ = wxT('b');
      } else {
         *pPitchName++ = wxT('G');
         *pPitchName++ = wxT('#');
      }
      break;
   case 9:
      *pPitchName++ = wxT('A');
      break;
   case 10:
      if (bWantFlats) {
         *pPitchName++ = wxT('B');
         *pPitchName++ = wxT('b');
      } else {
         *pPitchName++ = wxT('A');
         *pPitchName++ = wxT('#');
      }
      break;
   case 11:
      *pPitchName++ = wxT('B');
      break;
   }

   *pPitchName = wxT('\0');

   return gPitchName;
}

// PitchName_Absolute does the same thing as PitchName, but appends 
// the octave number, e.g., instead of "C" it will return "C4" 
// if the pitchNum corresonds to middle C, i.e., is 60. 
// ("Scientific Pitch Notation")
// Sharps are the default, unless, bWantFlats is true.
wxChar * PitchName_Absolute(double pitchNum, bool bWantFlats /* = false */)
{
   PitchName(pitchNum, bWantFlats);

   // PitchName sets pPitchName to the next available char in gPitchName,
   // so it's ready to append the register number.
   int octaveNum = ((int)((pitchNum + 0.5) / 12) - 1);
   wxSnprintf(pPitchName, 8, wxT("%d"), octaveNum);

   return gPitchName;
}

