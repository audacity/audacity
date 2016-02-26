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


double FreqToMIDInote(const double freq)
{
   // Make the calculation relative to A440 (A4), note number 69.
   return (69.0 + (12.0 * (log(freq / 440.0) / log(2.0))));
}

double MIDInoteToFreq(const double dMIDInote)
{
   return (440.0 * pow(2.0, (dMIDInote - 69.0) / 12.0));
}

unsigned int PitchIndex(const double dMIDInote)
{
   // MIDI numbers can be negative. Round in the right direction.
   double dRound = (dMIDInote < 0.0) ? -0.5 : 0.5;
   int nPitchIndex = ((int)(dMIDInote + dRound) % 12);

   // Because of the modulo, we know we're within 12 of positive, if dMIDInote is negative.
   if (nPitchIndex < 0)
      nPitchIndex += 12;

   return nPitchIndex;
}

int PitchOctave(const double dMIDInote)
{
   double dRound = (dMIDInote < 0.0) ? -0.5 : 0.5;
   return ((int)((dMIDInote + dRound) / 12.0) - 1);
}

wxString PitchName(const double dMIDInote, const bool bWantFlats /* = false */)
{
   switch (PitchIndex(dMIDInote)) {
   case 0:
      return wxT("C");
      break;
   case 1:
      return bWantFlats ? wxT("Db") : wxT("C#");
      break;
   case 2:
      return wxT("D");
      break;
   case 3:
      return bWantFlats ? wxT("Eb") : wxT("D#");
      break;
   case 4:
      return wxT("E");
      break;
   case 5:
      return wxT("F");
      break;
   case 6:
      return bWantFlats ? wxT("Gb") : wxT("F#");
      break;
   case 7:
      return wxT("G");
      break;
   case 8:
      return bWantFlats ? wxT("Ab") : wxT("G#");
      break;
   case 9:
      return wxT("A");
      break;
   case 10:
      return bWantFlats ? wxT("Bb") : wxT("A#");
      break;
   case 11:
      return wxT("B");
      break;
   }
   return wxEmptyString;
}

wxString PitchName_Absolute(const double dMIDInote, const bool bWantFlats /* = false */)
{
   return wxString::Format("%s%d", PitchName(dMIDInote, bWantFlats), PitchOctave(dMIDInote));
}

double PitchToMIDInote(const unsigned int nPitchIndex, const int nPitchOctave)
{
   return ((double)nPitchIndex + (((double)nPitchOctave + 1.0) * 12.0));
}

double PitchToFreq(const unsigned int nPitchIndex, const int nPitchOctave)
{
   return MIDInoteToFreq(PitchToMIDInote(nPitchIndex, nPitchOctave));
}
