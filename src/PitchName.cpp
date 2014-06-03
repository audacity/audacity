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


static wxChar gPitchName[10];
static wxChar * pPitchName;

wxChar * PitchName(const double dMIDInote, const bool bWantFlats /* = false */)
{
   pPitchName = gPitchName;

   switch (PitchIndex(dMIDInote)) {
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

wxChar * PitchName_Absolute(const double dMIDInote, const bool bWantFlats /* = false */)
{
   PitchName(dMIDInote, bWantFlats);

   // PitchName sets pPitchName to the next available char in gPitchName,
   // so it's ready to append the register number.
   wxSnprintf(pPitchName, 8, wxT("%d"), PitchOctave(dMIDInote));

   return gPitchName;
}

double PitchToMIDInote(const unsigned int nPitchIndex, const int nPitchOctave)
{
   return ((double)nPitchIndex + (((double)nPitchOctave + 1.0) * 12.0));
}

double PitchToFreq(const unsigned int nPitchIndex, const int nPitchOctave)
{
   return MIDInoteToFreq(PitchToMIDInote(nPitchIndex, nPitchOctave));
}
