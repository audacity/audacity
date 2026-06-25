/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2012 Audacity Team.
   License: GPL v2 or later.  See License.txt.

  PitchName.cpp
  Vaughan Johnson and Dominic Mazzoni.

******************************************************************//**

\file PitchName.cpp
\brief   Utilities for converting from frequency to pitch
  and from pitch to absolute (e.g., C4 for middle C)
  or nominal (A through G#) pitch name.

*//*******************************************************************/

#include "PitchName.h"
#include "au3-strings/Internat.h"
#include <math.h>

double FreqToMIDInote(const double freq)
{
    // Make the calculation relative to A440 (A4), note number 69.
    return 69.0 + (12.0 * (log(freq / 440.0) / log(2.0)));
}

double MIDInoteToFreq(const double dMIDInote)
{
    return 440.0 * pow(2.0, (dMIDInote - 69.0) / 12.0);
}

unsigned int PitchIndex(const double dMIDInote)
{
    // MIDI numbers can be negative. Round in the right direction.
    double dRound = (dMIDInote < 0.0) ? -0.5 : 0.5;
    int nPitchIndex = ((int)(dMIDInote + dRound) % 12);

    // Because of the modulo, we know we're within 12 of positive, if dMIDInote is negative.
    if (nPitchIndex < 0) {
        nPitchIndex += 12;
    }

    return nPitchIndex;
}

int PitchOctave(const double dMIDInote)
{
    double dRound = (dMIDInote < 0.0) ? -0.5 : 0.5;
    return (int)((dMIDInote + dRound) / 12.0) - 1;
}

TranslatableString PitchName(const double dMIDInote, const PitchNameChoice choice)
{
    static const TranslatableString sharpnames[12] = {
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "C"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "C\u266f"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D\u266f"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "E"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "F"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "F\u266f"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G\u266f"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A\u266f"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "B"),
    };

    static const TranslatableString flatnames[12] = {
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "C"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "E\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "E"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "F"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "B\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "B"),
    };

    static const TranslatableString bothnames[12] = {
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "C"),
        /*: Two, alternate names of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "C\u266f/D\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D"),
        /*: Two, alternate names of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "D\u266f/E\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "E"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "F"),
        /*: Two, alternate names of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "F\u266f/G\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G"),
        /*: Two, alternate names of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "G\u266f/A\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A"),
        /*: Two, alternate names of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "A\u266f/B\u266d"),
        /*: Name of a musical note in the 12-tone chromatic scale */
        TranslatableString("math", "B"),
    };

    const TranslatableString* table = nullptr;
    switch (choice) {
    case PitchNameChoice::Sharps: table = sharpnames;
        break;
    case PitchNameChoice::Flats: table = flatnames;
        break;
    case PitchNameChoice::Both: table = bothnames;
        break;
    default: wxASSERT(false);
        break;
    }

    return table[PitchIndex(dMIDInote)];
}

TranslatableString PitchName_Absolute(const double dMIDInote, const PitchNameChoice choice)
{
    // The format string is not localized.  Should it be?
    return au3::untranslatable(wxT("%1%2"))
           .Format(PitchName(dMIDInote, choice), PitchOctave(dMIDInote));
}

double PitchToMIDInote(const unsigned int nPitchIndex, const int nPitchOctave)
{
    return (double)nPitchIndex + (((double)nPitchOctave + 1.0) * 12.0);
}

double PitchToFreq(const unsigned int nPitchIndex, const int nPitchOctave)
{
    return MIDInoteToFreq(PitchToMIDInote(nPitchIndex, nPitchOctave));
}
