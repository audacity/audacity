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
#include "Internat.h"
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
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("C"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("C\u266f"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("D"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("D\u266f"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("E"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("F"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("F\u266f"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("G"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("G\u266f"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("A"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("A\u266f"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("B"),
    };

    static const TranslatableString flatnames[12] = {
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("C"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("D\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("D"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("E\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("E"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("F"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("G\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("G"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("A\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("A"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("B\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("B"),
    };

    static const TranslatableString bothnames[12] = {
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("C"),
        /* i18n-hint: Two, alternate names of a musical note in the 12-tone chromatic scale */
        XO("C\u266f/D\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("D"),
        /* i18n-hint: Two, alternate names of a musical note in the 12-tone chromatic scale */
        XO("D\u266f/E\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("E"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("F"),
        /* i18n-hint: Two, alternate names of a musical note in the 12-tone chromatic scale */
        XO("F\u266f/G\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("G"),
        /* i18n-hint: Two, alternate names of a musical note in the 12-tone chromatic scale */
        XO("G\u266f/A\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("A"),
        /* i18n-hint: Two, alternate names of a musical note in the 12-tone chromatic scale */
        XO("A\u266f/B\u266d"),
        /* i18n-hint: Name of a musical note in the 12-tone chromatic scale */
        XO("B"),
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
    return Verbatim(wxT("%s%d"))
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
