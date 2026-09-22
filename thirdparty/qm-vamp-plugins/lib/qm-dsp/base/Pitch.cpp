/* -*- c-basic-offset: 4 indent-tabs-mode: nil -*-  vi:set ts=8 sts=4 sw=4: */

/*
    QM DSP library
    Centre for Digital Music, Queen Mary, University of London.
    This file Copyright 2006 Chris Cannam.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License as
    published by the Free Software Foundation; either version 2 of the
    License, or (at your option) any later version.  See the file
    COPYING included with this distribution for more information.
*/

#include "Pitch.h"

#include <math.h>

float
Pitch::getFrequencyForPitch(int midiPitch,
                            float centsOffset,
                            float concertA)
{
    float p = float(midiPitch) + (centsOffset / 100);
    return concertA * powf(2.0, (p - 69.0) / 12.0);
}

int
Pitch::getPitchForFrequency(float frequency,
                            float *centsOffsetReturn,
                            float concertA)
{
    float p = 12.0 * (log(frequency / (concertA / 2.0)) / log(2.0)) + 57.0;

    int midiPitch = int(p + 0.00001);
    float centsOffset = (p - midiPitch) * 100.0;

    if (centsOffset >= 50.0) {
        midiPitch = midiPitch + 1;
        centsOffset = -(100.0 - centsOffset);
    }
    
    if (centsOffsetReturn) *centsOffsetReturn = centsOffset;
    return midiPitch;
}

