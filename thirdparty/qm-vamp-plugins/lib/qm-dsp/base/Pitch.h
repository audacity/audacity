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

#ifndef QM_DSP_PITCH_H
#define QM_DSP_PITCH_H

/**
 * Convert between musical pitch (i.e. MIDI pitch number) and
 * fundamental frequency.
 */
class Pitch
{
public:
    static float getFrequencyForPitch(int midiPitch,
                                      float centsOffset = 0,
                                      float concertA = 440.0);

    static int getPitchForFrequency(float frequency,
                                    float *centsOffsetReturn = 0,
                                    float concertA = 440.0);
};


#endif
