////////////////////////////////////////////////////////////////////////////////
///
/// Peak detection routine. 
///
/// The routine detects highest value on an array of values and calculates the 
/// precise peak location as a mass-center of the 'hump' around the peak value.
///
/// Author        : Copyright (c) Olli Parviainen
/// Author e-mail : oparviai 'at' iki.fi
/// SoundTouch WWW: http://www.surina.net/soundtouch
///
////////////////////////////////////////////////////////////////////////////////
//
// Last changed  : $Date: 2006-09-18 07:31:48 $
// File revision : $Revision: 1.2 $
//
// $Id: PeakFinder.cpp,v 1.2 2006-09-18 07:31:48 richardash1981 Exp $
//
////////////////////////////////////////////////////////////////////////////////
//
// License :
//
//  SoundTouch audio processing library
//  Copyright (c) Olli Parviainen
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
////////////////////////////////////////////////////////////////////////////////

#include <math.h>
#include <assert.h>

#include "PeakFinder.h"


PeakFinder::PeakFinder()
{
}


// Finds 'ground level' of a peak hump by starting from 'peakpos' and proceeding
// to direction defined by 'direction' until next 'hump' after minimum value will 
// begin
int PeakFinder::findGround(const float *data, int peakpos, int direction) const
{
    float refvalue;
    int lowpos;
    int pos;
    int climb_count;
    float delta;

    climb_count = 0;
    refvalue = data[peakpos];
    lowpos = peakpos;

    pos = peakpos;

    while ((pos > minPos) && (pos < maxPos))
    {
        int prevpos;

        prevpos = pos;
        pos += direction;

        // calculate derivate
        delta = data[pos] - data[prevpos];
        if (delta <= 0)
        {
            // going downhill, ok
            if (climb_count)
            {
                climb_count --;  // decrease climb count
            }

            // check if new minimum found
            if (data[pos] < refvalue)
            {
                // new minimum found
                lowpos = pos;
                refvalue = data[pos];
            }
        }
        else
        {
            // going uphill, increase climbing counter
            climb_count ++;
            if (climb_count > 5) break;    // we've been climbing too long => it's next uphill => quit
        }
    }
    return lowpos;
}


// Find offset where the value crosses the given level, when starting from 'peakpos' and
// proceeds to direction defined in 'direction'
int PeakFinder::findCrossingLevel(const float *data, float level, int peakpos, int direction) const
{
    float peaklevel;
    int pos;

    peaklevel = data[peakpos];
    assert(peaklevel >= level);
    pos = peakpos;
    while ((pos >= minPos) && (pos < maxPos))
    {
        if (data[pos + direction] < level) return pos;   // crossing found
        pos += direction;
    }
    return -1;  // not found
}


// Calculates the center of mass location of 'data' array items between 'firstPos' and 'lastPos'
float PeakFinder::calcMassCenter(const float *data, int firstPos, int lastPos) const
{
    int i;
    float sum;
    float wsum;

    sum = 0;
    wsum = 0;
    for (i = firstPos; i <= lastPos; i ++)
    {
        sum += (float)i * data[i];
        wsum += data[i];
    }
    return sum / wsum;
}


float PeakFinder::detectPeak(const float *data, int minPos, int maxPos) 
{
    #define max(x, y) (((x) > (y)) ? (x) : (y))

    int i;
    int peakpos;                // position of peak level
    float peakLevel;            // peak level
    int crosspos1, crosspos2;   // position where the peak 'hump' crosses cutting level
    float cutLevel;             // cutting value
    float groundLevel;          // ground level of the peak
    int gp1, gp2;               // bottom positions of the peak 'hump'

    this->minPos = minPos;
    this->maxPos = maxPos;

    // find absolute peak
    peakpos = minPos;
    peakLevel = data[minPos];
    for (i = minPos + 1; i < maxPos; i ++)
    {
        if (data[i] > peakLevel) 
        {
            peakLevel = data[i];
            peakpos = i;
        }
    }
    
    // find ground positions.
    gp1 = findGround(data, peakpos, -1);
    gp2 = findGround(data, peakpos, 1);

    groundLevel = max(data[gp1], data[gp2]);

    if (groundLevel < 1e-6) return 0;                // ground level too small => detection failed
    if ((peakLevel / groundLevel) < 1.3) return 0;   // peak less than 30% of the ground level => no good peak detected

    // calculate 70%-level of the peak
    cutLevel = 0.70f * peakLevel + 0.30f * groundLevel;
    // find mid-level crossings
    crosspos1 = findCrossingLevel(data, cutLevel, peakpos, -1);
    crosspos2 = findCrossingLevel(data, cutLevel, peakpos, 1);

    if ((crosspos1 < 0) || (crosspos2 < 0)) return 0;   // no crossing, no peak..

    // calculate mass center of the peak surroundings
    return calcMassCenter(data, crosspos1, crosspos2);
}


