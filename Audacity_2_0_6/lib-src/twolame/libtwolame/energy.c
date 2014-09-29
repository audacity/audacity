/*
 *  TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *  Copyright (C) 2001-2004 Michael Cheng
 *  Copyright (C) 2004-2006 The TwoLAME Project
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */


#include <stdio.h>
#include <stdlib.h>

#include "twolame.h"
#include "common.h"
#include "bitbuffer.h"
#include "energy.h"



// Returns the desired number of ancillary bits to be 
// reserved at the end of the frame
int get_required_energy_bits(twolame_options * glopts)
{
    if (glopts->mode == TWOLAME_MONO) {
        // only 2 bytes + zero at n-3 needed for energy level for mono channel
        return 24;

    } else {
        // 5 bytes for the stereo energy info
        return 40;
    }

}


// Calculates the energy levels of current frame and
// inserts it into the end of the frame
void do_energy_levels(twolame_options * glopts, bit_stream * bs)
{
    /* Reference: Using the BWF Energy Levels in AudioScience Bitstreams
       http://www.audioscience.com/internet/download/notes/note0001_MPEG_energy.pdf

       Specification of the Broadcast Wave Format: Supplement 1 - MPEG audio
       http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s1_tcm6-10545.pdf
       http://www.sr.se/utveckling/tu/bwf/

       The absolute peak of the PCM file for the left and right channel in this frame are written
       into the last 5 bytes of the frame.

       The last 5 bytes *must* be reserved for this to work correctly (otherwise you'll be
       overwriting mpeg audio data) */

    short int *leftpcm = glopts->buffer[0];
    short int *rightpcm = glopts->buffer[1];

    int i, leftMax, rightMax;
    unsigned char rhibyte, rlobyte, lhibyte, llobyte;

    // Get the position (in butes) of the end of the mpeg audio frame
    int frameEnd = buffer_sstell(bs) / 8;


    // find the maximum in the left and right channels
    leftMax = rightMax = -1;
    for (i = 0; i < TWOLAME_SAMPLES_PER_FRAME; i++) {
        if (abs(leftpcm[i]) > leftMax)
            leftMax = abs(leftpcm[i]);
        if (abs(rightpcm[i]) > rightMax)
            rightMax = abs(rightpcm[i]);
    }



    // fix any overflows
    if (leftMax > 32767)
        leftMax = 32767;

    if (rightMax > 32767)
        rightMax = 32767;



    // convert max value to hi/lo bytes and write into buffer
    lhibyte = leftMax / 256;
    llobyte = leftMax - 256 * lhibyte;

    // Write the left channel into the last two bytes of the frame
    bs->buf[frameEnd - 1] = llobyte;
    bs->buf[frameEnd - 2] = lhibyte;
    bs->buf[frameEnd - 3] = 0;

    // Only write the right channel energy info
    // if we're in stereo mode.
    if (glopts->mode != TWOLAME_MONO) {

        rhibyte = rightMax / 256;
        rlobyte = rightMax - 256 * rhibyte;

        bs->buf[frameEnd - 4] = rlobyte;
        bs->buf[frameEnd - 5] = rhibyte;
    }


}


// vim:ts=4:sw=4:nowrap: 
