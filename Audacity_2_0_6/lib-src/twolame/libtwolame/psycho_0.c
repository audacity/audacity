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
#include <math.h>

#include "twolame.h"
#include "common.h"
#include "ath.h"
#include "mem.h"
#include "psycho_0.h"

/* MFC Mar 03
   It's almost obscene how well this psycho model works for the amount of 
   computational effort that's put in.

   I got the idea from:
   Hyen-O Oh et al "Low power mpeg audio encoders using simplified psychoacoustic model
					and fast bit allocation"
					IEEE Trans on Consumer Electronics v47 n3 August 2001. p613

   All this model does is look at the lowest ATH value within the subband, and then looks
   at the scalefactors. It combines the two in a real dodgy way to get the SMRs.

   Although the output values aren't really close to any of the other psycho models, 
   the spread of values and the relative sizes of the values for the different subbands
   is about right 

   Feel free to make any sort of generic change you want. Add or subtract numbers, take
   logs, whatever. Fiddle with the numbers until we get a good SMR output */


static psycho_0_mem *psycho_0_init(twolame_options * glopts, int sfreq)
{
    FLOAT freqperline = (FLOAT) sfreq / 1024.0;
    psycho_0_mem *mem = (psycho_0_mem *) TWOLAME_MALLOC(sizeof(psycho_0_mem));
    int sb, i;

    for (sb = 0; sb < SBLIMIT; sb++) {
        mem->ath_min[sb] = 1000;    /* set it huge */
    }

    /* Find the minimum ATH in each subband */
    for (i = 0; i < 512; i++) {
        FLOAT thisfreq = i * freqperline;
        FLOAT ath_val = ath_db(thisfreq, 0);
        if (ath_val < mem->ath_min[i >> 4])
            mem->ath_min[i >> 4] = ath_val;
    }

    return mem;
}



void psycho_0(twolame_options * glopts, FLOAT SMR[2][SBLIMIT], unsigned int scalar[2][3][SBLIMIT])
{
    psycho_0_mem *mem;
    int nch = glopts->num_channels_out;
    int sfreq = glopts->samplerate_out;
    int ch, sb, gr;
    unsigned int minscaleindex[2][SBLIMIT]; /* Smaller scale indexes mean bigger scalefactors */

    if (!glopts->p0mem) {
        glopts->p0mem = psycho_0_init(glopts, sfreq);
    }
    mem = glopts->p0mem;


    /* call functions for critical boundaries, freq. */
    if (!glopts->p0mem) {       /* bands, bark values, and mapping */

    } else {

        mem = glopts->p0mem;

    }


    /* Find the minimum scalefactor index for each ch/sb */
    for (ch = 0; ch < nch; ch++)
        for (sb = 0; sb < SBLIMIT; sb++)
            minscaleindex[ch][sb] = scalar[ch][0][sb];

    for (ch = 0; ch < nch; ch++)
        for (gr = 1; gr < 3; gr++)
            for (sb = 0; sb < SBLIMIT; sb++)
                if (minscaleindex[ch][sb] > scalar[ch][gr][sb])
                    minscaleindex[ch][sb] = scalar[ch][gr][sb];

    /* Oh yeah. Fudge the hell out of the SMR calculations by combining the scalefactor table index 
       and the min ATH in that subband There are probably more elegant/correct ways of combining
       these values, but who cares? It works pretty well MFC Mar 03 */
    for (ch = 0; ch < nch; ch++)
        for (sb = 0; sb < SBLIMIT; sb++)
            SMR[ch][sb] = 2.0 * (30.0 - minscaleindex[ch][sb]) - mem->ath_min[sb];
}


void psycho_0_deinit(psycho_0_mem ** mem)
{

    if (mem == NULL || *mem == NULL)
        return;

    TWOLAME_FREE(*mem);
}



// vim:ts=4:sw=4:nowrap: 
