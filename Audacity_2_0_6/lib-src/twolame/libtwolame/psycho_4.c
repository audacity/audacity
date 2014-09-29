/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2001-2004 Michael Cheng
 *	Copyright (C) 2004-2006 The TwoLAME Project
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation; either
 *	version 2.1 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "twolame.h"
#include "common.h"
#include "mem.h"
#include "fft.h"
#include "ath.h"
#include "psycho_4.h"

/****************************************************************
PSYCHO_4 by MFC Feb 2003

This is a cleaned up implementation of psy model 2.
This is basically because I was sick of the inconsistencies between
the notation in the ISO docs and in the sourcecode.

I've nicked a bunch of stuff from LAME to make this a bit easier to grok
- ATH values (this also overcomes the lack of mpeg-2 tables
  which meant that LSF never had proper values)
- ath_freq2bark() to convert frequencies directly to bark values.
- spreading_function() isolated the calculation of the spreading function.
  Basically the same code as before, just isolated in its own function.
  LAME seem to does some extra tweaks to the ISO1117s model.
  Not really sure if they help or hinder, so I've commented them out (#ifdef LAME)

NB: Because of some of the tweaks to bark value calculation etc, it is now possible
to have 64 CBANDS. There's no real limit on the actual number of paritions. 
I wonder if it's worth experimenting with really higher numbers? Probably won't make
that much difference to the final SNR values, but it's something worth trying
	Maybe CBANDS should be a dynamic value, calculated by the psycho_init function
	CBANDS definition has been changed in encoder.h from 63 to 64

****************************************************************/


/* The static variables "r", "phi_sav", "new", "old" and "oldest" have	  
 to be remembered for the unpredictability measure.	 For "r" and		
 "phi_sav", the first index from the left is the channel select and		
 the second index is the "age" of the data.								*/


/* NMT is a constant 5.5dB. ISO11172 Sec D.2.4.h */
static const FLOAT NMT = 5.5;

/* The index into this array is a bark value 
   This array gives the 'minval' values from ISO11172 Tables D.3.x */
static const FLOAT minval[27] = {
    0.0,                        /* bark = 0 */
    20.0,                       /* 1 */
    20.0,                       /* 2 */
    20.0,                       /* 3 */
    20.0,                       /* 4 */
    20.0,                       /* 5 */
    17.0,                       /* 6 */
    15.0,                       /* 7 */
    10.0,                       /* 8 */
    7.0,                        /* 9 */
    4.4,                        /* 10 */
    4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,  /* 11 - 25 */
    3.5                         /* 26 */
};


/* Table covers angles from	 0 to TRIGTABLESIZE/TRIGTABLESCALE (3.142) radians 
   In steps of 1/TRIGTABLESCALE (0.0005) radians. 
   Largest absolute error: 0.0005
   Only create a table for cos, and then use trig to work out sin.
   sin(theta) = cos(PI/2 - theta)
   MFC March 2003 */
static void psycho_4_trigtable_init(psycho_4_mem * p4mem)
{

    int i;
    for (i = 0; i < TRIGTABLESIZE; i++) {
        p4mem->cos_table[i] = cos((FLOAT) i / TRIGTABLESCALE);
    }
}

#ifdef NEWTAN
static inline FLOAT psycho_4_cos(psycho_4_mem * p4mem, FLOAT phi)
{
    int index;
    int sign = 1;

    index = (int) (fabs(phi) * TRIGTABLESCALE);
    while (index >= TRIGTABLESIZE) {
        /* If we're larger than PI, then subtract PI until we aren't each time the sign will flip - 
           Year 11 trig again. MFC March 2003 */
        index -= TRIGTABLESIZE;
        sign *= -1;
    }
    return (sign * p4mem->cos_table[index]);
}
#endif

/* The spreading function.	Values returned in units of energy
   Argument 'bark' is the difference in bark values between the
   centre of two partitions.
   This has been taken from LAME. MFC Feb 2003 */
static FLOAT psycho_4_spreading_function(FLOAT bark)
{

    FLOAT tempx, x, tempy, temp;
    tempx = bark;
#ifdef LAME
    /* MP3 standard actually spreads these values a little more */
    if (tempx >= 0)
        tempx *= 3;
    else
        tempx *= 1.5;
#endif

    if (tempx >= 0.5 && tempx <= 2.5) {
        temp = tempx - 0.5;
        x = 8.0 * (temp * temp - 2.0 * temp);
    } else
        x = 0.0;
    tempx += 0.474;
    tempy = 15.811389 + 7.5 * tempx - 17.5 * sqrt(1.0 + tempx * tempx);

    if (tempy <= -60.0)
        return 0.0;

    tempx = exp((x + tempy) * LN_TO_LOG10);

#ifdef LAME
    /* I'm not sure where the magic value of 0.6609193 comes from. twolame will just keep using the 
       rnorm to normalise the spreading function MFC Feb 2003 */
    /* Normalization.  The spreading function should be normalized so that: +inf / | s3 [ bark ]
       d(bark) = 1 / -inf */
    tempx /= .6609193;
#endif
    return tempx;

}

/********************************
 * init psycho model 2
 ********************************/
static psycho_4_mem *psycho_4_init(twolame_options * glopts, int sfreq)
{
    psycho_4_mem *mem;
    FLOAT *cbval, *rnorm;
    FLOAT *window;
    FLOAT bark[HBLKSIZE], *ath;
    int *numlines;
    int *partition;
    FCB *s;
    FLOAT *tmn;
    int i, j;

    {
        mem = (psycho_4_mem *) TWOLAME_MALLOC(sizeof(psycho_4_mem));

        mem->tmn = (FLOAT *) TWOLAME_MALLOC(sizeof(DCB));
        mem->s = (FCB *) TWOLAME_MALLOC(sizeof(FCBCB));
        mem->lthr = (FHBLK *) TWOLAME_MALLOC(sizeof(F2HBLK));
        mem->r = (F2HBLK *) TWOLAME_MALLOC(sizeof(F22HBLK));
        mem->phi_sav = (F2HBLK *) TWOLAME_MALLOC(sizeof(F22HBLK));

        mem->new = 0;
        mem->old = 1;
        mem->oldest = 0;
    }

    {
        cbval = mem->cbval;
        rnorm = mem->rnorm;
        window = mem->window;
        // bark = mem->bark;
        ath = mem->ath;
        numlines = mem->numlines;
        partition = mem->partition;
        s = mem->s;
        tmn = mem->tmn;
    }


    /* Set up the SIN/COS tables */
    psycho_4_trigtable_init(mem);

    /* calculate HANN window coefficients */
    for (i = 0; i < BLKSIZE; i++)
        window[i] = 0.5 * (1 - cos(2.0 * PI * (i - 0.5) / BLKSIZE));

    /* For each FFT line from 0(DC) to 512(Nyquist) calculate - bark : the bark value of this fft
       line - ath : the absolute threshold of hearing for this line [ATH]

       Since it is a 1024 point FFT, each line in the fft corresponds to 1/1024 of the total
       frequency. Line 0 should correspond to DC - which doesn't really have a ATH afaik Line 1
       should be 1/1024th of the Sampling Freq Line 512 should be the nyquist freq */
    for (i = 0; i < HBLKSIZE; i++) {
        FLOAT freq = i * (FLOAT) sfreq / (FLOAT) BLKSIZE;
        bark[i] = ath_freq2bark(freq);
        /* The ath tables in the dist10 code seem to be a little out of kilter. they seem to start
           with index 0 corresponding to (sampling freq)/1024. When in doubt, i'm going to assume
           that the dist10 code is wrong. MFC Feb2003 */
        ath[i] = ath_energy(freq, glopts->athlevel);
        // fprintf(stderr,"%.2f ",ath[i]);
    }


    /* Work out the partitions Starting from line 0, all lines within 0.33 of the starting bark are 
       added to the same partition. When a line is greater by 0.33 of a bark, start a new
       partition. */
    {
        int partition_count = 0;    /* keep a count of the partitions */
        int cbase = 0;          /* current base index for the bark range calculation */
        for (i = 0; i < HBLKSIZE; i++) {
            if ((bark[i] - bark[cbase]) > 0.33) {   /* 1/3 critical band? */
                /* this frequency line is too different from the starting line, (in terms of the
                   bark distance) so close that previous partition, and make this line the first
                   member of the next partition */
                cbase = i;      /* Start the new partition from this frequency */
                partition_count++;
            }
            /* partition[i] tells us which partition the i'th frequency line is in */
            partition[i] = partition_count;
            /* keep a count of how many frequency lines are in each partition */
            numlines[partition_count]++;
        }
    }

    /* For each partition within the frequency space, calculate the average bark value - cbval
       [central bark value] */
    for (i = 0; i < HBLKSIZE; i++)
        cbval[partition[i]] += bark[i]; /* sum up all the bark values */
    for (i = 0; i < CBANDS; i++) {
        if (numlines[i] != 0)
            cbval[i] /= numlines[i];    /* divide by the number of values */
        else {
            cbval[i] = 0;       /* this isn't a partition */
        }
    }


    /* Calculate the spreading function. ISO 11172 Section D.2.3 */
    for (i = 0; i < CBANDS; i++) {
        for (j = 0; j < CBANDS; j++) {
            s[i][j] = psycho_4_spreading_function(1.05 * (cbval[i] - cbval[j]));
            rnorm[i] += s[i][j];    /* sum the spreading function values for each partition so that
                                       they can be normalised later on */
        }
    }

    /* Calculate Tone Masking Noise values. ISO 11172 Tables D.3.x */
    for (j = 0; j < CBANDS; j++)
        tmn[j] = MAX(15.5 + cbval[j], 24.5);


    if (glopts->verbosity > 6) {
        /* Dump All the Values to STDERR */
        int wlow, whigh = 0;
        int ntot = 0;
        fprintf(stderr, "psy model 4 init\n");
        fprintf(stderr, "index \tnlines \twlow \twhigh \tbval \tminval \ttmn\n");
        for (i = 0; i < CBANDS; i++)
            if (numlines[i] != 0) {
                wlow = whigh + 1;
                whigh = wlow + numlines[i] - 1;
                fprintf(stderr, "%i \t%i \t%i \t%i \t%5.2f \t%4.2f \t%4.2f\n", i + 1, numlines[i],
                        wlow, whigh, cbval[i], minval[(int) cbval[i]], tmn[i]);
                ntot += numlines[i];
            }
        fprintf(stderr, "total lines %i\n", ntot);
    }

    return (mem);
}


void psycho_4(twolame_options * glopts,
              short int buffer[2][1152], short int savebuf[2][1056], FLOAT smr[2][32])
/* to match prototype : FLOAT args are always FLOAT */
{
    psycho_4_mem *mem;
    unsigned int run, i, j, k, ch;
    FLOAT r_prime, phi_prime;
    FLOAT npart, epart;
    int new, old, oldest;
    FLOAT *grouped_c, *grouped_e;
    FLOAT *nb, *cb, *tb, *ecb, *bc;
    FLOAT *cbval, *rnorm;
    FLOAT *wsamp_r, *phi, *energy, *window;
    FLOAT *ath, *thr, *c;

    FLOAT *snrtmp[2];
    int *numlines;
    int *partition;
    FLOAT *tmn;
    FCB *s;
    FHBLK *lthr;
    F2HBLK *r, *phi_sav;

    int nch = glopts->num_channels_out;
    int sfreq = glopts->samplerate_out;

    if (!glopts->p4mem) {
        glopts->p4mem = psycho_4_init(glopts, sfreq);
    }

    mem = glopts->p4mem;
    {
        grouped_c = mem->grouped_c;
        grouped_e = mem->grouped_e;
        nb = mem->nb;
        cb = mem->cb;
        tb = mem->tb;
        ecb = mem->ecb;
        bc = mem->bc;
        rnorm = mem->rnorm;
        cbval = mem->cbval;
        wsamp_r = mem->wsamp_r;
        phi = mem->phi;
        energy = mem->energy;
        window = mem->window;
        ath = mem->ath;
        thr = mem->thr;
        c = mem->c;

        snrtmp[0] = mem->snrtmp[0];
        snrtmp[1] = mem->snrtmp[1];

        numlines = mem->numlines;
        partition = mem->partition;
        tmn = mem->tmn;
        s = mem->s;
        lthr = mem->lthr;
        r = mem->r;
        phi_sav = mem->phi_sav;
    }

    for (ch = 0; ch < nch; ch++) {
        for (run = 0; run < 2; run++) {
            /* Net offset is 480 samples (1056-576) for layer 2; this is because one must stagger
               input data by 256 samples to synchronize psychoacoustic model with filter bank
               outputs, then stagger so that center of 1024 FFT window lines up with center of 576
               "new" audio samples.

               flush = 384*3.0/2.0; = 576 syncsize = 1056; sync_flush = syncsize - flush; 480
               BLKSIZE = 1024 */
            {
                short int *bufferp = buffer[ch];
                for (j = 0; j < 480; j++) {
                    savebuf[ch][j] = savebuf[ch][j + 576];
                    wsamp_r[j] = window[j] * ((FLOAT) savebuf[ch][j]);
                }
                for (; j < 1024; j++) {
                    savebuf[ch][j] = *bufferp++;
                    wsamp_r[j] = window[j] * ((FLOAT) savebuf[ch][j]);
                }
                for (; j < 1056; j++)
                    savebuf[ch][j] = *bufferp++;
            }

            /* Compute FFT */
            psycho_2_fft(wsamp_r, energy, phi);

            /* calculate the unpredictability measure, given energy[f] and phi[f] (the age pointers 
               [new/old/oldest] are reset automatically on the second pass */
            {
                if (mem->new == 0) {
                    mem->new = 1;
                    mem->oldest = 1;
                } else {
                    mem->new = 0;
                    mem->oldest = 0;
                }
                if (mem->old == 0)
                    mem->old = 1;
                else
                    mem->old = 0;
            }
            old = mem->old;
            new = mem->new;
            oldest = mem->oldest;


            for (j = 0; j < HBLKSIZE; j++) {
#ifdef NEWATAN
                FLOAT temp1, temp2, temp3;
                r_prime = 2.0 * r[ch][old][j] - r[ch][oldest][j];
                phi_prime = 2.0 * phi_sav[ch][old][j] - phi_sav[ch][oldest][j];

                r[ch][new][j] = sqrt((FLOAT) energy[j]);
                phi_sav[ch][new][j] = phi[j];

                {
                    temp1 =
                        r[ch][new][j] * psycho_4_cos(mem, phi[j]) -
                        r_prime * psycho_4_cos(mem, phi_prime);
                    /* Remember your grade 11 trig? sin(theta) = cos(PI/2 - theta) */
                    temp2 =
                        r[ch][new][j] * psycho_4_cos(mem, PI2 - phi[j]) -
                        r_prime * psycho_4_cos(mem, PI2 - phi_prime);
                }


                temp3 = r[ch][new][j] + fabs((FLOAT) r_prime);
                if (temp3 != 0)
                    c[j] = sqrt(temp1 * temp1 + temp2 * temp2) / temp3;
                else
                    c[j] = 0;
#else
                FLOAT temp1, temp2, temp3;
                r_prime = 2.0 * r[ch][old][j] - r[ch][oldest][j];
                phi_prime = 2.0 * phi_sav[ch][old][j] - phi_sav[ch][oldest][j];

                r[ch][new][j] = sqrt((FLOAT) energy[j]);
                phi_sav[ch][new][j] = phi[j];


                temp1 = r[ch][new][j] * cos((FLOAT) phi[j]) - r_prime * cos((FLOAT) phi_prime);
                temp2 = r[ch][new][j] * sin((FLOAT) phi[j]) - r_prime * sin((FLOAT) phi_prime);

                temp3 = r[ch][new][j] + fabs((FLOAT) r_prime);
                if (temp3 != 0)
                    c[j] = sqrt(temp1 * temp1 + temp2 * temp2) / temp3;
                else
                    c[j] = 0;
#endif
            }

            /* For each partition, sum all the energy in that partition - grouped_e and calculated
               the energy-weighted unpredictability measure - grouped_c ISO 11172 Section D.2.4.e */
            for (j = 1; j < CBANDS; j++) {
                grouped_e[j] = 0;
                grouped_c[j] = 0;
            }
            grouped_e[0] = energy[0];
            grouped_c[0] = energy[0] * c[0];
            for (j = 1; j < HBLKSIZE; j++) {
                grouped_e[partition[j]] += energy[j];
                grouped_c[partition[j]] += energy[j] * c[j];
            }

            /* convolve the grouped energy-weighted unpredictability measure and the grouped energy 
               with the spreading function ISO 11172 D.2.4.f */
            for (j = 0; j < CBANDS; j++) {
                ecb[j] = 0;
                cb[j] = 0;
                for (k = 0; k < CBANDS; k++) {
                    if (s[j][k] != 0.0) {
                        ecb[j] += s[j][k] * grouped_e[k];
                        cb[j] += s[j][k] * grouped_c[k];
                    }
                }
                if (ecb[j] != 0)
                    cb[j] = cb[j] / ecb[j];
                else
                    cb[j] = 0;
            }

            /* Convert cb to tb (the tonality index) ISO11172 SecD.2.4.g */
            for (i = 0; i < CBANDS; i++) {
                if (cb[i] < 0.05)
                    cb[i] = 0.05;
                else if (cb[i] > 0.5)
                    cb[i] = 0.5;
                tb[i] = -0.301029996 - 0.434294482 * log((FLOAT) cb[i]);
            }


            /* Calculate the required SNR for each of the frequency partitions ISO 11172 Sect
               D.2.4.h */
            for (j = 0; j < CBANDS; j++) {
                FLOAT SNR, SNRtemp;
                SNRtemp = tmn[j] * tb[j] + NMT * (1.0 - tb[j]);
                SNR = MAX(SNRtemp, minval[(int) cbval[j]]);
                bc[j] = exp((FLOAT) - SNR * LN_TO_LOG10);
            }

            /* Calculate the permissible noise energy level in each of the frequency partitions.
               This section used to have pre-echo control but only for LayerI ISO 11172 Sec D.2.4.k 
               - Spread the threshold energy over FFT lines */
            for (j = 0; j < CBANDS; j++) {
                if (rnorm[j] && numlines[j])
                    nb[j] = ecb[j] * bc[j] / (rnorm[j] * numlines[j]);
                else
                    nb[j] = 0;
            }

            /* ISO11172 Sec D.2.4.l - thr[] the final energy threshold of audibility */
            for (j = 0; j < HBLKSIZE; j++)
                thr[j] = MAX(nb[partition[j]], ath[j]);

            /* Translate the 512 threshold values to the 32 filter bands of the coder Using ISO
               11172 Table D.5 and Section D.2.4.n */
            for (j = 0; j < 193; j += 16) {
                /* WIDTH = 0 */
                npart = 60802371420160.0;
                epart = 0.0;
                for (k = 0; k < 17; k++) {
                    if (thr[j + k] < npart)
                        npart = thr[j + k]; /* For WIDTH==0, find the minimum noise, and later
                                               multiply by the number of indexes i.e. 17 */
                    epart += energy[j + k];
                }
                snrtmp[run][j / 16] = 4.342944819 * log((FLOAT) (epart / (npart * 17.0)));
            }
            for (j = 208; j < (HBLKSIZE - 1); j += 16) {
                /* WIDTH = 1 */
                npart = 0.0;
                epart = 0.0;
                for (k = 0; k < 17; k++) {
                    npart += thr[j + k];    /* For WIDTH==1, sum the noise */
                    epart += energy[j + k];
                }
                snrtmp[run][j / 16] = 4.342944819 * log((FLOAT) (epart / npart));
            }
        }

        /* Pick the maximum value of the two runs ISO 11172 Sect D.2.1 */
        for (i = 0; i < 32; i++)
            smr[ch][i] = MAX(snrtmp[0][i], snrtmp[1][i]);

    }                           // now do other channel

}


void psycho_4_deinit(psycho_4_mem ** mem)
{

    if (mem == NULL || *mem == NULL)
        return;

    TWOLAME_FREE((*mem)->tmn);
    TWOLAME_FREE((*mem)->s);
    TWOLAME_FREE((*mem)->lthr);
    TWOLAME_FREE((*mem)->r);
    TWOLAME_FREE((*mem)->phi_sav);

    TWOLAME_FREE((*mem));
}



// vim:ts=4:sw=4:nowrap: 
