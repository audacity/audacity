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
#include "psycho_2.h"

/* The static variables "r", "phi_sav", "new", "old" and "oldest" have	  */
/* to be remembered for the unpredictability measure.  For "r" and		  */
/* "phi_sav", the first index from the left is the channel select and	  */
/* the second index is the "age" of the data.							  */


/* The following static variables are constants.						   */

static const FLOAT nmt = 5.5;

static const FLOAT crit_band[27] = { 0, 100, 200, 300, 400, 510, 630, 770,
    920, 1080, 1270, 1480, 1720, 2000, 2320, 2700,
    3150, 3700, 4400, 5300, 6400, 7700, 9500, 12000,
    15500, 25000, 30000
};

static const FLOAT bmax[27] = { 20.0, 20.0, 20.0, 20.0, 20.0, 17.0, 15.0,
    10.0, 7.0, 4.4, 4.5, 4.5, 4.5, 4.5,
    4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,
    4.5, 4.5, 4.5, 3.5, 3.5, 3.5
};

static void psycho_2_read_absthr(absthr, table)
 FLOAT *absthr;
 int table;
{
    int j;
#include "psycho_2_absthr.h"

    if ((table < 0) || (table > 3)) {
        fprintf(stderr, "internal error: wrong table number");
        return;
    }

    for (j = 0; j < HBLKSIZE; j++) {
        absthr[j] = absthr_table[table][j];
    }
    return;
}

/********************************
 * init psycho model 2
 ********************************/
psycho_2_mem *psycho_2_init(twolame_options * glopts, int sfreq)
{
    psycho_2_mem *mem;
    FLOAT *cbval, *rnorm;
    FLOAT *window;
    FLOAT *ath;
    int *numlines;
    int *partition;
    FCB *s;
    FLOAT *tmn;

    int i, j, itemp2;
    FLOAT freq_mult;
    FLOAT temp1, ftemp2, temp3;
    FLOAT bval_lo, *fthr;

    int sfreq_idx;

    {
        mem = (psycho_2_mem *) TWOLAME_MALLOC(sizeof(psycho_2_mem));
        if (!mem)
            return NULL;

        mem->tmn = (FLOAT *) TWOLAME_MALLOC(sizeof(DCB));
        mem->s = (FCB *) TWOLAME_MALLOC(sizeof(FCBCB));
        mem->lthr = (FHBLK *) TWOLAME_MALLOC(sizeof(F2HBLK));
        mem->r = (F2HBLK *) TWOLAME_MALLOC(sizeof(F22HBLK));
        mem->phi_sav = (F2HBLK *) TWOLAME_MALLOC(sizeof(F22HBLK));

        // static int new = 0, old = 1, oldest = 0;
        mem->new = 0;
        mem->old = 1;
        mem->oldest = 0;

        mem->flush = (int) (384 * 3.0 / 2.0);
        mem->syncsize = 1056;
        mem->sync_flush = mem->syncsize - mem->flush;
    }

    {
        cbval = mem->cbval;
        rnorm = mem->rnorm;
        window = mem->window;
        ath = mem->ath;
        numlines = mem->numlines;
        partition = mem->partition;
        s = mem->s;
        tmn = mem->tmn;
        fthr = mem->fthr;
    }

    switch (sfreq) {
    case 32000:
    case 16000:
        sfreq_idx = 0;
        break;
    case 44100:
    case 22050:
        sfreq_idx = 1;
        break;
    case 48000:
    case 24000:
        sfreq_idx = 2;
        break;
    default:
        fprintf(stderr, "error, invalid sampling frequency: %d Hz\n", sfreq);
        return NULL;
    }
    fprintf(stderr, "absthr[][] sampling frequency index: %d\n", sfreq_idx);
    psycho_2_read_absthr(mem->absthr, sfreq_idx);


    /* calculate HANN window coefficients */
    /* for(i=0;i<BLKSIZE;i++)window[i]=0.5*(1-cos(2.0*PI*i/(BLKSIZE-1.0))); */
    for (i = 0; i < BLKSIZE; i++)
        window[i] = 0.5 * (1 - cos(2.0 * PI * (i - 0.5) / BLKSIZE));
    /* reset states used in unpredictability measure */
    for (i = 0; i < HBLKSIZE; i++) {
        mem->r[0][0][i] = mem->r[1][0][i] = mem->r[0][1][i] = mem->r[1][1][i] = 0;
        mem->phi_sav[0][0][i] = mem->phi_sav[1][0][i] = 0;
        mem->phi_sav[0][1][i] = mem->phi_sav[1][1][i] = 0;
        mem->lthr[0][i] = 60802371420160.0;
        mem->lthr[1][i] = 60802371420160.0;
    }
  /*****************************************************************************
   * Initialization: Compute the following constants for use later			   *
   *	partition[HBLKSIZE] = the partition number associated with each		   *
   *						  frequency line								   *
   *	cbval[CBANDS]		= the center (average) bark value of each		   *
   *						  partition										   *
   *	numlines[CBANDS]	= the number of frequency lines in each partition  *
   *	tmn[CBANDS]			= tone masking noise							   *
   *****************************************************************************/
    /* compute fft frequency multiplicand */
    freq_mult = (FLOAT) sfreq / (FLOAT) BLKSIZE;

    /* calculate fft frequency, then bval of each line (use fthr[] as tmp storage) */
    for (i = 0; i < HBLKSIZE; i++) {
        temp1 = i * freq_mult;
        j = 1;
        while (temp1 > crit_band[j])
            j++;
        fthr[i] = j - 1 + (temp1 - crit_band[j - 1]) / (crit_band[j] - crit_band[j - 1]);
    }
    partition[0] = 0;
    /* temp2 is the counter of the number of frequency lines in each partition */
    itemp2 = 1;
    cbval[0] = fthr[0];
    bval_lo = fthr[0];
    for (i = 1; i < HBLKSIZE; i++) {
        if ((fthr[i] - bval_lo) > 0.33) {
            partition[i] = partition[i - 1] + 1;
            cbval[partition[i - 1]] = cbval[partition[i - 1]] / itemp2;
            cbval[partition[i]] = fthr[i];
            bval_lo = fthr[i];
            numlines[partition[i - 1]] = itemp2;
            itemp2 = 1;
        } else {
            partition[i] = partition[i - 1];
            cbval[partition[i]] += fthr[i];
            itemp2++;
        }
    }
    numlines[partition[i - 1]] = itemp2;
    cbval[partition[i - 1]] = cbval[partition[i - 1]] / itemp2;

  /************************************************************************
   * Now compute the spreading function, s[j][i], the value of the spread-*
   * ing function, centered at band j, for band i, store for later use	  *
   ************************************************************************/
    for (j = 0; j < CBANDS; j++) {
        for (i = 0; i < CBANDS; i++) {
            temp1 = (cbval[i] - cbval[j]) * 1.05;
            if (temp1 >= 0.5 && temp1 <= 2.5) {
                ftemp2 = temp1 - 0.5;
                ftemp2 = 8.0 * (ftemp2 * ftemp2 - 2.0 * ftemp2);
            } else
                ftemp2 = 0.0;
            temp1 += 0.474;
            temp3 = 15.811389 + 7.5 * temp1 - 17.5 * sqrt((FLOAT) (1.0 + temp1 * temp1));
            if (temp3 <= -100)
                s[i][j] = 0;
            else {
                temp3 = (ftemp2 + temp3) * LN_TO_LOG10;
                s[i][j] = exp(temp3);
            }
        }
    }

    /* Calculate Tone Masking Noise values */
    for (j = 0; j < CBANDS; j++) {
        temp1 = 15.5 + cbval[j];
        tmn[j] = (temp1 > 24.5) ? temp1 : 24.5;
        /* Calculate normalization factors for the net spreading functions */
        rnorm[j] = 0;
        for (i = 0; i < CBANDS; i++) {
            rnorm[j] += s[j][i];
        }
    }

    if (glopts->verbosity > 5) {
        /* Dump All the Values to stderr and exit */
        int wlow, whigh = 0;
        fprintf(stderr, "psy model 2 init\n");
        fprintf(stderr, "index \tnlines \twlow \twhigh \tbval \tminval \ttmn\n");
        for (i = 0; i < CBANDS; i++) {
            wlow = whigh + 1;
            whigh = wlow + numlines[i] - 1;
            fprintf(stderr, "%i \t%i \t%i \t%i \t%5.2f \t%4.2f \t%4.2f\n", i + 1, numlines[i], wlow,
                    whigh, cbval[i], bmax[(int) (cbval[i] + 0.5)], tmn[i]);
        }
    }

    return (mem);
}

void psycho_2(twolame_options * glopts, short int buffer[2][1152],
              short int savebuf[2][1056], FLOAT smr[2][32])
{
    psycho_2_mem *mem;
    unsigned int i, j, k, ch;
    int new, old, oldest;
    FLOAT r_prime, phi_prime;
    FLOAT minthres, sum_energy;
    FLOAT tb, temp1, temp2, temp3;
    FLOAT *grouped_c, *grouped_e;
    FLOAT *nb, *cb, *ecb, *bc;
    FLOAT *cbval, *rnorm;
    FLOAT *wsamp_r, *phi, *energy, *window;
    FLOAT *ath, *thr, *c;
    FLOAT *fthr;

    FLOAT *snrtmp[2];
    int *numlines;
    int *partition;
    FLOAT *tmn;
    FCB *s;
    FHBLK *lthr;
    F2HBLK *r, *phi_sav;
    FLOAT *absthr;

    int nch = glopts->num_channels_out;
    int sfreq = glopts->samplerate_out;


    if (!glopts->p2mem) {
        glopts->p2mem = psycho_2_init(glopts, sfreq);
    }
    mem = glopts->p2mem;
    {
        grouped_c = mem->grouped_c;
        grouped_e = mem->grouped_e;
        nb = mem->nb;
        cb = mem->cb;
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
        fthr = mem->fthr;
        absthr = mem->absthr;
    }


    for (ch = 0; ch < nch; ch++) {
        for (i = 0; i < 2; i++) {
      /*****************************************************************************
	   * Net offset is 480 samples (1056-576) for layer 2; this is because one must*
	   * stagger input data by 256 samples to synchronize psychoacoustic model with*
	   * filter bank outputs, then stagger so that center of 1024 FFT window lines *
	   * up with center of 576 "new" audio samples.								   *
	   
		   flush = 384*3.0/2.0;	 = 576
		   syncsize = 1056;
		   sync_flush = syncsize - flush;	480
		   BLKSIZE = 1024
	   *****************************************************************************/
            {
                short int *bufferp = buffer[ch];
                for (j = 0; j < 480; j++) {
                    savebuf[ch][j] = savebuf[ch][j + mem->flush];
                    wsamp_r[j] = window[j] * ((FLOAT) savebuf[ch][j]);
                }
                for (; j < 1024; j++) {
                    savebuf[ch][j] = *bufferp++;
                    wsamp_r[j] = window[j] * ((FLOAT) savebuf[ch][j]);
                }
                for (; j < 1056; j++)
                    savebuf[ch][j] = *bufferp++;
            }

      /**Compute FFT****************************************************************/
            psycho_2_fft(wsamp_r, energy, phi);
      /*****************************************************************************
	   * calculate the unpredictability measure, given energy[f] and phi[f]		   *
	   *****************************************************************************/
            /* only update data "age" pointers after you are done with both channels */
            /* for layer 1 computations, for the layer 2 FLOAT computations, the pointers */
            /* are reset automatically on the second pass */
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

                new = mem->new;
                old = mem->old;
                oldest = mem->oldest;
            }


            for (j = 0; j < HBLKSIZE; j++) {
                r_prime = 2.0 * r[ch][old][j] - r[ch][oldest][j];
                phi_prime = 2.0 * phi_sav[ch][old][j] - phi_sav[ch][oldest][j];
                r[ch][new][j] = sqrt((FLOAT) energy[j]);
                phi_sav[ch][new][j] = phi[j];
#ifdef SINCOS
                {
                    // 12% faster
                    // #warning "Use __sincos"
                    FLOAT sphi, cphi, sprime, cprime;
                    __sincos((FLOAT) phi[j], &sphi, &cphi);
                    __sincos((FLOAT) phi_prime, &sprime, &cprime);
                    temp1 = r[chn][new][j] * cphi - r_prime * cprime;
                    temp2 = r[chn][new][j] * sphi - r_prime * sprime;
                }
#else
                temp1 = r[ch][new][j] * cos((FLOAT) phi[j]) - r_prime * cos((FLOAT) phi_prime);
                temp2 = r[ch][new][j] * sin((FLOAT) phi[j]) - r_prime * sin((FLOAT) phi_prime);
#endif

                temp3 = r[ch][new][j] + fabs((FLOAT) r_prime);
                if (temp3 != 0)
                    c[j] = sqrt(temp1 * temp1 + temp2 * temp2) / temp3;
                else
                    c[j] = 0;
            }
      /*****************************************************************************
	   * Calculate the grouped, energy-weighted, unpredictability measure,		   *
	   * grouped_c[], and the grouped energy. grouped_e[]						   *
	   *****************************************************************************/

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

      /*****************************************************************************
	   * convolve the grouped energy-weighted unpredictability measure			   *
	   * and the grouped energy with the spreading function, s[j][k]			   *
	   *****************************************************************************/
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

      /*****************************************************************************
	   * Calculate the required SNR for each of the frequency partitions		   *
	   *		 this whole section can be accomplished by a table lookup		   *
	   *****************************************************************************/
            for (j = 0; j < CBANDS; j++) {
                if (cb[j] < .05)
                    cb[j] = 0.05;
                else if (cb[j] > .5)
                    cb[j] = 0.5;
                tb = -0.434294482 * log((FLOAT) cb[j]) - 0.301029996;
                cb[j] = tb;
                bc[j] = tmn[j] * tb + nmt * (1.0 - tb);
                k = (int) (cbval[j] + 0.5);
                bc[j] = (bc[j] > bmax[k]) ? bc[j] : bmax[k];
                bc[j] = exp((FLOAT) - bc[j] * LN_TO_LOG10);
            }

      /*****************************************************************************
	   * Calculate the permissible noise energy level in each of the frequency	   *
	   * partitions. Include absolute threshold and pre-echo controls			   *
	   *		 this whole section can be accomplished by a table lookup		   *
	   *****************************************************************************/
            for (j = 0; j < CBANDS; j++)
                if (rnorm[j] && numlines[j])
                    nb[j] = ecb[j] * bc[j] / (rnorm[j] * numlines[j]);
                else
                    nb[j] = 0;
            for (j = 0; j < HBLKSIZE; j++) {
                /* temp1 is the preliminary threshold */
                temp1 = nb[partition[j]];
                temp1 = (temp1 > absthr[j]) ? temp1 : absthr[j];
#ifdef LAYERI
                /* do not use pre-echo control for layer 2 because it may do bad things to the */
                /* MUSICAM bit allocation algorithm */
                if (lay == 1) {
                    fthr[j] = (temp1 < lthr[ch][j]) ? temp1 : lthr[ch][j];
                    temp2 = temp1 * 0.00316;
                    fthr[j] = (temp2 > fthr[j]) ? temp2 : fthr[j];
                } else
                    fthr[j] = temp1;
                lthr[ch][j] = LXMIN * temp1;
#else
                fthr[j] = temp1;
                lthr[ch][j] = LXMIN * temp1;
#endif
            }

      /*****************************************************************************
	   * Translate the 512 threshold values to the 32 filter bands of the coder	   *
	   *****************************************************************************/
            for (j = 0; j < 193; j += 16) {
                minthres = 60802371420160.0;
                sum_energy = 0.0;
                for (k = 0; k < 17; k++) {
                    if (minthres > fthr[j + k])
                        minthres = fthr[j + k];
                    sum_energy += energy[j + k];
                }
                snrtmp[i][j / 16] = sum_energy / (minthres * 17.0);
                snrtmp[i][j / 16] = 4.342944819 * log((FLOAT) snrtmp[i][j / 16]);
            }
            for (j = 208; j < (HBLKSIZE - 1); j += 16) {
                minthres = 0.0;
                sum_energy = 0.0;
                for (k = 0; k < 17; k++) {
                    minthres += fthr[j + k];
                    sum_energy += energy[j + k];
                }
                snrtmp[i][j / 16] = sum_energy / minthres;
                snrtmp[i][j / 16] = 4.342944819 * log((FLOAT) snrtmp[i][j / 16]);
            }
      /*****************************************************************************
	   * End of Psychoacuostic calculation loop									   *
	   *****************************************************************************/
        }
        for (i = 0; i < 32; i++) {
            smr[ch][i] = (snrtmp[0][i] > snrtmp[1][i]) ? snrtmp[0][i] : snrtmp[1][i];
        }

    }                           // next channel

}

void psycho_2_deinit(psycho_2_mem ** mem)
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
