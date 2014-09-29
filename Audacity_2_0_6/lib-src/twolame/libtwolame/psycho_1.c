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

#include "twolame.h"
#include "common.h"
#include "mem.h"
#include "fft.h"
#include "psycho_1.h"

/**********************************************************************

		This module implements the psychoacoustic model I for the
 MPEG encoder layer II. It uses simplified tonal and noise masking
 threshold analysis to generate SMR for the encoder bit allocation
 routine.

**********************************************************************/


static int *psycho_1_read_cbound(int lay, int freq, int *crit_band)
/* this function reads in critical	band boundaries */
{

#include "psycho_1_critband.h"
    int *cbound;
    int i, k;

    if ((lay < 1) || (lay > 2)) {
        fprintf(stderr, "Internal error (read_cbound())\n");
        return (NULL);
    }
    if ((freq < 0) || (freq > 6) || (freq == 3)) {
        fprintf(stderr, "Internal error (read_cbound())\n");
        return (NULL);
    }

    *crit_band = SecondCriticalBand[freq][0];
    cbound = (int *) TWOLAME_MALLOC(sizeof(int) * *crit_band);
    for (i = 0; i < *crit_band; i++) {
        k = SecondCriticalBand[freq][i + 1];
        if (k != 0) {
            cbound[i] = k;
        } else {
            fprintf(stderr, "Internal error (read_cbound())\n");
            return (NULL);
        }
    }
    return (cbound);
}

/* reads in the frequency bands and bark values */
static void psycho_1_read_freq_band(g_ptr * ltg, int lay, int freq, int *sub_size)
{

#include "psycho_1_freqtable.h"

    int i, k;

    if ((freq < 0) || (freq > 6) || (freq == 3)) {
        fprintf(stderr, "Internal error (read_freq_band())\n");
        return;
    }

    /* read input for freq. subbands */

    *sub_size = SecondFreqEntries[freq] + 1;
    *ltg = (g_ptr) TWOLAME_MALLOC(sizeof(g_thres) * *sub_size);
    (*ltg)[0].line = 0;         /* initialize global masking threshold */
    (*ltg)[0].bark = 0.0;
    (*ltg)[0].hear = 0.0;
    for (i = 1; i < *sub_size; i++) {
        k = SecondFreqSubband[freq][i - 1].line;
        if (k != 0) {
            (*ltg)[i].line = k;
            (*ltg)[i].bark = SecondFreqSubband[freq][i - 1].bark;
            (*ltg)[i].hear = SecondFreqSubband[freq][i - 1].hear;
        } else {
            fprintf(stderr, "Internal error (read_freq_band())\n");
            return;
        }
    }
}


static void psycho_1_make_map(int sub_size, mask power[HAN_SIZE], g_thres * ltg)
/* this function calculates the global masking threshold */
{
    int i, j;

    for (i = 1; i < sub_size; i++)
        for (j = ltg[i - 1].line; j <= ltg[i].line; j++)
            power[j].map = i;
}

static void psycho_1_init_add_db(psycho_1_mem * mem)
{
    int i;
    FLOAT x;
    for (i = 0; i < DBTAB; i++) {
        x = (FLOAT) i / 10.0;
        mem->dbtable[i] = 10 * log10(1 + pow(10.0, x / 10.0)) - x;
    }
}

static inline FLOAT add_db(psycho_1_mem * mem, FLOAT a, FLOAT b)
{
    /* MFC - if the difference between a and b is large (>99), then just return the largest one.
       (about 10% of the time) - For differences between 0 and 99, return the largest value, but
       add in a pre-calculated difference value. - the value 99 was chosen arbitarily. - maximum
       (a-b) i've seen is 572 */
    FLOAT fdiff;
    int idiff;
    fdiff = (10.0 * (a - b));

    if (fdiff > 990.0) {
        return a;
    }
    if (fdiff < -990.0) {
        return (b);
    }

    idiff = (int) fdiff;
    if (idiff >= 0) {
        return (a + mem->dbtable[idiff]);
    }

    return (b + mem->dbtable[-idiff]);
}

/****************************************************************
*		Window the samples then, 
*		 Fast Fourier transform of the input samples.
* 
*	(  call the FHT-based fft() in fft.c )
*	 
*
****************************************************************/
static void psycho_1_hann_fft_pickmax(FLOAT sample[FFT_SIZE], mask power[HAN_SIZE],
                                      FLOAT spike[SBLIMIT], FLOAT energy[FFT_SIZE])
{
    FLOAT x_real[FFT_SIZE];
    register int i, j;
    register FLOAT sqrt_8_over_3;
    static int init = 0;
    static FLOAT window[FFT_SIZE];
    FLOAT sum;

    if (!init) {
        /* calculate window function for the Fourier transform */
        /* These values need only be initiliased once, regardless of the caller */
        sqrt_8_over_3 = pow(8.0 / 3.0, 0.5);
        for (i = 0; i < FFT_SIZE; i++) {
            /* Hann window formula */
            window[i] = sqrt_8_over_3 * 0.5 * (1 - cos(2.0 * PI * i / (FFT_SIZE))) / FFT_SIZE;
        }
        init = 1;
    }
    for (i = 0; i < FFT_SIZE; i++)
        x_real[i] = (FLOAT) (sample[i] * window[i]);

    psycho_1_fft(x_real, energy, FFT_SIZE);

    for (i = 0; i < HAN_SIZE; i++) {    /* calculate power density spectrum */
        if (energy[i] < 1E-20)
            power[i].x = -200.0 + POWERNORM;
        else
            power[i].x = 10 * log10(energy[i]) + POWERNORM;
        power[i].next = STOP;
        power[i].type = FALSE;
    }

    /* Calculate the sum of spectral component in each subband from bound 4-16 */

#define CF 1073741824           /* pow(10, 0.1*POWERNORM) */
#define DBM	 1E-20              /* pow(10.0, 0.1*DBMIN */
    for (i = 0; i < HAN_SIZE; spike[i >> 4] = 10.0 * log10(sum), i += 16) {
        for (j = 0, sum = DBM; j < 16; j++)
            sum += CF * energy[i + j];
    }
}

/****************************************************************
*
*		 This function labels the tonal component in the power
* spectrum.
*
****************************************************************/

static void psycho_1_tonal_label(psycho_1_mem * mem, int *tone)
/* this function extracts (tonal)  sinusoidals from the spectrum  */
{
    int i, j, last = LAST, first, run, last_but_one = LAST; /* dpwe */
    FLOAT max;
    mask *power = mem->power;

    *tone = LAST;
    for (i = 2; i < HAN_SIZE - 12; i++) {
        if (power[i].x > power[i - 1].x && power[i].x >= power[i + 1].x) {
            power[i].type = TONE;
            power[i].next = LAST;
            if (last != LAST)
                power[last].next = i;
            else
                first = *tone = i;
            last = i;
        }
    }
    last = LAST;
    first = *tone;
    *tone = LAST;
    while ((first != LAST) && (first != STOP)) {    /* the conditions for the tonal */
        if (first < 3 || first > 500)
            run = 0;            /* otherwise k+/-j will be out of bounds */
        else if (first < 63)
            run = 2;            /* components in layer II, which */
        else if (first < 127)
            run = 3;            /* are the boundaries for calc.  */
        else if (first < 255)
            run = 6;            /* the tonal components */
        else
            run = 12;
        max = power[first].x - 7;   /* after calculation of tonal */
        for (j = 2; j <= run; j++)  /* components, set to local max */
            if (max < power[first - j].x || max < power[first + j].x) {
                power[first].type = FALSE;
                break;
            }
        if (power[first].type == TONE) {    /* extract tonal components */
            int help = first;
            if (*tone == LAST)
                *tone = first;
            while ((power[help].next != LAST) && (power[help].next - first) <= run)
                help = power[help].next;
            help = power[help].next;
            power[first].next = help;
            if ((first - last) <= run) {
                if (last_but_one != LAST)
                    power[last_but_one].next = first;
            }
            if (first > 1 && first < 500) { /* calculate the sum of the */
                FLOAT tmp;      /* powers of the components */
                tmp = add_db(mem, power[first - 1].x, power[first + 1].x);
                power[first].x = add_db(mem, power[first].x, tmp);
            }
            for (j = 1; j <= run; j++) {
                power[first - j].x = power[first + j].x = DBMIN;
                power[first - j].next = power[first + j].next = STOP;
                power[first - j].type = power[first + j].type = FALSE;
            }
            last_but_one = last;
            last = first;
            first = power[first].next;
        } else {
            int ll;
            if (last == LAST);  /* *tone = power[first].next; dpwe */
            else
                power[last].next = power[first].next;
            ll = first;
            first = power[first].next;
            power[ll].next = STOP;
        }
    }
}

/****************************************************************
*
*		 This function groups all the remaining non-tonal
* spectral lines into critical band where they are replaced by
* one single line.
*
****************************************************************/

static void psycho_1_noise_label(psycho_1_mem * mem, int *noise, FLOAT energy[FFT_SIZE])
{
    int i, j, centre, last = LAST;
    FLOAT index, weight, sum;
    int crit_band = mem->crit_band;
    int *cbound = mem->cbound;
    mask *power = mem->power;
    /* calculate the remaining spectral */
    for (i = 0; i < crit_band - 1; i++) {   /* lines for non-tonal components */
        for (j = cbound[i], weight = 0.0, sum = DBMIN; j < cbound[i + 1]; j++) {
            if (power[j].type != TONE) {
                if (power[j].x != DBMIN) {
                    sum = add_db(mem, power[j].x, sum);
                    /* Weight is used in finding the geometric mean of the noise energy within a
                       subband */
                    weight += CF * energy[j] * (FLOAT) (j - cbound[i]) / (FLOAT) (cbound[i + 1] - cbound[i]);   /* correction 
                                                                                                                 */
                    power[j].x = DBMIN;
                }
            }                   /* check to see if the spectral line is low dB, and if */
        }                       /* so replace the center of the critical band, which is */
        /* the center freq. of the noise component */

        if (sum <= DBMIN)
            centre = (cbound[i + 1] + cbound[i]) / 2;
        else {
            /* fprintf(stderr, "%i [%f %f] -", count++,weight/pow(10.0,0.1*sum),
               weight*pow(10.0,-0.1*sum)); */
            index = weight * pow(10.0, -0.1 * sum);
            centre = cbound[i] + (int) (index * (FLOAT) (cbound[i + 1] - cbound[i]));
        }


        /* locate next non-tonal component until finished; */
        /* add to list of non-tonal components */

        /* Masahiro Iwadare's fix for infinite looping problem? */
        if (power[centre].type == TONE) {
            if (power[centre + 1].type == TONE) {
                centre++;
            } else
                centre--;
        }

        if (last == LAST)
            *noise = centre;
        else {
            power[centre].next = LAST;
            power[last].next = centre;
        }
        power[centre].x = sum;
        power[centre].type = NOISE;
        last = centre;
    }
}

/****************************************************************
*
*		 This function reduces the number of noise and tonal
* component for further threshold analysis.
*
****************************************************************/

static void psycho_1_subsampling(mask power[HAN_SIZE], g_thres * ltg, int *tone, int *noise)
{
    int i, old;

    i = *tone;
    old = STOP;                 /* calculate tonal components for */

    while ((i != LAST) && (i != STOP)) {    /* reduction of spectral lines */
        if (power[i].x < ltg[power[i].map].hear) {
            power[i].type = FALSE;
            power[i].x = DBMIN;
            if (old == STOP)
                *tone = power[i].next;
            else
                power[old].next = power[i].next;
        } else
            old = i;
        i = power[i].next;
    }
    i = *noise;
    old = STOP;                 /* calculate non-tonal components for */
    while ((i != LAST) && (i != STOP)) {    /* reduction of spectral lines */
        if (power[i].x < ltg[power[i].map].hear) {
            power[i].type = FALSE;
            power[i].x = DBMIN;
            if (old == STOP)
                *noise = power[i].next;
            else
                power[old].next = power[i].next;
        } else
            old = i;
        i = power[i].next;
    }
    i = *tone;
    old = STOP;
    while ((i != LAST) && (i != STOP)) {    /* if more than one */
        if (power[i].next == LAST)
            break;              /* tonal component */
        if (ltg[power[power[i].next].map].bark -    /* is less than .5 */
            ltg[power[i].map].bark < 0.5) { /* bark, take the */
            if (power[power[i].next].x > power[i].x) {  /* maximum */
                if (old == STOP)
                    *tone = power[i].next;
                else
                    power[old].next = power[i].next;
                power[i].type = FALSE;
                power[i].x = DBMIN;
                i = power[i].next;
            } else {
                power[power[i].next].type = FALSE;
                power[power[i].next].x = DBMIN;
                power[i].next = power[power[i].next].next;
                old = i;
            }
        } else {
            old = i;
            i = power[i].next;
        }
    }
}

/****************************************************************
*
*		 This function calculates the individual threshold and
* sum with the quiet threshold to find the global threshold.
*
****************************************************************/

/* mainly just changed the way range checking was done MFC Nov 1999 */
static void psycho_1_threshold(psycho_1_mem * mem, int *tone, int *noise, int bit_rate)
{
    int sub_size = mem->sub_size;
    mask *power = mem->power;
    g_thres *ltg = mem->ltg;
    int k, t;
    FLOAT dz, tmps, vf;

    for (k = 1; k < sub_size; k++) {
        ltg[k].x = DBMIN;
        t = *tone;              /* calculate individual masking threshold for */
        while ((t != LAST) && (t != STOP)) {    /* components in order to find the global */
            dz = ltg[k].bark - ltg[power[t].map].bark;  /* distance of bark value */
            if (dz >= -3.0 && dz < 8.0) {
                tmps = -1.525 - 0.275 * ltg[power[t].map].bark - 4.5 + power[t].x;
                /* masking function for lower & upper slopes */
                if (dz < -1)
                    vf = 17 * (dz + 1) - (0.4 * power[t].x + 6);
                else if (dz < 0)
                    vf = (0.4 * power[t].x + 6) * dz;
                else if (dz < 1)
                    vf = (-17 * dz);
                else
                    vf = -(dz - 1) * (17 - 0.15 * power[t].x) - 17;
                ltg[k].x = add_db(mem, ltg[k].x, tmps + vf);
            }
            t = power[t].next;
        }

        t = *noise;             /* calculate individual masking threshold */
        while ((t != LAST) && (t != STOP)) {    /* for non-tonal components to find LTG */
            dz = ltg[k].bark - ltg[power[t].map].bark;  /* distance of bark value */
            if (dz >= -3.0 && dz < 8.0) {
                tmps = -1.525 - 0.175 * ltg[power[t].map].bark - 0.5 + power[t].x;
                /* masking function for lower & upper slopes */
                if (dz < -1)
                    vf = 17 * (dz + 1) - (0.4 * power[t].x + 6);
                else if (dz < 0)
                    vf = (0.4 * power[t].x + 6) * dz;
                else if (dz < 1)
                    vf = (-17 * dz);
                else
                    vf = -(dz - 1) * (17 - 0.15 * power[t].x) - 17;
                ltg[k].x = add_db(mem, ltg[k].x, tmps + vf);
            }
            t = power[t].next;
        }
        if (bit_rate < 96)
            ltg[k].x = add_db(mem, ltg[k].hear, ltg[k].x);
        else
            ltg[k].x = add_db(mem, ltg[k].hear - 12.0, ltg[k].x);
    }

}

/****************************************************************
*
*		 This function finds the minimum masking threshold and
* return the value to the encoder.
*
****************************************************************/

static void psycho_1_minimum_mask(int sub_size, g_thres * ltg, FLOAT ltmin[SBLIMIT], int sblimit)
{
    FLOAT min;
    int i, j;

    j = 1;
    for (i = 0; i < sblimit; i++)
        if (j >= sub_size - 1)  /* check subband limit, and */
            ltmin[i] = ltg[sub_size - 1].hear;  /* calculate the minimum masking */
        else {                  /* level of LTMIN for each subband */
            min = ltg[j].x;
            while (ltg[j].line >> 4 == i && j < sub_size) {
                if (min > ltg[j].x)
                    min = ltg[j].x;
                j++;
            }
            ltmin[i] = min;
        }
}

/*****************************************************************
*
*		 This procedure is called in musicin to pick out the
* smaller of the scalefactor or threshold.
*
*****************************************************************/

static void psycho_1_smr(FLOAT ltmin[SBLIMIT], FLOAT spike[SBLIMIT], FLOAT scale[SBLIMIT],
                         int sblimit)
{
    int i;
    FLOAT max;

    for (i = 0; i < sblimit; i++) { /* determine the signal */
        max = 20 * log10(scale[i] * 32768) - 10;    /* level for each subband */
        if (spike[i] > max)
            max = spike[i];     /* for the maximum scale */
        max -= ltmin[i];        /* factors */
        ltmin[i] = max;
    }
}


/*
static void psycho_1_dump(mask power[HAN_SIZE], int *tone, int *noise) {
  int t;

  fprintf(stderr,"1 Ton: ");
  t=*tone;
  while (t!=LAST && t!=STOP) {
	fprintf(stderr,"[%i] %3.0f ",t, power[t].x);
	t = power[t].next;
  }
  fprintf(stderr,"\n");
  
  fprintf(stderr,"1 Nos: ");
  t=*noise;
  while (t!=LAST && t!=STOP) {
	fprintf(stderr,"[%i] %3.0f ",t, power[t].x);
	t = power[t].next;
  }
  fprintf(stderr,"\n");
}
*/


void psycho_1(twolame_options * glopts, short buffer[2][1152], FLOAT scale[2][SBLIMIT],
              FLOAT ltmin[2][SBLIMIT])
{
    psycho_1_mem *mem;
    frame_header *header = &glopts->header;
    int nch = glopts->num_channels_out;
    int sblimit = glopts->sblimit;
    int k, i, tone = 0, noise = 0;
    FLOAT sample[FFT_SIZE];
    FLOAT spike[2][SBLIMIT];
    FLOAT *fft_buf[2];
    FLOAT energy[FFT_SIZE];

    /* call functions for critical boundaries, freq. */
    if (!glopts->p1mem) {       /* bands, bark values, and mapping */
        mem = (psycho_1_mem *) TWOLAME_MALLOC(sizeof(psycho_1_mem));

        mem->power = (mask_ptr) TWOLAME_MALLOC(sizeof(mask) * HAN_SIZE);
        if (header->version == TWOLAME_MPEG1) {
            mem->cbound =
                psycho_1_read_cbound(header->lay, header->samplerate_idx, &mem->crit_band);
            psycho_1_read_freq_band(&mem->ltg, header->lay, header->samplerate_idx, &mem->sub_size);
        } else {
            mem->cbound =
                psycho_1_read_cbound(header->lay, header->samplerate_idx + 4, &mem->crit_band);
            psycho_1_read_freq_band(&mem->ltg, header->lay, header->samplerate_idx + 4,
                                    &mem->sub_size);
        }
        psycho_1_make_map(mem->sub_size, mem->power, mem->ltg);
        for (i = 0; i < 1408; i++)
            mem->fft_buf[0][i] = mem->fft_buf[1][i] = 0;

        psycho_1_init_add_db(mem);  /* create the add_db table */

        mem->off[0] = 256;
        mem->off[1] = 256;

        glopts->p1mem = mem;
    }
    {
        mem = glopts->p1mem;

        fft_buf[0] = mem->fft_buf[0];
        fft_buf[1] = mem->fft_buf[1];
    }


    for (k = 0; k < nch; k++) {
        /* check pcm input for 3 blocks of 384 samples */
        /* sami's speedup, added in 02j saves about 4% overall during an encode */
        int ok = mem->off[k] % 1408;
        for (i = 0; i < 1152; i++) {
            fft_buf[k][ok++] = (FLOAT) buffer[k][i] / SCALE;
            if (ok >= 1408)
                ok = 0;
        }
        ok = (mem->off[k] + 1216) % 1408;
        for (i = 0; i < FFT_SIZE; i++) {
            sample[i] = fft_buf[k][ok++];
            if (ok >= 1408)
                ok = 0;
        }
        mem->off[k] += 1152;
        mem->off[k] %= 1408;

        psycho_1_hann_fft_pickmax(sample, mem->power, &spike[k][0], energy);
        psycho_1_tonal_label(mem, &tone);
        psycho_1_noise_label(mem, &noise, energy);
        // psycho_1_dump(power, &tone, &noise) ;
        psycho_1_subsampling(mem->power, mem->ltg, &tone, &noise);
        psycho_1_threshold(mem, &tone, &noise, glopts->bitrate / nch);
        psycho_1_minimum_mask(mem->sub_size, mem->ltg, &ltmin[k][0], sblimit);
        psycho_1_smr(&ltmin[k][0], &spike[k][0], &scale[k][0], sblimit);
    }

}

void psycho_1_deinit(psycho_1_mem ** mem)
{

    if (mem == NULL || *mem == NULL)
        return;

    TWOLAME_FREE((*mem)->cbound);
    TWOLAME_FREE((*mem)->ltg);
    TWOLAME_FREE((*mem)->power);
    TWOLAME_FREE((*mem));
}


// vim:ts=4:sw=4:nowrap: 
