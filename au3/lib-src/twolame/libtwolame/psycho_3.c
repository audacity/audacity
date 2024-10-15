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
 *	but WITHOUT ANY WARRANTY; without even the impelied warranty of
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
#include "psycho_3.h"

/* This is a reimplementation of psy model 1 using the ISO11172 standard.
   I found the original dist10 code (which is full of pointers) to be 
   a horrible thing to try and understand and debug.
   This implementation is not built for speed, but is rather meant to 
   clearly outline the steps specified by the standard (still, it's only
   a tiny fraction slower than the dist10 code, and nothing has been optimized)
   MFC Feb 2003 */

static inline FLOAT psycho_3_add_db(psycho_3_mem * mem, FLOAT a, FLOAT b)
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



/* ISO11172 Sec D.1 Step 1 - Window with HANN and then perform the FFT */
static void psycho_3_fft(FLOAT sample[BLKSIZE], FLOAT energy[BLKSIZE])
{
    FLOAT x_real[BLKSIZE];
    int i;
    static int init = 0;
    static FLOAT window[FFT_SIZE];

    if (!init) {                /* calculate window function for the Fourier transform */
        FLOAT sqrt_8_over_3 = pow(8.0 / 3.0, 0.5);
        for (i = 0; i < BLKSIZE; i++) {
            window[i] = sqrt_8_over_3 * 0.5 * (1 - cos(2.0 * PI * i / (BLKSIZE))) / BLKSIZE;
        }
        init++;
    }

    /* convolve the samples with the hann window */
    for (i = 0; i < BLKSIZE; i++)
        x_real[i] = (FLOAT) (sample[i] * window[i]);
    /* do the FFT */
    psycho_1_fft(x_real, energy, BLKSIZE);
}


/* Sect D.1 Step 1 - convert the energies into dB */
static void psycho_3_powerdensityspectrum(FLOAT energy[BLKSIZE], FLOAT power[HBLKSIZE])
{
    int i;
    for (i = 1; i < HBLKSIZE; i++) {
        if (energy[i] < 1E-20)
            power[i] = -200.0 + POWERNORM;
        else
            power[i] = 10 * log10(energy[i]) + POWERNORM;
    }
}


/* Sect D.1 Step 2 - Determine the sound pressure level in each subband */
static void psycho_3_spl(FLOAT * Lsb, FLOAT * power, FLOAT * scale)
{
    int i;
    FLOAT Xmax[SBLIMIT];

    for (i = 0; i < SBLIMIT; i++) {
        Xmax[i] = DBMIN;
    }
    /* Find the maximum SPL in the power spectrum */
    for (i = 1; i < HBLKSIZE; i++) {
        int index = (i - 1) >> 4;
        if (Xmax[index] < power[i])
            Xmax[index] = power[i];
    }

    /* Compare it to the sound pressure based upon the scale for this subband and pick the maximum
       one */
    for (i = 0; i < SBLIMIT; i++) {
        FLOAT val = 20 * log10(scale[i] * 32768) - 10;
        Lsb[i] = MAX(Xmax[i], val);
    }
}


/* Sect D.1 Step4b 
   A tone within the range (start -> end), must be 7.0 dB greater than
   all it's neighbours within +/- srange. Don't count its immediate neighbours. */
static void psycho_3_tonal_label_range(psycho_3_mem * mem, FLOAT * power, int *tonelabel,
                                       int *maxima, FLOAT * Xtm, int start, int end, int srange)
{
    int j, k;

    for (k = start; k < end; k++)   /* Search for all the maxima in this range */
        if (maxima[k] == 1) {
            tonelabel[k] = TONE;    /* assume it's a TONE and then prove otherwise */
            for (j = -srange; j <= +srange; j++)    /* Check the neighbours within +/- srange */
                if (abs(j) > 1) /* Don't count the immediate neighbours, or itself */
                    if ((power[k] - power[k + j]) < 7.0)
                        tonelabel[k] = 0;   /* Not greater by 7dB, therefore not a tone */
            if (tonelabel[k] == TONE) {
                /* Calculate the sound pressure level for this tone by summing the adjacent
                   spectral lines Xtm[k] = 10 * log10( pow(10.0, 0.1*power[k-1]) + pow(10.0,
                   0.1*power[k]) + pow(10.0, 0.1*power[k+1]) ); */
                FLOAT temp = psycho_3_add_db(mem, power[k - 1], power[k]);
                Xtm[k] = psycho_3_add_db(mem, temp, power[k + 1]);

                /* *ALL* spectral lines within +/- srange are set to -inf dB So that when we do the 
                   noise calculate, they are not counted */
                for (j = -srange; j <= +srange; j++)
                    power[k + j] = DBMIN;
            }
        }
}


/* Sect D.1 Step 4 Label the Tonal Components */
static void psycho_3_tonal_label(psycho_3_mem * mem, FLOAT power[HBLKSIZE], int *tonelabel,
                                 FLOAT Xtm[HBLKSIZE])
{
    int i;
    int maxima[HBLKSIZE];

    /* Find the maxima as per ISO11172 D.1.4.a */
    maxima[0] = maxima[HBLKSIZE - 1] = 0;
    tonelabel[0] = tonelabel[HBLKSIZE - 1] = 0;
    Xtm[0] = Xtm[HBLKSIZE - 1] = DBMIN;
    for (i = 1; i < HBLKSIZE - 1; i++) {
        tonelabel[i] = 0;
        Xtm[i] = DBMIN;
        if (power[i] > power[i - 1] && power[i] > power[i + 1]) /* The first criteria for a maximum 
                                                                 */
            maxima[i] = 1;
        else
            maxima[i] = 0;
    }

    {
        /* Now find the tones as per ISO11172 D.1 Step4.b */
        /* The standard is a bit vague (surprise surprise). So I'm going to assume that - a tone
           must be 7dB greater than *all* the relevant neighbours - once a tone is found, the
           neighbours are immediately set to -inf dB */

        psycho_3_tonal_label_range(mem, power, tonelabel, maxima, Xtm, 2, 63, 2);
        psycho_3_tonal_label_range(mem, power, tonelabel, maxima, Xtm, 63, 127, 3);
        psycho_3_tonal_label_range(mem, power, tonelabel, maxima, Xtm, 127, 255, 6);
        psycho_3_tonal_label_range(mem, power, tonelabel, maxima, Xtm, 255, 500, 12);

    }
}



static void psycho_3_init_add_db(psycho_3_mem * mem)
{
    int i;
    FLOAT x;
    for (i = 0; i < DBTAB; i++) {
        x = (FLOAT) i / 10.0;
        mem->dbtable[i] = 10 * log10(1 + pow(10.0, x / 10.0)) - x;
    }
}


/* D.1 Step 4.c Labelling non-tonal (noise) components 
   Sum the energies in each critical band (the tone energies have been removed 
   during the tone labelling).
   Find the "geometric mean" of these energies - i.e. find the best spot to put the
   sum of energies within this critical band. */
static void psycho_3_noise_label(psycho_3_mem * mem, FLOAT power[HBLKSIZE], FLOAT energy[BLKSIZE],
                                 int *tonelabel, int *noiselabel, FLOAT Xnm[HBLKSIZE])
{
    int i, j;
    int cbands = mem->cbands;
    int *cbandindex = mem->cbandindex;

    Xnm[0] = DBMIN;
    for (i = 0; i < cbands; i++) {
        /* for each critical band */
        FLOAT sum = DBMIN;
        FLOAT esum = 0;
        FLOAT centreweight = 0;
        int centre;
        for (j = cbandindex[i]; j < cbandindex[i + 1]; j++) {
            Xnm[j] = DBMIN;
            /* go through all the spectral lines within the critical band, adding the energies. The 
               tone energies have already been removed */
            if (power[j] != DBMIN) {
                /* Found a noise energy, add it to the sum */
                sum = psycho_3_add_db(mem, power[j], sum);

                /* calculations for the geometric mean FIXME MFC Feb 2003: Would it just be easier
                   to do the *whole* of psycho_1 in the energy domain rather than in the dB domain?
                   FIXME: This is just a lazy arsed arithmetic mean. Don't know if it's really going 
                   to make that much difference */
                esum += energy[j];  /* Calculate the sum of energies */
                centreweight += (j - cbandindex[i]) * energy[j];    /* And the energy moment */
            }
        }

        /* MEANX, crash on AMD64 without this hack. See
           https://sourceforge.net/tracker/?func=detail&atid=735435&aid=1453400&group_id=136040
           Probably a better way to do this */
        if (sum <= DBMIN || esum < 0.00001)
            /* If the energy sum is really small, just pretend the noise occurs in the centre
               frequency line */
            centre = (cbandindex[i] + cbandindex[i + 1]) / 2;
        else {
            /* Otherwise, work out the mean position of the noise, and put it there. */
            centre = cbandindex[i] + (int) (centreweight / esum);
        }
        // /MEANX
        Xnm[centre] = sum;
        noiselabel[centre] = NOISE;
    }
}


/* ISO11172 D.1 Step 5
   Get rid of noise/tones that aren't greater than the ATH
   If two tones are within 0.5bark, then delete the tone with the lower energy */
static void psycho_3_decimation(FLOAT * ath, int *tonelabel, FLOAT * Xtm, int *noiselabel,
                                FLOAT * Xnm, FLOAT * bark)
{
    int i;

    /* Delete components which aren't above the ATH */
    for (i = 1; i < HBLKSIZE; i++) {
        if (noiselabel[i] == NOISE) {
            if (Xnm[i] < ath[i]) {
                /* this masker isn't above the ATH : delete it */
                Xnm[i] = DBMIN;
                noiselabel[i] = 0;
            }
        }
        if (tonelabel[i] == TONE) {
            if (Xtm[i] < ath[i]) {
                Xtm[i] = DBMIN;
                tonelabel[i] = 0;
            }
        }
    }
    /* Search for tones that are within 0.5 bark */
    /* MFC FIXME Feb 2003: haven't done this yet */

}


/* ISO11172 Sect D.1 Step 6
   Calculation of individual masking thresholds
   Work out how each of the tones&noises maskes other frequencies 
   NOTE: Only a subset of other frequencies is checked. According to the 
   standard different subbands are subsampled to different amounts.
   See psycho_3_init and freq_subset */
static void psycho_3_threshold(psycho_3_mem * mem, FLOAT * LTg, int *tonelabel, FLOAT * Xtm,
                               int *noiselabel, FLOAT * Xnm, FLOAT * bark, FLOAT * ath,
                               int bit_rate, int *freq_subset)
{
    int i, j, k;
    FLOAT LTtm[SUBSIZE];
    FLOAT LTnm[SUBSIZE];

    for (i = 0; i < SUBSIZE; i++) {
        LTtm[i] = DBMIN;
        LTnm[i] = DBMIN;
    }
    /* Loop over the entire spectrum and find every noise and tone And then with each noise/tone
       work out how it masks the spectral lines around it */
    for (k = 1; k < HBLKSIZE; k++) {
        /* Find every tone */
        if (tonelabel[k] == TONE) {
            for (j = 0; j < SUBSIZE; j++) {
                /* figure out how it masks the levels around it */
                FLOAT dz = bark[freq_subset[j]] - bark[k];
                if (dz >= -3.0 && dz < 8.0) {
                    FLOAT vf;
                    FLOAT av = -1.525 - 0.275 * bark[k] - 4.5 + Xtm[k];
                    /* masking function for lower & upper slopes */
                    if (dz < -1)
                        vf = 17 * (dz + 1) - (0.4 * Xtm[k] + 6);
                    else if (dz < 0)
                        vf = (0.4 * Xtm[k] + 6) * dz;
                    else if (dz < 1)
                        vf = (-17 * dz);
                    else
                        vf = -(dz - 1) * (17 - 0.15 * Xtm[k]) - 17;
                    LTtm[j] = psycho_3_add_db(mem, LTtm[j], av + vf);
                }
            }
        }

        /* find every noise label */
        if (noiselabel[k] == NOISE) {
            for (j = 0; j < SUBSIZE; j++) {
                /* figure out how it masks the levels around it */
                FLOAT dz = bark[freq_subset[j]] - bark[k];
                if (dz >= -3.0 && dz < 8.0) {
                    FLOAT vf;
                    FLOAT av = -1.525 - 0.175 * bark[k] - 0.5 + Xnm[k];
                    /* masking function for lower & upper slopes */
                    if (dz < -1)
                        vf = 17 * (dz + 1) - (0.4 * Xnm[k] + 6);
                    else if (dz < 0)
                        vf = (0.4 * Xnm[k] + 6) * dz;
                    else if (dz < 1)
                        vf = (-17 * dz);
                    else
                        vf = -(dz - 1) * (17 - 0.15 * Xnm[k]) - 17;
                    LTnm[j] = psycho_3_add_db(mem, LTnm[j], av + vf);
                }
            }
        }
    }

    /* ISO11172 D.1 Step 7 Calculate the global masking threhold */
    for (i = 0; i < SUBSIZE; i++) {
        LTg[i] = psycho_3_add_db(mem, LTnm[i], LTtm[i]);
        if (bit_rate < 96)
            LTg[i] = psycho_3_add_db(mem, ath[freq_subset[i]], LTg[i]);
        else
            LTg[i] = psycho_3_add_db(mem, ath[freq_subset[i]] - 12.0, LTg[i]);
    }
}


/* Find the minimum LTg for each subband. ISO11172 Sec D.1 Step 8 */
static void psycho_3_minimummasking(FLOAT * LTg, FLOAT * LTmin, int *freq_subset)
{
    int i;

    for (i = 0; i < SBLIMIT; i++)
        LTmin[i] = 999999.9;

    for (i = 0; i < SUBSIZE; i++) {
        int index = freq_subset[i] >> 4;
        if (LTmin[index] > LTg[i]) {
            LTmin[index] = LTg[i];
        }
    }
}


/* ISO11172 Sect D.1 Step 9
   Calculate the signal-to-mask ratio 
   MFC FIXME Feb 2003 for better calling from
   twolame, add a "float SMR[]" array and return it */
static void psycho_3_smr(FLOAT * LTmin, FLOAT * Lsb)
{
    int i;
    for (i = 0; i < SBLIMIT; i++) {
        LTmin[i] = Lsb[i] - LTmin[i];
    }
}


static psycho_3_mem *psycho_3_init(twolame_options * glopts)
{
    int i;
    int cbase = 0;              /* current base index for the bark range calculation */
    FLOAT sfreq;
    psycho_3_mem *mem;
    int numlines[HBLKSIZE];
    FLOAT cbval[HBLKSIZE];
    int partition[HBLKSIZE];
    int *freq_subset;
    FLOAT *bark, *ath;
    int cbands = 0;
    int *cbandindex;

    mem = (psycho_3_mem *) TWOLAME_MALLOC(sizeof(psycho_3_mem));
    mem->off[0] = mem->off[1] = 256;
    freq_subset = mem->freq_subset;
    bark = mem->bark;
    ath = mem->ath;
    cbandindex = mem->cbandindex;

    /* Initialise the tables for the adding dB */
    psycho_3_init_add_db(mem);

    /* For each spectral line calculate the bark and the ATH (in dB) */
    sfreq = (FLOAT) glopts->samplerate_out;
    for (i = 1; i < HBLKSIZE; i++) {
        FLOAT freq = i * sfreq / BLKSIZE;
        bark[i] = ath_freq2bark(freq);
        ath[i] = ath_db(freq, glopts->athlevel);
    }

    {                           /* Work out the critical bands Starting from line 0, all lines
                                   within 1 bark of the starting bark are added to the same
                                   critical band. When a line is greater by 1.0 of a bark, start a
                                   new critical band.  */

        cbandindex[0] = 1;
        for (i = 1; i < HBLKSIZE; i++) {
            if ((bark[i] - bark[cbase]) > 1.0) {    /* 1 critical band? 1 bark? */
                /* this frequency line is too different from the starting line, (in terms of the
                   bark distance) so make this spectral line the first member of the next critical
                   band */
                cbase = i;      /* Start the new critical band from this frequency line */
                cbands++;
                cbandindex[cbands] = cbase;
            }
            /* partition[i] tells us which critical band the i'th frequency line is in */
            partition[i] = cbands;
            /* keep a count of how many frequency lines are in each partition */
            numlines[cbands]++;
        }

        cbands++;
        cbandindex[cbands] = 513;   /* Set the top of the last critical band */
        mem->cbands = cbands;   // make a not of the number of cbands

        /* For each crtical band calculate the average bark value cbval [central bark value] */
        for (i = 1; i < HBLKSIZE; i++)
            cbval[partition[i]] += bark[i]; /* sum up all the bark values */
        for (i = 1; i < CBANDS; i++) {
            if (numlines[i] != 0)
                cbval[i] /= numlines[i];    /* divide by the number of values */
            else {
                cbval[i] = 0;   /* this isn't a partition */
            }
        }
    }

    {
        /* For Step6 - For the calculation of individual masking thresholds the spectral lines are
           subsampled i.e. no need to work out the masking for every single spectral line.
           Depending upon which subband the calculation is for, you can skip a number of lines
           There are 16 lines per subband -> 32 * 16 = 512 Subband 0-2 : Every line (3 * 16 = 48
           lines) Subband 3-5 : Every Second line (3 * 16/2 = 24 lines) Subband 6-11 : Every 4th
           line (6 * 16/4 = 24 lines) Subband 12-31 : Every 12th line (20 * 16/8 = 40 lines)

           create this subset of frequencies (freq_subset) */
        int freq_index = 0;
        for (i = 1; i < (3 * 16) + 1; i++)
            freq_subset[freq_index++] = i;
        for (; i < (6 * 16) + 1; i += 2)
            freq_subset[freq_index++] = i;
        for (; i < (12 * 16) + 1; i += 4)
            freq_subset[freq_index++] = i;
        for (; i < (32 * 16) + 1; i += 8)
            freq_subset[freq_index++] = i;
    }

    if (glopts->verbosity > 4) {
        fprintf(stderr, "%i critical bands\n", cbands);
        for (i = 0; i < cbands; i++)
            fprintf(stderr, "cband %i spectral line index %i\n", i, cbandindex[i]);
        fprintf(stderr, "%i Subsampled spectral lines\n", SUBSIZE);
        for (i = 0; i < SUBSIZE; i++)
            fprintf(stderr, "%i Spectral line %i Bark %.2f\n", i, freq_subset[i],
                    bark[freq_subset[i]]);
    }

    return (mem);
}


static void psycho_3_dump(int *tonelabel, FLOAT * Xtm, int *noiselabel, FLOAT * Xnm)
{
    int i;
    fprintf(stderr, "3 Ton:");
    for (i = 1; i < HAN_SIZE; i++) {
        if (tonelabel[i] == TONE)
            fprintf(stderr, "[%i] %3.0f ", i, Xtm[i]);
    }
    fprintf(stderr, "\n");

    fprintf(stderr, "3 Nos:");
    for (i = 1; i < HAN_SIZE; i++) {
        if (noiselabel[i] == NOISE)
            fprintf(stderr, "[%i] %3.0f ", i, Xnm[i]);
    }
    fprintf(stderr, "\n");
}


void psycho_3(twolame_options * glopts, short int buffer[2][1152], FLOAT scale[2][32],
              FLOAT ltmin[2][32])
{
    psycho_3_mem *mem;
    int nch = glopts->num_channels_out;
    int k, i;
    FLOAT sample[BLKSIZE];

    FLOAT energy[BLKSIZE];
    FLOAT power[HBLKSIZE];
    FLOAT Xtm[HBLKSIZE], Xnm[HBLKSIZE];
    int tonelabel[HBLKSIZE], noiselabel[HBLKSIZE];
    FLOAT LTg[HBLKSIZE];
    FLOAT Lsb[SBLIMIT];

    if (!glopts->p3mem) {
        glopts->p3mem = psycho_3_init(glopts);
    }
    mem = glopts->p3mem;

    for (k = 0; k < nch; k++) {
        int ok = mem->off[k] % 1408;
        for (i = 0; i < 1152; i++) {
            mem->fft_buf[k][ok++] = (FLOAT) buffer[k][i] / SCALE;
            if (ok >= 1408)
                ok = 0;
        }
        ok = (mem->off[k] + 1216) % 1408;
        for (i = 0; i < BLKSIZE; i++) {
            sample[i] = mem->fft_buf[k][ok++];
            if (ok >= 1408)
                ok = 0;
        }

        mem->off[k] += 1152;
        mem->off[k] %= 1408;

        psycho_3_fft(sample, energy);
        psycho_3_powerdensityspectrum(energy, power);
        psycho_3_spl(Lsb, power, &scale[k][0]);
        psycho_3_tonal_label(mem, power, tonelabel, Xtm);
        psycho_3_noise_label(mem, power, energy, tonelabel, noiselabel, Xnm);
        if (glopts->verbosity > 8)
            psycho_3_dump(tonelabel, Xtm, noiselabel, Xnm);
        psycho_3_decimation(mem->ath, tonelabel, Xtm, noiselabel, Xnm, mem->bark);
        psycho_3_threshold(mem, LTg, tonelabel, Xtm, noiselabel, Xnm, mem->bark, mem->ath,
                           glopts->bitrate / nch, mem->freq_subset);
        psycho_3_minimummasking(LTg, &ltmin[k][0], mem->freq_subset);
        psycho_3_smr(&ltmin[k][0], Lsb);
    }
}


void psycho_3_deinit(psycho_3_mem ** mem)
{

    if (mem == NULL || *mem == NULL)
        return;

    TWOLAME_FREE(*mem);
}


// vim:ts=4:sw=4:nowrap: 
