/* 
 * layer1.c: Mpeg Layer-1 audio decoder 
 *
 * Copyright (C) 1999-2010 The L.A.M.E. project
 *
 * Initially written by Michael Hipp, see also AUTHORS and README.
 *  
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/* $Id: layer1.c,v 1.31 2017/08/23 13:22:23 robert Exp $ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include <assert.h>
#include "common.h"
#include "decode_i386.h"

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif

#include "layer1.h"

static int gd_are_hip_tables_layer1_initialized = 0;

void
hip_init_tables_layer1(void)
{
    if (gd_are_hip_tables_layer1_initialized) {
        return;
    }
    gd_are_hip_tables_layer1_initialized = 1;
}

typedef struct sideinfo_layer_I_struct
{
    unsigned char allocation[SBLIMIT][2]; 
    unsigned char scalefactor[SBLIMIT][2];
} sideinfo_layer_I;

static int
I_step_one(PMPSTR mp, sideinfo_layer_I* si)
{
    struct frame *fr = &(mp->fr);
    int     jsbound = (fr->mode == MPG_MD_JOINT_STEREO) ? (fr->mode_ext << 2) + 4 : 32;
    int     i;
    int     illegal_value_detected = 0;
    unsigned char const ba15 = 15; /* bit pattern not allowed, looks like sync(?) */
    memset(si, 0, sizeof(*si));
    assert(fr->stereo == 1 || fr->stereo == 2);

    if (fr->stereo == 2) {
        for (i = 0; i < jsbound; i++) {
            unsigned char b0 = get_leq_8_bits(mp, 4);       /* values 0-15 */
            unsigned char b1 = get_leq_8_bits(mp, 4);       /* values 0-15 */
            si->allocation[i][0] = b0;
            si->allocation[i][1] = b1;
            if (b0 == ba15 || b1 == ba15)
                illegal_value_detected = 1;
        }
        for (i = jsbound; i < SBLIMIT; i++) {
            unsigned char b = get_leq_8_bits(mp, 4);        /* values 0-15 */
            si->allocation[i][0] = b;
            si->allocation[i][1] = b;
            if (b == ba15)
                illegal_value_detected =  1;
        }
        for (i = 0; i < SBLIMIT; i++) {
            unsigned char n0 = si->allocation[i][0];
            unsigned char n1 = si->allocation[i][1];
            unsigned char b0 = n0 ? get_leq_8_bits(mp, 6) : 0;  /* values 0-63 */
            unsigned char b1 = n1 ? get_leq_8_bits(mp, 6) : 0;  /* values 0-63 */
            si->scalefactor[i][0] = b0;
            si->scalefactor[i][1] = b1;
        }
    }
    else {
        for (i = 0; i < SBLIMIT; i++) {
            unsigned char b0 =  get_leq_8_bits(mp, 4);          /* values 0-15 */
            si->allocation[i][0] = b0;
            if (b0 == ba15)
                illegal_value_detected = 1;
        }
        for (i = 0; i < SBLIMIT; i++) {
            unsigned char n0 = si->allocation[i][0];
            unsigned char b0 = n0 ? get_leq_8_bits(mp, 6) : 0;  /* values 0-63 */
            si->scalefactor[i][0] = b0;
        }
    }
    return illegal_value_detected;
}

static void
I_step_two(PMPSTR mp, sideinfo_layer_I *si, real fraction[2][SBLIMIT])
{
    double  r0, r1;
    struct frame *fr = &(mp->fr);
    int     ds_limit = fr->down_sample_sblimit;
    int     i;

    assert(fr->stereo == 1 || fr->stereo == 2);
    if (fr->stereo == 2) {
        int     jsbound = (fr->mode == MPG_MD_JOINT_STEREO) ? (fr->mode_ext << 2) + 4 : 32;
        for (i = 0; i < jsbound; i++) {
            unsigned char i0 = si->scalefactor[i][0];
            unsigned char i1 = si->scalefactor[i][1];
            unsigned char n0 = si->allocation[i][0];
            unsigned char n1 = si->allocation[i][1];
            assert( i0 < 64 );
            assert( i1 < 64 );
            assert( n0 < 16 );
            assert( n1 < 16 );
            if (n0 > 0) {
                unsigned short v = get_leq_16_bits(mp, n0 + 1); /* 0-65535 */
                r0 = (((-1) << n0) + v + 1) * muls[n0 + 1][i0];
            }
            else {
                r0 = 0;
            }
            if (n1 > 0) {
                unsigned short v = get_leq_16_bits(mp, n1 + 1); /* 0-65535 */
                r1 = (((-1) << n1) + v + 1) * muls[n1 + 1][i1];
            }
            else {
                r1 = 0;
            }
            fraction[0][i] = (real)r0;
            fraction[1][i] = (real)r1;
        }
        for (i = jsbound; i < SBLIMIT; i++) {
            unsigned char i0 = si->scalefactor[i][0];
            unsigned char i1 = si->scalefactor[i][1];
            unsigned char n = si->allocation[i][0];
            assert( i0 < 64 );
            assert( i1 < 64 );
            assert( n < 16 );
            if (n > 0) {
                unsigned short v = get_leq_16_bits(mp, n + 1); /* 0-65535 */
                unsigned int w = (((-1) << n) + v + 1);
                r0 = w * muls[n + 1][i0];
                r1 = w * muls[n + 1][i1];
            }
            else {
                r0 = r1 = 0;
            }
            fraction[0][i] = (real)r0;
            fraction[1][i] = (real)r1;
        }
        for (i = ds_limit; i < SBLIMIT; i++) {
            fraction[0][i] = 0.0;
            fraction[1][i] = 0.0;
        }
    }
    else {
        for (i = 0; i < SBLIMIT; i++) {
            unsigned char n = si->allocation[i][0];
            unsigned char j = si->scalefactor[i][0];
            assert( j < 64 );
            assert( n < 16 );
            if (n > 0) {
                unsigned short v = get_leq_16_bits(mp, n + 1);
                r0 = (((-1) << n) + v + 1) * muls[n + 1][j];
            }
            else {
                r0 = 0;
            }
            fraction[0][i] = (real)r0;
        }
        for (i = ds_limit; i < SBLIMIT; i++) {
            fraction[0][i] = 0.0;
        }
    }
}

int
decode_layer1_sideinfo(PMPSTR mp)
{
    (void) mp;
    /* FIXME: extract side information and check values */
    return 0;
}

int
decode_layer1_frame(PMPSTR mp, unsigned char *pcm_sample, int *pcm_point)
{
    real    fraction[2][SBLIMIT]; /* FIXME: change real -> double ? */
    sideinfo_layer_I si;
    struct frame *fr = &(mp->fr);
    int     single = fr->single;
    int     i, clip = 0;

    if (I_step_one(mp, &si)) {
        lame_report_fnc(mp->report_err, "hip: Aborting layer 1 decode, illegal bit allocation value\n");
        return -1;
    }
    if (fr->stereo == 1 || single == 3)
        single = 0;

    if (single >= 0) {
        /* decoding one of possibly two channels */
        for (i = 0; i < SCALE_BLOCK; i++) {
            I_step_two(mp, &si, fraction);
            clip += synth_1to1_mono(mp, (real *) fraction[single], pcm_sample, pcm_point);
        }
    }
    else {
        for (i = 0; i < SCALE_BLOCK; i++) {
            int     p1 = *pcm_point;
            I_step_two(mp, &si, fraction);
            clip += synth_1to1(mp, (real *) fraction[0], 0, pcm_sample, &p1);
            clip += synth_1to1(mp, (real *) fraction[1], 1, pcm_sample, pcm_point);
        }
    }

    return clip;
}
