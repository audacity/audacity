/* 
 * layer2.c: Mpeg Layer-2 audio decoder 
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
/* $Id: layer2.c,v 1.34 2017/08/22 23:31:07 robert Exp $ */

#ifdef HAVE_CONFIG_H
# include <config.h>
#endif

#include "common.h"
#include "layer2.h"
#include "l2tables.h"
#include "decode_i386.h"

#ifdef WITH_DMALLOC
#include <dmalloc.h>
#endif
#include <assert.h>

static int gd_are_hip_tables_layer2_initialized = 0;

static unsigned char grp_3tab[32 * 3] = { 0, }; /* used: 27 */
static unsigned char grp_5tab[128 * 3] = { 0, }; /* used: 125 */
static unsigned char grp_9tab[1024 * 3] = { 0, }; /* used: 729 */


void
hip_init_tables_layer2(void)
{
    static const double mulmul[27] = {
        0.0, -2.0 / 3.0, 2.0 / 3.0,
        2.0 / 7.0, 2.0 / 15.0, 2.0 / 31.0, 2.0 / 63.0, 2.0 / 127.0, 2.0 / 255.0,
        2.0 / 511.0, 2.0 / 1023.0, 2.0 / 2047.0, 2.0 / 4095.0, 2.0 / 8191.0,
        2.0 / 16383.0, 2.0 / 32767.0, 2.0 / 65535.0,
        -4.0 / 5.0, -2.0 / 5.0, 2.0 / 5.0, 4.0 / 5.0,
        -8.0 / 9.0, -4.0 / 9.0, -2.0 / 9.0, 2.0 / 9.0, 4.0 / 9.0, 8.0 / 9.0
    };
    static const unsigned char base[3][9] = {
        {1, 0, 2,},
        {17, 18, 0, 19, 20,},
        {21, 1, 22, 23, 0, 24, 25, 2, 26}
    };
    int     i, j, k, l, len;
    real   *table;
    static const int tablen[3] = { 3, 5, 9 };
    static unsigned char *itable, *tables[3] = { grp_3tab, grp_5tab, grp_9tab };

    if (gd_are_hip_tables_layer2_initialized) {
        return;
    }
    gd_are_hip_tables_layer2_initialized = 1;

    for (i = 0; i < 3; i++) {
        itable = tables[i];
        len = tablen[i];
        for (j = 0; j < len; j++)
            for (k = 0; k < len; k++)
                for (l = 0; l < len; l++) {
                    *itable++ = base[i][l];
                    *itable++ = base[i][k];
                    *itable++ = base[i][j];
                }
    }

    for (k = 0; k < 27; k++) {
        double  m = mulmul[k];
        table = muls[k];
        for (j = 3, i = 0; i < 63; i++, j--)
            *table++ = (real) (m * pow(2.0, (double) j / 3.0));
        *table++ = 0.0;
    }
}


static unsigned char*
grp_table_select(short d1, unsigned int idx)
{
    /* RH: it seems to be common, that idx is larger than the table's sizes.
           is it OK to return a zero vector in this case? FIXME
    /*/
    static unsigned char dummy_table[] = { 0,0,0 };
    unsigned int x;
    switch (d1) {
        case 3:
            x = 3*3*3;
            idx = idx < x ? idx : x;
            return &grp_3tab[3 * idx]; 
        case 5: 
            x = 5*5*5;
            idx = idx < x ? idx : x;
            return &grp_5tab[3 * idx]; 
        case 9:
            x = 9*9*9;
            idx = idx < x ? idx : x;
            return &grp_9tab[3 * idx];
        default:
            /* fatal error */
            assert(0);
    }
    return &dummy_table[0];
}

typedef struct sideinfo_layer_II_struct
{
    unsigned char allocation[SBLIMIT][2]; 
    unsigned char scalefactor[SBLIMIT][2][3]; /* subband / channel / block */
} sideinfo_layer_II;



static void
II_step_one(PMPSTR mp, sideinfo_layer_II *si, struct frame *fr)
{
    int     nch = fr->stereo;
    int     sblimit = fr->II_sblimit;
    int     jsbound = (fr->mode == MPG_MD_JOINT_STEREO) ? (fr->mode_ext << 2) + 4 : fr->II_sblimit;
    struct al_table2 const *alloc1 = fr->alloc;
    unsigned char scfsi[SBLIMIT][2];
    int     i, ch;

    memset(si, 0, sizeof(*si));
    if (jsbound > sblimit)
        jsbound = sblimit;
    if (nch == 2) {
        for (i = 0; i < jsbound; ++i) {
            short   step = alloc1->bits;
            unsigned char b0 = get_leq_8_bits(mp, step);
            unsigned char b1 = get_leq_8_bits(mp, step);
            alloc1 += ((size_t)1 << step);
            si->allocation[i][0] = b0;
            si->allocation[i][1] = b1;
        }
        for (i = jsbound; i < sblimit; ++i) {
            short   step = alloc1->bits;
            unsigned char b0 = get_leq_8_bits(mp, step);
            alloc1 += ((size_t)1 << step);
            si->allocation[i][0] = b0;
            si->allocation[i][1] = b0;
        }
        for (i = 0; i < sblimit; ++i) {
            unsigned char n0 = si->allocation[i][0];
            unsigned char n1 = si->allocation[i][1];
            unsigned char b0 = n0 ? get_leq_8_bits(mp, 2) : 0;
            unsigned char b1 = n1 ? get_leq_8_bits(mp, 2) : 0;
            scfsi[i][0] = b0;
            scfsi[i][1] = b1;
        }
    }
    else {              /* mono */
        for (i = 0; i < sblimit; ++i) {
            short   step = alloc1->bits;
            unsigned char b0 = get_leq_8_bits(mp, step);
            alloc1 += ((size_t)1 << step);
            si->allocation[i][0] = b0;
        }
        for (i = 0; i < sblimit; ++i) {
            unsigned char n0 = si->allocation[i][0];
            unsigned char b0 = n0 ? get_leq_8_bits(mp, 2) : 0;
            scfsi[i][0] = b0;
        }
    }
    for (i = 0; i < sblimit; ++i) {
        for (ch = 0; ch < nch; ++ch) {
            unsigned char s0 = 0, s1 = 0, s2 = 0;
            if (si->allocation[i][ch]) {
                switch (scfsi[i][ch]) {
                    case 0:
                        s0 = get_leq_8_bits(mp, 6);
                        s1 = get_leq_8_bits(mp, 6);
                        s2 = get_leq_8_bits(mp, 6);
                        break;
                    case 1:
                        s0 = get_leq_8_bits(mp, 6);
                        s1 = s0;
                        s2 = get_leq_8_bits(mp, 6);
                        break;
                    case 2:
                        s0 = get_leq_8_bits(mp, 6);
                        s1 = s0;
                        s2 = s0;
                        break;
                    case 3:
                        s0 = get_leq_8_bits(mp, 6); 
                        s1 = get_leq_8_bits(mp, 6);
                        s2 = s1;
                        break;
                    default:
                        assert(0);
                }
            }
            si->scalefactor[i][ch][0] = s0;
            si->scalefactor[i][ch][1] = s1;
            si->scalefactor[i][ch][2] = s2;
        }
    }
}

static void
II_step_two(PMPSTR mp, sideinfo_layer_II* si, struct frame *fr, int gr, real fraction[2][4][SBLIMIT])
{
    struct al_table2 const *alloc1 = fr->alloc;
    int     sblimit = fr->II_sblimit;
    int     jsbound = (fr->mode == MPG_MD_JOINT_STEREO) ? (fr->mode_ext << 2) + 4 : fr->II_sblimit;
    int     i, ch, nch = fr->stereo;
    double  cm, r0, r1, r2;

    if (jsbound > sblimit)
        jsbound = sblimit;
    for (i = 0; i < jsbound; ++i) {
        short   step = alloc1->bits;
        for (ch = 0; ch < nch; ++ch) {
            unsigned char ba = si->allocation[i][ch];
            if (ba) {
                unsigned char x1 = si->scalefactor[i][ch][gr];
                struct al_table2 const *alloc2 = alloc1 + ba;
                short   k = alloc2->bits;
                short   d1 = alloc2->d;
                assert( k <= 16 );
                k = (k <= 16) ? k : 16;
                assert( x1 < 64 );
                x1 = (x1 < 64) ? x1 : 63;
                if (d1 < 0) {
                    int v0 = getbits(mp, k);
                    int v1 = getbits(mp, k);
                    int v2 = getbits(mp, k);
                    cm = muls[k][x1];
                    r0 = (v0 + d1) * cm;
                    r1 = (v1 + d1) * cm;
                    r2 = (v2 + d1) * cm;
                }
                else {
                    unsigned int idx = getbits(mp, k);
                    unsigned char *tab = grp_table_select(d1, idx);
                    unsigned char k0 = tab[0];
                    unsigned char k1 = tab[1];
                    unsigned char k2 = tab[2];
                    r0 = muls[k0][x1];
                    r1 = muls[k1][x1];
                    r2 = muls[k2][x1];
                }
                fraction[ch][0][i] = (real) r0;
                fraction[ch][1][i] = (real) r1;
                fraction[ch][2][i] = (real) r2;
            }
            else {
                fraction[ch][0][i] = fraction[ch][1][i] = fraction[ch][2][i] = 0.0;
            }
        }
        alloc1 += ((size_t)1 << step);
    }

    for (i = jsbound; i < sblimit; i++) {
        short   step = alloc1->bits;
        unsigned char ba = si->allocation[i][0];
        if (ba) {
            struct al_table2 const *alloc2 = alloc1 + ba;
            short   k = alloc2->bits;
            short   d1 = alloc2->d;
            assert( k <= 16 );
            k = (k <= 16) ? k : 16;
            if (d1 < 0) {
                int v0 = getbits(mp, k);
                int v1 = getbits(mp, k);
                int v2 = getbits(mp, k);
                for (ch = 0; ch < nch; ++ch) {
                    unsigned char x1 = si->scalefactor[i][ch][gr];
                    assert( x1 < 64 );
                    x1 = (x1 < 64) ? x1 : 63;
                    cm = muls[k][x1];
                    r0 = (v0 + d1) * cm;
                    r1 = (v1 + d1) * cm;
                    r2 = (v2 + d1) * cm;
                    fraction[ch][0][i] = (real) r0;
                    fraction[ch][1][i] = (real) r1;
                    fraction[ch][2][i] = (real) r2;
                }
            }
            else {
                unsigned int idx = getbits(mp, k);
                unsigned char *tab = grp_table_select(d1, idx);
                unsigned char k0 = tab[0];
                unsigned char k1 = tab[1];
                unsigned char k2 = tab[2];
                for (ch = 0; ch < nch; ++ch) {
                    unsigned char x1 = si->scalefactor[i][ch][gr];
                    assert( x1 < 64 );
                    x1 = (x1 < 64) ? x1 : 63;
                    r0 = muls[k0][x1];
                    r1 = muls[k1][x1];
                    r2 = muls[k2][x1];
                    fraction[ch][0][i] = (real) r0;
                    fraction[ch][1][i] = (real) r1;
                    fraction[ch][2][i] = (real) r2;
                }
            }
        }
        else {
            fraction[0][0][i] = fraction[0][1][i] = fraction[0][2][i] = 0.0;
            fraction[1][0][i] = fraction[1][1][i] = fraction[1][2][i] = 0.0;
        }
        alloc1 += ((size_t)1 << step);
    }
    if (sblimit > fr->down_sample_sblimit) {
        sblimit = fr->down_sample_sblimit; 
    }
    for (ch = 0; ch < nch; ++ch) {
        for (i = sblimit; i < SBLIMIT; ++i) {
            fraction[ch][0][i] = fraction[ch][1][i] = fraction[ch][2][i] = 0.0;
        }
    }
}

static void
II_select_table(struct frame *fr)
{
    /* *INDENT-OFF* */
  static const int translate[3][2][16] =
   { { { 0,2,2,2,2,2,2,0,0,0,1,1,1,1,1,0 } ,
       { 0,2,2,0,0,0,1,1,1,1,1,1,1,1,1,0 } } ,
     { { 0,2,2,2,2,2,2,0,0,0,0,0,0,0,0,0 } ,
       { 0,2,2,0,0,0,0,0,0,0,0,0,0,0,0,0 } } ,
     { { 0,3,3,3,3,3,3,0,0,0,1,1,1,1,1,0 } ,
       { 0,3,3,0,0,0,1,1,1,1,1,1,1,1,1,0 } } };
    /* *INDENT-ON* */

    int     table, sblim;
    static const struct al_table2 *tables[5] = { alloc_0, alloc_1, alloc_2, alloc_3, alloc_4 };
    static const int sblims[5] = { 27, 30, 8, 12, 30 };

    if (fr->lsf)
        table = 4;
    else
        table = translate[fr->sampling_frequency][2 - fr->stereo][fr->bitrate_index];
    sblim = sblims[table];

    fr->alloc = (struct al_table2 const *) tables[table];
    fr->II_sblimit = sblim;
}


int
decode_layer2_sideinfo(PMPSTR mp)
{
    (void) mp;
    /* FIXME: extract side information and check values */
    return 0;
}

int
decode_layer2_frame(PMPSTR mp, unsigned char *pcm_sample, int *pcm_point)
{
    real    fraction[2][4][SBLIMIT]; /* pick_table clears unused subbands */
    sideinfo_layer_II si;
    struct frame *fr = &(mp->fr);
    int     single = fr->single;
    int     i, j, clip = 0;

    II_select_table(fr);
    II_step_one(mp, &si, fr);

    if (fr->stereo == 1 || single == 3)
        single = 0;

    if (single >= 0) {
        for (i = 0; i < SCALE_BLOCK; i++) {
            II_step_two(mp, &si, fr, i >> 2, fraction);
            for (j = 0; j < 3; j++) {
                clip += synth_1to1_mono(mp, fraction[single][j], pcm_sample, pcm_point);
            }
        }
    }
    else {
        for (i = 0; i < SCALE_BLOCK; i++) {
            II_step_two(mp, &si, fr, i >> 2, fraction);
            for (j = 0; j < 3; j++) {
                int     p1 = *pcm_point;
                clip += synth_1to1(mp, fraction[0][j], 0, pcm_sample, &p1);
                clip += synth_1to1(mp, fraction[1][j], 1, pcm_sample, pcm_point);
            }
        }
    }

    return clip;
}
