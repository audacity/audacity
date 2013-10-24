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
#include <string.h>
#include <math.h>

#include "twolame.h"
#include "common.h"
#include "mem.h"
#include "bitbuffer.h"
#include "enwindow.h"
#include "subband.h"


static void create_dct_matrix(FLOAT filter[16][32])
{
    register int i, k;

    for (i = 0; i < 16; i++)
        for (k = 0; k < 32; k++) {
            if ((filter[i][k] = 1e9 * cos((FLOAT) ((2 * i + 1) * k * PI64))) >= 0)
                modf(filter[i][k] + 0.5, &filter[i][k]);
            else
                modf(filter[i][k] - 0.5, &filter[i][k]);
            filter[i][k] *= 1e-9;
        }
}

int init_subband(subband_mem * smem)
{
    register int i, j;
    smem->off[0] = 0;
    smem->off[1] = 0;
    smem->half[0] = 0;
    smem->half[1] = 0;
    for (i = 0; i < 2; i++)
        for (j = 0; j < 512; j++)
            smem->x[i][j] = 0;
    create_dct_matrix(smem->m);

    return 0;
}


void window_filter_subband(subband_mem * smem, short *pBuffer, int ch, FLOAT s[SBLIMIT])
{
    register int i, j;
    int pa, pb, pc, pd, pe, pf, pg, ph;
    FLOAT t;
    FLOAT *dp, *dp2;
    const FLOAT *pEnw;
    FLOAT y[64];
    FLOAT yprime[32];

    dp = smem->x[ch] + smem->off[ch] + smem->half[ch] * 256;

    /* replace 32 oldest samples with 32 new samples */
    for (i = 0; i < 32; i++)
        dp[(31 - i) * 8] = (FLOAT) pBuffer[i] / SCALE;

    // looks like "school example" but does faster ...
    dp = (smem->x[ch] + smem->half[ch] * 256);
    pa = smem->off[ch];
    pb = (pa + 1) % 8;
    pc = (pa + 2) % 8;
    pd = (pa + 3) % 8;
    pe = (pa + 4) % 8;
    pf = (pa + 5) % 8;
    pg = (pa + 6) % 8;
    ph = (pa + 7) % 8;

    for (i = 0; i < 32; i++) {
        dp2 = dp + i * 8;
        pEnw = enwindow + i;
        t = dp2[pa] * pEnw[0];
        t += dp2[pb] * pEnw[64];
        t += dp2[pc] * pEnw[128];
        t += dp2[pd] * pEnw[192];
        t += dp2[pe] * pEnw[256];
        t += dp2[pf] * pEnw[320];
        t += dp2[pg] * pEnw[384];
        t += dp2[ph] * pEnw[448];
        y[i] = t;
    }

    yprime[0] = y[16];          // Michael Chen's dct filter

    dp = smem->half[ch] ? smem->x[ch] : (smem->x[ch] + 256);
    pa = smem->half[ch] ? (smem->off[ch] + 1) & 7 : smem->off[ch];
    pb = (pa + 1) % 8;
    pc = (pa + 2) % 8;
    pd = (pa + 3) % 8;
    pe = (pa + 4) % 8;
    pf = (pa + 5) % 8;
    pg = (pa + 6) % 8;
    ph = (pa + 7) % 8;

    for (i = 0; i < 32; i++) {
        dp2 = dp + i * 8;
        pEnw = enwindow + i + 32;
        t = dp2[pa] * pEnw[0];
        t += dp2[pb] * pEnw[64];
        t += dp2[pc] * pEnw[128];
        t += dp2[pd] * pEnw[192];
        t += dp2[pe] * pEnw[256];
        t += dp2[pf] * pEnw[320];
        t += dp2[pg] * pEnw[384];
        t += dp2[ph] * pEnw[448];
        y[i + 32] = t;
        // 1st pass on Michael Chen's dct filter
        if (i > 0 && i < 17)
            yprime[i] = y[i + 16] + y[16 - i];
    }

    // 2nd pass on Michael Chen's dct filter
    for (i = 17; i < 32; i++)
        yprime[i] = y[i + 16] - y[80 - i];

    for (i = 15; i >= 0; i--) {
        register FLOAT s0 = 0.0, s1 = 0.0;
        register FLOAT *mp = smem->m[i];
        register FLOAT *xinp = yprime;
        for (j = 0; j < 8; j++) {
            s0 += *mp++ * *xinp++;
            s1 += *mp++ * *xinp++;
            s0 += *mp++ * *xinp++;
            s1 += *mp++ * *xinp++;
        }
        s[i] = s0 + s1;
        s[31 - i] = s0 - s1;
    }

    smem->half[ch] = (smem->half[ch] + 1) & 1;

    if (smem->half[ch] == 1)
        smem->off[ch] = (smem->off[ch] + 7) & 7;
}


// vim:ts=4:sw=4:nowrap: 
