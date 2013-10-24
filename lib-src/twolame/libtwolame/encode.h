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


#ifndef	_ENCODE_H_
#define	_ENCODE_H_

int encode_init(twolame_options * glopts);

void scalefactor_calc(FLOAT sb_sample[][3][SCALE_BLOCK][SBLIMIT],
                      unsigned int scalar[][3][SBLIMIT], int nch, int sblimit);

void combine_lr(FLOAT sb_sample[2][3][SCALE_BLOCK][SBLIMIT],
                FLOAT joint_sample[3][SCALE_BLOCK][SBLIMIT], int sblimit);

void find_sf_max(twolame_options * glopts,
                 unsigned int sf_index[2][3][SBLIMIT], FLOAT sf_max[2][SBLIMIT]);

void sf_transmission_pattern(twolame_options * glopts,
                             unsigned int sf_index[2][3][SBLIMIT],
                             unsigned int sf_selectinfo[2][SBLIMIT]);

void write_header(twolame_options * glopts, bit_stream * bs);

void write_bit_alloc(twolame_options * glopts, unsigned int bit_alloc[2][SBLIMIT], bit_stream * bs);

void write_scalefactors(twolame_options * glopts,
                        unsigned int bit_alloc[2][SBLIMIT],
                        unsigned int sf_selectinfo[2][SBLIMIT],
                        unsigned int scalar[2][3][SBLIMIT], bit_stream * bs);

void subband_quantization(twolame_options * glopts,
                          unsigned int sf_index[2][3][SBLIMIT],
                          FLOAT sb_samples[2][3][SCALE_BLOCK][SBLIMIT],
                          unsigned int j_scale[3][SBLIMIT],
                          FLOAT j_samps[3][SCALE_BLOCK][SBLIMIT],
                          unsigned int bit_alloc[2][SBLIMIT],
                          unsigned int sbband[2][3][SCALE_BLOCK][SBLIMIT]);

void write_samples(twolame_options * glopts,
                   unsigned int sbband[2][3][SCALE_BLOCK][SBLIMIT],
                   unsigned int bit_alloc[2][SBLIMIT], bit_stream * bs);


/*******************************************************
   Bit Allocation Stuff 
******************************************************/

int bits_for_nonoise(twolame_options * glopts, FLOAT SMR[2][SBLIMIT],
                     unsigned int scfsi[2][SBLIMIT], FLOAT min_mnr,
                     unsigned int bit_alloc[2][SBLIMIT]);

int init_bit_allocation(twolame_options * glopts);

void main_bit_allocation(twolame_options * glopts, FLOAT SMR[2][SBLIMIT],
                         unsigned int scfsi[2][SBLIMIT],
                         unsigned int bit_alloc[2][SBLIMIT], int *adb);

int vbr_bit_allocation(twolame_options * glopts, FLOAT SMR[2][SBLIMIT],
                       unsigned int scfsi[2][SBLIMIT],
                       unsigned int bit_alloc[2][SBLIMIT], int *adb);

int a_bit_allocation(twolame_options * glopts, FLOAT SMR[2][SBLIMIT],
                     unsigned int scfsi[2][SBLIMIT], unsigned int bit_alloc[2][SBLIMIT], int *adb);

#endif


// vim:ts=4:sw=4:nowrap: 
