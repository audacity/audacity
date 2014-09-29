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

typedef struct wave_info_struct {
    FILE *soundfile;
    short channels;             // 1 - mono . 2 - stereo. d'uh.
    unsigned long num_samples;  // total number of samples/channel if available
    short samplesize;           // 8 bit or 16bit. could probably just auto scale. but i think
    // we'll just ignore 8bit sound. MFC May03
    int samplerate;             // in Hz
    int byteswap;
} wave_info_t;


wave_info_t *wave_init(char *inPath);
int wave_get_samples(wave_info_t * wave_info, short int pcm[], int num_samples);

// vim:ts=4:sw=4:nowrap: 
