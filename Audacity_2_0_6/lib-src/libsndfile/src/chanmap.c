/*
** Copyright (C) 2009-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
**	Mostly from "Apple Core Audio Format Specification 1.0":
**
**		http://developer.apple.com/documentation/MusicAudio/Reference/CAFSpec/CAFSpec.pdf
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "sndfile.h"
#include "common.h"
#include "chanmap.h"


static const AIFF_CAF_CHANNEL_MAP zero_chan [] =
{	{	(0 << 16) | 0, NULL, "Use channel descriptions." },
	{	(1 << 16) | 0, NULL, "Use channel bitmap." }
} ; /* zero_chan */


static const int one_chan_mono [1] = {	SF_CHANNEL_MAP_MONO } ;

static const AIFF_CAF_CHANNEL_MAP one_chan [] =
{	{	(100 << 16) | 1, one_chan_mono, "mono" }
} ; /* one_chan */


static const int two_channel_stereo [2] = {	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT } ;

static const AIFF_CAF_CHANNEL_MAP two_chan [] =
{	{	(101 << 16) | 2, two_channel_stereo, "stereo (L, R)" },
	{	(102 << 16) | 2, two_channel_stereo, "stereo headphones (L, R)" },
#if 0
	{	(103 << 16) | 2, NULL, "matrix stereo (Lt, Rt)" },
	{	(104 << 16) | 2, NULL, "2 channels (mid, side)" },
	{	(105 << 16) | 2, NULL, "coincident mic pair" },
	{	(106 << 16) | 2, NULL, "binaural stereo (L, R)"
		}
#endif
} ; /* two_chan */


static const int three_channel_mpeg_30a [3] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER } ;
static const int three_channel_mpeg_30b [3] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT } ;
static const int three_channel_itu_21 [3] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int three_channel_dvd_4 [3] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_LFE } ;

static const AIFF_CAF_CHANNEL_MAP three_chan [] =
{	{	(113 << 16) | 3, three_channel_mpeg_30a, "MPEG 3 0 A (L, R, C)" },
	{	(114 << 16) | 3, three_channel_mpeg_30b, "MPEG 3 0 B (C, L, R)" },
	{	(131 << 16) | 3, three_channel_itu_21, "ITU 2.1 (L, R, Cs)" },
	{	(133 << 16) | 3, three_channel_dvd_4, "DVD 4 (L, R, LFE)" }
} ; /* three_chan */


static const int four_channel_ambisonc_b [4] =
	{	SF_CHANNEL_MAP_AMBISONIC_B_W, SF_CHANNEL_MAP_AMBISONIC_B_X, SF_CHANNEL_MAP_AMBISONIC_B_Y, SF_CHANNEL_MAP_AMBISONIC_B_Z } ;
static const int four_channel_quad [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int four_channel_mpeg_40a [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int four_channel_mpeg_40b [4] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int four_channel_itu_23 [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int four_channel_dvd_5 [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int four_channel_dvd_10 [4] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LFE } ;

static const AIFF_CAF_CHANNEL_MAP four_chan [] =
{	{	(107 << 16) | 4, four_channel_ambisonc_b, "ambisonic B (W,  X, Y, Z)" },
	{	(108 << 16) | 4, four_channel_quad, "quad (Lfront, Rfront, Lrear, Rrear)" },
	{	(115 << 16) | 4, four_channel_mpeg_40a, "MPEG 4.0 A (L, R, C, Cs)" },
	{	(116 << 16) | 4, four_channel_mpeg_40b, "MPEG 4.0 B (C, L, R, Cs)" },
	{	(132 << 16) | 4, four_channel_itu_23, "ITU 2.3 (L, R, Ls, Rs)" },
	{	(134 << 16) | 4, four_channel_dvd_5, "DVD 5 (L, R, LFE, Cs)" },
	{	(136 << 16) | 4, four_channel_dvd_10, "DVD 10 (L, R, C, LFE)" }
} ; /* four_chan */


static const int five_channel_pentagonal [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_CENTER } ;
static const int five_channel_mpeg_50_a [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int five_channel_mpeg_50_b [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_CENTER } ;
static const int five_channel_mpeg_50_c [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int five_channel_mpeg_50_d [5] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int five_channel_dvd_6 [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int five_channel_dvd_11 [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int five_channel_dvd_18 [5] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_LFE } ;

static const AIFF_CAF_CHANNEL_MAP five_chan [] =
{	{	(109 << 16) | 5, five_channel_pentagonal, "pentagonal (L,  R, Lrear, Rrear, C)" },
	{	(117 << 16) | 5, five_channel_mpeg_50_a, "MPEG 5.0 A (L, R, C, Ls, Rs)" },
	{	(118 << 16) | 5, five_channel_mpeg_50_b, "MPEG 5.0 B (L, R, Ls, Rs, C)" },
	{	(119 << 16) | 5, five_channel_mpeg_50_c, "MPEG 5.0 C (L, C, R, Ls, Rs,)" },
	{	(120 << 16) | 5, five_channel_mpeg_50_d, "MPEG 5.0 D (C, L, R, Ls, Rs)" },
	{	(135 << 16) | 5, five_channel_dvd_6, "DVD 6 (L, R, LFE, Ls, Rs)" },
	{	(137 << 16) | 5, five_channel_dvd_11, "DVD 11 (L, R, C, LFE, Cs)" },
	{	(138 << 16) | 5, five_channel_dvd_18, "DVD 18 (L, R, Ls, Rs, LFE)" }
} ; /* five_chan */


static const int six_channel_mpeg_51_a [6] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT } ;
static const int six_channel_mpeg_51_b [6] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LFE } ;
static const int six_channel_mpeg_51_c [6] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_LFE } ;
static const int six_channel_mpeg_51_d [6] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_LFE } ;
static const int six_channel_audio_unit_60 [6] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int six_channel_aac_60 [6] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_REAR_CENTER } ;

static const AIFF_CAF_CHANNEL_MAP six_chan [] =
{	{	(110 << 16) | 6, NULL, "hexagonal (L, R, Lr, Rr, C, Rear)" },
	{	(121 << 16) | 6, six_channel_mpeg_51_a, "MPEG 5.1 A (L, R, C, LFE, Ls, Rs)" },
	{	(122 << 16) | 6, six_channel_mpeg_51_b, "MPEG 5.1 B (L, R, Ls, Rs, C, LFE)" },
	{	(123 << 16) | 6, six_channel_mpeg_51_c, "MPEG 5.1 C (L, C, R, Ls, Rs, LFE)" },
	{	(124 << 16) | 6, six_channel_mpeg_51_d, "MPEG 5.1 D (C, L, R, Ls, Rs, LFE)" },
	{	(139 << 16) | 6, six_channel_audio_unit_60, "AudioUnit 6.0 (L, R, Ls, Rs, C, Cs)" },
	{	(141 << 16) | 6, six_channel_aac_60, "AAC 6.0 (C, L, R, Ls, Rs, Cs)" }
} ; /* six_chan */


static const int six_channel_mpeg_61a [7] =
	{	SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LFE, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_REAR_CENTER } ;
static const int six_channel_aac_61 [7] =
	{	SF_CHANNEL_MAP_CENTER, SF_CHANNEL_MAP_LEFT, SF_CHANNEL_MAP_RIGHT, SF_CHANNEL_MAP_REAR_LEFT, SF_CHANNEL_MAP_REAR_RIGHT, SF_CHANNEL_MAP_REAR_CENTER, SF_CHANNEL_MAP_LFE } ;

static const AIFF_CAF_CHANNEL_MAP seven_chan [] =
{	{	(125 << 16) | 7, six_channel_mpeg_61a, "MPEG 6.1 A (L, R, C, LFE, Ls, Rs, Cs)" },
	{	(140 << 16) | 7, NULL, "AudioUnit 7.0 (L, R, Ls, Rs, C, Rls, Rrs)" },
	{	(142 << 16) | 7, six_channel_aac_61, "AAC 6.1 (C, L, R, Ls, Rs, Cs, Lfe)" },
	{	(143 << 16) | 7, NULL, "AAC 7.0 (C, L, R, Ls, Rs, Rls, Rrs,)" }
} ; /* seven_chan */


static const AIFF_CAF_CHANNEL_MAP eight_chan [] =
{	{	(111 << 16) | 8, NULL,
		// front left, front right, rear left, rear right,
		// front center, rear center, side left, side right
		"octagonal (Lf, Rf, Lr, Rr, Cf, Cr, Ls, Rs)"
		},
	{	(112 << 16) | 8, NULL,
		// left, right, rear left, rear right
		// top left, top right, top rear left, top rear right
		"cube (L, R, Lrear, Rrear, Ltop, Rtop, Ltoprear, Rtoprear)"
		},
	{	(126 << 16) | 8, NULL, "MPEG 7.1 A (L, R, C, LFE, Ls, Rs, Lc, Rc)" },
	{	(127 << 16) | 8, NULL, "MPEG 7.1 B (C, Lc, Rc, L, R, Ls, Rs, LFE)" },
	{	(128 << 16) | 8, NULL, "MPEG 7.1 C (L, R, C, LFE, Ls, R, Rls, Rrs)" },
	{	(129 << 16) | 8, NULL, "Emagic Default 7.1 (L, R, Ls, Rs, C, LFE, Lc, Rc)" },
	{	(130 << 16) | 8, NULL,
		// (ITU_5_1 plus a matrix encoded stereo mix)
		"SMPTE DTV (L, R, C, LFE, Ls, Rs, Lt, Rt)"
		},
	{	(144 << 16) | 8, NULL, "AAC octagonal (C, L, R, Ls, Rs, Rls, Rrs, Cs)" }
} ; /* eight_chan */



#if 0

TMH_10_2_std = (145 << 16) | 16,
// L R C Vhc Lsd Rsd Ls Rs Vhl Vhr Lw Rw Csd Cs LFE1 LFE2

TMH_10_2_full = (146 << 16) | 21,
// TMH_10_2_std plus: Lc Rc HI VI Haptic

#endif


typedef struct
{	const AIFF_CAF_CHANNEL_MAP * map ;
	int len ;
} MAP_MAP ;

static const MAP_MAP map [] =
{	{ zero_chan,	ARRAY_LEN (zero_chan) },
	{ one_chan,		ARRAY_LEN (one_chan) },
	{ two_chan,		ARRAY_LEN (two_chan) },
	{ three_chan,	ARRAY_LEN (three_chan) },
	{ four_chan,	ARRAY_LEN (four_chan) },
	{ five_chan,	ARRAY_LEN (five_chan) },
	{ six_chan,		ARRAY_LEN (six_chan) },
	{ seven_chan,	ARRAY_LEN (seven_chan) },
	{ eight_chan,	ARRAY_LEN (eight_chan) }
} ; /* map */


int
aiff_caf_find_channel_layout_tag (const int *chan_map, int channels)
{	const AIFF_CAF_CHANNEL_MAP * curr_map ;
	unsigned k, len ;

	if (channels < 1 || channels > ARRAY_LEN (map))
		return 0 ;

	curr_map = map [channels].map ;
	len = map [channels].len ;

	for (k = 0 ; k < len ; k++)
		if (curr_map [k].channel_map != NULL)
			if (memcmp (chan_map, curr_map [k].channel_map, channels * sizeof (chan_map [0])) == 0)
				return curr_map [k].channel_layout_tag ;

	return 0 ;
} /* aiff_caf_find_channel_layout_tag */

const AIFF_CAF_CHANNEL_MAP *
aiff_caf_of_channel_layout_tag (int tag)
{	const AIFF_CAF_CHANNEL_MAP * curr_map ;
	unsigned k, len ;
	int channels = tag & 0xffff ;

	if (channels < 0 || channels > ARRAY_LEN (map))
		return NULL ;

	curr_map = map [channels].map ;
	len = map [channels].len ;

	for (k = 0 ; k < len ; k++)
		if (curr_map [k].channel_layout_tag == tag)
			return curr_map + k ;

	return NULL ;
} /* aiff_caf_of_channel_layout_tag */
