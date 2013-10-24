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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

#include "twolame.h"
#include "bitbuffer.h"
#include "common.h"
#include "mem.h"
#include "util.h"
#include "energy.h"



/*********************************************/

int twolame_set_mode(twolame_options * glopts, TWOLAME_MPEG_mode mode)
{
    if (mode < TWOLAME_AUTO_MODE || mode > TWOLAME_MONO) {
        fprintf(stderr, "invalid mode %i\n", mode);
        return (-1);
    }
    glopts->mode = mode;
    return (0);
}

TWOLAME_MPEG_mode twolame_get_mode(twolame_options * glopts)
{
    return (glopts->mode);
}

const char *twolame_get_mode_name(twolame_options * glopts)
{
    static const char *mode_name[6] =
        { "Auto", "Stereo", "J-Stereo", "Dual-Channel", "Mono", "Illegal Mode" };
    int mode = glopts->mode;
    if (mode >= TWOLAME_AUTO_MODE && mode <= TWOLAME_MONO)
        return (mode_name[mode + 1]);
    else
        return (mode_name[5]);
}


int twolame_set_psymodel(twolame_options * glopts, int psymodel)
{
    glopts->psymodel = psymodel;
    return (0);
}

int twolame_get_psymodel(twolame_options * glopts)
{
    return (glopts->psymodel);
}


/* number of channels on the input stream */
int twolame_set_num_channels(twolame_options * glopts, int num_channels)
{
    glopts->num_channels_in = num_channels;
    return 0;
}

int twolame_get_num_channels(twolame_options * glopts)
{
    return (glopts->num_channels_in);
}

int twolame_set_scale(twolame_options * glopts, float scale)
{
    if (scale < 0) {
        fprintf(stderr, "invalid scaling amount %f\n", scale);
        return (-1);
    }
    glopts->scale = scale;
    return 0;
}

float twolame_get_scale(twolame_options * glopts)
{
    return (glopts->scale);
}

int twolame_set_scale_left(twolame_options * glopts, float scale)
{
    if (scale < 0) {
        fprintf(stderr, "invalid scaling amount %f\n", scale);
        return (-1);
    }
    glopts->scale_left = scale;
    return 0;
}

float twolame_get_scale_left(twolame_options * glopts)
{
    return (glopts->scale_left);
}

int twolame_set_scale_right(twolame_options * glopts, float scale)
{
    if (scale < 0) {
        fprintf(stderr, "invalid scaling amount %f\n", scale);
        return (-1);
    }
    glopts->scale_right = scale;
    return 0;
}

float twolame_get_scale_right(twolame_options * glopts)
{
    return (glopts->scale_right);
}


int twolame_set_in_samplerate(twolame_options * glopts, int samplerate)
{
    glopts->samplerate_in = samplerate;
    return (0);
}

int twolame_get_in_samplerate(twolame_options * glopts)
{
    return (glopts->samplerate_in);
}

int twolame_set_out_samplerate(twolame_options * glopts, int samplerate)
{
    glopts->samplerate_out = samplerate;
    return (0);
}

int twolame_get_out_samplerate(twolame_options * glopts)
{
    return (glopts->samplerate_out);
}

int twolame_set_brate(twolame_options * glopts, int bitrate)
{
    glopts->bitrate = bitrate;
    return (0);
}

int twolame_get_brate(twolame_options * glopts)
{
    return (glopts->bitrate);
}


/* Because the LAME API isn't always the best way ;) */
int twolame_set_bitrate(twolame_options * glopts, int bitrate)
{
    glopts->bitrate = bitrate;
    return (0);
}

int twolame_get_bitrate(twolame_options * glopts)
{
    return (glopts->bitrate);
}


int twolame_set_emphasis(twolame_options * glopts, TWOLAME_Emphasis emphasis)
{
    if (emphasis != TWOLAME_EMPHASIS_N &&
        emphasis != TWOLAME_EMPHASIS_5 && emphasis != TWOLAME_EMPHASIS_C)
        return (-1);
    glopts->emphasis = emphasis;
    return (0);
}

TWOLAME_Emphasis twolame_get_emphasis(twolame_options * glopts)
{
    return (glopts->emphasis);
}

int twolame_set_error_protection(twolame_options * glopts, int error_protection)
{
    if (error_protection)
        glopts->error_protection = TRUE;
    else
        error_protection = FALSE;
    return (0);
}

int twolame_get_error_protection(twolame_options * glopts)
{
    return (glopts->error_protection);
}

int twolame_set_copyright(twolame_options * glopts, int copyright)
{
    if (copyright)
        glopts->copyright = TRUE;
    else
        glopts->copyright = FALSE;
    return (0);
}

int twolame_get_copyright(twolame_options * glopts)
{
    return (glopts->copyright);
}

int twolame_set_original(twolame_options * glopts, int original)
{
    if (original)
        glopts->original = TRUE;
    else
        glopts->original = FALSE;
    return (0);
}

int twolame_get_original(twolame_options * glopts)
{
    return (glopts->original);
}

int twolame_set_padding(twolame_options * glopts, TWOLAME_Padding padding)
{
    if (padding)
        glopts->padding = TRUE;
    else
        glopts->padding = FALSE;

    return (0);
}

TWOLAME_Padding twolame_get_padding(twolame_options * glopts)
{
    return (glopts->padding);
}

int twolame_set_VBR(twolame_options * glopts, int vbr)
{
    if (vbr)
        glopts->vbr = TRUE;
    else
        glopts->vbr = FALSE;
    return (0);
}

int twolame_get_VBR(twolame_options * glopts)
{
    return (glopts->vbr);
}

int twolame_set_VBR_level(twolame_options * glopts, float level)
{
    // Limit is -50 to 50, but useful range is -10 to 10
    if (fabs(level) > 50.0)
        return (-1);
    else
        glopts->vbrlevel = level;
    return (0);
}

float twolame_get_VBR_level(twolame_options * glopts)
{
    return (glopts->vbrlevel);
}

// Deprecated:
int twolame_set_VBR_q(twolame_options * glopts, float level)
{
    // Limit is -50 to 50, but useful range is -10 to 10
    if (fabs(level) > 50.0)
        return (-1);
    else
        glopts->vbrlevel = level;
    return (0);
}

// Deprecated:
float twolame_get_VBR_q(twolame_options * glopts)
{
    return (glopts->vbrlevel);
}

int twolame_set_ATH_level(twolame_options * glopts, float level)
{
    glopts->athlevel = level;
    return (0);
}

float twolame_get_ATH_level(twolame_options * glopts)
{
    return (glopts->athlevel);
}

int twolame_set_quick_mode(twolame_options * glopts, int quickmode)
{
    if (quickmode)
        glopts->quickmode = TRUE;
    else
        glopts->quickmode = FALSE;
    return (0);
}

int twolame_get_quick_mode(twolame_options * glopts)
{
    return (glopts->quickmode);
}

int twolame_set_quick_count(twolame_options * glopts, int quickcount)
{
    glopts->quickcount = quickcount;
    return (0);
}

int twolame_get_quick_count(twolame_options * glopts)
{
    return (glopts->quickcount);
}


int twolame_set_verbosity(twolame_options * glopts, int verbosity)
{
    if (verbosity < 0 || verbosity > 10) {
        fprintf(stderr, "invalid verbosity level %i\n", verbosity);
        return (-1);
    }
    glopts->verbosity = verbosity;
    return (0);
}

int twolame_get_verbosity(twolame_options * glopts)
{
    return (glopts->verbosity);
}

int twolame_set_VBR_max_bitrate_kbps(twolame_options * glopts, int bitrate)
{
    glopts->vbr_max_bitrate = bitrate;
    return (0);
}

int twolame_get_VBR_max_bitrate_kbps(twolame_options * glopts)
{
    return (glopts->vbr_max_bitrate);
}

int twolame_set_num_ancillary_bits(twolame_options * glopts, int num)
{
    if (num < 0)
        return (-1);
    glopts->num_ancillary_bits = num;
    return (0);
}

int twolame_get_num_ancillary_bits(twolame_options * glopts)
{
    return (glopts->num_ancillary_bits);
}

int twolame_set_energy_levels(twolame_options * glopts, int energylevels)
{
    if (energylevels) {
        glopts->do_energy_levels = TRUE;
    } else {
        glopts->do_energy_levels = FALSE;
    }

    return (0);
}


int twolame_get_energy_levels(twolame_options * glopts)
{
    return (glopts->do_energy_levels);
}

int twolame_set_version(twolame_options * glopts, TWOLAME_MPEG_version version)
{
    if (version != 0 && version != 1)
        return (-1);
    glopts->version = version;
    return (0);
}

TWOLAME_MPEG_version twolame_get_version(twolame_options * glopts)
{
    return (glopts->version);
}

const char *twolame_get_version_name(twolame_options * glopts)
{
    return twolame_mpeg_version_name(glopts->version);
}






/* WARNING: DAB support is currently broken */

int twolame_set_DAB(twolame_options * glopts, int dab)
{

    fprintf(stderr, "Warning: DAB support is currently broken in this version of TwoLAME.\n");

    if (dab)
        glopts->do_dab = TRUE;
    else
        glopts->do_dab = FALSE;
    return (0);
}

int twolame_get_DAB(twolame_options * glopts)
{
    return (glopts->do_dab);
}

int twolame_set_DAB_xpad_length(twolame_options * glopts, int length)
{
    if (length < 0)
        return (-1);
    glopts->dab_xpad_len = length;
    return (0);
}

int twolame_get_DAB_xpad_length(twolame_options * glopts)
{
    return (glopts->dab_xpad_len);
}

int twolame_set_DAB_crc_length(twolame_options * glopts, int length)
{
    if (length < 0)
        return (-1);
    else
        glopts->dab_crc_len = length;
    return (0);
}

int twolame_get_DAB_crc_length(twolame_options * glopts)
{
    return (glopts->dab_crc_len);
}


// vim:ts=4:sw=4:nowrap: 
