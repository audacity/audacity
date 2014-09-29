/********************************************************************
 *                                                                  *
 * THIS FILE IS PART OF THE OggVorbis SOFTWARE CODEC SOURCE CODE.   *
 * USE, DISTRIBUTION AND REPRODUCTION OF THIS LIBRARY SOURCE IS     *
 * GOVERNED BY A BSD-STYLE SOURCE LICENSE INCLUDED WITH THIS SOURCE *
 * IN 'COPYING'. PLEASE READ THESE TERMS BEFORE DISTRIBUTING.       *
 *                                                                  *
 * THE OggVorbis SOURCE CODE IS (C) COPYRIGHT 1994-2007             *
 * by the Xiph.Org Foundation http://www.xiph.org/                  *
 *                                                                  *
 ********************************************************************

 function: utility functions for vorbis codec test suite.
 last mod: $Id: util.c 13293 2007-07-24 00:09:47Z erikd $

 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <errno.h>

#include <vorbis/codec.h>
#include <vorbis/vorbisenc.h>

#include "util.h"

void
gen_windowed_sine (float *data, int len, float maximum)
{    int k ;

    memset (data, 0, len * sizeof (float)) ;

    len /= 2 ;

    for (k = 0 ; k < len ; k++)
    {    data [k] = sin (2.0 * k * M_PI * 1.0 / 32.0 + 0.4) ;

        /* Apply Hanning Window. */
        data [k] *= maximum * (0.5 - 0.5 * cos (2.0 * M_PI * k / ((len) - 1))) ;
        }

    return ;
}

void
set_data_in (float * data, unsigned len, float value)
{        unsigned k ;

        for (k = 0 ; k < len ; k++)
                data [k] = value ;
}
