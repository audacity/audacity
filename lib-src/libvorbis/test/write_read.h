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

/* Write supplied data to an Ogg/Vorbis file with specified filename at
 * specified sample rate. Assumes a single channel of audio. */
void write_vorbis_data_or_die (const char *filename, int srate, float q,
                               const float * data, int count, int ch) ;

/* Read given Ogg/Vorbis file into data specified data array. This
 * function is basically the inverse of the one above. Again, assumes
 * a single channel of audio. */
void read_vorbis_data_or_die (const char *filename, int srate,
                        float * data, int count) ;

