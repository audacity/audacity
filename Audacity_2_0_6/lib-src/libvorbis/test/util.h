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

#define ARRAY_LEN(x)   (sizeof(x)/sizeof(x[0]))

/* Create simple test data consisting of a windowed sine wave. */
void gen_windowed_sine (float *data, int len, float maximum) ;

/* Set len values of data array to given value. */
void set_data_in (float * data, unsigned len, float value) ;
