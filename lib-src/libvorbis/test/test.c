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

 function: vorbis coded test suite using vorbisfile
 last mod: $Id: test.c 13293 2007-07-24 00:09:47Z erikd $

 ********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "util.h"
#include "write_read.h"

#define DATA_LEN        2048

#define MAX(a,b)        ((a) > (b) ? (a) : (b))


static int check_output (const float * data_in, unsigned len, float allowable);

int
main(void){
  static float data_out [DATA_LEN] ;
  static float data_in [DATA_LEN] ;

  /* Do safest and most used sample rates first. */
  int sample_rates [] = { 44100, 48000, 32000, 22050, 16000, 96000 } ;
  unsigned k ;
  int errors = 0 ;
  int ch;

  gen_windowed_sine (data_out, ARRAY_LEN (data_out), 0.95);

  for(ch=1;ch<=8;ch++){
    float q=-.05;
    printf("\nTesting %d channel%s\n\n",ch,ch==1?"":"s");
    while(q<1.){
      for (k = 0 ; k < ARRAY_LEN (sample_rates); k ++) {
        char filename [64] ;
        snprintf (filename, sizeof (filename), "vorbis_%dch_q%.1f_%u.ogg", ch,q*10,sample_rates [k]);

        printf ("    %-20s : ", filename);
        fflush (stdout);

        /* Set to know value. */
        set_data_in (data_in, ARRAY_LEN (data_in), 3.141);

        write_vorbis_data_or_die (filename, sample_rates [k], q, data_out, ARRAY_LEN (data_out),ch);
        read_vorbis_data_or_die (filename, sample_rates [k], data_in, ARRAY_LEN (data_in));

        if (check_output (data_in, ARRAY_LEN (data_in), (.15f - .1f*q)) != 0)
          errors ++ ;
        else {
          puts ("ok");
          remove (filename);
        }
      }
      q+=.1;
    }
  }

  if (errors)
    exit (1);

  return 0;
}

static int
check_output (const float * data_in, unsigned len, float allowable)
{
  float max_abs = 0.0 ;
  unsigned k ;

  for (k = 0 ; k < len ; k++) {
    float temp = fabs (data_in [k]);
    max_abs = MAX (max_abs, temp);
  }

  if (max_abs < 0.95-allowable) {
    printf ("Error : max_abs (%f) too small.\n", max_abs);
    return 1 ;
  } else if (max_abs > .95+allowable) {
    printf ("Error : max_abs (%f) too big.\n", max_abs);
    return 1 ;
  }

  return 0 ;
}

