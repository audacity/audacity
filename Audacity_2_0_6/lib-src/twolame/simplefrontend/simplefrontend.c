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
#include <stdlib.h>
#include <string.h>
#include <twolame.h>

#include "audio_wave.h"


#define MP2BUFSIZE		(16384)
#define AUDIOBUFSIZE	(9216)



static void usage(void)
{
    printf("stwolame <input wavefile> <output mp2 file>\n");
    exit(1);
}


int main(int argc, char **argv)
{
    twolame_options *encodeOptions;
    char *inputfilename = argv[1];
    char *outputfilename = argv[2];
    FILE *outfile;
    short int *pcmaudio;
    unsigned char *mp2buffer;
    int num_samples = 0;
    int mp2fill_size = 0;
    int frames = 0;
    wave_info_t *wave_info = NULL;

    if (argc != 3)
        usage();


    /* Allocate some space for the PCM audio data */
    if ((pcmaudio = (short *) calloc(AUDIOBUFSIZE, sizeof(short))) == NULL) {
        fprintf(stderr, "pcmaudio alloc failed\n");
        exit(99);
    }

    /* Allocate some space for the encoded MP2 audio data */
    if ((mp2buffer = (unsigned char *) calloc(MP2BUFSIZE, sizeof(unsigned char))) == NULL) {
        fprintf(stderr, "mp2buffer alloc failed\n");
        exit(99);
    }




    /* grab a set of default encode options */
    encodeOptions = twolame_init();
    printf("Using libtwolame version %s.\n", get_twolame_version());


    /* Open the wave file */
    if ((wave_info = wave_init(inputfilename)) == NULL) {
        fprintf(stderr, "Not a recognised WAV file.\n");
        exit(99);
    }
    // Use sound file to over-ride preferences for
    // mono/stereo and sampling-frequency
    twolame_set_num_channels(encodeOptions, wave_info->channels);
    if (wave_info->channels == 1) {
        twolame_set_mode(encodeOptions, TWOLAME_MONO);
    } else {
        twolame_set_mode(encodeOptions, TWOLAME_STEREO);
    }


    /* Set the input and output sample rate to the same */
    twolame_set_in_samplerate(encodeOptions, wave_info->samplerate);
    twolame_set_out_samplerate(encodeOptions, wave_info->samplerate);

    /* Set the bitrate to 192 kbps */
    twolame_set_bitrate(encodeOptions, 192);

    /* initialise twolame with this set of options */
    if (twolame_init_params(encodeOptions) != 0) {
        fprintf(stderr, "Error: configuring libtwolame encoder failed.\n");
        exit(99);
    }

    /* Open the output file for the encoded MP2 data */
    if ((outfile = fopen(outputfilename, "wb")) == 0) {
        fprintf(stderr, "error opening output file %s\n", outputfilename);
        exit(99);
    }
    // Read num_samples of audio data *per channel* from the input file
    while ((num_samples = wave_get_samples(wave_info, pcmaudio, AUDIOBUFSIZE)) != 0) {

        // Encode the audio!
        mp2fill_size =
            twolame_encode_buffer_interleaved(encodeOptions, pcmaudio, num_samples, mp2buffer,
                                              MP2BUFSIZE);

        // Write the MPEG bitstream to the file
        fwrite(mp2buffer, sizeof(unsigned char), mp2fill_size, outfile);

        // Display the number of MPEG audio frames we have encoded
        frames += (num_samples / TWOLAME_SAMPLES_PER_FRAME);
        printf("[%04i]\r", frames);
        fflush(stdout);
    }

    /* flush any remaining audio. (don't send any new audio data) There should only ever be a max
       of 1 frame on a flush. There may be zero frames if the audio data was an exact multiple of
       1152 */
    mp2fill_size = twolame_encode_flush(encodeOptions, mp2buffer, MP2BUFSIZE);
    fwrite(mp2buffer, sizeof(unsigned char), mp2fill_size, outfile);


    twolame_close(&encodeOptions);
    free(pcmaudio);

    printf("\nFinished nicely.\n");


    return (0);
}

/* vim:ts=4:sw=4:nowrap: */
