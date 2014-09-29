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
#include "audio_wave.h"


/*****************************************************************************
*
*  Routines to determine byte order (of this machine) and swap bytes
*
*****************************************************************************/

enum byte_order { order_unknown, order_bigEndian, order_littleEndian };

static enum byte_order DetermineByteOrder(void)
{
    char s[sizeof(long) + 1];
    union {
        long longval;
        char charval[sizeof(long)];
    } probe;
    probe.longval = 0x41424344L;    /* ABCD in ASCII */
    strncpy(s, probe.charval, sizeof(long));
    s[sizeof(long)] = '\0';
    /* printf("byte order is %s\n", s ); */

    if (strcmp(s, "ABCD") == 0)
        return order_bigEndian;
    else if (strcmp(s, "DCBA") == 0)
        return order_littleEndian;
    else
        return order_unknown;
}

static void SwapBytesInWords(short *loc, int words)
{
    int i;
    short thisval;
    char *dst, *src;
    src = (char *) &thisval;
    for (i = 0; i < words; i++) {
        thisval = *loc;
        dst = (char *) loc++;
        dst[0] = src[1];
        dst[1] = src[0];
    }
}




/*****************************************************************************
*
*  Read in specified number of samples
*
*****************************************************************************/

int wave_get_samples(wave_info_t * wave_info, short int pcm[], int numSamples)
{
    int samples_read;
    FILE *file = wave_info->soundfile;

    samples_read = fread(pcm, sizeof(short int), numSamples, file);

    if (wave_info->byteswap) {
        SwapBytesInWords(pcm, samples_read);
    }

    return (samples_read / wave_info->channels);
}




/*****************************************************************************
*
*  parse the wave header.
*  returns NULL if not open failed
*  returns a wave_info_t * if wave header successfully parsed.
*  needs to fill in : samplerate + channels
*  
*  POST: wave_info->soundfile is set to be at the start of the
*        PCM audio data 
*
*****************************************************************************/

wave_info_t *wave_init(char *inPath)
{
    unsigned char wave_header_buffer[40];   // HH fixed
    int wave_header_read = 0;
    int wave_header_stereo = -1;
    int wave_header_16bit = -1;
    unsigned long samplerate;
    enum byte_order NativeByteOrder = order_unknown;


    wave_info_t *wave_info = NULL;
    FILE *file;

    if ((file = fopen(inPath, "rb")) == NULL) {
        printf("WAV: cannot open input file: %s\n", inPath);
        return (NULL);
    }


    /************** WAVE ************************/
    /* Nick Burch <The_Leveller@newmail.net> */
    /********************************************/
    /* Wave File Headers: (Dec) */
    /* 8-11 = "WAVE" */
    /* 22 = Stereo / Mono */
    /* 01 = mono, 02 = stereo */
    /* 24 = Sampling Frequency */
    /* 32 = Data Rate */
    /* 01 = x1 (8bit Mono) */
    /* 02 = x2 (8bit Stereo or */
    /* 16bit Mono) */
    /* 04 = x4 (16bit Stereo) */
    /********************************************/

    fseek(file, 0, SEEK_SET);
    fread(wave_header_buffer, 1, 40, file);

    if (wave_header_buffer[8] == 'W' && wave_header_buffer[9] == 'A'
        && wave_header_buffer[10] == 'V' && wave_header_buffer[11] == 'E') {
        printf("Parsing Wave File Header\n");
        if (NativeByteOrder == order_unknown) {
            NativeByteOrder = DetermineByteOrder();
            if (NativeByteOrder == order_unknown) {
                printf("byte order not determined\n");
                fclose(file);
                return (NULL);
            }
        }

        if (NativeByteOrder == order_littleEndian) {
            samplerate = *(unsigned long *) (&wave_header_buffer[24]);
        } else {
            samplerate = wave_header_buffer[27] +
                (wave_header_buffer[26] << 8) +
                (wave_header_buffer[25] << 16) + (wave_header_buffer[24] << 24);
        }

        /* Wave File */
        wave_header_read = 1;
        switch (samplerate) {
        case 44100:
        case 48000:
        case 32000:
        case 24000:
        case 22050:
        case 16000:
            printf(">>> %ld Hz sampling freq selected\n", samplerate);
            break;
        default:
            /* Unknown Unsupported Frequency */
            printf(">>> Unknown samp freq %ld Hz in Wave Header\n", samplerate);
            printf(">>> Default 44.1 kHz samp freq selected\n");
            samplerate = 44100;
            break;
        }

        if ((long) wave_header_buffer[22] == 1) {
            printf(">>> Input Wave File is Mono\n");
            wave_header_stereo = 0;
        }
        if ((long) wave_header_buffer[22] == 2) {
            printf(">>> Input Wave File is Stereo\n");
            wave_header_stereo = 1;
        }
        if ((long) wave_header_buffer[32] == 1) {
            printf(">>> Input Wave File is 8 Bit\n");
            wave_header_16bit = 0;
            printf("Input File must be 16 Bit! Please Re-sample");
            fclose(file);
            return (NULL);
        }
        if ((long) wave_header_buffer[32] == 2) {
            if (wave_header_stereo == 1) {
                printf(">>> Input Wave File is 8 Bit\n");
                wave_header_16bit = 0;
                printf("Input File must be 16 Bit! Please Re-sample");
                fclose(file);
                return (NULL);
            } else {
                /* printf(">>> Input Wave File is 16 Bit\n" ); */
                wave_header_16bit = 1;
            }
        }
        if ((long) wave_header_buffer[32] == 4) {
            /* printf(">>> Input Wave File is 16 Bit\n" ); */
            wave_header_16bit = 1;
        }

        /* should probably use the wave header to determine size here FIXME MFC Feb 2003 */
        if (fseek(file, 44, SEEK_SET) != 0) {
            /* there's a way of calculating the size of the wave header. i'll just jump 44 to start 
               with */
            printf("Could not seek to PCM sound data in \"%s\".\n", inPath);
            fclose(file);
            return (NULL);
        }
        // Successfully processed the wave header
        wave_info = (wave_info_t *) calloc(1, sizeof(wave_info_t));
        wave_info->soundfile = file;
        if (wave_header_stereo == 1)
            wave_info->channels = 2;
        else
            wave_info->channels = 1;
        wave_info->samplerate = samplerate;

        // UNKNOWN. But we really should be able to work 
        // it out. FIX THIS. MFC May03.
        wave_info->num_samples = -1;

        // Enable byte swap for big endian machines
        if (NativeByteOrder == order_bigEndian)
            wave_info->byteswap = 1;
        else
            wave_info->byteswap = 0;

        return (wave_info);
    }
    // not a wave file
    fclose(file);
    return (NULL);
}

/* vim:ts=4:sw=4:nowrap: */
