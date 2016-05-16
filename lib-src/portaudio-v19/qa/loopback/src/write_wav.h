/*
 * PortAudio Portable Real-Time Audio Library
 * Latest Version at: http://www.portaudio.com
 *
 * Copyright (c) 1999-2010 Phil Burk and Ross Bencina
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/*
 * The text above constitutes the entire PortAudio license; however, 
 * the PortAudio community also makes the following non-binding requests:
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version. It is also 
 * requested that these non-binding requests be included along with the 
 * license above.
 */
#ifndef _WAV_WRITER_H
#define _WAV_WRITER_H

/*
 * WAV file writer.
 *
 * Author: Phil Burk
 */

#ifdef __cplusplus
extern "C" {
#endif

/* Define WAV Chunk and FORM types as 4 byte integers. */
#define RIFF_ID   (('R'<<24) | ('I'<<16) | ('F'<<8) | 'F')
#define WAVE_ID   (('W'<<24) | ('A'<<16) | ('V'<<8) | 'E')
#define FMT_ID    (('f'<<24) | ('m'<<16) | ('t'<<8) | ' ')
#define DATA_ID   (('d'<<24) | ('a'<<16) | ('t'<<8) | 'a')
#define FACT_ID   (('f'<<24) | ('a'<<16) | ('c'<<8) | 't')

/* Errors returned by Audio_ParseSampleImage_WAV */
#define WAV_ERR_CHUNK_SIZE     (-1)   /* Chunk size is illegal or past file size. */
#define WAV_ERR_FILE_TYPE      (-2)   /* Not a WAV file. */
#define WAV_ERR_ILLEGAL_VALUE  (-3)   /* Illegal or unsupported value. Eg. 927 bits/sample */
#define WAV_ERR_FORMAT_TYPE    (-4)   /* Unsupported format, eg. compressed. */
#define WAV_ERR_TRUNCATED      (-5)   /* End of file missing. */

/* WAV PCM data format ID */
#define WAVE_FORMAT_PCM        (1)
#define WAVE_FORMAT_IMA_ADPCM  (0x0011)

	
typedef struct WAV_Writer_s
{
    FILE *fid;
    /* Offset in file for data size. */
    int   dataSizeOffset;
    int   dataSize;
} WAV_Writer;

/*********************************************************************************
 * Open named file and write WAV header to the file.
 * The header includes the DATA chunk type and size.
 * Returns number of bytes written to file or negative error code.
 */
long Audio_WAV_OpenWriter( WAV_Writer *writer, const char *fileName, int frameRate, int samplesPerFrame );

/*********************************************************************************
 * Write to the data chunk portion of a WAV file.
 * Returns bytes written or negative error code.
 */
long Audio_WAV_WriteShorts( WAV_Writer *writer,
		short *samples,
		int numSamples
		);

/*********************************************************************************
 * Close WAV file.
 * Update chunk sizes so it can be read by audio applications.
 */
long Audio_WAV_CloseWriter( WAV_Writer *writer );

#ifdef __cplusplus
};
#endif

#endif /* _WAV_WRITER_H */
