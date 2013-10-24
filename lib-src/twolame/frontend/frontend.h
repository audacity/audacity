/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2004-2007 The TwoLAME Project
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
 *  $Id: frontend.h 156 2007-03-20 23:57:35Z nhumfrey $
 *
 */

#include <sndfile.h>


/*
  Constants
*/
#define MP2_BUF_SIZE		(16384)
#define AUDIO_BUF_SIZE		(9210)
#define MAX_NAME_SIZE		(1024)
#define OUTPUT_SUFFIX		".mp2"
#define DEFAULT_CHANNELS	(2)
#define DEFAULT_SAMPLERATE	(44100)


/*
  Result codes
*/
#define ERR_NO_ERROR		(0) // No Error (encoded ok)
#define ERR_NO_ENCODE		(1) // No Error (no encoding performed)
#define ERR_OPENING_INPUT	(2) // Error opening input file
#define ERR_OPENING_OUTPUT	(4) // Error opening output file
#define ERR_MEM_ALLOC		(6) // Error allocating memory
#define ERR_INVALID_PARAM	(8) // Error in chosen encoding parameters
#define ERR_READING_INPUT	(10)    // Error reading input
#define ERR_ENCODING		(12)    // Error occured during encoding
#define ERR_WRITING_OUTPUT	(14)    // Error occured writing to output file




typedef struct audioin_s {

    // Display information about input stream
    void (*print_info) (struct audioin_s *);

    // Read in some audio
    int (*read) (struct audioin_s *, short *buffer, int samples);

    // Return error string (if any)
    const char *(*error_str) (struct audioin_s *);

    // Close the inout stream
    int (*close) (struct audioin_s *);


    // Pointer to file / input stream
    void *file;

    // Pointer to linsndfile info structure
    SF_INFO *sfinfo;

    // Size of the samples (in bits)
    int samplesize;

} audioin_t;



/* Initialisers */
audioin_t *open_audioin_sndfile(char *filename, SF_INFO * sfinfo);
audioin_t *open_audioin_raw(char *filename, SF_INFO * sfinfo, int samplesize);
