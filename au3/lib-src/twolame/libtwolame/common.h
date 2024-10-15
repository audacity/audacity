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


#ifndef _COMMON_H
#define _COMMON_H

#ifdef _WIN32
# include "../win32/configwin.h"
#else
# include "config.h"
#endif

#include "twolame.h"



/***************************************************************************************
 Common Definitions
****************************************************************************************/

#ifndef FLOAT
#define			FLOAT					double
#endif

#define			NULL_CHAR				'\0'

#define			MAX_U_32_NUM			0xFFFFFFFF
#ifndef PI
#define			PI						3.14159265358979
#endif
#ifndef E
#define			E						2.71828182845
#endif
#define			PI2						PI/2
#define			PI4						PI/4
#define			PI64					PI/64
#define			LN_TO_LOG10				0.2302585093

#define			BITS_IN_A_BYTE			8
#define			WORD					16
#define			MAX_NAME_SIZE			255
#define			SBLIMIT					32
#define			SSLIMIT					18
#define			FFT_SIZE				1024
#define			HAN_SIZE				512
#define			SCALE_BLOCK				12
#define			SCALE_RANGE				64
#define			SCALE					32768
#define			CRC16_POLYNOMIAL		0x8005
#define			CRC8_POLYNOMIAL			0x1D

#define			MIN(A, B)		((A) < (B) ? (A) : (B))
#define			MAX(A, B)		((A) > (B) ? (A) : (B))


/* This is the smallest MNR a subband can have before it is counted
   as 'noisy' by the logic which chooses the number of JS subbands */

#define NOISY_MIN_MNR	0.0


/***************************************************************************************
  Psychacoustic Model 1/3 Definitions
****************************************************************************************/

#define NOISE			10
#define TONE			20
#define DBMIN			-200.0
#define LAST			-1
#define STOP			-100
#define POWERNORM		90.3090 /* = 20 * log10(32768) to normalize */
/* max output power to 96 dB per spec */


/***************************************************************************************
  Psychoacoustic Model 2/4 Definitions
****************************************************************************************/
#define LXMIN			32.0



/***************************************************************************************
psycho 0 mem struct
****************************************************************************************/

typedef struct psycho_0_mem_struct {
    FLOAT ath_min[SBLIMIT];
} psycho_0_mem;



/***************************************************************************************
psycho 1 mem struct
****************************************************************************************/
#define DBTAB 1000

typedef struct {
    int line;
    FLOAT bark, hear, x;
} g_thres, *g_ptr;

typedef struct {
    FLOAT x;
    int type, next, map;
} mask, *mask_ptr;

typedef struct psycho_1_mem_struct {
    int off[2];
    FLOAT fft_buf[2][1408];
    int *cbound;
    int crit_band;
    int sub_size;
    mask_ptr power;
    g_ptr ltg;
    FLOAT dbtable[DBTAB];
} psycho_1_mem;



/***************************************************************************************
Psycho3 memory structure
****************************************************************************************/
#define HBLKSIZE 513

#define SUBSIZE 136
typedef struct psycho_3_mem_struct {
    int off[2];
    int freq_subset[SUBSIZE];
    FLOAT bark[HBLKSIZE];
    FLOAT ath[HBLKSIZE];
    FLOAT fft_buf[2][1408];
#define CRITBANDMAX 32          /* this is much higher than it needs to be. really only about 24 */
    int cbands;                 /* How many critical bands there really are */
    int cbandindex[CRITBANDMAX];    /* The spectral line index of the start of each critical band */
    FLOAT dbtable[DBTAB];
} psycho_3_mem;



/***************************************************************************************
Psycho2 & 4 memory structure
****************************************************************************************/

#define LOGBLKSIZE		10
#define BLKSIZE			1024
#define HBLKSIZE		513
#define CBANDS			64
#define TRIGTABLESIZE	6284
#define TRIGTABLESCALE	2000.0
typedef int ICB[CBANDS];
typedef int IHBLK[HBLKSIZE];
typedef FLOAT F32[32];
typedef FLOAT F2_32[2][32];
typedef FLOAT FCB[CBANDS];
typedef FLOAT FCBCB[CBANDS][CBANDS];
typedef FLOAT FBLK[BLKSIZE];
typedef FLOAT FHBLK[HBLKSIZE];
typedef FLOAT F2HBLK[2][HBLKSIZE];
typedef FLOAT F22HBLK[2][2][HBLKSIZE];
typedef FLOAT DCB[CBANDS];

typedef struct psycho_4_mem_struct {
    int new;
    int old;
    int oldest;

    int flush;
    int sync_flush;
    int syncsize;

    FLOAT grouped_c[CBANDS];
    FLOAT grouped_e[CBANDS];
    FLOAT nb[CBANDS];
    FLOAT cb[CBANDS];
    FLOAT tb[CBANDS];
    FLOAT ecb[CBANDS];
    FLOAT bc[CBANDS];
    FLOAT cbval[CBANDS];
    FLOAT rnorm[CBANDS];
    FLOAT wsamp_r[BLKSIZE], phi[BLKSIZE], energy[BLKSIZE], window[BLKSIZE];
    FLOAT ath[HBLKSIZE], thr[HBLKSIZE], c[HBLKSIZE];
    FLOAT fthr[HBLKSIZE], absthr[HBLKSIZE]; // psy2 only
    int numlines[CBANDS];
    int partition[HBLKSIZE];
    FLOAT *tmn;
    FCB *s;
    FHBLK *lthr;
    F2HBLK *r, *phi_sav;
    FLOAT snrtmp[2][32];
    FLOAT cos_table[TRIGTABLESIZE];
} psycho_4_mem, psycho_2_mem;


/***************************************************************************************
 Subband utility structures
****************************************************************************************/

typedef struct subband_mem_struct {
    FLOAT x[2][512];
    FLOAT m[16][32];
    int off[2];
    int half[2];
} subband_mem;



/***************************************************************************************
 Header and frame information
****************************************************************************************/

/* Raw Header Information Structure */
typedef struct {
    int version;
    int lay;
    int error_protection;
    int bitrate_index;
    int samplerate_idx;
    int padding;
    int private_bit;
    int mode;
    int mode_ext;
    int copyright;
    int original;
    int emphasis;
} frame_header;



typedef unsigned int subband_t[2][3][SCALE_BLOCK][SBLIMIT];
typedef FLOAT jsb_sample_t[3][SCALE_BLOCK][SBLIMIT];
typedef FLOAT sb_sample_t[2][3][SCALE_BLOCK][SBLIMIT];



/***************************************************************************************
 twolame Global Options structure.
 Defaults shown in []
 ++ means it is an advanced option. Only use it if you know what you're doing.	  
****************************************************************************************/
struct twolame_options_struct {
    // Input PCM audio File Information
    int samplerate_in;          // mpeg1: 32000 [44100] 48000 
    // mpeg2: 16000 22050 24000 
    int samplerate_out;
    int num_channels_in;        // Number of channels on the input stream
    int num_channels_out;       // Number of channels on the output stream

    // Output MP2 File Information
    TWOLAME_MPEG_version version;   // 0 mpeg2 [1] mpeg1 
    int bitrate;                // for mpeg1:32, 48, 56, 64, 80, 96,112,128,160,[192], 224, 256,
    // 320, 384 
    // for mpeg2: 8, 16, 24, 32, 40, 48, 56, 64, 80, [96], 112, 128, 144, 160 
    TWOLAME_MPEG_mode mode;
    TWOLAME_Padding padding;    // [PAD_NO] 
    int do_energy_levels;       // Write energy level information into the end of the frame [FALSE]
    int num_ancillary_bits;     // Number of reserved ancillary bits [0] (Currently only available
    // for non-VBR modes)

    // Psychoacoustic Model options
    int psymodel;               // -1, 0, 1, 2, [3], 4 Psy model number
    FLOAT athlevel;             // Adjust the Absolute Threshold of Hearing curve by [0] dB
    int quickmode;              // Only calculate psy model ever X frames [FALSE] 
    int quickcount;             // Only calculate psy model every [10] frames

    // VBR Options
    int vbr;                    // turn on VBR mode TRUE [FALSE] 
    int vbr_upper_index;        // ++ [0] means no upper bitrate set for VBR mode. valid 1-15
    // depending on mode
    int vbr_max_bitrate;
    FLOAT vbrlevel;             // Set VBR quality. [0.0] (sensible range -10.0 -> 10.0)

    // Miscellaneous Options That Nobody Ever Uses
    TWOLAME_Emphasis emphasis;  // [n]one, 5(50/15 microseconds), c(ccitt j.17) 
    int copyright;              // [FALSE] 
    int original;               // [FALSE] 
    int private_bit;            // [0] Your very own bit in the header.
    int error_protection;       // [FALSE] 

    // Digital Audio Broadcasting Extensions
    unsigned int do_dab;        // Allocate space for the DigitalAudioBroadcasting info [FALSE] 
    unsigned int dab_crc_len;   // Number of CRC bytes for DAB [2], 4 
    unsigned int dab_crc[4];    // DAB CRC bytes are inserted here. User must insert them in frame
    unsigned int dab_xpad_len;  // Number of bytes in the XPAD

    // Processing Options
    int verbosity;              // Verbosity of output 0(never output a thing) [2] 100(output
    // everything)


    // Scaling
    FLOAT scale;
    FLOAT scale_left;
    FLOAT scale_right;



    // Bit allocation stuff
    int lower_index;
    int upper_index;
    int bitrateindextobits[15];
    int vbr_frame_count;        // Used for debugging VBR


    // Used by twolame_encode_frame
    int twolame_init;
    short int buffer[2][TWOLAME_SAMPLES_PER_FRAME]; // Sample buffer
    unsigned int samples_in_buffer; // Number of samples currently in buffer
    unsigned int psycount;
    unsigned int num_crc_bits;  // Number of bits CRC is calculated on

    unsigned int bit_alloc[2][SBLIMIT];
    unsigned int scfsi[2][SBLIMIT];
    unsigned int scalar[2][3][SBLIMIT];
    unsigned int j_scale[3][SBLIMIT];
    FLOAT smrdef[2][32];
    FLOAT smr[2][SBLIMIT];
    FLOAT max_sc[2][SBLIMIT];

    subband_t *subband;
    jsb_sample_t *j_sample;
    sb_sample_t *sb_sample;



    /* Resampling stuff */
    FLOAT resample_ratio;
    void *resample_handle[2];


    // memory for psycho models
    psycho_0_mem *p0mem;
    psycho_1_mem *p1mem;
    psycho_2_mem *p2mem;
    psycho_3_mem *p3mem;
    psycho_4_mem *p4mem;


    // memory for subband
    subband_mem smem;

    // Frame info
    frame_header header;
    int jsbound;                // first band of joint stereo coding
    int sblimit;                // total number of sub bands
    int tablenum;

    int vbrstats[15];
};

#endif                          // _COMMON_H


// vim:ts=4:sw=4:nowrap: 
