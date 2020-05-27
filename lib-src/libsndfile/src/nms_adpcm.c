/*
** Copyright (C) 1999-2014 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2017 Arthur Taylor <art@ified.ca>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

/*
** This is a Natural MicroSystems ADPCM encoder/decoder. It converts 14 bit linear
** PCM to and from either a 2, 3, or 4 bit ADPCM. NMS-ADPCM does not have appeared
** to have ever been publicly documented, and appears to have debuted in the early
** 90s in the Natural Access suite of PC-based telephony products. Raw NMS ADPCM
** files usually have a .vce extension, although this does not encode what bitrate
** is used.
**
** NMS-ADPCM is an 'optimised variation' of the ITU G.726 ADPCM scheme. The dominant
** variation is that it removes the tone (modem) operation mode, and it's associated
** voice/modem transition detection. This simplifies the computation of the step
** size multiplier, as all operations on it remain in a log domain.
*/

#include	"sfconfig.h"

#include	<math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"


#define NMS_SAMPLES_PER_BLOCK 160
#define NMS_BLOCK_SHORTS_32 41
#define NMS_BLOCK_SHORTS_24 31
#define NMS_BLOCK_SHORTS_16 21

/* Variable names from ITU G.726 spec */
struct nms_adpcm_state
{	/* Log of the step size multiplier. Operated on by codewords. */
	int yl ;

	/* Quantizer step size multiplier. Generated from yl. */
	int y ;

	/* Coefficents of the pole predictor */
	int a [2] ;

	/* Coefficents of the zero predictor  */
	int b [6] ;

	/* Previous quantized deltas (multiplied by 2^14) */
	int d_q [7] ;

	/* d_q [x] + s_ez [x], used by the pole-predictor for signs only. */
	int p [3] ;

	/* Previous reconstructed signal values. */
	int s_r [2] ;

	/* Zero predictor components of the signal estimate. */
	int s_ez ;

	/* Signal estimate, (including s_ez). */
	int s_e ;

	/* The most recent codeword (enc:generated, dec:inputted) */
	int Ik ;

	int parity ;

	/*
	** Offset into code tables for the bitrate.
	** 2-bit words: +0
	** 3-bit words: +8
	** 4-bit words: +16
	*/
	int t_off ;
} ;

enum nms_enc_type
{	NMS16,
	NMS24,
	NMS32
} ;

typedef struct
{	struct nms_adpcm_state state ;

	/* The encoding type */
	enum nms_enc_type type ;

	int shortsperblock ;
	int	blocks_total ;
	int block_curr, sample_curr ;

	unsigned short block [NMS_BLOCK_SHORTS_32] ;
	short samples [NMS_SAMPLES_PER_BLOCK] ;
} NMS_ADPCM_PRIVATE ;

/* Pre-computed exponential interval used in the antilog approximation. */
static unsigned int table_expn [] =
{	0x4000, 0x4167, 0x42d5, 0x444c,	0x45cb, 0x4752, 0x48e2, 0x4a7a,
	0x4c1b, 0x4dc7, 0x4f7a, 0x5138,	0x52ff, 0x54d1, 0x56ac, 0x5892,
	0x5a82, 0x5c7e, 0x5e84, 0x6096,	0x62b4, 0x64dd, 0x6712, 0x6954,
	0x6ba2, 0x6dfe, 0x7066, 0x72dc,	0x7560, 0x77f2, 0x7a93, 0x7d42,
} ;

/* Table mapping codewords to scale factor deltas. */
static int table_scale_factor_step [] =
{	0x0,	0x0,	0x0,	0x0,	0x4b0,	0x0,	0x0,	0x0,	/* 2-bit */
	-0x3c,	0x0,	0x90,	0x0,	0x2ee,	0x0,	0x898,	0x0,	/* 3-bit */
	-0x30,	0x12,	0x6b,	0xc8,	0x188,	0x2e0,	0x551,	0x1150,	/* 4-bit */
} ;

/* Table mapping codewords to quantized delta interval steps. */
static unsigned int table_step [] =
{	0x73F,	0,		0,		0,		0x1829,	0,		0,		0,		/* 2-bit */
	0x3EB,	0,		0xC18,	0,		0x1581,	0,		0x226E,	0,		/* 3-bit */
	0x20C,	0x635,	0xA83,	0xF12,	0x1418,	0x19E3,	0x211A,	0x2BBA,	/* 4-bit */
} ;

/* Binary search lookup table for quantizing using table_step. */
static int table_step_search [] =
{	0,		0x1F6D,	0,		-0x1F6D,	0,		0,			0,			0, /* 2-bit */
	0x1008,	0x1192,	0,		-0x219A,	0x1656,	-0x1656,	0,			0, /* 3-bit */
	0x872,	0x1277,	-0x8E6,	-0x232B,	0xD06,	-0x17D7,	-0x11D3,	0, /* 4-bit */
} ;


/*============================================================================================
** Static functions.
*/

static void nms_adpcm_update (struct nms_adpcm_state *s) ;
static void nms_adpcm_codec_init (struct nms_adpcm_state *s, enum nms_enc_type type) ;

static int16_t nms_adpcm_reconstruct_sample (struct nms_adpcm_state *s, uint8_t I) ;
static uint8_t nms_adpcm_encode_sample (struct nms_adpcm_state *s, int16_t sl) ;
static int16_t nms_adpcm_decode_sample (struct nms_adpcm_state *s, uint8_t code) ;

static void nms_adpcm_block_pack_16 (const int16_t codewords [], uint16_t block [], int16_t rms) ;
static void nms_adpcm_block_pack_24 (const int16_t codewords [], uint16_t block [], int16_t rms) ;
static void nms_adpcm_block_pack_32 (const int16_t codewords [], uint16_t block [], int16_t rms) ;

static void nms_adpcm_block_unpack_16 (const uint16_t block [], int16_t codewords [], int16_t *rms) ;
static void nms_adpcm_block_unpack_24 (const uint16_t block [], int16_t codewords [], int16_t *rms) ;
static void nms_adpcm_block_unpack_32 (const uint16_t block [], int16_t codewords [], int16_t *rms) ;

static int nms_adpcm_decode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, uint16_t block [], int16_t samples []) ;
static int nms_adpcm_encode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, int16_t samples [], uint16_t block []) ;

static sf_count_t nms_adpcm_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t nms_adpcm_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t nms_adpcm_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

static int nms_adpcm_close (SF_PRIVATE *psf) ;
static sf_count_t nms_adpcm_seek (SF_PRIVATE *psf, int mode, sf_count_t offset) ;

/*
** An exponential function (antilog) approximation.
**
** Maps [1,20480] to [1,1024] in an exponential relationship. This is
** approximately ret = b^exp where b = e^(ln(1024)/ln(20480)) ~= 1.0003385
*/
static inline int
nms_adpcm_antilog (int exp)
{	int ret ;

	ret = 0x1000 ;
	ret += (((exp & 0x3f) * 0x166b) >> 12) ;
	ret *= table_expn [(exp & 0x7c0) >> 6] ;
	ret >>= (26 - (exp >> 11)) ;

	return ret ;
} /* nms_adpcm_antilog */

static void
nms_adpcm_update (struct nms_adpcm_state *s)
{	/* Variable names from ITU G.726 spec */
	int a1ul ;
	int fa1 ;
	int i ;

	/* Decay and Modify the scale factor in the log domain based on the codeword. */
	s->yl = ((s->yl *0xf8) >> 8) + table_scale_factor_step [s->t_off + (s->Ik & 7)] ;
	if (s->yl < 2171)
		s->yl = 2171 ;
	else if (s->yl > 20480)
		s->yl = 20480 ;
	s->y = nms_adpcm_antilog (s->yl) ;

	/* Update the zero predictor coefficents. */
	for (i = 0 ; i < 6 ; i++)
	{	s->b [i] = (s->b [i] * 0xff) >> 8 ;
		if ((s->d_q [0] ^ s->d_q [i + 1]) >= 0)
			s->b [i] += 128 ;
		else
			s->b [i] -= 128 ;
		}

	/* Update the pole predictor coefficents. */
	fa1 = s->a [0] >> 5 ;
	if (fa1 < -256)
		fa1 = -256 ;
	else if (fa1 > 256)
		fa1 = 256 ;

	s->a [0] = (0xff * s->a [0]) >> 8 ;
	if (s->p [0] != 0 && s->p [1] != 0 && ((s->p [0] ^ s->p [1]) < 0))
		s->a [0] -= 192 ;
	else
	{	s->a [0] += 192 ;
		fa1 = -fa1 ;
		}

	s->a [1] = fa1 + ((0xfe * s->a [1]) >> 8) ;
	if (s->p [0] != 0 && s->p [2] != 0 && ((s->p [0] ^ s->p [2]) < 0))
		s->a [1] -= 128 ;
	else
		s->a [1] += 128 ;

	/* Stability constraints. */
	if (s->a [1] < -12288)
		s->a [1] = -12288 ;
	else if (s->a [1] > 12288)
		s->a [1] = 12288 ;
	a1ul = 15360 - s->a [1] ;
	if (s->a [0] >= a1ul)
		s->a [0] = a1ul ;
	else
	{	a1ul = -a1ul ;
		if (s->a [0] < a1ul)
			s->a [0] = a1ul ;
		} ;

	/* Compute the zero predictor estimate. Rotate past deltas too. */
	s->s_ez = 0 ;
	for (i = 5 ; i >= 0 ; i--)
	{	s->s_ez += s->d_q [i] * s->b [i] ;
		s->d_q [i + 1] = s->d_q [i] ;
		} ;

	/* Compute the signal estimate. */
	s->s_e = s->a [0] * s->s_r [0] + s->a [1] * s->s_r [1] + s->s_ez ;

	/* Return to scale */
	s->s_ez >>= 14 ;
	s->s_e >>= 14 ;

	/* Rotate members to prepare for next iteration. */
	s->s_r [1] = s->s_r [0] ;
	s->p [2] = s->p [1] ;
	s->p [1] = s->p [0] ;
} /* nms_adpcm_update */


static int16_t
nms_adpcm_reconstruct_sample (struct nms_adpcm_state *s, uint8_t I)
{	/* Variable names from ITU G.726 spec */
	int dqx ;

	/*
	** The ordering of the 12-bit right-shift is a precision loss. It agrees
	** with the output of a 16-bit NMSVCE.DLL, but disagrees with the output
	** of a CG6565 board.
	*/

	/* Look up the delta, scale and sign it. */
	dqx = table_step [s->t_off + (I & 7)] * s->y ;
	if (I & 8)
		dqx = -dqx ;

	/* Take from delta scale to actual scale. */
	dqx >>= 12 ;

	/* Set variables used as input for the next predictor update. */
	s->d_q [0] = dqx ;
	s->s_r [0] = s->s_e + dqx ;
	s->Ik = I & 0xf ;
	s->p [0] = s->s_ez + dqx ;

	return s->s_r [0] ;
} /* nms_adpcm_reconstruct_sample */

static void
nms_adpcm_codec_init (struct nms_adpcm_state *s, enum nms_enc_type type)
{	memset (s, 0, sizeof (struct nms_adpcm_state)) ;
	s->t_off = (type == NMS32) ? 16 : (type == NMS24) ? 8 : 0 ;
} /* nms_adpcm_codec_init */

/*
** nms_adpcm_encode_sample()
**
** Encode a linear 16-bit pcm sample into a 2,3, or 4 bit NMS-ADPCM codeword
** using and updating the predictor state.
*/
static uint8_t
nms_adpcm_encode_sample (struct nms_adpcm_state *s, int16_t sl)
{	/* Variable names from ITU G.726 spec */
	int d ;
	uint8_t I ;

	/* Down scale the sample from 16 => ~14 bits. */
	sl = (sl * 0x1fdf) / 0x7fff ;

	/* Compute estimate, and delta from actual value */
	nms_adpcm_update (s) ;
	d = sl - s->s_e ;

	/*
	** Vary the input signal. Not sure why. It agrees with NMSVCE.DLL and
	** a CG6565 board.
	*/
	if (s->parity ^= 1)
		d -= 2 ;

	/* Encode the delta signed-ness (Codeword bit 4) */
	if (d < 0)
	{	d = -d ;
		I = 8 ;
		}
	else
		I = 0 ;

	/* Increase magnitude to be in the range of the delta steps */
	d <<= 13 ;

	/* Quantize the delta using a binary search. */
	d += table_step_search [s->t_off + 3] * s->y ;
	/* Codeword bit 3 */
	if (d >= 0)
	{	d += table_step_search [s->t_off + 5] * s->y ;
		/* Codeword bit 2 */
		if (d >= 0)
		{	d += table_step_search [s->t_off + 6] * s->y ;
			/* Codeword bit 1 */
			if (d >= 0)
				I |= 7 ;
			else
				I |= 6 ;
			}
		else
		{	d += table_step_search [s->t_off + 4] * s->y ;
			/* Codeword bit 1 */
			if (d >= 0)
				I |= 5 ;
			else
				I |= 4 ;
			} ;
		}
	else {
		d += table_step_search [s->t_off + 1] * s->y ;
		/* Codeword bit 2 */
		if (d >= 0)
		{	d += table_step_search [s->t_off + 2] * s->y ;
			/* Codeword bit 1 */
			if (d >= 0)
				I |= 3 ;
			else
				I |= 2 ;
			}
		else {
			d += table_step_search [s->t_off + 0] * s->y ;
			/* Codeword bit 1 */
			if (d >= 0)
				I |= 1 ;
			else
				I |= 0 ;
			} ;
		} ;
	/* What's left in d is actually our quantizer noise. */

	/* Reduce the codeword size for the bitrate accordingly. */
	if (s->t_off == 8)
		I &= 0xe ;
	else if (s->t_off == 0)
		I &= 0xc ;

	/* Call reconstruct for side effects preparing for the next update. */
	nms_adpcm_reconstruct_sample (s, I) ;

	return I ;
} /* nms_adpcm_encode_sample */

/*
** nms_adpcm_decode_sample()
**
** Given a 2,3 or 4-bit NMS-ADPCM codeword, decode the next 16-bit linear PCM
** sample using and updating the predictor state.
*/
static int16_t
nms_adpcm_decode_sample (struct nms_adpcm_state *s, uint8_t I)
{	int sl ;

	nms_adpcm_update (s) ;
	sl = nms_adpcm_reconstruct_sample (s, I) ;

	/* Clamp to [-0x1fdf, 0x1fdf] (just under 14 bits resolution) */
	if (sl < -0x1fdf)
		sl = -0x1fdf ;
	else if (sl > 0x1fdf)
		sl = 0x1fdf ;

	/* Expand from 14 to 16 bits */
	sl = (sl * 0x7fff) / 0x1fdf ;

	return (int16_t) sl ;
} /* nms_adpcm_decode_sample */

/**
** NMS ADPCM Codeword packing scheme.
**
** The serialized form of NMS-ADPCM operates on blocks of 160 mono samples
** (20ms at 8000Hz.) Blocks are 42, 62 and 82 bytes in size for the 2, 3, and
** 4 bit codeword sizes respectively. The data is treated as an array of
** little-endian 2-byte shorts, and the data is packed into the first 20, 30
** or 40 shorts. The last short represents the block's root-mean-square
** average. This is apparently an optimization so that energy/silence
** detection processes can avoid decoding a block.
**
** All codewords are nibbles, with the least significant bits dropped as
** required for the 3 and 2 bit codeword sizes.
**
** Nibbles are packed into shorts in order of most significant to least. The
** 4-bit scheme is trivial. The three bit scheme reconstructs a fourth sample
** from the leftover bits of the proceeding three samples. The 2-bit scheme
** uses a two-pass, left two bit shift.
*/

/*
** Reads 21 shorts from block, unpacks 160 codewords of 2-bits each, writing
** each to its sequential array index of codewords. If rms is non-null, the
** read block rms is copied to its location.
*/
static void
nms_adpcm_block_unpack_16 (const uint16_t block [], int16_t codewords [], int16_t *rms)
{	int k ;
	uint16_t w = 0 ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{	/*
		** k % 8 == [0-3]: Top 2-bits of a nibble
		** k % 8 == [4-7]: Bottom 2-bits of a nibble
		*/
		if ((k & 4) == 0)
			w = *(block++) ;
		else
			w <<= 2 ;
		codewords [k++] = (w >> 12) & 0xc ;
		codewords [k++] = (w >> 8) & 0xc ;
		codewords [k++] = (w >> 4) & 0xc ;
		codewords [k++] = w & 0xc ;
		} ;

	/*
	** Every block ends with a short representing a RMS-approximation for the
	** block.
	**/
	if (rms)
		*rms = *block ;
} /* nms_adpcm_unpack_16 */

/*
** Reads 31 shorts from block, unpacks 160 codewords of 3-bits each, writing
** each to its sequential array index of codewords. If rms is non-null, the
** read block rms is copied to its location.
*/
static void
nms_adpcm_block_unpack_24 (const uint16_t block [], int16_t codewords [], int16_t *rms)
{	int k ;
	uint16_t w = 0, residual = 0 ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{		/*
		** k % 16 == [0, 11]: Unpack new nibble, build residual
		** k % 16 == [12, 15]: Unpack residual
		*/
		if ((k & 12) != 12)
		{	w = *(block++) ;
			residual = (residual << 1) | (w & 0x1111) ;
			}
		else
		{	w = residual << 1 ;
			residual = 0 ;
			} ;
		codewords [k++] = (w >> 12) & 0xe ;
		codewords [k++] = (w >> 8) & 0xe ;
		codewords [k++] = (w >> 4) & 0xe ;
		codewords [k++] = w & 0xe ;
		} ;

	/*
	** Every block ends with a short representing a RMS-approximation for the
	** block.
	**/
	if (rms)
		*rms = *block ;
} /* nms_adpcm_unpack_24 */

/*
** Reads 41 shorts from block, unpacks 160 codewords of 4-bits each, writing
** each to its sequential array index of codewords. If rms is non-null, the
** read block rms is copied to its location.
*/
static void
nms_adpcm_block_unpack_32 (const uint16_t block [], int16_t codewords [], int16_t *rms)
{	int k ;
	uint16_t w = 0 ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{	w = *(block++) ;
		codewords [k++] = (w >> 12) & 0xf ;
		codewords [k++] = (w >> 8) & 0xf ;
		codewords [k++] = (w >> 4) & 0xf ;
		codewords [k++] = w & 0xf ;
		} ;
	/*
	** Every block ends with a short representing a RMS-approximation for the
	** block.
	**/
	if (rms)
		*rms = *block ;
} /* nms_adpcm_unpack_32 */

/*
** Reads 160 indicies of codewords for one 2-bit codeword each, packing them
** into 20 shorts of block, and writes the short rms for a total of 42 bytes.
*/
static void
nms_adpcm_block_pack_16 (const int16_t codewords [], uint16_t block [], int16_t rms)
{	int k ;
	uint16_t w ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{	w = codewords [k++] << 12 ;
		w |= codewords [k++] << 8 ;
		w |= codewords [k++] << 4 ;
		w |= codewords [k++] ;
		w |= codewords [k++] << 10 ;
		w |= codewords [k++] << 6 ;
		w |= codewords [k++] << 2 ;
		w |= codewords [k++] >> 2 ;

		*(block++) = w ;
		} ;

	/* Every block ends with a short representing the blocks RMS */
	*block = rms ;
} /* nms_adpcm_pack_16 */

/*
** Reads 160 indicies of codewords for one 3-bit codeword each, packing them
** into 30 shorts of block, and writes the short rms for a total of 62 bytes.
*/
static void
nms_adpcm_block_pack_24 (const int16_t codewords [], uint16_t block [], int16_t rms)
{	int k ;
	uint16_t w [3] ;
	uint16_t residual ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{	w [0] = codewords [k++] << 12 ;
		w [0] |= codewords [k++] << 8 ;
		w [0] |= codewords [k++] << 4 ;
		w [0] |= codewords [k++] ;

		w [1] = codewords [k++] << 12 ;
		w [1] |= codewords [k++] << 8 ;
		w [1] |= codewords [k++] << 4 ;
		w [1] |= codewords [k++] ;

		w [2] = codewords [k++] << 12 ;
		w [2] |= codewords [k++] << 8 ;
		w [2] |= codewords [k++] << 4 ;
		w [2] |= codewords [k++] ;

		residual = codewords [k++] << 12 ;
		residual |= codewords [k++] << 8 ;
		residual |= codewords [k++] << 4 ;
		residual |= codewords [k++] ;

		residual >>= 1 ;
		w [2] |= (residual & 0x1111) ;
		residual >>= 1 ;
		w [1] |= (residual & 0x1111) ;
		residual >>= 1 ;
		w [0] |= (residual & 0x1111) ;

		*(block++) = w [0] ;
		*(block++) = w [1] ;
		*(block++) = w [2] ;
		} ;

	/* Every block ends with a short representing the blocks RMS */
	*block = rms ;
} /* nms_adpcm_pack_24 */

/*
** Reads 160 indicies of codewords for one 4-bit codeword each, packing them
** into 40 shorts of block, and writes the short rms for a total of 82 bytes.
*/
static void
nms_adpcm_block_pack_32 (const int16_t codewords [], uint16_t block [], int16_t rms)
{	int k ;
	uint16_t w ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; )
	{	w = codewords [k++] << 12 ;
		w |= codewords [k++] << 8 ;
		w |= codewords [k++] << 4 ;
		w |= codewords [k++] ;

		*(block++) = w ;
		} ;

	/* Every block ends with a short representing the blocks RMS */
	*block = rms ;
} /*nms_adpcm_block_pack_32 */

static int
nms_adpcm_decode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, uint16_t block [], int16_t samples [])
{	int k ;

	switch (pnms->type)
	{	case NMS16 :
			nms_adpcm_block_unpack_16 (block, samples, NULL) ;
			break ;
		case NMS24 :
			nms_adpcm_block_unpack_24 (block, samples, NULL) ;
			break ;
		case NMS32 :
			nms_adpcm_block_unpack_32 (block, samples, NULL) ;
			break ;

		default :
			psf_log_printf (psf, "*** Error : Unhandled NMS ADPCM type %d.\n", pnms->type) ;
			return 0 ;
		} ;

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; k++)
		samples [k] = nms_adpcm_decode_sample (&pnms->state, samples [k]) ;

	return NMS_SAMPLES_PER_BLOCK ;
} /* nms_adpcm_decode_block */

static int
nms_adpcm_encode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, int16_t samples [], uint16_t block [])
{	int k ;
	unsigned int rms = 0 ;

	/*
	** The rms we write is a complete lie. Considering that the various
	** other implementations I've tested don't completely agree, that this data
	** is usually ignored, and except for some weird offloading of "energy
	** detection", so long as we don't write zeros for non-zero data, I don't
	** think it really matters.
	*/

	for (k = 0 ; k < NMS_SAMPLES_PER_BLOCK ; k++)
	{	rms += (samples [k] * samples [k]) >> 2 ;
		samples [k] = nms_adpcm_encode_sample (&pnms->state, samples [k]) ;
		} ;

	rms <<= 12 ;
	switch (pnms->type)
	{	case NMS16 :
			nms_adpcm_block_pack_16 (samples, block, rms) ;
			break ;
		case NMS24 :
			nms_adpcm_block_pack_24 (samples, block, rms) ;
			break ;
		case NMS32 :
			nms_adpcm_block_pack_32 (samples, block, rms) ;
			break ;

		default :
			psf_log_printf (psf, "*** Error : Unhandled NMS ADPCM type %d.\n", pnms->type) ;
			return 0 ;
		} ;

	return NMS_SAMPLES_PER_BLOCK ;
} /* nms_adpcm_encode_block */

static int
psf_nms_adpcm_decode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms)
{	int k ;

	if ((k = psf_fread (pnms->block, sizeof (short), pnms->shortsperblock, psf)) != pnms->shortsperblock)
	{	psf_log_printf (psf, "*** Warning : short read (%d != %d).\n", k, pnms->shortsperblock) ;
		memset (pnms->block + (k * sizeof (short)), 0, (pnms->shortsperblock - k) * sizeof (short)) ;
		} ;

	if (CPU_IS_BIG_ENDIAN)
		endswap_short_array ((signed short *) pnms->block, pnms->shortsperblock) ;

	nms_adpcm_decode_block (psf, pnms, pnms->block, pnms->samples) ;

	return 1 ;
} /* nms_adpcm_decode_block */

static int
nms_adpcm_read_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, short *ptr, int len)
{	int	count, indx = 0 ;

	while (indx < len)
	{	if (pnms->sample_curr >= NMS_SAMPLES_PER_BLOCK)
		{	pnms->block_curr ++ ;
			pnms->sample_curr = 0 ;
			} ;

		if (pnms->block_curr > pnms->blocks_total)
		{	memset (&(ptr [indx]), 0, (len - indx) * sizeof (short)) ;
			return indx ;
			} ;

		if (pnms->sample_curr == 0)
			psf_nms_adpcm_decode_block (psf, pnms) ;

		count = NMS_SAMPLES_PER_BLOCK - pnms->sample_curr ;
		if (len - indx < count)
			count = len - indx ;

		memcpy (&(ptr [indx]), &(pnms->samples [pnms->sample_curr]), count * sizeof (short)) ;
		indx += count ;
		pnms->sample_curr += count ;
		} ;

	return indx ;
} /* nms_adpcm_read_block */

static sf_count_t
nms_adpcm_read_s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	NMS_ADPCM_PRIVATE 	*pnms ;
	int					readcount, count ;
	sf_count_t			total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	readcount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = nms_adpcm_read_block (psf, pnms, ptr, readcount) ;

		total += count ;
		len -= count ;

		if (count != readcount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_read_s */

static sf_count_t
nms_adpcm_read_i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE *) psf->codec_data ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = nms_adpcm_read_block (psf, pnms, sptr, readcount) ;

		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = arith_shift_left (sptr [k], 16) ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_read_i */

static sf_count_t
nms_adpcm_read_f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	float 		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = nms_adpcm_read_block (psf, pnms, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * sptr [k] ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_read_f */

static sf_count_t
nms_adpcm_read_d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, readcount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	readcount = (len >= bufferlen) ? bufferlen : len ;
		count = nms_adpcm_read_block (psf, pnms, sptr, readcount) ;
		for (k = 0 ; k < readcount ; k++)
			ptr [total + k] = normfact * (double) (sptr [k]) ;

		total += count ;
		len -= readcount ;
		if (count != readcount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_read_d */

static int
psf_nms_adpcm_encode_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms)
{	int k ;

	/* Encode the samples. */
	nms_adpcm_encode_block (psf, pnms, pnms->samples, pnms->block) ;

	if (CPU_IS_BIG_ENDIAN)
		endswap_short_array ((signed short *) pnms->block, pnms->shortsperblock) ;

	/* Write the block to disk. */
	if ((k = psf_fwrite (pnms->block, sizeof (short), pnms->shortsperblock, psf)) != pnms->shortsperblock)
		psf_log_printf (psf, "*** Warning : short write (%d != %d).\n", k, pnms->shortsperblock) ;

	pnms->sample_curr = 0 ;
	pnms->block_curr ++ ;

	return 1 ;
} /* psf_nms_adpcm_encode_block */

static int
nms_adpcm_write_block (SF_PRIVATE *psf, NMS_ADPCM_PRIVATE *pnms, const short *ptr, int len)
{	int	count, total = 0, indx = 0 ;

	while (indx < len)
	{	count = NMS_SAMPLES_PER_BLOCK - pnms->sample_curr ;

		if (count > len - indx)
			count = len - indx ;

		memcpy (&(pnms->samples [pnms->sample_curr]), &(ptr [indx]), count * sizeof (short)) ;
		indx += count ;
		pnms->sample_curr += count ;
		total = indx ;

		if (pnms->sample_curr >= NMS_SAMPLES_PER_BLOCK)
			psf_nms_adpcm_encode_block (psf, pnms) ;
		} ;

	return total ;
} /* nms_adpcm_write_block */

static sf_count_t
nms_adpcm_write_s (SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	NMS_ADPCM_PRIVATE 	*pnms ;
	int			writecount, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	while (len > 0)
	{	writecount = (len > 0x10000000) ? 0x10000000 : (int) len ;

		count = nms_adpcm_write_block (psf, pnms, ptr, writecount) ;

		total += count ;
		len -= count ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_write_s */

static sf_count_t
nms_adpcm_write_i (SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = ptr [total + k] >> 16 ;
		count = nms_adpcm_write_block (psf, pnms, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;
	return total ;
} /* nms_adpcm_write_i */

static sf_count_t
nms_adpcm_write_f (SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	float		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_float == SF_TRUE) ? (1.0 * 0x8000) : 1.0 ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrintf (normfact * ptr [total + k]) ;
		count = nms_adpcm_write_block (psf, pnms, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_write_f */

static sf_count_t
nms_adpcm_write_d (SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	BUF_UNION	ubuf ;
	NMS_ADPCM_PRIVATE *pnms ;
	short		*sptr ;
	int			k, bufferlen, writecount = 0, count ;
	sf_count_t	total = 0 ;
	double		normfact ;

	if (psf->codec_data == NULL)
		return 0 ;
	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	normfact = (psf->norm_double == SF_TRUE) ? (1.0 * 0x8000) : 1.0 ;

	sptr = ubuf.sbuf ;
	bufferlen = SF_BUFFER_LEN / sizeof (short) ;
	while (len > 0)
	{	writecount = (len >= bufferlen) ? bufferlen : len ;
		for (k = 0 ; k < writecount ; k++)
			sptr [k] = lrint (normfact * ptr [total + k]) ;
		count = nms_adpcm_write_block (psf, pnms, sptr, writecount) ;

		total += count ;
		len -= writecount ;
		if (count != writecount)
			break ;
		} ;

	return total ;
} /* nms_adpcm_write_d */

int
nms_adpcm_init (SF_PRIVATE *psf)
{	NMS_ADPCM_PRIVATE	*pnms ;

	if (psf->codec_data != NULL)
	{	psf_log_printf (psf, "*** psf->codec_data is not NULL.\n") ;
		return SFE_INTERNAL ;
		} ;

	psf->sf.seekable = SF_FALSE ;

	if (psf->sf.channels != 1)
		return SFE_NMS_ADPCM_NOT_MONO ;

	if ((pnms = calloc (1, sizeof (NMS_ADPCM_PRIVATE))) == NULL)
		return SFE_MALLOC_FAILED ;

	psf->codec_data = (void*) pnms ;

	pnms->block_curr = 0 ;
	pnms->sample_curr = 0 ;

	switch (SF_CODEC (psf->sf.format))
	{	case SF_FORMAT_NMS_ADPCM_16 :
					pnms->type = NMS16 ;
					pnms->shortsperblock = NMS_BLOCK_SHORTS_16 ;
					break ;
		case SF_FORMAT_NMS_ADPCM_24 :
					pnms->type = NMS24 ;
					pnms->shortsperblock = NMS_BLOCK_SHORTS_24 ;
					break ;
		case SF_FORMAT_NMS_ADPCM_32 :
					pnms->type = NMS32 ;
					pnms->shortsperblock = NMS_BLOCK_SHORTS_32 ;
					break ;

		default : return SFE_UNIMPLEMENTED ;
	} ;
	nms_adpcm_codec_init (&pnms->state, pnms->type) ;

	psf->filelength = psf_get_filelen (psf) ;
	if (psf->filelength < psf->dataoffset)
		psf->filelength = psf->dataoffset ;

	psf->datalength = psf->filelength - psf->dataoffset ;
	if (psf->dataend > 0)
		psf->datalength -= psf->filelength - psf->dataend ;

	if (psf->file.mode == SFM_READ)
	{	psf->read_short		= nms_adpcm_read_s ;
		psf->read_int		= nms_adpcm_read_i ;
		psf->read_float		= nms_adpcm_read_f ;
		psf->read_double	= nms_adpcm_read_d ;
		}
	else if (psf->file.mode == SFM_WRITE)
	{	psf->write_short	= nms_adpcm_write_s ;
		psf->write_int		= nms_adpcm_write_i ;
		psf->write_float	= nms_adpcm_write_f ;
		psf->write_double	= nms_adpcm_write_d ;
		} ;

	if (psf->datalength % (pnms->shortsperblock * sizeof (short)))
	{	psf_log_printf (psf, "*** Odd psf->datalength (%D) should be a multiple of %d\n",
						psf->datalength, pnms->shortsperblock * sizeof (short)) ;
		pnms->blocks_total = (psf->datalength / (pnms->shortsperblock * sizeof (short))) + 1 ;
		}
	else
		pnms->blocks_total = psf->datalength / (pnms->shortsperblock * sizeof (short)) ;

	psf->sf.frames		= pnms->blocks_total * NMS_SAMPLES_PER_BLOCK ;
	psf->codec_close	= nms_adpcm_close ;
	psf->seek			= nms_adpcm_seek ;

	return 0 ;
} /* nms_adpcm_init */

static int
nms_adpcm_close (SF_PRIVATE *psf)
{	NMS_ADPCM_PRIVATE *pnms ;

	pnms = (NMS_ADPCM_PRIVATE*) psf->codec_data ;

	/*
	** If a block has been partially assembled, write it out as the final
	** block.
	*/
	if (psf->file.mode == SFM_WRITE)
	{	if (pnms->sample_curr && pnms->sample_curr < NMS_SAMPLES_PER_BLOCK)
		{	memset (pnms->samples + pnms->sample_curr, 0, (NMS_SAMPLES_PER_BLOCK - pnms->sample_curr) * sizeof (short)) ;
			psf_nms_adpcm_encode_block (psf, pnms) ;
			}

		if (psf->write_header)
			psf->write_header (psf, SF_FALSE) ;
		}

	return 0 ;
} /* nms_adpcm_close */

static sf_count_t
nms_adpcm_seek (SF_PRIVATE *psf, int mode, sf_count_t offset)
{	NMS_ADPCM_PRIVATE *pnms ;

	pnms = (NMS_ADPCM_PRIVATE *) psf->codec_data ;

	/*
	** NMS ADPCM is symmetric, so transitioning from reading and writing is
	** possible, but unimplemented, as it would require syncing partial blocks.
	*/
	if (mode != psf->file.mode)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	/*
	** NMS ADPCM cannot be seek'ed, as codec state depends on previous samples,
	** so only a seek to 0 is supported.
	*/
	if (offset != 0)
	{	psf->error = SFE_BAD_SEEK ;
		return PSF_SEEK_ERROR ;
		} ;

	if (psf_fseek (psf, psf->dataoffset, SEEK_SET) == PSF_SEEK_ERROR)
			return PSF_SEEK_ERROR ;

	nms_adpcm_codec_init (&pnms->state, pnms->type) ;
	pnms->block_curr = 0 ;
	pnms->sample_curr = 0 ;
	return 0 ;
} /* nms_adpcm_seek */

