/*
 * This source code is a product of Sun Microsystems, Inc. and is provided
 * for unrestricted use.  Users may copy or modify this source code without
 * charge.
 *
 * SUN SOURCE CODE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING
 * THE WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * Sun source code is provided with no support and without any obligation on
 * the part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS SOFTWARE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even if
 * Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * g72x.c
 *
 * Common routines for G.721 and G.723 conversions.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "g72x.h"
#include "g72x_priv.h"

static G72x_STATE * g72x_state_new (void) ;
static int unpack_bytes (int bits, int blocksize, const unsigned char * block, short * samples) ;
static int pack_bytes (int bits, const short * samples, unsigned char * block) ;

static
short power2 [15] =
{	1, 2, 4, 8, 0x10, 0x20, 0x40, 0x80,
	0x100, 0x200, 0x400, 0x800, 0x1000, 0x2000, 0x4000
} ;

/*
 * quan ()
 *
 * quantizes the input val against the table of size short integers.
 * It returns i if table [i - 1] <= val < table [i].
 *
 * Using linear search for simple coding.
 */
static
int quan (int val, short *table, int size)
{
	int		i ;

	for (i = 0 ; i < size ; i++)
		if (val < *table++)
			break ;
	return i ;
}

/*
 * fmult ()
 *
 * returns the integer product of the 14-bit integer "an" and
 * "floating point" representation (4-bit exponent, 6-bit mantessa) "srn".
 */
static
int fmult (int an, int srn)
{
	short		anmag, anexp, anmant ;
	short		wanexp, wanmant ;
	short		retval ;

	anmag = (an > 0) ? an : ((-an) & 0x1FFF) ;
	anexp = quan (anmag, power2, 15) - 6 ;
	anmant = (anmag == 0) ? 32 :
				(anexp >= 0) ? anmag >> anexp : anmag << -anexp ;
	wanexp = anexp + ((srn >> 6) & 0xF) - 13 ;

	/*
	** The original was :
	**		wanmant = (anmant * (srn & 0x3F) + 0x30) >> 4 ;
	** but could see no valid reason for the + 0x30.
	** Removed it and it improved the SNR of the codec.
	*/

	wanmant = (anmant * (srn & 0x3F)) >> 4 ;

	retval = (wanexp >= 0) ? ((wanmant << wanexp) & 0x7FFF) : (wanmant >> -wanexp) ;

	return (((an ^ srn) < 0) ? -retval : retval) ;
}

static G72x_STATE * g72x_state_new (void)
{	return calloc (1, sizeof (G72x_STATE)) ;
}

/*
 * private_init_state ()
 *
 * This routine initializes and/or resets the G72x_PRIVATE structure
 * pointed to by 'state_ptr'.
 * All the initial state values are specified in the CCITT G.721 document.
 */
void private_init_state (G72x_STATE *state_ptr)
{
	int		cnta ;

	state_ptr->yl = 34816 ;
	state_ptr->yu = 544 ;
	state_ptr->dms = 0 ;
	state_ptr->dml = 0 ;
	state_ptr->ap = 0 ;
	for (cnta = 0 ; cnta < 2 ; cnta++)
	{	state_ptr->a [cnta] = 0 ;
		state_ptr->pk [cnta] = 0 ;
		state_ptr->sr [cnta] = 32 ;
		}
	for (cnta = 0 ; cnta < 6 ; cnta++)
	{	state_ptr->b [cnta] = 0 ;
		state_ptr->dq [cnta] = 32 ;
		}
	state_ptr->td = 0 ;
}	/* private_init_state */

struct g72x_state * g72x_reader_init (int codec, int *blocksize, int *samplesperblock)
{	G72x_STATE *pstate ;

	if ((pstate = g72x_state_new ()) == NULL)
		return NULL ;

	private_init_state (pstate) ;

	pstate->encoder = NULL ;

	switch (codec)
	{	case G723_16_BITS_PER_SAMPLE : /* 2 bits per sample. */
				pstate->decoder = g723_16_decoder ;
				*blocksize = G723_16_BYTES_PER_BLOCK ;
				*samplesperblock = G723_16_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 2 ;
				pstate->blocksize = G723_16_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G723_16_SAMPLES_PER_BLOCK ;
				break ;

		case G723_24_BITS_PER_SAMPLE : /* 3 bits per sample. */
				pstate->decoder = g723_24_decoder ;
				*blocksize = G723_24_BYTES_PER_BLOCK ;
				*samplesperblock = G723_24_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 3 ;
				pstate->blocksize = G723_24_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G723_24_SAMPLES_PER_BLOCK ;
				break ;

		case G721_32_BITS_PER_SAMPLE : /* 4 bits per sample. */
				pstate->decoder = g721_decoder ;
				*blocksize = G721_32_BYTES_PER_BLOCK ;
				*samplesperblock = G721_32_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 4 ;
				pstate->blocksize = G721_32_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G721_32_SAMPLES_PER_BLOCK ;
				break ;

		case G721_40_BITS_PER_SAMPLE : /* 5 bits per sample. */
				pstate->decoder = g723_40_decoder ;
				*blocksize = G721_40_BYTES_PER_BLOCK ;
				*samplesperblock = G721_40_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 5 ;
				pstate->blocksize = G721_40_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G721_40_SAMPLES_PER_BLOCK ;
				break ;

		default :
				free (pstate) ;
				return NULL ;
		} ;

	return pstate ;
}	/* g72x_reader_init */

struct g72x_state * g72x_writer_init (int codec, int *blocksize, int *samplesperblock)
{	G72x_STATE *pstate ;

	if ((pstate = g72x_state_new ()) == NULL)
		return NULL ;

	private_init_state (pstate) ;
	pstate->decoder = NULL ;

	switch (codec)
	{	case G723_16_BITS_PER_SAMPLE : /* 2 bits per sample. */
				pstate->encoder = g723_16_encoder ;
				*blocksize = G723_16_BYTES_PER_BLOCK ;
				*samplesperblock = G723_16_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 2 ;
				pstate->blocksize = G723_16_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G723_16_SAMPLES_PER_BLOCK ;
				break ;

		case G723_24_BITS_PER_SAMPLE : /* 3 bits per sample. */
				pstate->encoder = g723_24_encoder ;
				*blocksize = G723_24_BYTES_PER_BLOCK ;
				*samplesperblock = G723_24_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 3 ;
				pstate->blocksize = G723_24_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G723_24_SAMPLES_PER_BLOCK ;
				break ;

		case G721_32_BITS_PER_SAMPLE : /* 4 bits per sample. */
				pstate->encoder = g721_encoder ;
				*blocksize = G721_32_BYTES_PER_BLOCK ;
				*samplesperblock = G721_32_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 4 ;
				pstate->blocksize = G721_32_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G721_32_SAMPLES_PER_BLOCK ;
				break ;

		case G721_40_BITS_PER_SAMPLE : /* 5 bits per sample. */
				pstate->encoder = g723_40_encoder ;
				*blocksize = G721_40_BYTES_PER_BLOCK ;
				*samplesperblock = G721_40_SAMPLES_PER_BLOCK ;
				pstate->codec_bits = 5 ;
				pstate->blocksize = G721_40_BYTES_PER_BLOCK ;
				pstate->samplesperblock = G721_40_SAMPLES_PER_BLOCK ;
				break ;

		default :
				free (pstate) ;
				return NULL ;
		} ;

	return pstate ;
}	/* g72x_writer_init */

int g72x_decode_block (G72x_STATE *pstate, const unsigned char *block, short *samples)
{	int	k, count ;

	count = unpack_bytes (pstate->codec_bits, pstate->blocksize, block, samples) ;

	for (k = 0 ; k < count ; k++)
		samples [k] = pstate->decoder (samples [k], pstate) ;

	return 0 ;
}	/* g72x_decode_block */

int g72x_encode_block (G72x_STATE *pstate, short *samples, unsigned char *block)
{	int k, count ;

	for (k = 0 ; k < pstate->samplesperblock ; k++)
		samples [k] = pstate->encoder (samples [k], pstate) ;

	count = pack_bytes (pstate->codec_bits, samples, block) ;

	return count ;
}	/* g72x_encode_block */

/*
 * predictor_zero ()
 *
 * computes the estimated signal from 6-zero predictor.
 *
 */
int predictor_zero (G72x_STATE *state_ptr)
{
	int		i ;
	int		sezi ;

	sezi = fmult (state_ptr->b [0] >> 2, state_ptr->dq [0]) ;
	for (i = 1 ; i < 6 ; i++)			/* ACCUM */
		sezi += fmult (state_ptr->b [i] >> 2, state_ptr->dq [i]) ;
	return sezi ;
}
/*
 * predictor_pole ()
 *
 * computes the estimated signal from 2-pole predictor.
 *
 */
int predictor_pole (G72x_STATE *state_ptr)
{
	return (fmult (state_ptr->a [1] >> 2, state_ptr->sr [1]) +
			fmult (state_ptr->a [0] >> 2, state_ptr->sr [0])) ;
}
/*
 * step_size ()
 *
 * computes the quantization step size of the adaptive quantizer.
 *
 */
int step_size (G72x_STATE *state_ptr)
{
	int		y ;
	int		dif ;
	int		al ;

	if (state_ptr->ap >= 256)
		return (state_ptr->yu) ;
	else {
		y = state_ptr->yl >> 6 ;
		dif = state_ptr->yu - y ;
		al = state_ptr->ap >> 2 ;
		if (dif > 0)
			y += (dif * al) >> 6 ;
		else if (dif < 0)
			y += (dif * al + 0x3F) >> 6 ;
		return y ;
	}
}

/*
 * quantize ()
 *
 * Given a raw sample, 'd', of the difference signal and a
 * quantization step size scale factor, 'y', this routine returns the
 * ADPCM codeword to which that sample gets quantized.  The step
 * size scale factor division operation is done in the log base 2 domain
 * as a subtraction.
 */
int quantize (
	int		d,	/* Raw difference signal sample */
	int		y,	/* Step size multiplier */
	short	*table,	/* quantization table */
	int		size)	/* table size of short integers */
{
	short		dqm ;	/* Magnitude of 'd' */
	short		expon ;	/* Integer part of base 2 log of 'd' */
	short		mant ;	/* Fractional part of base 2 log */
	short		dl ;	/* Log of magnitude of 'd' */
	short		dln ;	/* Step size scale factor normalized log */
	int		i ;

	/*
	 * LOG
	 *
	 * Compute base 2 log of 'd', and store in 'dl'.
	 */
	dqm = abs (d) ;
	expon = quan (dqm >> 1, power2, 15) ;
	mant = ((dqm << 7) >> expon) & 0x7F ;	/* Fractional portion. */
	dl = (expon << 7) + mant ;

	/*
	 * SUBTB
	 *
	 * "Divide" by step size multiplier.
	 */
	dln = dl - (y >> 2) ;

	/*
	 * QUAN
	 *
	 * Obtain codword i for 'd'.
	 */
	i = quan (dln, table, size) ;
	if (d < 0)			/* take 1's complement of i */
		return ((size << 1) + 1 - i) ;
	else if (i == 0)		/* take 1's complement of 0 */
		return ((size << 1) + 1) ; /* new in 1988 */

	return i ;
}
/*
 * reconstruct ()
 *
 * Returns reconstructed difference signal 'dq' obtained from
 * codeword 'i' and quantization step size scale factor 'y'.
 * Multiplication is performed in log base 2 domain as addition.
 */
int
reconstruct (
	int		sign,	/* 0 for non-negative value */
	int		dqln,	/* G.72x codeword */
	int		y)	/* Step size multiplier */
{
	short		dql ;	/* Log of 'dq' magnitude */
	short		dex ;	/* Integer part of log */
	short		dqt ;
	short		dq ;	/* Reconstructed difference signal sample */

	dql = dqln + (y >> 2) ;	/* ADDA */

	if (dql < 0)
		return ((sign) ? -0x8000 : 0) ;
	else		/* ANTILOG */
	{	dex = (dql >> 7) & 15 ;
		dqt = 128 + (dql & 127) ;
		dq = (dqt << 7) >> (14 - dex) ;
		return ((sign) ? (dq - 0x8000) : dq) ;
		}
}


/*
 * update ()
 *
 * updates the state variables for each output code
 */
void
update (
	int		code_size,	/* distinguish 723_40 with others */
	int		y,		/* quantizer step size */
	int		wi,		/* scale factor multiplier */
	int		fi,		/* for long/short term energies */
	int		dq,		/* quantized prediction difference */
	int		sr,		/* reconstructed signal */
	int		dqsez,		/* difference from 2-pole predictor */
	G72x_STATE *state_ptr)	/* coder state pointer */
{
	int		cnt ;
	short		mag, expon ;	/* Adaptive predictor, FLOAT A */
	short		a2p = 0 ;	/* LIMC */
	short		a1ul ;		/* UPA1 */
	short		pks1 ;		/* UPA2 */
	short		fa1 ;
	char		tr ;		/* tone/transition detector */
	short		ylint, thr2, dqthr ;
	short		ylfrac, thr1 ;
	short		pk0 ;

	pk0 = (dqsez < 0) ? 1 : 0 ;	/* needed in updating predictor poles */

	mag = dq & 0x7FFF ;		/* prediction difference magnitude */
	/* TRANS */
	ylint = state_ptr->yl >> 15 ;	/* exponent part of yl */
	ylfrac = (state_ptr->yl >> 10) & 0x1F ;	/* fractional part of yl */
	thr1 = (32 + ylfrac) << ylint ;		/* threshold */
	thr2 = (ylint > 9) ? 31 << 10 : thr1 ;	/* limit thr2 to 31 << 10 */
	dqthr = (thr2 + (thr2 >> 1)) >> 1 ;	/* dqthr = 0.75 * thr2 */
	if (state_ptr->td == 0)		/* signal supposed voice */
		tr = 0 ;
	else if (mag <= dqthr)		/* supposed data, but small mag */
		tr = 0 ;			/* treated as voice */
	else				/* signal is data (modem) */
		tr = 1 ;

	/*
	 * Quantizer scale factor adaptation.
	 */

	/* FUNCTW & FILTD & DELAY */
	/* update non-steady state step size multiplier */
	state_ptr->yu = y + ((wi - y) >> 5) ;

	/* LIMB */
	if (state_ptr->yu < 544)	/* 544 <= yu <= 5120 */
		state_ptr->yu = 544 ;
	else if (state_ptr->yu > 5120)
		state_ptr->yu = 5120 ;

	/* FILTE & DELAY */
	/* update steady state step size multiplier */
	state_ptr->yl += state_ptr->yu + ((-state_ptr->yl) >> 6) ;

	/*
	 * Adaptive predictor coefficients.
	 */
	if (tr == 1) {			/* reset a's and b's for modem signal */
		state_ptr->a [0] = 0 ;
		state_ptr->a [1] = 0 ;
		state_ptr->b [0] = 0 ;
		state_ptr->b [1] = 0 ;
		state_ptr->b [2] = 0 ;
		state_ptr->b [3] = 0 ;
		state_ptr->b [4] = 0 ;
		state_ptr->b [5] = 0 ;
		}
	else			/* update a's and b's */
	{	pks1 = pk0 ^ state_ptr->pk [0] ;		/* UPA2 */

		/* update predictor pole a [1] */
		a2p = state_ptr->a [1] - (state_ptr->a [1] >> 7) ;
		if (dqsez != 0)
		{	fa1 = (pks1) ? state_ptr->a [0] : -state_ptr->a [0] ;
			if (fa1 < -8191)	/* a2p = function of fa1 */
				a2p -= 0x100 ;
			else if (fa1 > 8191)
				a2p += 0xFF ;
			else
				a2p += fa1 >> 5 ;

			if (pk0 ^ state_ptr->pk [1])
			{	/* LIMC */
				if (a2p <= -12160)
					a2p = -12288 ;
				else if (a2p >= 12416)
					a2p = 12288 ;
				else
					a2p -= 0x80 ;
				}
			else if (a2p <= -12416)
				a2p = -12288 ;
			else if (a2p >= 12160)
				a2p = 12288 ;
			else
				a2p += 0x80 ;
		}

		/* TRIGB & DELAY */
		state_ptr->a [1] = a2p ;

		/* UPA1 */
		/* update predictor pole a [0] */
		state_ptr->a [0] -= state_ptr->a [0] >> 8 ;
		if (dqsez != 0)
		{	if (pks1 == 0)
				state_ptr->a [0] += 192 ;
			else
				state_ptr->a [0] -= 192 ;
			} ;

		/* LIMD */
		a1ul = 15360 - a2p ;
		if (state_ptr->a [0] < -a1ul)
			state_ptr->a [0] = -a1ul ;
		else if (state_ptr->a [0] > a1ul)
			state_ptr->a [0] = a1ul ;

		/* UPB : update predictor zeros b [6] */
		for (cnt = 0 ; cnt < 6 ; cnt++)
		{	if (code_size == 5)		/* for 40Kbps G.723 */
				state_ptr->b [cnt] -= state_ptr->b [cnt] >> 9 ;
			else			/* for G.721 and 24Kbps G.723 */
				state_ptr->b [cnt] -= state_ptr->b [cnt] >> 8 ;
			if (dq & 0x7FFF)			/* XOR */
			{	if ((dq ^ state_ptr->dq [cnt]) >= 0)
					state_ptr->b [cnt] += 128 ;
				else
					state_ptr->b [cnt] -= 128 ;
				}
			}
		}

	for (cnt = 5 ; cnt > 0 ; cnt--)
		state_ptr->dq [cnt] = state_ptr->dq [cnt - 1] ;
	/* FLOAT A : convert dq [0] to 4-bit exp, 6-bit mantissa f.p. */
	if (mag == 0)
		state_ptr->dq [0] = (dq >= 0) ? 0x20 : 0xFC20 ;
	else
	{	expon = quan (mag, power2, 15) ;
		state_ptr->dq [0] = (dq >= 0) ?
			(expon << 6) + ((mag << 6) >> expon) :
			(expon << 6) + ((mag << 6) >> expon) - 0x400 ;
		}

	state_ptr->sr [1] = state_ptr->sr [0] ;
	/* FLOAT B : convert sr to 4-bit exp., 6-bit mantissa f.p. */
	if (sr == 0)
		state_ptr->sr [0] = 0x20 ;
	else if (sr > 0)
	{	expon = quan (sr, power2, 15) ;
		state_ptr->sr [0] = (expon << 6) + ((sr << 6) >> expon) ;
		}
	else if (sr > -32768)
	{	mag = -sr ;
		expon = quan (mag, power2, 15) ;
		state_ptr->sr [0] = (expon << 6) + ((mag << 6) >> expon) - 0x400 ;
		}
	else
		state_ptr->sr [0] = (short) 0xFC20 ;

	/* DELAY A */
	state_ptr->pk [1] = state_ptr->pk [0] ;
	state_ptr->pk [0] = pk0 ;

	/* TONE */
	if (tr == 1)		/* this sample has been treated as data */
		state_ptr->td = 0 ;	/* next one will be treated as voice */
	else if (a2p < -11776)	/* small sample-to-sample correlation */
		state_ptr->td = 1 ;	/* signal may be data */
	else				/* signal is voice */
		state_ptr->td = 0 ;

	/*
	 * Adaptation speed control.
	 */
	state_ptr->dms += (fi - state_ptr->dms) >> 5 ;		/* FILTA */
	state_ptr->dml += (((fi << 2) - state_ptr->dml) >> 7) ;	/* FILTB */

	if (tr == 1)
		state_ptr->ap = 256 ;
	else if (y < 1536)					/* SUBTC */
		state_ptr->ap += (0x200 - state_ptr->ap) >> 4 ;
	else if (state_ptr->td == 1)
		state_ptr->ap += (0x200 - state_ptr->ap) >> 4 ;
	else if (abs ((state_ptr->dms << 2) - state_ptr->dml) >= (state_ptr->dml >> 3))
		state_ptr->ap += (0x200 - state_ptr->ap) >> 4 ;
	else
		state_ptr->ap += (-state_ptr->ap) >> 4 ;

	return ;
} /* update */

/*------------------------------------------------------------------------------
*/

static int
unpack_bytes (int bits, int blocksize, const unsigned char * block, short * samples)
{	unsigned int	in_buffer = 0 ;
	unsigned char	in_byte ;
	int				k, in_bits = 0, bindex = 0 ;

	for (k = 0 ; bindex <= blocksize && k < G72x_BLOCK_SIZE ; k++)
	{	if (in_bits < bits)
		{	in_byte = block [bindex++] ;

			in_buffer |= (in_byte << in_bits) ;
			in_bits += 8 ;
			}
		samples [k] = in_buffer & ((1 << bits) - 1) ;
		in_buffer >>= bits ;
		in_bits -= bits ;
		} ;

	return k ;
} /* unpack_bytes */

static int
pack_bytes (int bits, const short * samples, unsigned char * block)
{
	unsigned int	out_buffer = 0 ;
	int				k, bindex = 0, out_bits = 0 ;
	unsigned char	out_byte ;

	for (k = 0 ; k < G72x_BLOCK_SIZE ; k++)
	{	out_buffer |= (samples [k] << out_bits) ;
		out_bits += bits ;
		if (out_bits >= 8)
		{	out_byte = out_buffer & 0xFF ;
			out_bits -= 8 ;
			out_buffer >>= 8 ;
			block [bindex++] = out_byte ;
			}
		} ;

	return bindex ;
} /* pack_bytes */

