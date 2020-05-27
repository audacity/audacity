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
 * g721.c
 *
 * Description:
 *
 * g721_encoder (), g721_decoder ()
 *
 * These routines comprise an implementation of the CCITT G.721 ADPCM
 * coding algorithm.  Essentially, this implementation is identical to
 * the bit level description except for a few deviations which
 * take advantage of work station attributes, such as hardware 2's
 * complement arithmetic and large memory.  Specifically, certain time
 * consuming operations such as multiplications are replaced
 * with lookup tables and software 2's complement operations are
 * replaced with hardware 2's complement.
 *
 * The deviation from the bit level specification (lookup tables)
 * preserves the bit level performance specifications.
 *
 * As outlined in the G.721 Recommendation, the algorithm is broken
 * down into modules.  Each section of code below is preceded by
 * the name of the module which it is implementing.
 *
 */

#include "g72x.h"
#include "g72x_priv.h"

static short qtab_721 [7] = { -124, 80, 178, 246, 300, 349, 400 } ;
/*
 * Maps G.721 code word to reconstructed scale factor normalized log
 * magnitude values.
 */
static short _dqlntab [16] = { -2048, 4, 135, 213, 273, 323, 373, 425,
							425, 373, 323, 273, 213, 135, 4, -2048 } ;

/* Maps G.721 code word to log of scale factor multiplier. */
static short _witab [16] = { -12, 18, 41, 64, 112, 198, 355, 1122,
							1122, 355, 198, 112, 64, 41, 18, -12 } ;
/*
 * Maps G.721 code words to a set of values whose long and short
 * term averages are computed and then compared to give an indication
 * how stationary (steady state) the signal is.
 */
static short _fitab [16] = { 0, 0, 0, 0x200, 0x200, 0x200, 0x600, 0xE00,
							0xE00, 0x600, 0x200, 0x200, 0x200, 0, 0, 0 } ;

/*
 * g721_encoder ()
 *
 * Encodes the input vale of linear PCM, A-law or u-law data sl and returns
 * the resulting code. -1 is returned for unknown input coding value.
 */
int
g721_encoder (
	int		sl,
	G72x_STATE *state_ptr)
{
	short		sezi, se, sez ;		/* ACCUM */
	short		d ;			/* SUBTA */
	short		sr ;			/* ADDB */
	short		y ;			/* MIX */
	short		dqsez ;			/* ADDC */
	short		dq, i ;

	/* linearize input sample to 14-bit PCM */
	sl >>= 2 ;			/* 14-bit dynamic range */

	sezi = predictor_zero (state_ptr) ;
	sez = sezi >> 1 ;
	se = (sezi + predictor_pole (state_ptr)) >> 1 ;	/* estimated signal */

	d = sl - se ;				/* estimation difference */

	/* quantize the prediction difference */
	y = step_size (state_ptr) ;		/* quantizer step size */
	i = quantize (d, y, qtab_721, 7) ;	/* i = ADPCM code */

	dq = reconstruct (i & 8, _dqlntab [i], y) ;	/* quantized est diff */

	sr = (dq < 0) ? se - (dq & 0x3FFF) : se + dq ;	/* reconst. signal */

	dqsez = sr + sez - se ;			/* pole prediction diff. */

	update (4, y, arith_shift_left (_witab [i], 5), _fitab [i], dq, sr, dqsez, state_ptr) ;

	return i ;
}

/*
 * g721_decoder ()
 *
 * Description:
 *
 * Decodes a 4-bit code of G.721 encoded data of i and
 * returns the resulting linear PCM, A-law or u-law value.
 * return -1 for unknown out_coding value.
 */
int
g721_decoder (
	int		i,
	G72x_STATE *state_ptr)
{
	short		sezi, sei, sez, se ;	/* ACCUM */
	short		y ;			/* MIX */
	short		sr ;			/* ADDB */
	short		dq ;
	short		dqsez ;

	i &= 0x0f ;			/* mask to get proper bits */
	sezi = predictor_zero (state_ptr) ;
	sez = sezi >> 1 ;
	sei = sezi + predictor_pole (state_ptr) ;
	se = sei >> 1 ;			/* se = estimated signal */

	y = step_size (state_ptr) ;	/* dynamic quantizer step size */

	dq = reconstruct (i & 0x08, _dqlntab [i], y) ; /* quantized diff. */

	sr = (dq < 0) ? (se - (dq & 0x3FFF)) : se + dq ;	/* reconst. signal */

	dqsez = sr - se + sez ;			/* pole prediction diff. */

	update (4, y, arith_shift_left (_witab [i], 5), _fitab [i], dq, sr, dqsez, state_ptr) ;

	/* sr was 14-bit dynamic range */
	return arith_shift_left (sr, 2) ;
}

