/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include <stdio.h>
#include <assert.h>

#include "gsm610_priv.h"

/*  4.2.13 .. 4.2.17  RPE ENCODING SECTION
 */

/* 4.2.13 */

static void Weighting_filter (
	register int16_t	* e,		/* signal [-5..0.39.44]	IN  */
	int16_t		* x		/* signal [0..39]	OUT */
)
/*
 *  The coefficients of the weighting filter are stored in a table
 *  (see table 4.4).  The following scaling is used:
 *
 *	H[0..10] = integer(real_H [0..10] * 8192) ;
 */
{
	/* int16_t			wt [50] ; */

	register int32_t	L_result ;
	register int		k /* , i */ ;

	/*  Initialization of a temporary working array wt[0...49]
	 */

	/* for (k =  0 ; k <=  4 ; k++) wt[k] = 0 ;
	 * for (k =  5 ; k <= 44 ; k++) wt[k] = *e++;
	 * for (k = 45 ; k <= 49 ; k++) wt[k] = 0 ;
	 *
	 *  (e[-5..-1] and e[40..44] are allocated by the caller,
	 *  are initially zero and are not written anywhere.)
	 */
	e -= 5 ;

	/*  Compute the signal x[0..39]
	 */
	for (k = 0 ; k <= 39 ; k++)
	{	L_result = 8192 >> 1 ;

		/* for (i = 0 ; i <= 10 ; i++) {
		 *	L_temp   = GSM_L_MULT(wt[k+i], gsm_H[i]) ;
		 *	L_result = GSM_L_ADD(L_result, L_temp) ;
		 * }
		 */

#undef	STEP
#define	STEP(i, H)	(e [k + i] * (int32_t) H)

		/*  Every one of these multiplications is done twice --
		 *  but I don't see an elegant way to optimize this.
		 *  Do you?
		 */

#ifdef	STUPID_COMPILER
		L_result += STEP (0, -134) ;
		L_result += STEP (1, -374) ;
					/* + STEP (2, 0)  */
		L_result += STEP (3, 2054) ;
		L_result += STEP (4, 5741) ;
		L_result += STEP (5, 8192) ;
		L_result += STEP (6, 5741) ;
		L_result += STEP (7, 2054) ;
					/* + STEP (8, 0)  */
		L_result += STEP (9, -374) ;
		L_result += STEP (10, -134) ;
#else
		L_result += STEP (0, -134)
				+ STEP (1, -374)
					/* + STEP (2, 0)  */
				+ STEP (3, 2054)
				+ STEP (4, 5741)
				+ STEP (5, 8192)
				+ STEP (6, 5741)
				+ STEP (7, 2054)
					/* + STEP (8, 0)  */
				+ STEP (9, -374)
				+ STEP (10, -134) ;
#endif

		/* L_result = GSM_L_ADD(L_result, L_result) ; (* scaling(x2) *)
		 * L_result = GSM_L_ADD(L_result, L_result) ; (* scaling(x4) *)
		 *
		 * x[k] = SASR(L_result, 16) ;
		 */

		/* 2 adds vs. >>16 => 14, minus one shift to compensate for
		 * those we lost when replacing L_MULT by '*'.
		 */

		L_result = SASR_L (L_result, 13) ;
		x [k] = (L_result < MIN_WORD ? MIN_WORD
			: (L_result > MAX_WORD ? MAX_WORD : L_result)) ;
	}
}

/* 4.2.14 */

static void RPE_grid_selection (
	int16_t		* x,		/* [0..39]		IN  */
	int16_t		* xM,		/* [0..12]		OUT */
	int16_t		* Mc_out	/*			OUT */
)
/*
 *  The signal x[0..39] is used to select the RPE grid which is
 *  represented by Mc.
 */
{
	register int		i ;
	register int32_t	L_result, L_temp ;
	int32_t		EM ;	/* xxx should be L_EM? */
	int16_t			Mc ;

	int32_t		L_common_0_3 ;

	EM = 0 ;
	Mc = 0 ;

	/* for (m = 0 ; m <= 3 ; m++) {
	 *	L_result = 0 ;
	 *
	 *
	 *	for (i = 0 ; i <= 12 ; i++) {
	 *
	 *		temp1	= SASR_W (x[m + 3*i], 2) ;
	 *
	 *		assert (temp1 != MIN_WORD) ;
	 *
	 *		L_temp   = GSM_L_MULT(temp1, temp1) ;
	 *		L_result = GSM_L_ADD(L_temp, L_result) ;
	 *	}
	 *
	 *	if (L_result > EM) {
	 *		Mc = m ;
	 *		EM = L_result ;
	 *	}
	 * }
	 */

#undef	STEP
#define	STEP(m, i)	L_temp = SASR_W (x [m + 3 * i], 2) ;	\
					L_result += L_temp * L_temp ;

	/* common part of 0 and 3 */

	L_result = 0 ;
	STEP (0, 1) ; STEP (0, 2) ; STEP (0, 3) ; STEP (0, 4) ;
	STEP (0, 5) ; STEP (0, 6) ; STEP (0, 7) ; STEP (0, 8) ;
	STEP (0, 9) ; STEP (0, 10) ; STEP (0, 11) ; STEP (0, 12) ;
	L_common_0_3 = L_result ;

	/* i = 0 */

	STEP (0, 0) ;
	L_result <<= 1 ;	/* implicit in L_MULT */
	EM = L_result ;

	/* i = 1 */

	L_result = 0 ;
	STEP (1, 0) ;
	STEP (1, 1) ; STEP (1, 2) ; STEP (1, 3) ; STEP (1, 4) ;
	STEP (1, 5) ; STEP (1, 6) ; STEP (1, 7) ; STEP (1, 8) ;
	STEP (1, 9) ; STEP (1, 10) ; STEP (1, 11) ; STEP (1, 12) ;
	L_result <<= 1 ;
	if (L_result > EM)
	{	Mc = 1 ;
		EM = L_result ;
		}

	/* i = 2 */

	L_result = 0 ;
	STEP (2, 0) ;
	STEP (2, 1) ; STEP (2, 2) ; STEP (2, 3) ; STEP (2, 4) ;
	STEP (2, 5) ; STEP (2, 6) ; STEP (2, 7) ; STEP (2, 8) ;
	STEP (2, 9) ; STEP (2, 10) ; STEP (2, 11) ; STEP (2, 12) ;
	L_result <<= 1 ;
	if (L_result > EM)
	{	Mc = 2 ;
		EM = L_result ;
		}

	/* i = 3 */

	L_result = L_common_0_3 ;
	STEP (3, 12) ;
	L_result <<= 1 ;
	if (L_result > EM)
	{	Mc = 3 ;
		EM = L_result ;
		}

	/*  Down-sampling by a factor 3 to get the selected xM [0..12]
	 *  RPE sequence.
	 */
	for (i = 0 ; i <= 12 ; i ++) xM [i] = x [Mc + 3 * i] ;
	*Mc_out = Mc ;
}

/* 4.12.15 */

static void APCM_quantization_xmaxc_to_exp_mant (
	int16_t		xmaxc,		/* IN 	*/
	int16_t		* expon_out,	/* OUT	*/
	int16_t		* mant_out)	/* OUT  */
{
	int16_t	expon, mant ;

	/* Compute expononent and mantissa of the decoded version of xmaxc
	 */

	expon = 0 ;
	if (xmaxc > 15) expon = SASR_W (xmaxc, 3) - 1 ;
	mant = xmaxc - (expon << 3) ;

	if (mant == 0)
	{	expon = -4 ;
		mant = 7 ;
		}
	else
	{	while (mant <= 7)
		{	mant = mant << 1 | 1 ;
			expon-- ;
			}
		mant -= 8 ;
		}

	assert (expon >= -4 && expon <= 6) ;
	assert (mant >= 0 && mant <= 7) ;

	*expon_out = expon ;
	*mant_out = mant ;
}

static void APCM_quantization (
	int16_t		* xM,		/* [0..12]		IN	*/
	int16_t		* xMc,		/* [0..12]		OUT	*/
	int16_t		* mant_out,	/* 			OUT	*/
	int16_t		* expon_out,	/*			OUT	*/
	int16_t		* xmaxc_out	/*			OUT	*/
)
{
	int	i, itest ;

	int16_t	xmax, xmaxc, temp, temp1, temp2 ;
	int16_t	expon, mant ;


	/*  Find the maximum absolute value xmax of xM [0..12].
	 */

	xmax = 0 ;
	for (i = 0 ; i <= 12 ; i++)
	{	temp = xM [i] ;
		temp = GSM_ABS (temp) ;
		if (temp > xmax) xmax = temp ;
		}

	/*  Qantizing and coding of xmax to get xmaxc.
	 */

	expon = 0 ;
	temp = SASR_W (xmax, 9) ;
	itest = 0 ;

	for (i = 0 ; i <= 5 ; i++)
	{	itest |= (temp <= 0) ;
		temp = SASR_W (temp, 1) ;

		assert (expon <= 5) ;
		if (itest == 0) expon++ ;		/* expon = add (expon, 1) */
		}

	assert (expon <= 6 && expon >= 0) ;
	temp = expon + 5 ;

	assert (temp <= 11 && temp >= 0) ;
	xmaxc = gsm_add (SASR_W (xmax, temp), (int16_t) (expon << 3)) ;

	/*   Quantizing and coding of the xM [0..12] RPE sequence
	 *   to get the xMc [0..12]
	 */

	APCM_quantization_xmaxc_to_exp_mant (xmaxc, &expon, &mant) ;

	/*  This computation uses the fact that the decoded version of xmaxc
	 *  can be calculated by using the expononent and the mantissa part of
	 *  xmaxc (logarithmic table).
	 *  So, this method avoids any division and uses only a scaling
	 *  of the RPE samples by a function of the expononent.  A direct
	 *  multiplication by the inverse of the mantissa (NRFAC[0..7]
	 *  found in table 4.5) gives the 3 bit coded version xMc [0..12]
	 *  of the RPE samples.
	 */


	/* Direct computation of xMc [0..12] using table 4.5
	 */

	assert (expon <= 4096 && expon >= -4096) ;
	assert (mant >= 0 && mant <= 7) ;

	temp1 = 6 - expon ;			/* normalization by the expononent */
	temp2 = gsm_NRFAC [mant] ;	/* inverse mantissa 		 */

	for (i = 0 ; i <= 12 ; i++)
	{	assert (temp1 >= 0 && temp1 < 16) ;

		temp = arith_shift_left (xM [i], temp1) ;
		temp = GSM_MULT (temp, temp2) ;
		temp = SASR_W (temp, 12) ;
		xMc [i] = temp + 4 ;		/* see note below */
	}

	/*  NOTE: This equation is used to make all the xMc [i] positive.
	 */

	*mant_out = mant ;
	*expon_out = expon ;
	*xmaxc_out = xmaxc ;
}

/* 4.2.16 */

static void APCM_inverse_quantization (
	register int16_t	* xMc,	/* [0..12]			IN 	*/
	int16_t		mant,
	int16_t		expon,
	register int16_t	* xMp)	/* [0..12]			OUT 	*/
/*
 *  This part is for decoding the RPE sequence of coded xMc [0..12]
 *  samples to obtain the xMp[0..12] array.  Table 4.6 is used to get
 *  the mantissa of xmaxc (FAC[0..7]).
 */
{
	int	i ;
	int16_t	temp, temp1, temp2, temp3 ;

	assert (mant >= 0 && mant <= 7) ;

	temp1 = gsm_FAC [mant] ;	/* see 4.2-15 for mant */
	temp2 = gsm_sub (6, expon) ;	/* see 4.2-15 for exp  */
	temp3 = gsm_asl (1, gsm_sub (temp2, 1)) ;

	for (i = 13 ; i-- ;)
	{	assert (*xMc <= 7 && *xMc >= 0) ;	/* 3 bit unsigned */

		/* temp = gsm_sub (*xMc++ << 1, 7) ; */
		temp = (*xMc++ << 1) - 7 ;			/* restore sign   */
		assert (temp <= 7 && temp >= -7) ;	/* 4 bit signed   */

		temp = arith_shift_left (temp, 12) ;	/* 16 bit signed  */
		temp = GSM_MULT_R (temp1, temp) ;
		temp = GSM_ADD (temp, temp3) ;
		*xMp++ = gsm_asr (temp, temp2) ;
	}
}

/* 4.2.17 */

static void RPE_grid_positioning (
	int16_t		Mc,		/* grid position	IN	*/
	register int16_t	* xMp,		/* [0..12]		IN	*/
	register int16_t	* ep		/* [0..39]		OUT	*/
)
/*
 *  This procedure computes the reconstructed long term residual signal
 *  ep[0..39] for the LTP analysis filter.  The inputs are the Mc
 *  which is the grid position selection and the xMp[0..12] decoded
 *  RPE samples which are upsampled by a factor of 3 by inserting zero
 *  values.
 */
{
	int	i = 13 ;

	assert (0 <= Mc && Mc <= 3) ;

	switch (Mc)
	{	case 3: *ep++ = 0 ;
				/* Falls through. */
		case 2: do
				{	*ep++ = 0 ;
				/* Falls through. */
		case 1:		*ep++ = 0 ;
				/* Falls through. */
		case 0:		*ep++ = *xMp++ ;
					} while (--i) ;
	}
	while (++Mc < 4) *ep++ = 0 ;
}

/* 4.2.18 */

/*  This procedure adds the reconstructed long term residual signal
 *  ep[0..39] to the estimated signal dpp[0..39] from the long term
 *  analysis filter to compute the reconstructed short term residual
 *  signal dp[-40..-1] ; also the reconstructed short term residual
 *  array dp[-120..-41] is updated.
 */

#if 0	/* Has been inlined in code.c */
void Gsm_Update_of_reconstructed_short_time_residual_signal (
	int16_t	* dpp,		/* [0...39]	IN	*/
	int16_t	* ep,		/* [0...39]	IN	*/
	int16_t	* dp)		/* [-120...-1]  IN/OUT 	*/
{
	int 		k ;

	for (k = 0 ; k <= 79 ; k++)
		dp [-120 + k] = dp [-80 + k] ;

	for (k = 0 ; k <= 39 ; k++)
		dp [-40 + k] = gsm_add (ep [k], dpp [k]) ;
}
#endif	/* Has been inlined in code.c */

void Gsm_RPE_Encoding (
	int16_t	* e,		/* -5..-1][0..39][40..44	IN/OUT  */
	int16_t	* xmaxc,	/* 				OUT */
	int16_t	* Mc,		/* 			  	OUT */
	int16_t	* xMc)		/* [0..12]			OUT */
{
	int16_t	x [40] ;
	int16_t	xM [13], xMp [13] ;
	int16_t	mant, expon ;

	Weighting_filter (e, x) ;
	RPE_grid_selection (x, xM, Mc) ;

	APCM_quantization (xM, xMc, &mant, &expon, xmaxc) ;
	APCM_inverse_quantization (xMc, mant, expon, xMp) ;

	RPE_grid_positioning (*Mc, xMp, e) ;

}

void Gsm_RPE_Decoding (
	int16_t 		xmaxcr,
	int16_t		Mcr,
	int16_t		* xMcr,	/* [0..12], 3 bits 		IN	*/
	int16_t		* erp	/* [0..39]			OUT 	*/
)
{
	int16_t	expon, mant ;
	int16_t	xMp [13] ;

	APCM_quantization_xmaxc_to_exp_mant (xmaxcr, &expon, &mant) ;
	APCM_inverse_quantization (xMcr, mant, expon, xMp) ;
	RPE_grid_positioning (Mcr, xMp, erp) ;
}
