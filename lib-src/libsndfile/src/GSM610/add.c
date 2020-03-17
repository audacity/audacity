/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

/*
 *  See private.h for the more commonly used macro versions.
 */

#include	<stdio.h>
#include	<assert.h>

#include	"gsm610_priv.h"

#define	saturate(x) 	\
	((x) < MIN_WORD ? MIN_WORD : (x) > MAX_WORD ? MAX_WORD: (x))

int16_t gsm_add (int16_t a, int16_t b)
{
	int32_t sum = (int32_t) a + (int32_t) b ;
	return saturate (sum) ;
}

int16_t gsm_sub (int16_t a, int16_t b)
{
	int32_t diff = (int32_t) a - (int32_t) b ;
	return saturate (diff) ;
}

int16_t gsm_mult (int16_t a, int16_t b)
{
	if (a == MIN_WORD && b == MIN_WORD)
		return MAX_WORD ;

	return SASR_L ((int32_t) a * (int32_t) b, 15) ;
}

int16_t gsm_mult_r (int16_t a, int16_t b)
{
	if (b == MIN_WORD && a == MIN_WORD)
		return MAX_WORD ;
	else
	{	int32_t prod = (int32_t) a * (int32_t) b + 16384 ;
		prod >>= 15 ;
		return prod & 0xFFFF ;
		}
}

int16_t gsm_abs (int16_t a)
{
	return a < 0 ? (a == MIN_WORD ? MAX_WORD : -a) : a ;
}

int32_t gsm_L_mult (int16_t a, int16_t b)
{
	assert (a != MIN_WORD || b != MIN_WORD) ;
	return ((int32_t) a * (int32_t) b) << 1 ;
}

int32_t gsm_L_add (int32_t a, int32_t b)
{
	if (a < 0)
	{	if (b >= 0)
			return a + b ;
		else
		{	uint32_t A = (uint32_t) - (a + 1) + (uint32_t) - (b + 1) ;
			return A >= MAX_LONGWORD ? MIN_LONGWORD : - (int32_t) A - 2 ;
			}
		}
	else if (b <= 0)
		return a + b ;
	else
	{	uint32_t A = (uint32_t) a + (uint32_t) b ;
		return A > MAX_LONGWORD ? MAX_LONGWORD : A ;
		}
}

int32_t gsm_L_sub (int32_t a, int32_t b)
{
	if (a >= 0)
	{	if (b >= 0)
			return a - b ;
		else
		{	/* a>=0, b<0 */
			uint32_t A = (uint32_t) a + - (b + 1) ;
			return A >= MAX_LONGWORD ? MAX_LONGWORD : (A + 1) ;
			}
		}
	else if (b <= 0)
		return a - b ;
	else
	{	/* a<0, b>0 */
		uint32_t A = (uint32_t) - (a + 1) + b ;
		return A >= MAX_LONGWORD ? MIN_LONGWORD : - (int32_t) A - 1 ;
		}
}

static unsigned char const bitoff [256] = {
	8, 7, 6, 6, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
} ;

int16_t gsm_norm (int32_t a)
/*
 * the number of left shifts needed to normalize the 32 bit
 * variable L_var1 for positive values on the interval
 *
 * with minimum of
 * minimum of 1073741824  (01000000000000000000000000000000) and
 * maximum of 2147483647  (01111111111111111111111111111111)
 *
 *
 * and for negative values on the interval with
 * minimum of -2147483648 (-10000000000000000000000000000000) and
 * maximum of -1073741824 (-1000000000000000000000000000000).
 *
 * in order to normalize the result, the following
 * operation must be done: L_norm_var1 = L_var1 << norm (L_var1) ;
 *
 * (That's 'ffs', only from the left, not the right..)
 */
{
	assert (a != 0) ;

	if (a < 0)
	{	if (a <= -1073741824) return 0 ;
		a = ~a ;
		}

	return a & 0xffff0000
		? (a & 0xff000000
			? -1 + bitoff [0xFF & (a >> 24)]
			: 7 + bitoff [0xFF & (a >> 16)])
		: (a & 0xff00
			? 15 + bitoff [0xFF & (a >> 8)]
			: 23 + bitoff [0xFF & a]) ;
}

int32_t gsm_L_asl (int32_t a, int n)
{
	if (n >= 32) return 0 ;
	if (n <= -32) return - (a < 0) ;
	if (n < 0) return gsm_L_asr (a, -n) ;
	return a << n ;
}

int16_t gsm_asr (int16_t a, int n)
{
	if (n >= 16) return - (a < 0) ;
	if (n <= -16) return 0 ;
	if (n < 0) return a << -n ;

	return SASR_W (a, (int16_t) n) ;
}

int16_t gsm_asl (int16_t a, int n)
{
	if (n >= 16) return 0 ;
	if (n <= -16) return - (a < 0) ;
	if (n < 0) return gsm_asr (a, -n) ;
	return a << n ;
}

int32_t gsm_L_asr (int32_t a, int n)
{
	if (n >= 32) return - (a < 0) ;
	if (n <= -32) return 0 ;
	if (n < 0) return a << -n ;

	return SASR_L (a, (int16_t) n) ;
}

/*
**	int16_t gsm_asr (int16_t a, int n)
**	{
**		if (n >= 16) return - (a < 0) ;
**		if (n <= -16) return 0 ;
**		if (n < 0) return a << -n ;
**
**	#	ifdef	SASR_W
**			return a >> n ;
**	#	else
**			if (a >= 0) return a >> n ;
**			else return - (int16_t) (- (uint16_t)a >> n) ;
**	#	endif
**	}
**
*/
/*
 *  (From p. 46, end of section 4.2.5)
 *
 *  NOTE: The following lines gives [sic] one correct implementation
 *  	 of the div (num, denum) arithmetic operation.  Compute div
 *        which is the integer division of num by denum: with denum
 *	 >= num > 0
 */

int16_t gsm_div (int16_t num, int16_t denum)
{
	int32_t	L_num = num ;
	int32_t	L_denum = denum ;
	int16_t		div = 0 ;
	int			k = 15 ;

	/* The parameter num sometimes becomes zero.
	* Although this is explicitly guarded against in 4.2.5,
	* we assume that the result should then be zero as well.
	*/

	/* assert (num != 0) ; */

	assert (num >= 0 && denum >= num) ;
	if (num == 0)
		return 0 ;

	while (k--)
	{	div <<= 1 ;
		L_num <<= 1 ;

		if (L_num >= L_denum)
		{	L_num -= L_denum ;
			div++ ;
			}
		}

	return div ;
}

