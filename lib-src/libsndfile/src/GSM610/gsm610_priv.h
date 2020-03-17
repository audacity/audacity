/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#ifndef	PRIVATE_H
#define	PRIVATE_H

#include <stdint.h>

/* Added by Erik de Castro Lopo */
#define	USE_FLOAT_MUL
#define	FAST
#define	WAV49

#ifdef __cplusplus
#error "This code is not designed to be compiled with a C++ compiler."
#endif
/* Added by Erik de Castro Lopo */

struct gsm_state
{	int16_t			dp0 [280] ;

	int16_t			z1 ;			/* preprocessing.c, Offset_com. */
	int32_t		L_z2 ;			/*                  Offset_com. */
	int				mp ;			/*                  Preemphasis	*/

	int16_t			u [8] ;			/* short_term_aly_filter.c	*/
	int16_t			LARpp [2][8] ; 	/*                              */
	int16_t			j ;				/*                              */

	int16_t			ltp_cut ;		/* long_term.c, LTP crosscorr.  */
	int16_t			nrp ; 			/* 40 */	/* long_term.c, synthesis	*/
	int16_t			v [9] ;			/* short_term.c, synthesis	*/
	int16_t			msr ;			/* decoder.c,	Postprocessing	*/

	char			verbose ;		/* only used if !NDEBUG		*/
	char			fast ;			/* only used if FAST		*/

	char			wav_fmt ;		/* only used if WAV49 defined	*/
	unsigned char	frame_index ;	/*            odd/even chaining	*/
	unsigned char	frame_chain ;	/*   half-byte to carry forward	*/

	/* Moved here from code.c where it was defined as static */
	int16_t e [50] ;
} ;

typedef struct gsm_state GSM_STATE ;

#define	MIN_WORD	(-32767 - 1)
#define	MAX_WORD	32767

#define	MIN_LONGWORD	(-2147483647 - 1)
#define	MAX_LONGWORD	2147483647

/* Signed arithmetic shift right. */
static inline int16_t
SASR_W (int16_t x, int16_t by)
{	if (x >= 0)
		return x >> by ;
	return ~ ((~x) >> by) ;
} /* SASR_W */

static inline int32_t
SASR_L (int32_t x, int16_t by)
{	if (x >= 0)
		return x >> by ;
	return ~ ((~x) >> by) ;
} /* SASR_L */

/* Signed arithmetic shift left. */
static inline int16_t
SASL_W (int16_t x, int16_t by)
{	if (x >= 0)
		return x << by ;
	return - ((-x) << by) ;
} /* SASR_W */

static inline int32_t
SASL_L (int32_t x, int16_t by)
{	if (x >= 0)
		return x << by ;
	return - ((-x) << by) ;
} /* SASR_L */

/*
 *	Prototypes from add.c
 */
int16_t	gsm_mult 		(int16_t a, int16_t b) ;
int32_t gsm_L_mult 	(int16_t a, int16_t b) ;
int16_t	gsm_mult_r		(int16_t a, int16_t b) ;

int16_t	gsm_div			(int16_t num, int16_t denum) ;

int16_t	gsm_add			(int16_t a, int16_t b) ;
int32_t gsm_L_add		(int32_t a, int32_t b) ;

int16_t	gsm_sub			(int16_t a, int16_t b) ;
int32_t gsm_L_sub		(int32_t a, int32_t b) ;

int16_t	gsm_abs			(int16_t a) ;

int16_t	gsm_norm		(int32_t a) ;

int32_t gsm_L_asl		(int32_t a, int n) ;
int16_t	gsm_asl			(int16_t a, int n) ;

int32_t gsm_L_asr		(int32_t a, int n) ;
int16_t	gsm_asr			(int16_t a, int n) ;

/*
 *  Inlined functions from add.h
 */

static inline int32_t
GSM_MULT_R (int16_t a, int16_t b)
{	return (((int32_t) (a)) * ((int32_t) (b)) + 16384) >> 15 ;
} /* GSM_MULT_R */

static inline int32_t
GSM_MULT (int16_t a, int16_t b)
{	return (((int32_t) (a)) * ((int32_t) (b))) >> 15 ;
} /* GSM_MULT */

static inline int32_t
GSM_L_MULT (int16_t a, int16_t b)
{	return ((int32_t) (a)) * ((int32_t) (b)) << 1 ;
} /* GSM_L_MULT */

static inline int32_t
GSM_L_ADD (int32_t a, int32_t b)
{	uint32_t utmp ;

	if (a < 0 && b < 0)
	{	utmp = (uint32_t) - ((a) + 1) + (uint32_t) - ((b) + 1) ;
		return (utmp >= (uint32_t) MAX_LONGWORD) ? MIN_LONGWORD : - (int32_t) utmp - 2 ;
		} ;

	if (a > 0 && b > 0)
	{	utmp = (uint32_t) a + (uint32_t) b ;
		return (utmp >= (uint32_t) MAX_LONGWORD) ? MAX_LONGWORD : utmp ;
		} ;

	return a + b ;
} /* GSM_L_ADD */

static inline int32_t
GSM_ADD (int16_t a, int16_t b)
{	int32_t ltmp ;

	ltmp = ((int32_t) a) + ((int32_t) b) ;

	if (ltmp >= MAX_WORD)
		return MAX_WORD ;
	if (ltmp <= MIN_WORD)
		return MIN_WORD ;

	return ltmp ;
} /* GSM_ADD */

static inline int32_t
GSM_SUB (int16_t a, int16_t b)
{	int32_t ltmp ;

	ltmp = ((int32_t) a) - ((int32_t) b) ;

	if (ltmp >= MAX_WORD)
		ltmp = MAX_WORD ;
	else if (ltmp <= MIN_WORD)
		ltmp = MIN_WORD ;

	return ltmp ;
} /* GSM_SUB */

static inline int16_t
GSM_ABS (int16_t a)
{
	if (a > 0)
		return a ;
	if (a == MIN_WORD)
		return MAX_WORD ;
	return -a ;
} /* GSM_ADD */


/*
 *  More prototypes from implementations..
 */
void Gsm_Coder (
		struct gsm_state	* S,
		int16_t	* s,		/* [0..159] samples		IN	*/
		int16_t	* LARc,		/* [0..7] LAR coefficients	OUT	*/
		int16_t	* Nc,		/* [0..3] LTP lag		OUT 	*/
		int16_t	* bc,		/* [0..3] coded LTP gain	OUT 	*/
		int16_t	* Mc,		/* [0..3] RPE grid selection	OUT     */
		int16_t	* xmaxc,	/* [0..3] Coded maximum amplitude OUT	*/
		int16_t	* xMc) ;	/* [13*4] normalized RPE samples OUT	*/

void Gsm_Long_Term_Predictor (		/* 4x for 160 samples */
		struct gsm_state * S,
		int16_t	* d,	/* [0..39]   residual signal	IN	*/
		int16_t	* dp,	/* [-120..-1] d'		IN	*/
		int16_t	* e,	/* [0..40] 			OUT	*/
		int16_t	* dpp,	/* [0..40] 			OUT	*/
		int16_t	* Nc,	/* correlation lag		OUT	*/
		int16_t	* bc) ;	/* gain factor			OUT	*/

void Gsm_LPC_Analysis (
		struct gsm_state * S,
		int16_t * s,		/* 0..159 signals	IN/OUT	*/
		int16_t * LARc) ;	/* 0..7   LARc's	OUT	*/

void Gsm_Preprocess (
		struct gsm_state * S,
		int16_t * s, int16_t * so) ;

void Gsm_Encoding (
		struct gsm_state * S,
		int16_t	* e,
		int16_t	* ep,
		int16_t	* xmaxc,
		int16_t	* Mc,
		int16_t	* xMc) ;

void Gsm_Short_Term_Analysis_Filter (
		struct gsm_state * S,
		int16_t	* LARc,	/* coded log area ratio [0..7]  IN	*/
		int16_t	* d) ;	/* st res. signal [0..159]	IN/OUT	*/

void Gsm_Decoder (
		struct gsm_state * S,
		int16_t	* LARcr,	/* [0..7]		IN	*/
		int16_t	* Ncr,		/* [0..3] 		IN 	*/
		int16_t	* bcr,		/* [0..3]		IN	*/
		int16_t	* Mcr,		/* [0..3] 		IN 	*/
		int16_t	* xmaxcr,	/* [0..3]		IN 	*/
		int16_t	* xMcr,		/* [0..13*4]		IN	*/
		int16_t	* s) ;		/* [0..159]		OUT 	*/

void Gsm_Decoding (
		struct gsm_state * S,
		int16_t 	xmaxcr,
		int16_t	Mcr,
		int16_t	* xMcr,		/* [0..12]		IN	*/
		int16_t	* erp) ;	/* [0..39]		OUT 	*/

void Gsm_Long_Term_Synthesis_Filtering (
		struct gsm_state* S,
		int16_t	Ncr,
		int16_t	bcr,
		int16_t	* erp,		/* [0..39]		  IN 	*/
		int16_t	* drp) ;	/* [-120..-1] IN, [0..40] OUT 	*/

void Gsm_RPE_Decoding (
	/*-struct gsm_state *S,-*/
		int16_t xmaxcr,
		int16_t Mcr,
		int16_t * xMcr,	/* [0..12], 3 bits             IN      */
		int16_t * erp) ;	/* [0..39]                     OUT     */

void Gsm_RPE_Encoding (
		/*-struct gsm_state * S,-*/
		int16_t	* e,			/* -5..-1][0..39][40..44     IN/OUT  */
		int16_t	* xmaxc,		/*                              OUT */
		int16_t	* Mc,			/*                              OUT */
		int16_t	* xMc) ;		/* [0..12]                      OUT */

void Gsm_Short_Term_Synthesis_Filter (
		struct gsm_state * S,
		int16_t	* LARcr, 	/* log area ratios [0..7]  IN	*/
		int16_t	* drp,		/* received d [0...39]	   IN	*/
		int16_t	* s) ;		/* signal   s [0..159]	  OUT	*/

void Gsm_Update_of_reconstructed_short_time_residual_signal (
		int16_t	* dpp,		/* [0...39]	IN	*/
		int16_t	* ep,		/* [0...39]	IN	*/
		int16_t	* dp) ;		/* [-120...-1]  IN/OUT 	*/

/*
 *  Tables from table.c
 */
#ifndef	GSM_TABLE_C

extern int16_t gsm_A [8], gsm_B [8], gsm_MIC [8], gsm_MAC [8] ;
extern int16_t gsm_INVA [8] ;
extern int16_t gsm_DLB [4], gsm_QLB [4] ;
extern int16_t gsm_H [11] ;
extern int16_t gsm_NRFAC [8] ;
extern int16_t gsm_FAC [8] ;

#endif	/* GSM_TABLE_C */


#if __GNUC__
#define ALWAYS_INLINE		__attribute__ ((always_inline))
#else
#define ALWAYS_INLINE
#endif


static inline int32_t ALWAYS_INLINE
arith_shift_left (int32_t x, int shift)
{	return (int32_t) (((uint32_t) x) << shift) ;
} /* arith_shift_left */

static inline int32_t ALWAYS_INLINE
arith_shift_right (int32_t x, int shift)
{	if (x >= 0)
		return x << shift ;
	return ~ ((~x) << shift) ;
} /* arith_shift_right */


/*
 *  Debugging
 */
#ifdef NDEBUG

#	define	gsm_debug_int16_ts(a, b, c, d)		/* nil */
#	define	gsm_debug_int32_ts(a, b, c, d)		/* nil */
#	define	gsm_debug_int16_t(a, b)			/* nil */
#	define	gsm_debug_int32_t(a, b)		/* nil */

#else	/* !NDEBUG => DEBUG */

	void gsm_debug_int16_ts		(char * name, int, int, int16_t *) ;
	void gsm_debug_int32_ts	(char * name, int, int, int32_t *) ;
	void gsm_debug_int32_t		(char * name, int32_t) ;
	void gsm_debug_int16_t			(char * name, int16_t) ;

#endif /* !NDEBUG */

#endif	/* PRIVATE_H */

