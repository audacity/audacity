/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#ifndef	PRIVATE_H
#define	PRIVATE_H

/* Added by Erik de Castro Lopo */
#define	USE_FLOAT_MUL
#define	FAST
#define	WAV49

#ifdef __cplusplus
#error "This code is not designed to be compiled with a C++ compiler."
#endif
/* Added by Erik de Castro Lopo */



typedef short				word;		/* 16 bit signed int	*/
typedef int					longword;	/* 32 bit signed int	*/

typedef unsigned short		uword;		/* unsigned word	*/
typedef unsigned int		ulongword;	/* unsigned longword	*/

struct gsm_state
{	word			dp0[ 280 ] ;

	word			z1;			/* preprocessing.c, Offset_com. */
	longword		L_z2;		/*                  Offset_com. */
	int				mp;			/*                  Preemphasis	*/

	word			u[8] ;			/* short_term_aly_filter.c	*/
	word			LARpp[2][8] ; 	/*                              */
	word			j;				/*                              */

	word	        ltp_cut;        /* long_term.c, LTP crosscorr.  */
	word			nrp; 			/* 40 */	/* long_term.c, synthesis	*/
	word			v[9] ;			/* short_term.c, synthesis	*/
	word			msr;			/* decoder.c,	Postprocessing	*/

	char			verbose;		/* only used if !NDEBUG		*/
	char			fast;			/* only used if FAST		*/

	char			wav_fmt;		/* only used if WAV49 defined	*/
	unsigned char	frame_index;	/*            odd/even chaining	*/
	unsigned char	frame_chain;	/*   half-byte to carry forward	*/

	/* Moved here from code.c where it was defined as static */
	word e[50] ;
} ;

typedef struct gsm_state GSM_STATE ;

#define	MIN_WORD	(-32767 - 1)
#define	MAX_WORD	  32767

#define	MIN_LONGWORD	(-2147483647 - 1)
#define	MAX_LONGWORD	  2147483647

/* Signed arithmetic shift right. */
static inline word
SASR_W (word x, word by)
{	return (x >> by) ;
} /* SASR */

static inline longword
SASR_L (longword x, word by)
{	return (x >> by) ;
} /* SASR */

/*
 *	Prototypes from add.c
 */
word	gsm_mult 		(word a, word b) ;
longword gsm_L_mult 	(word a, word b) ;
word	gsm_mult_r		(word a, word b) ;

word	gsm_div  		(word num, word denum) ;

word	gsm_add 		(word a, word b ) ;
longword gsm_L_add 	(longword a, longword b ) ;

word	gsm_sub 		(word a, word b) ;
longword gsm_L_sub 	(longword a, longword b) ;

word	gsm_abs 		(word a) ;

word	gsm_norm 		(longword a ) ;

longword gsm_L_asl  	(longword a, int n) ;
word	gsm_asl 		(word a, int n) ;

longword gsm_L_asr  	(longword a, int n) ;
word	gsm_asr  		(word a, int n) ;

/*
 *  Inlined functions from add.h
 */

static inline longword
GSM_MULT_R (word a, word b)
{	return (((longword) (a)) * ((longword) (b)) + 16384) >> 15 ;
} /* GSM_MULT_R */

static inline longword
GSM_MULT (word a, word b)
{	return (((longword) (a)) * ((longword) (b))) >> 15 ;
} /* GSM_MULT */

static inline longword
GSM_L_MULT (word a, word b)
{	return ((longword) (a)) * ((longword) (b)) << 1 ;
} /* GSM_L_MULT */

static inline longword
GSM_L_ADD (longword a, longword b)
{	ulongword utmp ;

	if (a < 0 && b < 0)
	{	utmp = (ulongword)-((a) + 1) + (ulongword)-((b) + 1) ;
		return (utmp >= (ulongword) MAX_LONGWORD) ? MIN_LONGWORD : -(longword)utmp-2 ;
		} ;

	if (a > 0 && b > 0)
	{	utmp = (ulongword) a + (ulongword) b ;
		return (utmp >= (ulongword) MAX_LONGWORD) ? MAX_LONGWORD : utmp ;
		} ;

	return a + b ;
} /* GSM_L_ADD */

static inline longword
GSM_ADD (word a, word b)
{	longword ltmp ;

	ltmp = ((longword) a) + ((longword) b) ;

	if (ltmp >= MAX_WORD)
		return MAX_WORD ;
	if (ltmp <= MIN_WORD)
		return MIN_WORD ;

	return ltmp ;
} /* GSM_ADD */

static inline longword
GSM_SUB (word a, word b)
{	longword ltmp ;

	ltmp = ((longword) a) - ((longword) b) ;

	if (ltmp >= MAX_WORD)
		ltmp = MAX_WORD ;
	else if (ltmp <= MIN_WORD)
		ltmp = MIN_WORD ;

	return ltmp ;
} /* GSM_SUB */

static inline word
GSM_ABS (word a)
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
		word	* s,	/* [0..159] samples		IN	*/
		word	* LARc,	/* [0..7] LAR coefficients	OUT	*/
		word	* Nc,	/* [0..3] LTP lag		OUT 	*/
		word	* bc,	/* [0..3] coded LTP gain	OUT 	*/
		word	* Mc,	/* [0..3] RPE grid selection	OUT     */
		word	* xmaxc,/* [0..3] Coded maximum amplitude OUT	*/
		word	* xMc) ;/* [13*4] normalized RPE samples OUT	*/

void Gsm_Long_Term_Predictor (		/* 4x for 160 samples */
		struct gsm_state * S,
		word	* d,	/* [0..39]   residual signal	IN	*/
		word	* dp,	/* [-120..-1] d'		IN	*/
		word	* e,	/* [0..40] 			OUT	*/
		word	* dpp,	/* [0..40] 			OUT	*/
		word	* Nc,	/* correlation lag		OUT	*/
		word	* bc) ;	/* gain factor			OUT	*/

void Gsm_LPC_Analysis (
		struct gsm_state * S,
		word * s,		/* 0..159 signals	IN/OUT	*/
		word * LARc) ;   /* 0..7   LARc's	OUT	*/

void Gsm_Preprocess (
		struct gsm_state * S,
		word * s, word * so) ;

void Gsm_Encoding (
		struct gsm_state * S,
		word	* e,
		word	* ep,
		word	* xmaxc,
		word	* Mc,
		word	* xMc) ;

void Gsm_Short_Term_Analysis_Filter (
		struct gsm_state * S,
		word	* LARc,	/* coded log area ratio [0..7]  IN	*/
		word	* d) ;	/* st res. signal [0..159]	IN/OUT	*/

void Gsm_Decoder (
		struct gsm_state * S,
		word	* LARcr,	/* [0..7]		IN	*/
		word	* Ncr,		/* [0..3] 		IN 	*/
		word	* bcr,		/* [0..3]		IN	*/
		word	* Mcr,		/* [0..3] 		IN 	*/
		word	* xmaxcr,	/* [0..3]		IN 	*/
		word	* xMcr,		/* [0..13*4]		IN	*/
		word	* s) ;		/* [0..159]		OUT 	*/

void Gsm_Decoding (
		struct gsm_state * S,
		word 	xmaxcr,
		word	Mcr,
		word	* xMcr,  	/* [0..12]		IN	*/
		word	* erp) ; 	/* [0..39]		OUT 	*/

void Gsm_Long_Term_Synthesis_Filtering (
		struct gsm_state* S,
		word	Ncr,
		word	bcr,
		word	* erp,		/* [0..39]		  IN 	*/
		word	* drp) ; 	/* [-120..-1] IN, [0..40] OUT 	*/

void Gsm_RPE_Decoding (
	/*-struct gsm_state *S,-*/
		word xmaxcr,
		word Mcr,
		word * xMcr,  /* [0..12], 3 bits             IN      */
		word * erp) ; /* [0..39]                     OUT     */

void Gsm_RPE_Encoding (
		/*-struct gsm_state * S,-*/
		word    * e,            /* -5..-1][0..39][40..44     IN/OUT  */
		word    * xmaxc,        /*                              OUT */
		word    * Mc,           /*                              OUT */
		word    * xMc) ;        /* [0..12]                      OUT */

void Gsm_Short_Term_Synthesis_Filter (
		struct gsm_state * S,
		word	* LARcr, 	/* log area ratios [0..7]  IN	*/
		word	* drp,		/* received d [0...39]	   IN	*/
		word	* s) ;		/* signal   s [0..159]	  OUT	*/

void Gsm_Update_of_reconstructed_short_time_residual_signal (
		word	* dpp,		/* [0...39]	IN	*/
		word	* ep,		/* [0...39]	IN	*/
		word	* dp) ;		/* [-120...-1]  IN/OUT 	*/

/*
 *  Tables from table.c
 */
#ifndef	GSM_TABLE_C

extern word gsm_A [8], gsm_B [8], gsm_MIC [8], gsm_MAC [8] ;
extern word gsm_INVA [8] ;
extern word gsm_DLB [4], gsm_QLB [4] ;
extern word gsm_H [11] ;
extern word gsm_NRFAC [8] ;
extern word gsm_FAC [8] ;

#endif	/* GSM_TABLE_C */

/*
 *  Debugging
 */
#ifdef NDEBUG

#	define	gsm_debug_words(a, b, c, d)		/* nil */
#	define	gsm_debug_longwords(a, b, c, d)		/* nil */
#	define	gsm_debug_word(a, b)			/* nil */
#	define	gsm_debug_longword(a, b)		/* nil */

#else	/* !NDEBUG => DEBUG */

	void  gsm_debug_words     (char * name, int, int, word *) ;
	void  gsm_debug_longwords (char * name, int, int, longword *) ;
	void  gsm_debug_longword  (char * name, longword) ;
	void  gsm_debug_word      (char * name, word) ;

#endif /* !NDEBUG */

#endif	/* PRIVATE_H */

