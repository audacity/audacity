/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */


#include	<stdlib.h>
#include	<string.h>

#include	"config.h"

#include	"gsm610_priv.h"
#include	"gsm.h"

/* 
 *  4.2 FIXED POINT IMPLEMENTATION OF THE RPE-LTP CODER 
 */

void Gsm_Coder (

	struct gsm_state	* State,

	word	* s,	/* [0..159] samples		  	IN	*/

/*
 * The RPE-LTD coder works on a frame by frame basis.  The length of
 * the frame is equal to 160 samples.  Some computations are done
 * once per frame to produce at the output of the coder the
 * LARc[1..8] parameters which are the coded LAR coefficients and 
 * also to realize the inverse filtering operation for the entire
 * frame (160 samples of signal d[0..159]).  These parts produce at
 * the output of the coder:
 */

	word	* LARc,	/* [0..7] LAR coefficients		OUT	*/

/*
 * Procedure 4.2.11 to 4.2.18 are to be executed four times per
 * frame.  That means once for each sub-segment RPE-LTP analysis of
 * 40 samples.  These parts produce at the output of the coder:
 */

	word	* Nc,	/* [0..3] LTP lag			OUT 	*/
	word	* bc,	/* [0..3] coded LTP gain		OUT 	*/
	word	* Mc,	/* [0..3] RPE grid selection		OUT     */
	word	* xmaxc,/* [0..3] Coded maximum amplitude	OUT	*/
	word	* xMc	/* [13*4] normalized RPE samples	OUT	*/
)
{
	int	k;
	word	* dp  = State->dp0 + 120;	/* [ -120...-1 ] */
	word	* dpp = dp;		/* [ 0...39 ]	 */

	word	so[160];

	Gsm_Preprocess			(State, s, so);
	Gsm_LPC_Analysis		(State, so, LARc);
	Gsm_Short_Term_Analysis_Filter	(State, LARc, so);

	for (k = 0; k <= 3; k++, xMc += 13) {

		Gsm_Long_Term_Predictor	( State,
					 so+k*40, /* d      [0..39] IN	*/
					 dp,	  /* dp  [-120..-1] IN	*/
					State->e + 5,	  /* e      [0..39] OUT	*/
					dpp,	  /* dpp    [0..39] OUT */
					 Nc++,
					 bc++);

		Gsm_RPE_Encoding	( /*-S,-*/
					State->e + 5,	/* e	  ][0..39][ IN/OUT */
					  xmaxc++, Mc++, xMc );
		/*
		 * Gsm_Update_of_reconstructed_short_time_residual_signal
		 *			( dpp, State->e + 5, dp );
		 */

		{ register int i;
		  for (i = 0; i <= 39; i++)
			dp[ i ] = GSM_ADD( State->e[5 + i], dpp[i] );
		}
		dp  += 40;
		dpp += 40;

	}
	(void)memcpy( (char *)State->dp0, (char *)(State->dp0 + 160),
		120 * sizeof(*State->dp0) );
}

