/*
 * Copyright 1992 by Jutta Degener and Carsten Bormann, Technische
 * Universitaet Berlin.  See the accompanying file "COPYRIGHT" for
 * details.  THERE IS ABSOLUTELY NO WARRANTY FOR THIS SOFTWARE.
 */

#include	"config.h"

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>



#include "gsm.h"
#include "gsm610_priv.h"

gsm gsm_create (void)
{
	gsm r ;

	r = malloc (sizeof (struct gsm_state)) ;
	if (!r) return r ;

	memset ((char *) r, 0, sizeof (struct gsm_state)) ;
	r->nrp = 40 ;

	return r ;
}

/* Added for libsndfile : May 6, 2002. Not sure if it works. */
void gsm_init (gsm state)
{
	memset (state, 0, sizeof (struct gsm_state)) ;
	state->nrp = 40 ;
}

