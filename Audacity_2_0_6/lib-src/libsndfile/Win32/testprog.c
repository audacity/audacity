/*	Simple test program to make sure that Win32 linking to libsndfile is
**	working.
*/

#include <stdio.h>

#include "sndfile.h"

int
main (void)
{	static char strbuffer [256] ;
	sf_command (NULL, SFC_GET_LIB_VERSION, strbuffer, sizeof (strbuffer)) ;
	puts (strbuffer) ;
	return 0 ;
}

