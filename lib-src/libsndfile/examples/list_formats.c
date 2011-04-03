/*
** Copyright (C) 2001-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without
** modification, are permitted provided that the following conditions are
** met:
**
**     * Redistributions of source code must retain the above copyright
**       notice, this list of conditions and the following disclaimer.
**     * Redistributions in binary form must reproduce the above copyright
**       notice, this list of conditions and the following disclaimer in
**       the documentation and/or other materials provided with the
**       distribution.
**     * Neither the author nor the names of any contributors may be used
**       to endorse or promote products derived from this software without
**       specific prior written permission.
**
** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
** "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
** TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
** PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
** CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
** EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
** PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
** OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
** WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
** OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
** ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<string.h>
#include	<math.h>

#include	<sndfile.h>

int
main (void)
{	SF_FORMAT_INFO	info ;
	SF_INFO 		sfinfo ;
	char buffer [128] ;
	int format, major_count, subtype_count, m, s ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	buffer [0] = 0 ;
	sf_command (NULL, SFC_GET_LIB_VERSION, buffer, sizeof (buffer)) ;
	if (strlen (buffer) < 1)
	{	printf ("Line %d: could not retrieve lib version.\n", __LINE__) ;
		exit (1) ;
		} ;
	printf ("Version : %s\n\n", buffer) ;

	sf_command (NULL, SFC_GET_FORMAT_MAJOR_COUNT, &major_count, sizeof (int)) ;
	sf_command (NULL, SFC_GET_FORMAT_SUBTYPE_COUNT, &subtype_count, sizeof (int)) ;

	sfinfo.channels = 1 ;
	for (m = 0 ; m < major_count ; m++)
	{	info.format = m ;
		sf_command (NULL, SFC_GET_FORMAT_MAJOR, &info, sizeof (info)) ;
		printf ("%s  (extension \"%s\")\n", info.name, info.extension) ;

		format = info.format ;

		for (s = 0 ; s < subtype_count ; s++)
		{	info.format = s ;
			sf_command (NULL, SFC_GET_FORMAT_SUBTYPE, &info, sizeof (info)) ;

			format = (format & SF_FORMAT_TYPEMASK) | info.format ;

			sfinfo.format = format ;
			if (sf_format_check (&sfinfo))
				printf ("   %s\n", info.name) ;
			} ;
		puts ("") ;
		} ;
	puts ("") ;

	return 0 ;
} /* main */

