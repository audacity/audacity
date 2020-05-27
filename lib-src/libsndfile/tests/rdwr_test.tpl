[+ AutoGen5 template c +]
/*
** Copyright (C) 2010-2012 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation ; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/stat.h>
#include <math.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if (HAVE_DECL_S_IRGRP == 0)
#include <sf_unistd.h>
#endif

#if (defined (WIN32) || defined (_WIN32))
#include <io.h>
#include <direct.h>
#endif

#include	<sndfile.h>

#include	"utils.h"

[+ FOR data_type
+]static void	rdwr_[+ (get "name") +]_test	(const char *filename) ;
[+ ENDFOR data_type
+]

int
main (void)
{
	rdwr_short_test ("rdwr_short.wav") ;
	rdwr_int_test ("rdwr_int.wav") ;
	rdwr_float_test ("rdwr_float.wav") ;
	rdwr_double_test ("rdwr_double.wav") ;
	rdwr_raw_test ("rdwr_raw.wav") ;

	return 0 ;
} /* main */


/*============================================================================================
**	Here are the test functions.
*/

[+ FOR data_type
+]static void
rdwr_[+ (get "name") +]_test	(const char *filename)
{	SNDFILE *file ;
	SF_INFO sfinfo ;
	sf_count_t frames ;
	[+ (get "type") +] buffer [160] ;

	print_test_name ("rdwr_[+ (get "name") +]_test", filename) ;

	memset (buffer, 0, sizeof (buffer)) ;

	/* Create sound file with no data. */
	sfinfo.format = SF_FORMAT_WAV | [+ (get "format") +] ;
	sfinfo.samplerate = 16000 ;
	sfinfo.channels = 1 ;

	unlink (filename) ;

	frames = ARRAY_LEN (buffer) ;

	/* Open again for read/write. */
	file = test_open_file_or_die (filename, SFM_RDWR, &sfinfo, SF_TRUE, __LINE__) ;

	test_write_[+ (get "name") +]_or_die (file, 0, buffer, frames, __LINE__) ;

	test_read_[+ (get "name") +]_or_die (file, 0, buffer, frames, __LINE__) ;

	sf_close (file) ;
	unlink (filename) ;

	puts ("ok") ;
	return ;
} /* rdwr_[+ (get "name") +]_test */

[+ ENDFOR data_type
+]

