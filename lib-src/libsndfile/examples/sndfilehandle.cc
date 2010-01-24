/*
** Copyright (C) 2007 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include	<cstdio>
#include	<cstring>

#include	<sndfile.hh>

#define		BUFFER_LEN		1024

static void
create_file (const char * fname, int format)
{	static short buffer [BUFFER_LEN] ;

	SndfileHandle file ;
	int channels = 2 ;
	int srate = 48000 ;

	printf ("Creating file named '%s'\n", fname) ;

	file = SndfileHandle (fname, SFM_WRITE, format, channels, srate) ;

	memset (buffer, 0, sizeof (buffer)) ;

	file.write (buffer, BUFFER_LEN) ;

	puts ("") ;
	/*
	**	The SndfileHandle object will automatically close the file and
	**	release all allocated memory when the object goes out of scope.
	**	This is the Resource Acquisition Is Initailization idom.
	**	See : http://en.wikipedia.org/wiki/Resource_Acquisition_Is_Initialization
	*/
} /* create_file */

static void
read_file (const char * fname)
{	static short buffer [BUFFER_LEN] ;

	SndfileHandle file ;

	file = SndfileHandle (fname) ;

	printf ("Opened file '%s'\n", fname) ;
	printf ("    Sample rate : %d\n", file.samplerate ()) ;
	printf ("    Channels    : %d\n", file.channels ()) ;

	file.read (buffer, BUFFER_LEN) ;

	puts ("") ;

	/* RAII takes care of destroying SndfileHandle object. */
} /* read_file */

int
main (void)
{	const char * fname = "test.wav" ;

	puts ("\nSimple example showing usage of the C++ SndfileHandle object.\n") ;

	create_file (fname, SF_FORMAT_WAV | SF_FORMAT_PCM_16) ;

	read_file (fname) ;

	puts ("Done.\n") ;
	return 0 ;
} /* main */


