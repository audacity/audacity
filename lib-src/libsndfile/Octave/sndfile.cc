/*
** Copyright (C) 2007 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <octave/oct.h>

#include "sndfile.h"

#define FOUR_GIG 		(0x100000000LL)
#define	BUFFER_FRAMES	8192


static int format_of_str (const std::string & fmt) ;
static void string_of_format (std::string & fmt, int format) ;


DEFUN_DLD (sfversion, args, nargout ,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{version} =} sfversion ()\n\
@cindex Reading sound files\n\
Return a string containing the libsndfile version.\n\
@seealso{sfread, sfwrite}\n\
@end deftypefn")
{	char buffer [256] ;
	octave_value_list retval ;

	/* Bail out if the input parameters are bad. */
	if (args.length () != 0 || nargout > 1)
	{	print_usage () ;
		return retval ;
		} ;

	sf_command (NULL, SFC_GET_LIB_VERSION, buffer, sizeof (buffer)) ;

	std::string version (buffer) ;

	retval.append (version) ;
	return retval ;
} /* sfversion */


DEFUN_DLD (sfread, args, nargout ,
"-*- texinfo -*-\n\
@deftypefn {Loadable Function} {@var{data},@var{srate},@var{format} =} sfread (@var{filename})\n\
@cindex Reading sound files\n\
Read a sound file from disk using libsndfile.\n\
@seealso{sfversion, sfwrite}\n\
@end deftypefn")
{	SNDFILE * file ;
	SF_INFO sfinfo ;

	octave_value_list retval ;

	int nargin  = args.length () ;

	/* Bail out if the input parameters are bad. */
	if ((nargin != 1) || !args (0) .is_string () || nargout < 1 || nargout > 3)
	{	print_usage () ;
		return retval ;
		} ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	std::string filename = args (0).string_value () ;

	if ((file = sf_open (filename.c_str (), SFM_READ, &sfinfo)) == NULL)
	{	error ("sfread: couldn't open file %s : %s", filename.c_str (), sf_strerror (NULL)) ;
		return retval ;
		} ;

	if (sfinfo.frames > FOUR_GIG)
		printf ("This is a really huge file (%lld frames).\nYou may run out of memory trying to load it.\n", (long long) sfinfo.frames) ;

	dim_vector dim = dim_vector () ;
	dim.resize (2) ;
	dim (0) = sfinfo.frames ;
	dim (1) = sfinfo.channels ;

	/* Should I be using Matrix instead? */
	NDArray out (dim, 0.0) ;

	float buffer [BUFFER_FRAMES * sfinfo.channels] ;
	int readcount ;
	sf_count_t total = 0 ;

	do
	{	readcount = sf_readf_float (file, buffer, BUFFER_FRAMES) ;

		/* Make sure we don't read more frames than we allocated. */
		if (total + readcount > sfinfo.frames)
			readcount = sfinfo.frames - total ;

		for (int ch = 0 ; ch < sfinfo.channels ; ch++)
		{	for (int k = 0 ; k < readcount ; k++)
				out (total + k, ch) = buffer [k * sfinfo.channels + ch] ;
			} ;

		total += readcount ;
	} while (readcount > 0 && total < sfinfo.frames) ;

	retval.append (out.squeeze ()) ;

	if (nargout >= 2)
		retval.append ((octave_uint32) sfinfo.samplerate) ;

	if (nargout >= 3)
	{	std::string fmt ("") ;
		string_of_format (fmt, sfinfo.format) ;
		retval.append (fmt) ;
		} ;

	/* Clean up. */
	sf_close (file) ;

	return retval ;
} /* sfread */

DEFUN_DLD (sfwrite, args, nargout ,
"-*- texinfo -*-\n\
@deftypefn {Function File} sfwrite (@var{filename},@var{data},@var{srate},@var{format})\n\
Write a sound file to disk using libsndfile.\n\
@seealso{sfread, sfversion}\n\
@end deftypefn\n\
")
{	SNDFILE * file ;
	SF_INFO sfinfo ;

    octave_value_list retval ;

    int nargin  = args.length () ;

    /* Bail out if the input parameters are bad. */
    if (nargin != 4 || !args (0).is_string () || !args (1).is_real_matrix ()
			|| !args (2).is_real_scalar () || !args (3).is_string ()
			|| nargout != 0)
	{	print_usage () ;
		return retval ;
    	} ;

    std::string filename = args (0).string_value () ;
    std::string format = args (3).string_value () ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;

	sfinfo.format = format_of_str (format) ;
	if (sfinfo.format == 0)
	{	error ("Bad format '%s'", format.c_str ()) ;
		return retval ;
		} ;

	sfinfo.samplerate = lrint (args (2).scalar_value ()) ;
	if (sfinfo.samplerate < 1)
	{	error ("Bad sample rate : %d.\n", sfinfo.samplerate) ;
		return retval ;
		} ;

	Matrix data = args (1).matrix_value () ;
	long rows = args (1).rows () ;
	long cols = args (1).columns () ;

	if (cols > rows)
	{	error ("Audio data should have one column per channel, but supplied data "
				"has %ld rows and %ld columns.\n", rows, cols) ;
		return retval ;
		} ;

	sfinfo.channels = cols ;

    if ((file = sf_open (filename.c_str (), SFM_WRITE, &sfinfo)) == NULL)
	{	error ("Couldn't open file %s : %s", filename.c_str (), sf_strerror (NULL)) ;
		return retval ;
    	} ;

	float buffer [BUFFER_FRAMES * sfinfo.channels] ;
	int writecount ;
	long total = 0 ;

	do
	{
		writecount = BUFFER_FRAMES ;

		/* Make sure we don't read more frames than we allocated. */
		if (total + writecount > rows)
			writecount = rows - total ;

		for (int ch = 0 ; ch < sfinfo.channels ; ch++)
		{	for (int k = 0 ; k < writecount ; k++)
				buffer [k * sfinfo.channels + ch] = data (total + k, ch) ;
			} ;

		if (writecount > 0)
			sf_writef_float (file, buffer, writecount) ;

		total += writecount ;
	} while (writecount > 0 && total < rows) ;

    /* Clean up. */
    sf_close (file) ;

    return retval ;
} /* sfwrite */


static void
str_split (const std::string & str, const std::string & delim, std::vector <std::string> & output)
{
    unsigned int offset = 0 ;
    size_t delim_index = 0 ;

    delim_index = str.find (delim, offset) ;

    while (delim_index != std::string::npos)
    {
        output.push_back (str.substr(offset, delim_index - offset)) ;
        offset += delim_index - offset + delim.length () ;
        delim_index = str.find (delim, offset) ;
    }

    output.push_back (str.substr (offset)) ;
} /* str_split */

static int
hash_of_str (const std::string & str)
{
	int hash = 0 ;

	for (unsigned k = 0 ; k < str.length () ; k++)
		hash = (hash * 3) + tolower (str [k]) ;

	return hash ;
} /* hash_of_str */

static int
major_format_of_hash (const std::string & str)
{	int hash ;

	hash = hash_of_str (str) ;

	switch (hash)
	{
		case 0x5c8 : /* 'wav' */ return SF_FORMAT_WAV ;
		case 0xf84 : /* 'aiff' */ return SF_FORMAT_AIFF ;
		case 0x198 : /* 'au' */ return SF_FORMAT_AU ;
		case 0x579 : /* 'paf' */ return SF_FORMAT_PAF ;
		case 0x5e5 : /* 'svx' */ return SF_FORMAT_SVX ;
		case 0x1118 : /* 'nist' */ return SF_FORMAT_NIST ;
		case 0x5d6 : /* 'voc' */ return SF_FORMAT_VOC ;
		case 0x324a : /* 'ircam' */ return SF_FORMAT_IRCAM ;
		case 0x505 : /* 'w64' */ return SF_FORMAT_W64 ;
		case 0x1078 : /* 'mat4' */ return SF_FORMAT_MAT4 ;
		case 0x1079 : /* 'mat5' */ return SF_FORMAT_MAT5 ;
		case 0x5b8 : /* 'pvf' */ return SF_FORMAT_PVF ;
		case 0x1d1 : /* 'xi' */ return SF_FORMAT_XI ;
		case 0x56f : /* 'htk' */ return SF_FORMAT_HTK ;
		case 0x5aa : /* 'sds' */ return SF_FORMAT_SDS ;
		case 0x53d : /* 'avr' */ return SF_FORMAT_AVR ;
		case 0x11d0 : /* 'wavx' */ return SF_FORMAT_WAVEX ;
		case 0x569 : /* 'sd2' */ return SF_FORMAT_SD2 ;
		case 0x1014 : /* 'flac' */ return SF_FORMAT_FLAC ;
		case 0x504 : /* 'caf' */ return SF_FORMAT_CAF ;
		case 0x5f6 : /* 'wve' */ return SF_FORMAT_WVE ;
		default : break ;
		} ;

	printf ("%s : hash '%s' -> 0x%x\n", __func__, str.c_str (), hash) ;

	return 0 ;
} /* major_format_of_hash */

static int
minor_format_of_hash (const std::string & str)
{	int hash ;

	hash = hash_of_str (str) ;

	switch (hash)
	{
		case 0x1085 : /* 'int8' */ return SF_FORMAT_PCM_S8 ;
		case 0x358a : /* 'uint8' */ return SF_FORMAT_PCM_U8 ;
		case 0x31b0 : /* 'int16' */ return SF_FORMAT_PCM_16 ;
		case 0x31b1 : /* 'int24' */ return SF_FORMAT_PCM_24 ;
		case 0x31b2 : /* 'int32' */ return SF_FORMAT_PCM_32 ;
		case 0x3128 : /* 'float' */ return SF_FORMAT_FLOAT ;
		case 0x937d : /* 'double' */ return SF_FORMAT_DOUBLE ;
		case 0x11bd : /* 'ulaw' */ return SF_FORMAT_ULAW ;
		case 0xfa1 : /* 'alaw' */ return SF_FORMAT_ALAW ;
		case 0xfc361 : /* 'ima_adpcm' */ return SF_FORMAT_IMA_ADPCM ;
		case 0x5739a : /* 'ms_adpcm' */ return SF_FORMAT_MS_ADPCM ;
		case 0x9450 : /* 'gsm610' */ return SF_FORMAT_GSM610 ;
		case 0x172a3 : /* 'g721_32' */ return SF_FORMAT_G721_32 ;
		case 0x172d8 : /* 'g723_24' */ return SF_FORMAT_G723_24 ;
		case 0x172da : /* 'g723_40' */ return SF_FORMAT_G723_40 ;
		default : break ;
		} ;

	printf ("%s : hash '%s' -> 0x%x\n", __func__, str.c_str (), hash) ;

	return 0 ;
} /* minor_format_of_hash */


static const char *
string_of_major_format (int format)
{
	switch (format & SF_FORMAT_TYPEMASK)
	{
		case SF_FORMAT_WAV : return "wav" ;
		case SF_FORMAT_AIFF : return "aiff" ;
		case SF_FORMAT_AU : return "au" ;
		case SF_FORMAT_PAF : return "paf" ;
		case SF_FORMAT_SVX : return "svx" ;
		case SF_FORMAT_NIST : return "nist" ;
		case SF_FORMAT_VOC : return "voc" ;
		case SF_FORMAT_IRCAM : return "ircam" ;
		case SF_FORMAT_W64 : return "w64" ;
		case SF_FORMAT_MAT4 : return "mat4" ;
		case SF_FORMAT_MAT5 : return "mat5" ;
		case SF_FORMAT_PVF : return "pvf" ;
		case SF_FORMAT_XI : return "xi" ;
		case SF_FORMAT_HTK : return "htk" ;
		case SF_FORMAT_SDS : return "sds" ;
		case SF_FORMAT_AVR : return "avr" ;
		case SF_FORMAT_WAVEX : return "wavx" ;
		case SF_FORMAT_SD2 : return "sd2" ;
		case SF_FORMAT_FLAC : return "flac" ;
		case SF_FORMAT_CAF : return "caf" ;
		case SF_FORMAT_WVE : return "wfe" ;
		default : break ;
		} ;

	return "unknown" ;
} /* string_of_major_format */

static const char *
string_of_minor_format (int format)
{
	switch (format & SF_FORMAT_SUBMASK)
	{
		case SF_FORMAT_PCM_S8 : return "int8" ;
		case SF_FORMAT_PCM_U8 : return "uint8" ;
		case SF_FORMAT_PCM_16 : return "int16" ;
		case SF_FORMAT_PCM_24 : return "int24" ;
		case SF_FORMAT_PCM_32 : return "int32" ;
		case SF_FORMAT_FLOAT : return "float" ;
		case SF_FORMAT_DOUBLE : return "double" ;
		case SF_FORMAT_ULAW : return "ulaw" ;
		case SF_FORMAT_ALAW : return "alaw" ;
		case SF_FORMAT_IMA_ADPCM : return "ima_adpcm" ;
		case SF_FORMAT_MS_ADPCM : return "ms_adpcm" ;
		case SF_FORMAT_GSM610 : return "gsm610" ;
		case SF_FORMAT_G721_32 : return "g721_32" ;
		case SF_FORMAT_G723_24 : return "g723_24" ;
		case SF_FORMAT_G723_40 : return "g723_40" ;
		default : break ;
		} ;
	
	return "unknown" ;
} /* string_of_minor_format */

static int
format_of_str (const std::string & fmt)
{
	std::vector <std::string> split ;

	str_split (fmt, "-", split) ;

	if (split.size () != 2)
		return 0 ;

	int major_fmt = major_format_of_hash (split.at (0)) ;
	if (major_fmt == 0)
		return 0 ;

	int minor_fmt = minor_format_of_hash (split.at (1)) ;
	if (minor_fmt == 0)
		return 0 ;

	return major_fmt | minor_fmt ;
} /* format_of_str */

static void
string_of_format (std::string & fmt, int format)
{
	char buffer [64] ;

	snprintf (buffer, sizeof (buffer), "%s-%s", string_of_major_format (format), string_of_minor_format (format)) ;

	fmt = buffer ;

	return ;
} /* string_of_format */
