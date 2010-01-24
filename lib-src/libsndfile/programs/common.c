/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2008 George Blood Audio
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <sndfile.h>

#include "common.h"

#define	 BUFFER_LEN	4096

#define	MIN(x,y)	((x) < (y) ? (x) : (y))

void
sfe_copy_data_fp (SNDFILE *outfile, SNDFILE *infile, int channels)
{	static double	data [BUFFER_LEN], max ;
	int		frames, readcount, k ;

	frames = BUFFER_LEN / channels ;
	readcount = frames ;

	sf_command (infile, SFC_CALC_SIGNAL_MAX, &max, sizeof (max)) ;

	if (max < 1.0)
	{	while (readcount > 0)
		{	readcount = sf_readf_double (infile, data, frames) ;
			sf_writef_double (outfile, data, readcount) ;
			} ;
		}
	else
	{	sf_command (infile, SFC_SET_NORM_DOUBLE, NULL, SF_FALSE) ;

		while (readcount > 0)
		{	readcount = sf_readf_double (infile, data, frames) ;
			for (k = 0 ; k < readcount * channels ; k++)
				data [k] /= max ;
			sf_writef_double (outfile, data, readcount) ;
			} ;
		} ;

	return ;
} /* sfe_copy_data_fp */

void
sfe_copy_data_int (SNDFILE *outfile, SNDFILE *infile, int channels)
{	static int	data [BUFFER_LEN] ;
	int		frames, readcount ;

	frames = BUFFER_LEN / channels ;
	readcount = frames ;

	while (readcount > 0)
	{	readcount = sf_readf_int (infile, data, frames) ;
		sf_writef_int (outfile, data, readcount) ;
		} ;

	return ;
} /* sfe_copy_data_int */

/*==============================================================================
*/

static int
merge_broadcast_info (SNDFILE * infile, SNDFILE * outfile, int format, const METADATA_INFO * info)
{	SF_BROADCAST_INFO_2K binfo ;
	int infileminor ;

	memset (&binfo, 0, sizeof (binfo)) ;

	if ((SF_FORMAT_TYPEMASK & format) != SF_FORMAT_WAV)
	{	printf ("Error : This is not a WAV file and hence broadcast info cannot be added to it.\n\n") ;
		return 1 ;
		} ;

	infileminor = SF_FORMAT_SUBMASK & format ;

	switch (infileminor)
	{	case SF_FORMAT_PCM_16 :
		case SF_FORMAT_PCM_24 :
		case SF_FORMAT_PCM_32 :
			break ;

		default :
			printf (
				"Warning : The EBU Technical Recommendation R68-2000 states that the only\n"
				"          allowed encodings are Linear PCM and MPEG3. This file is not in\n"
				"          the right format.\n\n"
				) ;
			break ;
		} ;

	if (sf_command (infile, SFC_GET_BROADCAST_INFO, &binfo, sizeof (binfo)) == 0)
	{	if (infile == outfile)
		{	printf (
				"Error : Attempting in-place broadcast info update, but file does not\n"
				"        have a 'bext' chunk to modify. The solution is to specify both\n"
				"        input and output files on the command line.\n\n"
				) ;
			return 1 ;
			} ;
		} ;

#define REPLACE_IF_NEW(x) \
		if (info->x != NULL) \
		{	memset (binfo.x, 0, sizeof (binfo.x)) ; \
			memcpy (binfo.x, info->x, MIN (strlen (info->x), sizeof (binfo.x))) ; \
			} ;

	REPLACE_IF_NEW (description) ;
	REPLACE_IF_NEW (originator) ;
	REPLACE_IF_NEW (originator_reference) ;
	REPLACE_IF_NEW (origination_date) ;
	REPLACE_IF_NEW (origination_time) ;
	REPLACE_IF_NEW (umid) ;

	/* Special case for coding_history because we may want to append. */
	if (info->coding_history != NULL)
	{	if (info->coding_hist_append)
		{	int slen = strlen (binfo.coding_history) ;

			while (slen > 1 && isspace (binfo.coding_history [slen - 1]))
				slen -- ;

			memcpy (binfo.coding_history + slen, info->coding_history, sizeof (binfo.coding_history) - slen) ;
			}
		else
		{	size_t slen = MIN (strlen (info->coding_history), sizeof (binfo.coding_history)) ;

			memset (binfo.coding_history, 0, sizeof (binfo.coding_history)) ;
			memcpy (binfo.coding_history, info->coding_history, slen) ;
			binfo.coding_history_size = slen ;
			} ;
		} ;

	if (sf_command (outfile, SFC_SET_BROADCAST_INFO, &binfo, sizeof (binfo)) == 0)
	{	printf ("Error : Setting of broadcast info chunks failed.\n\n") ;
		return 1 ;
		} ;

	return 0 ;
} /* merge_broadcast_info*/

static void
update_strings (SNDFILE * outfile, const METADATA_INFO * info)
{
	if (info->title != NULL)
		sf_set_string (outfile, SF_STR_TITLE, info->title) ;

	if (info->copyright != NULL)
		sf_set_string (outfile, SF_STR_TITLE, info->copyright) ;

	if (info->artist != NULL)
		sf_set_string (outfile, SF_STR_ARTIST, info->artist) ;

	if (info->comment != NULL)
		sf_set_string (outfile, SF_STR_TITLE, info->comment) ;

	if (info->date != NULL)
		sf_set_string (outfile, SF_STR_DATE, info->date) ;

	if (info->album != NULL)
		sf_set_string (outfile, SF_STR_TITLE, info->album) ;

	if (info->license != NULL)
		sf_set_string (outfile, SF_STR_TITLE, info->license) ;

} /* update_strings */



void
sfe_apply_metadata_changes (const char * filenames [2], const METADATA_INFO * info)
{	SNDFILE *infile = NULL, *outfile = NULL ;
	SF_INFO sfinfo ;
	METADATA_INFO tmpinfo ;
	int error_code = 0 ;

	memset (&sfinfo, 0, sizeof (sfinfo)) ;
	memset (&tmpinfo, 0, sizeof (tmpinfo)) ;

	if (filenames [1] == NULL)
		infile = outfile = sf_open (filenames [0], SFM_RDWR, &sfinfo) ;
	else
	{	infile = sf_open (filenames [0], SFM_READ, &sfinfo) ;

		/* Output must be WAV. */
		sfinfo.format = SF_FORMAT_WAV | (SF_FORMAT_SUBMASK & sfinfo.format) ;
		outfile = sf_open (filenames [1], SFM_WRITE, &sfinfo) ;
		} ;

	if (infile == NULL)
	{	printf ("Error : Not able to open input file '%s' : %s\n", filenames [0], sf_strerror (infile)) ;
		error_code = 1 ;
		goto cleanup_exit ;
		} ;

	if (outfile == NULL)
	{	printf ("Error : Not able to open output file '%s' : %s\n", filenames [1], sf_strerror (outfile)) ;
		error_code = 1 ;
		goto cleanup_exit ;
		} ;

	if (info->has_bext_fields && merge_broadcast_info (infile, outfile, sfinfo.format, info))
	{	error_code = 1 ;
		goto cleanup_exit ;
		} ;

	update_strings (outfile, info) ;

	if (infile != outfile)
	{	int infileminor = SF_FORMAT_SUBMASK & sfinfo.format ;

		/* If the input file is not the same as the output file, copy the data. */
		if ((infileminor == SF_FORMAT_DOUBLE) || (infileminor == SF_FORMAT_FLOAT))
			sfe_copy_data_fp (outfile, infile, sfinfo.channels) ;
		else
			sfe_copy_data_int (outfile, infile, sfinfo.channels) ;
		} ;

cleanup_exit :

	if (outfile != NULL && outfile != infile)
		sf_close (outfile) ;

	if (infile != NULL)
		sf_close (infile) ;

	if (error_code)
		exit (error_code) ;

	return ;
} /* sfe_apply_metadata_changes */

