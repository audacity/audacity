/*
** Copyright (C) 1999-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include <sndfile.h>

typedef struct
{	const char * title ;
	const char * copyright ;
	const char * artist ;
	const char * comment ;
	const char * date ;
	const char * album ;
	const char * license ;


	/* Stuff to go in the 'bext' chunk of WAV files. */
	int has_bext_fields ;
	int coding_hist_append ;

	const char * description ;
	const char * originator ;
	const char * originator_reference ;
	const char * origination_date ;
	const char * origination_time ;
	const char * umid ;
	const char * coding_history ;
} METADATA_INFO ;

typedef SF_BROADCAST_INFO_VAR (2048) SF_BROADCAST_INFO_2K ;

void sfe_apply_metadata_changes (const char * filenames [2], const METADATA_INFO * info) ;

void sfe_copy_data_fp (SNDFILE *outfile, SNDFILE *infile, int channels) ;

void sfe_copy_data_int (SNDFILE *outfile, SNDFILE *infile, int channels) ;
