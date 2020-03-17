/*
** Copyright (C) 2009-2017 Erik de Castro Lopo <erikd@mega-nerd.com>
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

/*
**	This needs to be a separate file so that we don't have to include
**	<windows.h> elsewhere (too many symbol clashes).
*/


#include "sfconfig.h"

#if OS_IS_WIN32
#include <windows.h>

#define ENABLE_SNDFILE_WINDOWS_PROTOTYPES 1
#include "sndfile.h"
#include "common.h"

extern int sf_errno ;

static void copy_filename (SF_PRIVATE * psf, LPCWSTR wpath) ;

SNDFILE*
sf_wchar_open (LPCWSTR wpath, int mode, SF_INFO *sfinfo)
{	SF_PRIVATE 	*psf ;
	char utf8name [512] ;

	if ((psf = psf_allocate ()) == NULL)
	{	sf_errno = SFE_MALLOC_FAILED ;
		return	NULL ;
		} ;

	psf_init_files (psf) ;

	if (WideCharToMultiByte (CP_UTF8, 0, wpath, -1, utf8name, sizeof (utf8name), NULL, NULL) == 0)
		psf->file.path.wc [0] = 0 ;

	psf_log_printf (psf, "File : '%s' (utf-8 converted from ucs-2)\n", utf8name) ;

	copy_filename (psf, wpath) ;
	psf->file.use_wchar = SF_TRUE ;
	psf->file.mode = mode ;

	psf->error = psf_fopen (psf) ;

	return psf_open_file (psf, sfinfo) ;
} /* sf_wchar_open */


static void
copy_filename (SF_PRIVATE *psf, LPCWSTR wpath)
{	const wchar_t *cwcptr ;
	wchar_t *wcptr ;

	wcsncpy (psf->file.path.wc, wpath, ARRAY_LEN (psf->file.path.wc)) ;
	psf->file.path.wc [ARRAY_LEN (psf->file.path.wc) - 1] = 0 ;
	if ((cwcptr = wcsrchr (wpath, '/')) || (cwcptr = wcsrchr (wpath, '\\')))
		cwcptr ++ ;
	else
		cwcptr = wpath ;

	wcsncpy (psf->file.name.wc, cwcptr, ARRAY_LEN (psf->file.name.wc)) ;
	psf->file.name.wc [ARRAY_LEN (psf->file.name.wc) - 1] = 0 ;

	/* Now grab the directory. */
	wcsncpy (psf->file.dir.wc, wpath, ARRAY_LEN (psf->file.dir.wc)) ;
	psf->file.dir.wc [ARRAY_LEN (psf->file.dir.wc) - 1] = 0 ;

	if ((wcptr = wcsrchr (psf->file.dir.wc, '/')) || (wcptr = wcsrchr (psf->file.dir.wc, '\\')))
		wcptr [1] = 0 ;
	else
		psf->file.dir.wc [0] = 0 ;

	return ;
} /* copy_filename */

#endif
