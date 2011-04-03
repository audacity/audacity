/*
** Copyright (C) 2010-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	"sfconfig.h"

#include	<stdio.h>
#include	<fcntl.h>
#include	<string.h>
#include	<ctype.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

int
id3_skip (SF_PRIVATE * psf)
{	unsigned char	buf [10] ;

	memset (buf, 0, sizeof (buf)) ;
	psf_binheader_readf (psf, "pb", 0, buf, 10) ;

	if (buf [0] == 'I' && buf [1] == 'D' && buf [2] == '3')
	{	int	offset = buf [6] & 0x7f ;
		offset = (offset << 7) | (buf [7] & 0x7f) ;
		offset = (offset << 7) | (buf [8] & 0x7f) ;
		offset = (offset << 7) | (buf [9] & 0x7f) ;

		psf_binheader_readf (psf, "j", make_size_t (offset)) ;

		psf_log_printf (psf, "ID3 length : %d\n--------------------\n", offset) ;

		psf->fileoffset = 10 + offset ;
		return 1 ;
		} ;

	return 0 ;
} /* id3_skip */
