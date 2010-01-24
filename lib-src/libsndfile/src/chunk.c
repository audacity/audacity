/*
** Copyright (C) 2008-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include	<stdlib.h>
#include	<string.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"


void
pchk4_store (PRIV_CHUNK4 * pchk, int marker, sf_count_t offset, sf_count_t len)
{
	if (pchk->count >= ARRAY_LEN (pchk->l))
		return ;

	pchk->l [pchk->count].chunk = marker ;
	pchk->l [pchk->count].offset = offset ;
	pchk->l [pchk->count].len = len ;

	pchk->count ++ ;

	return ;
} /* pchk4_store */

int
pchk4_find (PRIV_CHUNK4 * pchk, int marker)
{	int k ;

	for (k = 0 ; k < pchk->count ; k++)
		if (pchk->l [k].chunk == marker)
			return k ;

	return -1 ;
} /* pchk4_find */

