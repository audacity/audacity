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

#include "sfconfig.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "common.h"

#include "test_main.h"

#define	BCAST_MAX	512

typedef SF_BROADCAST_INFO_VAR (BCAST_MAX) SF_BROADCAST_INFO_512 ;

static void
fill_coding_history (SF_BROADCAST_INFO_512 * bi)
{	static const char *lines [] =
	{	"Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit.",
		"Donec dignissim erat\nvehicula libero condimentum\ndictum porta augue faucibus.",
		"Maecenas nec turpis\nsit amet quam\nfaucibus adipiscing.",
		"Mauris aliquam,\nlectus interdum\ntincidunt luctus.",
		"\n\n\n\n\n\n\n\n\n\n\n\n",
		"In auctor lorem\nvel est euismod\ncondimentum.",
		"\n\n\n\n\n\n\n\n\n\n\n\n",
		"Ut vitae magna\nid dui placerat vehicula\nin id lectus.",
		"\n\n\n\n\n\n\n\n\n\n\n\n",
		"Sed lacus leo,\nmolestie et luctus ac,\ntincidunt sit amet nisi.",
		"\n\n\n\n\n\n\n\n\n\n\n\n",
		"Sed ligula neque,\ngravida semper vulputate laoreet,\ngravida eu tellus.",
		"Donec dolor dolor,\nscelerisque in consequat ornare,\ntempor nec nisl."
	} ;
	int k ;

	bi->coding_history [0] = 0 ;

	for (k = 0 ; strlen (bi->coding_history) < bi->coding_history_size - 1 ; k ++)
		append_snprintf (bi->coding_history, bi->coding_history_size, "%s\n", lines [k % ARRAY_LEN (lines)]) ;

	return ;
} /* fill_coding_listory */

void
test_broadcast_var (void)
{	SF_PRIVATE	sf_private, *psf ;
	int k ;

	psf = &sf_private ;
	memset (psf, 0, sizeof (sf_private)) ;

	print_test_name ("Testing broadcast_var_set ") ;

	for (k = 64 ; k < BCAST_MAX ; k++)
	{
		SF_BROADCAST_INFO_512 bi ;

		memset (&bi, 0, sizeof (bi)) ;

		bi.coding_history_size = k ;
		fill_coding_history (&bi) ;
		bi.coding_history_size -- ;

		broadcast_var_set (psf, (SF_BROADCAST_INFO*) &bi, sizeof (bi)) ;
		} ;

	if (psf->broadcast_16k != NULL)
		free (psf->broadcast_16k) ;

	puts ("ok") ;
} /* test_broadcast_var */
