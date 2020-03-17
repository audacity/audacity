/*
** Copyright (C) 2012 Chris Roberts <c.roberts@csrfm.com>
** Copyright (C) 2006-2013 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2006 Paul Davis <paul@linuxaudiosystems.com>
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
#include <stddef.h>
#include <string.h>
#include <stdlib.h>
#include "common.h"



static inline size_t
cart_min_size (const SF_CART_INFO* info)
{	if (info == NULL)
		return 0 ;

	return offsetof (SF_CART_INFO, tag_text) + info->tag_text_size ;
} /* cart_min_size */

SF_CART_INFO_16K*
cart_var_alloc (void)
{	SF_CART_INFO_16K* thing ;
	thing = malloc (sizeof (SF_CART_INFO_16K)) ;
	return thing ;
} /* cart_var_alloc */

int
cart_var_set (SF_PRIVATE *psf, const SF_CART_INFO * info, size_t datasize)
{	size_t len ;

	if (info == NULL)
		return SF_FALSE ;

	if (cart_min_size (info) > datasize)
	{	psf->error = SFE_BAD_CART_INFO_SIZE ;
		return SF_FALSE ;
		} ;

	if (datasize >= sizeof (SF_CART_INFO_16K))
	{	psf->error = SFE_BAD_CART_INFO_TOO_BIG ;
		return SF_FALSE ;
		} ;

	if (psf->cart_16k == NULL)
	{	if ((psf->cart_16k = cart_var_alloc ()) == NULL)
		{	psf->error = SFE_MALLOC_FAILED ;
			return SF_FALSE ;
			} ;
		} ;

	memcpy (psf->cart_16k, info, offsetof (SF_CART_INFO, tag_text)) ;
	psf_strlcpy_crlf (psf->cart_16k->tag_text, info->tag_text, sizeof (psf->cart_16k->tag_text), datasize - offsetof (SF_CART_INFO, tag_text)) ;

	len = strlen (psf->cart_16k->tag_text) ;

	if (len > 0 && psf->cart_16k->tag_text [len - 1] != '\n')
		psf_strlcat (psf->cart_16k->tag_text, sizeof (psf->cart_16k->tag_text), "\r\n") ;

	/* Force tag_text_size to be even. */
	len = strlen (psf->cart_16k->tag_text) ;
	len += (len & 1) ? 1 : 2 ;

	psf->cart_16k->tag_text_size = len ;

	return SF_TRUE ;
} /* cart_var_set */


int
cart_var_get (SF_PRIVATE *psf, SF_CART_INFO * data, size_t datasize)
{	size_t size ;
	if (psf->cart_16k == NULL)
		return SF_FALSE ;

	size = SF_MIN (datasize, cart_min_size ((const SF_CART_INFO *) psf->cart_16k)) ;

	memcpy (data, psf->cart_16k, size) ;

	return SF_TRUE ;
} /* cart_var_get */


