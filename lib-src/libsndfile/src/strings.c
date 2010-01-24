/*
** Copyright (C) 2001-2009 Erik de Castro Lopo <erikd@mega-nerd.com>
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
#include	<string.h>
#include	<math.h>

#include	"sndfile.h"
#include	"common.h"

#define STRINGS_DEBUG 0
#if STRINGS_DEBUG
static void hexdump (void *data, int len) ;
#endif

int
psf_store_string (SF_PRIVATE *psf, int str_type, const char *str)
{	static char lsf_name [] = PACKAGE "-" VERSION ;
	static char bracket_name [] = " (" PACKAGE "-" VERSION ")" ;
	int		k, str_len, len_remaining, str_flags, str_type_replace = 0 ;

	if (str == NULL)
		return SFE_STR_BAD_STRING ;

	str_len = strlen (str) ;

	/* A few extra checks for write mode. */
	if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
	{	if ((psf->str_flags & SF_STR_ALLOW_START) == 0)
			return SFE_STR_NO_SUPPORT ;
		if (psf->have_written && (psf->str_flags & SF_STR_ALLOW_END) == 0)
			return SFE_STR_NO_SUPPORT ;
		/* Only allow zero length strings for software. */
		if (str_type != SF_STR_SOFTWARE && str_len == 0)
			return SFE_STR_BAD_STRING ;
		} ;

	/* Find the next free slot in table. */
	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
	{	/* If we find a matching entry clear it. */
		if (psf->strings [k].type == str_type)
			psf->strings [k].type = -1 ;

		if (psf->strings [k].type == 0)
			break ;
		} ;

	/* Determine flags */
	str_flags = SF_STR_LOCATE_START ;
	if (psf->mode == SFM_RDWR || psf->have_written || str_type_replace)
	{	if ((psf->str_flags & SF_STR_ALLOW_END) == 0)
			return SFE_STR_NO_ADD_END ;
		str_flags = SF_STR_LOCATE_END ;
		} ;

	/* More sanity checking. */
	if (k >= SF_MAX_STRINGS)
		return SFE_STR_MAX_COUNT ;

	if (k == 0 && psf->str_end != NULL)
	{	psf_log_printf (psf, "SFE_STR_WEIRD : k == 0 && psf->str_end != NULL\n") ;
		return SFE_STR_WEIRD ;
		} ;

	if (k != 0 && psf->str_end == NULL)
	{	psf_log_printf (psf, "SFE_STR_WEIRD : k != 0 && psf->str_end == NULL\n") ;
		return SFE_STR_WEIRD ;
		} ;

	/* Special case for the first string. */
	if (k == 0)
		psf->str_end = psf->str_storage ;

	len_remaining = SIGNED_SIZEOF (psf->str_storage) - (psf->str_end - psf->str_storage) ;

	if (len_remaining < str_len + 2)
		return SFE_STR_MAX_DATA ;

	switch (str_type)
	{	case SF_STR_SOFTWARE :
				/* In write mode, want to append libsndfile-version to string. */
				if (psf->mode == SFM_WRITE || psf->mode == SFM_RDWR)
				{	psf->strings [k].type = str_type ;
					psf->strings [k].str = psf->str_end ;
					psf->strings [k].flags = str_flags ;

					memcpy (psf->str_end, str, str_len + 1) ;
					psf->str_end += str_len ;

					/*
					** If the supplied string does not already contain a
					** libsndfile-X.Y.Z component, then add it.
					*/
					if (strstr (str, PACKAGE) == NULL && len_remaining > (int) (strlen (bracket_name) + str_len + 2))
					{	if (strlen (str) == 0)
							strncat (psf->str_end, lsf_name, len_remaining) ;
						else
							strncat (psf->str_end, bracket_name, len_remaining) ;
						psf->str_end += strlen (psf->str_end) ;
						} ;

					/* Plus one to catch string terminator. */
					psf->str_end += 1 ;
					break ;
					} ;

				/* Fall though if not write mode. */

		case SF_STR_TITLE :
		case SF_STR_COPYRIGHT :
		case SF_STR_ARTIST :
		case SF_STR_COMMENT :
		case SF_STR_DATE :
		case SF_STR_ALBUM :
		case SF_STR_LICENSE :
				psf->strings [k].type = str_type ;
				psf->strings [k].str = psf->str_end ;
				psf->strings [k].flags = str_flags ;

				/* Plus one to catch string terminator. */
				memcpy (psf->str_end, str, str_len + 1) ;
				psf->str_end += str_len + 1 ;
				break ;

		default :
			psf_log_printf (psf, "%s : SFE_STR_BAD_TYPE\n", __func__) ;
			return SFE_STR_BAD_TYPE ;
		} ;

	psf->str_flags |= str_flags ;

#if STRINGS_DEBUG
	psf_log_printf (psf, "str_storage          : %X\n", (int) psf->str_storage) ;
	psf_log_printf (psf, "str_end              : %X\n", (int) psf->str_end) ;
	psf_log_printf (psf, "sizeof (str_storage) : %d\n", SIGNED_SIZEOF (psf->str_storage)) ;
	psf_log_printf (psf, "used                 : %d\n", (int ) (psf->str_end - psf->str_storage)) ;
	psf_log_printf (psf, "remaining            : %d\n", SIGNED_SIZEOF (psf->str_storage) - (psf->str_end - psf->str_storage)) ;

	hexdump (psf->str_storage, 300) ;
#endif

	return 0 ;
} /* psf_store_string */

int
psf_set_string (SF_PRIVATE *psf, int str_type, const char *str)
{	if (psf->mode == SFM_READ)
		return SFE_STR_NOT_WRITE ;

	return psf_store_string (psf, str_type, str) ;
} /* psf_set_string */

const char*
psf_get_string (SF_PRIVATE *psf, int str_type)
{	int k ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
		if (str_type == psf->strings [k].type)
			return psf->strings [k].str ;

	return NULL ;
} /* psf_get_string */

int
psf_location_string_count (const SF_PRIVATE * psf, int location)
{	int k, count = 0 ;

	for (k = 0 ; k < SF_MAX_STRINGS ; k++)
		if (psf->strings [k].type > 0 && psf->strings [k].flags & location)
			count ++ ;

	return count ;
} /* psf_location_string_count */

/*==============================================================================
*/

#if STRINGS_DEBUG

#include <ctype.h>
static void
hexdump (void *data, int len)
{	unsigned char *ptr ;
	int k ;

	ptr = data ;

	puts ("---------------------------------------------------------") ;
	while (len >= 16)
	{	for (k = 0 ; k < 16 ; k++)
			printf ("%02X ", ptr [k] & 0xFF) ;
		printf ("   ") ;
		for (k = 0 ; k < 16 ; k++)
			printf ("%c", isprint (ptr [k]) ? ptr [k] : '.') ;
		puts ("") ;
		ptr += 16 ;
		len -= 16 ;
		} ;
} /* hexdump */

#endif
