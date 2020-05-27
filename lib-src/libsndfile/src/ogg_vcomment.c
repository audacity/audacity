/*
** Copyright (C) 2008-2019 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (C) 2018 Arthur Taylor <art@ified.ca>
**
** This program is free software ; you can redistribute it and/or modify
** it under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation ; either version 2.1 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY ; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
** GNU Lesser General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this program ; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include "sfconfig.h"

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "sndfile.h"
#include "sfendian.h"
#include "common.h"

#if HAVE_EXTERNAL_XIPH_LIBS

#include <ogg/ogg.h>

#include "ogg_vcomment.h"

typedef struct
{	int id ;
	const char *name ;
} STR_PAIR ;

/* See https://xiph.org/vorbis/doc/v-comment.html */
static STR_PAIR vorbiscomment_mapping [] =
{	{	SF_STR_TITLE,		"TITLE"			},
	{	SF_STR_COPYRIGHT,	"COPYRIGHT",	},
	{	SF_STR_SOFTWARE,	"ENCODER",		},
	{	SF_STR_ARTIST,		"ARTIST"		},
	{	SF_STR_COMMENT,		"COMMENT"		},
	{	SF_STR_DATE,		"DATE",			},
	{	SF_STR_ALBUM,		"ALBUM"			},
	{	SF_STR_LICENSE,		"LICENSE",		},
	{	SF_STR_TRACKNUMBER,	"TRACKNUMBER",	},
	{	SF_STR_GENRE,		"GENRE",		},
	{	0,					NULL,			},
} ;

/*-----------------------------------------------------------------------------------------------
** Private function prototypes.
*/

static int	vorbiscomment_lookup_id (const char *name) ;
static const char *	vorbiscomment_lookup_name (int id) ;

static inline size_t read_32bit_size_t (const unsigned char * ptr)
{	/* Read a 32 bit positive value from the provided pointer. */
	return LE2H_32_PTR (ptr) & 0x7fffffff ;
} /* read_32bit_size_t */

/*-----------------------------------------------------------------------------------------------
** Exported functions.
*/

int
vorbiscomment_read_tags (SF_PRIVATE *psf, ogg_packet *packet, vorbiscomment_ident *ident)
{	unsigned char *p, *ep ;
	char *tag, *c ;
	size_t tag_size, tag_len = 0 ;
	unsigned int ntags, i = 0 ;
	int id, ret = 0 ;

	/*
	** The smallest possible header is the ident string length plus two 4-byte
	** integers, (vender string length, tags count.)
	*/
	if (packet->bytes < (ident ? ident->length : 0) + 4 + 4)
		return SFE_MALFORMED_FILE ;

	/* Our working pointer. */
	p = packet->packet ;
	/* Our end pointer for bound checking. */
	ep = p + packet->bytes ;

	if (ident)
	{	if (memcmp (p, ident->ident, ident->length) != 0)
		{	psf_log_printf (psf, "Expected comment packet identifier missing.\n") ;
			return SFE_MALFORMED_FILE ;
			} ;
		p += ident->length ;
		} ;

	tag_size = 1024 ;
	tag = malloc (tag_size) ;
	/* Unlikely */
	if (!tag)
		return SFE_MALLOC_FAILED ;

	psf_log_printf (psf, "VorbisComment Metadata\n") ;

	/*
	** Vendor tag, manditory, no field name.
	*/
	tag_len = read_32bit_size_t (p) ;
	p += 4 ;
	if (tag_len > 0)
	{	/* Bound checking. 4 bytes for remaining manditory fields. */
		if (p + tag_len + 4 > ep)
		{	ret = SFE_MALFORMED_FILE ;
			goto free_tag_out ;
			} ;
		if (tag_len > tag_size - 1)
		{	free (tag) ;
			tag_size = tag_len + 1 ;
			tag = malloc (tag_size) ;
			/* Unlikely */
			if (!tag)
				return SFE_MALLOC_FAILED ;
			} ;
		memcpy (tag, p, tag_len) ; p += tag_len ;
		tag [tag_len] = '\0' ;
		psf_log_printf (psf, "  Vendor: %s\n", tag) ;
		} ;

	/*
	** List of tags of the form NAME=value
	** Allowable characters for NAME are the same as shell variable names.
	*/
	ntags = read_32bit_size_t (p) ;
	p += 4 ;
	for (i = 0 ; i < ntags ; i++)
	{	if (p + 4 > ep)
		{	ret = SFE_MALFORMED_FILE ;
			goto free_tag_out ;
			} ;
		tag_len = read_32bit_size_t (p) ;
		p += 4 ;
		if (p + tag_len > ep)
		{	ret = SFE_MALFORMED_FILE ;
			goto free_tag_out ;
			} ;
		if (tag_len > tag_size - 1)
		{	free (tag) ;
			tag_size = tag_len + 1 ;
			tag = malloc (tag_size) ;
			/* Unlikely */
			if (!tag)
				return SFE_MALLOC_FAILED ;
			} ;
		memcpy (tag, p, tag_len) ; p += tag_len ;
		tag [tag_len] = '\0' ;
		psf_log_printf (psf, "  %s\n", tag) ;
		for (c = tag ; *c ; c++)
		{	if (*c == '=')
				break ;
			*c = toupper (*c) ;
			} ;
		if (!c)
		{	psf_log_printf (psf, "Malformed Vorbis comment, no '=' found.\n") ;
			continue ;
			} ;
		*c = '\0' ;
		if ((id = vorbiscomment_lookup_id (tag)) != 0)
			psf_store_string (psf, id, c + 1) ;
		} ;

free_tag_out:
	if (tag != NULL)
		free (tag) ;
	return ret ;
} /* vorbiscomment_read_tags */

int
vorbiscomment_write_tags (SF_PRIVATE *psf, ogg_packet *packet, vorbiscomment_ident *ident, const char *vendor, int targetsize)
{	int i, ntags ;
	int tags_start ;
	const char *tag_name ;
	int tag_name_len, tag_body_len ;

	psf->header.ptr [0] = 0 ;
	psf->header.indx = 0 ;

	/* Packet identifier */
	if (ident)
		psf_binheader_writef (psf, "eb", BHWv (ident->ident), BHWz (ident->length)) ;

	/* Manditory Vendor Tag */
	tag_name_len = vendor ? strlen (vendor) : 0 ;
	psf_binheader_writef (psf, "e4b", BHW4 (tag_name_len), BHWv (vendor), BHWz (tag_name_len)) ;

	/* Tags Count. Skip for now, write after. */
	tags_start = psf->header.indx ;
	psf_binheader_writef (psf, "j", BHWj (4)) ;

	ntags = 0 ;
	/* Write each tag */
	for (i = 0 ; i < SF_MAX_STRINGS ; i++)
	{	if (psf->strings.data [i].type == 0)
			continue ;

		tag_name = vorbiscomment_lookup_name (psf->strings.data [i].type) ;
		if (tag_name == NULL)
			continue ;

		tag_name_len = strlen (tag_name) ;
		tag_body_len = strlen (psf->strings.storage + psf->strings.data [i].offset) ;
		if (targetsize > 0 && tag_name_len + tag_body_len + psf->header.indx > targetsize)
		{	/* If we are out of space, stop now. */
			return SFE_STR_MAX_DATA ;
			}
		psf_binheader_writef (psf, "e4b1b",
			BHW4 (tag_name_len + 1 + tag_body_len),
			BHWv (tag_name), BHWz (tag_name_len),
			BHW1 ('='),
			BHWv (psf->strings.storage + psf->strings.data [i].offset), BHWz (tag_body_len)) ;
		ntags++ ;
		} ;

	if (targetsize < 0)
	{	/*
		** Padding.
		**
		** Pad to a minimum of -targetsize, but make sure length % 255
		** = 254 so that we get the most out of the ogg segment lacing.
		*/
		psf_binheader_writef (psf, "z", BHWz ((psf->header.indx + -targetsize + 255) / 255 * 255 - 1)) ;
		}
	else if (targetsize > 0)
		psf_binheader_writef (psf, "z", BHWz (targetsize - psf->header.indx)) ;

	packet->packet = psf->header.ptr ;
	packet->bytes = psf->header.indx ;
	packet->b_o_s = 0 ;
	packet->e_o_s = 0 ;

	/* Seek back and write the tag count. */
	psf_binheader_writef (psf, "eo4", BHWo (tags_start), BHW4 (ntags)) ;

	return 0 ;
} /* vorbiscomment_write_tags */

/*==============================================================================
** Private functions.
*/

static int
vorbiscomment_lookup_id (const char * name)
{	STR_PAIR *p ;

	for (p = vorbiscomment_mapping ; p->id ; p++)
	{	if (!strcmp (name, p->name))
			return p->id ;
		} ;

	return 0 ;
} /* vorbiscomment_lookup_id */

static const char *
vorbiscomment_lookup_name (int id)
{	STR_PAIR *p ;

	for (p = vorbiscomment_mapping ; p->id ; p++)
	{	if (p->id == id)
			return p->name ;
		} ;

	return NULL ;
} /* vorbiscomment_lookup_name */

#endif /* HAVE_EXTERNAL_XIPH_LIBS */
