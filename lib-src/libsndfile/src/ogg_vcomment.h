/*
** Copyright (C) 2008-2018 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#ifndef SF_SRC_OGG_VCOMMENT_H
#define SF_SRC_OGG_VCOMMENT_H

/*
** Voriscomment identifier. Some Ogg stream embedding schemes require it.
*/
typedef struct
{	const char *ident ;
	int length ;
} vorbiscomment_ident ;

/*
** Read all vorbiscomment tags from *packet. Tags which match ones used
** by libsndfile strings are loaded into *psf. Ogg streams which require an
** identifier for the tags packet should pass it in *ident.
*/
int vorbiscomment_read_tags (SF_PRIVATE *psf, ogg_packet *packet, vorbiscomment_ident *ident) ;

/*
** Write metadata strings stored in *psf to *packet. The packet is optionally
** prefixed with *ident. The always-present vendor field should be the library
** used for encoding the audio data.
*/
int vorbiscomment_write_tags (SF_PRIVATE *psf, ogg_packet *packet, vorbiscomment_ident *ident, const char *vendor, int targetsize) ;

#endif /* SF_SRC_OGG_VCOMMENT_H */
