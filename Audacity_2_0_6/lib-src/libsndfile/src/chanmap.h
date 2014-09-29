/*
** Copyright (C) 2009-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

typedef struct
{	/* The tag in the AIFF or CAF file. */
	int channel_layout_tag ;

	/* The equivalent array of SF_CHANNEL_MAP_* entries. */
	const int * channel_map ;

	const char * name ;
} AIFF_CAF_CHANNEL_MAP ;


int aiff_caf_find_channel_layout_tag (const int *chan_map, int channels) ;

const AIFF_CAF_CHANNEL_MAP * aiff_caf_of_channel_layout_tag (int tag) ;
