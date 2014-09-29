/*
 *	TwoLAME: an optimized MPEG Audio Layer Two encoder
 *
 *	Copyright (C) 2001-2004 Michael Cheng
 *	Copyright (C) 2004-2006 The TwoLAME Project
 *
 *	This library is free software; you can redistribute it and/or
 *	modify it under the terms of the GNU Lesser General Public
 *	License as published by the Free Software Foundation; either
 *	version 2.1 of the License, or (at your option) any later version.
 *
 *	This library is distributed in the hope that it will be useful,
 *	but WITHOUT ANY WARRANTY; without even the implied warranty of
 *	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *	Lesser General Public License for more details.
 *
 *	You should have received a copy of the GNU Lesser General Public
 *	License along with this library; if not, write to the Free Software
 *	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  $Id$
 *
 */


#include <stdio.h>
#include <stdlib.h>

#include "twolame.h"
#include "common.h"
#include "bitbuffer.h"
#include "mem.h"


/*create bit buffer*/
bit_stream *buffer_init(unsigned char *buffer, int buffer_size)
{
    bit_stream *bs = (bit_stream *) TWOLAME_MALLOC(sizeof(bit_stream));

    bs->buf = buffer;
    bs->buf_size = buffer_size;
    bs->buf_byte_idx = 0;
    bs->buf_bit_idx = 8;
    bs->totbit = 0;
    bs->eob = FALSE;
    bs->eobs = FALSE;

    return bs;
}

/* Dellocate bit buffer */
void buffer_deinit(bit_stream ** bs)
{

    if (bs == NULL || *bs == NULL)
        return;

    TWOLAME_FREE(*bs);
}



// vim:ts=4:sw=4:nowrap: 
