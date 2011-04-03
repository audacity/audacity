/*
** Copyright (C) 2007-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
** Copyright (c) 2007 <robs@users.sourceforge.net>
**
** This library is free software; you can redistribute it and/or modify it
** under the terms of the GNU Lesser General Public License as published by
** the Free Software Foundation; either version 2 of the License, or (at
** your option) any later version.
**
** This library is distributed in the hope that it will be useful, but
** WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser
** General Public License for more details.
**
** You should have received a copy of the GNU Lesser General Public License
** along with this library.  If not, write to the Free Software Foundation,
** Fifth Floor, 51 Franklin Street, Boston, MA 02111-1301, USA.
*/

/* ADPCM: IMA, OKI <==> 16-bit PCM. */


#define		IMA_OKI_ADPCM_CODE_LEN	256
#define		IMA_OKI_ADPCM_PCM_LEN	(IMA_OKI_ADPCM_CODE_LEN *2)

typedef struct
{
	/* private: */
	int mask ;
	int last_output ;
	int step_index ;
	int max_step_index ;
	int const * steps ;

	/* public: */
	int errors ;
	int	code_count, pcm_count ;

	unsigned char	codes [IMA_OKI_ADPCM_CODE_LEN] ;
	short 			pcm [IMA_OKI_ADPCM_PCM_LEN] ;
} IMA_OKI_ADPCM ;

typedef enum
{	IMA_OKI_ADPCM_TYPE_IMA,
	IMA_OKI_ADPCM_TYPE_OKI
} IMA_OKI_ADPCM_TYPE ;

void ima_oki_adpcm_init		(IMA_OKI_ADPCM * state, IMA_OKI_ADPCM_TYPE type) ;

int	adpcm_decode	(IMA_OKI_ADPCM * state, int /* 0..15 */ code) ;
int	adpcm_encode	(IMA_OKI_ADPCM * state, int /* -32768..32767 */ sample) ;

void	ima_oki_adpcm_decode_block	(IMA_OKI_ADPCM * state) ;
void	ima_oki_adpcm_encode_block	(IMA_OKI_ADPCM * state) ;
