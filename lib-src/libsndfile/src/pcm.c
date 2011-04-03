/*
** Copyright (C) 1999-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
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

#include <math.h>

#include	"sndfile.h"
#include	"sfendian.h"
#include	"common.h"

/* Need to be able to handle 3 byte (24 bit) integers. So defined a
** type and use SIZEOF_TRIBYTE instead of (tribyte).
*/

typedef	void	tribyte ;

#define	SIZEOF_TRIBYTE	3

static sf_count_t	pcm_read_sc2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_uc2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bes2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_les2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bet2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_let2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bei2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_lei2s	(SF_PRIVATE *psf, short *ptr, sf_count_t len) ;

static sf_count_t	pcm_read_sc2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_uc2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bes2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_les2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bet2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_let2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bei2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_lei2i	(SF_PRIVATE *psf, int *ptr, sf_count_t len) ;

static sf_count_t	pcm_read_sc2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_uc2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bes2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_les2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bet2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_let2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bei2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_lei2f	(SF_PRIVATE *psf, float *ptr, sf_count_t len) ;

static sf_count_t	pcm_read_sc2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_uc2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bes2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_les2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bet2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_let2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_bei2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;
static sf_count_t	pcm_read_lei2d	(SF_PRIVATE *psf, double *ptr, sf_count_t len) ;

static sf_count_t	pcm_write_s2sc	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2uc	(SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2bes (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2les (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2bet (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2let (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2bei (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_s2lei (SF_PRIVATE *psf, const short *ptr, sf_count_t len) ;

static sf_count_t	pcm_write_i2sc	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2uc	(SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2bes (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2les (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2bet (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2let (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2bei (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_i2lei (SF_PRIVATE *psf, const int *ptr, sf_count_t len) ;

static sf_count_t	pcm_write_f2sc	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2uc	(SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2bes (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2les (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2bet (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2let (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2bei (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_f2lei (SF_PRIVATE *psf, const float *ptr, sf_count_t len) ;

static sf_count_t	pcm_write_d2sc	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2uc	(SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2bes (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2les (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2bet (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2let (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2bei (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;
static sf_count_t	pcm_write_d2lei (SF_PRIVATE *psf, const double *ptr, sf_count_t len) ;

/*-----------------------------------------------------------------------------------------------
*/

enum
{	/* Char type for 8 bit files. */
	SF_CHARS_SIGNED		= 200,
	SF_CHARS_UNSIGNED	= 201
} ;

/*-----------------------------------------------------------------------------------------------
*/

int
pcm_init (SF_PRIVATE *psf)
{	int chars = 0 ;

	if (psf->bytewidth == 0 || psf->sf.channels == 0)
	{	psf_log_printf (psf, "pcm_init : internal error : bytewitdh = %d, channels = %d\n", psf->bytewidth, psf->sf.channels) ;
		return SFE_INTERNAL ;
		} ;

	psf->blockwidth = psf->bytewidth * psf->sf.channels ;

	if ((SF_CODEC (psf->sf.format)) == SF_FORMAT_PCM_S8)
		chars = SF_CHARS_SIGNED ;
	else if ((SF_CODEC (psf->sf.format)) == SF_FORMAT_PCM_U8)
		chars = SF_CHARS_UNSIGNED ;

	if (CPU_IS_BIG_ENDIAN)
		psf->data_endswap = (psf->endian == SF_ENDIAN_BIG) ? SF_FALSE : SF_TRUE ;
	else
		psf->data_endswap = (psf->endian == SF_ENDIAN_LITTLE) ? SF_FALSE : SF_TRUE ;

	if (psf->file.mode == SFM_READ || psf->file.mode == SFM_RDWR)
	{	switch (psf->bytewidth * 0x10000 + psf->endian + chars)
		{	case (0x10000 + SF_ENDIAN_BIG + SF_CHARS_SIGNED) :
			case (0x10000 + SF_ENDIAN_LITTLE + SF_CHARS_SIGNED) :
					psf->read_short		= pcm_read_sc2s ;
					psf->read_int		= pcm_read_sc2i ;
					psf->read_float		= pcm_read_sc2f ;
					psf->read_double	= pcm_read_sc2d ;
					break ;
			case (0x10000 + SF_ENDIAN_BIG + SF_CHARS_UNSIGNED) :
			case (0x10000 + SF_ENDIAN_LITTLE + SF_CHARS_UNSIGNED) :
					psf->read_short		= pcm_read_uc2s ;
					psf->read_int		= pcm_read_uc2i ;
					psf->read_float		= pcm_read_uc2f ;
					psf->read_double	= pcm_read_uc2d ;
					break ;

			case (2 * 0x10000 + SF_ENDIAN_BIG) :
					psf->read_short		= pcm_read_bes2s ;
					psf->read_int		= pcm_read_bes2i ;
					psf->read_float		= pcm_read_bes2f ;
					psf->read_double	= pcm_read_bes2d ;
					break ;
			case (3 * 0x10000 + SF_ENDIAN_BIG) :
					psf->read_short		= pcm_read_bet2s ;
					psf->read_int		= pcm_read_bet2i ;
					psf->read_float		= pcm_read_bet2f ;
					psf->read_double	= pcm_read_bet2d ;
					break ;
			case (4 * 0x10000 + SF_ENDIAN_BIG) :

					psf->read_short		= pcm_read_bei2s ;
					psf->read_int		= pcm_read_bei2i ;
					psf->read_float		= pcm_read_bei2f ;
					psf->read_double	= pcm_read_bei2d ;
					break ;

			case (2 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->read_short		= pcm_read_les2s ;
					psf->read_int		= pcm_read_les2i ;
					psf->read_float		= pcm_read_les2f ;
					psf->read_double	= pcm_read_les2d ;
					break ;
			case (3 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->read_short		= pcm_read_let2s ;
					psf->read_int		= pcm_read_let2i ;
					psf->read_float		= pcm_read_let2f ;
					psf->read_double	= pcm_read_let2d ;
					break ;
			case (4 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->read_short		= pcm_read_lei2s ;
					psf->read_int		= pcm_read_lei2i ;
					psf->read_float		= pcm_read_lei2f ;
					psf->read_double	= pcm_read_lei2d ;
					break ;
			default :
				psf_log_printf (psf, "pcm.c returning SFE_UNIMPLEMENTED\nbytewidth %d    endian %d\n", psf->bytewidth, psf->endian) ;
				return SFE_UNIMPLEMENTED ;
			} ;
		} ;

	if (psf->file.mode == SFM_WRITE || psf->file.mode == SFM_RDWR)
	{	switch (psf->bytewidth * 0x10000 + psf->endian + chars)
		{	case (0x10000 + SF_ENDIAN_BIG + SF_CHARS_SIGNED) :
			case (0x10000 + SF_ENDIAN_LITTLE + SF_CHARS_SIGNED) :
					psf->write_short	= pcm_write_s2sc ;
					psf->write_int		= pcm_write_i2sc ;
					psf->write_float	= pcm_write_f2sc ;
					psf->write_double	= pcm_write_d2sc ;
					break ;
			case (0x10000 + SF_ENDIAN_BIG + SF_CHARS_UNSIGNED) :
			case (0x10000 + SF_ENDIAN_LITTLE + SF_CHARS_UNSIGNED) :
					psf->write_short	= pcm_write_s2uc ;
					psf->write_int		= pcm_write_i2uc ;
					psf->write_float	= pcm_write_f2uc ;
					psf->write_double	= pcm_write_d2uc ;
					break ;

			case (2 * 0x10000 + SF_ENDIAN_BIG) :
					psf->write_short	= pcm_write_s2bes ;
					psf->write_int		= pcm_write_i2bes ;
					psf->write_float	= pcm_write_f2bes ;
					psf->write_double	= pcm_write_d2bes ;
					break ;

			case (3 * 0x10000 + SF_ENDIAN_BIG) :
					psf->write_short	= pcm_write_s2bet ;
					psf->write_int		= pcm_write_i2bet ;
					psf->write_float	= pcm_write_f2bet ;
					psf->write_double	= pcm_write_d2bet ;
					break ;

			case (4 * 0x10000 + SF_ENDIAN_BIG) :
					psf->write_short	= pcm_write_s2bei ;
					psf->write_int		= pcm_write_i2bei ;
					psf->write_float	= pcm_write_f2bei ;
					psf->write_double	= pcm_write_d2bei ;
					break ;

			case (2 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->write_short	= pcm_write_s2les ;
					psf->write_int		= pcm_write_i2les ;
					psf->write_float	= pcm_write_f2les ;
					psf->write_double	= pcm_write_d2les ;
					break ;

			case (3 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->write_short	= pcm_write_s2let ;
					psf->write_int		= pcm_write_i2let ;
					psf->write_float	= pcm_write_f2let ;
					psf->write_double	= pcm_write_d2let ;
					break ;

			case (4 * 0x10000 + SF_ENDIAN_LITTLE) :
					psf->write_short	= pcm_write_s2lei ;
					psf->write_int		= pcm_write_i2lei ;
					psf->write_float	= pcm_write_f2lei ;
					psf->write_double	= pcm_write_d2lei ;
					break ;

			default :
				psf_log_printf (psf, "pcm.c returning SFE_UNIMPLEMENTED\nbytewidth %s    endian %d\n", psf->bytewidth, psf->endian) ;
				return SFE_UNIMPLEMENTED ;
			} ;

		} ;

	if (psf->filelength > psf->dataoffset)
	{	psf->datalength = (psf->dataend > 0) ? psf->dataend - psf->dataoffset :
							psf->filelength - psf->dataoffset ;
		}
	else
		psf->datalength = 0 ;

	psf->sf.frames = psf->blockwidth > 0 ? psf->datalength / psf->blockwidth : 0 ;

	return 0 ;
} /* pcm_init */

/*==============================================================================
*/

static inline void
sc2s_array	(signed char *src, int count, short *dest)
{	while (--count >= 0)
	{	dest [count] = src [count] << 8 ;
		} ;
} /* sc2s_array */

static inline void
uc2s_array	(unsigned char *src, int count, short *dest)
{	while (--count >= 0)
	{	dest [count] = (((short) src [count]) - 0x80) << 8 ;
		} ;
} /* uc2s_array */

static inline void
let2s_array (tribyte *src, int count, short *dest)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		dest [count] = LET2H_SHORT_PTR (ucptr) ;
		} ;
} /* let2s_array */

static inline void
bet2s_array (tribyte *src, int count, short *dest)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		dest [count] = BET2H_SHORT_PTR (ucptr) ;
			} ;
} /* bet2s_array */

static inline void
lei2s_array (int *src, int count, short *dest)
{	int value ;

	while (--count >= 0)
	{	value = LEI2H_INT (src [count]) ;
		dest [count] = value >> 16 ;
		} ;
} /* lei2s_array */

static inline void
bei2s_array (int *src, int count, short *dest)
{	int value ;

	while (--count >= 0)
	{	value = BEI2H_INT (src [count]) ;
		dest [count] = value >> 16 ;
		} ;
} /* bei2s_array */

/*--------------------------------------------------------------------------
*/

static inline void
sc2i_array	(signed char *src, int count, int *dest)
{	while (--count >= 0)
	{	dest [count] = ((int) src [count]) << 24 ;
		} ;
} /* sc2i_array */

static inline void
uc2i_array	(unsigned char *src, int count, int *dest)
{	while (--count >= 0)
	{	dest [count] = (((int) src [count]) - 128) << 24 ;
		} ;
} /* uc2i_array */

static inline void
bes2i_array (short *src, int count, int *dest)
{	short value ;

	while (--count >= 0)
	{	value = BES2H_SHORT (src [count]) ;
		dest [count] = value << 16 ;
		} ;
} /* bes2i_array */

static inline void
les2i_array (short *src, int count, int *dest)
{	short value ;

	while (--count >= 0)
	{	value = LES2H_SHORT (src [count]) ;
		dest [count] = value << 16 ;
		} ;
} /* les2i_array */

static inline void
bet2i_array (tribyte *src, int count, int *dest)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		dest [count] = BET2H_INT_PTR (ucptr) ;
			} ;
} /* bet2i_array */

static inline void
let2i_array (tribyte *src, int count, int *dest)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		dest [count] = LET2H_INT_PTR (ucptr) ;
		} ;
} /* let2i_array */

/*--------------------------------------------------------------------------
*/

static inline void
sc2f_array	(signed char *src, int count, float *dest, float normfact)
{	while (--count >= 0)
		dest [count] = ((float) src [count]) * normfact ;
} /* sc2f_array */

static inline void
uc2f_array	(unsigned char *src, int count, float *dest, float normfact)
{	while (--count >= 0)
		dest [count] = (((int) src [count]) - 128) * normfact ;
} /* uc2f_array */

static inline void
les2f_array (short *src, int count, float *dest, float normfact)
{	short	value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = LES2H_SHORT (value) ;
		dest [count] = ((float) value) * normfact ;
		} ;
} /* les2f_array */

static inline void
bes2f_array (short *src, int count, float *dest, float normfact)
{	short			value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = BES2H_SHORT (value) ;
		dest [count] = ((float) value) * normfact ;
		} ;
} /* bes2f_array */

static inline void
let2f_array (tribyte *src, int count, float *dest, float normfact)
{	unsigned char	*ucptr ;
	int 			value ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = LET2H_INT_PTR (ucptr) ;
		dest [count] = ((float) value) * normfact ;
		} ;
} /* let2f_array */

static inline void
bet2f_array (tribyte *src, int count, float *dest, float normfact)
{	unsigned char	*ucptr ;
	int				value ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = BET2H_INT_PTR (ucptr) ;
		dest [count] = ((float) value) * normfact ;
			} ;
} /* bet2f_array */

static inline void
lei2f_array (int *src, int count, float *dest, float normfact)
{	int 			value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = LEI2H_INT (value) ;
		dest [count] = ((float) value) * normfact ;
		} ;
} /* lei2f_array */

static inline void
bei2f_array (int *src, int count, float *dest, float normfact)
{	int 			value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = BEI2H_INT (value) ;
		dest [count] = ((float) value) * normfact ;
		} ;
} /* bei2f_array */

/*--------------------------------------------------------------------------
*/

static inline void
sc2d_array	(signed char *src, int count, double *dest, double normfact)
{	while (--count >= 0)
		dest [count] = ((double) src [count]) * normfact ;
} /* sc2d_array */

static inline void
uc2d_array	(unsigned char *src, int count, double *dest, double normfact)
{	while (--count >= 0)
		dest [count] = (((int) src [count]) - 128) * normfact ;
} /* uc2d_array */

static inline void
les2d_array (short *src, int count, double *dest, double normfact)
{	short	value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = LES2H_SHORT (value) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* les2d_array */

static inline void
bes2d_array (short *src, int count, double *dest, double normfact)
{	short	value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = BES2H_SHORT (value) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* bes2d_array */

static inline void
let2d_array (tribyte *src, int count, double *dest, double normfact)
{	unsigned char	*ucptr ;
	int				value ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = LET2H_INT_PTR (ucptr) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* let2d_array */

static inline void
bet2d_array (tribyte *src, int count, double *dest, double normfact)
{	unsigned char	*ucptr ;
	int				value ;

	ucptr = ((unsigned char*) src) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = (ucptr [0] << 24) | (ucptr [1] << 16) | (ucptr [2] << 8) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* bet2d_array */

static inline void
lei2d_array (int *src, int count, double *dest, double normfact)
{	int 	value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = LEI2H_INT (value) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* lei2d_array */

static inline void
bei2d_array (int *src, int count, double *dest, double normfact)
{	int 	value ;

	while (--count >= 0)
	{	value = src [count] ;
		value = BEI2H_INT (value) ;
		dest [count] = ((double) value) * normfact ;
		} ;
} /* bei2d_array */

/*--------------------------------------------------------------------------
*/

static inline void
s2sc_array	(const short *src, signed char *dest, int count)
{	while (--count >= 0)
		dest [count] = src [count] >> 8 ;
} /* s2sc_array */

static inline void
s2uc_array	(const short *src, unsigned char *dest, int count)
{	while (--count >= 0)
		dest [count] = (src [count] >> 8) + 0x80 ;
} /* s2uc_array */

static inline void
s2let_array (const short *src, tribyte *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		ucptr [0] = 0 ;
		ucptr [1] = src [count] ;
		ucptr [2] = src [count] >> 8 ;
		} ;
} /* s2let_array */

static inline void
s2bet_array (const short *src, tribyte *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		ucptr [2] = 0 ;
		ucptr [1] = src [count] ;
		ucptr [0] = src [count] >> 8 ;
		} ;
} /* s2bet_array */

static inline void
s2lei_array (const short *src, int *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 4 * count ;
	while (--count >= 0)
	{	ucptr -= 4 ;
		ucptr [0] = 0 ;
		ucptr [1] = 0 ;
		ucptr [2] = src [count] ;
		ucptr [3] = src [count] >> 8 ;
		} ;
} /* s2lei_array */

static inline void
s2bei_array (const short *src, int *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 4 * count ;
	while (--count >= 0)
	{	ucptr -= 4 ;
		ucptr [0] = src [count] >> 8 ;
		ucptr [1] = src [count] ;
		ucptr [2] = 0 ;
		ucptr [3] = 0 ;
		} ;
} /* s2bei_array */

/*--------------------------------------------------------------------------
*/

static inline void
i2sc_array	(const int *src, signed char *dest, int count)
{	while (--count >= 0)
		dest [count] = (src [count] >> 24) ;
} /* i2sc_array */

static inline void
i2uc_array	(const int *src, unsigned char *dest, int count)
{	while (--count >= 0)
		dest [count] = ((src [count] >> 24) + 128) ;
} /* i2uc_array */

static inline void
i2bes_array (const int *src, short *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 2 * count ;
	while (--count >= 0)
	{	ucptr -= 2 ;
		ucptr [0] = src [count] >> 24 ;
		ucptr [1] = src [count] >> 16 ;
		} ;
} /* i2bes_array */

static inline void
i2les_array (const int *src, short *dest, int count)
{	unsigned char	*ucptr ;

	ucptr = ((unsigned char*) dest) + 2 * count ;
	while (--count >= 0)
	{	ucptr -= 2 ;
		ucptr [0] = src [count] >> 16 ;
		ucptr [1] = src [count] >> 24 ;
		} ;
} /* i2les_array */

static inline void
i2let_array (const int *src, tribyte *dest, int count)
{	unsigned char	*ucptr ;
	int				value ;

	ucptr = ((unsigned char*) dest) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = src [count] >> 8 ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		} ;
} /* i2let_array */

static inline void
i2bet_array (const int *src, tribyte *dest, int count)
{	unsigned char	*ucptr ;
	int				value ;

	ucptr = ((unsigned char*) dest) + 3 * count ;
	while (--count >= 0)
	{	ucptr -= 3 ;
		value = src [count] >> 8 ;
		ucptr [2] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [0] = value >> 16 ;
		} ;
} /* i2bet_array */

/*===============================================================================================
*/

static sf_count_t
pcm_read_sc2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		sc2s_array (psf->u.scbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_sc2s */

static sf_count_t
pcm_read_uc2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		uc2s_array (psf->u.ucbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_uc2s */

static sf_count_t
pcm_read_bes2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int		total ;

	total = psf_fread (ptr, sizeof (short), len, psf) ;
	if (CPU_IS_LITTLE_ENDIAN)
		endswap_short_array (ptr, len) ;

	return total ;
} /* pcm_read_bes2s */

static sf_count_t
pcm_read_les2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int		total ;

	total = psf_fread (ptr, sizeof (short), len, psf) ;
	if (CPU_IS_BIG_ENDIAN)
		endswap_short_array (ptr, len) ;

	return total ;
} /* pcm_read_les2s */

static sf_count_t
pcm_read_bet2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		bet2s_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bet2s */

static sf_count_t
pcm_read_let2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		let2s_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_let2s */

static sf_count_t
pcm_read_bei2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		bei2s_array (psf->u.ibuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bei2s */

static sf_count_t
pcm_read_lei2s (SF_PRIVATE *psf, short *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		lei2s_array (psf->u.ibuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_lei2s */

/*-----------------------------------------------------------------------------------------------
*/

static sf_count_t
pcm_read_sc2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		sc2i_array (psf->u.scbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_sc2i */

static sf_count_t
pcm_read_uc2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		uc2i_array (psf->u.ucbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_uc2i */

static sf_count_t
pcm_read_bes2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		bes2i_array (psf->u.sbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bes2i */

static sf_count_t
pcm_read_les2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		les2i_array (psf->u.sbuf, readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_les2i */

static sf_count_t
pcm_read_bet2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		bet2i_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bet2i */

static sf_count_t
pcm_read_let2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		let2i_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_let2i */

static sf_count_t
pcm_read_bei2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int		total ;

	total = psf_fread (ptr, sizeof (int), len, psf) ;
	if (CPU_IS_LITTLE_ENDIAN)
		endswap_int_array	(ptr, len) ;

	return total ;
} /* pcm_read_bei2i */

static sf_count_t
pcm_read_lei2i (SF_PRIVATE *psf, int *ptr, sf_count_t len)
{	int		total ;

	total = psf_fread (ptr, sizeof (int), len, psf) ;
	if (CPU_IS_BIG_ENDIAN)
		endswap_int_array	(ptr, len) ;

	return total ;
} /* pcm_read_lei2i */

/*-----------------------------------------------------------------------------------------------
*/

static sf_count_t
pcm_read_sc2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		sc2f_array (psf->u.scbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_sc2f */

static sf_count_t
pcm_read_uc2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		uc2f_array (psf->u.ucbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_uc2f */

static sf_count_t
pcm_read_bes2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		bes2f_array (psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bes2f */

static sf_count_t
pcm_read_les2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		les2f_array (psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_les2f */

static sf_count_t
pcm_read_bet2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	/* Special normfactor because tribyte value is read into an int. */
	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80000000) : 1.0 / 256.0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		bet2f_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bet2f */

static sf_count_t
pcm_read_let2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	/* Special normfactor because tribyte value is read into an int. */
	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80000000) : 1.0 / 256.0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		let2f_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_let2f */

static sf_count_t
pcm_read_bei2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80000000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		bei2f_array (psf->u.ibuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bei2f */

static sf_count_t
pcm_read_lei2f (SF_PRIVATE *psf, float *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	float	normfact ;

	normfact = (psf->norm_float == SF_TRUE) ? 1.0 / ((float) 0x80000000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		lei2f_array (psf->u.ibuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_lei2f */

/*-----------------------------------------------------------------------------------------------
*/

static sf_count_t
pcm_read_sc2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		sc2d_array (psf->u.scbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_sc2d */

static sf_count_t
pcm_read_uc2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		uc2d_array (psf->u.ucbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_uc2d */

static sf_count_t
pcm_read_bes2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		bes2d_array (psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bes2d */

static sf_count_t
pcm_read_les2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x8000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		les2d_array (psf->u.sbuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_les2d */

static sf_count_t
pcm_read_bet2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80000000) : 1.0 / 256.0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		bet2d_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bet2d */

static sf_count_t
pcm_read_let2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	/* Special normfactor because tribyte value is read into an int. */
	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80000000) : 1.0 / 256.0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		let2d_array ((tribyte*) (psf->u.ucbuf), readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_let2d */

static sf_count_t
pcm_read_bei2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80000000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		bei2d_array (psf->u.ibuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_bei2d */

static sf_count_t
pcm_read_lei2d (SF_PRIVATE *psf, double *ptr, sf_count_t len)
{	int			bufferlen, readcount ;
	sf_count_t	total = 0 ;
	double		normfact ;

	normfact = (psf->norm_double == SF_TRUE) ? 1.0 / ((double) 0x80000000) : 1.0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		readcount = psf_fread (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		lei2d_array (psf->u.ibuf, readcount, ptr + total, normfact) ;
		total += readcount ;
		if (readcount < bufferlen)
			break ;
		len -= readcount ;
		} ;

	return total ;
} /* pcm_read_lei2d */

/*===============================================================================================
**-----------------------------------------------------------------------------------------------
**===============================================================================================
*/

static sf_count_t
pcm_write_s2sc	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2sc_array (ptr + total, psf->u.scbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2sc */

static sf_count_t
pcm_write_s2uc	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2uc_array (ptr + total, psf->u.ucbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2uc */

static sf_count_t
pcm_write_s2bes	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (CPU_IS_BIG_ENDIAN)
		return psf_fwrite (ptr, sizeof (short), len, psf) ;
	else

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		endswap_short_copy (psf->u.sbuf, ptr + total, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2bes */

static sf_count_t
pcm_write_s2les	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (CPU_IS_LITTLE_ENDIAN)
		return psf_fwrite (ptr, sizeof (short), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		endswap_short_copy (psf->u.sbuf, ptr + total, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2les */

static sf_count_t
pcm_write_s2bet	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2bet_array (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2bet */

static sf_count_t
pcm_write_s2let	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2let_array (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2let */

static sf_count_t
pcm_write_s2bei	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2bei_array (ptr + total, psf->u.ibuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2bei */

static sf_count_t
pcm_write_s2lei	(SF_PRIVATE *psf, const short *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		s2lei_array (ptr + total, psf->u.ibuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_s2lei */

/*-----------------------------------------------------------------------------------------------
*/

static sf_count_t
pcm_write_i2sc	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2sc_array (ptr + total, psf->u.scbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2sc */

static sf_count_t
pcm_write_i2uc	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2uc_array (ptr + total, psf->u.ucbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2uc */

static sf_count_t
pcm_write_i2bes	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2bes_array (ptr + total, psf->u.sbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2bes */

static sf_count_t
pcm_write_i2les	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2les_array (ptr + total, psf->u.sbuf, bufferlen) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2les */

static sf_count_t
pcm_write_i2bet	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2bet_array (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2bet */

static sf_count_t
pcm_write_i2let	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		i2let_array (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2les */

static sf_count_t
pcm_write_i2bei	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (CPU_IS_BIG_ENDIAN)
		return psf_fwrite (ptr, sizeof (int), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		endswap_int_copy (psf->u.ibuf, ptr + total, bufferlen) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2bei */

static sf_count_t
pcm_write_i2lei	(SF_PRIVATE *psf, const int *ptr, sf_count_t len)
{	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	if (CPU_IS_LITTLE_ENDIAN)
		return psf_fwrite (ptr, sizeof (int), len, psf) ;

	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		endswap_int_copy (psf->u.ibuf, ptr + total, bufferlen) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_i2lei */

/*------------------------------------------------------------------------------
**==============================================================================
**------------------------------------------------------------------------------
*/

static void
f2sc_array (const float *src, signed char *dest, int count, int normalize)
{	float normfact ;

	normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
	{	dest [count] = lrintf (src [count] * normfact) ;
		} ;
} /* f2sc_array */

static void
f2sc_clip_array (const float *src, signed char *dest, int count, int normalize)
{	float	normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x1000000) ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 127 ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = -128 ;
			continue ;
			} ;

		dest [count] = lrintf (scaled_value) >> 24 ;
		} ;
} /* f2sc_clip_array */

static sf_count_t
pcm_write_f2sc	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, signed char *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2sc_clip_array : f2sc_array ;
	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.scbuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2sc */

/*==============================================================================
*/

static	void
f2uc_array	(const float *src, unsigned char *dest, int count, int normalize)
{	float normfact ;

	normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
	{	dest [count] = lrintf (src [count] * normfact) + 128 ;
		} ;
} /* f2uc_array */

static	void
f2uc_clip_array	(const float *src, unsigned char *dest, int count, int normalize)
{	float	normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x1000000) ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 0xFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = 0 ;
			continue ;
			} ;

		dest [count] = (lrintf (scaled_value) >> 24) + 128 ;
		} ;
} /* f2uc_clip_array */

static sf_count_t
pcm_write_f2uc	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, unsigned char *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2uc_clip_array : f2uc_array ;
	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ucbuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2uc */

/*==============================================================================
*/

static void
f2bes_array (const float *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float 			normfact ;
	short			value ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [1] = value ;
		ucptr [0] = value >> 8 ;
			} ;
} /* f2bes_array */

static void
f2bes_clip_array (const float *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x10000) ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [1] = 0xFF ;
			ucptr [0] = 0x7F ;
			continue ;
		} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [1] = 0x00 ;
			ucptr [0] = 0x80 ;
			continue ;
			} ;

		value = lrintf (scaled_value) ;
		ucptr [1] = value >> 16 ;
		ucptr [0] = value >> 24 ;
		} ;
} /* f2bes_clip_array */

static sf_count_t
pcm_write_f2bes	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, short *t, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2bes_clip_array : f2bes_array ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.sbuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
				break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2bes */

/*==============================================================================
*/

static void
f2les_array (const float *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact ;
	int				value ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		} ;
} /* f2les_array */

static void
f2les_clip_array (const float *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x10000) ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x80 ;
			continue ;
			} ;

		value = lrintf (scaled_value) ;
		ucptr [0] = value >> 16 ;
		ucptr [1] = value >> 24 ;
		} ;
} /* f2les_clip_array */

static sf_count_t
pcm_write_f2les	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, short *t, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2les_clip_array : f2les_array ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.sbuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2les */

/*==============================================================================
*/

static void
f2let_array (const float *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float 			normfact ;
	int				value ;

	normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		} ;
} /* f2let_array */

static void
f2let_clip_array (const float *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x100) ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x80 ;
			continue ;
		} ;

		value = lrintf (scaled_value) ;
		ucptr [0] = value >> 8 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 24 ;
		} ;
} /* f2let_clip_array */

static sf_count_t
pcm_write_f2let	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, tribyte *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2let_clip_array : f2let_array ;
	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2let */

/*==============================================================================
*/

static void
f2bet_array (const float *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float 			normfact ;
	int				value ;

	normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [0] = value >> 16 ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value ;
		} ;
} /* f2bet_array */

static void
f2bet_clip_array (const float *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x100) ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0x7F ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0xFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x80 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x00 ;
			continue ;
		} ;

		value = lrint (scaled_value) ;
		ucptr [0] = value >> 24 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 8 ;
		} ;
} /* f2bet_clip_array */

static sf_count_t
pcm_write_f2bet	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, tribyte *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2bet_clip_array : f2bet_array ;
	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2bet */

/*==============================================================================
*/

static void
f2bei_array (const float *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact ;
	int				value ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;
	while (--count >= 0)
	{	ucptr -= 4 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [0] = value >> 24 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 8 ;
		ucptr [3] = value ;
		} ;
} /* f2bei_array */

static void
f2bei_clip_array (const float *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= 1.0 * 0x7FFFFFFF)
		{	ucptr [0] = 0x7F ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0xFF ;
			ucptr [3] = 0xFF ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x80 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x00 ;
			ucptr [3] = 0x00 ;
			continue ;
		} ;

		value = lrintf (scaled_value) ;
		ucptr [0] = value >> 24 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 8 ;
		ucptr [3] = value ;
		} ;
} /* f2bei_clip_array */

static sf_count_t
pcm_write_f2bei	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, int *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2bei_clip_array : f2bei_array ;
	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ibuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2bei */

/*==============================================================================
*/

static void
f2lei_array (const float *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact ;
	int				value ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		value = lrintf (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		ucptr [3] = value >> 24 ;
		} ;
} /* f2lei_array */

static void
f2lei_clip_array (const float *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	float			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0xFF ;
			ucptr [3] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x00 ;
			ucptr [3] = 0x80 ;
			continue ;
			} ;

		value = lrintf (scaled_value) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		ucptr [3] = value >> 24 ;
		} ;
} /* f2lei_clip_array */

static sf_count_t
pcm_write_f2lei	(SF_PRIVATE *psf, const float *ptr, sf_count_t len)
{	void		(*convert) (const float *, int *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? f2lei_clip_array : f2lei_array ;
	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ibuf, bufferlen, psf->norm_float) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_f2lei */

/*==============================================================================
*/

static void
d2sc_array	(const double *src, signed char *dest, int count, int normalize)
{	double	normfact ;

	normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
	{	dest [count] = lrint (src [count] * normfact) ;
		} ;
} /* d2sc_array */

static void
d2sc_clip_array	(const double *src, signed char *dest, int count, int normalize)
{	double	normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x1000000) ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 127 ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = -128 ;
			continue ;
			} ;

		dest [count] = lrintf (scaled_value) >> 24 ;
		} ;
} /* d2sc_clip_array */

static sf_count_t
pcm_write_d2sc	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, signed char *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2sc_clip_array : d2sc_array ;
	bufferlen = ARRAY_LEN (psf->u.scbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.scbuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.scbuf, sizeof (signed char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2sc */

/*==============================================================================
*/

static	void
d2uc_array	(const double *src, unsigned char *dest, int count, int normalize)
{	double normfact ;

	normfact = normalize ? (1.0 * 0x7F) : 1.0 ;

	while (--count >= 0)
	{	dest [count] = lrint (src [count] * normfact) + 128 ;
		} ;
} /* d2uc_array */

static	void
d2uc_clip_array	(const double *src, unsigned char *dest, int count, int normalize)
{	double	normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x1000000) ;

	while (--count >= 0)
	{	scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	dest [count] = 255 ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	dest [count] = 0 ;
			continue ;
			} ;

		dest [count] = (lrint (src [count] * normfact) >> 24) + 128 ;
		} ;
} /* d2uc_clip_array */

static sf_count_t
pcm_write_d2uc	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, unsigned char *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2uc_clip_array : d2uc_array ;
	bufferlen = ARRAY_LEN (psf->u.ucbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ucbuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.ucbuf, sizeof (unsigned char), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2uc */

/*==============================================================================
*/

static void
d2bes_array (const double *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	short			value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		value = lrint (src [count] * normfact) ;
		ucptr [1] = value ;
		ucptr [0] = value >> 8 ;
		} ;
} /* d2bes_array */

static void
d2bes_clip_array (const double *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	double			normfact, scaled_value ;
	int				value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x10000) ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [1] = 0xFF ;
			ucptr [0] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [1] = 0x00 ;
			ucptr [0] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [1] = value >> 16 ;
		ucptr [0] = value >> 24 ;
		} ;
} /* d2bes_clip_array */

static sf_count_t
pcm_write_d2bes	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, short *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2bes_clip_array : d2bes_array ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.sbuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2bes */

/*==============================================================================
*/

static void
d2les_array (const double *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	short			value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		value = lrint (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		} ;
} /* d2les_array */

static void
d2les_clip_array (const double *src, short *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x10000) ;
	ucptr = ((unsigned char*) dest) + 2 * count ;

	while (--count >= 0)
	{	ucptr -= 2 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [0] = value >> 16 ;
		ucptr [1] = value >> 24 ;
		} ;
} /* d2les_clip_array */

static sf_count_t
pcm_write_d2les	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, short *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2les_clip_array : d2les_array ;
	bufferlen = ARRAY_LEN (psf->u.sbuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.sbuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.sbuf, sizeof (short), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2les */

/*==============================================================================
*/

static void
d2let_array (const double *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		value = lrint (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		} ;
} /* d2let_array */

static void
d2let_clip_array (const double *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x100) ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [0] = value >> 8 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 24 ;
		} ;
} /* d2let_clip_array */

static sf_count_t
pcm_write_d2let	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, tribyte *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2let_clip_array : d2let_array ;
	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2let */

/*==============================================================================
*/

static void
d2bet_array (const double *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		value = lrint (src [count] * normfact) ;
		ucptr [2] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [0] = value >> 16 ;
		} ;
} /* d2bet_array */

static void
d2bet_clip_array (const double *src, tribyte *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : (1.0 * 0x100) ;
	ucptr = ((unsigned char*) dest) + 3 * count ;

	while (--count >= 0)
	{	ucptr -= 3 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [2] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [0] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [2] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [0] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [2] = value >> 8 ;
		ucptr [1] = value >> 16 ;
		ucptr [0] = value >> 24 ;
		} ;
} /* d2bet_clip_array */

static sf_count_t
pcm_write_d2bet	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, tribyte *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2bet_clip_array : d2bet_array ;
	bufferlen = sizeof (psf->u.ucbuf) / SIZEOF_TRIBYTE ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, (tribyte*) (psf->u.ucbuf), bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.ucbuf, SIZEOF_TRIBYTE, bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2bet */

/*==============================================================================
*/

static void
d2bei_array (const double *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		value = lrint (src [count] * normfact) ;
		ucptr [0] = value >> 24 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 8 ;
		ucptr [3] = value ;
		} ;
} /* d2bei_array */

static void
d2bei_clip_array (const double *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [3] = 0xFF ;
			ucptr [2] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [0] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [3] = 0x00 ;
			ucptr [2] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [0] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [0] = value >> 24 ;
		ucptr [1] = value >> 16 ;
		ucptr [2] = value >> 8 ;
		ucptr [3] = value ;
		} ;
} /* d2bei_clip_array */

static sf_count_t
pcm_write_d2bei	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, int *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2bei_clip_array : d2bei_array ;
	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ibuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2bei */

/*==============================================================================
*/

static void
d2lei_array (const double *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact ;

	normfact = normalize ? (1.0 * 0x7FFFFFFF) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		value = lrint (src [count] * normfact) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		ucptr [3] = value >> 24 ;
		} ;
} /* d2lei_array */

static void
d2lei_clip_array (const double *src, int *dest, int count, int normalize)
{	unsigned char	*ucptr ;
	int				value ;
	double			normfact, scaled_value ;

	normfact = normalize ? (8.0 * 0x10000000) : 1.0 ;
	ucptr = ((unsigned char*) dest) + 4 * count ;

	while (--count >= 0)
	{	ucptr -= 4 ;
		scaled_value = src [count] * normfact ;
		if (CPU_CLIPS_POSITIVE == 0 && scaled_value >= (1.0 * 0x7FFFFFFF))
		{	ucptr [0] = 0xFF ;
			ucptr [1] = 0xFF ;
			ucptr [2] = 0xFF ;
			ucptr [3] = 0x7F ;
			continue ;
			} ;
		if (CPU_CLIPS_NEGATIVE == 0 && scaled_value <= (-8.0 * 0x10000000))
		{	ucptr [0] = 0x00 ;
			ucptr [1] = 0x00 ;
			ucptr [2] = 0x00 ;
			ucptr [3] = 0x80 ;
			continue ;
			} ;

		value = lrint (scaled_value) ;
		ucptr [0] = value ;
		ucptr [1] = value >> 8 ;
		ucptr [2] = value >> 16 ;
		ucptr [3] = value >> 24 ;
		} ;
} /* d2lei_clip_array */

static sf_count_t
pcm_write_d2lei	(SF_PRIVATE *psf, const double *ptr, sf_count_t len)
{	void		(*convert) (const double *, int *, int, int) ;
	int			bufferlen, writecount ;
	sf_count_t	total = 0 ;

	convert = (psf->add_clipping) ? d2lei_clip_array : d2lei_array ;
	bufferlen = ARRAY_LEN (psf->u.ibuf) ;

	while (len > 0)
	{	if (len < bufferlen)
			bufferlen = (int) len ;
		convert (ptr + total, psf->u.ibuf, bufferlen, psf->norm_double) ;
		writecount = psf_fwrite (psf->u.ibuf, sizeof (int), bufferlen, psf) ;
		total += writecount ;
		if (writecount < bufferlen)
			break ;
		len -= writecount ;
		} ;

	return total ;
} /* pcm_write_d2lei */

