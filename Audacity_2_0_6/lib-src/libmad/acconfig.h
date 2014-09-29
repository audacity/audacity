/*
 * libmad - MPEG audio decoder library
 * Copyright (C) 2000-2001 Robert Leslie
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * $Id: acconfig.h,v 1.2 2001-10-21 22:26:32 dmazzoni Exp $
 */

# ifndef LIBMAD_CONFIG_H
# define LIBMAD_CONFIG_H

/*****************************************************************************
 * Definitions selected automatically by `configure'                         *
 *****************************************************************************/
@TOP@

/* Define to optimize for speed over accuracy. */
#undef OPT_SPEED

/* Define to optimize for accuracy over speed. */
#undef OPT_ACCURACY

/* Define to enable a fast subband synthesis approximation optimization. */
#undef OPT_SSO

/* Define to influence a strict interpretation of the ISO/IEC standards,
   even if this is in opposition with best accepted practices. */
#undef OPT_STRICT

/* Define if your MIPS CPU supports a 2-operand MADD instruction. */
#undef HAVE_MADD_ASM

/* Define if your MIPS CPU supports a 2-operand MADD16 instruction. */
#undef HAVE_MADD16_ASM

/* Define to enable diagnostic debugging support. */
#undef DEBUG

/* Define to disable debugging assertions. */
#undef NDEBUG

/* Define to enable experimental code. */
#undef EXPERIMENTAL

@BOTTOM@
/*****************************************************************************
 * End of automatically configured definitions                               *
 *****************************************************************************/

# endif
