/*
** Copyright (C) 2007-2011 Erik de Castro Lopo <erikd@mega-nerd.com>
**
** This program is free software; you can redistribute it and/or modify
** it under the terms of the GNU General Public License as published by
** the Free Software Foundation; either version 2 of the License, or
** (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <sndfile.h>

#include "utils.h"
#include "generate.h"

#define	SF_MAX(x,y)		((x) > (y) ? (x) : (y))

static float crappy_snare (float *output, int len, int offset, float gain, float maxabs) ;

void
generate_file (const char * filename, int format, int len)
{	float * output ;
	float maxabs = 0.0 ;

	output = calloc (len, sizeof (float)) ;

	maxabs = crappy_snare (output, len, 0, 0.95, maxabs) ;
	maxabs = crappy_snare (output, len, len / 4, 0.85, maxabs) ;
	maxabs = crappy_snare (output, len, 2 * len / 4, 0.85, maxabs) ;
	crappy_snare (output, len, 3 * len / 4, 0.85, maxabs) ;

	write_mono_file (filename, format, 44100, output, len) ;

	free (output) ;
} /* generate_file */

static inline float
rand_float (void)
{	return rand () / (0.5 * RAND_MAX) - 1.0 ;
} /* rand_float */

static float
crappy_snare (float *output, int len, int offset, float gain, float maxabs)
{	int k ;
	float env = 0.0 ;

	for (k = offset ; k < len && env < gain ; k++)
	{	env += 0.03 ;
		output [k] += env * rand_float () ;
		maxabs = SF_MAX (maxabs, fabs (output [k])) ;
		} ;

	for ( ; k < len && env > 1e-8 ; k++)
	{	env *= 0.995 ;
		output [k] += env * rand_float () ;
		maxabs = SF_MAX (maxabs, fabs (output [k])) ;
		} ;

	return maxabs ;
} /* crappy_snare */
