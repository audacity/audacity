/* SLV2
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 *  
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <raptor.h>
#include <slv2/scalepoints.h>
#include "slv2_internal.h"


SLV2ScalePoints
slv2_scale_points_new()
{
	return raptor_new_sequence((void (*)(void*))&slv2_scale_point_free, NULL);
}


void
slv2_scale_points_free(SLV2ScalePoints points)
{
	if (points)
		raptor_free_sequence(points);
}


unsigned
slv2_scale_points_size(SLV2ScalePoints points)
{
	return (points ? raptor_sequence_size(points) : 0);
}


SLV2ScalePoint
slv2_scale_points_get_at(SLV2ScalePoints points, unsigned index)
{
	if (index > INT_MAX)
		return NULL;
	else
		return (SLV2ScalePoint)raptor_sequence_get_at(points, (int)index);
}

