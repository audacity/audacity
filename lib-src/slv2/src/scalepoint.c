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

#define _XOPEN_SOURCE 500

#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <locale.h>
#include <raptor.h>
#include <slv2/value.h>
#include "slv2_internal.h"


/* private - ownership of value and label is taken */
SLV2ScalePoint
slv2_scale_point_new(SLV2Value value, SLV2Value label)
{
	SLV2ScalePoint point = (SLV2ScalePoint)malloc(sizeof(struct _SLV2ScalePoint));
	point->value = value;
	point->label= label;
	return point;
}


/* private */
void
slv2_scale_point_free(SLV2ScalePoint point)
{
	slv2_value_free(point->value);
	slv2_value_free(point->label);
	free(point);
}


SLV2Value
slv2_scale_point_get_value(SLV2ScalePoint p)
{
	return p->value;
}


SLV2Value
slv2_scale_point_get_label(SLV2ScalePoint p)
{
	return p->label;
}

