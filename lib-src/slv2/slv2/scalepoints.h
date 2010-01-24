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

#ifndef __SLV2_SCALE_POINTS_H__
#define __SLV2_SCALE_POINTS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <slv2/value.h>

/** \defgroup slv2_collections Collections of scale_points/objects
 *
 * Ordered collections of typed scale_points which are fast for random
 * access by index (i.e. a fancy array).
 *
 * @{
 */


/** Allocate a new, empty SLV2Values
 */
SLV2Values
slv2_scale_points_new();


/** Free an SLV2Values.
 */
void
slv2_scale_points_free(SLV2Values);


/** Get the number of scale points in a collection.
 */
unsigned
slv2_scale_points_size(SLV2Values points);


/** Get the scale point at the given index in a collection.
 *
 * @return the element at \a index, or NULL if index is out of range.
 *
 * Time = O(1)
 */
SLV2ScalePoint
slv2_scale_points_get_at(SLV2ScalePoints points, unsigned index);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_SCALE_POINTS_H__ */

