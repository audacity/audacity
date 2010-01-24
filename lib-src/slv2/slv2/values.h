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

#ifndef __SLV2_VALUES_H__
#define __SLV2_VALUES_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <slv2/value.h>

/** \defgroup slv2_collections Collections of values/objects
 *
 * Ordered collections of typed values which are fast for random
 * access by index (i.e. a fancy array).
 *
 * @{
 */


/** Allocate a new, empty SLV2Values
 */
SLV2Values
slv2_values_new();


/** Free an SLV2Values.
 */
void
slv2_values_free(SLV2Values);


/** Get the number of elements in a value collection.
 */
unsigned
slv2_values_size(SLV2Values values);


/** Get the value at a given index in the collection.
 *
 * @return the element at \a index, or NULL if index is out of range.
 *
 * Time = O(1)
 */
SLV2Value
slv2_values_get_at(SLV2Values values, unsigned index);


/** Return whether \a values contains \a value.
 *
 * Time = O(n)
 */
bool
slv2_values_contains(SLV2Values values, SLV2Value value);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_VALUES_H__ */

