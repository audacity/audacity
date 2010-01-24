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

#ifndef __SLV2_SCALE_POINT_H__
#define __SLV2_SCALE_POINT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <slv2/types.h>

/** \addtogroup slv2_data
 * @{
 */


/** Get the label of this scale point (enumeration value)
 *
 * Returned value is owned by \a point and must not be freed.
 */
SLV2Value
slv2_scale_point_get_label(SLV2ScalePoint point);


/** Get the value of this scale point (enumeration value)
 *
 * Returned value is owned by \a point and must not be freed.
 */
SLV2Value
slv2_scale_point_get_value(SLV2ScalePoint point);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_SCALE_POINT_H__ */
