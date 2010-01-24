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

#ifndef __SLV2_UTIL_H__
#define __SLV2_UTIL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdarg.h>


/** \defgroup slv2_util Utility functions
 *
 * @{
 */


/** Convert a full URI (eg file://foo/bar/baz.ttl) to a local path (e.g. /foo/bar/baz.ttl).
 *
 * Return value is shared and must not be deleted by caller.
 * \return \a uri converted to a path, or NULL on failure (URI is not local).
 */
const char* slv2_uri_to_path(const char* uri);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_UTIL_H__ */

