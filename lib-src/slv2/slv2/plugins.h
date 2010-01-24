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

#ifndef __SLV2_PLUGINS_H__
#define __SLV2_PLUGINS_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <slv2/types.h>
#include <slv2/plugin.h>

/** \addtogroup slv2_collections
 * @{
 */


/** Free a plugin plugins.
 *
 * Freeing a plugin collection does not destroy the plugins it contains
 * (plugins are owned by the world). \a plugins is invalid after this call.
 */
void
slv2_plugins_free(SLV2World   world,
                  SLV2Plugins plugins);


/** Get the number of plugins in the collection.
 */
unsigned
slv2_plugins_size(SLV2Plugins plugins);


/** Get a plugin from the collection by URI.
 *
 * Return value is shared (stored in \a plugins) and must not be freed or
 * modified by the caller in any way.
 *
 * Time = O(log2(n))
 * 
 * \return NULL if plugin with \a url not found in \a plugins.
 */
SLV2Plugin
slv2_plugins_get_by_uri(SLV2Plugins plugins,
                        SLV2Value   uri);


/** Get a plugin from the plugins by index.
 *
 * \a index has no significance other than as an index into this plugins.
 * Any \a index not less than slv2_plugins_get_length(plugins) will return NULL,
 * so all plugins in a plugins can be enumerated by repeated calls
 * to this function starting with \a index = 0.
 *
 * Time = O(1)
 *
 * \return NULL if \a index out of range.
 */
SLV2Plugin
slv2_plugins_get_at(SLV2Plugins plugins,
                    unsigned    index);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_PLUGINS_H__ */

