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

#ifndef __SLV2_PLUGIN_CLASSES_H__
#define __SLV2_PLUGIN_CLASSES_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <slv2/pluginclass.h>

/** \addtogroup slv2_collections
 * @{
 */


/** Get the number of plugins in the collection.
 */
unsigned
slv2_plugin_classes_size(SLV2PluginClasses classes);


/** Get a plugin class from the collection by URI.
 *
 * Return value is shared (stored in \a classes) and must not be freed or
 * modified by the caller in any way.
 *
 * Time = O(log2(n))
 * 
 * \return NULL if plugin with \a url not found in \a classes.
 */
SLV2PluginClass
slv2_plugin_classes_get_by_uri(SLV2PluginClasses classes,
                               SLV2Value         uri);


/** Get a plugin from the collection by index.
 *
 * \a index has no significance other than as an index into \a classes.
 * Any \a index not less than slv2_plugin_classes_get_length(classes) will return NULL,
 * so all plugin_classes in a classes can be enumerated by repeated calls
 * to this function starting with \a index = 0.
 *
 * Time = O(1)
 *
 * \return NULL if \a index out of range.
 */
SLV2PluginClass
slv2_plugin_classes_get_at(SLV2PluginClasses classes,
                           unsigned          index);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_PLUGIN_CLASSES_H__ */

