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

#ifndef __SLV2_PLUGIN_UI_H__
#define __SLV2_PLUGIN_UI_H__

#ifdef __cplusplus
extern "C" {
#endif

/** \addtogroup slv2_data
 * @{
 */


/** Get the URI of a Plugin UI.
 *
 * \param ui The Plugin UI
 *
 * Time = O(1)
 */
SLV2Value
slv2_ui_get_uri(SLV2UI ui);


/** Get the types (URIs of RDF classes) of a Plugin UI.
 *
 * \param ui The Plugin UI
 *
 * Time = O(1)
 */
SLV2Values
slv2_ui_get_classes(SLV2UI ui);


/** Check whether a plugin UI is a given type.
 *
 * \param ui        The Plugin UI
 * \param class_uri The URI of the LV2 UI type to check this UI against
 *
 * Time = O(1)
 */
bool
slv2_ui_is_a(SLV2UI ui, SLV2Value class_uri);


/** Get the URI for a Plugin UI's bundle.
 *
 * \param ui The Plugin UI
 *
 * Time = O(1)
 */
SLV2Value
slv2_ui_get_bundle_uri(SLV2UI ui);


/** Get the URI for a Plugin UI's shared library.
 *
 * \param ui The Plugin UI
 *
 * Time = O(1)
 */
SLV2Value
slv2_ui_get_binary_uri(SLV2UI ui);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_PLUGIN_UI_H__ */

