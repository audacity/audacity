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

#ifndef __SLV2_PORT_H__
#define __SLV2_PORT_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <slv2/types.h>
#include <slv2/plugin.h>
#include <slv2/port.h>
#include <slv2/values.h>

/** \addtogroup slv2_data
 * @{
 */


/** Port analog of slv2_plugin_get_value.
 *
 * Time = Query
 */
SLV2Values
slv2_port_get_value_by_qname(SLV2Plugin  plugin,
                             SLV2Port    port,
                             const char* property_uri);


/** Port analog of slv2_plugin_get_value_by_qname_i18n.
 *
 * Time = Query
 */
SLV2Values
slv2_port_get_value_by_qname_i18n(SLV2Plugin  plugin,
				  SLV2Port    port,
				  const char* property_uri);


/** Return the LV2 port properties of a port.
 *
 * Time = Query
 */
SLV2Values
slv2_port_get_properties(SLV2Plugin plugin,
                         SLV2Port   port);


/** Return whether a port has a certain property.
 *
 * Time = Query
 */
bool
slv2_port_has_property(SLV2Plugin p,
                       SLV2Port   port,
                       SLV2Value  property_uri);


/** Return whether a port is an event port and supports a certain event type.
 *
 * Time = Query
 */
bool
slv2_port_supports_event(SLV2Plugin p,
                         SLV2Port   port,
                         SLV2Value  event_uri);


/** Get the symbol of a port.
 *
 * The 'symbol' is a short string, a valid C identifier.
 * Returned value is owned by \a port and must not be freed.
 *
 * Time = Query
 */
SLV2Value
slv2_port_get_symbol(SLV2Plugin plugin,
                     SLV2Port   port);

/** Get the name of a port.
 *
 * This is guaranteed to return the untranslated name (the doap:name in the
 * data file without a language tag).  Returned value must be free()'d by
 * the caller.
 *
 * Time = Query
 */
SLV2Value
slv2_port_get_name(SLV2Plugin plugin,
                   SLV2Port   port);


/** Get all the classes of a port.
 *
 * This can be used to determine if a port is an input, output, audio,
 * control, midi, etc, etc, though it's simpler to use slv2_port_is_a.
 * The returned list does not include lv2:Port, which is implied.
 *
 * Returned value is shared and must not be destroyed by caller.
 *
 * Time = O(1)
 */
SLV2Values
slv2_port_get_classes(SLV2Plugin plugin,
                      SLV2Port   port);
                      

/** Determine if a port is of a given class (input, output, audio, etc).
 *
 * For convenience/performance/extensibility reasons, hosts are expected to
 * create an SLV2Value for each port class they "care about".  Well-known type
 * URI strings are defined (e.g. SLV2_PORT_CLASS_INPUT) for convenience, but
 * this function is designed so that SLV2 is usable with any port types
 * without requiring explicit support in SLV2.
 *
 * Time = O(n) (n pointer comparisons where n is the number of classes of
 * this port, so this method is suitable for realtime use on any sane port).
 */
bool
slv2_port_is_a(SLV2Plugin plugin,
               SLV2Port   port,
               SLV2Value  port_class);


/** Get the default, minimum, and maximum values of a port.
 *
 * @a def, @a min, and @a max are outputs, pass pointers to uninitialized
 * (i.e. NOT created with slv2_value_new) SLV2Value variables.  These will
 * be set to point at new values (which must be freed by the caller using
 * slv2_value_free), or NULL if the value does not exist.
 *
 * Time = Query
 */
void
slv2_port_get_range(SLV2Plugin plugin, 
                    SLV2Port   port,
                    SLV2Value* def,
                    SLV2Value* min,
                    SLV2Value* max);


/** Get the scale points (enumeration values) of a port.
 *
 * This returns a collection of 'interesting' named values of a port
 * (e.g. appropriate entries for a UI selector associated with this port).
 *
 * Returned value may be NULL if @a port has no scale points, otherwise it
 * must be freed by caller with slv2_scale_points_free.
 */
SLV2ScalePoints
slv2_port_get_scale_points(SLV2Plugin plugin,
                           SLV2Port   port);


/** @} */

#ifdef __cplusplus
}
#endif

#endif /* __SLV2_PORT_H__ */
