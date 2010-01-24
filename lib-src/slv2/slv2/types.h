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

#ifndef __SLV2_TYPES_H__
#define __SLV2_TYPES_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>

#define SLV2_NAMESPACE_LV2      "http://lv2plug.in/ns/lv2core#"
#define SLV2_PORT_CLASS_PORT    "http://lv2plug.in/ns/lv2core#Port"
#define SLV2_PORT_CLASS_INPUT   "http://lv2plug.in/ns/lv2core#InputPort"
#define SLV2_PORT_CLASS_OUTPUT  "http://lv2plug.in/ns/lv2core#OutputPort"
#define SLV2_PORT_CLASS_CONTROL "http://lv2plug.in/ns/lv2core#ControlPort"
#define SLV2_PORT_CLASS_AUDIO   "http://lv2plug.in/ns/lv2core#AudioPort"
#define SLV2_PORT_CLASS_EVENT   "http://lv2plug.in/ns/ext/event#EventPort"
#define SLV2_EVENT_CLASS_MIDI   "http://lv2plug.in/ns/ext/midi#MidiEvent"


/** A port on a plugin.  Opaque, but valid to compare to NULL. */
typedef struct _SLV2Port* SLV2Port;


/** A plugin.  Opaque, but valid to compare to NULL. */
typedef struct _SLV2Plugin* SLV2Plugin;


/** A collection of plugins.  Opaque, but valid to compare to NULL. */
typedef void* SLV2Plugins;


/** The world.  Opaque, but valid to compare to NULL. */
typedef struct _SLV2World* SLV2World;


/** A plugin class.  Opaque, but valid to compare to NULL. */
typedef struct _SLV2PluginClass* SLV2PluginClass;


/** A collection of plugin classes.  Opaque, but valid to compare to NULL. */
typedef void* SLV2PluginClasses;


/** A typed value */
typedef struct _SLV2Value* SLV2Value;


/** A collection of typed values. */
typedef void* SLV2Values;


/** A scale point */
typedef struct _SLV2ScalePoint* SLV2ScalePoint;


/** A collection of scale points. */
typedef void* SLV2ScalePoints;


/** A plugin UI */
typedef struct _SLV2UI* SLV2UI;


/** A collection of plugin UIs. */
typedef void* SLV2UIs;


#ifdef __cplusplus
}
#endif


#endif /* __SLV2_TYPES_H__ */

