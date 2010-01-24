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

#ifndef __SLV2_INTERNAL_H__
#define __SLV2_INTERNAL_H__

#ifdef __cplusplus
extern "C" {
#endif

#include <stdbool.h>
#include <stddef.h>
#include <stdlib.h>
#include <inttypes.h>
#include <librdf.h>
#include <slv2/types.h>
#include <slv2/lv2_ui.h>



/* ********* PORT ********* */


/** Reference to a port on some plugin. */
struct _SLV2Port {
	uint32_t   index;   ///< lv2:index
	SLV2Value  symbol;  ///< lv2:symbol
	SLV2Values classes; ///< rdf:type
};


SLV2Port slv2_port_new(SLV2World world, uint32_t index, const char* symbol);
SLV2Port slv2_port_duplicate(SLV2Port port);
void     slv2_port_free(SLV2Port port);



/* ********* Plugin ********* */


/** Record of an installed/available plugin.
 *
 * A simple reference to a plugin somewhere on the system. This just holds
 * paths of relevant files, the actual data therein isn't loaded into memory.
 */
struct _SLV2Plugin {
	struct _SLV2World*   world;
	SLV2Value            plugin_uri;
	SLV2Value            bundle_uri; ///< Bundle directory plugin was loaded from
	SLV2Value            binary_uri; ///< lv2:binary
	SLV2PluginClass      plugin_class;
	raptor_sequence*     data_uris;  ///< rdfs::seeAlso
	raptor_sequence*     ports;
	librdf_storage*      storage;
	librdf_model*        rdf;
};

SLV2Plugin slv2_plugin_new(SLV2World world, SLV2Value uri, librdf_uri* bundle_uri);
void       slv2_plugin_load(SLV2Plugin p);
void       slv2_plugin_free(SLV2Plugin plugin);
void       slv2_plugin_get_port_float_values(SLV2Plugin  p,
					     const char* qname,
					     float*      values);

librdf_query_results* slv2_plugin_query(SLV2Plugin  plugin,
                                        const char* sparql_str);



/* ********* Plugins ********* */


/** Create a new, empty plugin list.
 *
 * Returned object must be freed with slv2_plugins_free.
 */
SLV2Plugins
slv2_plugins_new();



/* ********* Instance ********* */


/** Pimpl portion of SLV2Instance */
struct _InstanceImpl {
	void* lib_handle;
};


/* ********* UI Instance ********* */
struct _SLV2UIInstanceImpl {
	void*                   lib_handle;
	const LV2UI_Descriptor* lv2ui_descriptor;
	LV2UI_Handle            lv2ui_handle;
	LV2UI_Widget            widget;
};


/* ********* Plugin Class ********* */


struct _SLV2PluginClass {
	struct _SLV2World* world;
	SLV2Value          parent_uri;
	SLV2Value          uri;
	SLV2Value          label;
};

SLV2PluginClass slv2_plugin_class_new(SLV2World world, librdf_uri* parent_uri,
                                      librdf_uri* uri, const char* label);
void slv2_plugin_class_free(SLV2PluginClass plugin_class);



/* ********* Plugin Classes ********* */


SLV2PluginClasses slv2_plugin_classes_new();
void              slv2_plugin_classes_free();



/* ********* World ********* */

/** Model of LV2 (RDF) data loaded from bundles.
 */
struct _SLV2World {
	bool              local_world;
	librdf_world*     world;
	librdf_storage*   storage;
	librdf_model*     model;
	librdf_parser*    parser;
	SLV2PluginClass   lv2_plugin_class;
	SLV2PluginClasses plugin_classes;
	SLV2Plugins       plugins;
	librdf_node*      lv2_specification_node;
	librdf_node*      lv2_plugin_node;
	librdf_node*      rdf_a_node;
	librdf_node*      xsd_integer_node;
	librdf_node*      xsd_decimal_node;
};

/** Load all bundles found in \a search_path.
 *
 * \param search_path A colon-delimited list of directories.  These directories
 * should contain LV2 bundle directories (ie the search path is a list of
 * parent directories of bundles, not a list of bundle directories).
 *
 * If \a search_path is NULL, \a world will be unmodified.
 * Use of this function is \b not recommended.  Use \ref slv2_world_load_all.
 */
void
slv2_world_load_path(SLV2World   world,
                     const char* search_path);

	
void
slv2_world_load_specifications(SLV2World world);

void
slv2_world_load_file(SLV2World world, librdf_uri* file_uri);


/* ********* Plugin UI ********* */

struct _SLV2UI {
	struct _SLV2World* world;
	SLV2Value          uri;
	SLV2Value          bundle_uri;
	SLV2Value          binary_uri;
	SLV2Values         classes;
};

SLV2UIs slv2_uis_new();
SLV2UI
slv2_ui_new(SLV2World   world,
            librdf_uri* uri,
            librdf_uri* type_uri,
            librdf_uri* binary_uri);

void slv2_ui_free(SLV2UI ui);


/* ********* Value ********* */

typedef enum _SLV2ValueType {
	SLV2_VALUE_URI,
	SLV2_VALUE_QNAME,
	SLV2_VALUE_STRING,
	SLV2_VALUE_INT,
	SLV2_VALUE_FLOAT,
} SLV2ValueType;

struct _SLV2Value {
	SLV2ValueType type;
	char*         str_val; ///< always present
	union {
		int         int_val;
		float       float_val;
		librdf_uri* uri_val;
	} val;
};

SLV2Value   slv2_value_new(SLV2World world, SLV2ValueType type, const char* val);
SLV2Value   slv2_value_new_librdf_node(SLV2World world, librdf_node* node);
SLV2Value   slv2_value_new_librdf_uri(SLV2World world, librdf_uri* uri);
void        slv2_value_set_numerics_from_string(SLV2Value val);
librdf_uri* slv2_value_as_librdf_uri(SLV2Value value);

	
/* ********* Values ********* */

void slv2_values_set_at(SLV2Values list, unsigned index, void* value);


/* ********* Scale Points ********* */

struct _SLV2ScalePoint {
	SLV2Value value;
	SLV2Value label;
};

SLV2ScalePoint slv2_scale_point_new(SLV2Value value, SLV2Value label);
void           slv2_scale_point_free(SLV2ScalePoint point);


/* String utility functions */

char* slv2_strjoin(const char* first, ...);

/* I18N utility functions */
char* slv2_get_lang();


#ifdef __cplusplus
}
#endif

#endif /* __SLV2_INTERNAL_H__ */

