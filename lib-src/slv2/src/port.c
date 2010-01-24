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

#define _XOPEN_SOURCE 500
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <slv2/port.h>
#include <slv2/types.h>
#include <slv2/util.h>
#include <slv2/values.h>
#include <slv2/scalepoints.h>
#include "slv2_internal.h"


/* private */
SLV2Port
slv2_port_new(SLV2World world, uint32_t index, const char* symbol)
{
	struct _SLV2Port* port = malloc(sizeof(struct _SLV2Port));
	port->index = index;
	port->symbol = slv2_value_new(world, SLV2_VALUE_STRING, symbol);
	port->classes = slv2_values_new();
	//port->node_id = strdup(node_id);
	return port;
}


/* private */
void
slv2_port_free(SLV2Port port)
{
	slv2_values_free(port->classes);
	slv2_value_free(port->symbol);
	free(port);
}


/* private */
SLV2Port
slv2_port_duplicate(SLV2Port port)
{
	SLV2Port ret = malloc(sizeof(struct _SLV2Port));
	ret->index = port->index;
	ret->symbol = slv2_value_duplicate(port->symbol);
	return ret;
}


bool
slv2_port_is_a(SLV2Plugin plugin,
               SLV2Port   port,
               SLV2Value  port_class)
{
	for (unsigned i=0; i < slv2_values_size(port->classes); ++i)
		if (slv2_value_equals(slv2_values_get_at(port->classes, i), port_class))
			return true;

	return false;
}


bool
slv2_port_has_property(SLV2Plugin p,
                       SLV2Port   port,
                       SLV2Value  property)
{
	assert(property);

	SLV2Values results = NULL;

	char* query = slv2_strjoin(
			"SELECT DISTINCT ?port WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port ."
			"?port lv2:symbol \"", slv2_value_as_string(port->symbol), "\";\n",
			"      lv2:portProperty <", slv2_value_as_uri(property), "> .\n}", NULL);
			
	results = slv2_plugin_query_variable(p, query, 0);

	const bool ret = (slv2_values_size(results) > 0);

	free(query);
	free(results);
	
	return ret;
}


bool
slv2_port_supports_event(SLV2Plugin p,
                         SLV2Port   port,
                         SLV2Value  event)
{
	assert(event);

	char* query = slv2_strjoin(
			"ASK WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port ."
			"?port lv2:symbol \"", slv2_value_as_string(port->symbol), "\";\n",
			"      lv2ev:supportsEvent <", event, "> .\n"
			"}", NULL);
			
	librdf_query_results* results = slv2_plugin_query(p, query);
	assert(librdf_query_results_is_boolean(results));

	const bool ret = librdf_query_results_get_boolean(results);

	free(query);
	librdf_free_query_results(results);
	
	return ret;
}


SLV2Values
slv2_port_get_value_by_qname(SLV2Plugin  p,
                             SLV2Port    port,
                             const char* property)
{
	assert(property);
	SLV2Values results = NULL;

	char* query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port .\n"
			"?port lv2:symbol \"", slv2_value_as_string(port->symbol), "\";\n\t",
			property, " ?value .\n"
			"FILTER(lang(?value) = \"\") }", NULL);
			
	results = slv2_plugin_query_variable(p, query, 0);

	free(query);
	return results;
}


SLV2Values
slv2_port_get_value_by_qname_i18n(SLV2Plugin  p,
				  SLV2Port    port,
				  const char* property)
{
	assert(property);
	SLV2Values results = NULL;

	char* query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port .\n"
			"?port lv2:symbol \"", slv2_value_as_string(port->symbol), "\";\n\t",
			property, " ?value .\n"
			"FILTER(lang(?value) = \"", slv2_get_lang(), 
			"\") }", NULL);
	
	results = slv2_plugin_query_variable(p, query, 0);

	free(query);
	return results;
}


SLV2Value
slv2_port_get_symbol(SLV2Plugin p,
                     SLV2Port   port)
{
	return port->symbol;
}

	
SLV2Value
slv2_port_get_name(SLV2Plugin p,
                   SLV2Port   port)
{
	SLV2Value  ret     = NULL;
	SLV2Values results = slv2_port_get_value_by_qname_i18n(p, port, "lv2:name");

	if (results && slv2_values_size(results) > 0) {
	  ret = slv2_value_duplicate(slv2_values_get_at(results, 0));
	  slv2_values_free(results);
	}
	
	else {
	  results = slv2_port_get_value_by_qname(p, port, "lv2:name");
	  if (results && slv2_values_size(results) > 0) {
	    ret = slv2_value_duplicate(slv2_values_get_at(results, 0));
	  }
	  slv2_values_free(results);
	}

	return ret;
}

	
SLV2Values
slv2_port_get_classes(SLV2Plugin p,
                      SLV2Port   port)
{
	return port->classes;
}


void
slv2_port_get_range(SLV2Plugin p, 
                    SLV2Port   port,
                    SLV2Value* def,
                    SLV2Value* min,
                    SLV2Value* max)
{
	if (def)
		*def = NULL;
	if (min)
		*min = NULL;
	if (max)
		*max = NULL;

	char* query = slv2_strjoin(
			"SELECT DISTINCT ?def ?min ?max WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port .\n"
			"?port lv2:symbol \"", slv2_value_as_string(port->symbol), "\".\n",
			"OPTIONAL { ?port lv2:default ?def }\n",
			"OPTIONAL { ?port lv2:minimum ?min }\n",
			"OPTIONAL { ?port lv2:maximum ?max }\n",
			"\n}", NULL);
	
	librdf_query_results* results = slv2_plugin_query(p, query);

    while (!librdf_query_results_finished(results)) {
		librdf_node* def_node = librdf_query_results_get_binding_value(results, 0);
		librdf_node* min_node = librdf_query_results_get_binding_value(results, 1);
		librdf_node* max_node = librdf_query_results_get_binding_value(results, 2);

		if (def && def_node && !*def)
			*def = slv2_value_new_librdf_node(p->world, def_node);
		if (min && min_node && !*min)
			*min = slv2_value_new_librdf_node(p->world, min_node);
		if (max && max_node && !*max)
			*max = slv2_value_new_librdf_node(p->world, max_node);

		if ((!def || *def) && (!min || *min) && (!max || *max))
			break;

		librdf_query_results_next(results);
	}
			
	librdf_free_query_results(results);

	free(query);
}


SLV2ScalePoints
slv2_port_get_scale_points(SLV2Plugin p,
                           SLV2Port port)
{
	char* query = slv2_strjoin(
			"SELECT DISTINCT ?value ?label WHERE {\n"
			"<", slv2_value_as_uri(p->plugin_uri), "> lv2:port ?port .\n"
			"?port  lv2:symbol \"", slv2_value_as_string(port->symbol), "\" ;\n",
			"       lv2:scalePoint ?point .\n"
			"?point rdf:value ?value ;\n"
			"       rdfs:label ?label .\n"
			"\n} ORDER BY ?value", NULL);
	
	librdf_query_results* results = slv2_plugin_query(p, query);
	
	SLV2ScalePoints ret = NULL;

    if (!librdf_query_results_finished(results))
		ret = slv2_scale_points_new();

    while (!librdf_query_results_finished(results)) {
	
		librdf_node* value_node = librdf_query_results_get_binding_value(results, 0);
		librdf_node* label_node = librdf_query_results_get_binding_value(results, 1);

		SLV2Value value = slv2_value_new_librdf_node(p->world, value_node);
		SLV2Value label = slv2_value_new_librdf_node(p->world, label_node);

		raptor_sequence_push(ret, slv2_scale_point_new(value, label));
		
		librdf_query_results_next(results);
	}
			
	librdf_free_query_results(results);

	free(query);

	assert(!ret || slv2_values_size(ret) > 0);

	return ret;
}



SLV2Values
slv2_port_get_properties(SLV2Plugin p,
                         SLV2Port   port)
{
	return slv2_port_get_value_by_qname(p, port, "lv2:portProperty");
}

