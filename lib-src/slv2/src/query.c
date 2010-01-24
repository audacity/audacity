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
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <librdf.h>
#include <locale.h>
#include <limits.h>
#include <slv2/plugin.h>
#include <slv2/util.h>
#include <slv2/values.h>
#include "slv2_internal.h"


static const char* slv2_query_prefixes =
	"PREFIX rdf:    <http://www.w3.org/1999/02/22-rdf-syntax-ns#>\n"
	"PREFIX rdfs:   <http://www.w3.org/2000/01/rdf-schema#>\n"
	"PREFIX doap:   <http://usefulinc.com/ns/doap#>\n"
	"PREFIX foaf:   <http://xmlns.com/foaf/0.1/>\n"
	"PREFIX lv2:    <http://lv2plug.in/ns/lv2core#>\n"
	"PREFIX lv2ev:  <http://lv2plug.in/ns/ext/event#>\n";


SLV2Values
slv2_query_get_variable_bindings(SLV2World             world,
                                 librdf_query_results* results,
                                 int                   variable)
{
	SLV2Values result = NULL;

    if (!librdf_query_results_finished(results))
		result = slv2_values_new();

    while (!librdf_query_results_finished(results)) {

        librdf_node* node =
            librdf_query_results_get_binding_value(results, variable);

		if (!node) {
			fprintf(stderr, "SLV2 ERROR: Variable %d bound to NULL.\n", variable);
        	librdf_query_results_next(results);
			continue;
		}
		
		librdf_uri* datatype_uri = NULL;
		SLV2ValueType type = SLV2_VALUE_STRING;
		
		librdf_uri* uri_val = NULL;
		const char* str_val = NULL;

		switch (librdf_node_get_type(node)) {
		case LIBRDF_NODE_TYPE_RESOURCE:
			type = SLV2_VALUE_URI;
			uri_val = librdf_node_get_uri(node);
			assert(uri_val);
			break;
		case LIBRDF_NODE_TYPE_LITERAL:
			datatype_uri = librdf_node_get_literal_value_datatype_uri(node);
			if (datatype_uri) {
				if (!strcmp((const char*)librdf_uri_as_string(datatype_uri),
							"http://www.w3.org/2001/XMLSchema#integer"))
					type = SLV2_VALUE_INT;
				else if (!strcmp((const char*)librdf_uri_as_string(datatype_uri),
							"http://www.w3.org/2001/XMLSchema#decimal"))
					type = SLV2_VALUE_FLOAT;
				else
					fprintf(stderr, "Unknown datatype %s\n", librdf_uri_as_string(datatype_uri));
			}
			str_val = (const char*)librdf_node_get_literal_value(node);
			break;
		case LIBRDF_NODE_TYPE_BLANK:
			str_val = (const char*)librdf_node_get_blank_identifier(node);
			break;
		case LIBRDF_NODE_TYPE_UNKNOWN:
		default:
			fprintf(stderr, "Unknown variable binding type %d\n", variable);
			break;
		}
			
		if (uri_val)
			raptor_sequence_push(result, slv2_value_new_librdf_uri(world, uri_val));
		else if (str_val)
			raptor_sequence_push(result, slv2_value_new(world, type, str_val));

		librdf_free_node(node);

        librdf_query_results_next(results);
    }

    return result;
}


size_t
slv2_query_count_bindings(librdf_query_results* results)
{
	size_t count = 0;

    while (!librdf_query_results_finished(results)) {
		++count;
        librdf_query_results_next(results);
    }

    return count;
}

	
librdf_query_results*
slv2_plugin_query(SLV2Plugin  plugin,
                  const char* sparql_str)
{
	if (!plugin->rdf)
		slv2_plugin_load(plugin);

	librdf_uri* base_uri = slv2_value_as_librdf_uri(plugin->plugin_uri);

	char* query_str = slv2_strjoin(slv2_query_prefixes, sparql_str, NULL);

	//printf("******** Query \n%s********\n", query_str);
	
	librdf_query* query = librdf_new_query(plugin->world->world, "sparql", NULL,
			(const unsigned char*)query_str, base_uri);
	
	if (!query) {
		fprintf(stderr, "ERROR: Could not create query\n");
		return NULL;
	}
	
	// FIXME: locale kludges to work around librdf bug
	char* locale = strdup(setlocale(LC_NUMERIC, NULL));

	setlocale(LC_NUMERIC, "POSIX");
	librdf_query_results* results = librdf_query_execute(query, plugin->rdf);
	setlocale(LC_NUMERIC, locale);
	
	free(locale);
	
	librdf_free_query(query);
	free(query_str);

	return results;
}


/** Query a single variable */
SLV2Values
slv2_plugin_query_variable(SLV2Plugin  plugin,
                           const char* sparql_str,
                           unsigned    variable)
{
	assert(variable < INT_MAX);

	librdf_query_results* results = slv2_plugin_query(plugin, sparql_str);

	SLV2Values ret = slv2_query_get_variable_bindings(plugin->world, results, (int)variable);
	
	librdf_free_query_results(results);

	return ret;
}


/** Run a query and count number of matches.
 *
 * More efficient than slv2_plugin_simple_query if you're only interested
 * in the number of results (ie slv2_plugin_num_ports).
 * 
 * Note the result of this function is probably meaningless unless the query
 * is a SELECT DISTINCT.
 */
unsigned
slv2_plugin_query_count(SLV2Plugin  plugin,
                        const char* sparql_str)
{
	librdf_query_results* results = slv2_plugin_query(plugin, sparql_str);

	unsigned ret = 0;

	if (results) {
		ret = slv2_query_count_bindings(results);
		librdf_free_query_results(results);
	}

	return ret;
}

