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

#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <librdf.h>
#include "slv2_internal.h"
#include <slv2/plugin.h>
#include <slv2/types.h>
#include <slv2/util.h>
#include <slv2/values.h>
#include <slv2/pluginclass.h>
#include <slv2/pluginclasses.h>
#include <slv2/pluginuis.h>


/* private
 * ownership of uri is taken */
SLV2Plugin
slv2_plugin_new(SLV2World world, SLV2Value uri, librdf_uri* bundle_uri)
{
	assert(bundle_uri);
	struct _SLV2Plugin* plugin = malloc(sizeof(struct _SLV2Plugin));
	plugin->world = world;
	plugin->plugin_uri = uri;
	plugin->bundle_uri = slv2_value_new_librdf_uri(world, bundle_uri);
	plugin->binary_uri = NULL;
	plugin->plugin_class = NULL;
	plugin->data_uris = slv2_values_new();
	plugin->ports = raptor_new_sequence((void (*)(void*))&slv2_port_free, NULL);
	plugin->storage = NULL;
	plugin->rdf = NULL;

	return plugin;
}


/* private */
void
slv2_plugin_free(SLV2Plugin p)
{
	slv2_value_free(p->plugin_uri);
	p->plugin_uri = NULL;
	
	slv2_value_free(p->bundle_uri);
	p->bundle_uri = NULL;
	
	slv2_value_free(p->binary_uri);
	p->binary_uri = NULL;
	
	raptor_free_sequence(p->ports);
	p->ports = NULL;

	if (p->rdf) {
		librdf_free_model(p->rdf);
		p->rdf = NULL;
	}
	
	if (p->storage) {
		librdf_free_storage(p->storage);
		p->storage = NULL;
	}
	
	slv2_values_free(p->data_uris);
	p->data_uris = NULL;

	free(p);
}


/** comparator for sorting */
int
slv2_port_compare_by_index(const void* a, const void* b)
{
	SLV2Port port_a = *(SLV2Port*)a;
	SLV2Port port_b = *(SLV2Port*)b;

	if (port_a->index < port_b->index)
		return -1;
	else if (port_a->index == port_b->index)
		return 0;
	else //if (port_a->index > port_b->index)
		return 1;
}


void
slv2_plugin_load(SLV2Plugin p)
{
	//printf("Loading cache for %s\n", (const char*)librdf_uri_as_string(p->plugin_uri));

	if (!p->storage) {
		assert(!p->rdf);
		//p->storage = librdf_new_storage(p->world->world, "hashes", NULL,
		//		"hash-type='memory'");
		p->storage = librdf_new_storage(p->world->world, "trees", NULL, NULL);
		if (!p->storage)
			p->storage = librdf_new_storage(p->world->world, "memory", NULL, NULL);
		p->rdf = librdf_new_model(p->world->world, p->storage, NULL);
	}

	// Parse all the plugin's data files into RDF model
	for (unsigned i=0; i < slv2_values_size(p->data_uris); ++i) {
		SLV2Value data_uri_val = slv2_values_get_at(p->data_uris, i);
		librdf_uri* data_uri = librdf_new_uri(p->world->world,
				(const unsigned char*)slv2_value_as_uri(data_uri_val));
		librdf_parser_parse_into_model(p->world->parser, data_uri, NULL, p->rdf);
		librdf_free_uri(data_uri);
	}

	// Load plugin_class
	const unsigned char* query = (const unsigned char*)
		"SELECT DISTINCT ?class WHERE { <> a ?class }";
	
	librdf_query* q = librdf_new_query(p->world->world, "sparql",
		NULL, query, slv2_value_as_librdf_uri(p->plugin_uri));
	
	librdf_query_results* results = librdf_query_execute(q, p->rdf);
		
	while (!librdf_query_results_finished(results)) {
		librdf_node* class_node = librdf_query_results_get_binding_value(results, 0);
		librdf_uri*  class_uri  = librdf_node_get_uri(class_node);

		SLV2Value class = slv2_value_new_librdf_uri(p->world, class_uri);
		
		if ( ! slv2_value_equals(class, p->world->lv2_plugin_class->uri)) {

			SLV2PluginClass plugin_class = slv2_plugin_classes_get_by_uri(
					p->world->plugin_classes, class);
			

			librdf_free_node(class_node);

			if (plugin_class) {
				p->plugin_class = plugin_class;
				slv2_value_free(class);
				break;
			}
		}

		slv2_value_free(class);
		librdf_query_results_next(results);
	}
	
	if (p->plugin_class == NULL)
		p->plugin_class = p->world->lv2_plugin_class;

	librdf_free_query_results(results);
	librdf_free_query(q);
	
	// Load ports
	query = (const unsigned char*)
		"PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
		"SELECT DISTINCT ?type ?symbol ?index WHERE {\n"
		"<>    :port    ?port .\n"
		"?port a        ?type ;\n"
		"      :symbol  ?symbol ;\n"
		"      :index   ?index .\n"
		"} ORDER BY (?index)";
	
	q = librdf_new_query(p->world->world, "sparql",
		NULL, query, slv2_value_as_librdf_uri(p->plugin_uri));
	
	results = librdf_query_execute(q, p->rdf);

	int num_ports = 0;
	int last_index = -1;

	while (!librdf_query_results_finished(results)) {
	
		librdf_node* type_node = librdf_query_results_get_binding_value(results, 0);
		librdf_node* symbol_node = librdf_query_results_get_binding_value(results, 1);
		librdf_node* index_node = librdf_query_results_get_binding_value(results, 2);

		assert(librdf_node_is_literal(symbol_node));
		assert(librdf_node_is_literal(index_node));

		const char* symbol = (const char*)librdf_node_get_literal_value(symbol_node);
		const char* index = (const char*)librdf_node_get_literal_value(index_node);

		//printf("PORT: %s %s %s\n", type, index, symbol);

		const int this_index = atoi(index);
		SLV2Port  this_port  = NULL;
		
		// Create a new SLV2Port, and add to template
		if (this_index == num_ports) {
			assert(this_index == last_index + 1);
			this_port = slv2_port_new(p->world, (unsigned)atoi(index), symbol);
			raptor_sequence_push(p->ports, this_port);
			++num_ports;
			++last_index;

		// More information about a port we already created
		} else if (this_index < num_ports) {
			this_port = slv2_plugin_get_port_by_index(p, this_index);
		
		// Got a port index out of whack, plugin or rasqal is broken
		} else {
			fprintf(stderr, "ERROR: Found port %d immediately after port %d\n",
					this_index, num_ports-1);
			fprintf(stderr, "Either the plugin %s or your version of rasqal is broken.\n",
					slv2_value_as_uri(p->plugin_uri));
			fprintf(stderr, "Please report (with rasqal version): http://dev.drobilla.net/newticket?component=SLV2\n");
		}
			
		if (this_port) {
			raptor_sequence_push(this_port->classes,
					slv2_value_new_librdf_uri(p->world, librdf_node_get_uri(type_node)));
		}

		librdf_free_node(type_node);
		librdf_free_node(symbol_node);
		librdf_free_node(index_node);
		
		librdf_query_results_next(results);
	}
	
	// Not necessary due to ORDER BY clause
	//raptor_sequence_sort(p->ports, slv2_port_compare_by_index);
	
	librdf_free_query_results(results);
	librdf_free_query(q);

	// Load binary URI
	query = (const unsigned char*)
		"PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
		"SELECT ?binary WHERE { <> :binary ?binary . }";
	
	q = librdf_new_query(p->world->world, "sparql",
		NULL, query, slv2_value_as_librdf_uri(p->plugin_uri));
	
	results = librdf_query_execute(q, p->rdf);

	if (!librdf_query_results_finished(results)) {
		librdf_node* binary_node = librdf_query_results_get_binding_value(results, 0);
		librdf_uri* binary_uri = librdf_node_get_uri(binary_node);

		SLV2Value binary = slv2_value_new_librdf_uri(p->world, binary_uri);
		p->binary_uri = binary;

		librdf_free_node(binary_node);
	}
	
	librdf_free_query_results(results);
	librdf_free_query(q);
}


SLV2Value
slv2_plugin_get_uri(SLV2Plugin p)
{
	assert(p);
	assert(p->plugin_uri);
	return p->plugin_uri;
}


SLV2Value
slv2_plugin_get_bundle_uri(SLV2Plugin p)
{
	assert(p);
	assert(p->bundle_uri);
	return p->bundle_uri;
}


SLV2Value
slv2_plugin_get_library_uri(SLV2Plugin p)
{
	assert(p);
	if (!p->binary_uri && !p->rdf)
		slv2_plugin_load(p);
	return p->binary_uri;
}


SLV2Values
slv2_plugin_get_data_uris(SLV2Plugin p)
{
	return p->data_uris;
}


SLV2PluginClass
slv2_plugin_get_class(SLV2Plugin p)
{
	// FIXME: Typical use case this will bring every single plugin model
	// into memory
	
	if (!p->rdf)
		slv2_plugin_load(p);

	return p->plugin_class;
}


bool
slv2_plugin_verify(SLV2Plugin plugin)
{
	char* query_str = 
		"SELECT DISTINCT ?type ?name ?license ?port WHERE {\n"
		"<> a ?type ;\n"
		"doap:name    ?name ;\n"
		"doap:license ?license ;\n"
		"lv2:port     [ lv2:index ?port ] .\n}";

	librdf_query_results* results = slv2_plugin_query(plugin, query_str);

	bool has_type    = false;
	bool has_name    = false;
	bool has_license = false;
	bool has_port    = false;

	while (!librdf_query_results_finished(results)) {
		librdf_node* type_node = librdf_query_results_get_binding_value(results, 0);
		const char* const type_str = (const char*)librdf_node_get_literal_value(type_node);
		librdf_node* name_node = librdf_query_results_get_binding_value(results, 1);
		librdf_node* license_node = librdf_query_results_get_binding_value(results, 2);
		librdf_node* port_node = librdf_query_results_get_binding_value(results, 3);

		if (!strcmp(type_str, "http://lv2plug.in/ns/lv2core#Plugin"))
			has_type = true;
		
		if (name_node)
			has_name = true;
		
		if (license_node)
			has_license = true;
		
		if (port_node)
			has_port = true;

		librdf_free_node(type_node);
		librdf_free_node(name_node);
		librdf_free_node(license_node);
		librdf_free_node(port_node);

		librdf_query_results_next(results);
	}

	librdf_free_query_results(results);

	if ( ! (has_type && has_name && has_license && has_port) ) {
		fprintf(stderr, "Invalid LV2 Plugin %s\n",
				slv2_value_as_uri(slv2_plugin_get_uri(plugin)));
		return false;
	} else {
		return true;
	}
}


SLV2Value
slv2_plugin_get_name(SLV2Plugin plugin)
{
	SLV2Values results = slv2_plugin_get_value_by_qname_i18n(plugin, "doap:name");
	SLV2Value  ret     = NULL;
	
	if (results) {
		SLV2Value val = slv2_values_get_at(results, 0);
		if (slv2_value_is_string(val))
			ret = slv2_value_duplicate(val);
		slv2_values_free(results);
	}
	
	else {
	  results = slv2_plugin_get_value_by_qname(plugin, "doap:name");
	  SLV2Value val = slv2_values_get_at(results, 0);
	  if (slv2_value_is_string(val))
	    ret = slv2_value_duplicate(val);
	  slv2_values_free(results);
	}

	return ret;
}


SLV2Values
slv2_plugin_get_value(SLV2Plugin p,
                      SLV2Value  predicate)
{
	char* query = NULL;
	
	/* Hack around broken RASQAL, full URI predicates don't work :/ */

	if (predicate->type == SLV2_VALUE_URI) {
		query = slv2_strjoin(
			"PREFIX slv2predicate: <", slv2_value_as_string(predicate), ">",
			"SELECT DISTINCT ?value WHERE { \n"
			"<> slv2predicate: ?value \n"
			"}\n", NULL);
	} else if (predicate->type == SLV2_VALUE_QNAME) {
    	query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE { \n"
			"<> ", slv2_value_as_string(predicate), " ?value \n"
			"}\n", NULL);
	} else {
		fprintf(stderr, "slv2_plugin_get_value error: "
				"predicate is not a URI or QNAME\n");
		return NULL;
	}

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	free(query);

	return result;
}


/* internal */
SLV2Values
slv2_plugin_get_value_by_qname(SLV2Plugin  p,
                               const char* predicate)
{
	char* query = NULL;
	
    query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE { \n"
			"<> ", predicate, " ?value . \n"
			"FILTER(lang(?value) = \"\") \n"
			"}\n", NULL);

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	free(query);

	return result;
}

	
/* internal, get i18nd value if possible */
SLV2Values
slv2_plugin_get_value_by_qname_i18n(SLV2Plugin  p,
				    const char* predicate)
{
	char* query = NULL;
	
	query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE { \n"
			"<> ", predicate, " ?value . \n"
			"FILTER(lang(?value) = \"", slv2_get_lang(), "\") \n"
			"}\n", NULL);

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	free(query);

	return result;
}

	
SLV2Values
slv2_plugin_get_value_for_subject(SLV2Plugin  p,
                                  SLV2Value   subject,
                                  SLV2Value   predicate)
{
	if ( ! slv2_value_is_uri(subject)) {
		fprintf(stderr, "slv2_plugin_get_value_for_subject error: "
				"subject is not a URI\n");
		return NULL;
	}

	char* query = NULL;

	/* Hack around broken RASQAL, full URI predicates don't work :/ */

	char* subject_token = slv2_value_get_turtle_token(subject);

	if (predicate->type == SLV2_VALUE_URI) {
		query = slv2_strjoin(
			"PREFIX slv2predicate: <", predicate, ">",
			"SELECT DISTINCT ?value WHERE { \n",
			subject_token, " slv2predicate: ?value \n"
			"}\n", NULL);
	} else if (predicate->type == SLV2_VALUE_URI) {
    	query = slv2_strjoin(
			"SELECT DISTINCT ?value WHERE { \n",
			subject_token, " ", predicate, " ?value \n"
			"}\n", NULL);
	} else {
		fprintf(stderr, "slv2_plugin_get_value error: "
				"predicate is not a URI or QNAME\n");
		free(subject_token);
		return NULL;
	}

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	free(query);
	free(subject_token);

	return result;
}


SLV2Values
slv2_plugin_get_properties(SLV2Plugin p)
{
	return slv2_plugin_get_value_by_qname(p, "lv2:pluginProperty");
}


SLV2Values
slv2_plugin_get_hints(SLV2Plugin p)
{
	return slv2_plugin_get_value_by_qname(p, "lv2:pluginHint");
}


uint32_t
slv2_plugin_get_num_ports(SLV2Plugin p)
{
	if (!p->rdf)
		slv2_plugin_load(p);
	
	return raptor_sequence_size(p->ports);
}


void
slv2_plugin_get_port_float_values(SLV2Plugin  p,
                                  const char* qname,
                                  float*      values)
{
	if (!p->rdf)
		slv2_plugin_load(p);

	const unsigned char* query;
	librdf_query* q;
	librdf_query_results* results;

	for (int i = 0; i < raptor_sequence_size(p->ports); ++i)
		values[i] = NAN;

	query = (const unsigned char*)slv2_strjoin(
			"PREFIX : <http://lv2plug.in/ns/lv2core#>\n"
			"SELECT DISTINCT ?index ?value WHERE {\n"
			"<>    :port    ?port .\n"
			"?port :index   ?index .\n"
			"?port ", qname, " ?value .\n"
			"} ", NULL);

	q = librdf_new_query(p->world->world, "sparql",
			NULL, query, slv2_value_as_librdf_uri(p->plugin_uri));

	results = librdf_query_execute(q, p->rdf);

	while (!librdf_query_results_finished(results)) {
		librdf_node* idx_node = librdf_query_results_get_binding_value(results, 0);
		librdf_node* val_node = librdf_query_results_get_binding_value(results, 1);
		assert(librdf_node_is_literal(idx_node));
		assert(librdf_node_is_literal(val_node));
		const int idx = atoi((const char*)librdf_node_get_literal_value(idx_node));
		const float val = atof((const char*)librdf_node_get_literal_value(val_node));
		values[idx] = val;
		librdf_free_node(idx_node);
		librdf_free_node(val_node);
		librdf_query_results_next(results);
	}

	librdf_free_query_results(results);
	librdf_free_query(q);
}


void
slv2_plugin_get_port_ranges_float(SLV2Plugin p, 
                                  float*     min_values, 
                                  float*     max_values,
                                  float*     def_values)
{
	if (min_values)
		slv2_plugin_get_port_float_values(p, ":minimum", min_values);

	if (max_values)
		slv2_plugin_get_port_float_values(p, ":maximum", max_values);

	if (def_values)
		slv2_plugin_get_port_float_values(p, ":default", def_values);
}


uint32_t
slv2_plugin_get_num_ports_of_class(SLV2Plugin p,
                                   SLV2Value  class_1, ...)
{
	uint32_t ret = 0;
	va_list  args;

	for (unsigned i=0; i < slv2_plugin_get_num_ports(p); ++i) {
		SLV2Port port = raptor_sequence_get_at(p->ports, i);
		if (!slv2_port_is_a(p, port, class_1))
			continue;

		va_start(args, class_1);
		
		bool matches = true;
		for (SLV2Value class_i = NULL; (class_i = va_arg(args, SLV2Value)) != NULL ; ) {
			if (!slv2_port_is_a(p, port, class_i)) {
				va_end(args);
				matches = false;
				break;
			}
		}

		if (matches)
			++ret;

		va_end(args);
	}

	return ret;
}


bool
slv2_plugin_has_latency(SLV2Plugin p)
{
    const char* const query = 
		"SELECT ?index WHERE {\n"
		"	<>      lv2:port         ?port .\n"
		"	?port   lv2:portProperty lv2:reportsLatency ;\n"
		"           lv2:index        ?index .\n"
		"}\n";

	SLV2Values results = slv2_plugin_query_variable(p, query, 0);
	const bool latent = (slv2_values_size(results) > 0);
	slv2_values_free(results);
	
	return latent;
}


uint32_t
slv2_plugin_get_latency_port_index(SLV2Plugin p)
{
    const char* const query = 
		"SELECT ?index WHERE {\n"
		"	<>      lv2:port         ?port .\n"
		"	?port   lv2:portProperty lv2:reportsLatency ;\n"
		"           lv2:index        ?index .\n"
		"}\n";

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	// FIXME: need a sane error handling strategy
	assert(slv2_values_size(result) > 0);
	SLV2Value val = slv2_values_get_at(result, 0);
	assert(slv2_value_is_int(val));

	return slv2_value_as_int(val);
}

	
bool
slv2_plugin_has_feature(SLV2Plugin p,
                        SLV2Value  feature)
{
	SLV2Values features = slv2_plugin_get_supported_features(p);
	
	const bool ret = features && feature && slv2_values_contains(features, feature);

	slv2_values_free(features);
	return ret;
}


SLV2Values
slv2_plugin_get_supported_features(SLV2Plugin p)
{
    const char* const query = 
		"SELECT DISTINCT ?feature WHERE {\n"
		"	{ <>  lv2:optionalFeature ?feature }\n"
		"	UNION\n"
		"	{ <>  lv2:requiredFeature ?feature }\n"
		"}\n";

	SLV2Values result = slv2_plugin_query_variable(p, query, 0);
	
	return result;
}


SLV2Values
slv2_plugin_get_optional_features(SLV2Plugin p)
{
	return slv2_plugin_get_value_by_qname(p, "lv2:optionalFeature");
}


SLV2Values
slv2_plugin_get_required_features(SLV2Plugin p)
{
	return slv2_plugin_get_value_by_qname(p, "lv2:requiredFeature");
}


SLV2Port
slv2_plugin_get_port_by_index(SLV2Plugin p,
                              uint32_t   index)
{
	if (!p->rdf)
		slv2_plugin_load(p);
	
	return raptor_sequence_get_at(p->ports, (int)index);
}


SLV2Port
slv2_plugin_get_port_by_symbol(SLV2Plugin p,
                               SLV2Value  symbol)
{
	if (!p->rdf)
		slv2_plugin_load(p);
	
	for (int i=0; i < raptor_sequence_size(p->ports); ++i) {
		SLV2Port port = raptor_sequence_get_at(p->ports, i);
		if (slv2_value_equals(port->symbol, symbol))
			return port;
	}

	return NULL;
}


SLV2Value
slv2_plugin_get_author_name(SLV2Plugin plugin)
{
	SLV2Value ret = NULL;

    const char* const query = 
		"SELECT ?name WHERE {\n"
		"	<>      doap:maintainer ?maint . \n"
		"	?maint  foaf:name ?name . \n"
		"}\n";

	SLV2Values results = slv2_plugin_query_variable(plugin, query, 0);
	
	if (results && slv2_values_size(results) > 0) {
		SLV2Value val = slv2_values_get_at(results, 0);
		if (slv2_value_is_string(val))
			ret = slv2_value_duplicate(val);
	}

	if (results)
		slv2_values_free(results);

	return ret;
}


SLV2Value
slv2_plugin_get_author_email(SLV2Plugin plugin)
{
	SLV2Value ret = NULL;

    const char* const query = 
		"SELECT ?email WHERE {\n"
		"	<>      doap:maintainer ?maint . \n"
		"	?maint  foaf:mbox ?email . \n"
		"}\n";
	
	SLV2Values results = slv2_plugin_query_variable(plugin, query, 0);
	
	if (results && slv2_values_size(results) > 0) {
		SLV2Value val = slv2_values_get_at(results, 0);
		if (slv2_value_is_uri(val))
			ret = slv2_value_duplicate(val);
	}

	if (results)
		slv2_values_free(results);

	return ret;
}

	
SLV2Value
slv2_plugin_get_author_homepage(SLV2Plugin plugin)
{
	SLV2Value ret = NULL;

    const char* const query = 
		"SELECT ?page WHERE {\n"
		"	<>      doap:maintainer ?maint . \n"
		"	?maint  foaf:homepage ?page . \n"
		"}\n";
	
	SLV2Values results = slv2_plugin_query_variable(plugin, query, 0);
	
	if (results && slv2_values_size(results) > 0) {
		SLV2Value val = slv2_values_get_at(results, 0);
		if (slv2_value_is_uri(val))
			ret = slv2_value_duplicate(val);
	}

	if (results)
		slv2_values_free(results);

	return ret;
}


SLV2UIs
slv2_plugin_get_uis(SLV2Plugin plugin)
{
    const char* const query_str =
		"PREFIX uiext: <http://lv2plug.in/ns/extensions/ui#>\n"
		"SELECT DISTINCT ?uri ?type ?binary WHERE {\n"
		"<>   uiext:ui     ?uri .\n"
		"?uri a            ?type ;\n"
		"     uiext:binary ?binary .\n"
		"}\n";

	librdf_query_results* results = slv2_plugin_query(plugin, query_str);

	SLV2UIs result = slv2_uis_new();

	while (!librdf_query_results_finished(results)) {
		librdf_node* uri_node    = librdf_query_results_get_binding_value(results, 0);
		librdf_node* type_node   = librdf_query_results_get_binding_value(results, 1);
		librdf_node* binary_node = librdf_query_results_get_binding_value(results, 2);

		SLV2UI ui = slv2_ui_new(plugin->world,
				librdf_node_get_uri(uri_node),
				librdf_node_get_uri(type_node),
				librdf_node_get_uri(binary_node));

		raptor_sequence_push(result, ui);

		librdf_free_node(uri_node);
		librdf_free_node(type_node);
		librdf_free_node(binary_node);

		librdf_query_results_next(results);
	}

	librdf_free_query_results(results);

	if (slv2_uis_size(result) > 0) {
		return result;
	} else {
		slv2_uis_free(result);
		return NULL;
	}
}

