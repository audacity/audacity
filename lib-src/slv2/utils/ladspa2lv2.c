/* ladspa2lv2
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 *  
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdbool.h>
#include <float.h>
#include <math.h>
#include <dlfcn.h>
#include <librdf.h>
#include "ladspa.h"

#define U(x) ((const unsigned char*)(x))
#define NS_RDF(x) "http://www.w3.org/1999/02/22-rdf-syntax-ns#" x
#define NS_LV2(x) "http://lv2plug.in/ns/lv2core#" x
#define NS_DOAP(x) "http://usefulinc.com/ns/doap#" x

librdf_world* world = NULL;


void
add_resource(librdf_model* model,
             librdf_node*  subject,
             const char*   predicate_uri,
             const char*   object_uri)
{
	librdf_node* predicate = librdf_new_node_from_uri_string(world, U(predicate_uri));
	librdf_node* object    = librdf_new_node_from_uri_string(world, U(object_uri));
	
	librdf_statement* triple = librdf_new_statement_from_nodes(world, subject, predicate, object);
	
	librdf_model_add_statement(model, triple);

	//librdf_free_statement(triple);
}


void
add_node(librdf_model* model,
             librdf_node*  subject,
             const char*   predicate_uri,
             librdf_node*  object)
{
	librdf_node* predicate = librdf_new_node_from_uri_string(world, U(predicate_uri));
	
	librdf_statement* triple = librdf_new_statement_from_nodes(world, subject, predicate, object);
	
	librdf_model_add_statement(model, triple);

	//librdf_free_statement(triple);
}


void
add_string(librdf_model* model,
           librdf_node*  subject,
           const char*   predicate_uri,
           const char*   object_string)
{
	librdf_node* predicate = librdf_new_node_from_uri_string(world, U(predicate_uri));
	librdf_node* object    = librdf_new_node_from_literal(world, U(object_string), NULL, 0);
	
	librdf_statement* triple = librdf_new_statement_from_nodes(world, subject, predicate, object);
	
	librdf_model_add_statement(model, triple);

	//librdf_free_statement(triple);
}


void
add_int(librdf_model* model,
        librdf_node*  subject,
        const char*   predicate_uri,
        int           object_int)
{
	static const size_t MAX_LEN = 21; // strlen(2^64) + 1
	char object_str[MAX_LEN];
	snprintf(object_str, MAX_LEN, "%d", object_int);

	librdf_uri* type = librdf_new_uri(world, U("http://www.w3.org/2001/XMLSchema#integer"));

	librdf_node* predicate = librdf_new_node_from_uri_string(world, U(predicate_uri));
	librdf_node* object    = librdf_new_node_from_typed_literal(world, U(object_str), NULL, type);
	
	librdf_statement* triple = librdf_new_statement_from_nodes(world, subject, predicate, object);
	
	librdf_model_add_statement(model, triple);

	//librdf_free_statement(triple);
	librdf_free_uri(type);
}


void
add_float(librdf_model* model,
          librdf_node*  subject,
          const char*   predicate_uri,
          float         object_float)
{
	static const size_t MAX_LEN = 64; // ?
	char object_str[MAX_LEN];
	snprintf(object_str, MAX_LEN, "%f", object_float);

	librdf_uri* type = librdf_new_uri(world, U("http://www.w3.org/2001/XMLSchema#decimal"));

	librdf_node* predicate = librdf_new_node_from_uri_string(world, U(predicate_uri));
	librdf_node* object    = librdf_new_node_from_typed_literal(world, U(object_str), NULL, type);
	
	librdf_statement* triple = librdf_new_statement_from_nodes(world, subject, predicate, object);
	
	librdf_model_add_statement(model, triple);

	//librdf_free_statement(triple);
	librdf_free_uri(type);
}


LADSPA_Descriptor*
load_ladspa_plugin(const char* lib_path, unsigned long index)
{
	void* const handle = dlopen(lib_path, RTLD_LAZY);
	if (handle == NULL)
		return NULL;

	LADSPA_Descriptor_Function df
		= (LADSPA_Descriptor_Function)dlsym(handle, "ladspa_descriptor");

	if (df == NULL) {
		dlclose(handle);
		return NULL;
	}	

	LADSPA_Descriptor* const descriptor = (LADSPA_Descriptor*)df(index);

	return descriptor;
}


void
add_port_range(LADSPA_Descriptor* plugin,
               unsigned long      port_index,
			   librdf_model*      model,
			   librdf_node*       port)
{
	LADSPA_PortRangeHintDescriptor hint_descriptor
		= plugin->PortRangeHints[port_index].HintDescriptor;

	bool range_valid = false;
	float upper=1.0f, lower=0.0f, normal=0.0f;

	/* Convert hints */

	if (LADSPA_IS_HINT_SAMPLE_RATE(hint_descriptor)) {
		add_resource(model, port, NS_LV2("portHint"), NS_LV2("sampleRate"));
		upper = plugin->PortRangeHints[port_index].UpperBound;
		lower = plugin->PortRangeHints[port_index].LowerBound;
		range_valid = true;
	}
	
	if (LADSPA_IS_HINT_INTEGER(hint_descriptor)) {
		add_resource(model, port, NS_LV2("portHint"), NS_LV2("integer"));
		upper = plugin->PortRangeHints[port_index].UpperBound;
		lower = plugin->PortRangeHints[port_index].LowerBound;
		range_valid = true;
	}
	
	if (LADSPA_IS_HINT_TOGGLED(hint_descriptor)) {
		add_resource(model, port, NS_LV2("portHint"), NS_LV2("toggled"));
		upper = 1.0;
		lower = 0.0;
		range_valid = true;
	}

	if (LADSPA_IS_HINT_LOGARITHMIC(hint_descriptor)) {
		/* FLT_EPSILON is defined as the different between 1.0 and the minimum
		 * float greater than 1.0.  So, if lower is < FLT_EPSILON, it will be 1.0
		 * and the logarithmic control will have a base of 1 and thus not change
		 */
		if (range_valid && lower < FLT_EPSILON)
			lower = FLT_EPSILON;
	}


	if (LADSPA_IS_HINT_HAS_DEFAULT(hint_descriptor)) {

		bool valid = true;

		if (range_valid && LADSPA_IS_HINT_DEFAULT_MINIMUM(hint_descriptor)) {
			normal = lower;
		} else if (range_valid && LADSPA_IS_HINT_DEFAULT_LOW(hint_descriptor)) {
			if (LADSPA_IS_HINT_LOGARITHMIC(hint_descriptor)) {
				normal = exp(log(lower) * 0.75 + log(upper) * 0.25);
			} else {
				normal = lower * 0.75 + upper * 0.25;
			}
		} else if (range_valid && LADSPA_IS_HINT_DEFAULT_MIDDLE(hint_descriptor)) {
			if (LADSPA_IS_HINT_LOGARITHMIC(hint_descriptor)) {
				normal = exp(log(lower) * 0.5 + log(upper) * 0.5);
			} else {
				normal = lower * 0.5 + upper * 0.5;
			}
		} else if (range_valid && LADSPA_IS_HINT_DEFAULT_HIGH(hint_descriptor)) {
			if (LADSPA_IS_HINT_LOGARITHMIC(hint_descriptor)) {
				normal = exp(log(lower) * 0.25 + log(upper) * 0.75);
			} else {
				normal = lower * 0.25 + upper * 0.75;
			}
		} else if (range_valid && LADSPA_IS_HINT_DEFAULT_MAXIMUM(hint_descriptor)) {
			normal = upper;
		} else if (LADSPA_IS_HINT_DEFAULT_0(hint_descriptor)) {
			normal = 0.0;
		} else if (LADSPA_IS_HINT_DEFAULT_1(hint_descriptor)) {
			normal = 1.0;
		} else if (LADSPA_IS_HINT_DEFAULT_100(hint_descriptor)) {
			normal = 100.0;
		} else if (LADSPA_IS_HINT_DEFAULT_440(hint_descriptor)) {
			normal = 440.0;
		} else {
			valid = false;
		}
		
		if (valid)
			add_float(model, port, NS_LV2("default"), normal);

	} else {  // No default hint
	
		if (range_valid && LADSPA_IS_HINT_BOUNDED_BELOW(hint_descriptor)) {
			normal = lower;
			add_float(model, port, NS_LV2("default"), normal);
		} else if (range_valid && LADSPA_IS_HINT_BOUNDED_ABOVE(hint_descriptor)) {
			normal = upper;
			add_float(model, port, NS_LV2("default"), normal);
		}
	}
}


void
write_lv2_turtle(LADSPA_Descriptor* descriptor, const char* plugin_uri, const char* filename)
{
	//librdf_storage* storage = librdf_new_storage(world,
	//		"hashes", NULL, "hash-type='memory'");
	librdf_storage* storage = librdf_new_storage(world, "memory", NULL, NULL);

	librdf_model* model = librdf_new_model(world, storage, NULL);
	librdf_serializer* serializer = librdf_new_serializer(world, "turtle", NULL, NULL);

	librdf_node* plugin = librdf_new_node_from_uri_string(world, U(plugin_uri));

	// Set up namespaces
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://www.w3.org/1999/02/22-rdf-syntax-ns#")), "rdf");
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://www.w3.org/2000/01/rdf-schema#")), "rdfs");
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://www.w3.org/2001/XMLSchema")), "xsd");
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://usefulinc.com/ns/doap#")), "doap");
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://xmlns.com/foaf/0.1/")), "foaf");
	librdf_serializer_set_namespace(serializer, librdf_new_uri(world,
			U("http://lv2plug.in/ns/lv2core#")), "lv2");
	
	add_resource(model, plugin,
		NS_RDF("type"),
		NS_LV2("Plugin"));
	
	add_string(model, plugin,
		NS_DOAP("name"),
		descriptor->Name);

	if (LADSPA_IS_HARD_RT_CAPABLE(descriptor->Properties))
		add_resource(model, plugin,
			NS_LV2("optionalFeature"),
		 	NS_LV2("hardRTCapable"));
	
	for (uint32_t i=0; i < descriptor->PortCount; ++i) {
		char index_str[32];
		snprintf(index_str, (size_t)32, "%u", i);

		const LADSPA_PortDescriptor port_descriptor
			= descriptor->PortDescriptors[i];

		librdf_node* port_node = librdf_new_node(world);

		add_node(model, plugin,
			NS_LV2("port"),
			port_node);

		add_int(model, port_node,
			NS_LV2("index"),
		 	(int)i);
	
		if (LADSPA_IS_PORT_INPUT(port_descriptor))
			add_resource(model, port_node,
				NS_RDF("type"),
				NS_LV2("InputPort"));
		else
			add_resource(model, port_node,
				NS_RDF("type"),
				NS_LV2("OutputPort"));
		
		if (LADSPA_IS_PORT_AUDIO(port_descriptor))
			add_resource(model, port_node,
				NS_RDF("type"),
				NS_LV2("AudioPort"));
		else
			add_resource(model, port_node,
				NS_RDF("type"),
				NS_LV2("ControlPort"));
		
		add_string(model, port_node,
			NS_LV2("name"),
		 	descriptor->PortNames[i]);

		add_port_range(descriptor, i, model, port_node);
	}

	librdf_serializer_serialize_model_to_file(serializer, filename, NULL, model);
}


void
print_usage()
{
	printf("Usage: ladspa2lv2 /path/to/ladspalib.so ladspa_index lv2_uri output_data_file\n");
	printf("Partially convert a LADSPA plugin to an LV2 plugin.\n");
	printf("This utility is for developers, it will not generate a usable\n");
	printf("LV2 plugin directly.\n\n");
}


int
main(int argc, char** argv)
{
	if (argc != 5) {
		print_usage();
		return 1;
	}

	const char* const   lib_path = argv[1];
	const unsigned long index    = atol(argv[2]);
	const char* const   uri      = argv[3];

	world = librdf_new_world();
	librdf_world_open(world);

	LADSPA_Descriptor* descriptor = load_ladspa_plugin(lib_path, index);

	if (descriptor) {
		printf("Loaded %s : %lu\n", lib_path, index);
		write_lv2_turtle(descriptor, uri, argv[4]);
	} else {
		printf("Failed to load %s : %lu\n", lib_path, index);
	}

	librdf_free_world(world);

	return 0;
}
