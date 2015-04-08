/*
  Copyright 2007-2014 David Robillard <http://drobilla.net>

  Permission to use, copy, modify, and/or distribute this software for any
  purpose with or without fee is hereby granted, provided that the above
  copyright notice and this permission notice appear in all copies.

  THIS SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*/

#define __STDC_LIMIT_MACROS

#include <assert.h>
#include <math.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "lilv_config.h"
#include "lilv_internal.h"

#include "lv2/lv2plug.in/ns/extensions/ui/ui.h"

#define NS_DOAP (const uint8_t*)"http://usefulinc.com/ns/doap#"
#define NS_FOAF (const uint8_t*)"http://xmlns.com/foaf/0.1/"

/** Ownership of `uri` is taken */
LilvPlugin*
lilv_plugin_new(LilvWorld* world, LilvNode* uri, LilvNode* bundle_uri)
{
	assert(bundle_uri);
	LilvPlugin* plugin = (LilvPlugin*)malloc(sizeof(LilvPlugin));
	plugin->world        = world;
	plugin->plugin_uri   = uri;
	plugin->bundle_uri   = bundle_uri;
	plugin->binary_uri   = NULL;
#ifdef LILV_DYN_MANIFEST
	plugin->dynmanifest  = NULL;
#endif
	plugin->plugin_class = NULL;
	plugin->data_uris    = lilv_nodes_new();
	plugin->ports        = NULL;
	plugin->num_ports    = 0;
	plugin->loaded       = false;
	plugin->replaced     = false;

	return plugin;
}

static void
lilv_plugin_free_ports(LilvPlugin* p)
{
	if (p->ports) {
		for (uint32_t i = 0; i < p->num_ports; ++i) {
			lilv_port_free(p, p->ports[i]);
		}
		free(p->ports);
		p->num_ports = 0;
		p->ports     = NULL;
	}
}

void
lilv_plugin_free(LilvPlugin* p)
{
#ifdef LILV_DYN_MANIFEST
	if (p->dynmanifest && --p->dynmanifest->refs == 0) {
		typedef int (*CloseFunc)(LV2_Dyn_Manifest_Handle);
		CloseFunc close_func = (CloseFunc)lilv_dlfunc(p->dynmanifest->lib,
		                                              "lv2_dyn_manifest_close");
		if (close_func) {
			close_func(p->dynmanifest->handle);
		}

		dlclose(p->dynmanifest->lib);
		lilv_node_free(p->dynmanifest->bundle);
		free(p->dynmanifest);
	}
#endif

	lilv_node_free(p->plugin_uri);
	p->plugin_uri = NULL;

	lilv_node_free(p->bundle_uri);
	p->bundle_uri = NULL;

	lilv_node_free(p->binary_uri);
	p->binary_uri = NULL;

	lilv_plugin_free_ports(p);

	lilv_nodes_free(p->data_uris);
	p->data_uris = NULL;

	free(p);
}

static LilvNode*
lilv_plugin_get_one(const LilvPlugin* p,
                    const SordNode*   subject,
                    const SordNode*   predicate)
{
	LilvNode* ret    = NULL;
	SordIter* stream = lilv_world_query_internal(
		p->world, subject, predicate, NULL);
	if (!sord_iter_end(stream)) {
		ret = lilv_node_new_from_node(p->world, sord_iter_get_node(stream, SORD_OBJECT));
	}
	sord_iter_free(stream);
	return ret;
}

LilvNode*
lilv_plugin_get_unique(const LilvPlugin* p,
                       const SordNode*   subject,
                       const SordNode*   predicate)
{
	LilvNode* ret = lilv_plugin_get_one(p, subject, predicate);
	if (!ret) {
		LILV_ERRORF("Multiple values found for (%s %s ...) property\n",
		            sord_node_get_string(subject),
		            sord_node_get_string(predicate));
	}
	return ret;
}

static void
lilv_plugin_load(LilvPlugin* p)
{
	SordNode*       bundle_uri_node  = p->bundle_uri->node;
	const SerdNode* bundle_uri_snode = sord_node_to_serd_node(bundle_uri_node);

	SerdEnv*    env    = serd_env_new(bundle_uri_snode);
	SerdReader* reader = sord_new_reader(p->world->model, env, SERD_TURTLE,
	                                     bundle_uri_node);

	SordIter* prototypes = sord_search(p->world->model,
	                                   p->plugin_uri->node,
	                                   p->world->uris.lv2_prototype,
	                                   NULL, NULL);
	FOREACH_MATCH(prototypes) {
		const SordNode* t         = sord_iter_get_node(prototypes, SORD_OBJECT);
		LilvNode*       prototype = lilv_node_new_from_node(p->world, t);

		lilv_world_load_resource(p->world, prototype);

		SordIter* statements = sord_search(
			p->world->model, prototype->node, NULL, NULL, NULL);
		FOREACH_MATCH(statements) {
			SordQuad quad;
			sord_iter_get(statements, quad);
			quad[0] = p->plugin_uri->node;
			sord_add(p->world->model, quad);
		}

		sord_iter_free(statements);
		lilv_node_free(prototype);
	}
	sord_iter_free(prototypes);

	// Parse all the plugin's data files into RDF model
	LILV_FOREACH(nodes, i, p->data_uris) {
		const LilvNode* data_uri = lilv_nodes_get(p->data_uris, i);

		serd_env_set_base_uri(env, sord_node_to_serd_node(data_uri->node));
		lilv_world_load_file(p->world, reader, data_uri);
	}

#ifdef LILV_DYN_MANIFEST
	typedef void* LV2_Dyn_Manifest_Handle;
	// Load and parse dynamic manifest data, if this is a library
	if (p->dynmanifest) {
		typedef int (*GetDataFunc)(LV2_Dyn_Manifest_Handle handle,
		                           FILE*                   fp,
		                           const char*             uri);
		GetDataFunc get_data_func = (GetDataFunc)lilv_dlfunc(
			p->dynmanifest->lib, "lv2_dyn_manifest_get_data");
		if (get_data_func) {
			const SordNode* bundle = p->dynmanifest->bundle->node;
			serd_env_set_base_uri(env, sord_node_to_serd_node(bundle));
			FILE* fd = tmpfile();
			get_data_func(p->dynmanifest->handle, fd,
			              lilv_node_as_string(p->plugin_uri));
			rewind(fd);
			serd_reader_add_blank_prefix(
				reader, lilv_world_blank_node_prefix(p->world));
			serd_reader_read_file_handle(
				reader, fd, (const uint8_t*)"(dyn-manifest)");
			fclose(fd);
		}
	}
#endif
	serd_reader_free(reader);
	serd_env_free(env);

	p->loaded = true;
}

static bool
is_symbol(const char* str)
{
	for (const char* s = str; *s; ++s) {
		if (!((*s >= 'a' && *s <= 'z') ||
		      (*s >= 'A' && *s <= 'Z') ||
		      (s > str && *s >= '0' && *s <= '9') ||
		      *s == '_')) {
			return false;
		}
	}
	return true;
}

static void
lilv_plugin_load_ports_if_necessary(const LilvPlugin* const_p)
{
	LilvPlugin* p = (LilvPlugin*)const_p;
	if (!p->loaded)
		lilv_plugin_load(p);

	if (!p->ports) {
		p->ports = (LilvPort**)malloc(sizeof(LilvPort*));
		p->ports[0] = NULL;

		SordIter* ports = lilv_world_query_internal(
			p->world,
			p->plugin_uri->node,
			p->world->uris.lv2_port,
			NULL);

		FOREACH_MATCH(ports) {
			const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);
			LilvNode* index = lilv_plugin_get_unique(
				p, port, p->world->uris.lv2_index);
			LilvNode* symbol = lilv_plugin_get_unique(
				p, port, p->world->uris.lv2_symbol);

			bool error = false;
			if (!lilv_node_is_string(symbol) ||
			    !is_symbol((const char*)sord_node_get_string(symbol->node))) {
				LILV_ERRORF("Plugin <%s> port symbol `%s' is invalid\n",
				            lilv_node_as_uri(p->plugin_uri),
				            lilv_node_as_string(symbol));
				error = true;
				goto done;
			}

			if (!lilv_node_is_int(index)) {
				LILV_ERRORF("Plugin <%s> port index is not an integer\n",
				            lilv_node_as_uri(p->plugin_uri));
				error = true;
				goto done;
			}

			uint32_t  this_index = lilv_node_as_int(index);
			LilvPort* this_port  = NULL;
			if (p->num_ports > this_index) {
				this_port = p->ports[this_index];
			} else {
				p->ports = (LilvPort**)realloc(
					p->ports, (this_index + 1) * sizeof(LilvPort*));
				memset(p->ports + p->num_ports, '\0',
				       (this_index - p->num_ports) * sizeof(LilvPort*));
				p->num_ports = this_index + 1;
			}

			// Havn't seen this port yet, add it to array
			if (!this_port) {
				this_port = lilv_port_new(p->world,
				                          port,
				                          this_index,
				                          lilv_node_as_string(symbol));
				p->ports[this_index] = this_port;
			}

			SordIter* types = lilv_world_query_internal(
				p->world, port, p->world->uris.rdf_a, NULL);
			FOREACH_MATCH(types) {
				const SordNode* type = sord_iter_get_node(types, SORD_OBJECT);
				if (sord_node_get_type(type) == SORD_URI) {
					zix_tree_insert(
						(ZixTree*)this_port->classes,
						lilv_node_new_from_node(p->world, type), NULL);
				} else {
					LILV_WARNF("Plugin <%s> port type is not a URI\n",
					           lilv_node_as_uri(p->plugin_uri));
				}
			}
			sord_iter_free(types);

		done:
			lilv_node_free(symbol);
			lilv_node_free(index);
			if (error) {  // Invalid plugin
				lilv_plugin_free_ports(p);
				break;
			}
		}
		sord_iter_free(ports);

		// Check sanity
		for (uint32_t i = 0; i < p->num_ports; ++i) {
			if (!p->ports[i]) {
				LILV_ERRORF("Plugin <%s> is missing port %d/%d\n",
				            lilv_node_as_uri(p->plugin_uri), i, p->num_ports);
				lilv_plugin_free_ports(p);
				break;
			}
		}
	}
}

void
lilv_plugin_load_if_necessary(const LilvPlugin* p)
{
	if (!p->loaded)
		lilv_plugin_load((LilvPlugin*)p);
}

LILV_API const LilvNode*
lilv_plugin_get_uri(const LilvPlugin* p)
{
	return p->plugin_uri;
}

LILV_API const LilvNode*
lilv_plugin_get_bundle_uri(const LilvPlugin* p)
{
	return p->bundle_uri;
}

LILV_API const LilvNode*
lilv_plugin_get_library_uri(const LilvPlugin* const_p)
{
	LilvPlugin* p = (LilvPlugin*)const_p;
	lilv_plugin_load_if_necessary(p);
	if (!p->binary_uri) {
		// <plugin> lv2:binary ?binary
		SordIter* i = lilv_world_query_internal(p->world,
		                                        p->plugin_uri->node,
		                                        p->world->uris.lv2_binary,
		                                        NULL);
		FOREACH_MATCH(i) {
			const SordNode* binary_node = sord_iter_get_node(i, SORD_OBJECT);
			if (sord_node_get_type(binary_node) == SORD_URI) {
				p->binary_uri = lilv_node_new_from_node(p->world, binary_node);
				break;
			}
		}
		sord_iter_free(i);
	}
	if (!p->binary_uri) {
		LILV_WARNF("Plugin <%s> has no lv2:binary\n",
		           lilv_node_as_uri(lilv_plugin_get_uri(p)));
	}
	return p->binary_uri;
}

LILV_API const LilvNodes*
lilv_plugin_get_data_uris(const LilvPlugin* p)
{
	return p->data_uris;
}

LILV_API const LilvPluginClass*
lilv_plugin_get_class(const LilvPlugin* const_p)
{
	LilvPlugin* p = (LilvPlugin*)const_p;
	lilv_plugin_load_if_necessary(p);
	if (!p->plugin_class) {
		// <plugin> a ?class
		SordIter* results = lilv_world_query_internal(p->world,
		                                              p->plugin_uri->node,
		                                              p->world->uris.rdf_a,
		                                              NULL);
		FOREACH_MATCH(results) {
			const SordNode* class_node = sord_iter_get_node(results, SORD_OBJECT);
			if (sord_node_get_type(class_node) != SORD_URI) {
				continue;
			}

			LilvNode* klass = lilv_node_new_from_node(p->world, class_node);
			if (!lilv_node_equals(klass, p->world->lv2_plugin_class->uri)) {
				const LilvPluginClass* pclass = lilv_plugin_classes_get_by_uri(
					p->world->plugin_classes, klass);

				if (pclass) {
					((LilvPlugin*)p)->plugin_class = pclass;
					lilv_node_free(klass);
					break;
				}
			}

			lilv_node_free(klass);
		}
		sord_iter_free(results);

		if (p->plugin_class == NULL)
			p->plugin_class = p->world->lv2_plugin_class;
	}
	return p->plugin_class;
}

static LilvNodes*
lilv_plugin_get_value_internal(const LilvPlugin* p,
                               const SordNode*   predicate)
{
	lilv_plugin_load_if_necessary(p);
	return lilv_world_find_nodes_internal(
		p->world, p->plugin_uri->node, predicate, NULL);
}

LILV_API bool
lilv_plugin_verify(const LilvPlugin* plugin)
{
	LilvNode*  rdf_type = lilv_new_uri(plugin->world, LILV_NS_RDF "type");
	LilvNodes* results  = lilv_plugin_get_value(plugin, rdf_type);
	lilv_node_free(rdf_type);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	results = lilv_plugin_get_value_internal(plugin,
	                                         plugin->world->uris.doap_name);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	LilvNode* lv2_port = lilv_new_uri(plugin->world, LV2_CORE__port);
	results = lilv_plugin_get_value(plugin, lv2_port);
	lilv_node_free(lv2_port);
	if (!results) {
		return false;
	}

	lilv_nodes_free(results);
	return true;
}

LILV_API LilvNode*
lilv_plugin_get_name(const LilvPlugin* plugin)
{
	LilvNodes* results = lilv_plugin_get_value_internal(
		plugin, plugin->world->uris.doap_name);

	LilvNode* ret = NULL;
	if (results) {
		LilvNode* val = lilv_nodes_get_first(results);
		if (lilv_node_is_string(val))
			ret = lilv_node_duplicate(val);
		lilv_nodes_free(results);
	}

	if (!ret)
		LILV_WARNF("Plugin <%s> has no (mandatory) doap:name\n",
		           lilv_node_as_string(lilv_plugin_get_uri(plugin)));

	return ret;
}

LILV_API LilvNodes*
lilv_plugin_get_value(const LilvPlugin* p,
                      const LilvNode*   predicate)
{
	lilv_plugin_load_if_necessary(p);
	return lilv_world_find_nodes(p->world, p->plugin_uri, predicate, NULL);
}

LILV_API uint32_t
lilv_plugin_get_num_ports(const LilvPlugin* p)
{
	lilv_plugin_load_ports_if_necessary(p);
	return p->num_ports;
}

LILV_API void
lilv_plugin_get_port_ranges_float(const LilvPlugin* p,
                                  float*            min_values,
                                  float*            max_values,
                                  float*            def_values)
{
	lilv_plugin_load_ports_if_necessary(p);
	LilvNode*  min    = NULL;
	LilvNode*  max    = NULL;
	LilvNode*  def    = NULL;
	LilvNode** minptr = min_values ? &min : NULL;
	LilvNode** maxptr = max_values ? &max : NULL;
	LilvNode** defptr = def_values ? &def : NULL;

	for (uint32_t i = 0; i < p->num_ports; ++i) {
		lilv_port_get_range(p, p->ports[i], defptr, minptr, maxptr);

		if (min_values) {
			if (lilv_node_is_float(min) || lilv_node_is_int(min)) {
				min_values[i] = lilv_node_as_float(min);
			} else {
				min_values[i] = NAN;
			}
		}

		if (max_values) {
			if (lilv_node_is_float(max) || lilv_node_is_int(max)) {
				max_values[i] = lilv_node_as_float(max);
			} else {
				max_values[i] = NAN;
			}
		}

		if (def_values) {
			if (lilv_node_is_float(def) || lilv_node_is_int(def)) {
				def_values[i] = lilv_node_as_float(def);
			} else {
				def_values[i] = NAN;
			}
		}

		lilv_node_free(def);
		lilv_node_free(min);
		lilv_node_free(max);
	}
}

LILV_API uint32_t
lilv_plugin_get_num_ports_of_class_va(const LilvPlugin* p,
                                      const LilvNode*   class_1,
                                      va_list           args)
{
	lilv_plugin_load_ports_if_necessary(p);

	uint32_t count = 0;

	// Build array of classes from args so we can walk it several times
	size_t           n_classes = 0;
	const LilvNode** classes   = NULL;
	for (LilvNode* c = NULL; (c = va_arg(args, LilvNode*)); ) {
		classes = (const LilvNode**)realloc(
			classes, ++n_classes * sizeof(LilvNode*));
		classes[n_classes - 1] = c;
	}

	// Check each port against every type
	for (unsigned i = 0; i < p->num_ports; ++i) {
		LilvPort* port = p->ports[i];
		if (port && lilv_port_is_a(p, port, class_1)) {
			bool matches = true;
			for (size_t j = 0; j < n_classes; ++j) {
				if (!lilv_port_is_a(p, port, classes[j])) {
					matches = false;
					break;
				}
			}

			if (matches) {
				++count;
			}
		}
	}

	free(classes);
	return count;
}

LILV_API uint32_t
lilv_plugin_get_num_ports_of_class(const LilvPlugin* p,
                                   const LilvNode*   class_1, ...)
{
	va_list args;
	va_start(args, class_1);

	uint32_t count = lilv_plugin_get_num_ports_of_class_va(p, class_1, args);

	va_end(args);
	return count;
}

LILV_API bool
lilv_plugin_has_latency(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);
	SordIter* ports = lilv_world_query_internal(
		p->world,
		p->plugin_uri->node,
		p->world->uris.lv2_port,
		NULL);

	bool ret = false;
	FOREACH_MATCH(ports) {
		const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);
		SordIter*       prop = lilv_world_query_internal(
			p->world,
			port,
			p->world->uris.lv2_portProperty,
			p->world->uris.lv2_reportsLatency);
		SordIter* des = lilv_world_query_internal(
			p->world,
			port,
			p->world->uris.lv2_designation,
			p->world->uris.lv2_latency);
		const bool latent = !sord_iter_end(prop) || !sord_iter_end(des);
		sord_iter_free(prop);
		sord_iter_free(des);
		if (latent) {
			ret = true;
			break;
		}
	}
	sord_iter_free(ports);

	return ret;
}

static const LilvPort*
lilv_plugin_get_port_by_property(const LilvPlugin* plugin,
                                 const SordNode*   port_property)
{
	lilv_plugin_load_ports_if_necessary(plugin);
	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		SordIter* iter = lilv_world_query_internal(
			plugin->world,
			port->node->node,
			plugin->world->uris.lv2_portProperty,
			port_property);

		const bool found = !sord_iter_end(iter);
		sord_iter_free(iter);

		if (found) {
			return port;
		}
	}

	return NULL;
}

LILV_API const LilvPort*
lilv_plugin_get_port_by_designation(const LilvPlugin* plugin,
                                    const LilvNode*   port_class,
                                    const LilvNode*   designation)
{
	LilvWorld* world = plugin->world;
	lilv_plugin_load_ports_if_necessary(plugin);
	for (uint32_t i = 0; i < plugin->num_ports; ++i) {
		LilvPort* port = plugin->ports[i];
		SordIter* iter = lilv_world_query_internal(
			world,
			port->node->node,
			world->uris.lv2_designation,
			designation->node);

		const bool found = !sord_iter_end(iter) &&
			(!port_class || lilv_port_is_a(plugin, port, port_class));
		sord_iter_free(iter);

		if (found) {
			return port;
		}
	}

	return NULL;
}

LILV_API uint32_t
lilv_plugin_get_latency_port_index(const LilvPlugin* p)
{
	const LilvPort* prop_port = lilv_plugin_get_port_by_property(
		p, p->world->uris.lv2_reportsLatency);
	const LilvPort* des_port = lilv_plugin_get_port_by_property(
		p, p->world->uris.lv2_latency);
	if (prop_port) {
		return prop_port->index;
	} else if (des_port) {
		return des_port->index;
	} else {
		return UINT32_MAX;
	}
}

LILV_API bool
lilv_plugin_has_feature(const LilvPlugin* p,
                        const LilvNode*   feature)
{
	lilv_plugin_load_if_necessary(p);
	const SordNode* predicates[] = { p->world->uris.lv2_requiredFeature,
	                                 p->world->uris.lv2_optionalFeature,
	                                 NULL };

	for (const SordNode** pred = predicates; *pred; ++pred) {
		if (lilv_world_ask_internal(
			    p->world, p->plugin_uri->node, *pred, feature->node)) {
			return true;
		}
	}
	return false;
}

LILV_API LilvNodes*
lilv_plugin_get_supported_features(const LilvPlugin* p)
{
	LilvNodes* optional = lilv_plugin_get_optional_features(p);
	LilvNodes* required = lilv_plugin_get_required_features(p);
	LilvNodes* result   = lilv_nodes_merge(optional, required);
	lilv_nodes_free(optional);
	lilv_nodes_free(required);
	return result;
}

LILV_API LilvNodes*
lilv_plugin_get_optional_features(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);
	return lilv_world_find_nodes_internal(p->world,
	                                      p->plugin_uri->node,
	                                      p->world->uris.lv2_optionalFeature,
	                                      NULL);
}

LILV_API LilvNodes*
lilv_plugin_get_required_features(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);
	return lilv_world_find_nodes_internal(p->world,
	                                      p->plugin_uri->node,
	                                      p->world->uris.lv2_requiredFeature,
	                                      NULL);
}

LILV_API bool
lilv_plugin_has_extension_data(const LilvPlugin* p,
                               const LilvNode*   uri)
{
	if (!lilv_node_is_uri(uri)) {
		LILV_ERRORF("Extension data `%s' is not a URI\n",
		            sord_node_get_string(uri->node));
		return false;
	}

	lilv_plugin_load_if_necessary(p);
	return lilv_world_ask_internal(
		p->world,
		p->plugin_uri->node,
		p->world->uris.lv2_extensionData,
		uri->node);
}

LILV_API LilvNodes*
lilv_plugin_get_extension_data(const LilvPlugin* p)
{
	return lilv_plugin_get_value_internal(p, p->world->uris.lv2_extensionData);
}

LILV_API const LilvPort*
lilv_plugin_get_port_by_index(const LilvPlugin* p,
                              uint32_t          index)
{
	lilv_plugin_load_ports_if_necessary(p);
	if (index < p->num_ports)
		return p->ports[index];
	else
		return NULL;
}

LILV_API const LilvPort*
lilv_plugin_get_port_by_symbol(const LilvPlugin* p,
                               const LilvNode*   symbol)
{
	lilv_plugin_load_ports_if_necessary(p);
	for (uint32_t i = 0; i < p->num_ports; ++i) {
		LilvPort* port = p->ports[i];
		if (lilv_node_equals(port->symbol, symbol))
			return port;
	}

	return NULL;
}

LILV_API LilvNode*
lilv_plugin_get_project(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);

	SordNode* lv2_project = sord_new_uri(p->world->world,
	                                     (const uint8_t*)LV2_CORE__project);

	SordIter* projects = lilv_world_query_internal(p->world,
	                                               p->plugin_uri->node,
	                                               lv2_project,
	                                               NULL);

	sord_node_free(p->world->world, lv2_project);

	if (sord_iter_end(projects)) {
		sord_iter_free(projects);
		return NULL;
	}

	const SordNode* project = sord_iter_get_node(projects, SORD_OBJECT);

	sord_iter_free(projects);
	return lilv_node_new_from_node(p->world, project);
}

static const SordNode*
lilv_plugin_get_author(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);

	SordNode* doap_maintainer = sord_new_uri(
		p->world->world, NS_DOAP "maintainer");

	SordIter* maintainers = lilv_world_query_internal(
		p->world,
		p->plugin_uri->node,
		doap_maintainer,
		NULL);

	if (sord_iter_end(maintainers)) {
		sord_iter_free(maintainers);

		LilvNode* project = lilv_plugin_get_project(p);
		if (!project) {
			sord_node_free(p->world->world, doap_maintainer);
			return NULL;
		}

		maintainers = lilv_world_query_internal(
			p->world,
			project->node,
			doap_maintainer,
			NULL);

		lilv_node_free(project);
	}

	sord_node_free(p->world->world, doap_maintainer);

	if (sord_iter_end(maintainers)) {
		sord_iter_free(maintainers);
		return NULL;
	}

	const SordNode* author = sord_iter_get_node(maintainers, SORD_OBJECT);

	sord_iter_free(maintainers);
	return author;
}

LILV_API LilvNode*
lilv_plugin_get_author_name(const LilvPlugin* plugin)
{
	const SordNode* author = lilv_plugin_get_author(plugin);
	if (author) {
		SordWorld* sworld    = plugin->world->world;
		SordNode*  foaf_name = sord_new_uri(sworld, NS_FOAF "name");
		LilvNode*  ret       = lilv_plugin_get_one(plugin, author, foaf_name);
		sord_node_free(sworld, foaf_name);
		return ret;
	}
	return NULL;
}

LILV_API LilvNode*
lilv_plugin_get_author_email(const LilvPlugin* plugin)
{
	const SordNode* author = lilv_plugin_get_author(plugin);
	if (author) {
		SordWorld* sworld    = plugin->world->world;
		SordNode*  foaf_mbox = sord_new_uri(sworld, NS_FOAF "mbox");
		LilvNode*  ret       = lilv_plugin_get_one(plugin, author, foaf_mbox);
		sord_node_free(sworld, foaf_mbox);
		return ret;
	}
	return NULL;
}

LILV_API LilvNode*
lilv_plugin_get_author_homepage(const LilvPlugin* plugin)
{
	const SordNode* author = lilv_plugin_get_author(plugin);
	if (author) {
		SordWorld* sworld        = plugin->world->world;
		SordNode*  foaf_homepage = sord_new_uri(sworld, NS_FOAF "homepage");
		LilvNode*  ret           = lilv_plugin_get_one(plugin, author, foaf_homepage);
		sord_node_free(sworld, foaf_homepage);
		return ret;
	}
	return NULL;
}

LILV_API bool
lilv_plugin_is_replaced(const LilvPlugin* plugin)
{
	return plugin->replaced;
}

LILV_API LilvUIs*
lilv_plugin_get_uis(const LilvPlugin* p)
{
	lilv_plugin_load_if_necessary(p);

	SordNode* ui_ui_node = sord_new_uri(p->world->world,
	                                    (const uint8_t*)LV2_UI__ui);
	SordNode* ui_binary_node = sord_new_uri(p->world->world,
	                                        (const uint8_t*)LV2_UI__binary);

	LilvUIs*  result = lilv_uis_new();
	SordIter* uis    = lilv_world_query_internal(p->world,
	                                             p->plugin_uri->node,
	                                             ui_ui_node,
	                                             NULL);

	FOREACH_MATCH(uis) {
		const SordNode* ui = sord_iter_get_node(uis, SORD_OBJECT);

		LilvNode* type   = lilv_plugin_get_unique(p, ui, p->world->uris.rdf_a);
		LilvNode* binary = lilv_plugin_get_unique(p, ui, ui_binary_node);

		if (sord_node_get_type(ui) != SORD_URI
		    || !lilv_node_is_uri(type)
		    || !lilv_node_is_uri(binary)) {
			lilv_node_free(binary);
			lilv_node_free(type);
			LILV_ERRORF("Corrupt UI <%s>\n", sord_node_get_string(ui));
			continue;
		}

		LilvUI* lilv_ui = lilv_ui_new(
			p->world,
			lilv_node_new_from_node(p->world, ui),
			type,
			binary);

		zix_tree_insert((ZixTree*)result, lilv_ui, NULL);
	}
	sord_iter_free(uis);

	sord_node_free(p->world->world, ui_binary_node);
	sord_node_free(p->world->world, ui_ui_node);

	if (lilv_uis_size(result) > 0) {
		return result;
	} else {
		lilv_uis_free(result);
		return NULL;
	}
}

LILV_API LilvNodes*
lilv_plugin_get_related(const LilvPlugin* plugin, const LilvNode* type)
{
	lilv_plugin_load_if_necessary(plugin);

	LilvWorld* const world   = plugin->world;
	LilvNodes* const related = lilv_world_find_nodes_internal(
		world,
		NULL,
		world->uris.lv2_appliesTo,
		lilv_plugin_get_uri(plugin)->node);

	if (!type) {
		return related;
	}

	LilvNodes* matches = lilv_nodes_new();
	LILV_FOREACH(nodes, i, related) {
		LilvNode* node  = (LilvNode*)lilv_collection_get((ZixTree*)related, i);
		if (lilv_world_ask_internal(
			    world, node->node, world->uris.rdf_a, type->node)) {
			zix_tree_insert((ZixTree*)matches,
			                lilv_node_new_from_node(world, node->node),
			                NULL);
		}
	}

	lilv_nodes_free(related);
	return matches;
}

static SerdEnv*
new_lv2_env(const SerdNode* base)
{
	SerdEnv* env = serd_env_new(base);

#define USTR(s) ((const uint8_t*)s)
	serd_env_set_prefix_from_strings(env, USTR("doap"), USTR(LILV_NS_DOAP));
	serd_env_set_prefix_from_strings(env, USTR("foaf"), USTR(LILV_NS_FOAF));
	serd_env_set_prefix_from_strings(env, USTR("lv2"),  USTR(LILV_NS_LV2));
	serd_env_set_prefix_from_strings(env, USTR("owl"),  USTR(LILV_NS_OWL));
	serd_env_set_prefix_from_strings(env, USTR("rdf"),  USTR(LILV_NS_RDF));
	serd_env_set_prefix_from_strings(env, USTR("rdfs"), USTR(LILV_NS_RDFS));
	serd_env_set_prefix_from_strings(env, USTR("xsd"),  USTR(LILV_NS_XSD));

	return env;
}

static void
maybe_write_prefixes(SerdWriter* writer, SerdEnv* env, FILE* file)
{
	fseek(file, 0, SEEK_END);
	if (ftell(file) == 0) {
		serd_env_foreach(
			env, (SerdPrefixSink)serd_writer_set_prefix, writer);
	} else {
		fprintf(file, "\n");
	}
}

LILV_API void
lilv_plugin_write_description(LilvWorld*        world,
                              const LilvPlugin* plugin,
                              const LilvNode*   base_uri,
                              FILE*             plugin_file)
{
	const LilvNode* subject   = lilv_plugin_get_uri(plugin);
	const uint32_t  num_ports = lilv_plugin_get_num_ports(plugin);
	const SerdNode* base      = sord_node_to_serd_node(base_uri->node);
	SerdEnv*        env       = new_lv2_env(base);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_ABBREVIATED|SERD_STYLE_CURIED),
		env,
		NULL,
		serd_file_sink,
		plugin_file);

	// Write prefixes if this is a new file
	maybe_write_prefixes(writer, env, plugin_file);

	// Write plugin description
	SordIter* plug_iter = lilv_world_query_internal(
		world, subject->node, NULL, NULL);
	sord_write_iter(plug_iter, writer);

	// Write port descriptions
	for (uint32_t i = 0; i < num_ports; ++i) {
		const LilvPort* port = plugin->ports[i];
		SordIter* port_iter = lilv_world_query_internal(
			world, port->node->node, NULL, NULL);
		sord_write_iter(port_iter, writer);
	}

	serd_writer_free(writer);
	serd_env_free(env);
}

LILV_API void
lilv_plugin_write_manifest_entry(LilvWorld*        world,
                                 const LilvPlugin* plugin,
                                 const LilvNode*   base_uri,
                                 FILE*             manifest_file,
                                 const char*       plugin_file_path)
{
	const LilvNode* subject = lilv_plugin_get_uri(plugin);
	const SerdNode* base    = sord_node_to_serd_node(base_uri->node);
	SerdEnv*        env     = new_lv2_env(base);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_ABBREVIATED|SERD_STYLE_CURIED),
		env,
		NULL,
		serd_file_sink,
		manifest_file);

	// Write prefixes if this is a new file
	maybe_write_prefixes(writer, env, manifest_file);

	// Write manifest entry
	serd_writer_write_statement(
		writer, 0, NULL,
		sord_node_to_serd_node(subject->node),
		sord_node_to_serd_node(plugin->world->uris.rdf_a),
		sord_node_to_serd_node(plugin->world->uris.lv2_Plugin), 0, 0);

	const SerdNode file_node = serd_node_from_string(
		SERD_URI, (const uint8_t*)plugin_file_path);
	serd_writer_write_statement(
		writer, 0, NULL,
		sord_node_to_serd_node(subject->node),
		sord_node_to_serd_node(plugin->world->uris.rdfs_seeAlso),
		&file_node, 0, 0);

	serd_writer_free(writer);
	serd_env_free(env);
}
