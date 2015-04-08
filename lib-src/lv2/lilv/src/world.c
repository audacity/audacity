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

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "lv2/lv2plug.in/ns/ext/presets/presets.h"

#include "lilv_internal.h"

LILV_API LilvWorld*
lilv_world_new(void)
{
	LilvWorld* world = (LilvWorld*)malloc(sizeof(LilvWorld));

	world->world = sord_world_new();
	if (!world->world)
		goto fail;

	world->model = sord_new(world->world, SORD_SPO|SORD_OPS, true);
	if (!world->model)
		goto fail;

	world->specs          = NULL;
	world->plugin_classes = lilv_plugin_classes_new();
	world->plugins        = lilv_plugins_new();
	world->loaded_files   = zix_tree_new(
		false, lilv_resource_node_cmp, NULL, (ZixDestroyFunc)lilv_node_free);

#ifdef LILV_NEW_LV2
	world->libs = zix_tree_new(false, lilv_lib_compare, NULL, NULL);
#endif

#define NS_DCTERMS "http://purl.org/dc/terms/"
#define NS_DYNMAN  "http://lv2plug.in/ns/ext/dynmanifest#"

#define NEW_URI(uri) sord_new_uri(world->world, (const uint8_t*)uri)

	world->uris.dc_replaces         = NEW_URI(NS_DCTERMS   "replaces");
	world->uris.dman_DynManifest    = NEW_URI(NS_DYNMAN    "DynManifest");
	world->uris.doap_name           = NEW_URI(LILV_NS_DOAP "name");
	world->uris.lv2_Plugin          = NEW_URI(LV2_CORE__Plugin);
	world->uris.lv2_Specification   = NEW_URI(LV2_CORE__Specification);
	world->uris.lv2_appliesTo       = NEW_URI(LV2_CORE__appliesTo);
	world->uris.lv2_binary          = NEW_URI(LV2_CORE__binary);
	world->uris.lv2_default         = NEW_URI(LV2_CORE__default);
	world->uris.lv2_designation     = NEW_URI(LV2_CORE__designation);
	world->uris.lv2_extensionData   = NEW_URI(LV2_CORE__extensionData);
	world->uris.lv2_index           = NEW_URI(LV2_CORE__index);
	world->uris.lv2_latency         = NEW_URI(LV2_CORE__latency);
	world->uris.lv2_maximum         = NEW_URI(LV2_CORE__maximum);
	world->uris.lv2_minimum         = NEW_URI(LV2_CORE__minimum);
	world->uris.lv2_name            = NEW_URI(LV2_CORE__name);
	world->uris.lv2_optionalFeature = NEW_URI(LV2_CORE__optionalFeature);
	world->uris.lv2_port            = NEW_URI(LV2_CORE__port);
	world->uris.lv2_portProperty    = NEW_URI(LV2_CORE__portProperty);
	world->uris.lv2_reportsLatency  = NEW_URI(LV2_CORE__reportsLatency);
	world->uris.lv2_requiredFeature = NEW_URI(LV2_CORE__requiredFeature);
	world->uris.lv2_symbol          = NEW_URI(LV2_CORE__symbol);
	world->uris.lv2_prototype       = NEW_URI(LV2_CORE__prototype);
	world->uris.pset_value          = NEW_URI(LV2_PRESETS__value);
	world->uris.rdf_a               = NEW_URI(LILV_NS_RDF  "type");
	world->uris.rdf_value           = NEW_URI(LILV_NS_RDF  "value");
	world->uris.rdfs_Class          = NEW_URI(LILV_NS_RDFS "Class");
	world->uris.rdfs_label          = NEW_URI(LILV_NS_RDFS "label");
	world->uris.rdfs_seeAlso        = NEW_URI(LILV_NS_RDFS "seeAlso");
	world->uris.rdfs_subClassOf     = NEW_URI(LILV_NS_RDFS "subClassOf");
	world->uris.xsd_base64Binary    = NEW_URI(LILV_NS_XSD  "base64Binary");
	world->uris.xsd_boolean         = NEW_URI(LILV_NS_XSD  "boolean");
	world->uris.xsd_decimal         = NEW_URI(LILV_NS_XSD  "decimal");
	world->uris.xsd_double          = NEW_URI(LILV_NS_XSD  "double");
	world->uris.xsd_integer         = NEW_URI(LILV_NS_XSD  "integer");
	world->uris.null_uri            = NULL;

	world->lv2_plugin_class = lilv_plugin_class_new(
		world, NULL, world->uris.lv2_Plugin, "Plugin");
	assert(world->lv2_plugin_class);

	world->n_read_files        = 0;
	world->opt.filter_language = true;
	world->opt.dyn_manifest    = true;

	return world;

fail:
	/* keep on rockin' in the */ free(world);
	return NULL;
}

LILV_API void
lilv_world_free(LilvWorld* world)
{
	if (!world) {
		return;
	}

	lilv_plugin_class_free(world->lv2_plugin_class);
	world->lv2_plugin_class = NULL;

	for (SordNode** n = (SordNode**)&world->uris; *n; ++n) {
		sord_node_free(world->world, *n);
	}

	for (LilvSpec* spec = world->specs; spec;) {
		LilvSpec* next = spec->next;
		sord_node_free(world->world, spec->spec);
		sord_node_free(world->world, spec->bundle);
		lilv_nodes_free(spec->data_uris);
		free(spec);
		spec = next;
	}
	world->specs = NULL;

	LILV_FOREACH(plugins, i, world->plugins) {
		const LilvPlugin* p = lilv_plugins_get(world->plugins, i);
		lilv_plugin_free((LilvPlugin*)p);
	}
	zix_tree_free((ZixTree*)world->plugins);
	world->plugins = NULL;

	zix_tree_free((ZixTree*)world->loaded_files);
	world->loaded_files = NULL;

#ifdef LILV_NEW_LV2
	zix_tree_free((ZixTree*)world->libs);
	world->libs = NULL;
#endif

	zix_tree_free((ZixTree*)world->plugin_classes);
	world->plugin_classes = NULL;

	sord_free(world->model);
	world->model = NULL;

	sord_world_free(world->world);
	world->world = NULL;

	free(world);
}

LILV_API void
lilv_world_set_option(LilvWorld*      world,
                      const char*     option,
                      const LilvNode* value)
{
	if (!strcmp(option, LILV_OPTION_DYN_MANIFEST)) {
		if (lilv_node_is_bool(value)) {
			world->opt.dyn_manifest = lilv_node_as_bool(value);
			return;
		}
	} else if (!strcmp(option, LILV_OPTION_FILTER_LANG)) {
		if (lilv_node_is_bool(value)) {
			world->opt.filter_language = lilv_node_as_bool(value);
			return;
		}
	}
	LILV_WARNF("Unrecognized or invalid option `%s'\n", option);
}

LILV_API LilvNodes*
lilv_world_find_nodes(LilvWorld*      world,
                      const LilvNode* subject,
                      const LilvNode* predicate,
                      const LilvNode* object)
{
	if (subject && !lilv_node_is_uri(subject) && !lilv_node_is_blank(subject)) {
		LILV_ERRORF("Subject `%s' is not a resource\n",
		            sord_node_get_string(subject->node));
		return NULL;
	} else if (!lilv_node_is_uri(predicate)) {
		LILV_ERRORF("Predicate `%s' is not a URI\n",
		            sord_node_get_string(predicate->node));
		return NULL;
	} else if (!subject && !object) {
		LILV_ERROR("Both subject and object are NULL\n");
		return NULL;
	}
	
	return lilv_world_find_nodes_internal(world,
	                                      subject ? subject->node : NULL,
	                                      predicate->node,
	                                      object ? object->node : NULL);
}

LILV_API LilvNode*
lilv_world_get(LilvWorld*      world,
               const LilvNode* subject,
               const LilvNode* predicate,
               const LilvNode* object)
{
	SordNode* snode = sord_get(world->model,
	                           subject   ? subject->node   : NULL,
	                           predicate ? predicate->node : NULL,
	                           object    ? object->node    : NULL,
	                           NULL);
	LilvNode* lnode = lilv_node_new_from_node(world, snode);
	sord_node_free(world->world, snode);
	return lnode;
}

SordIter*
lilv_world_query_internal(LilvWorld*      world,
                          const SordNode* subject,
                          const SordNode* predicate,
                          const SordNode* object)
{
	return sord_search(world->model, subject, predicate, object, NULL);
}

bool
lilv_world_ask_internal(LilvWorld*      world,
                        const SordNode* subject,
                        const SordNode* predicate,
                        const SordNode* object)
{
	return sord_ask(world->model, subject, predicate, object, NULL);
}

LILV_API bool
lilv_world_ask(LilvWorld*      world,
               const LilvNode* subject,
               const LilvNode* predicate,
               const LilvNode* object)
{
	return sord_ask(
		world->model, subject->node, predicate->node, object->node, NULL);
}

LilvNodes*
lilv_world_find_nodes_internal(LilvWorld*      world,
                               const SordNode* subject,
                               const SordNode* predicate,
                               const SordNode* object)
{
	return lilv_nodes_from_stream_objects(
		world,
		lilv_world_query_internal(world, subject, predicate, object),
		(object == NULL) ? SORD_OBJECT : SORD_SUBJECT);
}

static SerdNode
lilv_new_uri_relative_to_base(const uint8_t* uri_str,
                              const uint8_t* base_uri_str)
{
	SerdURI base_uri;
	if (serd_uri_parse(base_uri_str, &base_uri)) {
		return SERD_NODE_NULL;
	}

	SerdURI ignored;
	return serd_node_new_uri_from_string(uri_str, &base_uri, &ignored);
}

const uint8_t*
lilv_world_blank_node_prefix(LilvWorld* world)
{
	static char str[32];
	snprintf(str, sizeof(str), "%d", world->n_read_files++);
	return (const uint8_t*)str;
}

/** Comparator for sequences (e.g. world->plugins). */
int
lilv_header_compare_by_uri(const void* a, const void* b, void* user_data)
{
	const struct LilvHeader* const header_a = (const struct LilvHeader*)a;
	const struct LilvHeader* const header_b = (const struct LilvHeader*)b;
	return strcmp(lilv_node_as_uri(header_a->uri),
	              lilv_node_as_uri(header_b->uri));
}

/**
   Comparator for libraries (world->libs).

   Libraries do have a LilvHeader, but we must also compare the bundle to
   handle the case where the same library is loaded with different bundles, and
   consequently different contents (mainly plugins).
 */
int
lilv_lib_compare(const void* a, const void* b, void* user_data)
{
	const LilvLib* const lib_a = (const LilvLib*)a;
	const LilvLib* const lib_b = (const LilvLib*)b;
	int cmp = strcmp(lilv_node_as_uri(lib_a->uri),
	                 lilv_node_as_uri(lib_b->uri));
	return cmp ? cmp : strcmp(lib_a->bundle_path, lib_b->bundle_path);
}

/** Get an element of a collection of any object with an LilvHeader by URI. */
struct LilvHeader*
lilv_collection_get_by_uri(const ZixTree*  const_seq,
                           const LilvNode* uri)
{
	if (!lilv_node_is_uri(uri)) {
		return NULL;
	}

	ZixTree*          seq = (ZixTree*)const_seq;
	struct LilvHeader key = { NULL, (LilvNode*)uri };
	ZixTreeIter*      i   = NULL;
	ZixStatus         st  = zix_tree_find(seq, &key, &i);
	if (!st) {
		return (struct LilvHeader*)zix_tree_get(i);
	}

	return NULL;
}

static void
lilv_world_add_spec(LilvWorld*      world,
                    const SordNode* specification_node,
                    const SordNode* bundle_node)
{
	LilvSpec* spec = (LilvSpec*)malloc(sizeof(LilvSpec));
	spec->spec      = sord_node_copy(specification_node);
	spec->bundle    = sord_node_copy(bundle_node);
	spec->data_uris = lilv_nodes_new();

	// Add all data files (rdfs:seeAlso)
	SordIter* files = sord_search(world->model,
	                              specification_node,
	                              world->uris.rdfs_seeAlso,
	                              NULL,
	                              NULL);
	FOREACH_MATCH(files) {
		const SordNode* file_node = sord_iter_get_node(files, SORD_OBJECT);
		zix_tree_insert((ZixTree*)spec->data_uris,
		                lilv_node_new_from_node(world, file_node),
		                NULL);
	}
	sord_iter_free(files);

	// Add specification to world specification list
	spec->next   = world->specs;
	world->specs = spec;
}

static void
lilv_world_add_plugin(LilvWorld*       world,
                      const SordNode*  plugin_node,
                      const LilvNode*  manifest_uri,
                      void*            dynmanifest,
                      const SordNode*  bundle_node)
{
	LilvNode* plugin_uri = lilv_node_new_from_node(world, plugin_node);

	const LilvPlugin* last = lilv_plugins_get_by_uri(world->plugins,
	                                                 plugin_uri);
	if (last) {
		LILV_ERRORF("Duplicate plugin <%s>\n", lilv_node_as_uri(plugin_uri));
		LILV_ERRORF("... found in %s\n", lilv_node_as_string(
			            lilv_plugin_get_bundle_uri(last)));
		LILV_ERRORF("... and      %s\n", sord_node_get_string(bundle_node));
		lilv_node_free(plugin_uri);
		return;
	}

	// Create LilvPlugin
	LilvNode*   bundle_uri = lilv_node_new_from_node(world, bundle_node);
	LilvPlugin* plugin     = lilv_plugin_new(world, plugin_uri, bundle_uri);

	// Add manifest as plugin data file (as if it were rdfs:seeAlso)
	zix_tree_insert((ZixTree*)plugin->data_uris,
	                lilv_node_duplicate(manifest_uri),
	                NULL);

#ifdef LILV_DYN_MANIFEST
	// Set dynamic manifest library URI, if applicable
	if (dynmanifest) {
		plugin->dynmanifest = (LilvDynManifest*)dynmanifest;
		++((LilvDynManifest*)dynmanifest)->refs;
	}
#endif

	// Add all plugin data files (rdfs:seeAlso)
	SordIter* files = sord_search(world->model,
	                              plugin_node,
	                              world->uris.rdfs_seeAlso,
	                              NULL,
	                              NULL);
	FOREACH_MATCH(files) {
		const SordNode* file_node = sord_iter_get_node(files, SORD_OBJECT);
		zix_tree_insert((ZixTree*)plugin->data_uris,
		                lilv_node_new_from_node(world, file_node),
		                NULL);
	}
	sord_iter_free(files);

	// Add plugin to world plugin sequence
	zix_tree_insert((ZixTree*)world->plugins, plugin, NULL);
}

SerdStatus
lilv_world_load_graph(LilvWorld* world, SordNode* graph, const LilvNode* uri)
{
	const SerdNode* base   = sord_node_to_serd_node(uri->node);
	SerdEnv*        env    = serd_env_new(base);
	SerdReader*     reader = sord_new_reader(
		world->model, env, SERD_TURTLE, graph);

	const SerdStatus st = lilv_world_load_file(world, reader, uri);

	serd_env_free(env);
	serd_reader_free(reader);
	return st;
}

static void
lilv_world_load_dyn_manifest(LilvWorld*      world,
                             SordNode*       bundle_node,
                             const LilvNode* manifest)
{
#ifdef LILV_DYN_MANIFEST
	if (!world->opt.dyn_manifest) {
		return;
	}

	typedef void* LV2_Dyn_Manifest_Handle;
	LV2_Dyn_Manifest_Handle handle = NULL;

	// ?dman a dynman:DynManifest
	SordIter* dmanifests = sord_search(world->model,
	                                   NULL,
	                                   world->uris.rdf_a,
	                                   world->uris.dman_DynManifest,
	                                   bundle_node);
	FOREACH_MATCH(dmanifests) {
		const SordNode* dmanifest = sord_iter_get_node(dmanifests, SORD_SUBJECT);

		// ?dman lv2:binary ?binary
		SordIter* binaries = sord_search(world->model,
		                                 dmanifest,
		                                 world->uris.lv2_binary,
		                                 NULL,
		                                 bundle_node);
		if (sord_iter_end(binaries)) {
			sord_iter_free(binaries);
			LILV_ERRORF("Dynamic manifest in <%s> has no binaries, ignored\n",
			            sord_node_get_string(bundle_node));
			continue;
		}

		// Get binary path
		const SordNode* binary   = sord_iter_get_node(binaries, SORD_OBJECT);
		const uint8_t*  lib_uri  = sord_node_get_string(binary);
		const char*     lib_path = lilv_uri_to_path((const char*)lib_uri);
		if (!lib_path) {
			LILV_ERROR("No dynamic manifest library path\n");
			sord_iter_free(binaries);
			continue;
		}

		// Open library
		void* lib = dlopen(lib_path, RTLD_LAZY);
		if (!lib) {
			LILV_ERRORF("Failed to open dynmanifest library `%s'\n", lib_path);
			sord_iter_free(binaries);
			continue;
		}

		// Open dynamic manifest
		typedef int (*OpenFunc)(LV2_Dyn_Manifest_Handle*,
		                        const LV2_Feature *const *);
		OpenFunc dmopen = (OpenFunc)lilv_dlfunc(lib, "lv2_dyn_manifest_open");
		if (!dmopen || dmopen(&handle, &dman_features)) {
			LILV_ERRORF("No `lv2_dyn_manifest_open' in `%s'\n", lib_path);
			sord_iter_free(binaries);
			dlclose(lib);
			continue;
		}

		// Get subjects (the data that would be in manifest.ttl)
		typedef int (*GetSubjectsFunc)(LV2_Dyn_Manifest_Handle, FILE*);
		GetSubjectsFunc get_subjects_func = (GetSubjectsFunc)lilv_dlfunc(
			lib, "lv2_dyn_manifest_get_subjects");
		if (!get_subjects_func) {
			LILV_ERRORF("No `lv2_dyn_manifest_get_subjects' in `%s'\n",
			            lib_path);
			sord_iter_free(binaries);
			dlclose(lib);
			continue;
		}

		LilvDynManifest* desc = malloc(sizeof(LilvDynManifest));
		desc->bundle = lilv_node_new_from_node(world, bundle_node);
		desc->lib    = lib;
		desc->handle = handle;
		desc->refs   = 0;

		// Generate data file
		FILE* fd = tmpfile();
		get_subjects_func(handle, fd);
		rewind(fd);

		// Parse generated data file
		const SerdNode* base   = sord_node_to_serd_node(dmanifest);
		SerdEnv*        env    = serd_env_new(base);
		SerdReader*     reader = sord_new_reader(
			world->model, env, SERD_TURTLE, sord_node_copy(dmanifest));
		serd_reader_add_blank_prefix(reader,
		                             lilv_world_blank_node_prefix(world));
		serd_reader_read_file_handle(reader, fd,
		                             (const uint8_t*)"(dyn-manifest)");
		serd_reader_free(reader);
		serd_env_free(env);

		// Close (and automatically delete) temporary data file
		fclose(fd);

		// ?plugin a lv2:Plugin
		SordIter* plug_results = sord_search(
			world->model,
			NULL,
			world->uris.rdf_a,
			world->uris.lv2_Plugin,
			dmanifest);
		FOREACH_MATCH(plug_results) {
			const SordNode* plug = sord_iter_get_node(plug_results, SORD_SUBJECT);
			lilv_world_add_plugin(world, plug, manifest, desc, bundle_node);
		}
		sord_iter_free(plug_results);

		sord_iter_free(binaries);
	}
	sord_iter_free(dmanifests);
#endif  // LILV_DYN_MANIFEST
}

static
LilvNode*
lilv_world_get_manifest_uri(LilvWorld* world, LilvNode* bundle_uri)
{
	SerdNode manifest_uri = lilv_new_uri_relative_to_base(
		(const uint8_t*)"manifest.ttl",
		(const uint8_t*)sord_node_get_string(bundle_uri->node));
	LilvNode* manifest = lilv_new_uri(world, (const char*)manifest_uri.buf);
	serd_node_free(&manifest_uri);
	return manifest;
}

LILV_API void
lilv_world_load_bundle(LilvWorld* world, LilvNode* bundle_uri)
{
	if (!lilv_node_is_uri(bundle_uri)) {
		LILV_ERRORF("Bundle URI `%s' is not a URI\n",
		            sord_node_get_string(bundle_uri->node));
		return;
	}

	SordNode* bundle_node = bundle_uri->node;
	LilvNode* manifest    = lilv_world_get_manifest_uri(world, bundle_uri);

	// Read manifest into model with graph = bundle_node
	SerdStatus st = lilv_world_load_graph(world, bundle_node, manifest);
	if (st > SERD_FAILURE) {
		LILV_ERRORF("Error reading %s\n", lilv_node_as_string(manifest));
		lilv_node_free(manifest);
		return;
	}
		
	// ?plugin a lv2:Plugin
	SordIter* plug_results = sord_search(world->model,
	                                     NULL,
	                                     world->uris.rdf_a,
	                                     world->uris.lv2_Plugin,
	                                     bundle_node);
	FOREACH_MATCH(plug_results) {
		const SordNode* plug = sord_iter_get_node(plug_results, SORD_SUBJECT);
		lilv_world_add_plugin(world, plug, manifest, NULL, bundle_node);
	}
	sord_iter_free(plug_results);

	lilv_world_load_dyn_manifest(world, bundle_node, manifest);

	// ?specification a lv2:Specification
	SordIter* spec_results = sord_search(world->model,
	                                     NULL,
	                                     world->uris.rdf_a,
	                                     world->uris.lv2_Specification,
	                                     bundle_node);
	FOREACH_MATCH(spec_results) {
		const SordNode* spec = sord_iter_get_node(spec_results, SORD_SUBJECT);
		lilv_world_add_spec(world, spec, bundle_node);
	}
	sord_iter_free(spec_results);

	lilv_node_free(manifest);
}

static int
lilv_world_drop_graph(LilvWorld* world, LilvNode* graph)
{
	SordIter* i = sord_search(world->model, NULL, NULL, NULL, graph->node);
	
	while (!sord_iter_end(i)) {
		// Get quad and increment iter so sord_remove doesn't invalidate it
		SordQuad quad;
		sord_iter_get(i, quad);
		sord_iter_next(i);

		// Remove quad (nodes may now be deleted, quad is invalid)
		sord_remove(world->model, quad);
	}
	sord_iter_free(i);

	return 0;
}

/** Remove loaded_files entry so file will be reloaded if requested. */
static int
lilv_world_unload_file(LilvWorld* world, LilvNode* file)
{
	ZixTreeIter* iter;
	if (!zix_tree_find((ZixTree*)world->loaded_files, file, &iter)) {
		zix_tree_remove((ZixTree*)world->loaded_files, iter);
		return 0;
	}
	return 1;
}

LILV_API int
lilv_world_unload_bundle(LilvWorld* world, LilvNode* bundle_uri)
{
	// Remove loaded_files entry for manifest.ttl
	LilvNode* manifest = lilv_world_get_manifest_uri(world, bundle_uri);
	lilv_world_unload_file(world, manifest);
	lilv_node_free(manifest);

	// Drop everything in bundle graph
	return lilv_world_drop_graph(world, bundle_uri);
}

static void
load_dir_entry(const char* dir, const char* name, void* data)
{
	LilvWorld* world = (LilvWorld*)data;
	if (!strcmp(name, ".") || !strcmp(name, ".."))
		return;

	const char* scheme  = (dir[0] == '/') ? "file://" : "file:///";
	char*       uri     = lilv_strjoin(scheme, dir, "/", name, "/", NULL);
	LilvNode*   uri_val = lilv_new_uri(world, uri);

	lilv_world_load_bundle(world, uri_val);
	lilv_node_free(uri_val);
	free(uri);
}

/** Load all bundles in the directory at `dir_path`. */
static void
lilv_world_load_directory(LilvWorld* world, const char* dir_path)
{
	char* path = lilv_expand(dir_path);
	if (path) {
		lilv_dir_for_each(path, world, load_dir_entry);
		free(path);
	}
}

static const char*
first_path_sep(const char* path)
{
	for (const char* p = path; *p != '\0'; ++p) {
		if (*p == LILV_PATH_SEP[0]) {
			return p;
		}
	}
	return NULL;
}

/** Load all bundles found in `lv2_path`.
 * @param lv2_path A colon-delimited list of directories.  These directories
 * should contain LV2 bundle directories (ie the search path is a list of
 * parent directories of bundles, not a list of bundle directories).
 */
static void
lilv_world_load_path(LilvWorld*  world,
                     const char* lv2_path)
{
	while (lv2_path[0] != '\0') {
		const char* const sep = first_path_sep(lv2_path);
		if (sep) {
			const size_t dir_len = sep - lv2_path;
			char* const  dir     = (char*)malloc(dir_len + 1);
			memcpy(dir, lv2_path, dir_len);
			dir[dir_len] = '\0';
			lilv_world_load_directory(world, dir);
			free(dir);
			lv2_path += dir_len + 1;
		} else {
			lilv_world_load_directory(world, lv2_path);
			lv2_path = "\0";
		}
	}
}

static void
lilv_world_load_specifications(LilvWorld* world)
{
	for (LilvSpec* spec = world->specs; spec; spec = spec->next) {
		LILV_FOREACH(nodes, f, spec->data_uris) {
			LilvNode* file = (LilvNode*)lilv_collection_get(spec->data_uris, f);
			lilv_world_load_graph(world, NULL, file);
		}
	}
}

static void
lilv_world_load_plugin_classes(LilvWorld* world)
{
	/* FIXME: This loads all classes, not just lv2:Plugin subclasses.
	   However, if the host gets all the classes via lilv_plugin_class_get_children
	   starting with lv2:Plugin as the root (which is e.g. how a host would build
	   a menu), they won't be seen anyway...
	*/

	SordIter* classes = sord_search(world->model,
	                                NULL,
	                                world->uris.rdf_a,
	                                world->uris.rdfs_Class,
	                                NULL);
	FOREACH_MATCH(classes) {
		const SordNode* class_node = sord_iter_get_node(classes, SORD_SUBJECT);

		SordNode* parent = sord_get(
			world->model, class_node, world->uris.rdfs_subClassOf, NULL, NULL);
		if (!parent || sord_node_get_type(parent) != SORD_URI) {
			continue;
		}

		SordNode* label = sord_get(
			world->model, class_node, world->uris.rdfs_label, NULL, NULL);
		if (!label) {
			sord_node_free(world->world, parent);
			continue;
		}

		LilvPluginClass* pclass = lilv_plugin_class_new(
			world, parent, class_node,
			(const char*)sord_node_get_string(label));
		if (pclass) {
			zix_tree_insert((ZixTree*)world->plugin_classes, pclass, NULL);
		}

		sord_node_free(world->world, label);
		sord_node_free(world->world, parent);
	}
	sord_iter_free(classes);
}

LILV_API void
lilv_world_load_all(LilvWorld* world)
{
	const char* lv2_path = getenv("LV2_PATH");
	if (!lv2_path)
		lv2_path = LILV_DEFAULT_LV2_PATH;

	// Discover bundles and read all manifest files into model
	lilv_world_load_path(world, lv2_path);

	LILV_FOREACH(plugins, p, world->plugins) {
		const LilvPlugin* plugin = (const LilvPlugin*)lilv_collection_get(
			(ZixTree*)world->plugins, p);

		// ?new dc:replaces plugin
		if (sord_ask(world->model,
		             NULL,
		             world->uris.dc_replaces,
		             lilv_plugin_get_uri(plugin)->node,
		             NULL)) {
			// TODO: Check if replacement is a known plugin? (expensive)
			((LilvPlugin*)plugin)->replaced = true;
		}
	}

	// Query out things to cache
	lilv_world_load_specifications(world);
	lilv_world_load_plugin_classes(world);
}

SerdStatus
lilv_world_load_file(LilvWorld* world, SerdReader* reader, const LilvNode* uri)
{
	ZixTreeIter* iter;
	if (!zix_tree_find((ZixTree*)world->loaded_files, uri, &iter)) {
		return SERD_FAILURE;  // File has already been loaded
	}

	serd_reader_add_blank_prefix(reader, lilv_world_blank_node_prefix(world));
	const SerdStatus st = serd_reader_read_file(
		reader, sord_node_get_string(uri->node));
	if (st) {
		LILV_ERRORF("Error loading file `%s'\n", lilv_node_as_string(uri));
		return st;
	}
		
	zix_tree_insert((ZixTree*)world->loaded_files,
	                lilv_node_duplicate(uri),
	                NULL);
	return SERD_SUCCESS;
}

LILV_API int
lilv_world_load_resource(LilvWorld*      world,
                         const LilvNode* resource)
{
	if (!lilv_node_is_uri(resource) && !lilv_node_is_blank(resource)) {
		LILV_ERRORF("Node `%s' is not a resource\n",
		            sord_node_get_string(resource->node));
		return -1;
	}

	int       n_read = 0;
	SordIter* files  = sord_search(world->model,
	                               resource->node,
	                               world->uris.rdfs_seeAlso,
	                               NULL, NULL);
	FOREACH_MATCH(files) {
		const SordNode* file      = sord_iter_get_node(files, SORD_OBJECT);
		const uint8_t*  file_str  = sord_node_get_string(file);
		LilvNode*       file_node = lilv_node_new_from_node(world, file);
		if (sord_node_get_type(file) != SORD_URI) {
			LILV_ERRORF("rdfs:seeAlso node `%s' is not a URI\n", file_str);
		} else if (!lilv_world_load_graph(world, (SordNode*)file, file_node)) {
			++n_read;
		}
		lilv_node_free(file_node);
	}
	sord_iter_free(files);

	return n_read;
}

LILV_API int
lilv_world_unload_resource(LilvWorld*      world,
                           const LilvNode* resource)
{
	if (!lilv_node_is_uri(resource) && !lilv_node_is_blank(resource)) {
		LILV_ERRORF("Node `%s' is not a resource\n",
		            sord_node_get_string(resource->node));
		return -1;
	}

	int       n_dropped = 0;
	SordIter* files     = sord_search(world->model,
	                                  resource->node,
	                                  world->uris.rdfs_seeAlso,
	                                  NULL, NULL);
	FOREACH_MATCH(files) {
		const SordNode* file      = sord_iter_get_node(files, SORD_OBJECT);
		LilvNode*       file_node = lilv_node_new_from_node(world, file);
		if (sord_node_get_type(file) != SORD_URI) {
			LILV_ERRORF("rdfs:seeAlso node `%s' is not a URI\n",
			            sord_node_get_string(file));
		} else if (!lilv_world_drop_graph(world, file_node)) {
			lilv_world_unload_file(world, file_node);
			++n_dropped;
		}
		lilv_node_free(file_node);
	}
	sord_iter_free(files);

	return n_dropped;
}

LILV_API const LilvPluginClass*
lilv_world_get_plugin_class(const LilvWorld* world)
{
	return world->lv2_plugin_class;
}

LILV_API const LilvPluginClasses*
lilv_world_get_plugin_classes(const LilvWorld* world)
{
	return world->plugin_classes;
}

LILV_API const LilvPlugins*
lilv_world_get_all_plugins(const LilvWorld* world)
{
	return world->plugins;
}
