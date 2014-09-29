/*
  Copyright 2007-2012 David Robillard <http://drobilla.net>

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

#include <errno.h>
#include <stdio.h>
#include <string.h>

#include "lv2/lv2plug.in/ns/ext/atom/atom.h"
#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/presets/presets.h"
#include "lv2/lv2plug.in/ns/ext/state/state.h"

#include "lilv_config.h"
#include "lilv_internal.h"
#include "sratom/sratom.h"

#define USTR(s) ((const uint8_t*)(s))

typedef struct {
	void*    value;  ///< Value/Object
	size_t   size;   ///< Size of value
	uint32_t key;    ///< Key/Predicate (URID)
	uint32_t type;   ///< Type of value (URID)
	uint32_t flags;  ///< State flags (POD, etc)
} Property;

typedef struct {
	char*    symbol;  ///< Symbol of port
	void*    value;   ///< Value of port
	uint32_t size;    ///< Size of value
	uint32_t type;    ///< Type of value (URID)
} PortValue;

typedef struct {
	char* abs;  ///< Absolute path of actual file
	char* rel;  ///< Abstract path (relative path in state dir)
} PathMap;

struct LilvStateImpl {
	LilvNode*  plugin_uri;  ///< Plugin URI
	char*      dir;         ///< Save directory (if saved)
	char*      file_dir;    ///< Directory for files created by plugin
	char*      copy_dir;    ///< Directory for snapshots of external files
	char*      link_dir;    ///< Directory for links to external files
	char*      label;       ///< State/Preset label
	ZixTree*   abs2rel;     ///< PathMap sorted by abs
	ZixTree*   rel2abs;     ///< PathMap sorted by rel
	Property*  props;       ///< State properties
	PortValue* values;      ///< Port values
	uint32_t   atom_Path;   ///< atom:Path URID
	uint32_t   num_props;   ///< Number of state properties
	uint32_t   num_values;  ///< Number of port values
};

static int
abs_cmp(const void* a, const void* b, void* user_data)
{
	return strcmp(((const PathMap*)a)->abs, ((const PathMap*)b)->abs);
}

static int
rel_cmp(const void* a, const void* b, void* user_data)
{
	return strcmp(((const PathMap*)a)->rel, ((const PathMap*)b)->rel);
}

static int
property_cmp(const void* a, const void* b)
{
	return ((const Property*)a)->key - ((const Property*)b)->key;
}

static int
value_cmp(const void* a, const void* b)
{
	return strcmp(((const PortValue*)a)->symbol,
	              ((const PortValue*)b)->symbol);
}

static void
path_rel_free(void* ptr)
{
	free(((PathMap*)ptr)->abs);
	free(((PathMap*)ptr)->rel);
	free(ptr);
}

static PortValue*
append_port_value(LilvState*  state,
                  const char* port_symbol,
                  const void* value,
                  uint32_t    size,
                  uint32_t    type)
{
	if (value) {
		state->values = (PortValue*)realloc(
			state->values, (++state->num_values) * sizeof(PortValue));
		PortValue* pv = &state->values[state->num_values - 1];
		pv->symbol = lilv_strdup(port_symbol);
		pv->value  = malloc(size);
		pv->size   = size;
		pv->type   = type;
		memcpy(pv->value, value, size);
		return pv;
	}
	return NULL;
}

static const char*
lilv_state_rel2abs(const LilvState* state, const char* path)
{
	ZixTreeIter*  iter = NULL;
	const PathMap key  = { NULL, (char*)path };
	if (state->rel2abs && !zix_tree_find(state->rel2abs, &key, &iter)) {
		return ((const PathMap*)zix_tree_get(iter))->abs;
	}
	return path;
}

static LV2_State_Status
store_callback(LV2_State_Handle handle,
               uint32_t         key,
               const void*      value,
               size_t           size,
               uint32_t         type,
               uint32_t         flags)
{
	LilvState* const state = (LilvState*)handle;
	state->props = (Property*)realloc(
		state->props, (++state->num_props) * sizeof(Property));
	Property* const prop = &state->props[state->num_props - 1];

	if ((flags & LV2_STATE_IS_POD) || type == state->atom_Path) {
		prop->value = malloc(size);
		memcpy(prop->value, value, size);
	} else {
		LILV_WARN("Storing non-POD value\n");
		prop->value = (void*)value;
	}

	prop->size  = size;
	prop->key   = key;
	prop->type  = type;
	prop->flags = flags;

	return LV2_STATE_SUCCESS;
}

static const void*
retrieve_callback(LV2_State_Handle handle,
                  uint32_t         key,
                  size_t*          size,
                  uint32_t*        type,
                  uint32_t*        flags)
{
	const LilvState* const state      = (LilvState*)handle;
	const Property         search_key = { NULL, 0, key, 0, 0 };
	const Property* const  prop       = (Property*)bsearch(
		&search_key, state->props, state->num_props,
		sizeof(Property), property_cmp);

	if (prop) {
		*size  = prop->size;
		*type  = prop->type;
		*flags = prop->flags;
		return prop->value;
	}
	return NULL;
}

static bool
lilv_state_has_path(const char* path, void* state)
{
	return lilv_state_rel2abs((LilvState*)state, path) != path;
}

static char*
make_path(LV2_State_Make_Path_Handle handle, const char* path)
{
	LilvState* state = (LilvState*)handle;
	if (!lilv_path_exists(state->dir, NULL)) {
		lilv_mkdir_p(state->dir);
	}

	return lilv_path_join(state->dir, path);
}

static char*
abstract_path(LV2_State_Map_Path_Handle handle,
              const char*               abs_path)
{
	LilvState*    state     = (LilvState*)handle;
	char*         path      = NULL;
	char*         real_path = lilv_realpath(abs_path);
	const PathMap key       = { (char*)real_path, NULL };
	ZixTreeIter*  iter      = NULL;

	if (abs_path[0] == '\0') {
		return lilv_strdup(abs_path);
	} else if (!zix_tree_find(state->abs2rel, &key, &iter)) {
		// Already mapped path in a previous call
		PathMap* pm = (PathMap*)zix_tree_get(iter);
		free(real_path);
		return lilv_strdup(pm->rel);
	} else if (lilv_path_is_child(real_path, state->dir)) {
		// File in state directory (loaded, or created by plugin during save
		path = lilv_path_relative_to(real_path, state->dir);
	} else if (lilv_path_is_child(real_path, state->file_dir)) {
		// File created by plugin earlier
		path = lilv_path_relative_to(real_path, state->file_dir);
		if (state->copy_dir) {
			if (!lilv_path_exists(state->copy_dir, NULL)) {
				lilv_mkdir_p(state->copy_dir);
			}
			char* cpath = lilv_path_join(state->copy_dir, path);
			char* copy  = lilv_get_latest_copy(real_path, cpath);
			if (!copy || !lilv_file_equals(real_path, copy)) {
				// No recent enough copy, make a new one
				copy = lilv_find_free_path(cpath, lilv_path_exists, NULL);
				lilv_copy_file(real_path, copy);
			}
			free(real_path);
			free(cpath);

			// Refer to the latest copy in plugin state
			real_path = copy;
		}
	} else {
		// New path outside state directory
		const char* slash = strrchr(real_path, '/');
		const char* name  = slash ? (slash + 1) : real_path;

		// Find a free name in the (virtual) state directory
		path = lilv_find_free_path(name, lilv_state_has_path, state);
	}

	// Add record to path mapping
	PathMap* pm = (PathMap*)malloc(sizeof(PathMap));
	pm->abs = real_path;
	pm->rel = lilv_strdup(path);
	zix_tree_insert(state->abs2rel, pm, NULL);
	zix_tree_insert(state->rel2abs, pm, NULL);

	return path;
}

static char*
absolute_path(LV2_State_Map_Path_Handle handle,
              const char*               state_path)
{
	LilvState* state = (LilvState*)handle;
	char*      path  = NULL;
	if (lilv_path_is_absolute(state_path)) {
		// Absolute path, return identical path
		path = lilv_strdup(state_path);
	} else if (state->dir) {
		// Relative path inside state directory
		path = lilv_path_join(state->dir, state_path);
	} else {
		// State has not been saved, unmap
		path = lilv_strdup(lilv_state_rel2abs(state, state_path));
	}

	return path;
}

/** Return a new features array which is @c feature added to @c features. */
static const LV2_Feature**
add_features(const LV2_Feature *const * features,
             const LV2_Feature* map, const LV2_Feature* make)
{
	size_t n_features = 0;
	for (; features && features[n_features]; ++n_features) {}

	const LV2_Feature** ret = (const LV2_Feature**)calloc(
		n_features + 3, sizeof(LV2_Feature*));

	if (features) {
		memcpy(ret, features, n_features * sizeof(LV2_Feature*));
	}

	ret[n_features]     = map;
	ret[n_features + 1] = make;
	return ret;
}

static char*
absolute_dir(const char* path)
{
	char* abs_path = lilv_path_absolute(path);
	char* base     = lilv_path_join(abs_path, NULL);
	free(abs_path);
	return base;
}

static const char*
state_strerror(LV2_State_Status st)
{
	switch (st) {
	case LV2_STATE_SUCCESS:         return "Completed successfully";
	case LV2_STATE_ERR_BAD_TYPE:    return "Unsupported type";
	case LV2_STATE_ERR_BAD_FLAGS:   return "Unsupported flags";
	case LV2_STATE_ERR_NO_FEATURE:  return "Missing features";
	case LV2_STATE_ERR_NO_PROPERTY: return "Missing property";
	default:                        return "Unknown error";
	}
}

LILV_API
LilvState*
lilv_state_new_from_instance(const LilvPlugin*          plugin,
                             LilvInstance*              instance,
                             LV2_URID_Map*              map,
                             const char*                file_dir,
                             const char*                copy_dir,
                             const char*                link_dir,
                             const char*                save_dir,
                             LilvGetPortValueFunc       get_value,
                             void*                      user_data,
                             uint32_t                   flags,
                             const LV2_Feature *const * features)
{
	const LV2_Feature** sfeatures = NULL;
	LilvWorld* const    world     = plugin->world;
	LilvState* const    state     = (LilvState*)malloc(sizeof(LilvState));
	memset(state, '\0', sizeof(LilvState));
	state->plugin_uri = lilv_node_duplicate(lilv_plugin_get_uri(plugin));
	state->abs2rel    = zix_tree_new(false, abs_cmp, NULL, path_rel_free);
	state->rel2abs    = zix_tree_new(false, rel_cmp, NULL, NULL);
	state->file_dir   = file_dir ? absolute_dir(file_dir) : NULL;
	state->copy_dir   = copy_dir ? absolute_dir(copy_dir) : NULL;
	state->link_dir   = link_dir ? absolute_dir(link_dir) : NULL;
	state->dir        = save_dir ? absolute_dir(save_dir) : NULL;
	state->atom_Path  = map->map(map->handle, LV2_ATOM__Path);

	LV2_State_Map_Path  pmap          = { state, abstract_path, absolute_path };
	LV2_Feature         pmap_feature  = { LV2_STATE__mapPath, &pmap };
	LV2_State_Make_Path pmake         = { state, make_path };
	LV2_Feature         pmake_feature = { LV2_STATE__makePath, &pmake };
	features = sfeatures = add_features(features, &pmap_feature,
	                                    save_dir ? &pmake_feature : NULL);

	// Store port values
	if (get_value) {
		LilvNode* lv2_ControlPort = lilv_new_uri(world, LILV_URI_CONTROL_PORT);
		LilvNode* lv2_InputPort   = lilv_new_uri(world, LILV_URI_INPUT_PORT);
		for (uint32_t i = 0; i < plugin->num_ports; ++i) {
			const LilvPort* const port = plugin->ports[i];
			if (lilv_port_is_a(plugin, port, lv2_ControlPort)
			    && lilv_port_is_a(plugin, port, lv2_InputPort)) {
				uint32_t size, type;
				const char* sym   = lilv_node_as_string(port->symbol);
				const void* value = get_value(sym, user_data, &size, &type);
				append_port_value(state, sym, value, size, type);
			}
		}
		lilv_node_free(lv2_ControlPort);
		lilv_node_free(lv2_InputPort);
	}

	// Store properties
	const LV2_Descriptor*      desc  = instance->lv2_descriptor;
	const LV2_State_Interface* iface = (desc->extension_data)
		? (LV2_State_Interface*)desc->extension_data(LV2_STATE__interface)
		: NULL;

	if (iface) {
		LV2_State_Status st = iface->save(
			instance->lv2_handle, store_callback, state, flags, features);
		if (st) {
			LILV_ERRORF("Error saving plugin state: %s\n", state_strerror(st));
			free(state->props);
			state->props     = NULL;
			state->num_props = 0;
		} else {
			qsort(state->props, state->num_props, sizeof(Property), property_cmp);
		}
	}

	qsort(state->values, state->num_values, sizeof(PortValue), value_cmp);

	free(sfeatures);
	return state;
}

LILV_API
void
lilv_state_restore(const LilvState*           state,
                   LilvInstance*              instance,
                   LilvSetPortValueFunc       set_value,
                   void*                      user_data,
                   uint32_t                   flags,
                   const LV2_Feature *const * features)
{
	LV2_State_Map_Path map_path = {
		(LilvState*)state, abstract_path, absolute_path };
	LV2_Feature map_feature = { LV2_STATE__mapPath, &map_path };

	const LV2_Feature** sfeatures = add_features(features, &map_feature, NULL);

	const LV2_Descriptor*      desc  = instance->lv2_descriptor;
	const LV2_State_Interface* iface = (desc->extension_data)
		? (LV2_State_Interface*)desc->extension_data(LV2_STATE__interface)
		: NULL;

	if (iface) {
		iface->restore(instance->lv2_handle, retrieve_callback,
		               (LV2_State_Handle)state, flags, sfeatures);
	}

	free(sfeatures);

	if (set_value) {
		for (uint32_t i = 0; i < state->num_values; ++i) {
			const PortValue* val = &state->values[i];
			set_value(val->symbol, user_data,
			          val->value, val->size, val->type);
		}
	}
}

static LilvState*
new_state_from_model(LilvWorld*       world,
                     LV2_URID_Map*    map,
                     SordModel*       model,
                     const SordNode*  node,
                     const char*      dir)
{
	LilvState* const state = (LilvState*)malloc(sizeof(LilvState));
	memset(state, '\0', sizeof(LilvState));
	state->dir       = lilv_strdup(dir);
	state->atom_Path = map->map(map->handle, LV2_ATOM__Path);

	// Get the plugin URI this state applies to
	SordIter* i = sord_search(model, node, world->uris.lv2_appliesTo, 0, 0);
	if (i) {
		const SordNode* object = sord_iter_get_node(i, SORD_OBJECT);
		const SordNode* graph  = sord_iter_get_node(i, SORD_GRAPH);
		state->plugin_uri = lilv_node_new_from_node(world, object);
		if (!state->dir && graph) {
			state->dir = lilv_strdup((const char*)sord_node_get_string(graph));
		}
		sord_iter_free(i);
	} else if (sord_ask(model,
	                    node,
	                    world->uris.rdf_a,
	                    world->uris.lv2_Plugin, 0)) {
		// Loading plugin description as state (default state)
		state->plugin_uri = lilv_node_new_from_node(world, node);
	} else {
		LILV_ERRORF("State %s missing lv2:appliesTo property\n",
		            sord_node_get_string(node));
	}

	// Get the state label
	i = sord_search(model, node, world->uris.rdfs_label, NULL, NULL);
	if (i) {
		const SordNode* object = sord_iter_get_node(i, SORD_OBJECT);
		const SordNode* graph  = sord_iter_get_node(i, SORD_GRAPH);
		state->label = lilv_strdup((const char*)sord_node_get_string(object));
		if (!state->dir) {
			state->dir = lilv_strdup((const char*)sord_node_get_string(graph));
		}
		sord_iter_free(i);
	}

	Sratom*        sratom = sratom_new(map);
	SerdChunk      chunk  = { NULL, 0 };
	LV2_Atom_Forge forge;
	lv2_atom_forge_init(&forge, map);
	lv2_atom_forge_set_sink(
		&forge, sratom_forge_sink, sratom_forge_deref, &chunk);

	// Get port values
	SordIter* ports = sord_search(model, node, world->uris.lv2_port, 0, 0);
	FOREACH_MATCH(ports) {
		const SordNode* port = sord_iter_get_node(ports, SORD_OBJECT);

		SordNode* label  = sord_get(model, port, world->uris.rdfs_label, 0, 0);
		SordNode* symbol = sord_get(model, port, world->uris.lv2_symbol, 0, 0);
		SordNode* value  = sord_get(model, port, world->uris.pset_value, 0, 0);
		if (!value) {
			value = sord_get(model, port, world->uris.lv2_default, 0, 0);
		}
		if (!symbol) {
			LILV_ERRORF("State `%s' port missing symbol.\n",
			            sord_node_get_string(node));
		} else if (value) {
			chunk.len = 0;
			sratom_read(sratom, &forge, world->world, model, value);
			LV2_Atom* atom = (LV2_Atom*)chunk.buf;

			append_port_value(state,
			                  (const char*)sord_node_get_string(symbol),
			                  LV2_ATOM_BODY(atom), atom->size, atom->type);

			if (label) {
				lilv_state_set_label(state,
				                     (const char*)sord_node_get_string(label));
			}
		}
		sord_node_free(world->world, value);
		sord_node_free(world->world, symbol);
		sord_node_free(world->world, label);
	}
	sord_iter_free(ports);

	// Get properties
	SordNode* statep     = sord_new_uri(world->world, USTR(LV2_STATE__state));
	SordNode* state_node = sord_get(model, node, statep, NULL, NULL);
	if (state_node) {
		SordIter* props = sord_search(model, state_node, 0, 0, 0);
		FOREACH_MATCH(props) {
			const SordNode* p = sord_iter_get_node(props, SORD_PREDICATE);
			const SordNode* o = sord_iter_get_node(props, SORD_OBJECT);

			chunk.len = 0;
			lv2_atom_forge_set_sink(
				&forge, sratom_forge_sink, sratom_forge_deref, &chunk);

			sratom_read(sratom, &forge, world->world, model, o);
			LV2_Atom* atom  = (LV2_Atom*)chunk.buf;
			uint32_t  flags = LV2_STATE_IS_POD|LV2_STATE_IS_PORTABLE;
			Property  prop  = { NULL, 0, 0, 0, flags };

			prop.key   = map->map(map->handle, (const char*)sord_node_get_string(p));
			prop.type  = atom->type;
			prop.size  = atom->size;
			prop.value = malloc(atom->size);
			memcpy(prop.value, LV2_ATOM_BODY(atom), atom->size);
			if (atom->type == forge.Path) {
				prop.flags = LV2_STATE_IS_PORTABLE;
			}

			if (prop.value) {
				state->props = (Property*)realloc(
					state->props, (++state->num_props) * sizeof(Property));
				state->props[state->num_props - 1] = prop;
			}
		}
		sord_iter_free(props);
	}
	sord_node_free(world->world, state_node);
	sord_node_free(world->world, statep);

	free((void*)chunk.buf);
	sratom_free(sratom);

	qsort(state->props, state->num_props, sizeof(Property), property_cmp);
	qsort(state->values, state->num_values, sizeof(PortValue), value_cmp);

	return state;
}

LILV_API
LilvState*
lilv_state_new_from_world(LilvWorld*      world,
                          LV2_URID_Map*   map,
                          const LilvNode* node)
{
	if (!lilv_node_is_uri(node) && !lilv_node_is_blank(node)) {
		LILV_ERRORF("Subject `%s' is not a URI or blank node.\n",
		            lilv_node_as_string(node));
		return NULL;
	}

	LilvState* state = new_state_from_model(
		world, map, world->model, node->node, NULL);

	return state;
}

LILV_API
LilvState*
lilv_state_new_from_file(LilvWorld*      world,
                         LV2_URID_Map*   map,
                         const LilvNode* subject,
                         const char*     path)
{
	if (subject && !lilv_node_is_uri(subject)
	    && !lilv_node_is_blank(subject)) {
		LILV_ERRORF("Subject `%s' is not a URI or blank node.\n",
		            lilv_node_as_string(subject));
		return NULL;
	}

	uint8_t*    abs_path = (uint8_t*)lilv_path_absolute(path);
	SerdNode    node     = serd_node_new_file_uri(abs_path, NULL, NULL, 0);
	SerdEnv*    env      = serd_env_new(&node);
	SordModel*  model    = sord_new(world->world, SORD_SPO, false);
	SerdReader* reader   = sord_new_reader(model, env, SERD_TURTLE, NULL);

	serd_reader_read_file(reader, node.buf);

	SordNode* subject_node = (subject)
		? subject->node
		: sord_node_from_serd_node(world->world, env, &node, NULL, NULL);

	char* dirname   = lilv_dirname(path);
	char* real_path = lilv_realpath(dirname);
	LilvState* state = new_state_from_model(
		world, map, model, subject_node, real_path);
	free(dirname);
	free(real_path);

	serd_node_free(&node);
	free(abs_path);
	serd_reader_free(reader);
	sord_free(model);
	serd_env_free(env);
	return state;
}

static void
set_prefixes(SerdEnv* env)
{
#define SET_PSET(e, p, u) serd_env_set_prefix_from_strings(e, p, u)
	SET_PSET(env, USTR("atom"),  USTR(LV2_ATOM_PREFIX));
	SET_PSET(env, USTR("lv2"),   USTR(LV2_CORE_PREFIX));
	SET_PSET(env, USTR("pset"),  USTR(LV2_PRESETS_PREFIX));
	SET_PSET(env, USTR("rdf"),   USTR(LILV_NS_RDF));
	SET_PSET(env, USTR("rdfs"),  USTR(LILV_NS_RDFS));
	SET_PSET(env, USTR("state"), USTR(LV2_STATE_PREFIX));
	SET_PSET(env, USTR("xsd"),   USTR(LILV_NS_XSD));
}

LILV_API
LilvState*
lilv_state_new_from_string(LilvWorld*    world,
                           LV2_URID_Map* map,
                           const char*   str)
{
	if (!str) {
		return NULL;
	}

	SerdNode    base   = SERD_NODE_NULL;
	SerdEnv*    env    = serd_env_new(&base);
	SordModel*  model  = sord_new(world->world, SORD_SPO|SORD_OPS, false);
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	set_prefixes(env);
	serd_reader_read_string(reader, USTR(str));

	SordNode* o = sord_new_uri(world->world, USTR(LV2_PRESETS__Preset));
	SordNode* s = sord_get(model, NULL, world->uris.rdf_a, o, NULL);

	LilvState* state = new_state_from_model(world, map, model, s, NULL);

	sord_node_free(world->world, s);
	sord_node_free(world->world, o);
	serd_reader_free(reader);
	sord_free(model);
	serd_env_free(env);

	return state;
}

static SerdWriter*
ttl_writer(SerdSink sink, void* stream, const SerdNode* base, SerdEnv** new_env)
{
	SerdURI base_uri = SERD_URI_NULL;
	if (base && base->buf) {
		serd_uri_parse(base->buf, &base_uri);
	}

	SerdEnv* env = serd_env_new(base);
	set_prefixes(env);

	SerdWriter* writer = serd_writer_new(
		SERD_TURTLE,
		(SerdStyle)(SERD_STYLE_RESOLVED|SERD_STYLE_ABBREVIATED|SERD_STYLE_CURIED),
		env,
		&base_uri,
		sink,
		stream);

	*new_env = env;
	return writer;
}

static SerdWriter*
ttl_file_writer(FILE* fd, const SerdNode* node, SerdEnv** env)
{
	SerdWriter* writer = ttl_writer(serd_file_sink, fd, node, env);

	fseek(fd, 0, SEEK_END);
	if (ftell(fd) == 0) {
		serd_env_foreach(*env, (SerdPrefixSink)serd_writer_set_prefix, writer);
	} else {
		fprintf(fd, "\n");
	}

	return writer;
}

static int
add_state_to_manifest(const LilvNode* plugin_uri,
                      const char*     manifest_path,
                      const char*     state_uri,
                      const char*     state_path)
{
	FILE* fd = fopen((char*)manifest_path, "a");
	if (!fd) {
		LILV_ERRORF("Failed to open %s (%s)\n",
		            manifest_path, strerror(errno));
		return 4;
	}

	lilv_flock(fd, true);

	SerdNode    file     = serd_node_new_file_uri(USTR(state_path), 0, 0, 0);
	SerdNode    manifest = serd_node_new_file_uri(USTR(manifest_path), 0, 0, 0);
	SerdEnv*    env      = NULL;
	SerdWriter* writer   = ttl_file_writer(fd, &manifest, &env);

	if (!state_uri) {
		state_uri = (const char*)file.buf;
	}

	// <state> a pset:Preset
	SerdNode s = serd_node_from_string(SERD_URI, USTR(state_uri));
	SerdNode p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDF "type"));
	SerdNode o = serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__Preset));
	serd_writer_write_statement(writer, 0, NULL, &s, &p, &o, NULL, NULL);

	// <state> rdfs:seeAlso <file>
	p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDFS "seeAlso"));
	serd_writer_write_statement(writer, 0, NULL, &s, &p, &file, NULL, NULL);

	// <state> lv2:appliesTo <plugin>
	p = serd_node_from_string(SERD_URI, USTR(LV2_CORE__appliesTo));
	o = serd_node_from_string(
		SERD_URI, USTR(lilv_node_as_string(plugin_uri)));
	serd_writer_write_statement(writer, 0, NULL, &s, &p, &o, NULL, NULL);

	serd_node_free(&file);
	serd_node_free(&manifest);
	serd_writer_free(writer);
	serd_env_free(env);

	lilv_flock(fd, false);
	fclose(fd);

	return 0;
}

static bool
link_exists(const char* path, void* data)
{
	const char* target = (const char*)data;
	if (!lilv_path_exists(path, NULL)) {
		return false;
	}
	char* real_path = lilv_realpath(path);
	bool  matches   = !strcmp(real_path, target);
	free(real_path);
	return !matches;
}

static int
lilv_state_write(LilvWorld*       world,
                 LV2_URID_Map*    map,
                 LV2_URID_Unmap*  unmap,
                 const LilvState* state,
                 SerdWriter*      writer,
                 const char*      uri,
                 const char*      dir)
{
	SerdNode lv2_appliesTo = serd_node_from_string(
		SERD_CURIE, USTR("lv2:appliesTo"));

	const SerdNode* plugin_uri = sord_node_to_serd_node(
		state->plugin_uri->node);

	SerdNode subject = serd_node_from_string(SERD_URI, USTR(uri ? uri : ""));

	// <subject> a pset:Preset
	SerdNode p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDF "type"));
	SerdNode o = serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__Preset));
	serd_writer_write_statement(writer, 0, NULL,
	                            &subject, &p, &o, NULL, NULL);

	// <subject> lv2:appliesTo <http://example.org/plugin>
	serd_writer_write_statement(writer, 0, NULL,
	                            &subject,
	                            &lv2_appliesTo,
	                            plugin_uri, NULL, NULL);

	// <subject> rdfs:label label
	if (state->label) {
		p = serd_node_from_string(SERD_URI, USTR(LILV_NS_RDFS "label"));
		o = serd_node_from_string(SERD_LITERAL, USTR(state->label));
		serd_writer_write_statement(writer, 0,
		                            NULL, &subject, &p, &o, NULL, NULL);
	}

	SerdEnv*        env  = serd_writer_get_env(writer);
	const SerdNode* base = serd_env_get_base_uri(env, NULL);

	Sratom* sratom = sratom_new(map);
	sratom_set_sink(sratom, (const char*)base->buf,
	                (SerdStatementSink)serd_writer_write_statement,
	                (SerdEndSink)serd_writer_end_anon,
	                writer);

	// Write port values as pretty numbers
	sratom_set_pretty_numbers(sratom, true);

	// Write port values
	for (uint32_t i = 0; i < state->num_values; ++i) {
		PortValue* const value = &state->values[i];

		const SerdNode port = serd_node_from_string(
			SERD_BLANK, USTR(value->symbol));

		// <> lv2:port _:symbol
		p = serd_node_from_string(SERD_URI, USTR(LV2_CORE__port));
		serd_writer_write_statement(writer, SERD_ANON_O_BEGIN,
		                            NULL, &subject, &p, &port, NULL, NULL);

		// _:symbol lv2:symbol "symbol"
		p = serd_node_from_string(SERD_URI, USTR(LV2_CORE__symbol));
		o = serd_node_from_string(SERD_LITERAL, USTR(value->symbol));
		serd_writer_write_statement(writer, SERD_ANON_CONT,
		                            NULL, &port, &p, &o, NULL, NULL);

		// _:symbol pset:value value
		p = serd_node_from_string(SERD_URI, USTR(LV2_PRESETS__value));
		sratom_write(sratom, unmap, SERD_ANON_CONT, &port, &p,
		             value->type, value->size, value->value);

		serd_writer_end_anon(writer, &port);
	}

	// Write property values with precise types
	sratom_set_pretty_numbers(sratom, false);

	// Write properties
	const SerdNode state_node = serd_node_from_string(SERD_BLANK,
	                                                  USTR("2state"));
	if (state->num_props > 0) {
		p = serd_node_from_string(SERD_URI, USTR(LV2_STATE__state));
		serd_writer_write_statement(writer, SERD_ANON_O_BEGIN, NULL,
		                            &subject, &p, &state_node, NULL, NULL);
	}
	for (uint32_t i = 0; i < state->num_props; ++i) {
		Property*   prop = &state->props[i];
		const char* key  = unmap->unmap(unmap->handle, prop->key);

		p = serd_node_from_string(SERD_URI, USTR(key));
		if (prop->type == state->atom_Path && !dir) {
			const char* path     = (const char*)prop->value;
			const char* abs_path = lilv_state_rel2abs(state, path);
			sratom_write(sratom, unmap, SERD_ANON_CONT,
			             &state_node, &p, prop->type,
			             strlen(abs_path) + 1, abs_path);
		} else {
			sratom_write(sratom, unmap, SERD_ANON_CONT,
			             &state_node, &p, prop->type, prop->size, prop->value);
		}
	}
	if (state->num_props > 0) {
		serd_writer_end_anon(writer, &state_node);
	}

	sratom_free(sratom);
	return 0;
}

static void
lilv_state_make_links(const LilvState* state, const char* dir)
{
	// Create symlinks to files
	for (ZixTreeIter* i = zix_tree_begin(state->abs2rel);
	     i != zix_tree_end(state->abs2rel);
	     i = zix_tree_iter_next(i)) {
		const PathMap* pm = (const PathMap*)zix_tree_get(i);

		char* path = lilv_path_join(dir, pm->rel);
		if (lilv_path_is_child(pm->abs, state->copy_dir)
		    && strcmp(state->copy_dir, dir)) {
			// Link directly to snapshot in the copy directory
			char* target = lilv_path_relative_to(pm->abs, dir);
			lilv_symlink(target, path);
			free(target);
		} else if (!lilv_path_is_child(pm->abs, dir)) {
			const char* link_dir = state->link_dir ? state->link_dir : dir;
			char*       pat      = lilv_path_join(link_dir, pm->rel);
			if (!strcmp(dir, link_dir)) {
				// Link directory is save directory, make link at exact path
				remove(pat);
				lilv_symlink(pm->abs, pat);
			} else {
				// Make a link in the link directory to external file
				char* lpath = lilv_find_free_path(pat, link_exists, pm->abs);
				if (!lilv_path_exists(lpath, NULL)) {
					lilv_symlink(pm->abs, lpath);
				}

				// Make a link in the save directory to the external link
				char* target = lilv_path_relative_to(lpath, dir);
				lilv_symlink(target, path);
				free(target);
				free(lpath);
			}
			free(pat);
		}
		free(path);
	}
}

LILV_API
int
lilv_state_save(LilvWorld*       world,
                LV2_URID_Map*    map,
                LV2_URID_Unmap*  unmap,
                const LilvState* state,
                const char*      uri,
                const char*      dir,
                const char*      filename)
{
	if (!filename || !dir || lilv_mkdir_p(dir)) {
		return 1;
	}

	char*       abs_dir = absolute_dir(dir);
	char* const path    = lilv_path_join(abs_dir, filename);
	FILE*       fd      = fopen(path, "w");
	if (!fd) {
		LILV_ERRORF("Failed to open %s (%s)\n", path, strerror(errno));
		free(abs_dir);
		free(path);
		return 4;
	}

	// FIXME: make parameter non-const?
	if (state->dir && strcmp(state->dir, abs_dir)) {
		free(state->dir);
		((LilvState*)state)->dir = lilv_strdup(abs_dir);
	}

	// Create symlinks to files if necessary
	lilv_state_make_links(state, abs_dir);

	// Write state to Turtle file
	SerdNode    file   = serd_node_new_file_uri(USTR(path), NULL, NULL, false);
	SerdEnv*    env    = NULL;
	SerdWriter* writer = ttl_file_writer(fd, &file, &env);

	SerdNode node = uri ? serd_node_from_string(SERD_URI, USTR(uri)) : file;
	int ret       = lilv_state_write(
		world, map, unmap, state, writer, (const char*)node.buf, dir);

	serd_node_free(&file);
	serd_writer_free(writer);
	serd_env_free(env);
	fclose(fd);

	char* const manifest = lilv_path_join(abs_dir, "manifest.ttl");
	add_state_to_manifest(state->plugin_uri, manifest, uri, path);

	free(manifest);
	free(abs_dir);
	free(path);
	return ret;
}

LILV_API
char*
lilv_state_to_string(LilvWorld*       world,
                     LV2_URID_Map*    map,
                     LV2_URID_Unmap*  unmap,
                     const LilvState* state,
                     const char*      uri,
                     const char*      base_uri)
{
	if (!uri) {
		LILV_ERROR("Attempt to serialise state with no URI\n");
		return NULL;
	}

	SerdChunk   chunk  = { NULL, 0 };
	SerdEnv*    env    = NULL;
	SerdNode    base   = serd_node_from_string(SERD_URI, USTR(base_uri));
	SerdWriter* writer = ttl_writer(serd_chunk_sink, &chunk, &base, &env);

	lilv_state_write(world, map, unmap, state, writer, uri, NULL);

	serd_writer_free(writer);
	serd_env_free(env);
	return (char*)serd_chunk_sink_finish(&chunk);
}

LILV_API
void
lilv_state_free(LilvState* state)
{
	if (state) {
		for (uint32_t i = 0; i < state->num_props; ++i) {
			free(state->props[i].value);
		}
		for (uint32_t i = 0; i < state->num_values; ++i) {
			free(state->values[i].value);
			free(state->values[i].symbol);
		}
		lilv_node_free(state->plugin_uri);
		zix_tree_free(state->abs2rel);
		zix_tree_free(state->rel2abs);
		free(state->props);
		free(state->values);
		free(state->label);
		free(state->dir);
		free(state->file_dir);
		free(state->copy_dir);
		free(state->link_dir);
		free(state);
	}
}

LILV_API
bool
lilv_state_equals(const LilvState* a, const LilvState* b)
{
	if (!lilv_node_equals(a->plugin_uri, b->plugin_uri)
	    || (a->label && !b->label)
	    || (b->label && !a->label)
	    || (a->label && b->label && strcmp(a->label, b->label))
	    || a->num_props != b->num_props
	    || a->num_values != b->num_values) {
		return false;
	}

	for (uint32_t i = 0; i < a->num_values; ++i) {
		PortValue* const av = &a->values[i];
		PortValue* const bv = &b->values[i];
		if (av->size != bv->size || av->type != bv->type
		    || strcmp(av->symbol, bv->symbol)
		    || memcmp(av->value, bv->value, av->size)) {
			return false;
		}
	}

	for (uint32_t i = 0; i < a->num_props; ++i) {
		Property* const ap = &a->props[i];
		Property* const bp = &b->props[i];
		if (ap->key != bp->key
		    || ap->type != bp->type
		    || ap->flags != bp->flags) {
			return false;
		} else if (ap->type == a->atom_Path) {
			if (!lilv_file_equals(lilv_state_rel2abs(a, (char*)ap->value),
			                      lilv_state_rel2abs(b, (char*)bp->value))) {
				return false;
			}
		} else if (ap->size != bp->size
		           || memcmp(ap->value, bp->value, ap->size)) {
			return false;
		}
	}

	return true;
}

LILV_API
unsigned
lilv_state_get_num_properties(const LilvState* state)
{
	return state->num_props;
}

LILV_API
const LilvNode*
lilv_state_get_plugin_uri(const LilvState* state)
{
	return state->plugin_uri;
}

LILV_API
const char*
lilv_state_get_label(const LilvState* state)
{
	return state->label;
}

LILV_API
void
lilv_state_set_label(LilvState* state, const char* label)
{
	const size_t len = strlen(label);
	state->label = (char*)realloc(state->label, len + 1);
	memcpy(state->label, label, len + 1);
}
