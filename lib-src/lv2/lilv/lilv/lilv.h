/*
  Copyright 2007-2019 David Robillard <http://drobilla.net>

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

/**
   @file lilv.h API for Lilv, a lightweight LV2 host library.
*/

#ifndef LILV_LILV_H
#define LILV_LILV_H

#include "lv2/core/lv2.h"
#include "lv2/urid/urid.h"

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#ifdef LILV_SHARED
#    ifdef _WIN32
#        define LILV_LIB_IMPORT __declspec(dllimport)
#        define LILV_LIB_EXPORT __declspec(dllexport)
#    else
#        define LILV_LIB_IMPORT __attribute__((visibility("default")))
#        define LILV_LIB_EXPORT __attribute__((visibility("default")))
#    endif
#    ifdef LILV_INTERNAL
#        define LILV_API LILV_LIB_EXPORT
#    else
#        define LILV_API LILV_LIB_IMPORT
#    endif
#else
#    define LILV_API
#endif
#if defined(__GNUC__) && (__GNUC__ > 3 || \
                          (__GNUC__ == 3 && __GNUC_MINOR__ >= 1))
#    define LILV_DEPRECATED __attribute__((__deprecated__))
#else
#    define LILV_DEPRECATED
#endif

#ifdef __cplusplus
extern "C" {
#endif

#define LILV_NS_DOAP "http://usefulinc.com/ns/doap#"
#define LILV_NS_FOAF "http://xmlns.com/foaf/0.1/"
#define LILV_NS_LILV "http://drobilla.net/ns/lilv#"
#define LILV_NS_LV2  "http://lv2plug.in/ns/lv2core#"
#define LILV_NS_OWL  "http://www.w3.org/2002/07/owl#"
#define LILV_NS_RDF  "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define LILV_NS_RDFS "http://www.w3.org/2000/01/rdf-schema#"
#define LILV_NS_XSD  "http://www.w3.org/2001/XMLSchema#"

#define LILV_URI_ATOM_PORT    "http://lv2plug.in/ns/ext/atom#AtomPort"
#define LILV_URI_AUDIO_PORT   "http://lv2plug.in/ns/lv2core#AudioPort"
#define LILV_URI_CONTROL_PORT "http://lv2plug.in/ns/lv2core#ControlPort"
#define LILV_URI_CV_PORT      "http://lv2plug.in/ns/lv2core#CVPort"
#define LILV_URI_EVENT_PORT   "http://lv2plug.in/ns/ext/event#EventPort"
#define LILV_URI_INPUT_PORT   "http://lv2plug.in/ns/lv2core#InputPort"
#define LILV_URI_MIDI_EVENT   "http://lv2plug.in/ns/ext/midi#MidiEvent"
#define LILV_URI_OUTPUT_PORT  "http://lv2plug.in/ns/lv2core#OutputPort"
#define LILV_URI_PORT         "http://lv2plug.in/ns/lv2core#Port"

typedef struct LilvPluginImpl      LilvPlugin;       /**< LV2 Plugin. */
typedef struct LilvPluginClassImpl LilvPluginClass;  /**< Plugin Class. */
typedef struct LilvPortImpl        LilvPort;         /**< Port. */
typedef struct LilvScalePointImpl  LilvScalePoint;   /**< Scale Point. */
typedef struct LilvUIImpl          LilvUI;           /**< Plugin UI. */
typedef struct LilvNodeImpl        LilvNode;         /**< Typed Value. */
typedef struct LilvWorldImpl       LilvWorld;        /**< Lilv World. */
typedef struct LilvInstanceImpl    LilvInstance;     /**< Plugin instance. */
typedef struct LilvStateImpl       LilvState;        /**< Plugin state. */

typedef void LilvIter;           /**< Collection iterator */
typedef void LilvPluginClasses;  /**< set<PluginClass>. */
typedef void LilvPlugins;        /**< set<Plugin>. */
typedef void LilvScalePoints;    /**< set<ScalePoint>. */
typedef void LilvUIs;            /**< set<UI>. */
typedef void LilvNodes;          /**< set<Node>. */

/**
   @defgroup lilv Lilv
   Lilv is a simple yet powerful C API for using LV2 plugins.

   For more information about LV2, see <http://lv2plug.in>.
   For more information about Lilv, see <http://drobilla.net/software/lilv>.
   @{
*/

/**
   Free memory allocated by Lilv.

   This function exists because some systems require memory allocated by a
   library to be freed by code in the same library.  It is otherwise equivalent
   to the standard C free() function.
*/
LILV_API void
lilv_free(void* ptr);

/**
   @name Node
   @{
*/

/**
   Convert a file URI string to a local path string.
   For example, "file://foo/bar/baz.ttl" returns "/foo/bar/baz.ttl".
   Return value is shared and must not be deleted by caller.
   This function does not handle escaping correctly and should not be used for
   general file URIs.  Use lilv_file_uri_parse() instead.
   @return `uri` converted to a path, or NULL on failure (URI is not local).
*/
LILV_API LILV_DEPRECATED const char*
lilv_uri_to_path(const char* uri);

/**
   Convert a file URI string to a local path string.
   For example, "file://foo/bar%20one/baz.ttl" returns "/foo/bar one/baz.ttl".
   Return value must be freed by caller with lilv_free().
   @param uri The file URI to parse.
   @param hostname If non-NULL, set to the hostname in the URI, if any.
   @return `uri` converted to a path, or NULL on failure (URI is not local).
*/
LILV_API char*
lilv_file_uri_parse(const char* uri, char** hostname);

/**
   Create a new URI value.
   Returned value must be freed by caller with lilv_node_free().
*/
LILV_API LilvNode*
lilv_new_uri(LilvWorld* world, const char* uri);

/**
   Create a new file URI value.
   @param world The world.
   @param host Host name, or NULL.
   @param path Path on host.
   @return A new node that must be freed by caller.

   Relative paths are resolved against the current working directory.  Note
   that this may yield unexpected results if `host` is another machine.
*/
LILV_API LilvNode*
lilv_new_file_uri(LilvWorld* world, const char* host, const char* path);

/**
   Create a new string value (with no language).
   Returned value must be freed by caller with lilv_node_free().
*/
LILV_API LilvNode*
lilv_new_string(LilvWorld* world, const char* str);

/**
   Create a new integer value.
   Returned value must be freed by caller with lilv_node_free().
*/
LILV_API LilvNode*
lilv_new_int(LilvWorld* world, int val);

/**
   Create a new floating point value.
   Returned value must be freed by caller with lilv_node_free().
*/
LILV_API LilvNode*
lilv_new_float(LilvWorld* world, float val);

/**
   Create a new boolean value.
   Returned value must be freed by caller with lilv_node_free().
*/
LILV_API LilvNode*
lilv_new_bool(LilvWorld* world, bool val);

/**
   Free a LilvNode.
   It is safe to call this function on NULL.
*/
LILV_API void
lilv_node_free(LilvNode* val);

/**
   Duplicate a LilvNode.
*/
LILV_API LilvNode*
lilv_node_duplicate(const LilvNode* val);

/**
   Return whether two values are equivalent.
*/
LILV_API bool
lilv_node_equals(const LilvNode* value, const LilvNode* other);

/**
   Return this value as a Turtle/SPARQL token.
   Returned value must be freed by caller with lilv_free().
   <table>
   <caption>Example Turtle Tokens</caption>
   <tr><th>URI</th><td>&lt;http://example.org/foo &gt;</td></tr>
   <tr><th>QName</th><td>doap:name</td></tr>
   <tr><th>String</th><td>"this is a string"</td></tr>
   <tr><th>Float</th><td>1.0</td></tr>
   <tr><th>Integer</th><td>1</td></tr>
   <tr><th>Boolean</th><td>true</td></tr>
   </table>
*/
LILV_API char*
lilv_node_get_turtle_token(const LilvNode* value);

/**
   Return whether the value is a URI (resource).
*/
LILV_API bool
lilv_node_is_uri(const LilvNode* value);

/**
   Return this value as a URI string, e.g. "http://example.org/foo".
   Valid to call only if `lilv_node_is_uri(value)` returns true.
   Returned value is owned by `value` and must not be freed by caller.
*/
LILV_API const char*
lilv_node_as_uri(const LilvNode* value);

/**
   Return whether the value is a blank node (resource with no URI).
*/
LILV_API bool
lilv_node_is_blank(const LilvNode* value);

/**
   Return this value as a blank node identifier, e.g. "genid03".
   Valid to call only if `lilv_node_is_blank(value)` returns true.
   Returned value is owned by `value` and must not be freed by caller.
*/
LILV_API const char*
lilv_node_as_blank(const LilvNode* value);

/**
   Return whether this value is a literal (i.e. not a URI).
   Returns true if `value` is a string or numeric value.
*/
LILV_API bool
lilv_node_is_literal(const LilvNode* value);

/**
   Return whether this value is a string literal.
   Returns true if `value` is a string value (and not numeric).
*/
LILV_API bool
lilv_node_is_string(const LilvNode* value);

/**
   Return `value` as a string.
*/
LILV_API const char*
lilv_node_as_string(const LilvNode* value);

/**
   Return the path of a file URI node.
   Returns NULL if `value` is not a file URI.
   Returned value must be freed by caller with lilv_free().
*/
LILV_API char*
lilv_node_get_path(const LilvNode* value, char** hostname);

/**
   Return whether this value is a decimal literal.
*/
LILV_API bool
lilv_node_is_float(const LilvNode* value);

/**
   Return `value` as a float.
   Valid to call only if `lilv_node_is_float(value)` or
   `lilv_node_is_int(value)` returns true.
*/
LILV_API float
lilv_node_as_float(const LilvNode* value);

/**
   Return whether this value is an integer literal.
*/
LILV_API bool
lilv_node_is_int(const LilvNode* value);

/**
   Return `value` as an integer.
   Valid to call only if `lilv_node_is_int(value)` returns true.
*/
LILV_API int
lilv_node_as_int(const LilvNode* value);

/**
   Return whether this value is a boolean.
*/
LILV_API bool
lilv_node_is_bool(const LilvNode* value);

/**
   Return `value` as a bool.
   Valid to call only if `lilv_node_is_bool(value)` returns true.
*/
LILV_API bool
lilv_node_as_bool(const LilvNode* value);

/**
   @}
   @name Collections
   Lilv has several collection types for holding various types of value:
   <ul>
   <li>LilvPlugins (function prefix "lilv_plugins_")</li>
   <li>LilvPluginClasses (function prefix "lilv_plugin_classes_")</li>
   <li>LilvScalePoints (function prefix "lilv_scale_points_")</li>
   <li>LilvNodes (function prefix "lilv_nodes_")</li>
   <li>LilvUIs (function prefix "lilv_uis_")</li>
   </ul>

   Each collection type supports a similar basic API (except LilvPlugins which
   is internal and thus lacks a free function):
   <ul>
   <li>void PREFIX_free (coll)</li>
   <li>unsigned PREFIX_size (coll)</li>
   <li>LilvIter* PREFIX_begin (coll)</li>
   </ul>
   @{
*/

/* Collections */

/**
   Iterate over each element of a collection.
   @code
   LILV_FOREACH(plugin_classes, i, classes) {
      LilvPluginClass c = lilv_plugin_classes_get(classes, i);
      // ...
   }
   @endcode
*/
#define LILV_FOREACH(colltype, iter, collection) \
	for (LilvIter* iter = lilv_ ## colltype ## _begin(collection); \
	     !lilv_ ## colltype ## _is_end(collection, iter); \
	     (iter) = lilv_ ## colltype ## _next(collection, iter))

/* LilvPluginClasses */

LILV_API void
lilv_plugin_classes_free(LilvPluginClasses* collection);

LILV_API unsigned
lilv_plugin_classes_size(const LilvPluginClasses* collection);

LILV_API LilvIter*
lilv_plugin_classes_begin(const LilvPluginClasses* collection);

LILV_API const LilvPluginClass*
lilv_plugin_classes_get(const LilvPluginClasses* collection, LilvIter* i);

LILV_API LilvIter*
lilv_plugin_classes_next(const LilvPluginClasses* collection, LilvIter* i);

LILV_API bool
lilv_plugin_classes_is_end(const LilvPluginClasses* collection, LilvIter* i);

/**
   Get a plugin class from `classes` by URI.
   Return value is shared (stored in `classes`) and must not be freed or
   modified by the caller in any way.
   @return NULL if no plugin class with `uri` is found in `classes`.
*/
LILV_API const LilvPluginClass*
lilv_plugin_classes_get_by_uri(const LilvPluginClasses* classes,
                               const LilvNode*          uri);

/* ScalePoints */

LILV_API void
lilv_scale_points_free(LilvScalePoints* collection);

LILV_API unsigned
lilv_scale_points_size(const LilvScalePoints* collection);

LILV_API LilvIter*
lilv_scale_points_begin(const LilvScalePoints* collection);

LILV_API const LilvScalePoint*
lilv_scale_points_get(const LilvScalePoints* collection, LilvIter* i);

LILV_API LilvIter*
lilv_scale_points_next(const LilvScalePoints* collection, LilvIter* i);

LILV_API bool
lilv_scale_points_is_end(const LilvScalePoints* collection, LilvIter* i);

/* UIs */

LILV_API void
lilv_uis_free(LilvUIs* collection);

LILV_API unsigned
lilv_uis_size(const LilvUIs* collection);

LILV_API LilvIter*
lilv_uis_begin(const LilvUIs* collection);

LILV_API const LilvUI*
lilv_uis_get(const LilvUIs* collection, LilvIter* i);

LILV_API LilvIter*
lilv_uis_next(const LilvUIs* collection, LilvIter* i);

LILV_API bool
lilv_uis_is_end(const LilvUIs* collection, LilvIter* i);

/**
   Get a UI from `uis` by URI.
   Return value is shared (stored in `uis`) and must not be freed or
   modified by the caller in any way.
   @return NULL if no UI with `uri` is found in `list`.
*/
LILV_API const LilvUI*
lilv_uis_get_by_uri(const LilvUIs*  uis,
                    const LilvNode* uri);

/* Nodes */

LILV_API void
lilv_nodes_free(LilvNodes* collection);

LILV_API unsigned
lilv_nodes_size(const LilvNodes* collection);

LILV_API LilvIter*
lilv_nodes_begin(const LilvNodes* collection);

LILV_API const LilvNode*
lilv_nodes_get(const LilvNodes* collection, LilvIter* i);

LILV_API LilvIter*
lilv_nodes_next(const LilvNodes* collection, LilvIter* i);

LILV_API bool
lilv_nodes_is_end(const LilvNodes* collection, LilvIter* i);

LILV_API LilvNode*
lilv_nodes_get_first(const LilvNodes* collection);

/**
   Return whether `values` contains `value`.
*/
LILV_API bool
lilv_nodes_contains(const LilvNodes* nodes, const LilvNode* value);

/**
   Return a new LilvNodes that contains all nodes from both `a` and `b`.
*/
LILV_API LilvNodes*
lilv_nodes_merge(const LilvNodes* a, const LilvNodes* b);

/* Plugins */

LILV_API unsigned
lilv_plugins_size(const LilvPlugins* collection);

LILV_API LilvIter*
lilv_plugins_begin(const LilvPlugins* collection);

LILV_API const LilvPlugin*
lilv_plugins_get(const LilvPlugins* collection, LilvIter* i);

LILV_API LilvIter*
lilv_plugins_next(const LilvPlugins* collection, LilvIter* i);

LILV_API bool
lilv_plugins_is_end(const LilvPlugins* collection, LilvIter* i);

/**
   Get a plugin from `plugins` by URI.
   Return value is shared (stored in `plugins`) and must not be freed or
   modified by the caller in any way.
   @return NULL if no plugin with `uri` is found in `plugins`.
*/
LILV_API const LilvPlugin*
lilv_plugins_get_by_uri(const LilvPlugins* plugins,
                        const LilvNode*    uri);

/**
   @}
   @name World
   The "world" represents all Lilv state, and is used to discover/load/cache
   LV2 data (plugins, UIs, and extensions).
   Normal hosts which just need to load plugins by URI should simply use
   lilv_world_load_all() to discover/load the system's LV2 resources.
   @{
*/

/**
   Initialize a new, empty world.
   If initialization fails, NULL is returned.
*/
LILV_API LilvWorld*
lilv_world_new(void);

/**
   Enable/disable language filtering.
   Language filtering applies to any functions that return (a) value(s).
   With filtering enabled, Lilv will automatically return the best value(s)
   for the current LANG.  With filtering disabled, all matching values will
   be returned regardless of language tag.  Filtering is enabled by default.
*/
#define LILV_OPTION_FILTER_LANG "http://drobilla.net/ns/lilv#filter-lang"

/**
   Enable/disable dynamic manifest support.
   Dynamic manifest data will only be loaded if this option is true.
*/
#define LILV_OPTION_DYN_MANIFEST "http://drobilla.net/ns/lilv#dyn-manifest"

/**
   Set application-specific LV2_PATH.  This overrides the LV2_PATH from the
   environment, so that lilv will only look inside the given path.  This can be
   used to make self-contained applications.
*/
#define LILV_OPTION_LV2_PATH "http://drobilla.net/ns/lilv#lv2-path"

/**
   Set an option option for `world`.

   Currently recognized options:
   @ref LILV_OPTION_FILTER_LANG
   @ref LILV_OPTION_DYN_MANIFEST
   @ref LILV_OPTION_LV2_PATH
*/
LILV_API void
lilv_world_set_option(LilvWorld*      world,
                      const char*     uri,
                      const LilvNode* value);

/**
   Destroy the world, mwahaha.
   It is safe to call this function on NULL.
   Note that destroying `world` will destroy all the objects it contains
   (e.g. instances of LilvPlugin).  Do not destroy the world until you are
   finished with all objects that came from it.
*/
LILV_API void
lilv_world_free(LilvWorld* world);

/**
   Load all installed LV2 bundles on the system.
   This is the recommended way for hosts to load LV2 data.  It implements the
   established/standard best practice for discovering all LV2 data on the
   system.  The environment variable LV2_PATH may be used to control where
   this function will look for bundles.

   Hosts should use this function rather than explicitly load bundles, except
   in special circumstances (e.g. development utilities, or hosts that ship
   with special plugin bundles which are installed to a known location).
*/
LILV_API void
lilv_world_load_all(LilvWorld* world);

/**
   Load a specific bundle.
   `bundle_uri` must be a fully qualified URI to the bundle directory,
   with the trailing slash, eg. file:///usr/lib/lv2/foo.lv2/

   Normal hosts should not need this function (use lilv_world_load_all()).

   Hosts MUST NOT attach any long-term significance to bundle paths
   (e.g. in save files), since there are no guarantees they will remain
   unchanged between (or even during) program invocations. Plugins (among
   other things) MUST be identified by URIs (not paths) in save files.
*/
LILV_API void
lilv_world_load_bundle(LilvWorld*      world,
                       const LilvNode* bundle_uri);

/**
   Load all specifications from currently loaded bundles.

   This is for hosts that explicitly load specific bundles, its use is not
   necessary when using lilv_world_load_all().  This function parses the
   specifications and adds them to the model.
*/
LILV_API void
lilv_world_load_specifications(LilvWorld* world);

/**
   Load all plugin classes from currently loaded specifications.

   Must be called after lilv_world_load_specifications().  This is for hosts
   that explicitly load specific bundles, its use is not necessary when using
   lilv_world_load_all().
*/
LILV_API void
lilv_world_load_plugin_classes(LilvWorld* world);

/**
   Unload a specific bundle.

   This unloads statements loaded by lilv_world_load_bundle().  Note that this
   is not necessarily all information loaded from the bundle.  If any resources
   have been separately loaded with lilv_world_load_resource(), they must be
   separately unloaded with lilv_world_unload_resource().
*/
LILV_API int
lilv_world_unload_bundle(LilvWorld* world, const LilvNode* bundle_uri);

/**
   Load all the data associated with the given `resource`.
   @param world The world.
   @param resource Must be a subject (i.e. a URI or a blank node).
   @return The number of files parsed, or -1 on error

   All accessible data files linked to `resource` with rdfs:seeAlso will be
   loaded into the world model.
*/
LILV_API int
lilv_world_load_resource(LilvWorld*      world,
                         const LilvNode* resource);

/**
   Unload all the data associated with the given `resource`.
   @param world The world.
   @param resource Must be a subject (i.e. a URI or a blank node).

   This unloads all data loaded by a previous call to
   lilv_world_load_resource() with the given `resource`.
*/
LILV_API int
lilv_world_unload_resource(LilvWorld*      world,
                           const LilvNode* resource);

/**
   Get the parent of all other plugin classes, lv2:Plugin.
*/
LILV_API const LilvPluginClass*
lilv_world_get_plugin_class(const LilvWorld* world);

/**
   Return a list of all found plugin classes.
   Returned list is owned by world and must not be freed by the caller.
*/
LILV_API const LilvPluginClasses*
lilv_world_get_plugin_classes(const LilvWorld* world);

/**
   Return a list of all found plugins.
   The returned list contains just enough references to query
   or instantiate plugins.  The data for a particular plugin will not be
   loaded into memory until a call to an lilv_plugin_* function results in
   a query (at which time the data is cached with the LilvPlugin so future
   queries are very fast).

   The returned list and the plugins it contains are owned by `world`
   and must not be freed by caller.
*/
LILV_API const LilvPlugins*
lilv_world_get_all_plugins(const LilvWorld* world);

/**
   Find nodes matching a triple pattern.
   Either `subject` or `object` may be NULL (i.e. a wildcard), but not both.
   @return All matches for the wildcard field, or NULL.
*/
LILV_API LilvNodes*
lilv_world_find_nodes(LilvWorld*      world,
                      const LilvNode* subject,
                      const LilvNode* predicate,
                      const LilvNode* object);

/**
   Find a single node that matches a pattern.
   Exactly one of `subject`, `predicate`, `object` must be NULL.
   This function is equivalent to
   lilv_nodes_get_first(lilv_world_find_nodes(...)) but simplifies the common
   case of only wanting a single value.
   @return the first matching node, or NULL if no matches are found.
*/
LILV_API LilvNode*
lilv_world_get(LilvWorld*      world,
               const LilvNode* subject,
               const LilvNode* predicate,
               const LilvNode* object);

/**
   Return true iff a statement matching a certain pattern exists.

   This is useful for checking if particular statement exists without having to
   bother with collections and memory management.

   @param world The world.
   @param subject Subject of statement, or NULL for anything.
   @param predicate Predicate (key) of statement, or NULL for anything.
   @param object Object (value) of statement, or NULL for anything.
*/
LILV_API bool
lilv_world_ask(LilvWorld*      world,
               const LilvNode* subject,
               const LilvNode* predicate,
               const LilvNode* object);

/**
   Get an LV2 symbol for some subject.

   This will return the lv2:symbol property of the subject if it is given
   explicitly, and otherwise will attempt to derive a symbol from the URI.
   @return A string node that is a valid LV2 symbol, or NULL on error.
*/
LILV_API LilvNode*
lilv_world_get_symbol(LilvWorld* world, const LilvNode* subject);

/**
   @}
   @name Plugin
   @{
*/

/**
   Check if `plugin` is valid.
   This is not a rigorous validator, but can be used to reject some malformed
   plugins that could cause bugs (e.g. plugins with missing required fields).

   Note that normal hosts do NOT need to use this - lilv does not
   load invalid plugins into plugin lists.  This is included for plugin
   testing utilities, etc.
   @return true iff `plugin` is valid.
*/
LILV_API bool
lilv_plugin_verify(const LilvPlugin* plugin);

/**
   Get the URI of `plugin`.
   Any serialization that refers to plugins should refer to them by this.
   Hosts SHOULD NOT save any filesystem paths, plugin indexes, etc. in saved
   files; save only the URI.

   The URI is a globally unique identifier for one specific plugin.  Two
   plugins with the same URI are compatible in port signature, and should
   be guaranteed to work in a compatible and consistent way.  If a plugin
   is upgraded in an incompatible way (eg if it has different ports), it
   MUST have a different URI than it's predecessor.

   @return A shared URI value which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_plugin_get_uri(const LilvPlugin* plugin);

/**
   Get the (resolvable) URI of the plugin's "main" bundle.
   This returns the URI of the bundle where the plugin itself was found.  Note
   that the data for a plugin may be spread over many bundles, that is,
   lilv_plugin_get_data_uris() may return URIs which are not within this
   bundle.

   Typical hosts should not need to use this function.
   Note this always returns a fully qualified URI.  If you want a local
   filesystem path, use lilv_file_uri_parse().
   @return a shared string which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_plugin_get_bundle_uri(const LilvPlugin* plugin);

/**
   Get the (resolvable) URIs of the RDF data files that define a plugin.
   Typical hosts should not need to use this function.
   Note this always returns fully qualified URIs.  If you want local
   filesystem paths, use lilv_file_uri_parse().
   @return a list of complete URLs eg. "file:///foo/ABundle.lv2/aplug.ttl",
   which is shared and must not be modified or freed.
*/
LILV_API const LilvNodes*
lilv_plugin_get_data_uris(const LilvPlugin* plugin);

/**
   Get the (resolvable) URI of the shared library for `plugin`.
   Note this always returns a fully qualified URI.  If you want a local
   filesystem path, use lilv_file_uri_parse().
   @return a shared string which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_plugin_get_library_uri(const LilvPlugin* plugin);

/**
   Get the name of `plugin`.
   This returns the name (doap:name) of the plugin.  The name may be
   translated according to the current locale, this value MUST NOT be used
   as a plugin identifier (use the URI for that).
   Returned value must be freed by the caller.
*/
LILV_API LilvNode*
lilv_plugin_get_name(const LilvPlugin* plugin);

/**
   Get the class this plugin belongs to (e.g. Filters).
*/
LILV_API const LilvPluginClass*
lilv_plugin_get_class(const LilvPlugin* plugin);

/**
   Get a value associated with the plugin in a plugin's data files.
   `predicate` must be either a URI or a QName.

   Returns the ?object of all triples found of the form:

   <code>&lt;plugin-uri&gt; predicate ?object</code>

   May return NULL if the property was not found, or if object(s) is not
   sensibly represented as a LilvNodes (e.g. blank nodes).
   Return value must be freed by caller with lilv_nodes_free().
*/
LILV_API LilvNodes*
lilv_plugin_get_value(const LilvPlugin* plugin,
                      const LilvNode*   predicate);

/**
   Return whether a feature is supported by a plugin.
   This will return true if the feature is an optional or required feature
   of the plugin.
*/
LILV_API bool
lilv_plugin_has_feature(const LilvPlugin* plugin,
                        const LilvNode*   feature);

/**
   Get the LV2 Features supported (required or optionally) by a plugin.
   A feature is "supported" by a plugin if it is required OR optional.

   Since required features have special rules the host must obey, this function
   probably shouldn't be used by normal hosts.  Using lilv_plugin_get_optional_features()
   and lilv_plugin_get_required_features() separately is best in most cases.

   Returned value must be freed by caller with lilv_nodes_free().
*/
LILV_API LilvNodes*
lilv_plugin_get_supported_features(const LilvPlugin* plugin);

/**
   Get the LV2 Features required by a plugin.
   If a feature is required by a plugin, hosts MUST NOT use the plugin if they do not
   understand (or are unable to support) that feature.

   All values returned here MUST be passed to the plugin's instantiate method
   (along with data, if necessary, as defined by the feature specification)
   or plugin instantiation will fail.

   Return value must be freed by caller with lilv_nodes_free().
*/
LILV_API LilvNodes*
lilv_plugin_get_required_features(const LilvPlugin* plugin);

/**
   Get the LV2 Features optionally supported by a plugin.
   Hosts MAY ignore optional plugin features for whatever reasons.  Plugins
   MUST operate (at least somewhat) if they are instantiated without being
   passed optional features.

   Return value must be freed by caller with lilv_nodes_free().
*/
LILV_API LilvNodes*
lilv_plugin_get_optional_features(const LilvPlugin* plugin);

/**
   Return whether or not a plugin provides a specific extension data.
*/
LILV_API bool
lilv_plugin_has_extension_data(const LilvPlugin* plugin,
                               const LilvNode*   uri);

/**
   Get a sequence of all extension data provided by a plugin.
   This can be used to find which URIs lilv_instance_get_extension_data()
   will return a value for without instantiating the plugin.
*/
LILV_API LilvNodes*
lilv_plugin_get_extension_data(const LilvPlugin* plugin);

/**
   Get the number of ports on this plugin.
*/
LILV_API uint32_t
lilv_plugin_get_num_ports(const LilvPlugin* plugin);

/**
   Get the port ranges (minimum, maximum and default values) for all ports.
   `min_values`, `max_values` and `def_values` must either point to an array
   of N floats, where N is the value returned by lilv_plugin_get_num_ports()
   for this plugin, or NULL.  The elements of the array will be set to the
   the minimum, maximum and default values of the ports on this plugin,
   with array index corresponding to port index.  If a port doesn't have a
   minimum, maximum or default value, or the port's type is not float, the
   corresponding array element will be set to NAN.

   This is a convenience method for the common case of getting the range of
   all float ports on a plugin, and may be significantly faster than
   repeated calls to lilv_port_get_range().
*/
LILV_API void
lilv_plugin_get_port_ranges_float(const LilvPlugin* plugin,
                                  float*            min_values,
                                  float*            max_values,
                                  float*            def_values);

/**
   Get the number of ports on this plugin that are members of some class(es).
   Note that this is a varargs function so ports fitting any type 'profile'
   desired can be found quickly.  REMEMBER TO TERMINATE THE PARAMETER LIST
   OF THIS FUNCTION WITH NULL OR VERY NASTY THINGS WILL HAPPEN.
*/
LILV_API uint32_t
lilv_plugin_get_num_ports_of_class(const LilvPlugin* plugin,
                                   const LilvNode*   class_1, ...);

/**
   Variant of lilv_plugin_get_num_ports_of_class() that takes a va_list.

   This function calls va_arg() on `args` but does not call va_end().
*/
LILV_API uint32_t
lilv_plugin_get_num_ports_of_class_va(const LilvPlugin* plugin,
                                      const LilvNode*   class_1,
                                      va_list           args);

/**
   Return whether or not the plugin introduces (and reports) latency.
   The index of the latency port can be found with
   lilv_plugin_get_latency_port() ONLY if this function returns true.
*/
LILV_API bool
lilv_plugin_has_latency(const LilvPlugin* plugin);

/**
   Return the index of the plugin's latency port.
   It is a fatal error to call this on a plugin without checking if the port
   exists by first calling lilv_plugin_has_latency().

   Any plugin that introduces unwanted latency that should be compensated for
   (by hosts with the ability/need) MUST provide this port, which is a control
   rate output port that reports the latency for each cycle in frames.
*/
LILV_API uint32_t
lilv_plugin_get_latency_port_index(const LilvPlugin* plugin);

/**
   Get a port on `plugin` by `index`.
*/
LILV_API const LilvPort*
lilv_plugin_get_port_by_index(const LilvPlugin* plugin,
                              uint32_t          index);

/**
   Get a port on `plugin` by `symbol`.
   Note this function is slower than lilv_plugin_get_port_by_index(),
   especially on plugins with a very large number of ports.
*/
LILV_API const LilvPort*
lilv_plugin_get_port_by_symbol(const LilvPlugin* plugin,
                               const LilvNode*   symbol);

/**
   Get a port on `plugin` by its lv2:designation.

   The designation of a port describes the meaning, assignment, allocation or
   role of the port, e.g. "left channel" or "gain".  If found, the port with
   matching `port_class` and `designation` is be returned, otherwise NULL is
   returned.  The `port_class` can be used to distinguish the input and output
   ports for a particular designation.  If `port_class` is NULL, any port with
   the given designation will be returned.
*/
LILV_API const LilvPort*
lilv_plugin_get_port_by_designation(const LilvPlugin* plugin,
                                    const LilvNode*   port_class,
                                    const LilvNode*   designation);

/**
   Get the project the plugin is a part of.

   More information about the project can be read via lilv_world_find_nodes(),
   typically using properties from DOAP (e.g. doap:name).
*/
LILV_API LilvNode*
lilv_plugin_get_project(const LilvPlugin* plugin);

/**
   Get the full name of the plugin's author.
   Returns NULL if author name is not present.
   Returned value must be freed by caller.
*/
LILV_API LilvNode*
lilv_plugin_get_author_name(const LilvPlugin* plugin);

/**
   Get the email address of the plugin's author.
   Returns NULL if author email address is not present.
   Returned value must be freed by caller.
*/
LILV_API LilvNode*
lilv_plugin_get_author_email(const LilvPlugin* plugin);

/**
   Get the address of the plugin author's home page.
   Returns NULL if author homepage is not present.
   Returned value must be freed by caller.
*/
LILV_API LilvNode*
lilv_plugin_get_author_homepage(const LilvPlugin* plugin);

/**
   Return true iff `plugin` has been replaced by another plugin.

   The plugin will still be usable, but hosts should hide them from their
   user interfaces to prevent users from using deprecated plugins.
*/
LILV_API bool
lilv_plugin_is_replaced(const LilvPlugin* plugin);

/**
   Write the Turtle description of `plugin` to `plugin_file`.

   This function is particularly useful for porting plugins in conjunction with
   an LV2 bridge such as NASPRO.
*/
LILV_API void
lilv_plugin_write_description(LilvWorld*        world,
                              const LilvPlugin* plugin,
                              const LilvNode*   base_uri,
                              FILE*             plugin_file);

/**
   Write a manifest entry for `plugin` to `manifest_file`.

   This function is intended for use with lilv_plugin_write_description() to
   write a complete description of a plugin to a bundle.
*/
LILV_API void
lilv_plugin_write_manifest_entry(LilvWorld*        world,
                                 const LilvPlugin* plugin,
                                 const LilvNode*   base_uri,
                                 FILE*             manifest_file,
                                 const char*       plugin_file_path);

/**
   Get the resources related to `plugin` with lv2:appliesTo.

   Some plugin-related resources are not linked directly to the plugin with
   rdfs:seeAlso and thus will not be automatically loaded along with the plugin
   data (usually for performance reasons).  All such resources of the given @c
   type related to `plugin` can be accessed with this function.

   If `type` is NULL, all such resources will be returned, regardless of type.

   To actually load the data for each returned resource, use
   lilv_world_load_resource().
*/
LILV_API LilvNodes*
lilv_plugin_get_related(const LilvPlugin* plugin, const LilvNode* type);

/**
   @}
   @name Port
   @{
*/

/**
   Get the RDF node of `port`.

   Ports nodes may be may be URIs or blank nodes.

   @return A shared node which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_port_get_node(const LilvPlugin* plugin,
                   const LilvPort*   port);

/**
   Port analog of lilv_plugin_get_value().
*/
LILV_API LilvNodes*
lilv_port_get_value(const LilvPlugin* plugin,
                    const LilvPort*   port,
                    const LilvNode*   predicate);

/**
   Get a single property value of a port.

   This is equivalent to lilv_nodes_get_first(lilv_port_get_value(...)) but is
   simpler to use in the common case of only caring about one value.  The
   caller is responsible for freeing the returned node.
*/
LILV_API LilvNode*
lilv_port_get(const LilvPlugin* plugin,
              const LilvPort*   port,
              const LilvNode*   predicate);

/**
   Return the LV2 port properties of a port.
*/
LILV_API LilvNodes*
lilv_port_get_properties(const LilvPlugin* plugin,
                         const LilvPort*   port);

/**
   Return whether a port has a certain property.
*/
LILV_API bool
lilv_port_has_property(const LilvPlugin* plugin,
                       const LilvPort*   port,
                       const LilvNode*   property);

/**
   Return whether a port supports a certain event type.

   More precisely, this returns true iff the port has an atom:supports or an
   ev:supportsEvent property with `event_type` as the value.
*/
LILV_API bool
lilv_port_supports_event(const LilvPlugin* plugin,
                         const LilvPort*   port,
                         const LilvNode*   event_type);

/**
   Get the index of a port.
   The index is only valid for the life of the plugin and may change between
   versions.  For a stable identifier, use the symbol.
*/
LILV_API uint32_t
lilv_port_get_index(const LilvPlugin* plugin,
                    const LilvPort*   port);

/**
   Get the symbol of a port.
   The 'symbol' is a short string, a valid C identifier.
   Returned value is owned by `port` and must not be freed.
*/
LILV_API const LilvNode*
lilv_port_get_symbol(const LilvPlugin* plugin,
                     const LilvPort*   port);

/**
   Get the name of a port.
   This is guaranteed to return the untranslated name (the doap:name in the
   data file without a language tag).  Returned value must be freed by
   the caller.
*/
LILV_API LilvNode*
lilv_port_get_name(const LilvPlugin* plugin,
                   const LilvPort*   port);

/**
   Get all the classes of a port.
   This can be used to determine if a port is an input, output, audio,
   control, midi, etc, etc, though it's simpler to use lilv_port_is_a().
   The returned list does not include lv2:Port, which is implied.
   Returned value is shared and must not be destroyed by caller.
*/
LILV_API const LilvNodes*
lilv_port_get_classes(const LilvPlugin* plugin,
                      const LilvPort*   port);

/**
   Determine if a port is of a given class (input, output, audio, etc).
   For convenience/performance/extensibility reasons, hosts are expected to
   create a LilvNode for each port class they "care about".  Well-known type
   URI strings are defined (e.g. LILV_URI_INPUT_PORT) for convenience, but
   this function is designed so that Lilv is usable with any port types
   without requiring explicit support in Lilv.
*/
LILV_API bool
lilv_port_is_a(const LilvPlugin* plugin,
               const LilvPort*   port,
               const LilvNode*   port_class);

/**
   Get the default, minimum, and maximum values of a port.

   `def`, `min`, and `max` are outputs, pass pointers to uninitialized
   LilvNode* variables.  These will be set to point at new values (which must
   be freed by the caller using lilv_node_free()), or NULL if the value does
   not exist.
*/
LILV_API void
lilv_port_get_range(const LilvPlugin* plugin,
                    const LilvPort*   port,
                    LilvNode**        def,
                    LilvNode**        min,
                    LilvNode**        max);

/**
   Get the scale points (enumeration values) of a port.
   This returns a collection of 'interesting' named values of a port
   (e.g. appropriate entries for a UI selector associated with this port).
   Returned value may be NULL if `port` has no scale points, otherwise it
   must be freed by caller with lilv_scale_points_free().
*/
LILV_API LilvScalePoints*
lilv_port_get_scale_points(const LilvPlugin* plugin,
                           const LilvPort*   port);

/**
   @}
   @name Plugin State
   @{
*/

/**
   Load a state snapshot from the world RDF model.
   This function can be used to load the default state of a plugin by passing
   the plugin URI as the `subject` parameter.
   @param world The world.
   @param map URID mapper.
   @param node The subject of the state description (e.g. a preset URI).
   @return A new LilvState which must be freed with lilv_state_free(), or NULL.
*/
LILV_API LilvState*
lilv_state_new_from_world(LilvWorld*      world,
                          LV2_URID_Map*   map,
                          const LilvNode* node);

/**
   Load a state snapshot from a file.
   @param world The world.
   @param map URID mapper.
   @param subject The subject of the state description (e.g. a preset URI).
   @param path The path of the file containing the state description.
   @return A new LilvState which must be freed with lilv_state_free().

   If `subject` is NULL, it is taken to be the URI of the file (i.e.
   "<>" in Turtle).

   This function parses the file separately to create the state, it does not
   parse the file into the world model, i.e. the returned state is the only
   new memory consumed once this function returns.
*/
LILV_API LilvState*
lilv_state_new_from_file(LilvWorld*      world,
                         LV2_URID_Map*   map,
                         const LilvNode* subject,
                         const char*     path);

/**
   Load a state snapshot from a string made by lilv_state_to_string().
*/
LILV_API LilvState*
lilv_state_new_from_string(LilvWorld*    world,
                           LV2_URID_Map* map,
                           const char*   str);

/**
   Function to get a port value.
   @param port_symbol The symbol of the port.
   @param user_data The user_data passed to lilv_state_new_from_instance().
   @param size (Output) The size of the returned value.
   @param type (Output) The URID of the type of the returned value.
   @return A pointer to the port value.

   This function MUST set `size` and `type` appropriately.
*/
typedef const void* (*LilvGetPortValueFunc)(const char* port_symbol,
                                            void*       user_data,
                                            uint32_t*   size,
                                            uint32_t*   type);

/**
   Create a new state snapshot from a plugin instance.

   @param plugin The plugin this state applies to.

   @param instance An instance of `plugin`.

   @param map The map to use for mapping URIs in state.

   @param scratch_dir Directory of files created by the plugin earlier, or
   NULL.  This is for hosts that support file creation at any time with state
   state:makePath.  These files will be copied as necessary to `copy_dir` and
   not be referred to directly in state (a temporary directory is appropriate).

   @param copy_dir Directory of copies of files in `scratch_dir`, or NULL.
   This directory will have the same structure as `scratch_dir` but with
   possibly modified file names to distinguish revisions.  This allows the
   saved state to contain the exact contents of the scratch file at save time,
   so that the state is not ruined if the file is later modified (for example,
   by the plugin continuing to record).  This can be the same as `save_dir` to
   create a copy in the state bundle, but can also be a separate directory
   which allows multiple state snapshots to share a single copy if the file has
   not changed.

   @param link_dir Directory of links to external files, or NULL.  A link will
   be made in this directory to any external files referred to in plugin state.
   In turn, links will be created in the save directory to these links (e.g.
   save_dir/file => link_dir/file => /foo/bar/file).  This allows many state
   snapshots to share a single link to an external file, so archival (e.g. with
   tar -h) will not create several copies of the file.  If this is not
   required, it can be the same as `save_dir`.

   @param save_dir Directory of files created by plugin during save (or NULL).
   This is typically the bundle directory later passed to lilv_state_save().

   @param get_value Function to get port values (or NULL).  If NULL, the
   returned state will not represent port values.  This should only be NULL in
   hosts that save and restore port values via some other mechanism.

   @param user_data User data to pass to `get_value`.

   @param flags Bitwise OR of LV2_State_Flags values.

   @param features Features to pass LV2_State_Interface.save().

   @return A new LilvState which must be freed with lilv_state_free().

   This function may be called simultaneously with any instance function
   (except discovery functions) unless the threading class of that function
   explicitly disallows this.

   To support advanced file functionality, there are several directory
   parameters.  Simple hosts that only wish to save a single plugins state once
   may simply use the same directory for all of them (or pass NULL to not
   support files at all).  The multiple parameters are necessary to support
   saving an instances state many times while avoiding any duplication of data.

   If supported (via state:makePath passed to LV2_Descriptor::instantiate()),
   `scratch_dir` should be the directory where any files created by the plugin
   (not during save time, e.g. during instantiation) are stored.  These files
   will be copied to preserve their state at this time.plugin-created files are
   stored.  Lilv will assume any files within this directory (recursively) are
   created by the plugin and all other files are immutable.  Note that this
   function does not save the state, use lilv_state_save() for that.

   See <a href="http://lv2plug.in/ns/ext/state/state.h">state.h</a> from the
   LV2 State extension for details on the `flags` and `features` parameters.
*/
LILV_API LilvState*
lilv_state_new_from_instance(const LilvPlugin*          plugin,
                             LilvInstance*              instance,
                             LV2_URID_Map*              map,
                             const char*                scratch_dir,
                             const char*                copy_dir,
                             const char*                link_dir,
                             const char*                save_dir,
                             LilvGetPortValueFunc       get_value,
                             void*                      user_data,
                             uint32_t                   flags,
                             const LV2_Feature *const * features);

/**
   Free `state`.
*/
LILV_API void
lilv_state_free(LilvState* state);

/**
   Return true iff `a` is equivalent to `b`.
*/
LILV_API bool
lilv_state_equals(const LilvState* a, const LilvState* b);

/**
   Return the number of properties in `state`.
*/
LILV_API unsigned
lilv_state_get_num_properties(const LilvState* state);

/**
   Get the URI of the plugin `state` applies to.
*/
LILV_API const LilvNode*
lilv_state_get_plugin_uri(const LilvState* state);

/**
   Get the URI of `state`.

   This may return NULL if the state has not been saved and has no URI.
*/
LILV_API const LilvNode*
lilv_state_get_uri(const LilvState* state);

/**
   Get the label of `state`.
*/
LILV_API const char*
lilv_state_get_label(const LilvState* state);

/**
   Set the label of `state`.
*/
LILV_API void
lilv_state_set_label(LilvState*  state,
                     const char* label);

/**
   Set a metadata property on `state`.
   @param state The state to set the metadata for.
   @param key The key to store `value` under (URID).
   @param value Pointer to the value to be stored.
   @param size The size of `value` in bytes.
   @param type The type of `value` (URID).
   @param flags LV2_State_Flags for `value`.
   @return 0 on success.

   This is a generic version of lilv_state_set_label(), which sets metadata
   properties visible to hosts, but not plugins.  This allows storing useful
   information such as comments or preset banks.
*/
LILV_API int
lilv_state_set_metadata(LilvState*  state,
                        uint32_t    key,
                        const void* value,
                        size_t      size,
                        uint32_t    type,
                        uint32_t    flags);

/**
   Function to set a port value.
   @param port_symbol The symbol of the port.
   @param user_data The user_data passed to lilv_state_restore().
   @param size The size of `value`.
   @param type The URID of the type of `value`.
   @param value A pointer to the port value.
*/
typedef void (*LilvSetPortValueFunc)(const char* port_symbol,
                                     void*       user_data,
                                     const void* value,
                                     uint32_t    size,
                                     uint32_t    type);

/**
   Enumerate the port values in a state snapshot.
   @param state The state to retrieve port values from.
   @param set_value A function to receive port values.
   @param user_data User data to pass to `set_value`.

   This function is a subset of lilv_state_restore() that only fires the
   `set_value` callback and does not directly affect a plugin instance.  This
   is useful in hosts that need to retrieve the port values in a state snapshot
   for special handling.
*/
LILV_API void
lilv_state_emit_port_values(const LilvState*     state,
                            LilvSetPortValueFunc set_value,
                            void*                user_data);

/**
   Restore a plugin instance from a state snapshot.
   @param state The state to restore, which must apply to the correct plugin.
   @param instance An instance of the plugin `state` applies to, or NULL.
   @param set_value A function to set a port value (may be NULL).
   @param user_data User data to pass to `set_value`.
   @param flags Bitwise OR of LV2_State_Flags values.
   @param features Features to pass LV2_State_Interface.restore().

   This will set all the properties of `instance`, if given, to the values
   stored in `state`.  If `set_value` is provided, it will be called (with the
   given `user_data`) to restore each port value, otherwise the host must
   restore the port values itself (using lilv_state_get_port_value()) in order
   to completely restore `state`.

   If the state has properties and `instance` is given, this function is in
   the "instantiation" threading class, i.e. it MUST NOT be called
   simultaneously with any function on the same plugin instance.  If the state
   has no properties, only port values are set via `set_value`.

   See <a href="http://lv2plug.in/ns/ext/state/state.h">state.h</a> from the
   LV2 State extension for details on the `flags` and `features` parameters.
*/
LILV_API void
lilv_state_restore(const LilvState*           state,
                   LilvInstance*              instance,
                   LilvSetPortValueFunc       set_value,
                   void*                      user_data,
                   uint32_t                   flags,
                   const LV2_Feature *const * features);

/**
   Save state to a file.
   @param world The world.
   @param map URID mapper.
   @param unmap URID unmapper.
   @param state State to save.
   @param uri URI of state, may be NULL.
   @param dir Path of the bundle directory to save into.
   @param filename Path of the state file relative to `dir`.

   The format of state on disk is compatible with that defined in the LV2
   preset extension, i.e. this function may be used to save presets which can
   be loaded by any host.

   If `uri` is NULL, the preset URI will be a file URI, but the bundle
   can safely be moved (i.e. the state file will use "<>" as the subject).
*/
LILV_API int
lilv_state_save(LilvWorld*                 world,
                LV2_URID_Map*              map,
                LV2_URID_Unmap*            unmap,
                const LilvState*           state,
                const char*                uri,
                const char*                dir,
                const char*                filename);

/**
   Save state to a string.  This function does not use the filesystem.

   @param world The world.
   @param map URID mapper.
   @param unmap URID unmapper.
   @param state The state to serialize.
   @param uri URI for the state description (mandatory).
   @param base_uri Base URI for serialisation.  Unless you know what you are
   doing, pass NULL for this, otherwise the state may not be restorable via
   lilv_state_new_from_string().
*/
LILV_API char*
lilv_state_to_string(LilvWorld*       world,
                     LV2_URID_Map*    map,
                     LV2_URID_Unmap*  unmap,
                     const LilvState* state,
                     const char*      uri,
                     const char*      base_uri);

/**
   Unload a state from the world and delete all associated files.
   @param world The world.
   @param state State to remove from the system.

   This function DELETES FILES/DIRECTORIES FROM THE FILESYSTEM!  It is intended
   for removing user-saved presets, but can delete any state the user has
   permission to delete, including presets shipped with plugins.

   The rdfs:seeAlso file for the state will be removed.  The entry in the
   bundle's manifest.ttl is removed, and if this results in an empty manifest,
   then the manifest file is removed.  If this results in an empty bundle, then
   the bundle directory is removed as well.
*/
LILV_API int
lilv_state_delete(LilvWorld*       world,
                  const LilvState* state);

/**
   @}
   @name Scale Point
   @{
*/

/**
   Get the label of this scale point (enumeration value)
   Returned value is owned by `point` and must not be freed.
*/
LILV_API const LilvNode*
lilv_scale_point_get_label(const LilvScalePoint* point);

/**
   Get the value of this scale point (enumeration value)
   Returned value is owned by `point` and must not be freed.
*/
LILV_API const LilvNode*
lilv_scale_point_get_value(const LilvScalePoint* point);

/**
   @}
   @name Plugin Class
   @{
*/

/**
   Get the URI of this class' superclass.
   Returned value is owned by `plugin_class` and must not be freed by caller.
   Returned value may be NULL, if class has no parent.
*/
LILV_API const LilvNode*
lilv_plugin_class_get_parent_uri(const LilvPluginClass* plugin_class);

/**
   Get the URI of this plugin class.
   Returned value is owned by `plugin_class` and must not be freed by caller.
*/
LILV_API const LilvNode*
lilv_plugin_class_get_uri(const LilvPluginClass* plugin_class);

/**
   Get the label of this plugin class, ie "Oscillators".
   Returned value is owned by `plugin_class` and must not be freed by caller.
*/
LILV_API const LilvNode*
lilv_plugin_class_get_label(const LilvPluginClass* plugin_class);

/**
   Get the subclasses of this plugin class.
   Returned value must be freed by caller with lilv_plugin_classes_free().
*/
LILV_API LilvPluginClasses*
lilv_plugin_class_get_children(const LilvPluginClass* plugin_class);

/**
   @}
   @name Plugin Instance
   @{
*/

/**
   @cond LILV_DOCUMENT_INSTANCE_IMPL
*/

/* Instance of a plugin.
   This is exposed in the ABI to allow inlining of performance critical
   functions like lilv_instance_run() (simple wrappers of functions in lv2.h).
   This is for performance reasons, user code should not use this definition
   in any way (which is why it is not machine documented).
   Truly private implementation details are hidden via `pimpl`.
*/
struct LilvInstanceImpl {
	const LV2_Descriptor* lv2_descriptor;
	LV2_Handle            lv2_handle;
	void*                 pimpl;
};

/**
   @endcond
*/

/**
   Instantiate a plugin.
   The returned value is a lightweight handle for an LV2 plugin instance,
   it does not refer to `plugin`, or any other Lilv state.  The caller must
   eventually free it with lilv_instance_free().
   `features` is a NULL-terminated array of features the host supports.
   NULL may be passed if the host supports no additional features.
   @return NULL if instantiation failed.
*/
LILV_API LilvInstance*
lilv_plugin_instantiate(const LilvPlugin*        plugin,
                        double                   sample_rate,
                        const LV2_Feature*const* features);

/**
   Free a plugin instance.
   It is safe to call this function on NULL.
   `instance` is invalid after this call.
*/
LILV_API void
lilv_instance_free(LilvInstance* instance);

#ifndef LILV_INTERNAL

/**
   Get the URI of the plugin which `instance` is an instance of.
   Returned string is shared and must not be modified or deleted.
*/
static inline const char*
lilv_instance_get_uri(const LilvInstance* instance)
{
	return instance->lv2_descriptor->URI;
}

/**
   Connect a port to a data location.
   This may be called regardless of whether the plugin is activated,
   activation and deactivation does not destroy port connections.
*/
static inline void
lilv_instance_connect_port(LilvInstance* instance,
                           uint32_t      port_index,
                           void*         data_location)
{
	instance->lv2_descriptor->connect_port
		(instance->lv2_handle, port_index, data_location);
}

/**
   Activate a plugin instance.
   This resets all state information in the plugin, except for port data
   locations (as set by lilv_instance_connect_port()).  This MUST be called
   before calling lilv_instance_run().
*/
static inline void
lilv_instance_activate(LilvInstance* instance)
{
	if (instance->lv2_descriptor->activate) {
		instance->lv2_descriptor->activate(instance->lv2_handle);
	}
}

/**
   Run `instance` for `sample_count` frames.
   If the hint lv2:hardRTCapable is set for this plugin, this function is
   guaranteed not to block.
*/
static inline void
lilv_instance_run(LilvInstance* instance,
                  uint32_t      sample_count)
{
	instance->lv2_descriptor->run(instance->lv2_handle, sample_count);
}

/**
   Deactivate a plugin instance.
   Note that to run the plugin after this you must activate it, which will
   reset all state information (except port connections).
*/
static inline void
lilv_instance_deactivate(LilvInstance* instance)
{
	if (instance->lv2_descriptor->deactivate) {
		instance->lv2_descriptor->deactivate(instance->lv2_handle);
	}
}

/**
   Get extension data from the plugin instance.
   The type and semantics of the data returned is specific to the particular
   extension, though in all cases it is shared and must not be deleted.
*/
static inline const void*
lilv_instance_get_extension_data(const LilvInstance* instance,
                                 const char*         uri)
{
	if (instance->lv2_descriptor->extension_data) {
		return instance->lv2_descriptor->extension_data(uri);
	} else {
		return NULL;
	}
}

/**
   Get the LV2_Descriptor of the plugin instance.
   Normally hosts should not need to access the LV2_Descriptor directly,
   use the lilv_instance_* functions.

   The returned descriptor is shared and must not be deleted.
*/
static inline const LV2_Descriptor*
lilv_instance_get_descriptor(const LilvInstance* instance)
{
	return instance->lv2_descriptor;
}

/**
   Get the LV2_Handle of the plugin instance.
   Normally hosts should not need to access the LV2_Handle directly,
   use the lilv_instance_* functions.

   The returned handle is shared and must not be deleted.
*/
static inline LV2_Handle
lilv_instance_get_handle(const LilvInstance* instance)
{
	return instance->lv2_handle;
}

#endif /* LILV_INTERNAL */

/**
   @}
   @name Plugin UI
   @{
*/

/**
   Get all UIs for `plugin`.
   Returned value must be freed by caller using lilv_uis_free().
*/
LILV_API LilvUIs*
lilv_plugin_get_uis(const LilvPlugin* plugin);

/**
   Get the URI of a Plugin UI.
   @param ui The Plugin UI
   @return a shared value which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_ui_get_uri(const LilvUI* ui);

/**
   Get the types (URIs of RDF classes) of a Plugin UI.
   @param ui The Plugin UI
   @return a shared value which must not be modified or freed.

   Note that in most cases lilv_ui_is_supported() should be used, which avoids
   the need to use this function (and type specific logic).
*/
LILV_API const LilvNodes*
lilv_ui_get_classes(const LilvUI* ui);

/**
   Check whether a plugin UI has a given type.
   @param ui        The Plugin UI
   @param class_uri The URI of the LV2 UI type to check this UI against
*/
LILV_API bool
lilv_ui_is_a(const LilvUI* ui, const LilvNode* class_uri);

/**
   Function to determine whether a UI type is supported.

   This is provided by the user and must return non-zero iff using a UI of type
   `ui_type_uri` in a container of type `container_type_uri` is supported.
*/
typedef unsigned (*LilvUISupportedFunc)(const char* container_type_uri,
                                        const char* ui_type_uri);

/**
   Return true iff a Plugin UI is supported as a given widget type.
   @param ui The Plugin UI
   @param supported_func User provided supported predicate.
   @param container_type The widget type to host the UI within.
   @param ui_type (Output) If non-NULL, set to the native type of the UI
   which is owned by `ui` and must not be freed by the caller.
   @return The embedding quality level returned by `supported_func`.
*/
LILV_API unsigned
lilv_ui_is_supported(const LilvUI*       ui,
                     LilvUISupportedFunc supported_func,
                     const LilvNode*     container_type,
                     const LilvNode**    ui_type);

/**
   Get the URI for a Plugin UI's bundle.
   @param ui The Plugin UI
   @return a shared value which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_ui_get_bundle_uri(const LilvUI* ui);

/**
   Get the URI for a Plugin UI's shared library.
   @param ui The Plugin UI
   @return a shared value which must not be modified or freed.
*/
LILV_API const LilvNode*
lilv_ui_get_binary_uri(const LilvUI* ui);

/**
   @}
   @}
*/

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LILV_LILV_H */
