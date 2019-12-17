/*
  Copyright 2007-2019 David Robillard <http://drobilla.net>
  Copyright 2008 Krzysztof Foltman

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

#define _POSIX_C_SOURCE 200809L /* for setenv */
#define _XOPEN_SOURCE   600     /* for mkstemp */

#include "../src/lilv_internal.h"

#ifdef _WIN32
#    include <direct.h>
#    include <io.h>
#    define mkdir(path, flags) _mkdir(path)
#    define setenv(n, v, r) SetEnvironmentVariable((n), (v))
#    define unsetenv(n) SetEnvironmentVariable((n), NULL)
#    define mkstemp(pat) _mktemp(pat)
#else
#    include <unistd.h>
#endif

#include "lilv/lilv.h"
#include "lv2/core/lv2.h"
#include "lv2/presets/presets.h"
#include "lv2/state/state.h"
#include "lv2/urid/urid.h"
#include "serd/serd.h"

#include <assert.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define TEST_PATH_MAX 1024

#if defined(__APPLE__)
#    define SHLIB_EXT ".dylib"
#elif defined(_WIN32)
#    define SHLIB_EXT ".dll"
#else
#    define SHLIB_EXT ".so"
#endif

static char test_bundle_path[TEST_PATH_MAX + sizeof("/.lv2/lilv-test.lv2")];
static char test_bundle_uri[sizeof(test_bundle_path) + sizeof("file:///")];
static char test_manifest_path[sizeof(test_bundle_path) + sizeof("/manifest.ttl")];
static char test_content_path[sizeof(test_bundle_path) + sizeof("plugin.ttl")];

static LilvWorld* world;

int test_count  = 0;
int error_count = 0;

static void
delete_bundle(void)
{
	unlink(test_content_path);
	unlink(test_manifest_path);
	remove(test_bundle_path);
}

static void
init_tests(void)
{
	char* test_path = lilv_realpath(LILV_TEST_DIR);

	snprintf(test_bundle_path, sizeof(test_bundle_path),
	         "%s/test_lv2_path/lilv-test.lv2", test_path);
	lilv_mkdir_p(test_bundle_path);

	SerdNode s = serd_node_new_file_uri(
		(const uint8_t*)test_bundle_path, NULL, NULL, true);

	snprintf(test_bundle_uri, sizeof(test_bundle_uri), "%s/",
	         (const char*)s.buf);
	snprintf(test_manifest_path, sizeof(test_manifest_path), "%s/manifest.ttl",
	         test_bundle_path);
	snprintf(test_content_path, sizeof(test_content_path), "%s/plugin.ttl",
	         test_bundle_path);

	serd_node_free(&s);
	lilv_free(test_path);

	delete_bundle();
}

static void
fatal_error(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(1);
}

static void
write_file(const char* name, const char* content)
{
	FILE* f = fopen(name, "w");
	size_t len = strlen(content);
	if (fwrite(content, 1, len, f) != len) {
		fatal_error("Failed to write to file '%s' (%s)\n",
		            name, strerror(errno));
	}
	fclose(f);
}

static int
init_world(void)
{
	world = lilv_world_new();

	// Set custom LV2_PATH in build directory to only use test data
	char*     test_path = lilv_realpath(LILV_TEST_DIR);
	char*     lv2_path  = lilv_strjoin(test_path, "/test_lv2_path", NULL);
	LilvNode* path      = lilv_new_string(world, lv2_path);
	lilv_world_set_option(world, LILV_OPTION_LV2_PATH, path);
	free(lv2_path);
	free(test_path);
	lilv_node_free(path);

	return world != NULL;
}

static int
load_all_bundles(void)
{
	if (!init_world()) {
		return 0;
	}
	lilv_world_load_all(world);
	return 1;
}

static void
create_bundle(const char* manifest, const char* content)
{
	if (mkdir(test_bundle_path, 0700) && errno != EEXIST) {
		fatal_error("Failed to create directory '%s' (%s)\n",
		            test_bundle_path, strerror(errno));
	}
	write_file(test_manifest_path, manifest);
	write_file(test_content_path, content);
}

static int
start_bundle(const char* manifest, const char* content)
{
	create_bundle(manifest, content);
	return load_all_bundles();
}

static void
unload_bundle(void)
{
	if (world) {
		lilv_world_free(world);
	}
	world = NULL;
}

static void
cleanup(void)
{
	delete_bundle();
}

static void
set_env(const char* name, const char* value)
{
#ifdef _WIN32
	// setenv on Windows does not modify the current process' environment
	const size_t len = strlen(name) + 1 + strlen(value) + 1;
	char*        str = (char*)calloc(1, len);
	snprintf(str, len, "%s=%s", name, value);
	putenv(str);
	free(str);
#else
	setenv(name, value, 1);
#endif
}

/*****************************************************************************/

#define TEST_CASE(name) { #name, test_##name }
#define TEST_ASSERT(check) do {\
	test_count++;\
	if (!(check)) {\
		error_count++;\
		fprintf(stderr, "lilv_test.c:%d: error: test `%s' failed\n", __LINE__, #check);\
		abort();\
	}\
} while (0)

typedef int (*TestFunc)(void);

struct TestCase {
	const char* title;
	TestFunc func;
};

#define PREFIX_ATOM "@prefix atom: <http://lv2plug.in/ns/ext/atom#> . \n"
#define PREFIX_LINE "@prefix : <http://example.org/> .\n"
#define PREFIX_LV2 "@prefix lv2: <http://lv2plug.in/ns/lv2core#> .\n"
#define PREFIX_LV2EV "@prefix lv2ev: <http://lv2plug.in/ns/ext/event#> . \n"
#define PREFIX_LV2UI "@prefix lv2ui: <http://lv2plug.in/ns/extensions/ui#> .\n"
#define PREFIX_RDF "@prefix rdf:  <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
#define PREFIX_RDFS "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n"
#define PREFIX_FOAF "@prefix foaf: <http://xmlns.com/foaf/0.1/> .\n"
#define PREFIX_DOAP "@prefix doap: <http://usefulinc.com/ns/doap#> .\n"
#define PREFIX_PSET "@prefix pset: <http://lv2plug.in/ns/ext/presets#> .\n"

#define MANIFEST_PREFIXES PREFIX_LINE PREFIX_LV2 PREFIX_RDFS
#define BUNDLE_PREFIXES PREFIX_ATOM PREFIX_LINE PREFIX_LV2 PREFIX_RDF PREFIX_RDFS PREFIX_FOAF PREFIX_DOAP PREFIX_PSET
#define PLUGIN_NAME(name) "doap:name \"" name "\""
#define LICENSE_GPL "doap:license <http://usefulinc.com/doap/licenses/gpl>"

static const char* uris_plugin = "http://example.org/plug";
static LilvNode* plugin_uri_value;
static LilvNode* plugin2_uri_value;

/*****************************************************************************/

static void
init_uris(void)
{
	plugin_uri_value = lilv_new_uri(world, uris_plugin);
	plugin2_uri_value = lilv_new_uri(world, "http://example.org/foobar");
	TEST_ASSERT(plugin_uri_value);
	TEST_ASSERT(plugin2_uri_value);
}

static void
cleanup_uris(void)
{
	lilv_node_free(plugin2_uri_value);
	lilv_node_free(plugin_uri_value);
	plugin2_uri_value = NULL;
	plugin_uri_value = NULL;
}

/*****************************************************************************/

static int
test_value(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"Foo\" ; "
			"] .")) {
		return 0;
	}

	init_uris();

	LilvNode* uval = lilv_new_uri(world, "http://example.org");
	LilvNode* sval = lilv_new_string(world, "Foo");
	LilvNode* ival = lilv_new_int(world, 42);
	LilvNode* fval = lilv_new_float(world, 1.6180);

	TEST_ASSERT(lilv_node_is_uri(uval));
	TEST_ASSERT(lilv_node_is_string(sval));
	TEST_ASSERT(lilv_node_is_int(ival));
	TEST_ASSERT(lilv_node_is_float(fval));

	TEST_ASSERT(!lilv_node_is_literal(NULL));
	TEST_ASSERT(!lilv_node_is_literal(uval));
	TEST_ASSERT(lilv_node_is_literal(sval));
	TEST_ASSERT(lilv_node_is_literal(ival));
	TEST_ASSERT(lilv_node_is_literal(fval));
	TEST_ASSERT(!lilv_node_get_path(fval, NULL));

	TEST_ASSERT(!strcmp(lilv_node_as_uri(uval), "http://example.org"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(sval), "Foo"));
	TEST_ASSERT(lilv_node_as_int(ival) == 42);
	TEST_ASSERT(fabs(lilv_node_as_float(fval) - 1.6180) < FLT_EPSILON);
	TEST_ASSERT(isnan(lilv_node_as_float(sval)));

#if defined(__clang__)
#    pragma clang diagnostic push
#    pragma clang diagnostic ignored "-Wdeprecated-declarations"
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic push
#    pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif

	TEST_ASSERT(!strcmp(lilv_uri_to_path("file:///foo"), "/foo"));

#if defined(__clang__)
#    pragma clang diagnostic pop
#elif __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 6)
#    pragma GCC diagnostic pop
#endif

	LilvNode* loc_abs  = lilv_new_file_uri(world, NULL, "/foo/bar");
	LilvNode* loc_rel  = lilv_new_file_uri(world, NULL, "foo");
	LilvNode* host_abs = lilv_new_file_uri(world, "host", "/foo/bar");
	LilvNode* host_rel = lilv_new_file_uri(world, "host", "foo");

	TEST_ASSERT(!strcmp(lilv_node_as_uri(loc_abs), "file:///foo/bar"));
	TEST_ASSERT(!strncmp(lilv_node_as_uri(loc_rel), "file:///", 8));
	TEST_ASSERT(!strcmp(lilv_node_as_uri(host_abs), "file://host/foo/bar"));
	TEST_ASSERT(!strncmp(lilv_node_as_uri(host_rel), "file://host/", 12));

	lilv_node_free(host_rel);
	lilv_node_free(host_abs);
	lilv_node_free(loc_rel);
	lilv_node_free(loc_abs);

	char* tok = lilv_node_get_turtle_token(uval);
	TEST_ASSERT(!strcmp(tok, "<http://example.org>"));
	lilv_free(tok);
	tok = lilv_node_get_turtle_token(sval);
	TEST_ASSERT(!strcmp(tok, "Foo"));
	lilv_free(tok);
	tok = lilv_node_get_turtle_token(ival);
	TEST_ASSERT(!strcmp(tok, "42"));
	lilv_free(tok);
	tok = lilv_node_get_turtle_token(fval);
	TEST_ASSERT(!strncmp(tok, "1.6180", 6));
	lilv_free(tok);

	LilvNode* uval_e = lilv_new_uri(world, "http://example.org");
	LilvNode* sval_e = lilv_new_string(world, "Foo");
	LilvNode* ival_e = lilv_new_int(world, 42);
	LilvNode* fval_e = lilv_new_float(world, 1.6180);
	LilvNode* uval_ne = lilv_new_uri(world, "http://no-example.org");
	LilvNode* sval_ne = lilv_new_string(world, "Bar");
	LilvNode* ival_ne = lilv_new_int(world, 24);
	LilvNode* fval_ne = lilv_new_float(world, 3.14159);

	TEST_ASSERT(lilv_node_equals(uval, uval_e));
	TEST_ASSERT(lilv_node_equals(sval, sval_e));
	TEST_ASSERT(lilv_node_equals(ival, ival_e));
	TEST_ASSERT(lilv_node_equals(fval, fval_e));

	TEST_ASSERT(!lilv_node_equals(uval, uval_ne));
	TEST_ASSERT(!lilv_node_equals(sval, sval_ne));
	TEST_ASSERT(!lilv_node_equals(ival, ival_ne));
	TEST_ASSERT(!lilv_node_equals(fval, fval_ne));

	TEST_ASSERT(!lilv_node_equals(uval, sval));
	TEST_ASSERT(!lilv_node_equals(sval, ival));
	TEST_ASSERT(!lilv_node_equals(ival, fval));

	LilvNode* uval_dup = lilv_node_duplicate(uval);
	TEST_ASSERT(lilv_node_equals(uval, uval_dup));

	LilvNode* ifval = lilv_new_float(world, 42.0);
	TEST_ASSERT(!lilv_node_equals(ival, ifval));
	lilv_node_free(ifval);

	LilvNode* nil = NULL;
	TEST_ASSERT(!lilv_node_equals(uval, nil));
	TEST_ASSERT(!lilv_node_equals(nil, uval));
	TEST_ASSERT(lilv_node_equals(nil, nil));

	LilvNode* nil2 = lilv_node_duplicate(nil);
	TEST_ASSERT(lilv_node_equals(nil, nil2));

	lilv_node_free(uval);
	lilv_node_free(sval);
	lilv_node_free(ival);
	lilv_node_free(fval);
	lilv_node_free(uval_e);
	lilv_node_free(sval_e);
	lilv_node_free(ival_e);
	lilv_node_free(fval_e);
	lilv_node_free(uval_ne);
	lilv_node_free(sval_ne);
	lilv_node_free(ival_ne);
	lilv_node_free(fval_ne);
	lilv_node_free(uval_dup);
	lilv_node_free(nil2);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_util(void)
{
	TEST_ASSERT(!lilv_realpath(NULL));

	char a_path[16];
	char b_path[16];
	strncpy(a_path, "copy_a_XXXXXX", sizeof(a_path));
	strncpy(b_path, "copy_b_XXXXXX",  sizeof(b_path));
	mkstemp(a_path);
	mkstemp(b_path);

	FILE* fa = fopen(a_path, "w");
	FILE* fb = fopen(b_path, "w");
	fprintf(fa, "AA\n");
	fprintf(fb, "AB\n");
	fclose(fa);
	fclose(fb);

	TEST_ASSERT(lilv_copy_file("does/not/exist", "copy"));
	TEST_ASSERT(lilv_copy_file(a_path, "not/a/dir/copy"));
	TEST_ASSERT(!lilv_copy_file(a_path, "copy_c"));
	TEST_ASSERT(!lilv_file_equals(a_path, b_path));
	TEST_ASSERT(lilv_file_equals(a_path, a_path));
	TEST_ASSERT(lilv_file_equals(a_path, "copy_c"));
	TEST_ASSERT(!lilv_file_equals("does/not/exist", b_path));
	TEST_ASSERT(!lilv_file_equals(a_path, "does/not/exist"));
	TEST_ASSERT(!lilv_file_equals("does/not/exist", "/does/not/either"));
	return 1;
}

/*****************************************************************************/

static int discovery_plugin_found = 0;

static void
discovery_verify_plugin(const LilvPlugin* plugin)
{
	const LilvNode* value = lilv_plugin_get_uri(plugin);
	if (lilv_node_equals(value, plugin_uri_value)) {
		const LilvNode* lib_uri = NULL;
		TEST_ASSERT(!lilv_node_equals(value, plugin2_uri_value));
		discovery_plugin_found = 1;
		lib_uri = lilv_plugin_get_library_uri(plugin);
		TEST_ASSERT(lib_uri);
		TEST_ASSERT(lilv_node_is_uri(lib_uri));
		TEST_ASSERT(lilv_node_as_uri(lib_uri));
		TEST_ASSERT(strstr(lilv_node_as_uri(lib_uri), "foo" SHLIB_EXT));
		TEST_ASSERT(lilv_plugin_verify(plugin));
	}
}

static int
test_discovery(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ;"
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:port [ a lv2:ControlPort ; a lv2:InputPort ;"
			" lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; ] .")) {
		return 0;
	}

	init_uris();

	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	TEST_ASSERT(lilv_plugins_size(plugins) > 0);

	const LilvPlugin* explug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(explug != NULL);
	const LilvPlugin* explug2 = lilv_plugins_get_by_uri(plugins, plugin2_uri_value);
	TEST_ASSERT(explug2 == NULL);

	if (explug) {
		LilvNode* name = lilv_plugin_get_name(explug);
		TEST_ASSERT(!strcmp(lilv_node_as_string(name), "Test plugin"));
		lilv_node_free(name);
	}

	discovery_plugin_found = 0;
	LILV_FOREACH(plugins, i, plugins)
		discovery_verify_plugin(lilv_plugins_get(plugins, i));

	TEST_ASSERT(discovery_plugin_found);
	plugins = NULL;

	cleanup_uris();

	return 1;
}

/*****************************************************************************/

static int
test_verify(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:port [ a lv2:ControlPort ; a lv2:InputPort ;"
			" lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ] .")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* explug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(explug);
	TEST_ASSERT(lilv_plugin_verify(explug));
	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_no_verify(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin . ")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* explug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(explug);
	TEST_ASSERT(!lilv_plugin_verify(explug));
	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_classes(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"Foo\" ; "
			"] .")) {
		return 0;
	}

	init_uris();
	const LilvPluginClass*   plugin   = lilv_world_get_plugin_class(world);
	const LilvPluginClasses* classes  = lilv_world_get_plugin_classes(world);
	LilvPluginClasses*       children = lilv_plugin_class_get_children(plugin);

	TEST_ASSERT(lilv_plugin_class_get_parent_uri(plugin) == NULL);
	TEST_ASSERT(lilv_plugin_classes_size(classes) > lilv_plugin_classes_size(children));
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_plugin_class_get_label(plugin)), "Plugin"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_plugin_class_get_uri(plugin)),
	                    "http://lv2plug.in/ns/lv2core#Plugin"));

	LILV_FOREACH(plugin_classes, i, children) {
		TEST_ASSERT(lilv_node_equals(
				lilv_plugin_class_get_parent_uri(lilv_plugin_classes_get(children, i)),
				lilv_plugin_class_get_uri(plugin)));
	}

	LilvNode* some_uri = lilv_new_uri(world, "http://example.org/whatever");
	TEST_ASSERT(lilv_plugin_classes_get_by_uri(classes, some_uri) == NULL);
	lilv_node_free(some_uri);

	lilv_plugin_classes_free(children);

	lilv_plugin_class_free(NULL);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_plugin(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:optionalFeature lv2:hardRTCapable ; "
			"lv2:requiredFeature <http://lv2plug.in/ns/ext/event> ; "
			"lv2:extensionData <http://example.org/extdata> ;"
			":foo 1.6180 ; "
			":bar true ; "
			":baz false ; "
			":blank [ a <http://example.org/blank> ] ; "
			"doap:maintainer [ foaf:name \"David Robillard\" ; "
			"  foaf:homepage <http://drobilla.net> ; foaf:mbox <mailto:d@drobilla.net> ] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			":thing doap:name \"Something else\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* plug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	const LilvPluginClass* klass = lilv_plugin_get_class(plug);
	const LilvNode* klass_uri = lilv_plugin_class_get_uri(klass);
	TEST_ASSERT(!strcmp(lilv_node_as_string(klass_uri),
			"http://lv2plug.in/ns/lv2core#CompressorPlugin"));

	LilvNode* rdf_type = lilv_new_uri(
		world, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type");
	TEST_ASSERT(lilv_world_ask(world,
	                           lilv_plugin_get_uri(plug),
	                           rdf_type,
	                           klass_uri));
	lilv_node_free(rdf_type);

	TEST_ASSERT(!lilv_plugin_is_replaced(plug));
	TEST_ASSERT(!lilv_plugin_get_related(plug, NULL));

	const LilvNode* plug_bundle_uri = lilv_plugin_get_bundle_uri(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(plug_bundle_uri), test_bundle_uri));

	const LilvNodes* data_uris = lilv_plugin_get_data_uris(plug);
	TEST_ASSERT(lilv_nodes_size(data_uris) == 2);

	LilvNode* project = lilv_plugin_get_project(plug);
	TEST_ASSERT(!project);

	char* manifest_uri = (char*)malloc(TEST_PATH_MAX);
	char* data_uri     = (char*)malloc(TEST_PATH_MAX);
	snprintf(manifest_uri, TEST_PATH_MAX, "%s%s",
			lilv_node_as_string(plug_bundle_uri), "manifest.ttl");
	snprintf(data_uri, TEST_PATH_MAX, "%s%s",
			lilv_node_as_string(plug_bundle_uri), "plugin.ttl");

	LilvNode* manifest_uri_val = lilv_new_uri(world, manifest_uri);
	TEST_ASSERT(lilv_nodes_contains(data_uris, manifest_uri_val));
	lilv_node_free(manifest_uri_val);

	LilvNode* data_uri_val = lilv_new_uri(world, data_uri);
	TEST_ASSERT(lilv_nodes_contains(data_uris, data_uri_val));
	lilv_node_free(data_uri_val);

	LilvNode* unknown_uri_val = lilv_new_uri(world, "http://example.org/unknown");
	TEST_ASSERT(!lilv_nodes_contains(data_uris, unknown_uri_val));
	lilv_node_free(unknown_uri_val);

	free(manifest_uri);
	free(data_uri);

	float mins[3];
	float maxs[3];
	float defs[3];
	lilv_plugin_get_port_ranges_float(plug, mins, maxs, defs);
	TEST_ASSERT(mins[0] == -1.0f);
	TEST_ASSERT(maxs[0] == 1.0f);
	TEST_ASSERT(defs[0] == 0.5f);

	LilvNode* audio_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#AudioPort");
	LilvNode* control_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#ControlPort");
	LilvNode* in_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#InputPort");
	LilvNode* out_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#OutputPort");

	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, control_class, NULL) == 3);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, audio_class, NULL) == 0);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, in_class, NULL) == 2);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, out_class, NULL) == 1);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, control_class, in_class, NULL) == 2);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, control_class, out_class, NULL) == 1);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, audio_class, in_class, NULL) == 0);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, audio_class, out_class, NULL) == 0);

	TEST_ASSERT(lilv_plugin_has_latency(plug));
	TEST_ASSERT(lilv_plugin_get_latency_port_index(plug) == 2);

	LilvNode* lv2_latency = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#latency");
	const LilvPort* latency_port = lilv_plugin_get_port_by_designation(
		plug, out_class, lv2_latency);
	lilv_node_free(lv2_latency);

	TEST_ASSERT(latency_port);
	TEST_ASSERT(lilv_port_get_index(plug, latency_port) == 2);
	TEST_ASSERT(lilv_node_is_blank(lilv_port_get_node(plug, latency_port)));

	LilvNode* rt_feature = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#hardRTCapable");
	LilvNode* event_feature = lilv_new_uri(world,
			"http://lv2plug.in/ns/ext/event");
	LilvNode* pretend_feature = lilv_new_uri(world,
			"http://example.org/solvesWorldHunger");

	TEST_ASSERT(lilv_plugin_has_feature(plug, rt_feature));
	TEST_ASSERT(lilv_plugin_has_feature(plug, event_feature));
	TEST_ASSERT(!lilv_plugin_has_feature(plug, pretend_feature));

	lilv_node_free(rt_feature);
	lilv_node_free(event_feature);
	lilv_node_free(pretend_feature);

	LilvNodes* supported = lilv_plugin_get_supported_features(plug);
	LilvNodes* required = lilv_plugin_get_required_features(plug);
	LilvNodes* optional = lilv_plugin_get_optional_features(plug);
	TEST_ASSERT(lilv_nodes_size(supported) == 2);
	TEST_ASSERT(lilv_nodes_size(required) == 1);
	TEST_ASSERT(lilv_nodes_size(optional) == 1);
	lilv_nodes_free(supported);
	lilv_nodes_free(required);
	lilv_nodes_free(optional);

	LilvNode*  foo_p = lilv_new_uri(world, "http://example.org/foo");
	LilvNodes* foos  = lilv_plugin_get_value(plug, foo_p);
	TEST_ASSERT(lilv_nodes_size(foos) == 1);
	TEST_ASSERT(fabs(lilv_node_as_float(lilv_nodes_get_first(foos)) - 1.6180) < FLT_EPSILON);
	lilv_node_free(foo_p);
	lilv_nodes_free(foos);

	LilvNode*  bar_p = lilv_new_uri(world, "http://example.org/bar");
	LilvNodes* bars  = lilv_plugin_get_value(plug, bar_p);
	TEST_ASSERT(lilv_nodes_size(bars) == 1);
	TEST_ASSERT(lilv_node_as_bool(lilv_nodes_get_first(bars)) == true);
	lilv_node_free(bar_p);
	lilv_nodes_free(bars);

	LilvNode*  baz_p = lilv_new_uri(world, "http://example.org/baz");
	LilvNodes* bazs  = lilv_plugin_get_value(plug, baz_p);
	TEST_ASSERT(lilv_nodes_size(bazs) == 1);
	TEST_ASSERT(lilv_node_as_bool(lilv_nodes_get_first(bazs)) == false);
	lilv_node_free(baz_p);
	lilv_nodes_free(bazs);

	LilvNode*  blank_p = lilv_new_uri(world, "http://example.org/blank");
	LilvNodes* blanks  = lilv_plugin_get_value(plug, blank_p);
	TEST_ASSERT(lilv_nodes_size(blanks) == 1);
	LilvNode*  blank = lilv_nodes_get_first(blanks);
	TEST_ASSERT(lilv_node_is_blank(blank));
	const char* blank_str = lilv_node_as_blank(blank);
	char*       blank_tok = lilv_node_get_turtle_token(blank);
	TEST_ASSERT(!strncmp(blank_tok, "_:", 2));
	TEST_ASSERT(!strcmp(blank_tok + 2, blank_str));
	lilv_free(blank_tok);
	lilv_node_free(blank_p);
	lilv_nodes_free(blanks);

	LilvNode* author_name = lilv_plugin_get_author_name(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_name), "David Robillard"));
	lilv_node_free(author_name);

	LilvNode* author_email = lilv_plugin_get_author_email(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_email), "mailto:d@drobilla.net"));
	lilv_node_free(author_email);

	LilvNode* author_homepage = lilv_plugin_get_author_homepage(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_homepage), "http://drobilla.net"));
	lilv_node_free(author_homepage);

	LilvNode* thing_uri = lilv_new_uri(world, "http://example.org/thing");
	LilvNode* name_p = lilv_new_uri(world, "http://usefulinc.com/ns/doap#name");
	LilvNodes* thing_names = lilv_world_find_nodes(world, thing_uri, name_p, NULL);
	TEST_ASSERT(lilv_nodes_size(thing_names) == 1);
	LilvNode* thing_name = lilv_nodes_get_first(thing_names);
	TEST_ASSERT(thing_name);
	TEST_ASSERT(lilv_node_is_string(thing_name));
	TEST_ASSERT(!strcmp(lilv_node_as_string(thing_name), "Something else"));
	LilvNode* thing_name2 = lilv_world_get(world, thing_uri, name_p, NULL);
	TEST_ASSERT(lilv_node_equals(thing_name, thing_name2));

	LilvUIs* uis = lilv_plugin_get_uis(plug);
	TEST_ASSERT(lilv_uis_size(uis) == 0);
	lilv_uis_free(uis);

	LilvNode* extdata = lilv_new_uri(world, "http://example.org/extdata");
	LilvNode* noextdata = lilv_new_uri(world, "http://example.org/noextdata");
	LilvNodes* extdatas = lilv_plugin_get_extension_data(plug);
	TEST_ASSERT(lilv_plugin_has_extension_data(plug, extdata));
	TEST_ASSERT(!lilv_plugin_has_extension_data(plug, noextdata));
	TEST_ASSERT(lilv_nodes_size(extdatas) == 1);
	TEST_ASSERT(lilv_node_equals(lilv_nodes_get_first(extdatas), extdata));
	lilv_node_free(noextdata);
	lilv_node_free(extdata);
	lilv_nodes_free(extdatas);

	lilv_nodes_free(thing_names);
	lilv_node_free(thing_uri);
	lilv_node_free(thing_name2);
	lilv_node_free(name_p);
	lilv_node_free(control_class);
	lilv_node_free(audio_class);
	lilv_node_free(in_class);
	lilv_node_free(out_class);
	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_project(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin with project") " ; "
			LICENSE_GPL " ; "
			"lv2:project [ "
			"  doap:maintainer [ "
			"    foaf:name \"David Robillard\" ; "
			"    foaf:homepage <http://drobilla.net> ; foaf:mbox <mailto:d@drobilla.net> ] ; "
			"] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			":thing doap:name \"Something else\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* plug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvNode* author_name = lilv_plugin_get_author_name(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_name), "David Robillard"));
	lilv_node_free(author_name);

	LilvNode* author_email = lilv_plugin_get_author_email(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_email), "mailto:d@drobilla.net"));
	lilv_node_free(author_email);

	LilvNode* author_homepage = lilv_plugin_get_author_homepage(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(author_homepage), "http://drobilla.net"));
	lilv_node_free(author_homepage);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_no_author(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin with project") " ; "
			LICENSE_GPL " ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			":thing doap:name \"Something else\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* plug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvNode* author_name = lilv_plugin_get_author_name(plug);
	TEST_ASSERT(!author_name);

	LilvNode* author_email = lilv_plugin_get_author_email(plug);
	TEST_ASSERT(!author_email);

	LilvNode* author_homepage = lilv_plugin_get_author_homepage(plug);
	TEST_ASSERT(!author_homepage);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_project_no_author(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin with project") " ; "
			LICENSE_GPL " ; "
			"lv2:project [ "
			"  doap:name \"Fake project\" ;"
			"] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			":thing doap:name \"Something else\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* plug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvNode* author_name = lilv_plugin_get_author_name(plug);
	TEST_ASSERT(!author_name);

	LilvNode* author_email = lilv_plugin_get_author_email(plug);
	TEST_ASSERT(!author_email);

	LilvNode* author_homepage = lilv_plugin_get_author_homepage(plug);
	TEST_ASSERT(!author_homepage);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_preset(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin with project") " ; "
			LICENSE_GPL " ; "
			"lv2:project [ "
			"  doap:name \"Fake project\" ;"
			"] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			"<http://example.org/preset> a pset:Preset ;"
			"  lv2:appliesTo :plug ;"
	                  "  rdfs:label \"some preset\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvNode*  pset_Preset = lilv_new_uri(world, LV2_PRESETS__Preset);
	LilvNodes* related     = lilv_plugin_get_related(plug, pset_Preset);

	TEST_ASSERT(lilv_nodes_size(related) == 1);

	lilv_node_free(pset_Preset);
	lilv_nodes_free(related);
	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_prototype(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":prot a lv2:PluginBase ; rdfs:seeAlso <plugin.ttl> .\n"
			":plug a lv2:Plugin ; lv2:binary <inst" SHLIB_EXT "> ; lv2:prototype :prot .\n",
			BUNDLE_PREFIXES
			":prot a lv2:Plugin ; a lv2:CompressorPlugin ; "
			LICENSE_GPL " ; "
			"lv2:project [ "
			"  doap:name \"Fake project\" ;"
			"] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency ; "
			"  lv2:designation lv2:latency "
			"] . \n"
			":plug doap:name \"Instance\" .\n")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	// Test non-inherited property
	LilvNode* name = lilv_plugin_get_name(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "Instance"));
	lilv_node_free(name);

	// Test inherited property
	const LilvNode* binary = lilv_plugin_get_library_uri(plug);
	TEST_ASSERT(strstr(lilv_node_as_string(binary), "inst" SHLIB_EXT));

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_port(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES PREFIX_LV2EV
			":plug a lv2:Plugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"doap:homepage <http://example.org/someplug> ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; "
			"  lv2:name \"store\" ; "
			"  lv2:name \"Laden\"@de-de ; lv2:name \"Geschaeft\"@de-at ; "
			"  lv2:name \"tienda\"@es ; "
			"  rdfs:comment \"comment\"@en , \"commentaires\"@fr ; "
     		"  lv2:portProperty lv2:integer ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 ; "
			"  lv2:scalePoint [ rdfs:label \"Sin\"; rdf:value 3 ] ; "
			"  lv2:scalePoint [ rdfs:label \"Cos\"; rdf:value 4 ] "
			"] , [\n"
			"  a lv2:EventPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"event_in\" ; "
			"  lv2:name \"Event Input\" ; "
     		"  lv2ev:supportsEvent <http://example.org/event> ;"
     		"  atom:supports <http://example.org/atomEvent> "
			"] , [\n"
			"  a lv2:AudioPort ; a lv2:InputPort ; "
			"  lv2:index 2 ; lv2:symbol \"audio_in\" ; "
			"  lv2:name \"Audio Input\" ; "
			"] , [\n"
			"  a lv2:AudioPort ; a lv2:OutputPort ; "
			"  lv2:index 3 ; lv2:symbol \"audio_out\" ; "
			"  lv2:name \"Audio Output\" ; "
			"] .")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvNode*       psym = lilv_new_string(world, "foo");
	const LilvPort* p    = lilv_plugin_get_port_by_index(plug, 0);
	const LilvPort* p2   = lilv_plugin_get_port_by_symbol(plug, psym);
	lilv_node_free(psym);
	TEST_ASSERT(p != NULL);
	TEST_ASSERT(p2 != NULL);
	TEST_ASSERT(p == p2);

	LilvNode*       nopsym = lilv_new_string(world, "thisaintnoportfoo");
	const LilvPort* p3     = lilv_plugin_get_port_by_symbol(plug, nopsym);
	TEST_ASSERT(p3 == NULL);
	lilv_node_free(nopsym);

	// Try getting an invalid property
	LilvNode*  num     = lilv_new_int(world, 1);
	LilvNodes* nothing = lilv_port_get_value(plug, p, num);
	TEST_ASSERT(!nothing);
	lilv_node_free(num);

	LilvNode* audio_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#AudioPort");
	LilvNode* control_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#ControlPort");
	LilvNode* in_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#InputPort");
	LilvNode* out_class = lilv_new_uri(world,
			"http://lv2plug.in/ns/lv2core#OutputPort");

	TEST_ASSERT(lilv_nodes_size(lilv_port_get_classes(plug, p)) == 2);
	TEST_ASSERT(lilv_plugin_get_num_ports(plug) == 4);
	TEST_ASSERT(lilv_port_is_a(plug, p, control_class));
	TEST_ASSERT(lilv_port_is_a(plug, p, in_class));
	TEST_ASSERT(!lilv_port_is_a(plug, p, audio_class));

	LilvNodes* port_properties = lilv_port_get_properties(plug, p);
	TEST_ASSERT(lilv_nodes_size(port_properties) == 1);
	lilv_nodes_free(port_properties);

	// Untranslated name (current locale is set to "C" in main)
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_port_get_symbol(plug, p)), "foo"));
	LilvNode* name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "store"));
	lilv_node_free(name);

	// Exact language match
	set_env("LANG", "de_DE");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "Laden"));
	lilv_node_free(name);

	// Exact language match (with charset suffix)
	set_env("LANG", "de_AT.utf8");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "Geschaeft"));
	lilv_node_free(name);

	// Partial language match (choose value translated for different country)
	set_env("LANG", "de_CH");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT((!strcmp(lilv_node_as_string(name), "Laden"))
	            ||(!strcmp(lilv_node_as_string(name), "Geschaeft")));
	lilv_node_free(name);

	// Partial language match (choose country-less language tagged value)
	set_env("LANG", "es_MX");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "tienda"));
	lilv_node_free(name);

	// No language match (choose untranslated value)
	set_env("LANG", "cn");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "store"));
	lilv_node_free(name);

	// Invalid language
	set_env("LANG", "1!");
	name = lilv_port_get_name(plug, p);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "store"));
	lilv_node_free(name);

	set_env("LANG", "en_CA.utf-8");

	// Language tagged value with no untranslated values
	LilvNode*  rdfs_comment = lilv_new_uri(world, LILV_NS_RDFS "comment");
	LilvNodes* comments     = lilv_port_get_value(plug, p, rdfs_comment);
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_nodes_get_first(comments)),
	                    "comment"));
	LilvNode* comment = lilv_port_get(plug, p, rdfs_comment);
	TEST_ASSERT(!strcmp(lilv_node_as_string(comment), "comment"));
	lilv_node_free(comment);
	lilv_nodes_free(comments);

	set_env("LANG", "fr");

	comments = lilv_port_get_value(plug, p, rdfs_comment);
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_nodes_get_first(comments)),
	                    "commentaires"));
	lilv_nodes_free(comments);

	set_env("LANG", "cn");

	comments = lilv_port_get_value(plug, p, rdfs_comment);
	TEST_ASSERT(!comments);
	lilv_nodes_free(comments);

	lilv_node_free(rdfs_comment);

	set_env("LANG", "C");  // Reset locale

	LilvScalePoints* points = lilv_port_get_scale_points(plug, p);
	TEST_ASSERT(lilv_scale_points_size(points) == 2);

	LilvIter* sp_iter = lilv_scale_points_begin(points);
	const LilvScalePoint* sp0 = lilv_scale_points_get(points, sp_iter);
	TEST_ASSERT(sp0);
	sp_iter = lilv_scale_points_next(points, sp_iter);
	const LilvScalePoint* sp1 = lilv_scale_points_get(points, sp_iter);
	TEST_ASSERT(sp1);

	TEST_ASSERT(
		((!strcmp(lilv_node_as_string(lilv_scale_point_get_label(sp0)), "Sin")
		  && lilv_node_as_float(lilv_scale_point_get_value(sp0)) == 3)
		 &&
		 (!strcmp(lilv_node_as_string(lilv_scale_point_get_label(sp1)), "Cos")
		  && lilv_node_as_float(lilv_scale_point_get_value(sp1)) == 4))
		||
		((!strcmp(lilv_node_as_string(lilv_scale_point_get_label(sp0)), "Cos")
		  && lilv_node_as_float(lilv_scale_point_get_value(sp0)) == 4)
		 &&
		 (!strcmp(lilv_node_as_string(lilv_scale_point_get_label(sp1)), "Sin")
		  && lilv_node_as_float(lilv_scale_point_get_value(sp1)) == 3)));

	LilvNode* homepage_p = lilv_new_uri(world, "http://usefulinc.com/ns/doap#homepage");
	LilvNodes* homepages = lilv_plugin_get_value(plug, homepage_p);
	TEST_ASSERT(lilv_nodes_size(homepages) == 1);
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_nodes_get_first(homepages)),
			"http://example.org/someplug"));

	LilvNode *min, *max, *def;
	lilv_port_get_range(plug, p, &def, &min, &max);
	TEST_ASSERT(def);
	TEST_ASSERT(min);
	TEST_ASSERT(max);
	TEST_ASSERT(lilv_node_as_float(def) == 0.5);
	TEST_ASSERT(lilv_node_as_float(min) == -1.0);
	TEST_ASSERT(lilv_node_as_float(max) == 1.0);

	LilvNode* integer_prop = lilv_new_uri(world, "http://lv2plug.in/ns/lv2core#integer");
	LilvNode* toggled_prop = lilv_new_uri(world, "http://lv2plug.in/ns/lv2core#toggled");

	TEST_ASSERT(lilv_port_has_property(plug, p, integer_prop));
	TEST_ASSERT(!lilv_port_has_property(plug, p, toggled_prop));

	const LilvPort* ep = lilv_plugin_get_port_by_index(plug, 1);

	LilvNode* event_type = lilv_new_uri(world, "http://example.org/event");
	LilvNode* event_type_2 = lilv_new_uri(world, "http://example.org/otherEvent");
	LilvNode* atom_event = lilv_new_uri(world, "http://example.org/atomEvent");
	TEST_ASSERT(lilv_port_supports_event(plug, ep, event_type));
	TEST_ASSERT(!lilv_port_supports_event(plug, ep, event_type_2));
	TEST_ASSERT(lilv_port_supports_event(plug, ep, atom_event));

	LilvNode* name_p = lilv_new_uri(world, "http://lv2plug.in/ns/lv2core#name");
	LilvNodes* names = lilv_port_get_value(plug, p, name_p);
	TEST_ASSERT(lilv_nodes_size(names) == 1);
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_nodes_get_first(names)),
	                    "store"));
	lilv_nodes_free(names);

	LilvNode* true_val  = lilv_new_bool(world, true);
	LilvNode* false_val = lilv_new_bool(world, false);

	TEST_ASSERT(!lilv_node_equals(true_val, false_val));

	lilv_world_set_option(world, LILV_OPTION_FILTER_LANG, false_val);
	names = lilv_port_get_value(plug, p, name_p);
	TEST_ASSERT(lilv_nodes_size(names) == 4);
	lilv_nodes_free(names);
	lilv_world_set_option(world, LILV_OPTION_FILTER_LANG, true_val);

	lilv_node_free(false_val);
	lilv_node_free(true_val);

	names = lilv_port_get_value(plug, ep, name_p);
	TEST_ASSERT(lilv_nodes_size(names) == 1);
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_nodes_get_first(names)),
	                    "Event Input"));

	const LilvPort* ap_in = lilv_plugin_get_port_by_index(plug, 2);

	TEST_ASSERT(lilv_port_is_a(plug, ap_in, in_class));
	TEST_ASSERT(!lilv_port_is_a(plug, ap_in, out_class));
	TEST_ASSERT(lilv_port_is_a(plug, ap_in, audio_class));
	TEST_ASSERT(!lilv_port_is_a(plug, ap_in, control_class));

	const LilvPort* ap_out = lilv_plugin_get_port_by_index(plug, 3);

	TEST_ASSERT(lilv_port_is_a(plug, ap_out, out_class));
	TEST_ASSERT(!lilv_port_is_a(plug, ap_out, in_class));
	TEST_ASSERT(lilv_port_is_a(plug, ap_out, audio_class));
	TEST_ASSERT(!lilv_port_is_a(plug, ap_out, control_class));

	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, control_class, in_class , NULL) == 1);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, audio_class  , in_class , NULL) == 1);
	TEST_ASSERT(lilv_plugin_get_num_ports_of_class(plug, audio_class  , out_class, NULL) == 1);

	lilv_nodes_free(names);
	lilv_node_free(name_p);

	lilv_node_free(integer_prop);
	lilv_node_free(toggled_prop);
	lilv_node_free(event_type);
	lilv_node_free(event_type_2);
	lilv_node_free(atom_event);

	lilv_node_free(min);
	lilv_node_free(max);
	lilv_node_free(def);

	lilv_node_free(homepage_p);
	lilv_nodes_free(homepages);

	lilv_scale_points_free(points);
	lilv_node_free(control_class);
	lilv_node_free(audio_class);
	lilv_node_free(out_class);
	lilv_node_free(in_class);
	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static unsigned
ui_supported(const char* container_type_uri,
             const char* ui_type_uri)
{
	return !strcmp(container_type_uri, ui_type_uri);
}

static int
test_ui(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES PREFIX_LV2UI
			":plug a lv2:Plugin ; a lv2:CompressorPlugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"lv2:optionalFeature lv2:hardRTCapable ; "
		    "lv2:requiredFeature <http://lv2plug.in/ns/ext/event> ; "
			"lv2ui:ui :ui , :ui2 , :ui3 , :ui4 ; "
			"doap:maintainer [ foaf:name \"David Robillard\" ; "
			"  foaf:homepage <http://drobilla.net> ; foaf:mbox <mailto:d@drobilla.net> ] ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"foo\" ; lv2:name \"bar\" ; "
			"  lv2:minimum -1.0 ; lv2:maximum 1.0 ; lv2:default 0.5 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 1 ; lv2:symbol \"bar\" ; lv2:name \"Baz\" ; "
			"  lv2:minimum -2.0 ; lv2:maximum 2.0 ; lv2:default 1.0 "
			"] , [ "
			"  a lv2:ControlPort ; a lv2:OutputPort ; "
			"  lv2:index 2 ; lv2:symbol \"latency\" ; lv2:name \"Latency\" ; "
			"  lv2:portProperty lv2:reportsLatency "
			"] .\n"
			":ui a lv2ui:GtkUI ; "
			"  lv2ui:requiredFeature lv2ui:makeResident ; "
			"  lv2ui:binary <ui" SHLIB_EXT "> ; "
			"  lv2ui:optionalFeature lv2ui:ext_presets . "
			":ui2 a lv2ui:GtkUI ; lv2ui:binary <ui2" SHLIB_EXT "> . "
			":ui3 a lv2ui:GtkUI ; lv2ui:binary <ui3" SHLIB_EXT "> . "
			":ui4 a lv2ui:GtkUI ; lv2ui:binary <ui4" SHLIB_EXT "> . ")) {
		return 0;
	}

	init_uris();
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* plug = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	LilvUIs* uis = lilv_plugin_get_uis(plug);
	TEST_ASSERT(lilv_uis_size(uis) == 4);

	const LilvUI* ui0 = lilv_uis_get(uis, lilv_uis_begin(uis));
	TEST_ASSERT(ui0);

	LilvNode* ui_uri = lilv_new_uri(world, "http://example.org/ui");
	LilvNode* ui2_uri = lilv_new_uri(world, "http://example.org/ui3");
	LilvNode* ui3_uri = lilv_new_uri(world, "http://example.org/ui4");
	LilvNode* noui_uri = lilv_new_uri(world, "http://example.org/notaui");

	const LilvUI* ui0_2 = lilv_uis_get_by_uri(uis, ui_uri);
	TEST_ASSERT(ui0 == ui0_2);
	TEST_ASSERT(lilv_node_equals(lilv_ui_get_uri(ui0_2), ui_uri));

	const LilvUI* ui2 = lilv_uis_get_by_uri(uis, ui2_uri);
	TEST_ASSERT(ui2 != ui0);

	const LilvUI* ui3 = lilv_uis_get_by_uri(uis, ui3_uri);
	TEST_ASSERT(ui3 != ui0);

	const LilvUI* noui = lilv_uis_get_by_uri(uis, noui_uri);
	TEST_ASSERT(noui == NULL);

	const LilvNodes* classes = lilv_ui_get_classes(ui0);
	TEST_ASSERT(lilv_nodes_size(classes) == 1);

	LilvNode* ui_class_uri = lilv_new_uri(world,
			"http://lv2plug.in/ns/extensions/ui#GtkUI");

	LilvNode* unknown_ui_class_uri = lilv_new_uri(world,
			"http://example.org/mysteryUI");

	TEST_ASSERT(lilv_node_equals(lilv_nodes_get_first(classes), ui_class_uri));
	TEST_ASSERT(lilv_ui_is_a(ui0, ui_class_uri));

	const LilvNode* ui_type = NULL;
	TEST_ASSERT(lilv_ui_is_supported(ui0, ui_supported, ui_class_uri, &ui_type));
	TEST_ASSERT(!lilv_ui_is_supported(ui0, ui_supported, unknown_ui_class_uri, &ui_type));
	TEST_ASSERT(lilv_node_equals(ui_type, ui_class_uri));

	const LilvNode* plug_bundle_uri = lilv_plugin_get_bundle_uri(plug);
	const LilvNode* ui_bundle_uri   = lilv_ui_get_bundle_uri(ui0);
	TEST_ASSERT(lilv_node_equals(plug_bundle_uri, ui_bundle_uri));

	char* ui_binary_uri_str = (char*)malloc(TEST_PATH_MAX);
	snprintf(ui_binary_uri_str, TEST_PATH_MAX, "%s%s",
			lilv_node_as_string(plug_bundle_uri), "ui" SHLIB_EXT);

	const LilvNode* ui_binary_uri = lilv_ui_get_binary_uri(ui0);

	LilvNode* expected_uri = lilv_new_uri(world, ui_binary_uri_str);
	TEST_ASSERT(lilv_node_equals(expected_uri, ui_binary_uri));

	free(ui_binary_uri_str);
	lilv_node_free(unknown_ui_class_uri);
	lilv_node_free(ui_class_uri);
	lilv_node_free(ui_uri);
	lilv_node_free(ui2_uri);
	lilv_node_free(ui3_uri);
	lilv_node_free(noui_uri);
	lilv_node_free(expected_uri);
	lilv_uis_free(uis);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

#ifndef _WIN32

uint32_t atom_Float = 0;
float    in         = 1.0;
float    out        = 42.0;
float    control    = 1234.0;

static const void*
get_port_value(const char* port_symbol,
               void*       user_data,
               uint32_t*   size,
               uint32_t*   type)
{
	if (!strcmp(port_symbol, "input")) {
		*size = sizeof(float);
		*type = atom_Float;
		return &in;
	} else if (!strcmp(port_symbol, "output")) {
		*size = sizeof(float);
		*type = atom_Float;
		return &out;
	} else if (!strcmp(port_symbol, "control")) {
		*size = sizeof(float);
		*type = atom_Float;
		return &control;
	} else {
		fprintf(stderr, "error: get_port_value for nonexistent port `%s'\n",
		        port_symbol);
		*size = *type = 0;
		return NULL;
	}
}

static void
set_port_value(const char*     port_symbol,
               void*           user_data,
               const void*     value,
               uint32_t        size,
               uint32_t        type)
{
	if (!strcmp(port_symbol, "input")) {
		in = *(const float*)value;
	} else if (!strcmp(port_symbol, "output")) {
		out = *(const float*)value;
	} else if (!strcmp(port_symbol, "control")) {
		control = *(const float*)value;
	} else {
		fprintf(stderr, "error: set_port_value for nonexistent port `%s'\n",
		        port_symbol);
	}
}

char** uris   = NULL;
size_t n_uris = 0;

static LV2_URID
map_uri(LV2_URID_Map_Handle handle,
        const char*         uri)
{
	for (size_t i = 0; i < n_uris; ++i) {
		if (!strcmp(uris[i], uri)) {
			return i + 1;
		}
	}

	assert(serd_uri_string_has_scheme((const uint8_t*)uri));
	uris = (char**)realloc(uris, ++n_uris * sizeof(char*));
	uris[n_uris - 1] = lilv_strdup(uri);
	return n_uris;
}

static const char*
unmap_uri(LV2_URID_Map_Handle handle,
          LV2_URID            urid)
{
	if (urid > 0 && urid <= n_uris) {
		return uris[urid - 1];
	}
	return NULL;
}

static char* temp_dir = NULL;

static char*
lilv_make_path(LV2_State_Make_Path_Handle handle,
               const char*                path)
{
	return lilv_path_join(temp_dir, path);
}

static int
test_state(void)
{
	init_world();

	uint8_t*   abs_bundle = (uint8_t*)lilv_path_absolute(LILV_TEST_BUNDLE);
	SerdNode   bundle     = serd_node_new_file_uri(abs_bundle, 0, 0, true);
	LilvNode*  bundle_uri = lilv_new_uri(world, (const char*)bundle.buf);
	LilvNode*  plugin_uri = lilv_new_uri(world,
	                                     "http://example.org/lilv-test-plugin");
	lilv_world_load_bundle(world, bundle_uri);
	free(abs_bundle);
	serd_node_free(&bundle);

	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plugin  = lilv_plugins_get_by_uri(plugins, plugin_uri);
	TEST_ASSERT(plugin);

	LV2_URID_Map       map           = { NULL, map_uri };
	LV2_Feature        map_feature   = { LV2_URID_MAP_URI, &map };
	LV2_URID_Unmap     unmap         = { NULL, unmap_uri };
	LV2_Feature        unmap_feature = { LV2_URID_UNMAP_URI, &unmap };
	const LV2_Feature* features[]    = { &map_feature, &unmap_feature, NULL };

	atom_Float = map.map(map.handle, "http://lv2plug.in/ns/ext/atom#Float");

	LilvNode*  num     = lilv_new_int(world, 5);
	LilvState* nostate = lilv_state_new_from_file(world, &map, num, "/junk");
	TEST_ASSERT(!nostate);

	LilvInstance* instance = lilv_plugin_instantiate(plugin, 48000.0, features);
	TEST_ASSERT(instance);
	lilv_instance_activate(instance);
	lilv_instance_connect_port(instance, 0, &in);
	lilv_instance_connect_port(instance, 1, &out);
	lilv_instance_run(instance, 1);
	TEST_ASSERT(in == 1.0);
	TEST_ASSERT(out == 1.0);

	temp_dir = lilv_realpath("temp");

	const char* scratch_dir = NULL;
	char*       copy_dir    = NULL;
	char*       link_dir    = NULL;
	char*       save_dir    = NULL;

	// Get instance state state
	LilvState* state = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, save_dir,
		get_port_value, world, 0, NULL);

	// Get another instance state
	LilvState* state2 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, save_dir,
		get_port_value, world, 0, NULL);

	// Ensure they are equal
	TEST_ASSERT(lilv_state_equals(state, state2));

	// Check that we can't delete unsaved state
	TEST_ASSERT(lilv_state_delete(world, state));

	// Check that state has no URI
	TEST_ASSERT(!lilv_state_get_uri(state));

	// Check that we can't save a state with no URI
	char* bad_state_str = lilv_state_to_string(
		world, &map, &unmap, state, NULL, NULL);
	TEST_ASSERT(!bad_state_str);

	// Check that we can't restore the NULL string (and it doesn't crash)
	LilvState* bad_state = lilv_state_new_from_string(world, &map, NULL);
	TEST_ASSERT(!bad_state);

	// Save state to a string
	char* state1_str = lilv_state_to_string(
		world, &map, &unmap, state, "http://example.org/state1", NULL);

	// Restore from string
	LilvState* from_str = lilv_state_new_from_string(world, &map, state1_str);

	// Ensure they are equal
	TEST_ASSERT(lilv_state_equals(state, from_str));
	lilv_free(state1_str);

	const LilvNode* state_plugin_uri = lilv_state_get_plugin_uri(state);
	TEST_ASSERT(lilv_node_equals(state_plugin_uri, plugin_uri));

	// Tinker with the label of the first state
	TEST_ASSERT(lilv_state_get_label(state) == NULL);
	lilv_state_set_label(state, "Test State Old Label");
	TEST_ASSERT(!strcmp(lilv_state_get_label(state), "Test State Old Label"));
	lilv_state_set_label(state, "Test State");
	TEST_ASSERT(!strcmp(lilv_state_get_label(state), "Test State"));

	TEST_ASSERT(!lilv_state_equals(state, state2));  // Label changed

	// Run and get a new instance state (which should now differ)
	lilv_instance_run(instance, 1);
	LilvState* state3 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, save_dir,
		get_port_value, world, 0, NULL);
	TEST_ASSERT(!lilv_state_equals(state2, state3));  // num_runs changed

	// Restore instance state to original state
	lilv_state_restore(state2, instance, set_port_value, NULL, 0, NULL);

	// Take a new snapshot and ensure it matches the set state
	LilvState* state4 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, save_dir,
		get_port_value, world, 0, NULL);
	TEST_ASSERT(lilv_state_equals(state2, state4));

	// Set some metadata properties
	lilv_state_set_metadata(state, map.map(map.handle, LILV_NS_RDFS "comment"),
	                        "This is a comment",
	                        strlen("This is a comment") + 1,
	                        map.map(map.handle, "http://lv2plug.in/ns/ext/atom#Literal"),
	                        LV2_STATE_IS_POD);
	lilv_state_set_metadata(state, map.map(map.handle, "http://example.org/metablob"),
	                        "LIVEBEEF",
	                        strlen("LIVEBEEF") + 1,
	                        map.map(map.handle, "http://example.org/MetaBlob"),
	                        0);

	// Save state to a directory
	int ret = lilv_state_save(world, &map, &unmap, state, NULL,
	                          "state/state.lv2", "state.ttl");
	TEST_ASSERT(!ret);

	// Load state from directory
	LilvState* state5 = lilv_state_new_from_file(world, &map, NULL,
	                                             "state/state.lv2/state.ttl");

	TEST_ASSERT(lilv_state_equals(state, state5));  // Round trip accuracy
	TEST_ASSERT(lilv_state_get_num_properties(state) == 8);

	// Attempt to save state to nowhere (error)
	ret = lilv_state_save(world, &map, &unmap, state, NULL, NULL, NULL);
	TEST_ASSERT(ret);

	// Save another state to the same directory (update manifest)
	ret = lilv_state_save(world, &map, &unmap, state, NULL,
	                      "state/state.lv2", "state2.ttl");
	TEST_ASSERT(!ret);

	// Save state with URI to a directory
	const char* state_uri = "http://example.org/state";
	ret = lilv_state_save(world, &map, &unmap, state, state_uri,
	                      "state/state6.lv2", "state6.ttl");
	TEST_ASSERT(!ret);

	// Load default bundle into world and load state from it
	uint8_t*  state6_path       = (uint8_t*)lilv_path_absolute("state/state6.lv2/");
	SerdNode  state6_uri        = serd_node_new_file_uri(state6_path, 0, 0, true);
	LilvNode* test_state_bundle = lilv_new_uri(world, (const char*)state6_uri.buf);
	LilvNode* test_state_node   = lilv_new_uri(world, state_uri);
	lilv_world_load_bundle(world, test_state_bundle);
	lilv_world_load_resource(world, test_state_node);
	serd_node_free(&state6_uri);
	lilv_free(state6_path);

	LilvState* state6 = lilv_state_new_from_world(world, &map, test_state_node);
	TEST_ASSERT(lilv_state_equals(state, state6));  // Round trip accuracy

	// Check that loaded state has correct URI
	TEST_ASSERT(lilv_state_get_uri(state6));
	TEST_ASSERT(!strcmp(lilv_node_as_string(lilv_state_get_uri(state6)),
	                    state_uri));

	lilv_world_unload_resource(world, test_state_node);
	lilv_world_unload_bundle(world, test_state_bundle);

	LilvState* state6_2 = lilv_state_new_from_world(world, &map, test_state_node);
	TEST_ASSERT(!state6_2);  // No longer present
	lilv_state_free(state6_2);

	lilv_node_free(test_state_bundle);
	lilv_node_free(test_state_node);

	unsetenv("LV2_STATE_BUNDLE");

	// Make directories and test files support
	mkdir("temp", 0700);
	scratch_dir = temp_dir;
	mkdir("files", 0700);
	copy_dir = lilv_realpath("files");
	mkdir("links", 0700);
	link_dir = lilv_realpath("links");

	LV2_State_Make_Path make_path         = { NULL, lilv_make_path };
	LV2_Feature         make_path_feature = { LV2_STATE__makePath, &make_path };
	const LV2_Feature*  ffeatures[]       = { &make_path_feature, &map_feature, NULL };

	lilv_instance_deactivate(instance);
	lilv_instance_free(instance);
	instance = lilv_plugin_instantiate(plugin, 48000.0, ffeatures);
	lilv_instance_activate(instance);
	lilv_instance_connect_port(instance, 0, &in);
	lilv_instance_connect_port(instance, 1, &out);
	lilv_instance_run(instance, 1);

	// Test instantiating twice
	LilvInstance* instance2 = lilv_plugin_instantiate(plugin, 48000.0, ffeatures);
	if (!instance2) {
		fatal_error("Failed to create multiple instances of <%s>\n",
		            lilv_node_as_uri(state_plugin_uri));
		return 0;
	}
	lilv_instance_free(instance2);

	// Get instance state state
	LilvState* fstate = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, "state/fstate.lv2",
		get_port_value, world, 0, ffeatures);

	// Get another instance state
	LilvState* fstate2 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, "state/fstate2.lv2",
		get_port_value, world, 0, ffeatures);

	// Should be identical
	TEST_ASSERT(lilv_state_equals(fstate, fstate2));

	// Run, writing more to rec file
	lilv_instance_run(instance, 2);

	// Get yet another instance state
	LilvState* fstate3 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, "state/fstate3.lv2",
		get_port_value, world, 0, ffeatures);

	// Should be different
	TEST_ASSERT(!lilv_state_equals(fstate, fstate3));

	// Save state to a directory
	ret = lilv_state_save(world, &map, &unmap, fstate, NULL,
	                      "state/fstate.lv2", "fstate.ttl");
	TEST_ASSERT(!ret);

	// Load state from directory
	LilvState* fstate4 = lilv_state_new_from_file(
		world, &map, NULL, "state/fstate.lv2/fstate.ttl");
	TEST_ASSERT(lilv_state_equals(fstate, fstate4));  // Round trip accuracy

	// Restore instance state to loaded state
	lilv_state_restore(fstate4, instance, set_port_value, NULL, 0, ffeatures);

	// Take a new snapshot and ensure it matches
	LilvState* fstate5 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, "state/fstate5.lv2",
		get_port_value, world, 0, ffeatures);
	TEST_ASSERT(lilv_state_equals(fstate3, fstate5));

	// Save state to a (different) directory again
	ret = lilv_state_save(world, &map, &unmap, fstate, NULL,
	                      "state/fstate6.lv2", "fstate6.ttl");
	TEST_ASSERT(!ret);

	// Reload it and ensure it's identical to the other loaded version
	LilvState* fstate6 = lilv_state_new_from_file(
		world, &map, NULL, "state/fstate6.lv2/fstate6.ttl");
	TEST_ASSERT(lilv_state_equals(fstate4, fstate6));

	// Run, changing rec file (without changing size)
	lilv_instance_run(instance, 3);

	// Take a new snapshot
	LilvState* fstate7 = lilv_state_new_from_instance(
		plugin, instance, &map,
		scratch_dir, copy_dir, link_dir, "state/fstate7.lv2",
		get_port_value, world, 0, ffeatures);
	TEST_ASSERT(!lilv_state_equals(fstate6, fstate7));

	// Save the changed state to a (different) directory again
	ret = lilv_state_save(world, &map, &unmap, fstate7, NULL,
	                      "state/fstate7.lv2", "fstate7.ttl");
	TEST_ASSERT(!ret);

	// Reload it and ensure it's changed
	LilvState* fstate72 = lilv_state_new_from_file(
		world, &map, NULL, "state/fstate7.lv2/fstate7.ttl");
	TEST_ASSERT(lilv_state_equals(fstate72, fstate7));
	TEST_ASSERT(!lilv_state_equals(fstate6, fstate72));

	// Delete saved state
	lilv_state_delete(world, fstate7);

	lilv_instance_deactivate(instance);
	lilv_instance_free(instance);

	lilv_node_free(num);

	lilv_state_free(state);
	lilv_state_free(from_str);
	lilv_state_free(state2);
	lilv_state_free(state3);
	lilv_state_free(state4);
	lilv_state_free(state5);
	lilv_state_free(state6);
	lilv_state_free(fstate);
	lilv_state_free(fstate2);
	lilv_state_free(fstate3);
	lilv_state_free(fstate4);
	lilv_state_free(fstate5);
	lilv_state_free(fstate6);
	lilv_state_free(fstate7);
	lilv_state_free(fstate72);

	// Free URI map
	for (size_t i = 0; i < n_uris; ++i) {
		free(uris[i]);
	}
	free(uris);
	n_uris = 0;

	lilv_node_free(plugin_uri);
	lilv_node_free(bundle_uri);
	free(link_dir);
	free(copy_dir);
	free(temp_dir);

	cleanup_uris();
	return 1;
}
#endif

/*****************************************************************************/

static int
test_bad_port_symbol(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES PREFIX_LV2EV
			":plug a lv2:Plugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"doap:homepage <http://example.org/someplug> ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index 0 ; lv2:symbol \"0invalid\" ;"
			"  lv2:name \"Invalid\" ; "
			"] .")) {
		return 0;
	}

	init_uris();

	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);

	uint32_t n_ports = lilv_plugin_get_num_ports(plug);
	TEST_ASSERT(n_ports == 0);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_bad_port_index(void)
{
	if (!start_bundle(MANIFEST_PREFIXES
			":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
			BUNDLE_PREFIXES PREFIX_LV2EV
			":plug a lv2:Plugin ; "
			PLUGIN_NAME("Test plugin") " ; "
			LICENSE_GPL " ; "
			"doap:homepage <http://example.org/someplug> ; "
			"lv2:port [ "
			"  a lv2:ControlPort ; a lv2:InputPort ; "
			"  lv2:index \"notaninteger\" ; lv2:symbol \"invalid\" ;"
			"  lv2:name \"Invalid\" ; "
			"] .")) {
		return 0;
	}

	init_uris();

	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);

	uint32_t n_ports = lilv_plugin_get_num_ports(plug);
	TEST_ASSERT(n_ports == 0);

	cleanup_uris();
	return 1;
}

/*****************************************************************************/

static int
test_string(void)
{
	char* s = NULL;

	TEST_ASSERT(!strcmp((s = lilv_dirname("/foo/bar")), "/foo")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("/foo/bar/")), "/foo")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("/foo///bar/")), "/foo")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("/foo///bar//")), "/foo")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("foo")), ".")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("/foo")), "/")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("/")), "/")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_dirname("//")), "/")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_relative_to("/a/b", "/a/")), "b")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_relative_to("/a", "/b/c/")), "/a")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_relative_to("/a/b/c", "/a/b/d/")), "../c")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_relative_to("/a/b/c", "/a/b/d/e/")), "../../c")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join("/a", "b")), "/a/b")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join("/a", "/b")), "/a/b")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join("/a/", "/b")), "/a/b")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join("/a/", "b")), "/a/b")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join("/a", NULL)), "/a/")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_path_join(NULL, "/b")), "/b")); free(s);

#ifndef _WIN32
	setenv("LILV_TEST_1", "test", 1);
	char* home_foo = lilv_strjoin(getenv("HOME"), "/foo", NULL);
	TEST_ASSERT(!strcmp((s = lilv_expand("$LILV_TEST_1")), "test")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_expand("~")), getenv("HOME"))); free(s);
	TEST_ASSERT(!strcmp((s = lilv_expand("~foo")), "~foo")); free(s);
	TEST_ASSERT(!strcmp((s = lilv_expand("~/foo")), home_foo)); free(s);
	TEST_ASSERT(!strcmp((s = lilv_expand("$NOT_A_VAR")), "$NOT_A_VAR")); free(s);
	free(home_foo);
	unsetenv("LILV_TEST_1");
#endif

	return 1;
}

/*****************************************************************************/

static int
test_world(void)
{
	if (!init_world()) {
		return 0;
	}

	LilvNode* num = lilv_new_int(world, 4);
	LilvNode* uri = lilv_new_uri(world, "http://example.org/object");

	LilvNodes* matches = lilv_world_find_nodes(world, num, NULL, NULL);
	TEST_ASSERT(!matches);

	matches = lilv_world_find_nodes(world, NULL, num, NULL);
	TEST_ASSERT(!matches);

	matches = lilv_world_find_nodes(world, NULL, uri, NULL);
	TEST_ASSERT(!matches);

	lilv_node_free(uri);
	lilv_node_free(num);

	lilv_world_unload_bundle(world, NULL);

	return 1;
}

/*****************************************************************************/

static int
test_reload_bundle(void)
{
	// Create a simple plugin bundle
	create_bundle(MANIFEST_PREFIXES
	              ":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
	              BUNDLE_PREFIXES
	              ":plug a lv2:Plugin ; "
	              PLUGIN_NAME("First name") " .");

	if (!init_world()) {
		return 0;
	}

	init_uris();
	lilv_world_load_specifications(world);

	// Load bundle
	LilvNode* bundle_uri = lilv_new_uri(world, test_bundle_uri);
	lilv_world_load_bundle(world, bundle_uri);

	// Check that plugin is present
	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  plug    = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug);

	// Check that plugin name is correct
	LilvNode* name = lilv_plugin_get_name(plug);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name), "First name"));
	lilv_node_free(name);

	// Unload bundle from world and delete it
	lilv_world_unload_bundle(world, bundle_uri);
	delete_bundle();

	// Create a new version of the same bundle, but with a different name
	create_bundle(MANIFEST_PREFIXES
	              ":plug a lv2:Plugin ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
	              BUNDLE_PREFIXES
	              ":plug a lv2:Plugin ; "
	              PLUGIN_NAME("Second name") " .");

	// Check that plugin is no longer in the world's plugin list
	TEST_ASSERT(lilv_plugins_size(plugins) == 0);

	// Load new bundle
	lilv_world_load_bundle(world, bundle_uri);

	// Check that plugin is present again and is the same LilvPlugin
	const LilvPlugin* plug2 = lilv_plugins_get_by_uri(plugins, plugin_uri_value);
	TEST_ASSERT(plug2);
	TEST_ASSERT(plug2 == plug);

	// Check that plugin now has new name
	LilvNode* name2 = lilv_plugin_get_name(plug2);
	TEST_ASSERT(name2);
	TEST_ASSERT(!strcmp(lilv_node_as_string(name2), "Second name"));
	lilv_node_free(name2);

	// Load new bundle again (noop)
	lilv_world_load_bundle(world, bundle_uri);

	cleanup_uris();
	lilv_node_free(bundle_uri);
	lilv_world_free(world);
	world = NULL;

	return 1;
}

/*****************************************************************************/

static int
test_replace_version(void)
{
	if (!init_world()) {
		return 0;
	}

	LilvNode* plug_uri         = lilv_new_uri(world, "http://example.org/versioned");
	LilvNode* lv2_minorVersion = lilv_new_uri(world, LV2_CORE__minorVersion);
	LilvNode* lv2_microVersion = lilv_new_uri(world, LV2_CORE__microVersion);
	LilvNode* minor            = NULL;
	LilvNode* micro            = NULL;

	char* old_bundle_path = lilv_strjoin(LILV_TEST_DIR, "old_version.lv2/", 0);

	// Load plugin from old bundle
	LilvNode* old_bundle = lilv_new_file_uri(world, NULL, old_bundle_path);
	lilv_world_load_bundle(world, old_bundle);
	lilv_world_load_resource(world, plug_uri);

	// Check version
	const LilvPlugins* plugins  = lilv_world_get_all_plugins(world);
	const LilvPlugin*  old_plug = lilv_plugins_get_by_uri(plugins, plug_uri);
	TEST_ASSERT(old_plug);
	minor = lilv_world_get(world, plug_uri, lv2_minorVersion, 0);
	micro = lilv_world_get(world, plug_uri, lv2_microVersion, 0);
	TEST_ASSERT(!strcmp(lilv_node_as_string(minor), "1"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(micro), "0"));
	lilv_node_free(micro);
	lilv_node_free(minor);

	char* new_bundle_path = lilv_strjoin(LILV_TEST_DIR, "new_version.lv2/", 0);

	// Load plugin from new bundle
	LilvNode* new_bundle = lilv_new_file_uri(world, NULL, new_bundle_path);
	lilv_world_load_bundle(world, new_bundle);
	lilv_world_load_resource(world, plug_uri);

	// Check that version in the world model has changed
	plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin* new_plug = lilv_plugins_get_by_uri(plugins, plug_uri);
	TEST_ASSERT(new_plug);
	TEST_ASSERT(lilv_node_equals(lilv_plugin_get_bundle_uri(new_plug), new_bundle));
	minor = lilv_world_get(world, plug_uri, lv2_minorVersion, 0);
	micro = lilv_world_get(world, plug_uri, lv2_microVersion, 0);
	TEST_ASSERT(!strcmp(lilv_node_as_string(minor), "2"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(micro), "1"));
	lilv_node_free(micro);
	lilv_node_free(minor);

	// Try to load the old version again
	lilv_world_load_bundle(world, old_bundle);
	lilv_world_load_resource(world, plug_uri);

	// Check that version in the world model has not changed
	plugins = lilv_world_get_all_plugins(world);
	new_plug = lilv_plugins_get_by_uri(plugins, plug_uri);
	TEST_ASSERT(new_plug);
	minor = lilv_world_get(world, plug_uri, lv2_minorVersion, 0);
	micro = lilv_world_get(world, plug_uri, lv2_microVersion, 0);
	TEST_ASSERT(!strcmp(lilv_node_as_string(minor), "2"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(micro), "1"));
	lilv_node_free(micro);
	lilv_node_free(minor);

	lilv_node_free(new_bundle);
	lilv_node_free(old_bundle);
	free(new_bundle_path);
	free(old_bundle_path);
	lilv_node_free(plug_uri);
	lilv_node_free(lv2_minorVersion);
	lilv_node_free(lv2_microVersion);
	return 1;
}

/*****************************************************************************/

static int
test_get_symbol(void)
{
	if (!start_bundle(
		    MANIFEST_PREFIXES
		    ":plug a lv2:Plugin ; lv2:symbol \"plugsym\" ; lv2:binary <foo" SHLIB_EXT "> ; rdfs:seeAlso <plugin.ttl> .\n",
		    BUNDLE_PREFIXES PREFIX_LV2EV
		    ":plug a lv2:Plugin ; "
		    PLUGIN_NAME("Test plugin") " ; "
		    "lv2:symbol \"plugsym\" .")) {
		return 0;
	}

	init_uris();

	LilvNode* plug_sym      = lilv_world_get_symbol(world, plugin_uri_value);
	LilvNode* path          = lilv_new_uri(world, "http://example.org/foo");
	LilvNode* path_sym      = lilv_world_get_symbol(world, path);
	LilvNode* query         = lilv_new_uri(world, "http://example.org/foo?bar=baz");
	LilvNode* query_sym     = lilv_world_get_symbol(world, query);
	LilvNode* frag          = lilv_new_uri(world, "http://example.org/foo#bar");
	LilvNode* frag_sym      = lilv_world_get_symbol(world, frag);
	LilvNode* queryfrag     = lilv_new_uri(world, "http://example.org/foo?bar=baz#quux");
	LilvNode* queryfrag_sym = lilv_world_get_symbol(world, queryfrag);
	LilvNode* nonuri        = lilv_new_int(world, 42);

	TEST_ASSERT(lilv_world_get_symbol(world, nonuri) == NULL);
	TEST_ASSERT(!strcmp(lilv_node_as_string(plug_sym), "plugsym"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(path_sym), "foo"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(query_sym), "bar_baz"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(frag_sym), "bar"));
	TEST_ASSERT(!strcmp(lilv_node_as_string(queryfrag_sym), "quux"));

	lilv_node_free(nonuri);
	lilv_node_free(queryfrag_sym);
	lilv_node_free(queryfrag);
	lilv_node_free(frag_sym);
	lilv_node_free(frag);
	lilv_node_free(query_sym);
	lilv_node_free(query);
	lilv_node_free(path_sym);
	lilv_node_free(path);
	lilv_node_free(plug_sym);
	cleanup_uris();

	return 1;
}

/*****************************************************************************/

/* add tests here */
static struct TestCase tests[] = {
	TEST_CASE(util),
	TEST_CASE(value),
	TEST_CASE(verify),
	TEST_CASE(no_verify),
	TEST_CASE(discovery),
	TEST_CASE(classes),
	TEST_CASE(plugin),
	TEST_CASE(project),
	TEST_CASE(no_author),
	TEST_CASE(project_no_author),
	TEST_CASE(preset),
	TEST_CASE(prototype),
	TEST_CASE(port),
	TEST_CASE(ui),
	TEST_CASE(bad_port_symbol),
	TEST_CASE(bad_port_index),
	TEST_CASE(bad_port_index),
	TEST_CASE(string),
	TEST_CASE(world),
	// FIXME: State is not currently working on Windows
#ifndef _WIN32
	TEST_CASE(state),
#endif
	TEST_CASE(reload_bundle),
	TEST_CASE(replace_version),
	TEST_CASE(get_symbol),
	{ NULL, NULL }
};

static void
run_tests(void)
{
	int i;
	for (i = 0; tests[i].title; i++) {
		printf("*** Test %s\n", tests[i].title);
		if (!tests[i].func()) {
			printf("\nTest failed\n");
			/* test case that wasn't able to be executed at all counts as 1 test + 1 error */
			error_count++;
			test_count++;
		}
		unload_bundle();
		cleanup();
	}
}

int
main(int argc, char* argv[])
{
	if (argc != 1) {
		printf("Syntax: %s\n", argv[0]);
		return 0;
	}
	set_env("LANG", "C");
	init_tests();
	run_tests();
	cleanup();
	printf("\n*** Test Results: %d tests, %d errors\n\n", test_count, error_count);
	return error_count ? 1 : 0;
}
