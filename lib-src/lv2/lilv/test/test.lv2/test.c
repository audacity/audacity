/*
  Lilv Test Plugin
  Copyright 2011-2019 David Robillard <d@drobilla.net>

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

#define _POSIX_C_SOURCE 200809L

#include "lv2/atom/atom.h"
#include "lv2/core/lv2.h"
#include "lv2/state/state.h"
#include "lv2/urid/urid.h"

#ifdef _MSC_VER
#    include <io.h>
#    define mkstemp(pat) _mktemp(pat)
#endif

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TEST_URI "http://example.org/lilv-test-plugin"

#define TMP_TEMPLATE "lilv_testXXXXXX"

enum {
	TEST_INPUT   = 0,
	TEST_OUTPUT  = 1,
	TEST_CONTROL = 2
};

typedef struct {
	LV2_URID_Map* map;

	struct {
		LV2_URID atom_Float;
	} uris;

	char tmp_file_path[sizeof(TMP_TEMPLATE)];
	char* rec_file_path;
	FILE* rec_file;

	float*   input;
	float*   output;
	unsigned num_runs;
} Test;

static void
free_path(char* path)
{
	/* FIXME: Temporary hack to avoid mismatched malloc/free crashes on
	   Windows.  The specifications needs a feature for this. */
#ifndef _WIN32
	free(path);
#endif
}

static void
cleanup(LV2_Handle instance)
{
	Test* test = (Test*)instance;
	if (test->rec_file) {
		fclose(test->rec_file);
	}
	free_path(test->rec_file_path);
	free(instance);
}

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	Test* test = (Test*)instance;
	switch (port) {
	case TEST_INPUT:
		test->input = (float*)data;
		break;
	case TEST_OUTPUT:
		test->output = (float*)data;
		break;
	case TEST_CONTROL:
		test->output = (float*)data;
		break;
	default:
		break;
	}
}

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               path,
            const LV2_Feature* const* features)
{
	Test* test = (Test*)calloc(1, sizeof(Test));
	if (!test) {
		return NULL;
	}

	strncpy(test->tmp_file_path, TMP_TEMPLATE, strlen(TMP_TEMPLATE) + 1);
	mkstemp(test->tmp_file_path);

	LV2_State_Make_Path* make_path = NULL;

	for (int i = 0; features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_URID_URI "#map")) {
			test->map = (LV2_URID_Map*)features[i]->data;
			test->uris.atom_Float = test->map->map(
				test->map->handle, LV2_ATOM__Float);
		} else if (!strcmp(features[i]->URI, LV2_STATE__makePath)) {
			make_path = (LV2_State_Make_Path*)features[i]->data;
		}
	}

	if (!test->map) {
		fprintf(stderr, "Host does not support urid:map\n");
		free(test);
		return NULL;
	}

	if (make_path) {
		test->rec_file_path = make_path->path(make_path->handle, "recfile");
		if (!(test->rec_file = fopen(test->rec_file_path, "w"))) {
			fprintf(stderr, "ERROR: Failed to open rec file\n");
		}
		fprintf(test->rec_file, "instantiate\n");
	}

	return (LV2_Handle)test;
}

static void
run(LV2_Handle instance,
    uint32_t   sample_count)
{
	Test* test = (Test*)instance;
	*test->output = *test->input;
	if (sample_count == 1) {
		++test->num_runs;
	} else if (sample_count == 2 && test->rec_file) {
		// Append to rec file (changes size)
		fprintf(test->rec_file, "run\n");
	} else if (sample_count == 3 && test->rec_file) {
		// Change the first byte of rec file (doesn't change size)
		fseek(test->rec_file, 0, SEEK_SET);
		fprintf(test->rec_file, "X");
		fseek(test->rec_file, 0, SEEK_END);
	}
}

static uint32_t
map_uri(Test* plugin, const char* uri)
{
	return plugin->map->map(plugin->map->handle, uri);
}

static LV2_State_Status
save(LV2_Handle                instance,
     LV2_State_Store_Function  store,
     void*                     callback_data,
     uint32_t                  flags,
     const LV2_Feature* const* features)
{
	Test* plugin = (Test*)instance;

	LV2_State_Map_Path*  map_path  = NULL;
	LV2_State_Make_Path* make_path = NULL;
	for (int i = 0; features && features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_STATE__mapPath)) {
			map_path = (LV2_State_Map_Path*)features[i]->data;
		} else if (!strcmp(features[i]->URI, LV2_STATE__makePath)) {
			make_path = (LV2_State_Make_Path*)features[i]->data;
		}
	}

	store(callback_data,
	      map_uri(plugin, "http://example.org/greeting"),
	      "hello",
	      strlen("hello") + 1,
	      map_uri(plugin, LV2_ATOM__String),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	const uint32_t urid = map_uri(plugin, "http://example.org/urivalue");
	store(callback_data,
	      map_uri(plugin, "http://example.org/uri"),
	      &urid,
	      sizeof(uint32_t),
	      map_uri(plugin, LV2_ATOM__URID),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	// Try to store second value for the same property (should fail)
	const uint32_t urid2 = map_uri(plugin, "http://example.org/urivalue2");
	if (!store(callback_data,
	           map_uri(plugin, "http://example.org/uri"),
	           &urid2,
	           sizeof(uint32_t),
	           map_uri(plugin, LV2_ATOM__URID),
	           LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE)) {
		return LV2_STATE_ERR_UNKNOWN;
	}

	// Try to store with a null key (should fail)
	if (!store(callback_data,
	           0,
	           &urid2,
	           sizeof(uint32_t),
	           map_uri(plugin, LV2_ATOM__URID),
	           LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE)) {
		return LV2_STATE_ERR_UNKNOWN;
	}

	store(callback_data,
	      map_uri(plugin, "http://example.org/num-runs"),
	      &plugin->num_runs,
	      sizeof(plugin->num_runs),
	      map_uri(plugin, LV2_ATOM__Int),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	const float two = 2.0f;
	store(callback_data,
	      map_uri(plugin, "http://example.org/two"),
	      &two,
	      sizeof(two),
	      map_uri(plugin, LV2_ATOM__Float),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	const uint32_t affirmative = 1;
	store(callback_data,
	      map_uri(plugin, "http://example.org/true"),
	      &affirmative,
	      sizeof(affirmative),
	      map_uri(plugin, LV2_ATOM__Bool),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	const uint32_t negative = 0;
	store(callback_data,
	      map_uri(plugin, "http://example.org/false"),
	      &negative,
	      sizeof(negative),
	      map_uri(plugin, LV2_ATOM__Bool),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	const uint8_t blob[] = "I am a blob of arbitrary data.";
	store(callback_data,
	      map_uri(plugin, "http://example.org/blob"),
	      blob,
	      sizeof(blob),
	      map_uri(plugin, "http://example.org/SomeUnknownType"),
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	if (map_path) {
		FILE* file = fopen(plugin->tmp_file_path, "w");
		fprintf(file, "Hello\n");
		fclose(file);
		char* apath = map_path->abstract_path(map_path->handle,
		                                      plugin->tmp_file_path);
		char* apath2 = map_path->abstract_path(map_path->handle,
		                                       plugin->tmp_file_path);
		if (strcmp(apath, apath2)) {
			fprintf(stderr, "ERROR: Path %s != %s\n", apath, apath2);
		}

		store(callback_data,
		      map_uri(plugin, "http://example.org/extfile"),
		      apath,
		      strlen(apath) + 1,
		      map_uri(plugin, LV2_ATOM__Path),
		      LV2_STATE_IS_POD);

		free_path(apath);
		free_path(apath2);

		if (plugin->rec_file) {
			fflush(plugin->rec_file);
			apath = map_path->abstract_path(map_path->handle,
			                                plugin->rec_file_path);

			store(callback_data,
			      map_uri(plugin, "http://example.org/recfile"),
			      apath,
			      strlen(apath) + 1,
			      map_uri(plugin, LV2_ATOM__Path),
			      LV2_STATE_IS_POD);

			free_path(apath);
		}

		if (make_path) {
			char* spath = make_path->path(make_path->handle, "save");
			FILE* sfile = fopen(spath, "w");
			fprintf(sfile, "save");
			fclose(sfile);

			apath = map_path->abstract_path(map_path->handle, spath);
			store(callback_data,
			      map_uri(plugin, "http://example.org/save-file"),
			      apath,
			      strlen(apath) + 1,
			      map_uri(plugin, LV2_ATOM__Path),
			      LV2_STATE_IS_POD);
			free_path(apath);
			free_path(spath);
		}
	}

	return LV2_STATE_SUCCESS;
}

static LV2_State_Status
restore(LV2_Handle                  instance,
        LV2_State_Retrieve_Function retrieve,
        void*                       callback_data,
        uint32_t                    flags,
        const LV2_Feature* const*   features)
{
	Test* plugin = (Test*)instance;

	LV2_State_Map_Path* map_path = NULL;
	for (int i = 0; features && features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_STATE__mapPath)) {
			map_path = (LV2_State_Map_Path*)features[i]->data;
		}
	}

	size_t   size;
	uint32_t type;
	uint32_t valflags;

	plugin->num_runs = *(int32_t*)retrieve(
		callback_data,
		map_uri(plugin, "http://example.org/num-runs"),
		&size, &type, &valflags);

	if (!map_path) {
		return LV2_STATE_ERR_NO_FEATURE;
	}

	char* apath = (char*)retrieve(
		callback_data,
		map_uri(plugin, "http://example.org/extfile"),
		&size, &type, &valflags);

	if (valflags != LV2_STATE_IS_POD) {
		fprintf(stderr, "error: Restored bad file flags\n");
		return LV2_STATE_ERR_BAD_FLAGS;
	}

	if (apath) {
		char*  path   = map_path->absolute_path(map_path->handle, apath);
		FILE*  f      = fopen(path, "r");
		char   str[8];
		size_t n_read = fread(str, 1, sizeof(str), f);
		fclose(f);
		if (strncmp(str, "Hello\n", n_read)) {
			fprintf(stderr, "error: Restored bad file contents `%s' != `Hello'\n",
			        str);
		}
		free_path(path);
	}

	apath = (char*)retrieve(
		callback_data,
		map_uri(plugin, "http://example.org/save-file"),
		&size, &type, &valflags);
	if (apath) {
		char* spath = map_path->absolute_path(map_path->handle, apath);
		FILE* sfile = fopen(spath, "r");
		if (!sfile) {
			fprintf(stderr, "error: Failed to open save file %s\n", spath);
		} else {
			fclose(sfile);
		}
		free_path(spath);
	} else {
		fprintf(stderr, "error: Failed to restore save file.\n");
	}

	return LV2_STATE_SUCCESS;
}

static const void*
extension_data(const char* uri)
{
	static const LV2_State_Interface state = { save, restore };
	if (!strcmp(uri, LV2_STATE__interface)) {
		return &state;
	}
	return NULL;
}

static const LV2_Descriptor descriptor = {
	TEST_URI,
	instantiate,
	connect_port,
	NULL, // activate,
	run,
	NULL, // deactivate,
	cleanup,
	extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor* lv2_descriptor(uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}
