/*
  Copyright 2012 David Robillard <http://drobilla.net>

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

#include "lilv_internal.h"

LilvLib*
lilv_lib_open(LilvWorld*               world,
              const LilvNode*          uri,
              const char*              bundle_path,
              const LV2_Feature*const* features)
{
	ZixTreeIter*            i   = NULL;
	const struct LilvHeader key = { world, (LilvNode*)uri };
	if (!zix_tree_find(world->libs, &key, &i)) {
		LilvLib* llib = (LilvLib*)zix_tree_get(i);
		++llib->refs;
		return llib;
	}

	const char* const lib_uri  = lilv_node_as_uri(uri);
	const char* const lib_path = lilv_uri_to_path(lib_uri);
	if (!lib_path) {
		return NULL;
	}

	dlerror();
	void* lib = dlopen(lib_path, RTLD_NOW);
	if (!lib) {
		LILV_ERRORF("Failed to open library %s (%s)\n", lib_path, dlerror());
		return NULL;
	}

	LV2_Descriptor_Function df = (LV2_Descriptor_Function)
		lilv_dlfunc(lib, "lv2_descriptor");

#ifdef LILV_NEW_LV2
	LV2_Lib_Descriptor_Function ldf = (LV2_Lib_Descriptor_Function)
		lilv_dlfunc(lib, "lv2_lib_descriptor");

	const LV2_Lib_Descriptor* desc = NULL;
	if (ldf) {
		desc = ldf(bundle_path, features);
		if (!desc) {
			LILV_ERRORF("Call to `lv2_lib_descriptor' in %s failed\n", lib_path);
			return NULL;
		}
	} else
#endif
	if (!df) {
		LILV_ERRORF("No `lv2_descriptor' or `lv2_lib_descriptor' in %s\n",
		            lib_path);
		dlclose(lib);
		return NULL;
	}

	LilvLib* llib = (LilvLib*)malloc(sizeof(LilvLib));
	llib->world          = world;
	llib->uri            = lilv_node_duplicate(uri);
	llib->lib            = lib;
	llib->lv2_descriptor = df;
#ifdef LILV_NEW_LV2
	llib->desc           = desc;
#endif
	llib->refs           = 1;

	zix_tree_insert(world->libs, llib, NULL);
	return llib;
}

const LV2_Descriptor*
lilv_lib_get_plugin(LilvLib* lib, uint32_t index)
{
	if (lib->lv2_descriptor) {
		return lib->lv2_descriptor(index);
	}
#ifdef LILV_NEW_LV2
	if (lib->desc) {
		return lib->desc->get_plugin(lib->desc->handle, index);
	}
#endif
	return NULL;
}

void
lilv_lib_close(LilvLib* lib)
{
	if (--lib->refs == 0) {
		dlclose(lib->lib);

		ZixTreeIter* i = NULL;
		if (lib->world->libs && !zix_tree_find(lib->world->libs, lib, &i)) {
			zix_tree_remove(lib->world->libs, i);
		}

		lilv_node_free(lib->uri);
		free(lib);
	}
}
