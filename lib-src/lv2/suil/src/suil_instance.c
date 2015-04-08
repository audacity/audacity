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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "./suil_config.h"
#include "./suil_internal.h"

#define GTK2_UI_URI LV2_UI__GtkUI
#define QT4_UI_URI  LV2_UI__Qt4UI
#define X11_UI_URI  LV2_UI__X11UI
#define WIN_UI_URI  LV2_UI_PREFIX "WindowsUI"

SUIL_API
unsigned
suil_ui_supported(const char* container_type_uri,
                  const char* ui_type_uri)
{
	enum {
		SUIL_WRAPPING_UNSUPPORTED = 0,
		SUIL_WRAPPING_NATIVE      = 1,
		SUIL_WRAPPING_EMBEDDED    = 2
	};
	if (!strcmp(container_type_uri, ui_type_uri)) {
		return SUIL_WRAPPING_NATIVE;
	} else if ((!strcmp(container_type_uri, GTK2_UI_URI)
	            && !strcmp(ui_type_uri, QT4_UI_URI))
	           || (!strcmp(container_type_uri, QT4_UI_URI)
	               && !strcmp(ui_type_uri, GTK2_UI_URI))
	           || (!strcmp(container_type_uri, GTK2_UI_URI)
	               && !strcmp(ui_type_uri, X11_UI_URI))
	           || (!strcmp(container_type_uri, GTK2_UI_URI)
	               && !strcmp(ui_type_uri, WIN_UI_URI))
	           || (!strcmp(container_type_uri, QT4_UI_URI)
	               && !strcmp(ui_type_uri, X11_UI_URI))) {
		return SUIL_WRAPPING_EMBEDDED;
	} else {
		return SUIL_WRAPPING_UNSUPPORTED;
	}
}

static SuilWrapper*
open_wrapper(SuilHost*      host,
             const char*    container_type_uri,
             const char*    ui_type_uri,
             LV2_Feature*** features,
             unsigned       n_features)
{
	if (!container_type_uri || !strcmp(container_type_uri, ui_type_uri)) {
		return NULL;
	}

	const char* module_name = NULL;
	if (!strcmp(container_type_uri, QT4_UI_URI)
	    && !strcmp(ui_type_uri, GTK2_UI_URI)) {
		module_name = "suil_gtk2_in_qt4";
	} else if (!strcmp(container_type_uri, GTK2_UI_URI)
	           && !strcmp(ui_type_uri, QT4_UI_URI)) {
		module_name = "suil_qt4_in_gtk2";
	} else if (!strcmp(container_type_uri, GTK2_UI_URI)
	           && !strcmp(ui_type_uri, X11_UI_URI)) {
		module_name = "suil_x11_in_gtk2";
	} else if (!strcmp(container_type_uri, GTK2_UI_URI)
	           && !strcmp(ui_type_uri, WIN_UI_URI)) {
		module_name = "suil_win_in_gtk2";
	} else if (!strcmp(container_type_uri, QT4_UI_URI)
	           && !strcmp(ui_type_uri, X11_UI_URI)) {
		module_name = "suil_x11_in_qt4";
	}

	if (!module_name) {
		SUIL_ERRORF("Unable to wrap UI type <%s> as type <%s>\n",
		            ui_type_uri, container_type_uri);
		return NULL;
	}

	const char* const env_dir = getenv("SUIL_MODULE_DIR");
	const char* const mod_dir = env_dir ? env_dir : SUIL_MODULE_DIR;

	const size_t path_len = strlen(mod_dir)
		+ strlen(SUIL_DIR_SEP SUIL_MODULE_PREFIX SUIL_MODULE_EXT)
		+ strlen(module_name)
		+ 2;

	char* const path = (char*)calloc(path_len, 1);
	snprintf(path, path_len, "%s%s%s%s%s",
	         mod_dir, SUIL_DIR_SEP,
	         SUIL_MODULE_PREFIX, module_name, SUIL_MODULE_EXT);

	// Open wrap module
	dlerror();
	void* lib = dlopen(path, RTLD_NOW);
	if (!lib) {
		SUIL_ERRORF("Unable to open wrap module %s (%s)\n", path, dlerror());
		return NULL;
	}

	SuilWrapperNewFunc wrapper_new = (SuilWrapperNewFunc)suil_dlfunc(
		lib, "suil_wrapper_new");

	SuilWrapper* wrapper = wrapper_new
		? wrapper_new(host,
		              container_type_uri,
		              ui_type_uri,
		              features,
		              n_features)
		: NULL;

	if (!wrapper) {
		SUIL_ERRORF("Corrupt module %s\n", path);
		dlclose(lib);
		free(path);
		return NULL;
	}

	free(path);

	wrapper->lib = lib;

	return wrapper;
}

SUIL_API
SuilInstance*
suil_instance_new(SuilHost*                 host,
                  SuilController            controller,
                  const char*               container_type_uri,
                  const char*               plugin_uri,
                  const char*               ui_uri,
                  const char*               ui_type_uri,
                  const char*               ui_bundle_path,
                  const char*               ui_binary_path,
                  const LV2_Feature* const* features)
{
	// Open UI library
	dlerror();
	void* lib = dlopen(ui_binary_path, RTLD_NOW);
	if (!lib) {
		SUIL_ERRORF("Unable to open UI library %s (%s)\n",
		            ui_binary_path, dlerror());
		return NULL;
	}

	// Get discovery function
	LV2UI_DescriptorFunction df = (LV2UI_DescriptorFunction)
		suil_dlfunc(lib, "lv2ui_descriptor");
	if (!df) {
		SUIL_ERRORF("Broken LV2 UI %s (no lv2ui_descriptor symbol found)\n",
		            ui_binary_path);
		dlclose(lib);
		return NULL;
	}

	// Get UI descriptor
	const LV2UI_Descriptor* descriptor = NULL;
	for (uint32_t i = 0; true; ++i) {
		const LV2UI_Descriptor* ld = df(i);
		if (!ld) {
			break;
		} else if (!strcmp(ld->URI, ui_uri)) {
			descriptor = ld;
			break;
		}
	}
	if (!descriptor) {
		SUIL_ERRORF("Failed to find descriptor for <%s> in %s\n",
		            ui_uri, ui_binary_path);
		dlclose(lib);
		return NULL;
	}

	// Create SuilInstance
	SuilInstance* instance = (SuilInstance*)calloc(1, sizeof(SuilInstance));
	if (!instance) {
		SUIL_ERRORF("Failed to allocate memory for <%s> instance\n", ui_uri);
		dlclose(lib);
		return NULL;
	}

	instance->lib_handle = lib;
	instance->descriptor = descriptor;

	// Make UI features array
	instance->features = (LV2_Feature**)malloc(sizeof(LV2_Feature**));
	instance->features[0] = NULL;

	// Copy user provided features
	const LV2_Feature* const* fi         = features;
	unsigned                  n_features = 0;
	while (fi && *fi) {
		const LV2_Feature* f = *fi++;
		suil_add_feature(&instance->features, &n_features, f->URI, f->data);
	}

	// Add additional features implemented by SuilHost functions
	if (host->index_func) {
		instance->port_map.handle     = controller;
		instance->port_map.port_index = host->index_func;
		suil_add_feature(&instance->features, &n_features,
		                 LV2_UI__portMap, &instance->port_map);
	}
	if (host->subscribe_func && host->unsubscribe_func) {
		instance->port_subscribe.handle      = controller;
		instance->port_subscribe.subscribe   = host->subscribe_func;
		instance->port_subscribe.unsubscribe = host->unsubscribe_func;
		suil_add_feature(&instance->features, &n_features,
		                 LV2_UI__portSubscribe, &instance->port_subscribe);
	}
	if (host->touch_func) {
		instance->touch.handle = controller;
		instance->touch.touch  = host->touch_func;
		suil_add_feature(&instance->features, &n_features,
		                 LV2_UI__touch, &instance->touch);
	}

	// Open wrapper (this may add additional features)
	instance->wrapper = open_wrapper(
		host, container_type_uri, ui_type_uri, &instance->features, n_features);

	// Instantiate UI
	instance->handle = descriptor->instantiate(
		descriptor,
		plugin_uri,
		ui_bundle_path,
		host->write_func,
		controller,
		&instance->ui_widget,
		(const LV2_Feature* const*)instance->features);

	// Failed to instantiate UI
	if (!instance->handle) {
		SUIL_ERRORF("Failed to instantiate UI <%s> in %s\n",
		            ui_uri, ui_binary_path);
		suil_instance_free(instance);
		return NULL;
	}

	if (instance->wrapper) {
		if (instance->wrapper->wrap(instance->wrapper, instance)) {
			SUIL_ERRORF("Failed to wrap UI <%s> in type <%s>\n",
			            ui_uri, container_type_uri);
			suil_instance_free(instance);
			return NULL;
		}
	} else {
		instance->host_widget = instance->ui_widget;
	}

	return instance;
}

SUIL_API
void
suil_instance_free(SuilInstance* instance)
{
	if (instance) {
		for (unsigned i = 0; instance->features[i]; ++i) {
			free(instance->features[i]);
		}
		free(instance->features);

		// Call wrapper free function to destroy widgets and drop references
		if (instance->wrapper && instance->wrapper->free) {
			instance->wrapper->free(instance->wrapper);
		}

		// Call cleanup to destroy UI (if it still exists at this point)
		if (instance->handle) {
			instance->descriptor->cleanup(instance->handle);
		}

		// Close libraries and free everything
		if (instance->wrapper) {
			dlclose(instance->wrapper->lib);
			free(instance->wrapper);
		}
		dlclose(instance->lib_handle);
		free(instance);
	}
}

SUIL_API
SuilHandle
suil_instance_get_handle(SuilInstance* instance)
{
	return instance->handle;
}

SUIL_API
LV2UI_Widget
suil_instance_get_widget(SuilInstance* instance)
{
	return instance->host_widget;
}

SUIL_API
void
suil_instance_port_event(SuilInstance* instance,
                         uint32_t      port_index,
                         uint32_t      buffer_size,
                         uint32_t      format,
                         const void*   buffer)
{
	if (instance->descriptor->port_event) {
		instance->descriptor->port_event(instance->handle,
		                                 port_index,
		                                 buffer_size,
		                                 format,
		                                 buffer);
	}
}

SUIL_API
const void*
suil_instance_extension_data(SuilInstance* instance,
                             const char*   uri)
{
	if (instance->descriptor->extension_data) {
		return instance->descriptor->extension_data(uri);
	}
	return NULL;
}
