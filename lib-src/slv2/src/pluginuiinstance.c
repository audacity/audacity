/* SLV2
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 * Author: Lars Luthman
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <dlfcn.h>
#include <slv2/types.h>
#include <slv2/plugin.h>
#include <slv2/pluginui.h>
#include <slv2/pluginuiinstance.h>
#include <slv2/util.h>
#include "slv2_internal.h"


SLV2UIInstance
slv2_ui_instantiate(SLV2Plugin                     plugin,
                    SLV2UI                         ui,
                    LV2UI_Write_Function           write_function,
                    LV2UI_Controller               controller,
                    const LV2_Feature* const*      features)
{
	struct _SLV2UIInstance* result = NULL;
	
	bool local_features = (features == NULL);
	if (local_features) {
		features = malloc(sizeof(LV2_Feature));
		((LV2_Feature**)features)[0] = NULL;
	}
	
	const char* const lib_uri = slv2_value_as_string(slv2_ui_get_binary_uri(ui));
	const char* const lib_path = slv2_uri_to_path(lib_uri);
	
	if (!lib_path)
		return NULL;
	
	dlerror();
	void* lib = dlopen(lib_path, RTLD_NOW);
	if (!lib) {
		fprintf(stderr, "Unable to open UI library %s (%s)\n", lib_path, dlerror());
		return NULL;
	}
	
	LV2UI_DescriptorFunction df = dlsym(lib, "lv2ui_descriptor");
	
	if (!df) {
		fprintf(stderr, "Could not find symbol 'lv2ui_descriptor', "
				"%s is not a LV2 plugin UI.\n", lib_path);
		dlclose(lib);
		return NULL;
	} else {
		
		const char* bundle_path = slv2_uri_to_path(slv2_value_as_uri(slv2_ui_get_bundle_uri(ui)));
		
		for (uint32_t i=0; 1; ++i) {
			
			const LV2UI_Descriptor* ld = df(i);
				
			if (!ld) {
				fprintf(stderr, "Did not find UI %s in %s\n",
						slv2_value_as_uri(slv2_ui_get_uri(ui)), lib_path);
				dlclose(lib);
				break; // return NULL
			} else if (!strcmp(ld->URI, slv2_value_as_uri(slv2_ui_get_uri(ui)))) {
	
				assert(plugin->plugin_uri);

				printf("Found UI %s at index %u in:\n\t%s\n\n",
				       slv2_value_as_uri(plugin->plugin_uri), i, lib_path);

				assert(ld->instantiate);

				// Create SLV2UIInstance to return
				result = malloc(sizeof(struct _SLV2UIInstance));
				struct _SLV2UIInstanceImpl* impl = malloc(sizeof(struct _SLV2UIInstanceImpl));
				impl->lv2ui_descriptor = ld;
				impl->lv2ui_handle = ld->instantiate(ld, 
						slv2_value_as_uri(slv2_plugin_get_uri(plugin)),
						(char*)bundle_path, 
						write_function,
						controller,
						&impl->widget,
						features);
				impl->lib_handle = lib;
				result->pimpl = impl;
				break;
			}
		}
	}


	// Failed to instantiate
	if (result == NULL || result->pimpl->lv2ui_handle == NULL) {
		//printf("Failed to instantiate %s\n", plugin->plugin_uri);
		free(result);
		return NULL;
	}
	
	// Failed to create a widget, but still got a handle - this means that
	// the plugin is buggy
	if (result->pimpl->widget == NULL) {
		slv2_ui_instance_free(result);
		return NULL;
	}

	if (local_features)
		free((LV2_Feature**)features);

	return result;
}


void
slv2_ui_instance_free(SLV2UIInstance instance)
{
	if (instance == NULL)
		return;

	struct _SLV2UIInstance* i = (struct _SLV2UIInstance*)instance;
	i->pimpl->lv2ui_descriptor->cleanup(i->pimpl->lv2ui_handle);
	i->pimpl->lv2ui_descriptor = NULL;
	dlclose(i->pimpl->lib_handle);
	i->pimpl->lib_handle = NULL;
	free(i->pimpl);
	i->pimpl = NULL;
	free(i);
}


LV2UI_Widget
slv2_ui_instance_get_widget(SLV2UIInstance instance) {
	return instance->pimpl->widget;
}


const LV2UI_Descriptor*
slv2_ui_instance_get_descriptor(SLV2UIInstance instance) {
	return instance->pimpl->lv2ui_descriptor;
}


LV2UI_Handle
slv2_ui_instance_get_handle(SLV2UIInstance instance) {
	return instance->pimpl->lv2ui_handle;
}

