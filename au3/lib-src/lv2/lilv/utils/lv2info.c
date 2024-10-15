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

#include "lilv_config.h"

#include "lilv/lilv.h"
#include "lv2/core/lv2.h"
#include "lv2/event/event.h"
#include "lv2/port-groups/port-groups.h"
#include "lv2/presets/presets.h"

#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

LilvNode* applies_to_pred     = NULL;
LilvNode* control_class       = NULL;
LilvNode* event_class         = NULL;
LilvNode* group_pred          = NULL;
LilvNode* label_pred          = NULL;
LilvNode* preset_class        = NULL;
LilvNode* designation_pred    = NULL;
LilvNode* supports_event_pred = NULL;

static void
print_port(const LilvPlugin* p,
           uint32_t          index,
           float*            mins,
           float*            maxes,
           float*            defaults)
{
	const LilvPort* port = lilv_plugin_get_port_by_index(p, index);

	printf("\n\tPort %d:\n", index);

	if (!port) {
		printf("\t\tERROR: Illegal/nonexistent port\n");
		return;
	}

	bool first = true;

	const LilvNodes* classes = lilv_port_get_classes(p, port);
	printf("\t\tType:        ");
	LILV_FOREACH(nodes, i, classes) {
		const LilvNode* value = lilv_nodes_get(classes, i);
		if (!first) {
			printf("\n\t\t             ");
		}
		printf("%s", lilv_node_as_uri(value));
		first = false;
	}

	if (lilv_port_is_a(p, port, event_class)) {
		LilvNodes* supported = lilv_port_get_value(
			p, port, supports_event_pred);
		if (lilv_nodes_size(supported) > 0) {
			printf("\n\t\tSupported events:\n");
			LILV_FOREACH(nodes, i, supported) {
				const LilvNode* value = lilv_nodes_get(supported, i);
				printf("\t\t\t%s\n", lilv_node_as_uri(value));
			}
		}
		lilv_nodes_free(supported);
	}

	LilvScalePoints* points = lilv_port_get_scale_points(p, port);
	if (points) {
		printf("\n\t\tScale Points:\n");
	}
	LILV_FOREACH(scale_points, i, points) {
		const LilvScalePoint* point = lilv_scale_points_get(points, i);
		printf("\t\t\t%s = \"%s\"\n",
				lilv_node_as_string(lilv_scale_point_get_value(point)),
				lilv_node_as_string(lilv_scale_point_get_label(point)));
	}
	lilv_scale_points_free(points);

	const LilvNode* sym = lilv_port_get_symbol(p, port);
	printf("\n\t\tSymbol:      %s\n", lilv_node_as_string(sym));

	LilvNode* name = lilv_port_get_name(p, port);
	printf("\t\tName:        %s\n", lilv_node_as_string(name));
	lilv_node_free(name);

	LilvNodes* groups = lilv_port_get_value(p, port, group_pred);
	if (lilv_nodes_size(groups) > 0) {
		printf("\t\tGroup:       %s\n",
		       lilv_node_as_string(lilv_nodes_get_first(groups)));
	}
	lilv_nodes_free(groups);

	LilvNodes* designations = lilv_port_get_value(p, port, designation_pred);
	if (lilv_nodes_size(designations) > 0) {
		printf("\t\tDesignation: %s\n",
		       lilv_node_as_string(lilv_nodes_get_first(designations)));
	}
	lilv_nodes_free(designations);

	if (lilv_port_is_a(p, port, control_class)) {
		if (!isnan(mins[index])) {
			printf("\t\tMinimum:     %f\n", mins[index]);
		}
		if (!isnan(maxes[index])) {
			printf("\t\tMaximum:     %f\n", maxes[index]);
		}
		if (!isnan(defaults[index])) {
			printf("\t\tDefault:     %f\n", defaults[index]);
		}
	}

	LilvNodes* properties = lilv_port_get_properties(p, port);
	if (lilv_nodes_size(properties) > 0) {
		printf("\t\tProperties:  ");
	}
	first = true;
	LILV_FOREACH(nodes, i, properties) {
		if (!first) {
			printf("\t\t             ");
		}
		printf("%s\n", lilv_node_as_uri(lilv_nodes_get(properties, i)));
		first = false;
	}
	if (lilv_nodes_size(properties) > 0) {
		printf("\n");
	}
	lilv_nodes_free(properties);
}

static void
print_plugin(LilvWorld*        world,
             const LilvPlugin* p)
{
	LilvNode* val = NULL;

	printf("%s\n\n", lilv_node_as_uri(lilv_plugin_get_uri(p)));

	val = lilv_plugin_get_name(p);
	if (val) {
		printf("\tName:              %s\n", lilv_node_as_string(val));
		lilv_node_free(val);
	}

	const LilvPluginClass* pclass      = lilv_plugin_get_class(p);
	const LilvNode*       class_label = lilv_plugin_class_get_label(pclass);
	if (class_label) {
		printf("\tClass:             %s\n", lilv_node_as_string(class_label));
	}

	val = lilv_plugin_get_author_name(p);
	if (val) {
		printf("\tAuthor:            %s\n", lilv_node_as_string(val));
		lilv_node_free(val);
	}

	val = lilv_plugin_get_author_email(p);
	if (val) {
		printf("\tAuthor Email:      %s\n", lilv_node_as_uri(val));
		lilv_node_free(val);
	}

	val = lilv_plugin_get_author_homepage(p);
	if (val) {
		printf("\tAuthor Homepage:   %s\n", lilv_node_as_uri(val));
		lilv_node_free(val);
	}

	if (lilv_plugin_has_latency(p)) {
		uint32_t latency_port = lilv_plugin_get_latency_port_index(p);
		printf("\tHas latency:       yes, reported by port %d\n", latency_port);
	} else {
		printf("\tHas latency:       no\n");
	}

	printf("\tBundle:            %s\n",
	       lilv_node_as_uri(lilv_plugin_get_bundle_uri(p)));

	const LilvNode* binary_uri = lilv_plugin_get_library_uri(p);
	if (binary_uri) {
		printf("\tBinary:            %s\n",
		       lilv_node_as_uri(lilv_plugin_get_library_uri(p)));
	}

	LilvUIs* uis = lilv_plugin_get_uis(p);
	if (lilv_nodes_size(uis) > 0) {
		printf("\tUIs:\n");
		LILV_FOREACH(uis, i, uis) {
			const LilvUI* ui = lilv_uis_get(uis, i);
			printf("\t\t%s\n", lilv_node_as_uri(lilv_ui_get_uri(ui)));

			const char* binary = lilv_node_as_uri(lilv_ui_get_binary_uri(ui));

			const LilvNodes* types = lilv_ui_get_classes(ui);
			LILV_FOREACH(nodes, t, types) {
				printf("\t\t\tClass:  %s\n",
				       lilv_node_as_uri(lilv_nodes_get(types, t)));
			}

			if (binary) {
				printf("\t\t\tBinary: %s\n", binary);
			}

			printf("\t\t\tBundle: %s\n",
			       lilv_node_as_uri(lilv_ui_get_bundle_uri(ui)));
		}
	}
	lilv_uis_free(uis);

	printf("\tData URIs:         ");
	const LilvNodes* data_uris = lilv_plugin_get_data_uris(p);
	bool first = true;
	LILV_FOREACH(nodes, i, data_uris) {
		if (!first) {
			printf("\n\t                   ");
		}
		printf("%s", lilv_node_as_uri(lilv_nodes_get(data_uris, i)));
		first = false;
	}
	printf("\n");

	/* Required Features */

	LilvNodes* features = lilv_plugin_get_required_features(p);
	if (features) {
		printf("\tRequired Features: ");
	}
	first = true;
	LILV_FOREACH(nodes, i, features) {
		if (!first) {
			printf("\n\t                   ");
		}
		printf("%s", lilv_node_as_uri(lilv_nodes_get(features, i)));
		first = false;
	}
	if (features) {
		printf("\n");
	}
	lilv_nodes_free(features);

	/* Optional Features */

	features = lilv_plugin_get_optional_features(p);
	if (features) {
		printf("\tOptional Features: ");
	}
	first = true;
	LILV_FOREACH(nodes, i, features) {
		if (!first) {
			printf("\n\t                   ");
		}
		printf("%s", lilv_node_as_uri(lilv_nodes_get(features, i)));
		first = false;
	}
	if (features) {
		printf("\n");
	}
	lilv_nodes_free(features);

	/* Extension Data */

	LilvNodes* data = lilv_plugin_get_extension_data(p);
	if (data) {
		printf("\tExtension Data:    ");
	}
	first = true;
	LILV_FOREACH(nodes, i, data) {
		if (!first) {
			printf("\n\t                   ");
		}
		printf("%s", lilv_node_as_uri(lilv_nodes_get(data, i)));
		first = false;
	}
	if (data) {
		printf("\n");
	}
	lilv_nodes_free(data);

	/* Presets */

	LilvNodes* presets = lilv_plugin_get_related(p, preset_class);
	if (presets) {
		printf("\tPresets: \n");
	}
	LILV_FOREACH(nodes, i, presets) {
		const LilvNode* preset = lilv_nodes_get(presets, i);
		lilv_world_load_resource(world, preset);
		LilvNodes* titles = lilv_world_find_nodes(
			world, preset, label_pred, NULL);
		if (titles) {
			const LilvNode* title = lilv_nodes_get_first(titles);
			printf("\t         %s\n", lilv_node_as_string(title));
			lilv_nodes_free(titles);
		} else {
			fprintf(stderr, "Preset <%s> has no rdfs:label\n",
			        lilv_node_as_string(lilv_nodes_get(presets, i)));
		}
	}
	lilv_nodes_free(presets);

	/* Ports */

	const uint32_t num_ports = lilv_plugin_get_num_ports(p);
	float* mins     = (float*)calloc(num_ports, sizeof(float));
	float* maxes    = (float*)calloc(num_ports, sizeof(float));
	float* defaults = (float*)calloc(num_ports, sizeof(float));
	lilv_plugin_get_port_ranges_float(p, mins, maxes, defaults);

	for (uint32_t i = 0; i < num_ports; ++i) {
		print_port(p, i, mins, maxes, defaults);
	}

	free(mins);
	free(maxes);
	free(defaults);
}

static void
print_version(void)
{
	printf(
		"lv2info (lilv) " LILV_VERSION "\n"
		"Copyright 2007-2019 David Robillard <http://drobilla.net>\n"
		"License: <http://www.opensource.org/licenses/isc-license>\n"
		"This is free software: you are free to change and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.\n");
}

static void
print_usage(void)
{
	printf(
		"Usage: lv2info [OPTION]... PLUGIN_URI\n"
		"Print information about an installed LV2 plugin.\n\n"
		"  -p FILE      Write Turtle description of plugin to FILE\n"
		"  -m FILE      Add record of plugin to manifest FILE\n"
		"  --help       Display this help and exit\n"
		"  --version    Display version information and exit\n\n"
		"For -p and -m, Turtle files are appended to (not overwritten),\n"
		"and @prefix directives are only written if the file was empty.\n"
		"This allows several plugins to be added to a single file.\n");
}

int
main(int argc, char** argv)
{
	if (argc == 1) {
		print_usage();
		return 1;
	}

	const char* plugin_file   = NULL;
	const char* manifest_file = NULL;
	const char* plugin_uri    = NULL;
	for (int i = 1; i < argc; ++i) {
		if (!strcmp(argv[i], "--version")) {
			print_version();
			return 0;
		} else if (!strcmp(argv[i], "--help")) {
			print_usage();
			return 0;
		} else if (!strcmp(argv[i], "-p")) {
			plugin_file = argv[++i];
		} else if (!strcmp(argv[i], "-m")) {
			manifest_file = argv[++i];
		} else if (argv[i][0] == '-') {
			print_usage();
			return 1;
		} else if (i == argc - 1) {
			plugin_uri = argv[i];
		}
	}

	int ret = 0;

	LilvWorld* world = lilv_world_new();
	lilv_world_load_all(world);

	LilvNode* uri = lilv_new_uri(world, plugin_uri);
	if (!uri) {
		fprintf(stderr, "Invalid plugin URI\n");
		lilv_world_free(world);
		return 1;
	}

	applies_to_pred     = lilv_new_uri(world, LV2_CORE__appliesTo);
	control_class       = lilv_new_uri(world, LILV_URI_CONTROL_PORT);
	event_class         = lilv_new_uri(world, LILV_URI_EVENT_PORT);
	group_pred          = lilv_new_uri(world, LV2_PORT_GROUPS__group);
	label_pred          = lilv_new_uri(world, LILV_NS_RDFS "label");
	preset_class        = lilv_new_uri(world, LV2_PRESETS__Preset);
	designation_pred    = lilv_new_uri(world, LV2_CORE__designation);
	supports_event_pred = lilv_new_uri(world, LV2_EVENT__supportsEvent);

	const LilvPlugins* plugins = lilv_world_get_all_plugins(world);
	const LilvPlugin*  p       = lilv_plugins_get_by_uri(plugins, uri);

	if (p && plugin_file) {
		LilvNode* base = lilv_new_file_uri(world, NULL, plugin_file);

		FILE* plugin_fd = fopen(plugin_file, "a");
		if (plugin_fd) {
			lilv_plugin_write_description(world, p, base, plugin_fd);
			fclose(plugin_fd);
		} else {
			fprintf(stderr, "error: Failed to open %s\n", plugin_file);
		}

		if (manifest_file) {
			FILE* manifest_fd = fopen(manifest_file, "a");
			if (manifest_fd) {
				lilv_plugin_write_manifest_entry(
					world, p, base, manifest_fd, plugin_file);
				fclose(manifest_fd);
			} else {
				fprintf(stderr, "error: Failed to open %s\n", manifest_file);
			}
		}
		lilv_node_free(base);
	} else if (p) {
		print_plugin(world, p);
	} else {
		fprintf(stderr, "Plugin not found.\n");
	}

	ret = (p != NULL ? 0 : -1);

	lilv_node_free(uri);

	lilv_node_free(supports_event_pred);
	lilv_node_free(designation_pred);
	lilv_node_free(preset_class);
	lilv_node_free(label_pred);
	lilv_node_free(group_pred);
	lilv_node_free(event_class);
	lilv_node_free(control_class);
	lilv_node_free(applies_to_pred);

	lilv_world_free(world);
	return ret;
}

