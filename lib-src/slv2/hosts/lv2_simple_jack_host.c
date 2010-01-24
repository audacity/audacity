/* SLV2 Simple Jack Host Example
 * Copyright (C) 2007 Dave Robillard <http://drobilla.net>
 *  
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation; either version 2 of the License, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
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
#include <slv2/slv2.h>
#include <jack/jack.h>


/** This program's data */
struct JackHost {
	jack_client_t* jack_client;   /**< Jack client */
	SLV2World      world;         /**< SLV2 "world" object */
	SLV2Plugin     plugin;        /**< Plugin "class" (actually just a few strings) */
	SLV2Instance   instance;      /**< Plugin "instance" (loaded shared lib) */
	uint32_t       num_ports;     /**< Size of the two following arrays: */
	jack_port_t**  jack_ports;    /**< For audio ports, otherwise NULL */
	float*         controls;      /**< For control ports, otherwise 0.0f */
	SLV2Value      input_class;   /**< Input port class (URI) */
	SLV2Value      control_class; /**< Control port class (URI) */
	SLV2Value      audio_class;   /**< Audio port class (URI) */
};


void die(const char* msg);
void create_port(struct JackHost* host, uint32_t port_index);
int  jack_process_cb(jack_nframes_t nframes, void* data);
void list_plugins(SLV2Plugins list);


int
main(int argc, char** argv)
{
	struct JackHost host;
	host.jack_client = NULL;
	host.num_ports   = 0;
	host.jack_ports  = NULL;
	host.controls    = NULL;
	
	/* Find all installed plugins */
	host.world = slv2_world_new();
	slv2_world_load_all(host.world);
	SLV2Plugins plugins = slv2_world_get_all_plugins(host.world);

	/* Set up the port classes this app supports */
	host.input_class = slv2_value_new_uri(host.world, SLV2_PORT_CLASS_INPUT);
	host.audio_class = slv2_value_new_uri(host.world, SLV2_PORT_CLASS_OUTPUT);
	/* Note that SLV2_PORT_CLASS_* are simply strings defined for convenience.
	 * host.control_class = slv2_value_new(host.world, SLV2_PORT_CLASS_CONTROL);
	 * is the same as: */
	host.control_class = slv2_value_new_uri(host.world,
			"http://lv2plug.in/ns/lv2core#ControlPort");

	/* Find the plugin to run */
	const char* plugin_uri_str = (argc == 2) ? argv[1] : NULL;
	
	if (!plugin_uri_str) {
		fprintf(stderr, "\nYou must specify a plugin URI to load.\n");
		fprintf(stderr, "\nKnown plugins:\n\n");
		list_plugins(plugins);
		slv2_world_free(host.world);
		return EXIT_FAILURE;
	}

	printf("URI:\t%s\n", plugin_uri_str);
	SLV2Value plugin_uri = slv2_value_new_uri(host.world, plugin_uri_str);
	host.plugin = slv2_plugins_get_by_uri(plugins, plugin_uri);
	slv2_value_free(plugin_uri);
	
	if (!host.plugin) {
		fprintf(stderr, "Failed to find plugin %s.\n", plugin_uri_str);
		slv2_world_free(host.world);
		return EXIT_FAILURE;
	}

	/* Get the plugin's name */
	SLV2Value name = slv2_plugin_get_name(host.plugin);
	const char* name_str = slv2_value_as_string(name);
	printf("Plugin Name:\t%s\n", slv2_value_as_string(name));

	/* Truncate plugin name to suit JACK (if necessary) */
	char* jack_name = NULL;
	if (strlen(name_str) >= (unsigned)jack_client_name_size() - 1) {
		jack_name = calloc(jack_client_name_size(), sizeof(char));
		strncpy(jack_name, name_str, jack_client_name_size() - 1);
	} else {
		jack_name = strdup(name_str);
	}

	/* Connect to JACK */
	printf("JACK Name:\t%s\n", jack_name);
	host.jack_client = jack_client_open(jack_name, JackNullOption, NULL);
	
	free(jack_name);
	slv2_value_free(name);

	if (!host.jack_client)
		die("Failed to connect to JACK.");
	else
		printf("Connected to JACK.\n");
	
	/* Instantiate the plugin */
	host.instance = slv2_plugin_instantiate(
		host.plugin, jack_get_sample_rate(host.jack_client), NULL);
	if (!host.instance)
		die("Failed to instantiate plugin.\n");
	else
		printf("Succesfully instantiated plugin.\n");

	jack_set_process_callback(host.jack_client, &jack_process_cb, (void*)(&host));
	
	/* Create ports */
	host.num_ports  = slv2_plugin_get_num_ports(host.plugin);
	host.jack_ports = calloc((size_t)host.num_ports, sizeof(jack_port_t*));
	host.controls   = calloc((size_t)host.num_ports, sizeof(float*));
	
	for (uint32_t i=0; i < host.num_ports; ++i)
		create_port(&host, i);
	
	/* Activate plugin and JACK */
	slv2_instance_activate(host.instance);
	jack_activate(host.jack_client);
	
	/* Run */
	printf("Press enter to quit: ");
	getc(stdin);
	printf("\n");
	
	/* Deactivate JACK */
	jack_deactivate(host.jack_client);
	
	printf("Shutting down JACK.\n");
	for (unsigned long i=0; i < host.num_ports; ++i) {
		if (host.jack_ports[i] != NULL) {
			jack_port_unregister(host.jack_client, host.jack_ports[i]);
			host.jack_ports[i] = NULL;
		}
	}
	jack_client_close(host.jack_client);
	
	/* Deactivate plugin */
	slv2_instance_deactivate(host.instance);
	slv2_instance_free(host.instance);

	/* Clean up */
	slv2_value_free(host.input_class);
	slv2_value_free(host.audio_class);
	slv2_value_free(host.control_class);
	slv2_plugins_free(host.world, plugins);
	slv2_world_free(host.world);

	return 0;
}


/** Abort and exit on error */
void
die(const char* msg)
{
	fprintf(stderr, "%s\n", msg);
	exit(EXIT_FAILURE);
}


/** Creates a port and connects the plugin instance to it's data location.
 *
 * For audio ports, creates a jack port and connects plugin port to buffer.
 *
 * For control ports, sets controls array to default value and connects plugin
 * port to that element.
 */
void
create_port(struct JackHost* host,
            uint32_t         index)
{
	SLV2Port port = slv2_plugin_get_port_by_index(host->plugin, index);

	/* Get the port symbol (label) for console printing */
	SLV2Value symbol       = slv2_port_get_symbol(host->plugin, port);
	const char* symbol_str = slv2_value_as_string(symbol);
	
	/* Initialize the port array elements */
	host->jack_ports[index] = NULL;
	host->controls[index]   = 0.0f;
	
	/* Connect control ports to controls array */
	if (slv2_port_is_a(host->plugin, port, host->control_class)) {

		/* Set default control values for inputs */
		if (slv2_port_is_a(host->plugin, port, host->input_class)) {
			SLV2Value def;
			slv2_port_get_range(host->plugin, port, &def, NULL, NULL);
			host->controls[index] = slv2_value_as_float(def);
			printf("Set %s to %f\n", symbol_str, host->controls[index]);
			slv2_value_free(def);
		}
		
		slv2_instance_connect_port(host->instance, index, &host->controls[index]);

	} else if (slv2_port_is_a(host->plugin, port, host->audio_class)) {

		host->jack_ports[index] = jack_port_register(host->jack_client,
			symbol_str, JACK_DEFAULT_AUDIO_TYPE,
			slv2_port_is_a(host->plugin, port, host->input_class)
				? JackPortIsInput : JackPortIsOutput,
			0);

	} else {
		// Simple examples don't have to be robust :)
		die("ERROR: Unknown port type, aborting messily!\n");
	}
}


/** Jack process callback. */
int
jack_process_cb(jack_nframes_t nframes, void* data)
{
	struct JackHost* host = (struct JackHost*)data;

	/* Connect plugin ports directly to JACK buffers */
	for (uint32_t i=0; i < host->num_ports; ++i)
		if (host->jack_ports[i] != NULL)
			slv2_instance_connect_port(host->instance, i,
				jack_port_get_buffer(host->jack_ports[i], nframes));
	
	/* Run plugin for this cycle */
	slv2_instance_run(host->instance, nframes);

	return 0;
}


void
list_plugins(SLV2Plugins list)
{
	for (unsigned i=0; i < slv2_plugins_size(list); ++i) {
		SLV2Plugin p = slv2_plugins_get_at(list, i);
		printf("%s\n", slv2_value_as_uri(slv2_plugin_get_uri(p)));
	}
}
