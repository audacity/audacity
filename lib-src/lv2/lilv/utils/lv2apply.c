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

#include "lilv/lilv.h"

#include "lv2/core/lv2.h"

#include <math.h>
#include <sndfile.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/** Control port value set from the command line */
typedef struct Param {
	const char* sym;    ///< Port symbol
	float       value;  ///< Control value
} Param;

/** Port type (only float ports are supported) */
typedef enum {
	TYPE_CONTROL,
	TYPE_AUDIO
} PortType;

/** Runtime port information */
typedef struct {
	const LilvPort* lilv_port;  ///< Port description
	PortType        type;       ///< Datatype
	uint32_t        index;      ///< Port index
	float           value;      ///< Control value (if applicable)
	bool            is_input;   ///< True iff an input port
	bool            optional;   ///< True iff connection optional
} Port;

/** Application state */
typedef struct {
	LilvWorld*        world;
	const LilvPlugin* plugin;
	LilvInstance*     instance;
	const char*       in_path;
	const char*       out_path;
	SNDFILE*          in_file;
	SNDFILE*          out_file;
	unsigned          n_params;
	Param*            params;
	unsigned          n_ports;
	unsigned          n_audio_in;
	unsigned          n_audio_out;
	Port*             ports;
} LV2Apply;

static int fatal(LV2Apply* self, int status, const char* fmt, ...);

/** Open a sound file with error handling. */
static SNDFILE*
sopen(LV2Apply* self, const char* path, int mode, SF_INFO* fmt)
{
	SNDFILE*  file = sf_open(path, mode, fmt);
	const int st   = sf_error(file);
	if (st) {
		fatal(self, 1, "Failed to open %s (%s)\n", path, sf_error_number(st));
		return NULL;
	}
	return file;
}

/** Close a sound file with error handling. */
static void
sclose(const char* path, SNDFILE* file)
{
	int st;
	if (file && (st = sf_close(file))) {
		fatal(NULL, 1, "Failed to close %s (%s)\n", path, sf_error_number(st));
	}
}

/**
   Read a single frame from a file into an interleaved buffer.

   If more channels are required than are available in the file, the remaining
   channels are distributed in a round-robin fashion (LRLRL).
*/
static bool
sread(SNDFILE* file, unsigned file_chans, float* buf, unsigned buf_chans)
{
	const sf_count_t n_read = sf_readf_float(file, buf, 1);
	for (unsigned i = file_chans - 1; i < buf_chans; ++i) {
		buf[i] = buf[i % file_chans];
	}
	return n_read == 1;
}

/** Clean up all resources. */
static int
cleanup(int status, LV2Apply* self)
{
	sclose(self->in_path, self->in_file);
	sclose(self->out_path, self->out_file);
	lilv_instance_free(self->instance);
	lilv_world_free(self->world);
	free(self->ports);
	free(self->params);
	return status;
}

/** Print a fatal error and clean up for exit. */
static int
fatal(LV2Apply* self, int status, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);
	return self ? cleanup(status, self) : status;
}

/**
   Create port structures from data (via create_port()) for all ports.
*/
static int
create_ports(LV2Apply* self)
{
	LilvWorld*     world   = self->world;
	const uint32_t n_ports = lilv_plugin_get_num_ports(self->plugin);

	self->n_ports = n_ports;
	self->ports   = (Port*)calloc(self->n_ports, sizeof(Port));

	/* Get default values for all ports */
	float* values = (float*)calloc(n_ports, sizeof(float));
	lilv_plugin_get_port_ranges_float(self->plugin, NULL, NULL, values);

	LilvNode* lv2_InputPort          = lilv_new_uri(world, LV2_CORE__InputPort);
	LilvNode* lv2_OutputPort         = lilv_new_uri(world, LV2_CORE__OutputPort);
	LilvNode* lv2_AudioPort          = lilv_new_uri(world, LV2_CORE__AudioPort);
	LilvNode* lv2_ControlPort        = lilv_new_uri(world, LV2_CORE__ControlPort);
	LilvNode* lv2_connectionOptional = lilv_new_uri(world, LV2_CORE__connectionOptional);

	for (uint32_t i = 0; i < n_ports; ++i) {
		Port*           port  = &self->ports[i];
		const LilvPort* lport = lilv_plugin_get_port_by_index(self->plugin, i);

		port->lilv_port = lport;
		port->index     = i;
		port->value     = isnan(values[i]) ? 0.0f : values[i];
		port->optional  = lilv_port_has_property(
			self->plugin, lport, lv2_connectionOptional);

		/* Check if port is an input or output */
		if (lilv_port_is_a(self->plugin, lport, lv2_InputPort)) {
			port->is_input = true;
		} else if (!lilv_port_is_a(self->plugin, lport, lv2_OutputPort) &&
		           !port->optional) {
			return fatal(self, 1, "Port %d is neither input nor output\n", i);
		}

		/* Check if port is an audio or control port */
		if (lilv_port_is_a(self->plugin, lport, lv2_ControlPort)) {
			port->type = TYPE_CONTROL;
		} else if (lilv_port_is_a(self->plugin, lport, lv2_AudioPort)) {
			port->type = TYPE_AUDIO;
			if (port->is_input) {
				++self->n_audio_in;
			} else {
				++self->n_audio_out;
			}
		} else if (!port->optional) {
			return fatal(self, 1, "Port %d has unsupported type\n", i);
		}
	}

	lilv_node_free(lv2_connectionOptional);
	lilv_node_free(lv2_ControlPort);
	lilv_node_free(lv2_AudioPort);
	lilv_node_free(lv2_OutputPort);
	lilv_node_free(lv2_InputPort);
	free(values);

	return 0;
}

static void
print_version(void)
{
	printf(
		"lv2apply (lilv) " LILV_VERSION "\n"
		"Copyright 2007-2019 David Robillard <http://drobilla.net>\n"
		"License: <http://www.opensource.org/licenses/isc-license>\n"
		"This is free software: you are free to change and redistribute it.\n"
		"There is NO WARRANTY, to the extent permitted by law.\n");
}

static int
print_usage(int status)
{
	fprintf(status ? stderr : stdout,
	        "Usage: lv2apply [OPTION]... PLUGIN_URI\n"
	        "Apply an LV2 plugin to an audio file.\n\n"
	        "  -i IN_FILE   Input file\n"
	        "  -o OUT_FILE  Output file\n"
	        "  -c SYM VAL   Control value\n"
	        "  --help       Display this help and exit\n"
	        "  --version    Display version information and exit\n");
	return status;
}

int
main(int argc, char** argv)
{
	LV2Apply self = {
		NULL, NULL, NULL, NULL, NULL, NULL, NULL, 0, NULL, 0, 0, 0, NULL
	};

	/* Parse command line arguments */
	const char* plugin_uri = NULL;
	for (int i = 1; i < argc; ++i) {
		if (!strcmp(argv[i], "--version")) {
			free(self.params);
			print_version();
			return 0;
		} else if (!strcmp(argv[i], "--help")) {
			free(self.params);
			return print_usage(0);
		} else if (!strcmp(argv[i], "-i")) {
			self.in_path = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			self.out_path = argv[++i];
		} else if (!strcmp(argv[i], "-c")) {
			if (argc < i + 3) {
				return fatal(&self, 1, "Missing argument for -c\n");
			}
			self.params = (Param*)realloc(self.params,
			                              ++self.n_params * sizeof(Param));
			self.params[self.n_params - 1].sym   = argv[++i];
			self.params[self.n_params - 1].value = atof(argv[++i]);
		} else if (argv[i][0] == '-') {
			free(self.params);
			return print_usage(1);
		} else if (i == argc - 1) {
			plugin_uri = argv[i];
		}
	}

	/* Check that required arguments are given */
	if (!self.in_path || !self.out_path || !plugin_uri) {
		free(self.params);
		return print_usage(1);
	}

	/* Create world and plugin URI */
	self.world = lilv_world_new();
	LilvNode* uri = lilv_new_uri(self.world, plugin_uri);
	if (!uri) {
		return fatal(&self, 2, "Invalid plugin URI <%s>\n", plugin_uri);
	}

	/* Discover world */
	lilv_world_load_all(self.world);

	/* Get plugin */
	const LilvPlugins* plugins = lilv_world_get_all_plugins(self.world);
	const LilvPlugin*  plugin  = lilv_plugins_get_by_uri(plugins, uri);
	lilv_node_free(uri);
	if (!(self.plugin = plugin)) {
		return fatal(&self, 3, "Plugin <%s> not found\n", plugin_uri);
	}

	/* Open input file */
	SF_INFO in_fmt = { 0, 0, 0, 0, 0, 0 };
	if (!(self.in_file = sopen(&self, self.in_path, SFM_READ, &in_fmt))) {
		return 4;
	}

	/* Create port structures */
	if (create_ports(&self)) {
		return 5;
	}

	if (self.n_audio_in == 0 ||
	    (in_fmt.channels != (int)self.n_audio_in && in_fmt.channels != 1)) {
		return fatal(&self, 6, "Unable to map %d inputs to %d ports\n",
		             in_fmt.channels, self.n_audio_in);
	}

	/* Set control values */
	for (unsigned i = 0; i < self.n_params; ++i) {
		const Param*    param = &self.params[i];
		LilvNode*       sym   = lilv_new_string(self.world, param->sym);
		const LilvPort* port  = lilv_plugin_get_port_by_symbol(plugin, sym);
		lilv_node_free(sym);
		if (!port) {
			return fatal(&self, 7, "Unknown port `%s'\n", param->sym);
		}

		self.ports[lilv_port_get_index(plugin, port)].value = param->value;
	}

	/* Open output file */
	SF_INFO out_fmt = in_fmt;
	out_fmt.channels = self.n_audio_out;
	if (!(self.out_file = sopen(&self, self.out_path, SFM_WRITE, &out_fmt))) {
		return 8;
	}

	/* Instantiate plugin and connect ports */
	const uint32_t n_ports = lilv_plugin_get_num_ports(plugin);
	float          in_buf[self.n_audio_in];
	float          out_buf[self.n_audio_out];
	self.instance = lilv_plugin_instantiate(
		self.plugin, in_fmt.samplerate, NULL);
	for (uint32_t p = 0, i = 0, o = 0; p < n_ports; ++p) {
		if (self.ports[p].type == TYPE_CONTROL) {
			lilv_instance_connect_port(self.instance, p, &self.ports[p].value);
		} else if (self.ports[p].type == TYPE_AUDIO) {
			if (self.ports[p].is_input) {
				lilv_instance_connect_port(self.instance, p, in_buf + i++);
			} else {
				lilv_instance_connect_port(self.instance, p, out_buf + o++);
			}
		} else {
			lilv_instance_connect_port(self.instance, p, NULL);
		}
	}

	/* Ports are now connected to buffers in interleaved format, so we can run
	   a single frame at a time and avoid having to interleave buffers to
	   read/write from/to sndfile. */

	lilv_instance_activate(self.instance);
	while (sread(self.in_file, in_fmt.channels, in_buf, self.n_audio_in)) {
		lilv_instance_run(self.instance, 1);
		if (sf_writef_float(self.out_file, out_buf, 1) != 1) {
			return fatal(&self, 9, "Failed to write to output file\n");
		}
	}
	lilv_instance_deactivate(self.instance);

	return cleanup(0, &self);
}
