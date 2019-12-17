/*
  Copyright 2011-2016 David Robillard <http://drobilla.net>

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
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#    include <windows.h>
#endif

#include "serd/serd.h"
#include "sord/sord.h"
#include "sord_config.h"

#define SORDI_ERROR(msg)       fprintf(stderr, "sordi: " msg);
#define SORDI_ERRORF(fmt, ...) fprintf(stderr, "sordi: " fmt, __VA_ARGS__);

typedef struct {
	SerdWriter* writer;
	SerdEnv*    env;
	SerdNode    base_uri_node;
	SerdURI     base_uri;
	SordModel*  sord;
} State;

static int
print_version(void)
{
	printf("sordi " SORD_VERSION " <http://drobilla.net/software/sord>\n");
	printf("Copyright 2011-2016 David Robillard <http://drobilla.net>.\n"
	       "License: <http://www.opensource.org/licenses/isc>\n"
	       "This is free software; you are free to change and redistribute it."
	       "\nThere is NO WARRANTY, to the extent permitted by law.\n");
	return 0;
}

static int
print_usage(const char* name, bool error)
{
	FILE* const os = error ? stderr : stdout;
	fprintf(os, "%s", error ? "\n" : "");
	fprintf(os, "Usage: %s [OPTION]... INPUT [BASE_URI]\n", name);
	fprintf(os, "Load and re-serialise RDF data.\n");
	fprintf(os, "Use - for INPUT to read from standard input.\n\n");
	fprintf(os, "  -h           Display this help and exit\n");
	fprintf(os, "  -i SYNTAX    Input syntax (`turtle' or `ntriples')\n");
	fprintf(os, "  -o SYNTAX    Output syntax (`turtle' or `ntriples')\n");
	fprintf(os, "  -s INPUT     Parse INPUT as string (terminates options)\n");
	fprintf(os, "  -v           Display version information and exit\n");
	return error ? 1 : 0;
}

static bool
set_syntax(SerdSyntax* syntax, const char* name)
{
	if (!strcmp(name, "turtle")) {
		*syntax = SERD_TURTLE;
	} else if (!strcmp(name, "ntriples")) {
		*syntax = SERD_NTRIPLES;
	} else {
		SORDI_ERRORF("unknown syntax `%s'\n", name);
		return false;
	}
	return true;
}

int
main(int argc, char** argv)
{
	if (argc < 2) {
		return print_usage(argv[0], true);
	}

	FILE*          in_fd         = NULL;
	SerdSyntax     input_syntax  = SERD_TURTLE;
	SerdSyntax     output_syntax = SERD_NTRIPLES;
	bool           from_file     = true;
	const uint8_t* in_name       = NULL;
	int a = 1;
	for (; a < argc && argv[a][0] == '-'; ++a) {
		if (argv[a][1] == '\0') {
			in_name = (const uint8_t*)"(stdin)";
			in_fd   = stdin;
			break;
		} else if (argv[a][1] == 'h') {
			return print_usage(argv[0], false);
		} else if (argv[a][1] == 'v') {
			return print_version();
		} else if (argv[a][1] == 's') {
			in_name = (const uint8_t*)"(string)";
			from_file = false;
			++a;
			break;
		} else if (argv[a][1] == 'i') {
			if (++a == argc) {
				SORDI_ERROR("option requires an argument -- 'i'\n\n");
				return print_usage(argv[0], true);
			}
			if (!set_syntax(&input_syntax, argv[a])) {
				return print_usage(argv[0], true);
			}
		} else if (argv[a][1] == 'o') {
			if (++a == argc) {
				SORDI_ERROR("option requires an argument -- 'o'\n\n");
				return print_usage(argv[0], true);
			}
			if (!set_syntax(&output_syntax, argv[a])) {
				return print_usage(argv[0], true);
			}
		} else {
			SORDI_ERRORF("invalid option -- '%s'\n", argv[a] + 1);
			return print_usage(argv[0], true);
		}
	}

	if (a == argc) {
		SORDI_ERROR("missing input\n");
		return print_usage(argv[0], true);
	}

	const uint8_t* input = (const uint8_t*)argv[a++];
	if (from_file) {
		in_name = in_name ? in_name : input;
		if (!in_fd) {
			input = serd_uri_to_path(in_name);
			if (!input || !(in_fd = fopen((const char*)input, "rb"))) {
				return 1;
			}
		}
	}

	SerdURI  base_uri = SERD_URI_NULL;
	SerdNode base     = SERD_NODE_NULL;
	if (a < argc) {  // Base URI given on command line
		base = serd_node_new_uri_from_string(
			(const uint8_t*)argv[a], NULL, &base_uri);
	} else if (from_file && in_fd != stdin) {  // Use input file URI
		base = serd_node_new_file_uri(input, NULL, &base_uri, true);
	}

	SordWorld*  world  = sord_world_new();
	SordModel*  sord   = sord_new(world, SORD_SPO|SORD_OPS, false);
	SerdEnv*    env    = serd_env_new(&base);
	SerdReader* reader = sord_new_reader(sord, env, input_syntax, NULL);

	SerdStatus status = (from_file)
		? serd_reader_read_file_handle(reader, in_fd, in_name)
		: serd_reader_read_string(reader, input);

	serd_reader_free(reader);

	FILE*    out_fd    = stdout;
	SerdEnv* write_env = serd_env_new(&base);

	int output_style = SERD_STYLE_RESOLVED;
	if (output_syntax == SERD_NTRIPLES) {
		output_style |= SERD_STYLE_ASCII;
	} else {
		output_style |= SERD_STYLE_CURIED | SERD_STYLE_ABBREVIATED;
	}

	SerdWriter* writer = serd_writer_new(
		output_syntax,
		(SerdStyle)output_style,
		write_env, &base_uri, serd_file_sink, stdout);

	// Write @prefix directives
	serd_env_foreach(env,
	                 (SerdPrefixSink)serd_writer_set_prefix,
	                 writer);

	// Write statements
	sord_write(sord, writer, NULL);

	serd_writer_finish(writer);
	serd_writer_free(writer);

	serd_env_free(env);
	serd_env_free(write_env);
	serd_node_free(&base);

	sord_free(sord);
	sord_world_free(world);

	if (from_file) {
		fclose(in_fd);
	}

	if (fclose(out_fd)) {
		perror("sordi: write error");
		status = SERD_ERR_UNKNOWN;
	}

	return (status > SERD_FAILURE) ? 1 : 0;
}
