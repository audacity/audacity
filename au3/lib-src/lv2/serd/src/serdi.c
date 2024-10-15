/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>

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

#include "serd_internal.h"

#ifdef _WIN32
#include <fcntl.h>
#include <io.h>
#endif

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#define SERDI_ERROR(msg)       fprintf(stderr, "serdi: " msg);
#define SERDI_ERRORF(fmt, ...) fprintf(stderr, "serdi: " fmt, __VA_ARGS__);

typedef struct {
	SerdSyntax  syntax;
	const char* name;
	const char* extension;
} Syntax;

static const Syntax syntaxes[] = {
	{SERD_TURTLE,   "turtle",   ".ttl"},
	{SERD_NTRIPLES, "ntriples", ".nt"},
	{SERD_NQUADS,   "nquads",   ".nq"},
	{SERD_TRIG,     "trig",     ".trig"},
	{(SerdSyntax)0, NULL, NULL}
};

static SerdSyntax
get_syntax(const char* name)
{
	for (const Syntax* s = syntaxes; s->name; ++s) {
		if (!serd_strncasecmp(s->name, name, strlen(name))) {
			return s->syntax;
		}
	}
	SERDI_ERRORF("unknown syntax `%s'\n", name);
	return (SerdSyntax)0;
}

static SerdSyntax
guess_syntax(const char* filename)
{
	const char* ext = strrchr(filename, '.');
	if (ext) {
		for (const Syntax* s = syntaxes; s->name; ++s) {
			if (!serd_strncasecmp(s->extension, ext, strlen(ext))) {
				return s->syntax;
			}
		}
	}
	return (SerdSyntax)0;
}

static int
print_version(void)
{
	printf("serdi " SERD_VERSION " <http://drobilla.net/software/serd>\n");
	printf("Copyright 2011-2017 David Robillard <http://drobilla.net>.\n"
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
	fprintf(os, "Read and write RDF syntax.\n");
	fprintf(os, "Use - for INPUT to read from standard input.\n\n");
	fprintf(os, "  -a           Write ASCII output if possible.\n");
	fprintf(os, "  -b           Fast bulk output for large serialisations.\n");
	fprintf(os, "  -c PREFIX    Chop PREFIX from matching blank node IDs.\n");
	fprintf(os, "  -e           Eat input one character at a time.\n");
	fprintf(os, "  -f           Keep full URIs in input (don't qualify).\n");
	fprintf(os, "  -h           Display this help and exit.\n");
	fprintf(os, "  -i SYNTAX    Input syntax: turtle/ntriples/trig/nquads.\n");
	fprintf(os, "  -l           Lax (non-strict) parsing.\n");
	fprintf(os, "  -o SYNTAX    Output syntax: turtle/ntriples/nquads.\n");
	fprintf(os, "  -p PREFIX    Add PREFIX to blank node IDs.\n");
	fprintf(os, "  -q           Suppress all output except data.\n");
	fprintf(os, "  -r ROOT_URI  Keep relative URIs within ROOT_URI.\n");
	fprintf(os, "  -s INPUT     Parse INPUT as string (terminates options).\n");
	fprintf(os, "  -v           Display version information and exit.\n");
	return error ? 1 : 0;
}

static int
missing_arg(const char* name, char opt)
{
	SERDI_ERRORF("option requires an argument -- '%c'\n", opt);
	return print_usage(name, true);
}

static SerdStatus
quiet_error_sink(void* handle, const SerdError* e)
{
	(void)handle;
	(void)e;
	return SERD_SUCCESS;
}

int
main(int argc, char** argv)
{
	if (argc < 2) {
		return print_usage(argv[0], true);
	}

	FILE*          in_fd         = NULL;
	SerdSyntax     input_syntax  = (SerdSyntax)0;
	SerdSyntax     output_syntax = (SerdSyntax)0;
	bool           from_file     = true;
	bool           ascii         = false;
	bool           bulk_read     = true;
	bool           bulk_write    = false;
	bool           full_uris     = false;
	bool           lax           = false;
	bool           quiet         = false;
	const uint8_t* in_name       = NULL;
	const uint8_t* add_prefix    = NULL;
	const uint8_t* chop_prefix   = NULL;
	const uint8_t* root_uri      = NULL;
	int            a             = 1;
	for (; a < argc && argv[a][0] == '-'; ++a) {
		if (argv[a][1] == '\0') {
			in_name = (const uint8_t*)"(stdin)";
			in_fd   = stdin;
			break;
		} else if (argv[a][1] == 'a') {
			ascii = true;
		} else if (argv[a][1] == 'b') {
			bulk_write = true;
		} else if (argv[a][1] == 'e') {
			bulk_read = false;
		} else if (argv[a][1] == 'f') {
			full_uris = true;
		} else if (argv[a][1] == 'h') {
			return print_usage(argv[0], false);
		} else if (argv[a][1] == 'l') {
			lax = true;
		} else if (argv[a][1] == 'q') {
			quiet = true;
		} else if (argv[a][1] == 'v') {
			return print_version();
		} else if (argv[a][1] == 's') {
			in_name = (const uint8_t*)"(string)";
			from_file = false;
			++a;
			break;
		} else if (argv[a][1] == 'i') {
			if (++a == argc) {
				return missing_arg(argv[0], 'i');
			} else if (!(input_syntax = get_syntax(argv[a]))) {
				return print_usage(argv[0], true);
			}
		} else if (argv[a][1] == 'o') {
			if (++a == argc) {
				return missing_arg(argv[0], 'o');
			} else if (!(output_syntax = get_syntax(argv[a]))) {
				return print_usage(argv[0], true);
			}
		} else if (argv[a][1] == 'p') {
			if (++a == argc) {
				return missing_arg(argv[0], 'p');
			}
			add_prefix = (const uint8_t*)argv[a];
		} else if (argv[a][1] == 'c') {
			if (++a == argc) {
				return missing_arg(argv[0], 'c');
			}
			chop_prefix = (const uint8_t*)argv[a];
		} else if (argv[a][1] == 'r') {
			if (++a == argc) {
				return missing_arg(argv[0], 'r');
			}
			root_uri = (const uint8_t*)argv[a];
		} else {
			SERDI_ERRORF("invalid option -- '%s'\n", argv[a] + 1);
			return print_usage(argv[0], true);
		}
	}

	if (a == argc) {
		SERDI_ERROR("missing input\n");
		return 1;
	}

#ifdef _WIN32
	_setmode(fileno(stdin), _O_BINARY);
	_setmode(fileno(stdout), _O_BINARY);
#endif

	const uint8_t* input = (const uint8_t*)argv[a++];
	if (from_file) {
		in_name = in_name ? in_name : input;
		if (!in_fd) {
			input = serd_uri_to_path(in_name);
			if (!input || !(in_fd = serd_fopen((const char*)input, "rb"))) {
				return 1;
			}
		}
	}

	if (!input_syntax && !(input_syntax = guess_syntax((const char*)in_name))) {
		input_syntax = SERD_TRIG;
	}

	if (!output_syntax) {
		output_syntax = (
			(input_syntax == SERD_TURTLE || input_syntax == SERD_NTRIPLES)
			? SERD_NTRIPLES
			: SERD_NQUADS);
	}

	SerdURI  base_uri = SERD_URI_NULL;
	SerdNode base     = SERD_NODE_NULL;
	if (a < argc) {  // Base URI given on command line
		base = serd_node_new_uri_from_string(
			(const uint8_t*)argv[a], NULL, &base_uri);
	} else if (from_file && in_fd != stdin) {  // Use input file URI
		base = serd_node_new_file_uri(input, NULL, &base_uri, true);
	}

	FILE*    out_fd = stdout;
	SerdEnv* env    = serd_env_new(&base);

	int output_style = 0;
	if (output_syntax == SERD_NTRIPLES || ascii) {
		output_style |= SERD_STYLE_ASCII;
	} else if (output_syntax == SERD_TURTLE) {
		output_style |= SERD_STYLE_ABBREVIATED;
		if (!full_uris) {
			output_style |= SERD_STYLE_CURIED;
		}
	}

	if ((input_syntax == SERD_TURTLE || input_syntax == SERD_TRIG) ||
	    (output_style & SERD_STYLE_CURIED)) {
		// Base URI may change and/or we're abbreviating URIs, so must resolve
		output_style |= SERD_STYLE_RESOLVED;
	}

	if (bulk_write) {
		output_style |= SERD_STYLE_BULK;
	}

	SerdWriter* writer = serd_writer_new(
		output_syntax, (SerdStyle)output_style,
		env, &base_uri, serd_file_sink, out_fd);

	SerdReader* reader = serd_reader_new(
		input_syntax, writer, NULL,
		(SerdBaseSink)serd_writer_set_base_uri,
		(SerdPrefixSink)serd_writer_set_prefix,
		(SerdStatementSink)serd_writer_write_statement,
		(SerdEndSink)serd_writer_end_anon);

	serd_reader_set_strict(reader, !lax);
	if (quiet) {
		serd_reader_set_error_sink(reader, quiet_error_sink, NULL);
		serd_writer_set_error_sink(writer, quiet_error_sink, NULL);
	}

	SerdNode root = serd_node_from_string(SERD_URI, root_uri);
	serd_writer_set_root_uri(writer, &root);
	serd_writer_chop_blank_prefix(writer, chop_prefix);
	serd_reader_add_blank_prefix(reader, add_prefix);

	SerdStatus status = SERD_SUCCESS;
	if (!from_file) {
		status = serd_reader_read_string(reader, input);
	} else if (bulk_read) {
		status = serd_reader_read_file_handle(reader, in_fd, in_name);
	} else {
		status = serd_reader_start_stream(reader, in_fd, in_name, false);
		while (!status) {
			status = serd_reader_read_chunk(reader);
		}
		serd_reader_end_stream(reader);
	}

	serd_reader_free(reader);
	serd_writer_finish(writer);
	serd_writer_free(writer);
	serd_env_free(env);
	serd_node_free(&base);

	if (from_file) {
		fclose(in_fd);
	}

	if (fclose(out_fd)) {
		perror("serdi: write error");
		status = SERD_ERR_UNKNOWN;
	}

	return (status > SERD_FAILURE) ? 1 : 0;
}
