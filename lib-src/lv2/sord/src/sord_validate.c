/*
  Copyright 2012-2013 David Robillard <http://drobilla.net>

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

#define _BSD_SOURCE  // for realpath

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#    include <windows.h>
#endif

#include "serd/serd.h"
#include "sord/sord.h"
#include "sord_config.h"

#ifdef HAVE_PCRE
#    include <pcre.h>
#endif

#define USTR(s) ((const uint8_t*)s)

#define NS_foaf (const uint8_t*)"http://xmlns.com/foaf/0.1/"
#define NS_owl  (const uint8_t*)"http://www.w3.org/2002/07/owl#"
#define NS_rdf  (const uint8_t*)"http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define NS_rdfs (const uint8_t*)"http://www.w3.org/2000/01/rdf-schema#"
#define NS_xsd  (const uint8_t*)"http://www.w3.org/2001/XMLSchema#"

typedef struct {
	SordNode* foaf_Document;
	SordNode* owl_AnnotationProperty;
	SordNode* owl_Class;
	SordNode* owl_DatatypeProperty;
	SordNode* owl_FunctionalProperty;
	SordNode* owl_InverseFunctionalProperty;
	SordNode* owl_ObjectProperty;
	SordNode* owl_OntologyProperty;
	SordNode* owl_Thing;
	SordNode* owl_equivalentClass;
	SordNode* owl_onDatatype;
	SordNode* owl_withRestrictions;
	SordNode* rdf_Property;
	SordNode* rdf_first;
	SordNode* rdf_rest;
	SordNode* rdf_type;
	SordNode* rdfs_Class;
	SordNode* rdfs_Literal;
	SordNode* rdfs_Resource;
	SordNode* rdfs_domain;
	SordNode* rdfs_range;
	SordNode* rdfs_subClassOf;
	SordNode* xsd_anyURI;
	SordNode* xsd_decimal;
	SordNode* xsd_maxInclusive;
	SordNode* xsd_minInclusive;
	SordNode* xsd_pattern;
	SordNode* xsd_string;
} URIs;

int  n_errors        = 0;
int  n_restrictions  = 0;
bool one_line_errors = false;

static int
print_version(void)
{
	printf("sord_validate " SORD_VERSION
	       " <http://drobilla.net/software/sord>\n");
	printf("Copyright 2012-2013 David Robillard <http://drobilla.net>.\n"
	       "License: <http://www.opensource.org/licenses/isc>\n"
	       "This is free software; you are free to change and redistribute it."
	       "\nThere is NO WARRANTY, to the extent permitted by law.\n");
	return 0;
}

static int
print_usage(const char* name, bool error)
{
	FILE* const os = error ? stderr : stdout;
	fprintf(os, "Usage: %s [OPTION]... INPUT...\n", name);
	fprintf(os, "Validate RDF data\n\n");
	fprintf(os, "  -h  Display this help and exit\n");
	fprintf(os, "  -l  Print errors on a single line.\n");
	fprintf(os, "  -v  Display version information and exit\n");
	fprintf(os,
	        "Validate RDF data.  This is a simple validator which checks\n"
	        "that all used properties are actually defined.  It does not do\n"
	        "any fancy file retrieval, the files passed on the command line\n"
	        "are the only data that is read.  In other words, you must pass\n"
	        "the definition of all vocabularies used on the command line.\n");
	return error ? 1 : 0;
}

static uint8_t*
absolute_path(const uint8_t* path)
{
#ifdef _WIN32
	char* out = (char*)malloc(MAX_PATH);
	GetFullPathName((const char*)path, MAX_PATH, out, NULL);
	return (uint8_t*)out;
#else
	return (uint8_t*)realpath((const char*)path, NULL);
#endif
}

static void
error(const char* msg, const SordQuad quad)
{
	const char* sep = one_line_errors ? "\t" : "\n       ";
	++n_errors;
	fprintf(stderr, "error: %s:%s%s%s%s%s%s\n",
	        msg,
	        sep, (const char*)sord_node_get_string(quad[SORD_SUBJECT]),
	        sep, (const char*)sord_node_get_string(quad[SORD_PREDICATE]),
	        sep, (const char*)sord_node_get_string(quad[SORD_OBJECT]));
}

static bool
is_descendant_of(SordModel*      model,
                 const URIs*     uris,
                 const SordNode* child,
                 const SordNode* parent,
                 const SordNode* pred)
{
	if (!child) {
		return false;
	} else if (sord_node_equals(child, parent) ||
	           sord_ask(model, child, uris->owl_equivalentClass, parent, NULL)) {
		return true;
	}

	SordIter* i = sord_search(model, child, pred, NULL, NULL);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		const SordNode* o = sord_iter_get_node(i, SORD_OBJECT);
		if (sord_node_equals(child, o)) {
			continue;  // Weird class is explicitly a descendent of itself
		}
		if (is_descendant_of(model, uris, o, parent, pred)) {
			sord_iter_free(i);
			return true;
		}
	}
	sord_iter_free(i);

	return false;
}

static bool
regexp_match(const uint8_t* pat, const char* str)
{
#ifdef HAVE_PCRE
	// Append a $ to the pattern so we only match if the entire string matches
	const size_t len  = strlen((const char*)pat);
	char* const  regx = malloc(len + 2);
	memcpy(regx, pat, len);
	regx[len]     = '$';
	regx[len + 1] = '\0';

	const char* err;
	int         erroffset;
	pcre*       re = pcre_compile(regx, PCRE_ANCHORED, &err, &erroffset, NULL);
	free(regx);
	if (!re) {
		fprintf(stderr, "Error in pattern `%s' at offset %d (%s)\n",
		        pat, erroffset, err);
		return false;
	}

	const bool ret = pcre_exec(re, NULL, str, strlen(str), 0, 0, NULL, 0) >= 0;
	pcre_free(re);
	return ret;
#endif  // HAVE_PCRE
	return true;
}

static bool
check_restriction(SordModel*      model,
                  const URIs*     uris,
                  const SordNode* literal,
                  const SordNode* type,
                  const SordNode* restriction)
{
	size_t      len = 0;
	const char* str = (const char*)sord_node_get_string_counted(literal, &len);
	++n_restrictions;

	// Check xsd:pattern
	SordIter* p = sord_search(model, restriction, uris->xsd_pattern, 0, 0);
	if (p) {
		const SordNode* pat  = sord_iter_get_node(p, SORD_OBJECT);
		const bool      good = regexp_match(sord_node_get_string(pat), str);
		if (!good) {
			fprintf(stderr, "`%s' does not match <%s> pattern `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(pat));
		}

		sord_iter_free(p);
		return good;
	}

	/* We'll do some comparison tricks for xsd:decimal types, where
	   lexicographical comparison would be incorrect.  Note that if the
	   literal's type is a descendant of xsd:decimal, we'll end up checking it
	   against the xsd:decimal pattern so there's no need to validate digits
	   here.  At worst we'll get a false positive but it will fail later. */
	const bool is_decimal = is_descendant_of(
		model, uris, type, uris->xsd_decimal, uris->owl_onDatatype);

	// Check xsd:minInclusive
	SordIter* l = sord_search(model, restriction, uris->xsd_minInclusive, 0, 0);
	if (l) {
		const SordNode* lower     = sord_iter_get_node(l, SORD_OBJECT);
		size_t          lower_len = 0;
		const char*     lower_str = (const char*)sord_node_get_string_counted(lower, &lower_len);
		bool            good      = false;
		if (!is_decimal || len == lower_len) {
			 // Not decimal, or equal lengths, strcmp
			good = (strcmp(str, lower_str) >= 0);
		} else {
			// Decimal with different length, only good if longer than the min
			good = (len > lower_len);
		}
		if (!good) {
			fprintf(stderr, "`%s' is not >= <%s> minimum `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(lower));
		}
			        
		sord_iter_free(l);
		return good;
	}

	// Check xsd:maxInclusive
	SordIter* u = sord_search(model, restriction, uris->xsd_maxInclusive, 0, 0);
	if (u) {
		const SordNode* upper     = sord_iter_get_node(u, SORD_OBJECT);
		size_t          upper_len = 0;
		const char*     upper_str = (const char*)sord_node_get_string_counted(upper, &upper_len);
		bool            good      = false;
		if (!is_decimal || len == upper_len) {
			 // Not decimal, or equal lengths, strcmp
			good = (strcmp(str, upper_str) <= 0);
		} else {
			// Decimal with different length, only good if shorter than the max
			good = (len < upper_len);
		}
		if (!good) {
			fprintf(stderr, "`%s' is not <= <%s> maximum `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(upper));
		}
			        
		sord_iter_free(u);
		return good;
	}

	--n_restrictions;
	return true;  // Unknown restriction, be quietly tolerant
}

static bool
literal_is_valid(SordModel*      model,
                 const URIs*     uris,
                 const SordNode* literal,
                 const SordNode* type)
{
	if (!type) {
		return true;
	}

	// Find restrictions list
	SordIter* rs = sord_search(model, type, uris->owl_withRestrictions, 0, 0);
	if (sord_iter_end(rs)) {
		return true;  // No restrictions
	}

	// Walk list, checking each restriction
	const SordNode* head = sord_iter_get_node(rs, SORD_OBJECT);
	while (head) {
		SordIter* f = sord_search(model, head, uris->rdf_first, 0, 0);
		if (!f) {
			break;  // Reached end of restrictions list without failure
		}

		// Check this restriction
		const bool good = check_restriction(
			 model, uris, literal, type, sord_iter_get_node(f, SORD_OBJECT));
		sord_iter_free(f);

		if (!good) {
			sord_iter_free(rs);
			return false;  // Failed, literal is invalid
		}

		// Seek to next list node
		SordIter* n = sord_search(model, head, uris->rdf_rest, 0, 0);
		head = n ? sord_iter_get_node(n, SORD_OBJECT) : NULL;
		sord_iter_free(n);
	}

	sord_iter_free(rs);

	SordIter* s = sord_search(model, type, uris->owl_onDatatype, 0, 0);
	if (s) {
		const SordNode* super = sord_iter_get_node(s, SORD_OBJECT);
		const bool      good  = literal_is_valid(model, uris, literal, super);
		sord_iter_free(s);
		return good;  // Match iff literal also matches supertype
	}

	return true;  // Matches top level type
}

static bool
check_type(SordModel*      model,
           URIs*           uris,
           const SordNode* node,
           const SordNode* type)
{
	if (sord_node_equals(type, uris->rdfs_Resource) ||
	    sord_node_equals(type, uris->owl_Thing)) {
		return true;
	}

	if (sord_node_get_type(node) == SORD_LITERAL) {
		if (sord_node_equals(type, uris->rdfs_Literal) ||
		    sord_node_equals(type, uris->xsd_string)) {
			return true;
		} else {
			return literal_is_valid(model, uris, node, type);
		}
	} else if (sord_node_get_type(node) == SORD_URI) {
		if (sord_node_equals(type, uris->foaf_Document)) {
			return true;  // Questionable...
		} else if (is_descendant_of(
			           model, uris,
			           type, uris->xsd_anyURI, uris->owl_onDatatype)) {
			/* Type is any URI and this is a URI, so pass.  Restrictions on
			   anyURI subtypes are not currently checked (very uncommon). */
			return true;  // Type is anyURI, and this is a URI
		} else {
			SordIter* t = sord_search(model, node, uris->rdf_type, NULL, NULL);
			for (; !sord_iter_end(t); sord_iter_next(t)) {
				if (is_descendant_of(model, uris,
				                     sord_iter_get_node(t, SORD_OBJECT),
				                     type,
				                     uris->rdfs_subClassOf)) {
					sord_iter_free(t);
					return true;
				}
			}
			sord_iter_free(t);
			return false;
		}
	} else {
		return true;  // Blanks often lack explicit types, ignore
	}

	return false;
}

int
main(int argc, char** argv)
{
	if (argc < 2) {
		return print_usage(argv[0], true);
	}

	int a = 1;
	for (; a < argc && argv[a][0] == '-'; ++a) {
		if (argv[a][1] == 'l') {
			one_line_errors = true;
		} else if (argv[a][1] == 'v') {
			return print_version();
		} else {
			fprintf(stderr, "%s: Unknown option `%s'\n", argv[0], argv[a]);
			return print_usage(argv[0], true);
		}
	}

	SordWorld*  world  = sord_world_new();
	SordModel*  model  = sord_new(world, SORD_SPO|SORD_OPS, false);
	SerdEnv*    env    = serd_env_new(&SERD_NODE_NULL);
	SerdReader* reader = sord_new_reader(model, env, SERD_TURTLE, NULL);

	for (; a < argc; ++a) {
		const uint8_t* input   = (const uint8_t*)argv[a];
		uint8_t*       in_path = absolute_path(serd_uri_to_path(input));

		if (!in_path) {
			fprintf(stderr, "Skipping file %s\n", input);
			continue;
		}

		SerdURI  base_uri;
		SerdNode base_uri_node = serd_node_new_file_uri(
			in_path, NULL, &base_uri, false);

		serd_env_set_base_uri(env, &base_uri_node);
		const SerdStatus st = serd_reader_read_file(reader, in_path);
		if (st) {
			fprintf(stderr, "error reading %s: %s\n",
			        in_path, serd_strerror(st));
		}

		serd_node_free(&base_uri_node);
		free(in_path);
	}
	serd_reader_free(reader);
	serd_env_free(env);

#define URI(prefix, suffix) \
	uris.prefix##_##suffix = sord_new_uri(world, NS_##prefix #suffix)

	URIs uris;
	URI(foaf, Document);
	URI(owl, AnnotationProperty);
	URI(owl, Class);
	URI(owl, DatatypeProperty);
	URI(owl, FunctionalProperty);
	URI(owl, InverseFunctionalProperty);
	URI(owl, ObjectProperty);
	URI(owl, OntologyProperty);
	URI(owl, Thing);
	URI(owl, equivalentClass);
	URI(owl, onDatatype);
	URI(owl, withRestrictions);
	URI(rdf, Property);
	URI(rdf, first);
	URI(rdf, rest);
	URI(rdf, type);
	URI(rdfs, Class);
	URI(rdfs, Literal);
	URI(rdfs, Resource);
	URI(rdfs, domain);
	URI(rdfs, range);
	URI(rdfs, subClassOf);
	URI(xsd, anyURI);
	URI(xsd, decimal);
	URI(xsd, maxInclusive);
	URI(xsd, minInclusive);
	URI(xsd, pattern);
	URI(xsd, string);

#ifndef HAVE_PCRE
	fprintf(stderr, "warning: Built without PCRE, datatypes not checked.\n");
#endif

	SordIter* i = sord_begin(model);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		SordQuad quad;
		sord_iter_get(i, quad);

		const SordNode* subj = quad[SORD_SUBJECT];
		const SordNode* pred = quad[SORD_PREDICATE];
		const SordNode* obj  = quad[SORD_OBJECT];

		bool is_any_property = false;
		SordIter* t = sord_search(model, pred, uris.rdf_type, NULL, NULL);
		for (; !sord_iter_end(t); sord_iter_next(t)) {
			if (is_descendant_of(model, &uris,
			                     sord_iter_get_node(t, SORD_OBJECT),
			                     uris.rdf_Property,
			                     uris.rdfs_subClassOf)) {
				is_any_property = true;
				break;
			}
		}
		sord_iter_free(t);

		const bool is_ObjectProperty = sord_ask(
			model, pred, uris.rdf_type, uris.owl_ObjectProperty, 0);
		const bool is_FunctionalProperty = sord_ask(
			model, pred, uris.rdf_type, uris.owl_FunctionalProperty, 0);
		const bool is_InverseFunctionalProperty = sord_ask(
			model, pred, uris.rdf_type, uris.owl_InverseFunctionalProperty, 0);
		const bool is_DatatypeProperty = sord_ask(
			model, pred, uris.rdf_type, uris.owl_DatatypeProperty, 0);

		if (!is_any_property) {
			error("Use of undefined property", quad);
		}

		if (is_DatatypeProperty &&
		    sord_node_get_type(obj) != SORD_LITERAL) {
			error("Datatype property with non-literal value", quad);
		}

		if (is_ObjectProperty &&
		    sord_node_get_type(obj) == SORD_LITERAL) {
			error("Object property with literal value", quad);
		}

		if (is_FunctionalProperty &&
		    sord_count(model, subj, pred, NULL, NULL) > 1) {
			error("Functional property with several objects", quad);
		}

		if (is_InverseFunctionalProperty &&
		    sord_count(model, NULL, pred, obj, NULL) > 1) {
			error("Inverse functional property with several subjects", quad);
		}

		if (sord_node_equals(pred, uris.rdf_type) &&
		    !sord_ask(model, obj, uris.rdf_type, uris.rdfs_Class, NULL) &&
		    !sord_ask(model, obj, uris.rdf_type, uris.owl_Class, NULL)) {
			error("Type is not a rdfs:Class or owl:Class", quad);
		}

		if (sord_node_get_type(obj) == SORD_LITERAL &&
		    !literal_is_valid(model, &uris, obj, sord_node_get_datatype(obj))) {
			error("Literal does not match datatype", quad);
		}

		SordIter* r = sord_search(model, pred, uris.rdfs_range, NULL, NULL);
		if (r) {
			const SordNode* range = sord_iter_get_node(r, SORD_OBJECT);
			if (!check_type(model, &uris, obj, range)) {
				error("Object not in property range", quad);
				fprintf(stderr, "note: Range is <%s>\n",
				        sord_node_get_string(range));
			}
			sord_iter_free(r);
		}

		SordIter* d = sord_search(model, pred, uris.rdfs_domain, NULL, NULL);
		if (d) {
			const SordNode* domain = sord_iter_get_node(d, SORD_OBJECT);
			if (!check_type(model, &uris, subj, domain)) {
				error("Subject not in property domain", quad);
				fprintf(stderr, "note: Domain is <%s>\n",
				        sord_node_get_string(domain));
			}
			sord_iter_free(d);
		}
	}
	sord_iter_free(i);

	printf("Found %d errors among %d files (checked %d restrictions)\n",
	       n_errors, argc - 1, n_restrictions);

	sord_free(model);
	sord_world_free(world);
	return 0;
}
