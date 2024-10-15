/*
  Copyright 2012-2017 David Robillard <http://drobilla.net>

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

#define _BSD_SOURCE     1  // for realpath
#define _DEFAULT_SOURCE 1  // for realpath

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

#define USTR(s) ((const uint8_t*)(s))

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
	SordNode* owl_Restriction;
	SordNode* owl_Thing;
	SordNode* owl_cardinality;
	SordNode* owl_equivalentClass;
	SordNode* owl_maxCardinality;
	SordNode* owl_minCardinality;
	SordNode* owl_onDatatype;
	SordNode* owl_onProperty;
	SordNode* owl_someValuesFrom;
	SordNode* owl_withRestrictions;
	SordNode* rdf_PlainLiteral;
	SordNode* rdf_Property;
	SordNode* rdf_first;
	SordNode* rdf_rest;
	SordNode* rdf_type;
	SordNode* rdfs_Class;
	SordNode* rdfs_Literal;
	SordNode* rdfs_Resource;
	SordNode* rdfs_domain;
	SordNode* rdfs_label;
	SordNode* rdfs_range;
	SordNode* rdfs_subClassOf;
	SordNode* xsd_anyURI;
	SordNode* xsd_decimal;
	SordNode* xsd_double;
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
	printf("Copyright 2012-2017 David Robillard <http://drobilla.net>.\n"
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

static int
errorf(const SordQuad quad, const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args);
	va_end(args);

	const char* sep = one_line_errors ? "\t" : "\n       ";
	fprintf(stderr, "%s%s%s%s%s%s\n",
	        sep, (const char*)sord_node_get_string(quad[SORD_SUBJECT]),
	        sep, (const char*)sord_node_get_string(quad[SORD_PREDICATE]),
	        sep, (const char*)sord_node_get_string(quad[SORD_OBJECT]));

	++n_errors;
	return 1;
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
	} else if (
		sord_node_equals(child, parent) ||
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
	char* const  regx = (char*)malloc(len + 2);
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

static int
bound_cmp(SordModel*      model,
          const URIs*     uris,
          const SordNode* literal,
          const SordNode* type,
          const SordNode* bound)
{
	const char*     str        = (const char*)sord_node_get_string(literal);
	const char*     bound_str  = (const char*)sord_node_get_string(bound);
	const SordNode* pred       = uris->owl_onDatatype;
	const bool      is_numeric =
		is_descendant_of(model, uris, type, uris->xsd_decimal, pred) ||
		is_descendant_of(model, uris, type, uris->xsd_double, pred);

	if (is_numeric) {
		const double fbound   = serd_strtod(bound_str, NULL);
		const double fliteral = serd_strtod(str, NULL);
		return ((fliteral < fbound) ? -1 :
		        (fliteral > fbound) ? 1 :
		        0);
	} else {
		return strcmp(str, bound_str);
	}
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

	// Check xsd:pattern
	SordIter* p = sord_search(model, restriction, uris->xsd_pattern, 0, 0);
	if (p) {
		const SordNode* pat  = sord_iter_get_node(p, SORD_OBJECT);
		if (!regexp_match(sord_node_get_string(pat), str)) {
			fprintf(stderr, "`%s' does not match <%s> pattern `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(pat));
			sord_iter_free(p);
			return false;
		}
		sord_iter_free(p);
		++n_restrictions;
	}

	// Check xsd:minInclusive
	SordIter* l = sord_search(model, restriction, uris->xsd_minInclusive, 0, 0);
	if (l) {
		const SordNode* lower = sord_iter_get_node(l, SORD_OBJECT);
		if (bound_cmp(model, uris, literal, type, lower) < 0) {
			fprintf(stderr, "`%s' is not >= <%s> minimum `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(lower));
			sord_iter_free(l);
			return false;
		}
		sord_iter_free(l);
		++n_restrictions;
	}

	// Check xsd:maxInclusive
	SordIter* u = sord_search(model, restriction, uris->xsd_maxInclusive, 0, 0);
	if (u) {
		const SordNode* upper = sord_iter_get_node(u, SORD_OBJECT);
		if (bound_cmp(model, uris, literal, type, upper) > 0) {
			fprintf(stderr, "`%s' is not <= <%s> maximum `%s'\n",
			        sord_node_get_string(literal),
			        sord_node_get_string(type),
			        sord_node_get_string(upper));
			sord_iter_free(u);
			return false;
		}
		sord_iter_free(u);
		++n_restrictions;
	}

	return true;  // Unknown restriction, be quietly tolerant
}

static bool
literal_is_valid(SordModel*      model,
                 const URIs*     uris,
                 const SordQuad  quad,
                 const SordNode* literal,
                 const SordNode* type)
{
	if (!type) {
		return true;
	}

	/* Check that literal data is related to required type.  We don't do a
	   strict subtype check here because e.g. an xsd:decimal might be a valid
	   xsd:unsignedInt, which the pattern checks will verify, but if the
	   literal type is not related to the required type at all
	   (e.g. xsd:decimal and xsd:string) there is a problem. */
	const SordNode* datatype = sord_node_get_datatype(literal);
	if (datatype && datatype != type) {
		if (!is_descendant_of(
			    model, uris,
			    datatype, type, uris->owl_onDatatype) &&
		    !is_descendant_of(
			    model, uris,
			    type, datatype, uris->owl_onDatatype) &&
		    !(sord_node_equals(datatype, uris->xsd_decimal) &&
		      is_descendant_of(
			      model, uris,
			      type, uris->xsd_double, uris->owl_onDatatype))) {
			errorf(quad,
			       "Literal `%s' datatype <%s> is not compatible with <%s>\n",
			       sord_node_get_string(literal),
			       sord_node_get_string(datatype),
			       sord_node_get_string(type));
			return false;
		}
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
		const bool      good  = literal_is_valid(
			model, uris, quad, literal, super);
		sord_iter_free(s);
		return good;  // Match iff literal also matches supertype
	}

	return true;  // Matches top level type
}

static bool
check_type(SordModel*      model,
           const URIs*     uris,
           const SordQuad  quad,
           const SordNode* node,
           const SordNode* type)
{
	if (sord_node_equals(type, uris->rdfs_Resource) ||
	    sord_node_equals(type, uris->owl_Thing)) {
		return true;
	}

	if (sord_node_get_type(node) == SORD_LITERAL) {
		if (sord_node_equals(type, uris->rdfs_Literal)) {
			return true;
		} else if (sord_node_equals(type, uris->rdf_PlainLiteral)) {
			return !sord_node_get_language(node);
		} else {
			return literal_is_valid(model, uris, quad, node, type);
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

static uint64_t
count_non_blanks(SordIter* i, SordQuadIndex field)
{
	uint64_t n = 0;
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		const SordNode* node = sord_iter_get_node(i, field);
		if (sord_node_get_type(node) != SORD_BLANK) {
			++n;
		}
	}
	return n;
}

static int
check_properties(SordModel* model, URIs* uris)
{
	int       st = 0;
	SordIter* i  = sord_begin(model);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		SordQuad quad;
		sord_iter_get(i, quad);

		const SordNode* subj = quad[SORD_SUBJECT];
		const SordNode* pred = quad[SORD_PREDICATE];
		const SordNode* obj  = quad[SORD_OBJECT];

		bool is_any_property = false;
		SordIter* t = sord_search(model, pred, uris->rdf_type, NULL, NULL);
		for (; !sord_iter_end(t); sord_iter_next(t)) {
			if (is_descendant_of(model, uris,
			                     sord_iter_get_node(t, SORD_OBJECT),
			                     uris->rdf_Property,
			                     uris->rdfs_subClassOf)) {
				is_any_property = true;
				break;
			}
		}
		sord_iter_free(t);

		const bool is_ObjectProperty = sord_ask(
			model, pred, uris->rdf_type, uris->owl_ObjectProperty, 0);
		const bool is_FunctionalProperty = sord_ask(
			model, pred, uris->rdf_type, uris->owl_FunctionalProperty, 0);
		const bool is_InverseFunctionalProperty = sord_ask(
			model, pred, uris->rdf_type, uris->owl_InverseFunctionalProperty, 0);
		const bool is_DatatypeProperty = sord_ask(
			model, pred, uris->rdf_type, uris->owl_DatatypeProperty, 0);

		if (!is_any_property) {
			st = errorf(quad, "Use of undefined property");
		}

		if (!sord_ask(model, pred, uris->rdfs_label, NULL, NULL)) {
			st = errorf(quad, "Property <%s> has no label",
			            sord_node_get_string(pred));
		}

		if (is_DatatypeProperty &&
		    sord_node_get_type(obj) != SORD_LITERAL) {
			st = errorf(quad, "Datatype property with non-literal value");
		}

		if (is_ObjectProperty &&
		    sord_node_get_type(obj) == SORD_LITERAL) {
			st = errorf(quad, "Object property with literal value");
		}

		if (is_FunctionalProperty) {
			SordIter*      o = sord_search(model, subj, pred, NULL, NULL);
			const uint64_t n = count_non_blanks(o, SORD_OBJECT);
			if (n > 1) {
				st = errorf(quad, "Functional property with %u objects", n);
			}
			sord_iter_free(o);
		}

		if (is_InverseFunctionalProperty) {
			SordIter*      s = sord_search(model, NULL, pred, obj, NULL);
			const unsigned n = count_non_blanks(s, SORD_SUBJECT);
			if (n > 1) {
				st = errorf(
					quad, "Inverse functional property with %u subjects", n);
			}
			sord_iter_free(s);
		}

		if (sord_node_equals(pred, uris->rdf_type) &&
		    !sord_ask(model, obj, uris->rdf_type, uris->rdfs_Class, NULL) &&
		    !sord_ask(model, obj, uris->rdf_type, uris->owl_Class, NULL)) {
			st = errorf(quad, "Type is not a rdfs:Class or owl:Class");
		}

		if (sord_node_get_type(obj) == SORD_LITERAL &&
		    !literal_is_valid(model, uris, quad,
		                      obj, sord_node_get_datatype(obj))) {
			st = errorf(quad, "Literal does not match datatype");
		}

		SordIter* r = sord_search(model, pred, uris->rdfs_range, NULL, NULL);
		for (; !sord_iter_end(r); sord_iter_next(r)) {
			const SordNode* range = sord_iter_get_node(r, SORD_OBJECT);
			if (!check_type(model, uris, quad, obj, range)) {
				st = errorf(quad, "Object not in range <%s>\n",
				            sord_node_get_string(range));
			}
		}
		sord_iter_free(r);

		SordIter* d = sord_search(model, pred, uris->rdfs_domain, NULL, NULL);
		if (d) {
			const SordNode* domain = sord_iter_get_node(d, SORD_OBJECT);
			if (!check_type(model, uris, quad, subj, domain)) {
				st = errorf(quad, "Subject not in domain <%s>",
				            sord_node_get_string(domain));
			}
			sord_iter_free(d);
		}
	}
	sord_iter_free(i);

	return st;
}

static int
check_instance(SordModel*      model,
               const URIs*     uris,
               const SordNode* restriction,
               const SordQuad  quad)
{
	const SordNode* instance = quad[SORD_SUBJECT];
	int             st       = 0;

	const SordNode* prop = sord_get(
		model, restriction, uris->owl_onProperty, NULL, NULL);
	if (!prop) {
		return 0;
	}

	const unsigned values = sord_count(model, instance, prop, NULL, NULL);

	// Check exact cardinality
	const SordNode* card = sord_get(
		model, restriction, uris->owl_cardinality, NULL, NULL);
	if (card) {
		const unsigned c = atoi((const char*)sord_node_get_string(card));
		if (values != c) {
			st = errorf(quad, "Property %s on %s has %u != %u values",
			            sord_node_get_string(prop),
			            sord_node_get_string(instance),
			            values, c);
		}
	}

	// Check minimum cardinality
	const SordNode* minCard = sord_get(
		model, restriction, uris->owl_minCardinality, NULL, NULL);
	if (minCard) {
		const unsigned m = atoi((const char*)sord_node_get_string(minCard));
		if (values < m) {
			st = errorf(quad, "Property %s on %s has %u < %u values",
			            sord_node_get_string(prop),
			            sord_node_get_string(instance),
			            values, m);
		}
	}

	// Check maximum cardinality
	const SordNode* maxCard = sord_get(
		model, restriction, uris->owl_maxCardinality, NULL, NULL);
	if (maxCard) {
		const unsigned m = atoi((const char*)sord_node_get_string(maxCard));
		if (values < m) {
			st = errorf(quad, "Property %s on %s has %u > %u values",
			            sord_node_get_string(prop),
			            sord_node_get_string(instance),
			            values, m);
		}
	}

	// Check someValuesFrom
	SordIter* sf = sord_search(
		model, restriction, uris->owl_someValuesFrom, NULL, NULL);
	if (sf) {
		const SordNode* type = sord_iter_get_node(sf, SORD_OBJECT);

		SordIter* v     = sord_search(model, instance, prop, NULL, NULL);
		bool      found = false;
		for (; !sord_iter_end(v); sord_iter_next(v)) {
			const SordNode* value = sord_iter_get_node(v, SORD_OBJECT);
			if (check_type(model, uris, quad, value, type)) {
				found = true;
				break;
			}
		}
		if (!found) {
			st = errorf(quad, "%s has no <%s> values of type <%s>\n",
			            sord_node_get_string(instance),
			            sord_node_get_string(prop),
			            sord_node_get_string(type));
		}
		sord_iter_free(v);
	}
	sord_iter_free(sf);

	return st;
}

static int
check_class_instances(SordModel*      model,
                      const URIs*     uris,
                      const SordNode* restriction,
                      const SordNode* klass)
{
	// Check immediate instances of this class
	SordIter* i = sord_search(model, NULL, uris->rdf_type, klass, NULL);
	for (; !sord_iter_end(i); sord_iter_next(i)) {
		SordQuad quad;
		sord_iter_get(i, quad);
		check_instance(model, uris, restriction, quad);
	}
	sord_iter_free(i);

	// Check instances of all subclasses recursively
	SordIter* s = sord_search(model, NULL, uris->rdfs_subClassOf, klass, NULL);
	for (; !sord_iter_end(s); sord_iter_next(s)) {
		const SordNode* subklass = sord_iter_get_node(s, SORD_SUBJECT);
		check_class_instances(model, uris, restriction, subklass);
	}
	sord_iter_free(s);

	return 0;
}

static int
check_instances(SordModel* model, const URIs* uris)
{
	int       st = 0;
	SordIter* r = sord_search(
		model, NULL, uris->rdf_type, uris->owl_Restriction, NULL);
	for (; !sord_iter_end(r); sord_iter_next(r)) {
		const SordNode* restriction = sord_iter_get_node(r, SORD_SUBJECT);
		const SordNode* prop        = sord_get(
			model, restriction, uris->owl_onProperty, NULL, NULL);
		if (!prop) {
			continue;
		}

		SordIter* c = sord_search(
			model, NULL, uris->rdfs_subClassOf, restriction, NULL);
		for (; !sord_iter_end(c); sord_iter_next(c)) {
			const SordNode* klass = sord_iter_get_node(c, SORD_SUBJECT);
			check_class_instances(model, uris, restriction, klass);
		}
		sord_iter_free(c);
	}
	sord_iter_free(r);

	return st;
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
			in_path, NULL, &base_uri, true);

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
	URI(owl, Restriction);
	URI(owl, Thing);
	URI(owl, cardinality);
	URI(owl, equivalentClass);
	URI(owl, maxCardinality);
	URI(owl, minCardinality);
	URI(owl, onDatatype);
	URI(owl, onProperty);
	URI(owl, someValuesFrom);
	URI(owl, withRestrictions);
	URI(rdf, PlainLiteral);
	URI(rdf, Property);
	URI(rdf, first);
	URI(rdf, rest);
	URI(rdf, type);
	URI(rdfs, Class);
	URI(rdfs, Literal);
	URI(rdfs, Resource);
	URI(rdfs, domain);
	URI(rdfs, label);
	URI(rdfs, range);
	URI(rdfs, subClassOf);
	URI(xsd, anyURI);
	URI(xsd, decimal);
	URI(xsd, double);
	URI(xsd, maxInclusive);
	URI(xsd, minInclusive);
	URI(xsd, pattern);
	URI(xsd, string);

#ifndef HAVE_PCRE
	fprintf(stderr, "warning: Built without PCRE, datatypes not checked.\n");
#endif

	const int prop_st = check_properties(model, &uris);
	const int inst_st = check_instances(model, &uris);

	printf("Found %d errors among %d files (checked %d restrictions)\n",
	       n_errors, argc - 1, n_restrictions);

	sord_free(model);
	sord_world_free(world);
	return prop_st || inst_st;
}
