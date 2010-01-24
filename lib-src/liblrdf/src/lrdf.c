#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <raptor.h>
#include <ladspa.h>
#include <time.h>
#include <sys/types.h>
#if !defined(WIN32)
#include <unistd.h>
#include <sys/time.h>
#endif

#include "lrdf.h"
#include "lrdf_md5.h"

/* XXX The size if that hash table, should be dyunamic, but this will do for
 * now */
#define LRDF_HASH_SIZE 1024

static unsigned int lrdf_uid = 0;	/* A unique(ish) id to append to genid's to
					 * avoid clashses */

static lrdf_statement *triples = NULL;
static lrdf_statement *free_triples;
static lrdf_string_hash *resources_hash[LRDF_HASH_SIZE];
static lrdf_string_hash *literals_hash[LRDF_HASH_SIZE];
static lrdf_triple_hash *subj_hash[LRDF_HASH_SIZE];
static lrdf_triple_hash *obj_hash[LRDF_HASH_SIZE];
static lrdf_triple_hash *pred_hash[LRDF_HASH_SIZE];
static lrdf_closure_hash *subclass_hash[LRDF_HASH_SIZE];
static lrdf_closure_hash *superclass_hash[LRDF_HASH_SIZE];
static lrdf_hash rdf_resource_h;

/* Internal functions */
void lrdf_more_triples(int count);
lrdf_statement *lrdf_alloc_statement();
lrdf_statement *lrdf_all_triples();
static char *lrdf_check_hash(lrdf_string_hash ** tbl, lrdf_hash hash, const char
			     *str);
static char *lrdf_find_string_hash(lrdf_string_hash ** tbl,
				   lrdf_hash hash);
static void lrdf_add_triple_hash(lrdf_triple_hash ** tbl, lrdf_hash hash,
				 lrdf_statement * s);
static void lrdf_remove_triple_hash(lrdf_triple_hash ** tbl,
				    lrdf_hash hash, lrdf_statement * s);
static void lrdf_add_closure_hash(lrdf_closure_hash ** tbl,
				  lrdf_hash subject, lrdf_hash object);
static void lrdf_store(void *user_data,
		       const raptor_statement * statement);
void lrdf_free_statements(lrdf_statement * s);
void lrdf_copy_statement(lrdf_statement * from, lrdf_statement * to);
void lrdf_rebuild_taxonomic_closure(lrdf_closure_hash ** fwd_tbl,
				    lrdf_closure_hash ** rev_tbl);
static lrdf_uris *lrdf_uris_new(int size);
int lrdf_read_file_intl(const char *uri);
static void lrdf_uris_append(lrdf_uris * base, lrdf_uris * add);
static inline lrdf_hash lrdf_gen_hash(const char *str);
void lrdf_free_string_hash(lrdf_string_hash * h[]);
void lrdf_free_triple_hash(lrdf_triple_hash * h[]);
void lrdf_free_closure_hash(lrdf_closure_hash * h[]);

static inline lrdf_hash lrdf_gen_hash(const char *str)
{
    lrdf_hash data[2];

    md5_buffer(str, strlen(str), data);

    return data[0];
}

void lrdf_init()
{
    unsigned int i;
    struct timeval tv;

    raptor_init();
    lrdf_more_triples(256);

    /* A UID to add to genids to make them safer */
    gettimeofday(&tv, NULL);
    lrdf_uid = (unsigned int) getpid();
    lrdf_uid ^= (unsigned int) tv.tv_usec;

    /* Global value for the hash of rdf:Resource, saves time */
    rdf_resource_h = lrdf_gen_hash(RDF_RESOURCE);

    /* Make sure all the hashes are empty, just incase */
    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	resources_hash[i] = NULL;
	literals_hash[i] = NULL;
	subj_hash[i] = NULL;
	obj_hash[i] = NULL;
	pred_hash[i] = NULL;
	subclass_hash[i] = NULL;
	superclass_hash[i] = NULL;
    }

    /* Make sure we have rdf:Resource in our hash tables */
    lrdf_check_hash(resources_hash, rdf_resource_h, RDF_RESOURCE);
}

void lrdf_more_triples(int count)
{
    int i;
    lrdf_statement *new;

    new = (lrdf_statement *) calloc(count, sizeof(lrdf_statement));
    for (i = 0; i < count - 1; i++) {
	new[i].next = new + i + 1;
    }
    new[count - 1].next = free_triples;
    free_triples = new;
}

void lrdf_cleanup()
{
    raptor_finish();

    lrdf_free_string_hash(resources_hash);
    lrdf_free_string_hash(literals_hash);
    lrdf_free_triple_hash(subj_hash);
    lrdf_free_triple_hash(obj_hash);
    lrdf_free_triple_hash(pred_hash);
    lrdf_free_closure_hash(subclass_hash);
    lrdf_free_closure_hash(superclass_hash);
}

lrdf_statement *lrdf_alloc_statement()
{
    lrdf_statement *s;

    if (free_triples == NULL) {
	lrdf_more_triples(256);
    }
    s = free_triples;
    free_triples = free_triples->next;
    s->next = NULL;

    return s;
}

void lrdf_free_statements(lrdf_statement * s)
{
    lrdf_statement *next;

    for (; s != NULL; s = next) {
	next = s->next;
	s->next = free_triples;
	free_triples = s;
    }
}

void lrdf_add_triple(const char *source, const char *subject, const char
		     *predicate, const char *object,
		     enum lrdf_objtype literal)
{
    lrdf_statement *s = lrdf_alloc_statement();

    s->shash = lrdf_gen_hash(subject);
    s->phash = lrdf_gen_hash(predicate);
    s->ohash = lrdf_gen_hash(object);
    s->next = triples;
    triples = s;

    s->subject = lrdf_check_hash(resources_hash, s->shash, subject);
    s->predicate = lrdf_check_hash(resources_hash, s->phash, predicate);
    if (literal == lrdf_literal) {
	s->object = lrdf_check_hash(literals_hash, s->ohash, object);
	s->object_type = lrdf_literal;
    } else {
	s->object = lrdf_check_hash(resources_hash, s->ohash, object);
	s->object_type = lrdf_uri;
    }

    lrdf_add_triple_hash(subj_hash, s->shash, s);
    lrdf_add_triple_hash(obj_hash, s->ohash, s);
    lrdf_add_triple_hash(pred_hash, s->phash, s);

    if (source) {
	s->source = lrdf_gen_hash(source);
    } else {
	s->source = 0;
    }
}

void lrdf_remove_uri_matches(const char *uri)
{
    lrdf_statement p;

    p.subject = (char *)uri;
    p.predicate = NULL;
    p.object = NULL;
    lrdf_remove_matches(&p);
    p.subject = NULL;
    p.predicate = (char *)uri;
    lrdf_remove_matches(&p);
    p.predicate = NULL;
    p.object = (char *)uri;
    lrdf_remove_matches(&p);

    /* we could also remove the hash of the uri from the lookup tables, but we
     * don't. natch */
}

void lrdf_remove_matches(lrdf_statement *pattern)
{
    lrdf_statement *s;
    lrdf_statement *it;

    while ((s = lrdf_one_match(pattern))) {
	/* If the head triple is the one we want to remove */
	if (triples == s) {
	    triples = s->next;
	    lrdf_remove_triple_hash(subj_hash, s->shash, s);
	    lrdf_remove_triple_hash(pred_hash, s->phash, s);
	    lrdf_remove_triple_hash(obj_hash, s->ohash, s);
	    s->next = NULL;
	    lrdf_free_statements(s);
	    continue;
	}

	/* Else its somwehere in the tail of the list */
	for (it = triples; it; it = it->next) {
	    if (it->next == s) {
		it->next = it->next->next;
		lrdf_remove_triple_hash(subj_hash, s->shash, s);
		lrdf_remove_triple_hash(pred_hash, s->phash, s);
		lrdf_remove_triple_hash(obj_hash, s->ohash, s);
		s->next = NULL;
		lrdf_free_statements(s);
		break;
	    }
	}
    }
}

static void lrdf_store(void *user_data, const raptor_statement * statement)
{
    lrdf_statement *s = lrdf_alloc_statement();
    char tmps[128], tmpp[128], tmpo[128];
    char *subj = (char *) statement->subject,
	 *pred = (char *) statement->predicate,
	 *obj = (char *) statement->object;

    if (statement->subject_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
	snprintf(tmps, 127, "_:%s.%x", subj, lrdf_uid);
	subj = tmps;
    }
    if (statement->predicate_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
	snprintf(tmpp, 127, "_:%s.%x", pred, lrdf_uid);
	pred = tmpp;
    }
    if (statement->object_type == RAPTOR_IDENTIFIER_TYPE_ANONYMOUS) {
	snprintf(tmpo, 127, "_:%s.%x", obj, lrdf_uid);
	obj = tmpo;
    }

    s->shash = lrdf_gen_hash(subj);
    s->phash = lrdf_gen_hash(pred);
    s->ohash = lrdf_gen_hash(obj);
    s->next = triples;
    triples = s;

    s->subject = lrdf_check_hash(resources_hash, s->shash, subj);
    s->predicate = lrdf_check_hash(resources_hash, s->phash, pred);
    if (statement->object_type == RAPTOR_IDENTIFIER_TYPE_LITERAL) {
	s->object = lrdf_check_hash(literals_hash, s->ohash, obj);
	s->object_type = lrdf_literal;
    } else {
	s->object = lrdf_check_hash(resources_hash, s->ohash, obj);
	s->object_type = lrdf_uri;
    }

    lrdf_add_triple_hash(subj_hash, s->shash, s);
    lrdf_add_triple_hash(obj_hash, s->ohash, s);
    lrdf_add_triple_hash(pred_hash, s->phash, s);

    s->source = *((lrdf_hash *) user_data);
}

static char *lrdf_check_hash(lrdf_string_hash ** tbl, lrdf_hash hash, const char
			     *str)
{
    lrdf_string_hash *tmp, *newe;
    char *tmps, *newstr;

    if ((tmps = lrdf_find_string_hash(tbl, hash))) {
	return tmps;
    } else {
	tmp = tbl[hash & (LRDF_HASH_SIZE - 1)];
	newstr = strdup(str);
	newe = (lrdf_string_hash *) malloc(sizeof(lrdf_string_hash));
	newe->hash = hash;
	newe->str = newstr;
	newe->next = tmp;
	tbl[hash & (LRDF_HASH_SIZE - 1)] = newe;

	return newstr;
    }
}

static char *lrdf_find_string_hash(lrdf_string_hash ** tbl, lrdf_hash hash)
{
    lrdf_string_hash *p = tbl[hash & (LRDF_HASH_SIZE - 1)];

    while (p) {
	if (p->hash == hash) {
	    return p->str;
	}
	p = p->next;
    }

    return NULL;
}

static void lrdf_add_triple_hash(lrdf_triple_hash ** tbl, lrdf_hash hash,
				 lrdf_statement * s)
{
    lrdf_triple_hash *p = tbl[hash & (LRDF_HASH_SIZE - 1)];
    lrdf_triple_hash *newe = malloc(sizeof(lrdf_triple_hash));

    newe->hash = hash;
    newe->triple = s;
    newe->next = p;
    tbl[hash & (LRDF_HASH_SIZE - 1)] = newe;
}

static void lrdf_remove_triple_hash(lrdf_triple_hash ** tbl,
				    lrdf_hash hash, lrdf_statement * s)
{
    lrdf_triple_hash *p = tbl[hash & (LRDF_HASH_SIZE - 1)];
    lrdf_triple_hash *it;

    /* The entry we want to remove is the first */
    if (p && p->triple == s) {
	it = p->next;
	free(p);
	tbl[hash & (LRDF_HASH_SIZE - 1)] = it;
	return;
    }

    /* The entry is somewhere in the list */
    for (it = p; it; it = it->next) {
	if (it->next && it->next->triple == s) {
	    p = it->next;
	    it->next = it->next->next;
	    free(p);
	    return;
	}
    }

    fprintf(stderr,
	    "lrdf: tried to remove non-existant triple hash %llx\n", hash);
}

static void lrdf_add_closure_hash(lrdf_closure_hash ** tbl,
				  lrdf_hash subject, lrdf_hash object)
{
    lrdf_closure_hash *p = tbl[subject & (LRDF_HASH_SIZE - 1)];
    lrdf_closure_hash *newe = malloc(sizeof(lrdf_closure_hash));

    newe->subject = subject;
    newe->object = object;
    newe->next = p;
    tbl[subject & (LRDF_HASH_SIZE - 1)] = newe;
}

int lrdf_export_by_source(const char *src, const char *file)
{
    lrdf_hash source = lrdf_gen_hash(src);
    lrdf_statement *s;
    const char *outfile = file;
    FILE *out;

    if (!strncasecmp(file, "file:", 5)) {
	outfile = file + 5;
    }
    if (!(out = fopen(outfile, "w"))) {
	fprintf(stderr, "lrdf: trying to write '%s'\n", outfile);
	perror("");
	return -1;
    }

    for (s = triples; s; s = s->next) {
	if (s->source == source) {
	    if (s->object_type == lrdf_uri) {
		fprintf(out, "<%s> <%s> <%s> .\n", s->subject,
			s->predicate, s->object);
	    } else {
		fprintf(out, "<%s> <%s> \"%s\" .\n",
			s->subject, s->predicate, s->object);
	    }
	}
    }
    fclose(out);

    return 0;
}

void lrdf_rebuild_caches()
{
    lrdf_rebuild_taxonomic_closure(subclass_hash, superclass_hash);
}

void lrdf_rebuild_taxonomic_closure(lrdf_closure_hash ** fwd_tbl,
				    lrdf_closure_hash ** rev_tbl)
{
    lrdf_string_hash *tmp[LRDF_HASH_SIZE];
    lrdf_string_hash *hit;
    char **uris;
    int *pathto;
    lrdf_statement q;
    lrdf_statement *m;
    lrdf_statement *it;
    unsigned int class_count = 0;
    unsigned int i, j, k;

    /* Ensure the tmp table is cleared out */
    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	tmp[i] = NULL;
    }

    /* Find all explicitly named classes */
    q.subject = NULL;
    q.predicate = RDF_TYPE;
    q.object = RDFS_CLASS;
    m = lrdf_matches(&q);
    for (it = m; it; it = it->next) {
	lrdf_check_hash(tmp, it->shash, it->subject);
    }
    lrdf_free_statements(m);

    /* Find all implicitly name classes */
    q.subject = NULL;
    q.predicate = RDFS_SUBCLASSOF;
    q.object = NULL;
    m = lrdf_matches(&q);
    for (it = m; it != NULL; it = it->next) {
	lrdf_check_hash(tmp, it->shash, it->subject);
	lrdf_check_hash(tmp, it->ohash, it->object);
    }

    /* Count unique class uris */
    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	for (hit = tmp[i]; hit; hit = hit->next) {
	    class_count++;
	}
    }

    uris = malloc(class_count * sizeof(char *));
    class_count = 0;
    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	for (hit = tmp[i]; hit; hit = hit->next) {
	    uris[class_count] = hit->str;
	    hit->str = (char *) class_count++;
	}
    }

    pathto = calloc(class_count * class_count, sizeof(int));
    for (it = m; it != NULL; it = it->next) {
	/* The subclass is the matrix column */
	int c = (int) lrdf_find_string_hash(tmp, it->shash);
	/* And the superclass is the row */
	int r = (int) lrdf_find_string_hash(tmp, it->ohash);

	pathto[c + class_count * r] = 1;
    }
    lrdf_free_statements(m);

    /* Warshall's algorithm
     *
     * $adjacent[X][Z] and $adjacent[Z][Y] => $adjacent[X][Y]
     */

    for (k = 0; k < class_count; k++) {
	for (i = 0; i < class_count; i++) {
	    for (j = 0; j < class_count; j++) {
		if (pathto[i + class_count * j] != 1) {
		    pathto[i + class_count * j] =
			pathto[i + class_count * k] &&
			pathto[k + class_count * j];
		}
	    }
	}
    }

    /* Clear out and free the forward and reverse tables */
    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	lrdf_closure_hash *next;
	lrdf_closure_hash *hit;

	for (hit = fwd_tbl[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit);
	}
	fwd_tbl[i] = NULL;
	for (hit = rev_tbl[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit);
	}
	rev_tbl[i] = NULL;
    }

    for (i = 0; i < class_count; i++) {
	lrdf_hash class_h = lrdf_gen_hash(uris[i]);
	lrdf_hash subclass_h;

	/* Every class is a subclass of itsself */
	lrdf_add_closure_hash(fwd_tbl, class_h, class_h);
	lrdf_add_closure_hash(rev_tbl, class_h, class_h);

	/* ...and rdf:Resource */
	lrdf_add_closure_hash(fwd_tbl, rdf_resource_h, class_h);
	lrdf_add_closure_hash(rev_tbl, class_h, rdf_resource_h);

	for (j = 0; j < class_count; j++) {
	    subclass_h = lrdf_gen_hash(uris[j]);
	    if (pathto[j + class_count * i]) {
		lrdf_add_closure_hash(fwd_tbl, class_h, subclass_h);
		lrdf_add_closure_hash(rev_tbl, subclass_h, class_h);
	    }
	}
    }

    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	lrdf_string_hash *next;
	lrdf_string_hash *hit;

	for (hit = tmp[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit);
	}
    }

    for (i = 0; i < class_count; i++) {
	free(uris[i]);
    }
    free(uris);
    free(pathto);
}

static void lrdf_error_handler(void *data, raptor_locator * locator,
			       const char *message);

static void lrdf_error_handler(void *data, raptor_locator * locator,
			       const char *message)
{
    fprintf(stderr, "liblrdf: error - ");
    raptor_print_locator(stderr, locator);
    fprintf(stderr, " - %s\n", message);

    raptor_parse_abort((raptor_parser*)data);
}

static void lrdf_warning_handler(void *data, raptor_locator * locator,
				 const char *message);

static void lrdf_warning_handler(void *data, raptor_locator * locator,
				 const char *message)
{
    fprintf(stderr, "liblrdf: warning - ");
    raptor_print_locator(stderr, locator);
    fprintf(stderr, " - %s\n", message);
}


int lrdf_read_files(const char *uri[])
{
    unsigned int i;

    for (i = 0; uri[i] != NULL; i++) {
	if (lrdf_read_file_intl(uri[i]) != 0) {
	    return 1;
	}
    }
    lrdf_rebuild_caches();

    return 0;
}

int lrdf_read_file(const char *uri)
{
    int ret;

    ret = lrdf_read_file_intl(uri);
    lrdf_rebuild_caches();

    return ret;
}

int lrdf_read_file_intl(const char *uri)
{
    raptor_parser *parser = NULL;
    raptor_uri *ruri, *furi;
    lrdf_hash source;

    //printf("lrdf: reading %s\n", uri);
    ruri = raptor_new_uri(uri);
    furi = raptor_new_uri(uri);
    source = lrdf_gen_hash(uri);
    lrdf_check_hash(resources_hash, source, uri);

    if (strstr(uri, ".rdf")) {
	parser = raptor_new_parser("rdfxml");
    } else {
	parser = raptor_new_parser("ntriples");
    }
    if (!parser) {
	fprintf(stderr, "liblrdf: failed to create parser\n");
	raptor_free_uri(ruri);
	return 1;
    }

    raptor_set_error_handler(parser, parser, lrdf_error_handler);
    raptor_set_warning_handler(parser, NULL, lrdf_warning_handler);
    raptor_set_statement_handler(parser, &source, lrdf_store);
    raptor_set_default_generate_id_parameters(parser, NULL, ++lrdf_uid);

    if (raptor_parse_file(parser, furi, ruri)) {
	raptor_free_uri(furi);
	raptor_free_uri(ruri);
	raptor_free_parser(parser);
	return 1;
    }

    raptor_free_uri(ruri);
    raptor_free_parser(parser);

    return 0;
}

char *lrdf_get_default_uri(unsigned long id)
{
    lrdf_statement *types;
    lrdf_statement *it;
    lrdf_statement type_s;
    lrdf_statement plugin_s;
    char *uri = NULL;
    char plugin_uri[64];

    snprintf(plugin_uri, 64, "http://ladspa.org/ontology#%ld", id);
    type_s.subject = NULL;
    type_s.predicate = RDF_TYPE;
    type_s.object_type = lrdf_uri;
    type_s.object = "http://ladspa.org/ontology#Default";
    types = lrdf_matches(&type_s);
    for (it = types; it != NULL; it = it->next) {
	plugin_s.subject = plugin_uri;
	plugin_s.predicate = LADSPA_BASE "hasSetting";
	plugin_s.object = it->subject;
	if (lrdf_exists_match(&plugin_s)) {
	    uri = it->subject;
	    break;
	}
    }
    lrdf_free_statements(types);

    return uri;
}

lrdf_uris *lrdf_get_setting_uris(unsigned long id)
{
    lrdf_statement *settings;
    lrdf_statement *it;
    lrdf_statement plugin_s;
    lrdf_uris *ret;
    char **uris;
    char plugin_uri[64];
    int scnt = 0;

    snprintf(plugin_uri, 64, "http://ladspa.org/ontology#%ld", id);
    plugin_s.subject = plugin_uri;
    plugin_s.predicate = LADSPA_BASE "hasSetting";
    plugin_s.object = NULL;
    settings = lrdf_matches(&plugin_s);
    for (it = settings; it != NULL; it = it->next) {
	scnt++;
    }

    ret = malloc(sizeof(lrdf_uris));
    uris = (char **) calloc(scnt + 1, sizeof(char **));
    ret->items = uris;

    for (it = settings, scnt = 0; it != NULL; it = it->next) {
	uris[scnt++] = it->object;
    }
    lrdf_free_statements(settings);
    ret->count = scnt;

    return ret;
}

lrdf_defaults *lrdf_get_setting_values(const char *uri)
{
    lrdf_statement *portvalues;
    lrdf_statement *it;
    lrdf_statement *port;
    lrdf_statement portv_s;
    lrdf_statement port_s;
    lrdf_defaults *ret;
    lrdf_portvalue *list;
    int pvcount = 0;
    char *pos;
    char *port_uri;

    if (!uri) {
	return NULL;
    }

    /* Find portvalues associated with setting URI */
    portv_s.subject = (char *)uri;
    portv_s.predicate = LADSPA_BASE "hasPortValue";
    portv_s.object = NULL;
    portvalues = lrdf_matches(&portv_s);

    for (it = portvalues; it != NULL; it = it->next) {
	pvcount++;
    }
    if (pvcount == 0) {
	return NULL;
    }

    ret = (lrdf_defaults *) calloc(1, sizeof(lrdf_defaults));
    list = (lrdf_portvalue *) calloc(pvcount, sizeof(lrdf_portvalue));
    ret->count = pvcount;
    ret->items = list;

    for (it = portvalues, pvcount = 0; it != NULL;
	 it = it->next, pvcount++) {
	/* Find setting's port */
	port_s.subject = it->object;
	port_s.predicate = LADSPA_BASE "forPort";
	port_s.object = NULL;
	port = lrdf_one_match(&port_s);
	if (port != NULL) {
	    port_uri = port->object;
	    pos = strrchr(port_uri, '.');
	    list[pvcount].pid = atoi(pos + 1);

	    /* Find port's set value */
	    port_s.predicate = RDF_BASE "value";
	    port = lrdf_one_match(&port_s);
	    if (port != NULL) {
		list[pvcount].value = atof(port->object);
	    }

	    /* Find port's short name */
	    port_s.subject = port_uri;
	    port_s.predicate = LADSPA_BASE "hasLabel";
	    port_s.object = NULL;
	    port = lrdf_one_match(&port_s);
	    if (port != NULL && port->object != NULL) {
		list[pvcount].label = port->object;
	    }
	}
    }

    return ret;
}

lrdf_defaults *lrdf_get_scale_values(unsigned long id, unsigned long port)
{
    char port_uri[128];
    lrdf_statement scale_p;
    lrdf_statement *scale_s;
    char *scale_uri;
    lrdf_statement p1;
    lrdf_uris *ulist;
    lrdf_defaults *ret;
    lrdf_portvalue *list;
    int i;

    snprintf(port_uri, 127, LADSPA_BASE "%ld.%ld", id, port);

    /* Find Scale associated with port */
    scale_p.subject = port_uri;
    scale_p.predicate = LADSPA_BASE "hasScale";
    scale_p.object = NULL;
    scale_s = lrdf_matches(&scale_p);

    if (!scale_s) {
	return NULL;
    }

    scale_uri = scale_s->object;

    p1.subject = scale_uri;
    p1.predicate = LADSPA_BASE "hasPoint";
    p1.object = "?";
    p1.next = NULL;
    ulist = lrdf_match_multi(&p1);
    if (!ulist) {
	return NULL;
    }

    ret = (lrdf_defaults *) calloc(1, sizeof(lrdf_defaults));
    list = (lrdf_portvalue *) calloc(ulist->count, sizeof(lrdf_portvalue));
    ret->count = ulist->count;
    ret->items = list;

    for (i=0; i < ulist->count; i++) {
	list[i].pid = port;

	scale_p.subject = ulist->items[i];
	scale_p.predicate = RDF_BASE "value";
	scale_p.object = NULL;
	scale_s = lrdf_one_match(&scale_p);
	list[i].value = atof(scale_s->object);

	scale_p.predicate = LADSPA_BASE "hasLabel";
	scale_s = lrdf_one_match(&scale_p);
	list[i].label = scale_s->object;
    }

    return ret;
}

void lrdf_free_setting_values(lrdf_defaults * def)
{
    if (def) {
	free(def->items);
	free(def);
    }
}

char *lrdf_get_setting_metadata(const char *uri, const char *element)
{
    lrdf_statement meta_s;
    lrdf_statement *m;
    char dc_uri[128];

    snprintf(dc_uri, 128, DC_BASE "%s", element);
    meta_s.subject = (char *)uri;
    meta_s.predicate = dc_uri;
    meta_s.object = NULL;

    m = lrdf_one_match(&meta_s);
    if (m) {
	return m->object;
    }

    return NULL;
}

/* lrdf_free_uris:
 *
 * Called on the return values from lrdf_get_subclasses etc. to free up the
 * memory allocated by them.
 */

void lrdf_free_uris(lrdf_uris * u)
{
    if (u) {
	free(u->items);
	free(u);
    }
}

static lrdf_uris *lrdf_uris_new(int size)
{
    lrdf_uris *nu;

    nu = malloc(sizeof(lrdf_uris));
    nu->items = malloc(size * sizeof(char *));
    nu->size = size;
    nu->count = 0;

    return nu;
}

static void lrdf_uris_append(lrdf_uris * base, lrdf_uris * add)
{
    unsigned int i;

    if (!add) {
	return;
    }

    if (base->count + add->count > base->size) {
	base->size *= 2;
	base->items = realloc(base->items, base->size);
    }
    for (i = 0; i < add->count; i++) {
	base->items[i + base->count] = add->items[i];
    }
    base->count += add->count;
}

/* lrdf_get_subclasses
 *
 * Returns a list of the direct subclasses of a given class
 */

lrdf_uris *lrdf_get_subclasses(const char *uri)
{
    lrdf_statement sc_s;
    lrdf_statement *m;
    lrdf_statement *it;
    lrdf_uris *ret;
    char **uris;
    int count = 0;

    ret = malloc(sizeof(lrdf_uris));
    uris = malloc(256 * sizeof(char *));
    ret->items = uris;

    sc_s.subject = NULL;
    sc_s.predicate = RDFS_BASE "subClassOf";
    sc_s.object = (char *)uri;
    m = lrdf_matches(&sc_s);
    if (m == NULL) {
	free(ret);
	free(uris);

	return NULL;
    }
    for (it = m; it != NULL; it = it->next) {
	uris[count++] = it->subject;
    }
    lrdf_free_statements(m);
    ret->count = count;

    return ret;
}

/* lrdf_get_all_subclasses:
 *
 * Returns a list of all the subclasses of uri
 */
lrdf_uris *lrdf_get_all_subclasses(const char *uri)
{
    lrdf_uris *ret;
    lrdf_closure_hash *ch;
    lrdf_closure_hash *hit;
    lrdf_hash class;
    int count = 0;

    ret = malloc(sizeof(lrdf_uris));

    class = lrdf_gen_hash(uri);
    ch = subclass_hash[class & (LRDF_HASH_SIZE - 1)];
    for (hit = ch; hit; hit = hit->next) {
	if (class == hit->subject) {
	    count++;
	}
    }
    if (count == 0) {
	return NULL;
    }
    ret = lrdf_uris_new(count);
    ret->count = count;
    count = 0;
    for (hit = ch; hit; hit = hit->next) {
	if (class == hit->subject) {
	    ret->items[count++] =
		lrdf_find_string_hash(resources_hash, hit->object);
	}
    }

    return ret;
}

/* lrdf_get_all_superclasses:
 *
 * Returns a list of all the superlasses of uri
 */
lrdf_uris *lrdf_get_all_superclasses(const char *uri)
{
    lrdf_uris *ret;
    lrdf_closure_hash *ch;
    lrdf_closure_hash *hit;
    lrdf_hash class;
    int count = 0;

    ret = malloc(sizeof(lrdf_uris));

    class = lrdf_gen_hash(uri);
    ch = superclass_hash[class & (LRDF_HASH_SIZE - 1)];
    for (hit = ch; hit; hit = hit->next) {
	if (class == hit->subject) {
	    count++;
	}
    }
    if (count == 0) {
	return NULL;
    }
    ret = lrdf_uris_new(count);
    ret->count = count;
    count = 0;
    for (hit = ch; hit; hit = hit->next) {
	if (class == hit->subject) {
	    ret->items[count++] =
		lrdf_find_string_hash(resources_hash, hit->object);
	}
    }

    return ret;
}

/* lrdf_get_instances
 *
 * Returns a list of the instances of a given class
 */

lrdf_uris *lrdf_get_instances(const char *uri)
{
    lrdf_statement inst_s;
    lrdf_statement *m;
    lrdf_statement *it;
    lrdf_uris *ret;
    char **uris;
    int count = 0;

    ret = lrdf_uris_new(256);
    uris = ret->items;

    inst_s.subject = NULL;
    inst_s.predicate = RDF_BASE "type";
    inst_s.object = (char *)uri;
    m = lrdf_matches(&inst_s);
    if (m == NULL) {
	free(ret);
	free(uris);

	return NULL;
    }
    for (it = m; it != NULL; it = it->next) {
	uris[count++] = it->subject;
    }
    lrdf_free_statements(m);
    ret->count = count;

    return ret;
}

/* lrdf_get_all_instances:
 *
 * Returns the URIs of all the instances of 'uri' and all the instances of all
 * its subclasses.
 */

lrdf_uris *lrdf_get_all_instances(const char *uri)
{
    unsigned int i;
    lrdf_uris *u, *v;
    lrdf_uris *ret = NULL;

    u = lrdf_get_all_subclasses(uri);
    if (u->count > 0) {
	ret = lrdf_uris_new(256);
	for (i = 0; i < u->count; i++) {
	    v = lrdf_get_instances(u->items[i]);
	    lrdf_uris_append(ret, v);
	    lrdf_free_uris(v);
	}
    }

    return ret;
}

char *lrdf_get_label(const char *uri)
{
    lrdf_statement lab_s;
    lrdf_statement *label;

    lab_s.subject = (char *)uri;
    lab_s.predicate = LADSPA_BASE "hasLabel";
    lab_s.object = NULL;
    label = lrdf_one_match(&lab_s);

    if (label == NULL) {
	return NULL;
    }

    return label->object;
}

/* XXX nasty hack */

unsigned long lrdf_get_uid(const char *uri)
{
    char *pos;

    pos = strrchr(uri, '#');
    if (pos != NULL) {
	return atol(pos + 1);
    }

    return 0;
}

/* lrdf_matches:
 *
 * Returns a NULL terminated vector of lrdf_statements that match the
 * sepecifed pattern, where a NULL in any position matches any uri.
 */

lrdf_statement *lrdf_matches(lrdf_statement * pattern)
{
    lrdf_triple_hash *th;
    lrdf_triple_hash *start;
    lrdf_statement *s;
    lrdf_statement *ret = NULL;

#ifdef DEBUG
    printf("Looking for (%s, %s, %s)\n", pattern->subject,
	   pattern->predicate, pattern->object);
#endif

    if (pattern->subject) {
	pattern->shash = lrdf_gen_hash(pattern->subject);
    }
    if (pattern->predicate) {
	pattern->phash = lrdf_gen_hash(pattern->predicate);
    }
    if (pattern->object) {
	pattern->ohash = lrdf_gen_hash(pattern->object);
    }

    if (pattern->subject) {
	start = subj_hash[pattern->shash & (LRDF_HASH_SIZE - 1)];
    } else if (pattern->predicate) {
	start = pred_hash[pattern->phash & (LRDF_HASH_SIZE - 1)];
    } else if (pattern->object) {
	start = obj_hash[pattern->ohash & (LRDF_HASH_SIZE - 1)];
    } else {
	/* None of the triple parts were specified, can't do anything
	 * useful with that, except return everything and that is
	 * stupid. If you want everything look thorugh the list */
	fprintf(stderr, "lrdf: null triple specified for search\n");
	return NULL;
    }

    for (th = start; th; th = th->next) {
	s = th->triple;

	if ((pattern->subject == NULL ||
	     pattern->shash == s->shash) &&
	    (pattern->predicate == NULL ||
	     pattern->phash == s->phash) &&
	    (pattern->object == NULL || pattern->ohash == s->ohash)) {
	    lrdf_statement *new = lrdf_alloc_statement();

#ifdef DEBUG
	    printf("Found (%s, %s, %s)\n", pattern->subject,
		   pattern->predicate, pattern->object);
	    printf("  =   (%s, %s, %s)\n", s->subject, s->predicate,
		   s->object);
	    printf("      (%llx, %llx, %llx)\n", pattern->shash,
		   pattern->phash, pattern->ohash);
	    printf("  =   (%llx, %llx, %llx)\n", s->shash, s->phash,
		   s->ohash);
#endif
	    lrdf_copy_statement(s, new);
	    new->next = ret;
	    ret = new;
	}
    }

    return ret;
}

/* lrdf_one_match:
 *
 * returns a pointer to the first matching triple if one exists, or NULL
 * otherwise
 */

lrdf_statement *lrdf_one_match(lrdf_statement *pattern)
{
    lrdf_triple_hash *th;
    lrdf_triple_hash *start;
    lrdf_statement *s;

    if (pattern->subject) {
	pattern->shash = lrdf_gen_hash(pattern->subject);
    }
    if (pattern->predicate) {
	pattern->phash = lrdf_gen_hash(pattern->predicate);
    }
    if (pattern->object) {
	pattern->ohash = lrdf_gen_hash(pattern->object);
    }

    if (pattern->subject) {
	start = subj_hash[pattern->shash & (LRDF_HASH_SIZE - 1)];
    } else if (pattern->predicate) {
	start = pred_hash[pattern->phash & (LRDF_HASH_SIZE - 1)];
    } else if (pattern->object) {
	start = obj_hash[pattern->ohash & (LRDF_HASH_SIZE - 1)];
    } else {
	/* None of the triple parts were specified, can't do anything
	 * useful with that, except return everything and that is
	 * stupid. If you want everything look thorugh the list */
	fprintf(stderr, "lrdf: null triple specified for search\n");
	return NULL;
    }

    for (th = start; th; th = th->next) {
	s = th->triple;

	if ((pattern->subject == NULL ||
	     pattern->shash == s->shash) &&
	    (pattern->predicate == NULL ||
	     pattern->phash == s->phash) &&
	    (pattern->object == NULL || pattern->ohash == s->ohash)) {
	    return s;
	}
    }

    return NULL;
}

/* lrdf_exists_match:
 *
 * returns true if a triple mathcing the pattern exists, false otherwise
 */

int lrdf_exists_match(lrdf_statement *pattern)
{
    return (lrdf_one_match(pattern) != NULL);
}

/* lrdf_copy_statement:
 *
 * copies the subject, predicate and object of a statement to another
 * statement. does not affect the linked list pointer.
 */

void lrdf_copy_statement(lrdf_statement * from, lrdf_statement * to)
{
    to->subject = from->subject;
    to->predicate = from->predicate;
    to->object = from->object;
    to->object_type = from->object_type;
    to->shash = from->shash;
    to->phash = from->phash;
    to->ohash = from->ohash;
}

void lrdf_free_string_hash(lrdf_string_hash * h[])
{
    unsigned int i;

    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	lrdf_string_hash *next;
	lrdf_string_hash *hit;

	for (hit = h[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit->str);
	    free(hit);
	}
    }
}

void lrdf_free_triple_hash(lrdf_triple_hash * h[])
{
    unsigned int i;

    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	lrdf_triple_hash *next;
	lrdf_triple_hash *hit;

	for (hit = h[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit);
	}
    }
}

void lrdf_free_closure_hash(lrdf_closure_hash * h[])
{
    unsigned int i;

    for (i = 0; i < LRDF_HASH_SIZE; i++) {
	lrdf_closure_hash *next;
	lrdf_closure_hash *hit;

	for (hit = h[i]; hit; hit = next) {
	    next = hit->next;
	    free(hit);
	}
    }
}

lrdf_statement *lrdf_all_statements()
{
    return triples;
}

char* lrdf_add_preset(const char *source, const char *label, unsigned long id,
                      lrdf_defaults *vals)
{
    char plugin_uri[64];
    char* setting_uri;
    static int sid = 0;
    int i;
    setting_uri = malloc(64 * sizeof(char));

    snprintf(plugin_uri, 64, "http://ladspa.org/ontology#%ld", id);
    snprintf(setting_uri, 64, "http://plugin.org.uk/genid#%d.%d", lrdf_uid, sid++);

    lrdf_add_triple(source, plugin_uri, LADSPA_BASE "hasSetting", setting_uri,
		    lrdf_uri);
    lrdf_add_triple(source, setting_uri, RDF_BASE "type", LADSPA_BASE "Preset",
		    lrdf_uri);
    lrdf_add_triple(source, setting_uri, LADSPA_BASE "hasLabel", label,
		    lrdf_literal);

    for (i=0; i<vals->count; i++) {
	char value_uri[64];
	char port_uri[64];
	char value_lit[64];
	snprintf(value_uri, 64, "http://plugin.org.uk/genid#%d.%d", lrdf_uid,
		 sid++);
	snprintf(port_uri, 64, "%s.%ld", plugin_uri, vals->items[i].pid);
	snprintf(value_lit, 64, "%f", vals->items[i].value);

	lrdf_add_triple(source, setting_uri, LADSPA_BASE "hasPortValue",
			value_uri, lrdf_uri);
	lrdf_add_triple(source, value_uri, RDF_BASE "value",
			value_lit, lrdf_literal);
	lrdf_add_triple(source, value_uri, LADSPA_BASE "forPort",
			port_uri, lrdf_uri);
    }

    return setting_uri;
}

/* vi:set ts=8 sts=4 sw=4: */
