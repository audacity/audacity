#ifndef LRDF_H
#define LRDF_H

#ifdef __cplusplus
extern "C" {
#endif

#include <raptor.h>
#include <string.h>
#include <sys/types.h>

#if defined(WIN32)
#include <time.h>
typedef __int64 int64_t;
#define inline __inline
#define strncasecmp strnicmp
#define snprintf sprintf_s
#include <Windows.h>
#include <WinSock.h>
#define EPOCHFILETIME (116444736000000000LL)
struct timezone {
    int tz_minuteswest; /* minutes W of Greenwich */
    int tz_dsttime;     /* type of dst correction */
};
__inline int gettimeofday(struct timeval *tv, struct timezone *tz)
{
    FILETIME        ft;
    LARGE_INTEGER   li;
    __int64         t;
    static int      tzflag;

    if (tv)
    {
        GetSystemTimeAsFileTime(&ft);
        li.LowPart  = ft.dwLowDateTime;
        li.HighPart = ft.dwHighDateTime;
        t  = li.QuadPart;       /* In 100-nanosecond intervals */
        t -= EPOCHFILETIME;     /* Offset to the Epoch time */
        t /= 10;                /* In microseconds */
        tv->tv_sec  = (long)(t / 1000000);
        tv->tv_usec = (long)(t % 1000000);
    }

    if (tz)
    {
        if (!tzflag)
        {
            _tzset();
            tzflag++;
        }
        tz->tz_minuteswest = _timezone / 60;
        tz->tz_dsttime = _daylight;
    }

    return 0;
}
#endif

#include "lrdf_types.h"

// #define DEBUG 1

#define RDF_BASE "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
#define RDF_TYPE RDF_BASE "type"
#define RDF_RESOURCE RDF_BASE "Resource"
#define RDFS_BASE "http://www.w3.org/2000/01/rdf-schema#"
#define RDFS_CLASS RDFS_BASE "Class"
#define RDFS_SUBCLASSOF RDFS_BASE "subClassOf"
#define LADSPA_BASE "http://ladspa.org/ontology#"
#define DC_BASE "http://purl.org/dc/elements/1.1/"

/* Data functions */

/* Call lrdf_init before using any of the functions below */

void lrdf_init();

/* Call this at the end to clean up the caches and so on */

void lrdf_cleanup();

/* Call lrdf_read_files to read a set of files, it is more efficient than 
 * calling lrdf_read_file repeatedly
 *
 * Pass in the files as file: uri, eg
 * "file:///usr/local/share/ladspa/rdf/ladspa.rdfs". This URI will be written
 * stored as the source of the triple. See lrdf_add_triple, and
 * lrdf_export_by_source for uses for this.
 */

int lrdf_read_files(const char *uri[]);

/* lrdf_read_file:
 *
 * Loads the file specifed by uri into the internal representation.
 *
 * uri can point to either an RDF-XML .rdf file or an RDF-Ntriples .n3 file.
 * the source of the triples will be set to the file uri. c.f. lrdf_read_file
 */

int lrdf_read_file(const char *uri);

/* lrdf_add_triple:
 *
 * This is a way to manually add a triple to the store.
 *
 * The source argument is a way of grouping triples so that they can be
 * exported later. If you do not care set it to NULL. See examples/add_test.c
 * for an example.
 *
 * The subject and predicate arguments are fully qualified URIs for those
 * parts. object may also be a string, if indicated by literal.
 *
 * The literal argument specifies wether the object argument is to be
 * interpreted as a URI or Literal. Set it to lrdf_literal for literals and
 * lrdf_uri for uris.
 *
 * NB If you add any triples relating to the taxonomy (Classes) you /must/
 *    run lrdf_rebuild_caches() afterwards, otherwise you will get odd results
 *    from queries.
 */

void lrdf_add_triple(const char *source, const char *subject, const char
				*predicate, const char *object, enum
				lrdf_objtype literal);

/* lrdf_add_preset:
 *
 * This function adds a preset object to the store.
 *
 * source must be a unique URI that represents the setting object (I recommend
 * the file: URI it will be saved under), label is a name for the preset, ID is
 * the UID number of the plugin and vals is a pointer to a lrdf_defaults
 * structure containing the values to be written.
 *
 * returns the URI of the added preset.  The caller is responsible for freeing
 * it.
 */

char* lrdf_add_preset(const char *source, const char *label, unsigned long id,
                      lrdf_defaults *vals);

/* lrdf_remove_matches:
 *
 * This function will remove all triples from the store that match the
 * specified pattern.
 *
 * NB. It is horribly inefficient! Currently O(N) complexity. If you need to
 * make extensive use of it please contact me and I will look at restructuring
 * the data to make it faster.
 */

void lrdf_remove_matches(lrdf_statement *pattern);

/* lrdf_remove_uri_matches:
 *
 * This function will remove all triples contating the URI passed in as an
 * argument. 
 */

void lrdf_remove_uri_matches(const char *uri);

/* lrdf_rebuild_caches
 *
 * The rebuilds the taxonomic closue matrix thats used to accelerate taxonomic
 * inference queries. User should only need to call this after manually adding
 * triples with lrdf_add_triple.
 */

void lrdf_rebuild_caches();

/* lrdf_export_by_source
 *
 * writes all the triples associated with the source 'src' to the file 'file',
 * file can be specified as a UNIX path for file: URI. Currently the export
 * format is NTriples (.n3), but it may change to RDF-XML in the future.
 */

int lrdf_export_by_source(const char *src, const char *file);

/* Generic RDF access functions */

/* lrdf_match_multi:
 *
 * Returns a lrdf_uris vector of all the URIs (or Literals) that match the
 * pattern list specified. eg:
 * 
 * ("?", "rdfs:label", NULL), ("?", "foo:bpm", "120")
 *
 * Will return all the values for '?' which match the patterns, ie. have an
 * rdfs:label and have the foo:bpm value of 120.
 */

lrdf_uris *lrdf_match_multi(lrdf_statement *patterns);

/* lrdf_matches:
 *
 * Returns a NULL terminated vector of lrdf_statements that match the
 * sepecifed pattern, where a NULL in any position matches any uri.
 *
 * To free the space allocated call lrdf_free_statements() on the return value;
 */

lrdf_statement *lrdf_matches(lrdf_statement *pattern);

/* lrdf_one_match:
 *
 * returns a pointer to the first matching triple if one exists, or NULL
 * otherwise
 */

lrdf_statement *lrdf_one_match(lrdf_statement *pattern);

/* lrdf_exists_match:
 *
 * returns 1 if a triple mathcing the pattern exists, 0 otherwise
 */

int lrdf_exists_match(lrdf_statement *pattern);

/* lrdf_get_all_superclasses:
 *
 * Returns a list of all the superlasses of uri, including itsself
 */

lrdf_uris *lrdf_get_all_superclasses(const char *uri);

/* lrdf_get_subclasses
 *
 * Returns a list of the direct subclasses of a given class, ie. all classes
 * that have been explicity asserted to be subclasses of uri. Does not
 * (usually) include itsself.
 */

lrdf_uris *lrdf_get_subclasses(const char *uri);

/* lrdf_get_all_subclasses:
 *
 * Returns a list of all the sublasses of uri
 */

lrdf_uris *lrdf_get_all_subclasses(const char *uri);

/* lrdf_get_instances
 *
 * Returns a list of the instances of a given class
 */

lrdf_uris *lrdf_get_instances(const char *uri);

/* lrdf_get_all_instances:
 * 
 * Returns the URIs of all the instances of 'uri' and all the instances of all
 * its subclasses.
 *
 * Doesn't currently remove duplicates, but it could be changed to do that if
 * required.
 */

lrdf_uris *lrdf_get_all_instances(const char *uri);

/* lrdf_all_statements:
 *
 * Returns a pointer to a list of all the triples in the system, DO NOT free
 * the returned list, it is hte real list. */

lrdf_statement *lrdf_all_statements();

/* lrdf_free_uris:
 *
 * Called on the return values from lrdf_get_subclasses etc. to free up the
 * memory allocated by them.
 */

void lrdf_free_uris(lrdf_uris *u);

void lrdf_free_statements(lrdf_statement *s);

/* LADSPA Ontology specific functions */

char *lrdf_get_setting_metadata(const char *uri, const char *element);

char *lrdf_get_default_uri(unsigned long id);

lrdf_uris *lrdf_get_setting_uris(unsigned long id);

unsigned long lrdf_get_uid(const char *uri);

lrdf_defaults *lrdf_get_setting_values(const char *uri);

lrdf_defaults *lrdf_get_scale_values(unsigned long id, unsigned long port);

void lrdf_free_setting_values(lrdf_defaults *def);

char *lrdf_get_label(const char *uri);

#ifdef __cplusplus
}
#endif

#endif
