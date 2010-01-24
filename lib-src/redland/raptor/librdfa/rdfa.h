/**
 * Copyright 2008 Digital Bazaar, Inc.
 *
 * This file is part of librdfa.
 *
 * librdfa is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * librdfa is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with librdfa. If not, see <http://www.gnu.org/licenses/>.
 *
 * The librdfa library is the Fastest RDFa Parser in the Universe. It is
 * a stream parser, meaning that it takes an XML data as input and spits
 * out RDF triples as it comes across them in the stream. Due to this
 * processing approach, librdfa has a very, very small memory footprint.
 * It is also very fast and can operate on hundreds of gigabytes of XML
 * data without breaking a sweat.
 *
 * Usage:
 *
 * rdfacontext* context = rdfa_create_context(base_uri);
 * context->callback_data = your_user_data;
 * rdfa_set_triple_handler(context, triple_function);
 * rdfa_set_buffer_filler(context, buffer_filler_function);
 * rdfa_parse(context);
 * rdfa_destroy_context(context);
 */
#ifndef _LIBRDFA_RDFA_H_
#define _LIBRDFA_RDFA_H_
#include <stdlib.h>

#ifdef LIBRDFA_IN_RAPTOR
#ifdef HAVE_CONFIG_H
#include <raptor_config.h>
#endif

#ifdef WIN32
#include <win32_raptor_config.h>
#endif
#include "raptor.h"
#include "raptor_internal.h"
#else
#include <expat.h>
#endif

#ifdef __cplusplus
extern "C"
{
#endif

#define DEBUG 0

#define RDFA_PARSE_WARNING -2
#define RDFA_PARSE_FAILED -1
#define RDFA_PARSE_UNKNOWN 0
#define RDFA_PARSE_SUCCESS 1

#define MAX_URI_MAPPINGS 512
#define MAX_INCOMPLETE_TRIPLES 1024

#define XMLNS_DEFAULT_MAPPING "XMLNS_DEFAULT"

/**
 * An RDF resource type is used to denote the content of a triple's
 * object value.
 */
typedef enum
{
   RDF_TYPE_NAMESPACE_PREFIX,
   RDF_TYPE_IRI,
   RDF_TYPE_PLAIN_LITERAL,
   RDF_TYPE_XML_LITERAL,
   RDF_TYPE_TYPED_LITERAL,
   RDF_TYPE_UNKNOWN
} rdfresource_t;

/**
 * An RDF triple is the result of an RDFa statement that contains, at
 * the very least, a subject, a predicate and an object. It is the
 * smallest, complete statement one can make in RDF.
 */
typedef struct rdftriple
{
   char* subject;
   char* predicate;
   char* object;
   rdfresource_t object_type;
   char* datatype;
   char* language;
} rdftriple;

/**
 * The specification for a callback that is capable of handling
 * triples. Produces a triple that must be freed once the application
 * is done with the object.
 */
typedef void (*triple_handler_fp)(rdftriple*, void*);

/**
 * The specification for a callback that is capable of handling
 * triples.
 */
typedef size_t (*buffer_filler_fp)(char*, size_t, void*);

/**
 * An RDFA list item is used to hold each datum in an rdfa list. It
 * contains a list of flags as well as the data for the list member.
 */
typedef struct rdfalistitem
{
   unsigned char flags;
   void* data;
} rdfalistitem;

/**
 * An RDFa list is used to store multiple text strings that have a set
 * of attributes associated with them. These can be lists of CURIEs,
 * or lists of incomplete triples. The structure grows with use, but
 * cannot be shrunk.
 */
typedef struct rdfalist
{   
   rdfalistitem** items;
   size_t num_items;
   size_t max_items;
} rdfalist;

/**
 * The RDFa Parser structure is responsible for keeping track of the state of
 * the current RDFa parser. Things such as the default namespace, 
 * CURIE mappings, and other context-specific 
 */
typedef struct rdfacontext
{
   char* base;
   char* parent_subject;
   char* parent_object;
#ifndef LIBRDFA_IN_RAPTOR
   char** uri_mappings;
#endif
   rdfalist* incomplete_triples;
   rdfalist* local_incomplete_triples;
   char* language;

   triple_handler_fp triple_callback;
   buffer_filler_fp buffer_filler_callback;

   unsigned char recurse;
   unsigned char skip_element;
   char* new_subject;
   char* current_object_resource;

   char* content;
   char* datatype;
   rdfalist* property;
   char* plain_literal;
   size_t plain_literal_size;
   char* xml_literal;
   size_t xml_literal_size;

   void* callback_data;

   /* parse state */
   size_t bnode_count;
   char* underscore_colon_bnode_name;
   unsigned char xml_literal_namespaces_inserted;
   size_t wb_allocated;
   char* working_buffer;
   size_t wb_offset;
#ifdef LIBRDFA_IN_RAPTOR
   /* a pointer (in every context) to the error_handlers structure
    * held in the raptor_parser object */
   raptor_error_handlers *error_handlers;
   raptor_uri* base_uri;
   raptor_sax2* sax2;
   raptor_namespace_handler namespace_handler;
   void* namespace_handler_user_data;
#else
   XML_Parser parser;
#endif
   int done;
   rdfalist* context_stack;
   size_t wb_preread;
   int preread;
} rdfacontext;

/**
 * Creates an initial context for RDFa.
 *
 * @param base The base URI that should be used for the parser.
 *
 * @return a pointer to the base RDFa context, or NULL if memory
 *         allocation failed.
 */
rdfacontext* rdfa_create_context(const char* base);

/**
 * Sets the triple handler for the application.
 *
 * @param context the base rdfa context for the application.
 * @param th the triple handler function.
 */
void rdfa_set_triple_handler(rdfacontext* context, triple_handler_fp th);

/**
 * Sets the buffer filler for the application.
 *
 * @param context the base rdfa context for the application.
 * @param bf the buffer filler function.
 */
void rdfa_set_buffer_filler(rdfacontext* context, buffer_filler_fp bf);

/**
 * Starts processing given the base rdfa context.
 *
 * @param context the base rdfa context.
 *
 * @return RDFA_PARSE_SUCCESS if everything went well. RDFA_PARSE_FAILED
 *         if there was a fatal error and RDFA_PARSE_WARNING if there
 *         was a non-fatal error.
 */
int rdfa_parse(rdfacontext* context);

int rdfa_parse_start(rdfacontext* context);

int rdfa_parse_chunk(rdfacontext* context, char* data, size_t wblen, int done);

void rdfa_parse_end(rdfacontext* context);

void rdfa_init_context(rdfacontext* context);

/**
 * Destroys the given rdfa context by freeing all memory associated
 * with the context.
 *
 * @param context the rdfa context.
 */
void rdfa_free_context(rdfacontext* context);

#ifdef __cplusplus
}
#endif

#endif
