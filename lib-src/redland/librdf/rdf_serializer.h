/* -*- Mode: c; c-basic-offset: 2 -*-
 *
 * rdf_serializer.h - RDF Serializer Factory / Serializer interfaces and definition
 *
 * Copyright (C) 2002-2008, David Beckett http://www.dajobe.org/
 * Copyright (C) 2002-2005, University of Bristol, UK http://www.bristol.ac.uk/
 * 
 * This package is Free Software and part of Redland http://librdf.org/
 * 
 * It is licensed under the following three licenses as alternatives:
 *   1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
 *   2. GNU General Public License (GPL) V2 or any newer version
 *   3. Apache License, V2.0 or any newer version
 * 
 * You may not use this file except in compliance with at least one of
 * the above three licenses.
 * 
 * See LICENSE.html or LICENSE.txt at the top of this package for the
 * complete terms and further detail along with the license texts for
 * the licenses in COPYING.LIB, COPYING and LICENSE-2.0.txt respectively.
 * 
 * 
 */



#ifndef LIBRDF_SERIALIZER_H
#define LIBRDF_SERIALIZER_H

#ifdef LIBRDF_INTERNAL
#include <rdf_serializer_internal.h>
#endif


#ifdef __cplusplus
extern "C" {
#endif

#include <raptor.h>

/* class methods */
REDLAND_API
void librdf_serializer_register_factory(librdf_world *world, const char *name, const char *label, const char *mime_type, const unsigned char *uri_string, void (*factory) (librdf_serializer_factory*));

REDLAND_API
int librdf_serializer_enumerate(librdf_world* world, const unsigned int counter, const char **name, const char **label);

/* constructor */
REDLAND_API
librdf_serializer* librdf_new_serializer(librdf_world* world, const char *name, const char *mime_type, librdf_uri *type_uri);
REDLAND_API
librdf_serializer* librdf_new_serializer_from_factory(librdf_world* world, librdf_serializer_factory *factory);

/* destructor */
REDLAND_API
void librdf_free_serializer(librdf_serializer *serializer);


/* methods */
REDLAND_API REDLAND_DEPRECATED
int librdf_serializer_serialize_model(librdf_serializer* serializer, FILE *handle, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
int librdf_serializer_serialize_stream_to_file_handle(librdf_serializer* serializer, FILE *handle, librdf_uri* base_uri, librdf_stream *stream);
REDLAND_API
int librdf_serializer_serialize_model_to_file_handle(librdf_serializer* serializer, FILE *handle, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
int librdf_serializer_serialize_stream_to_file(librdf_serializer* serializer, const char *name, librdf_uri* base_uri, librdf_stream* stream);
REDLAND_API
int librdf_serializer_serialize_model_to_file(librdf_serializer* serializer, const char *name, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
unsigned char* librdf_serializer_serialize_stream_to_string(librdf_serializer* serializer, librdf_uri* base_uri, librdf_stream* stream);
REDLAND_API
unsigned char* librdf_serializer_serialize_model_to_string(librdf_serializer* serializer, librdf_uri* base_uri, librdf_model* model);
REDLAND_API
unsigned char* librdf_serializer_serialize_stream_to_counted_string(librdf_serializer* serializer, librdf_uri* base_uri, librdf_stream* stream, size_t *length_p);
REDLAND_API
unsigned char* librdf_serializer_serialize_model_to_counted_string(librdf_serializer* serializer, librdf_uri* base_uri, librdf_model* model, size_t *length_p);
REDLAND_API
int librdf_serializer_serialize_stream_to_iostream(librdf_serializer* serializer, librdf_uri* base_uri, librdf_stream *stream, raptor_iostream* iostr);
REDLAND_API
int librdf_serializer_serialize_model_to_iostream(librdf_serializer* serializer, librdf_uri* base_uri, librdf_model *model, raptor_iostream* iostr);
REDLAND_API
void librdf_serializer_set_error(librdf_serializer* serializer, void *user_data, void (*error_fn)(void *user_data, const char *msg, ...));
REDLAND_API
void librdf_serializer_set_warning(librdf_serializer* serializer, void *user_data, void (*warning_fn)(void *user_data, const char *msg, ...));

REDLAND_API
librdf_node* librdf_serializer_get_feature(librdf_serializer* serializer, librdf_uri *feature);
REDLAND_API
int librdf_serializer_set_feature(librdf_serializer* serializer, librdf_uri *feature, librdf_node* value);
REDLAND_API
int librdf_serializer_set_namespace(librdf_serializer* serializer, librdf_uri *uri, const char *prefix);


#ifdef __cplusplus
}
#endif

#endif
