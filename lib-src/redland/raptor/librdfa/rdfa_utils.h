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
 * This file contains functions used for common rdfa utility functions.
 */
#ifndef _RDFA_UTILS_H_
#define _RDFA_UTILS_H_
#include "rdfa.h"

#ifdef __cplusplus
extern "C"
{
#endif

/**
 * A CURIE type can be safe, unsafe, and Internationalized Resource
 * Identifier, reference-only or invalid.
 */
typedef enum
{
   CURIE_TYPE_SAFE,
   CURIE_TYPE_IRI_OR_UNSAFE,
   CURIE_TYPE_LINK_TYPE,
   CURIE_TYPE_INVALID
}  curie_t;

/**
 * A CURIE parse type lets the CURIE processor know what type of CURIE
 * is being parsed so that the proper namespace resolution may occur.
 */
typedef enum
{
   CURIE_PARSE_ABOUT_RESOURCE,
   CURIE_PARSE_PROPERTY,
   CURIE_PARSE_INSTANCEOF_DATATYPE,
   CURIE_PARSE_HREF_SRC,
   CURIE_PARSE_RELREV   
} curieparse_t;

/**
 * The list member flag type is used to attach attribute information
 * to list member data.
 */
typedef enum
{
   RDFALIST_FLAG_NONE = 0,
   RDFALIST_FLAG_FORWARD = (1 << 1),
   RDFALIST_FLAG_REVERSE = (1 << 2),
   RDFALIST_FLAG_TEXT = (1 << 3),
   RDFALIST_FLAG_CONTEXT = (1 << 4),
   RDFALIST_FLAG_LAST = (1 << 5)
} liflag_t;

/**
 * Initializes a mapping given the number of elements the mapping is
 * expected to hold.
 *
 * @param elements the maximum number of elements the mapping is
 *                 supposed to hold.
 *
 * @return an initialized char**, with all of the elements set to NULL.
 */
char** rdfa_create_mapping(size_t elements);

/**
 * Copies the entire contents of a mapping verbatim and returns a
 * pointer to the copied mapping.
 *
 * @param mapping the mapping to copy
 *
 * @return the copied mapping, with all of the memory newly
 *         allocated. You MUST free the returned mapping when you are
 *         done with it.
 */
char** rdfa_copy_mapping(char** mapping);

/**
 * Updates the given mapping when presented with a key and a value. If
 * the key doesn't exist in the mapping, it is created.
 *
 * @param mapping the mapping to update.
 * @param key the key.
 * @param value the value.
 */
void rdfa_update_mapping(char** mapping, const char* key, const char* value);

/**
 * Gets the value for a given mapping when presented with a key. If
 * the key doesn't exist in the mapping, NULL is returned.
 *
 * @param mapping the mapping to search.
 * @param key the key.
 *
 * @return value the value in the mapping for the given key.
 */
const char* rdfa_get_mapping(char** mapping, const char* key);

/**
 * Gets the current mapping for the given mapping and increments the
 * mapping to the next value in the chain. 
 *
 * @param mapping the mapping to use and increment.
 * @param key the key that will be retrieved, NULL if the mapping is
 *            blank or you are at the end of the mapping.
 * @param value the value that is associated with the key. NULL if the
 *              mapping is blank or you are at the end of the mapping.
 */
void rdfa_next_mapping(char** mapping, char** key, char** value);

/**
 * Prints the mapping to the screen in a human-readable way.
 *
 * @param mapping the mapping to print to the screen.
 */
void rdfa_print_mapping(char** mapping);

/**
 * Frees all memory associated with a mapping.
 *
 * @param mapping the mapping to free.
 */
void rdfa_free_mapping(char** mapping);

/**
 * Creates a list and initializes it to the given size.
 *
 * @param size the starting size of the list.
 */
rdfalist* rdfa_create_list(size_t size);

/**
 * Copies the given list.
 *
 * @param list the list to copy.
 *
 * @return the copied list. You MUST free the memory associated with
 *         the returned list once you are done with it.
 */
rdfalist* rdfa_copy_list(rdfalist* list);

/**
 * Adds an item to the end of the list.
 *
 * @param list the list to add the item to.
 * @param data the data to add to the list.
 * @param flags the flags to attach to the item.
 */
void rdfa_add_item(rdfalist* list, void* data, liflag_t flags);

/**
 * Pushes an item onto the top of a stack. This function uses a list
 * for the underlying implementation.
 *
 * @param stack the stack to add the item to.
 * @param data the data to add to the stack.
 * @param flags the flags to attach to the item.
 */
void rdfa_push_item(rdfalist* stack, void* data, liflag_t flags);

/**
 * Pops an item off of the top of a stack. This function uses a list
 * for the underlying implementation 
 *
 * @param stack the stack to pop the item off of.
 *
 * @return the item that was just popped off of the top of the
 *         stack. You MUST free the memory associated with the return
 *         value.
 */
void* rdfa_pop_item(rdfalist* stack);

/**
 * Prints the list to the screen in a human-readable way.
 *
 * @param list the list to print to the screen.
 */
void rdfa_print_list(rdfalist* list);

/**
 * Frees all memory associated with the given list.
 *
 * @param list the list to free.
 */
void rdfa_free_list(rdfalist* list);

/**
 * Replaces an old string with a new string, freeing the old memory
 * and allocating new memory for the new string.
 *
 * @param old_string the old string to free and replace.
 * @param new_string the new string to copy to the old_string's
 *                   location.
 *
 * @return a pointer to the newly allocated string.
 */
char* rdfa_replace_string(char* old_string, const char* new_string);

/**
 * Appends a new string to the old string, expanding the old string's
 * memory area if needed. The old string's size must be provided and
 * will be updated to the new length.
 * 
 * @param old_string the old string to reallocate if needed.
 * @param string_size the old string's length, to be updated.
 * @param suffix the string to append to the old_string.
 * @param suffix_size the size of the suffix string.
 *
 * @return a pointer to the newly re-allocated string.
 */
char* rdfa_n_append_string(
   char* old_string, size_t* string_size,
   const char* suffix, size_t suffix_size);

/**
 * Joins two strings together and returns a newly allocated string
 * with both strings joined.
 *
 * @param prefix the beginning part of the string.
 * @param suffix the ending part of the string.
 *
 * @return a pointer to the newly allocated string that has both
 *         prefix and suffix in it.
 */
char* rdfa_join_string(const char* prefix, const char* suffix);

/**
 * Canonicalizes a given string by condensing all whitespace to single
 * spaces and stripping leading and trailing whitespace.
 *
 * @param str the string to canonicalize.
 *
 * @return a pointer to a newly allocated string that contains the
 *         canonicalized text.
 */
char* rdfa_canonicalize_string(const char* str);

/**
 * Creates a triple given the subject, predicate, object, datatype and
 * language for the triple.
 *
 * @param subject the subject for the triple.
 * @param predicate the predicate for the triple.
 * @param object the object for the triple.
 * @param object_type the type of the object, which must be an rdfresource_t.
 * @param datatype the datatype of the triple.
 * @param language the language for the triple.
 *
 * @return a newly allocated triple with all of the given
 *         information. This triple MUST be free()'d when you are done
 *         with it.
 */
rdftriple* rdfa_create_triple(const char* subject, const char* predicate,
   const char* object, rdfresource_t object_type, const char* datatype,
   const char* language);

/**
 * Prints a triple in a human-readable fashion.
 *
 * @triple the triple to display.
 */
void rdfa_print_triple(rdftriple* triple);

/**
 * Frees the memory associated with a triple.
 */
void rdfa_free_triple(rdftriple* triple);

/**
 * Resolves a given uri by appending it to the context's base parameter.
 *
 * @param context the current processing context.
 * @param uri the URI part to process.
 *
 * @return the fully qualified IRI. The memory returned from this
 *         function MUST be freed.
 */
char* rdfa_resolve_uri(rdfacontext* context, const char* uri);

/**
 * Resolves a given uri depending on whether or not it is a fully
 * qualified IRI or a CURIE.
 *
 * @param context the current processing context.
 * @param uri the URI part to process.
 * @param mode the CURIE processing mode to use when parsing the CURIE.
 *
 * @return the fully qualified IRI. The memory returned from this
 *         function MUST be freed.
 */
char* rdfa_resolve_curie(
   rdfacontext* context, const char* uri, curieparse_t mode);

/**
 * Resolves one or more CURIEs into fully qualified IRIs.
 *
 * @param rdfa_context the current processing context.
 * @param uris a list of URIs.
 * @param mode the CURIE parsing mode to use, one of
 *             CURIE_PARSE_INSTANCEOF, CURIE_PARSE_RELREV, or
 *             CURIE_PARSE_PROPERTY.
 *
 * @return an RDFa list if one or more IRIs were generated, NULL if not.
 */
rdfalist* rdfa_resolve_curie_list(
   rdfacontext* rdfa_context, const char* uris, curieparse_t mode);

curie_t get_curie_type(const char* uri);

char* rdfa_resolve_relrev_curie(rdfacontext* context, const char* uri);

char* rdfa_resolve_property_curie(rdfacontext* context, const char* uri);

void rdfa_update_language(rdfacontext* context, const char* lang);

char* rdfa_create_bnode(rdfacontext* context);

// All functions that rdfa.c needs.
void rdfa_update_uri_mappings(rdfacontext* context, const char* attr, const char* value);
void rdfa_establish_new_subject(
   rdfacontext* context, const char* name, const char* about, const char* src,
   const char* resource, const char* href, const rdfalist* type_of);
void rdfa_establish_new_subject_with_relrev(
   rdfacontext* context, const char* name, const char* about, const char* src,
   const char* resource, const char* href, const rdfalist* type_of);
void rdfa_complete_incomplete_triples(rdfacontext* context);
void rdfa_complete_type_triples(rdfacontext* context, const rdfalist* type_of);
void rdfa_complete_relrev_triples(
   rdfacontext* context, const rdfalist* rel, const rdfalist* rev);
void rdfa_save_incomplete_triples(
   rdfacontext* context, const rdfalist* rel, const rdfalist* rev);
void rdfa_complete_object_literal_triples(rdfacontext* context);

/* triple.c - needed by namespace.c */
void rdfa_generate_namespace_triple(
   rdfacontext* context, const char* prefix, const char* iri);

#ifdef __cplusplus
}
#endif

#endif
