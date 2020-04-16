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

/**
   @file sord.h API for Sord, a lightweight RDF model library.
*/

#ifndef SORD_SORD_H
#define SORD_SORD_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include "serd/serd.h"

#ifdef SORD_SHARED
#    ifdef _WIN32
#        define SORD_LIB_IMPORT __declspec(dllimport)
#        define SORD_LIB_EXPORT __declspec(dllexport)
#    else
#        define SORD_LIB_IMPORT __attribute__((visibility("default")))
#        define SORD_LIB_EXPORT __attribute__((visibility("default")))
#    endif
#    ifdef SORD_INTERNAL
#        define SORD_API SORD_LIB_EXPORT
#    else
#        define SORD_API SORD_LIB_IMPORT
#    endif
#else
#    define SORD_API
#endif

#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

/**
   @defgroup sord Sord
   A lightweight RDF model library.

   Sord stores RDF (subject object predicate context) quads, where the context
   may be omitted (to represent triples in the default graph).
   @{
*/

/**
   Sord World.
   The World represents all library state, including interned strings.
*/
typedef struct SordWorldImpl SordWorld;

/**
   Sord Model.

   A model is an indexed set of Quads (i.e. it can contain several RDF
   graphs).  It may be searched using various patterns depending on which
   indices are enabled.
*/
typedef struct SordModelImpl SordModel;

/**
   Model Inserter.

   An inserter is used for writing statements to a model using the Serd sink
   interface.  This makes it simple to write to a model directly using a
   SerdReader, or any other code that writes statements to a SerdStatementSink.
*/
typedef struct SordInserterImpl SordInserter;

/**
   Model Iterator.
*/
typedef struct SordIterImpl SordIter;

/**
   RDF Node.
   A Node is a component of a Quad.  Nodes may be URIs, blank nodes, or
   (in the case of quad objects only) string literals. Literal nodes may
   have an associate language or datatype (but not both).
*/
typedef struct SordNodeImpl SordNode;

/**
   Quad of nodes (a statement), or a quad pattern.

   Nodes are ordered (S P O G).  The ID of the default graph is 0.
*/
typedef const SordNode* SordQuad[4];

/**
   Index into a SordQuad.
*/
typedef enum {
	SORD_SUBJECT   = 0,  /**< Subject */
	SORD_PREDICATE = 1,  /**< Predicate ("key") */
	SORD_OBJECT    = 2,  /**< Object    ("value") */
	SORD_GRAPH     = 3   /**< Graph     ("context") */
} SordQuadIndex;

/**
   Type of a node.
*/
typedef enum {
	SORD_URI     = 1,  /**< URI */
	SORD_BLANK   = 2,  /**< Blank node identifier */
	SORD_LITERAL = 3   /**< Literal (string with optional lang or datatype) */
} SordNodeType;

/**
   Indexing option.
*/
typedef enum {
	SORD_SPO = 1,       /**< Subject,   Predicate, Object */
	SORD_SOP = 1 << 1,  /**< Subject,   Object,    Predicate */
	SORD_OPS = 1 << 2,  /**< Object,    Predicate, Subject */
	SORD_OSP = 1 << 3,  /**< Object,    Subject,   Predicate */
	SORD_PSO = 1 << 4,  /**< Predicate, Subject,   Object */
	SORD_POS = 1 << 5   /**< Predicate, Object,    Subject */
} SordIndexOption;

/**
   @name World
   @{
*/

/**
   Create a new Sord World.
   It is safe to use multiple worlds in one process, though no data
   (e.g. nodes) can be shared between worlds, and this should be avoided if
   possible for performance reasons.
*/
SORD_API
SordWorld*
sord_world_new(void);

/**
   Free `world`.
*/
SORD_API
void
sord_world_free(SordWorld* world);

/**
   Set a function to be called when errors occur.

   The `error_sink` will be called with `handle` as its first argument.  If
   no error function is set, errors are printed to stderr.
*/
SORD_API
void
sord_world_set_error_sink(SordWorld*    world,
                          SerdErrorSink error_sink,
                          void*         handle);

/**
   @}
   @name Node
   @{
*/

/**
   Get a URI node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_uri(SordWorld* world, const uint8_t* uri);

/**
   Get a URI node from a relative URI string.
*/
SORD_API
SordNode*
sord_new_relative_uri(SordWorld*     world,
                      const uint8_t* uri,
                      const uint8_t* base_uri);

/**
   Get a blank node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_blank(SordWorld* world, const uint8_t* str);

/**
   Get a literal node from a string.

   Note this function measures `str`, which is a common bottleneck.
   Use sord_node_from_serd_node() instead if `str` is already measured.
*/
SORD_API
SordNode*
sord_new_literal(SordWorld*     world,
                 SordNode*      datatype,
                 const uint8_t* str,
                 const char*    lang);

/**
   Copy a node (obtain a reference).

   Node that since nodes are interned and reference counted, this does not
   actually create a deep copy of `node`.
*/
SORD_API
SordNode*
sord_node_copy(const SordNode* node);

/**
   Free a node (drop a reference).
*/
SORD_API
void
sord_node_free(SordWorld* world, SordNode* node);

/**
   Return the type of a node (SORD_URI, SORD_BLANK, or SORD_LITERAL).
*/
SORD_API
SordNodeType
sord_node_get_type(const SordNode* node);

/**
   Return the string value of a node.
*/
SORD_API
const uint8_t*
sord_node_get_string(const SordNode* node);

/**
   Return the string value of a node, and set `bytes` to its length in bytes.
*/
SORD_API
const uint8_t*
sord_node_get_string_counted(const SordNode* node, size_t* bytes);

/**
   Return the string value of a node, and set `bytes` to its length in bytes,
   and `count` to its length in characters.
*/
SORD_API
const uint8_t*
sord_node_get_string_measured(const SordNode* node,
                              size_t*         bytes,
                              size_t*         chars);

/**
   Return the language of a literal node (or NULL).
*/
SORD_API
const char*
sord_node_get_language(const SordNode* node);

/**
   Return the datatype URI of a literal node (or NULL).
*/
SORD_API
SordNode*
sord_node_get_datatype(const SordNode* node);

/**
   Return the flags (string attributes) of a node.
*/
SORD_API
SerdNodeFlags
sord_node_get_flags(const SordNode* node);

/**
   Return true iff node can be serialised as an inline object.

   More specifically, this returns true iff the node is the object field
   of exactly one statement, and therefore can be inlined since it needn't
   be referred to by name.
*/
SORD_API
bool
sord_node_is_inline_object(const SordNode* node);

/**
   Return true iff `a` is equal to `b`.

   Note this is much faster than comparing the node's strings.
*/
SORD_API
bool
sord_node_equals(const SordNode* a,
                 const SordNode* b);

/**
   Return a SordNode as a SerdNode.

   The returned node is shared and must not be freed or modified.
*/
SORD_API
const SerdNode*
sord_node_to_serd_node(const SordNode* node);

/**
   Create a new SordNode from a SerdNode.

   The returned node must be freed using sord_node_free().
*/
SORD_API
SordNode*
sord_node_from_serd_node(SordWorld*      world,
                         SerdEnv*        env,
                         const SerdNode* node,
                         const SerdNode* datatype,
                         const SerdNode* lang);

/**
   @}
   @name Model
   @{
*/

/**
   Create a new model.

   @param world The world in which to make this model.

   @param indices SordIndexOption flags (e.g. SORD_SPO|SORD_OPS).  Be sure to
   enable an index where the most significant node(s) are not variables in your
   queries (e.g. to make (? P O) queries, enable either SORD_OPS or SORD_POS).

   @param graphs If true, store (and index) graph contexts.
*/
SORD_API
SordModel*
sord_new(SordWorld* world,
         unsigned  indices,
         bool      graphs);

/**
   Close and free `model`.
*/
SORD_API
void
sord_free(SordModel* model);

/**
   Get the world associated with `model`.
*/
SORD_API
SordWorld*
sord_get_world(SordModel* model);

/**
   Return the number of nodes stored in `world`.

   Nodes are included in this count iff they are a part of a quad in `world`.
*/
SORD_API
size_t
sord_num_nodes(const SordWorld* world);

/**
   Return the number of quads stored in `model`.
*/
SORD_API
size_t
sord_num_quads(const SordModel* model);

/**
   Return an iterator to the start of `model`.
*/
SORD_API
SordIter*
sord_begin(const SordModel* model);

/**
   Search for statements by a quad pattern.
   @return an iterator to the first match, or NULL if no matches found.
*/
SORD_API
SordIter*
sord_find(SordModel* model, const SordQuad pat);

/**
   Search for statements by nodes.
   @return an iterator to the first match, or NULL if no matches found.
*/
SORD_API
SordIter*
sord_search(SordModel*      model,
            const SordNode* s,
            const SordNode* p,
            const SordNode* o,
            const SordNode* g);
/**
   Search for a single node that matches a pattern.
   Exactly one of `s`, `p`, `o` must be NULL.
   This function is mainly useful for predicates that only have one value.
   The returned node must be freed using sord_node_free().
   @return the first matching node, or NULL if no matches are found.
*/
SORD_API
SordNode*
sord_get(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g);

/**
   Return true iff a statement exists.
*/
SORD_API
bool
sord_ask(SordModel*      model,
         const SordNode* s,
         const SordNode* p,
         const SordNode* o,
         const SordNode* g);

/**
   Return the number of matching statements.
*/
SORD_API
uint64_t
sord_count(SordModel*      model,
           const SordNode* s,
           const SordNode* p,
           const SordNode* o,
           const SordNode* g);

/**
   Check if `model` contains a triple pattern.

   @return true if `model` contains a match for `pat`, otherwise false.
*/
SORD_API
bool
sord_contains(SordModel* model, const SordQuad pat);

/**
   Add a quad to a model.

   Calling this function invalidates all iterators on `model`.

   @return true on success, false, on error.
*/
SORD_API
bool
sord_add(SordModel* model, const SordQuad tup);

/**
   Remove a quad from a model.

   Calling this function invalidates all iterators on `model`.  To remove quads
   while iterating, use sord_erase() instead.
*/
SORD_API
void
sord_remove(SordModel* model, const SordQuad tup);

/**
   Remove a quad from a model via an iterator.

   Calling this function invalidates all iterators on `model` except `iter`.

   @param model The model which `iter` points to.
   @param iter Iterator to the element to erase, which is incremented to the
   next value on return.
*/
SORD_API
SerdStatus
sord_erase(SordModel* model, SordIter* iter);

/**
   @}
   @name Inserter
   @{
*/

/**
   Create an inserter for writing statements to a model.
*/
SORD_API
SordInserter*
sord_inserter_new(SordModel* model,
                  SerdEnv*   env);

/**
   Free an inserter.
*/
SORD_API
void
sord_inserter_free(SordInserter* inserter);

/**
   Set the current base URI for writing to the model.

   Note this function can be safely casted to SerdBaseSink.
*/
SORD_API
SerdStatus
sord_inserter_set_base_uri(SordInserter*   inserter,
                           const SerdNode* uri);

/**
   Set a namespace prefix for writing to the model.

   Note this function can be safely casted to SerdPrefixSink.
*/
SORD_API
SerdStatus
sord_inserter_set_prefix(SordInserter*   inserter,
                         const SerdNode* name,
                         const SerdNode* uri);

/**
   Write a statement to the model.

   Note this function can be safely casted to SerdStatementSink.
*/
SORD_API
SerdStatus
sord_inserter_write_statement(SordInserter*      inserter,
                              SerdStatementFlags flags,
                              const SerdNode*    graph,
                              const SerdNode*    subject,
                              const SerdNode*    predicate,
                              const SerdNode*    object,
                              const SerdNode*    object_datatype,
                              const SerdNode*    object_lang);

/**
   @}
   @name Iteration
   @{
*/

/**
   Set `quad` to the quad pointed to by `iter`.
*/
SORD_API
void
sord_iter_get(const SordIter* iter, SordQuad tup);

/**
   Return a field of the quad pointed to by `iter`.

   Returns NULL if `iter` is NULL or is at the end.
*/
SORD_API
const SordNode*
sord_iter_get_node(const SordIter* iter, SordQuadIndex index);

/**
   Return the store pointed to by `iter`.
*/
SORD_API
const SordModel*
sord_iter_get_model(SordIter* iter);

/**
   Increment `iter` to point to the next statement.
*/
SORD_API
bool
sord_iter_next(SordIter* iter);

/**
   Return true iff `iter` is at the end of its range.
*/
SORD_API
bool
sord_iter_end(const SordIter* iter);

/**
   Free `iter`.
*/
SORD_API
void
sord_iter_free(SordIter* iter);

/**
   @}
   @name Utilities
   @{
*/

/**
   Match two quads (using ID comparison only).

   This function is a straightforward and fast equivalence match with wildcard
   support (ID 0 is a wildcard). It does not actually read node data.
   @return true iff `x` and `y` match.
*/
SORD_API
bool
sord_quad_match(const SordQuad x, const SordQuad y);

/**
   @}
   @name Serialisation
   @{
*/

/**
   Return a reader that will read into `model`.
*/
SORD_API
SerdReader*
sord_new_reader(SordModel* model,
                SerdEnv*   env,
                SerdSyntax syntax,
                SordNode*  graph);

/**
   Write a model to a writer.
*/
SORD_API
bool
sord_write(SordModel*  model,
           SerdWriter* writer,
           SordNode*   graph);

/**
   Write a range to a writer.

   This increments `iter` to its end, then frees it.
*/
SORD_API
bool
sord_write_iter(SordIter*   iter,
                SerdWriter* writer);

/**
   @}
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* SORD_SORD_H */
