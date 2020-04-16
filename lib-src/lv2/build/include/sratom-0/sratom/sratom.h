/*
  Copyright 2012-2016 David Robillard <http://drobilla.net>

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
   @file sratom.h API for Sratom, an LV2 Atom RDF serialisation library.
*/

#ifndef SRATOM_SRATOM_H
#define SRATOM_SRATOM_H

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/urid/urid.h"
#include "serd/serd.h"
#include "sord/sord.h"

#include <stdbool.h>
#include <stdint.h>

#ifdef SRATOM_SHARED
#    ifdef _WIN32
#        define SRATOM_LIB_IMPORT __declspec(dllimport)
#        define SRATOM_LIB_EXPORT __declspec(dllexport)
#    else
#        define SRATOM_LIB_IMPORT __attribute__((visibility("default")))
#        define SRATOM_LIB_EXPORT __attribute__((visibility("default")))
#    endif
#    ifdef SRATOM_INTERNAL
#        define SRATOM_API SRATOM_LIB_EXPORT
#    else
#        define SRATOM_API SRATOM_LIB_IMPORT
#    endif
#else
#    define SRATOM_API
#endif

#ifdef __cplusplus
extern "C" {
#endif

/**
   @defgroup sratom Sratom
   An LV2 Atom RDF serialisation library.
   @{
*/

/**
   Atom serialiser.
*/
typedef struct SratomImpl Sratom;

/**
   Mode for reading resources to LV2 Objects.

   This affects how resources (which are either blank nodes or have URIs) are
   read by sratom_read(), since they may be read as simple references (a URI or
   blank node ID) or a complete description (an atom "Object").

   Currently, blank nodes are always read as Objects, but support for reading
   blank node IDs may be added in the future.
*/
typedef enum {
	/**
	   Read blank nodes as Objects, and named resources as URIs.
	*/
	SRATOM_OBJECT_MODE_BLANK,

	/**
	   Read blank nodes and the main subject as Objects, and any other named
	   resources as URIs.  The "main subject" is the subject parameter passed
	   to sratom_read(); if this is a resource it will be read as an Object,
	   but all other named resources encountered will be read as URIs.
	*/
	SRATOM_OBJECT_MODE_BLANK_SUBJECT
} SratomObjectMode;

/**
   Create a new Atom serialiser.
*/
SRATOM_API
Sratom*
sratom_new(LV2_URID_Map* map);

/**
   Free an Atom serialisation.
*/
SRATOM_API
void
sratom_free(Sratom* sratom);

/**
   Set the environment for reading or writing Turtle.

   This can be used to set namespace prefixes and a base URI for
   sratom_to_turtle() and sratom_from_turtle().
*/
SRATOM_API
void
sratom_set_env(Sratom*  sratom,
               SerdEnv* env);

/**
   Set the sink(s) where sratom will write its output.

   This must be called before calling sratom_write().
*/
SRATOM_API
void
sratom_set_sink(Sratom*           sratom,
                const char*       base_uri,
                SerdStatementSink sink,
                SerdEndSink       end_sink,
                void*             handle);

/**
   Write pretty numeric literals.

   If `pretty_numbers` is true, numbers will be written as pretty Turtle
   literals, rather than string literals with precise types.  The cost of this
   is that the types might get fudged on a round-trip to RDF and back.
*/
SRATOM_API
void
sratom_set_pretty_numbers(Sratom* sratom,
                          bool    pretty_numbers);

/**
   Configure how resources will be read to form LV2 Objects.
*/
SRATOM_API
void
sratom_set_object_mode(Sratom*          sratom,
                       SratomObjectMode object_mode);

/**
   Write an Atom to RDF.
   The serialised atom is written to the sink set by sratom_set_sink().
   @return 0 on success, or a non-zero error code otherwise.
*/
SRATOM_API
int
sratom_write(Sratom*         sratom,
             LV2_URID_Unmap* unmap,
             uint32_t        flags,
             const SerdNode* subject,
             const SerdNode* predicate,
             uint32_t        type_urid,
             uint32_t        size,
             const void*     body);

/**
   Read an Atom from RDF.
   The resulting atom will be written to `forge`.
*/
SRATOM_API
void
sratom_read(Sratom*         sratom,
            LV2_Atom_Forge* forge,
            SordWorld*      world,
            SordModel*      model,
            const SordNode* node);

/**
   Serialise an Atom to a Turtle string.
   The returned string must be free()'d by the caller.
*/
SRATOM_API
char*
sratom_to_turtle(Sratom*         sratom,
                 LV2_URID_Unmap* unmap,
                 const char*     base_uri,
                 const SerdNode* subject,
                 const SerdNode* predicate,
                 uint32_t        type,
                 uint32_t        size,
                 const void*     body);

/**
   Read an Atom from a Turtle string.
   The returned atom must be free()'d by the caller.
*/
SRATOM_API
LV2_Atom*
sratom_from_turtle(Sratom*         sratom,
                   const char*     base_uri,
                   const SerdNode* subject,
                   const SerdNode* predicate,
                   const char*     str);

/**
   A convenient resizing sink for LV2_Atom_Forge.
   The handle must point to an initialized SerdChunk.
*/
SRATOM_API
LV2_Atom_Forge_Ref
sratom_forge_sink(LV2_Atom_Forge_Sink_Handle handle,
                  const void*                buf,
                  uint32_t                   size);

/**
   The corresponding deref function for sratom_forge_sink.
*/
SRATOM_API
LV2_Atom*
sratom_forge_deref(LV2_Atom_Forge_Sink_Handle handle,
                   LV2_Atom_Forge_Ref         ref);

/**
   @}
*/

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif  /* SRATOM_SRATOM_H */
