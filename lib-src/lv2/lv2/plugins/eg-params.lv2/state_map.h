/*
  LV2 State Map
  Copyright 2016 David Robillard <d@drobilla.net>

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

#include "lv2/atom/atom.h"
#include "lv2/urid/urid.h"

#include <stdarg.h>
#include <stdlib.h>

/** Entry in an array that serves as a dictionary of properties. */
typedef struct {
	const char* uri;
	LV2_URID    urid;
	LV2_Atom*   value;
} StateMapItem;

/** Comparator for StateMapItems sorted by URID. */
static int
state_map_cmp(const void* a, const void* b)
{
	const StateMapItem* ka = (const StateMapItem*)a;
	const StateMapItem* kb = (const StateMapItem*)b;
	if (ka->urid < kb->urid) {
		return -1;
	} else if (kb->urid < ka->urid) {
		return 1;
	}
	return 0;
}

/** Helper macro for terse state map initialisation. */
#define STATE_MAP_INIT(type, ptr) \
	(LV2_ATOM__ ## type), \
	(sizeof(*ptr) - sizeof(LV2_Atom)), \
	(ptr)

/**
   Initialise a state map.

   The variable parameters list must be NULL terminated, and is a sequence of
   const char* uri, const char* type, uint32_t size, LV2_Atom* value.  The
   value must point to a valid atom that resides elsewhere, the state map is
   only an index and does not contain actual state values.  The macro
   STATE_MAP_INIT can be used to make simpler code when state is composed of
   standard atom types, for example:

   struct Plugin {
       LV2_URID_Map* map;
       StateMapItem  props[3];
       // ...
   };

   state_map_init(
       self->props, self->map, self->map->handle,
       PLUG_URI "#gain",   STATE_MAP_INIT(Float,  &state->gain),
       PLUG_URI "#offset", STATE_MAP_INIT(Int,    &state->offset),
       PLUG_URI "#file",   STATE_MAP_INIT(Path,   &state->file),
       NULL);
*/
static void
state_map_init(StateMapItem        dict[],
               LV2_URID_Map*       map,
               LV2_URID_Map_Handle handle,
               /* const char* uri, const char* type, uint32_t size, LV2_Atom* value */ ...)
{
	// Set dict entries from parameters
	unsigned i = 0;
	va_list  args;
	va_start(args, handle);
	for (const char* uri; (uri = va_arg(args, const char*)); ++i) {
		const char*     type  = va_arg(args, const char*);
		const uint32_t  size  = va_arg(args, uint32_t);
		LV2_Atom* const value = va_arg(args, LV2_Atom*);
		dict[i].uri         = uri;
		dict[i].urid        = map->map(map->handle, uri);
		dict[i].value       = value;
		dict[i].value->size = size;
		dict[i].value->type = map->map(map->handle, type);
	}
	va_end(args);

	// Sort for fast lookup by URID by state_map_find()
	qsort(dict, i, sizeof(StateMapItem), state_map_cmp);
}

/**
   Retrieve an item from a state map by URID.

   This takes O(lg(n)) time, and is useful for implementing generic property
   access with little code, for example to respond to patch:Get messages for a
   specific property.
*/
static StateMapItem*
state_map_find(StateMapItem dict[], uint32_t n_entries, LV2_URID urid)
{
	const StateMapItem key = { NULL, urid, NULL };
	return (StateMapItem*)bsearch(
		&key, dict, n_entries, sizeof(StateMapItem), state_map_cmp);
}

