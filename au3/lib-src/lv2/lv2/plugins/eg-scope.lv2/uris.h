/*
  Copyright 2013 Robin Gareus <robin@gareus.org>

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

#ifndef SCO_URIS_H
#define SCO_URIS_H

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/parameters/parameters.h"
#include "lv2/urid/urid.h"

#define SCO_URI "http://lv2plug.in/plugins/eg-scope"

typedef struct {
	// URIs defined in LV2 specifications
	LV2_URID atom_Vector;
	LV2_URID atom_Float;
	LV2_URID atom_Int;
	LV2_URID atom_eventTransfer;
	LV2_URID param_sampleRate;

	/* URIs defined for this plugin.  It is best to re-use existing URIs as
	   much as possible, but plugins may need more vocabulary specific to their
	   needs.  These are used as types and properties for plugin:UI
	   communication, as well as for saving state. */
	LV2_URID RawAudio;
	LV2_URID channelID;
	LV2_URID audioData;
	LV2_URID ui_On;
	LV2_URID ui_Off;
	LV2_URID ui_State;
	LV2_URID ui_spp;
	LV2_URID ui_amp;
} ScoLV2URIs;

static inline void
map_sco_uris(LV2_URID_Map* map, ScoLV2URIs* uris)
{
	uris->atom_Vector        = map->map(map->handle, LV2_ATOM__Vector);
	uris->atom_Float         = map->map(map->handle, LV2_ATOM__Float);
	uris->atom_Int           = map->map(map->handle, LV2_ATOM__Int);
	uris->atom_eventTransfer = map->map(map->handle, LV2_ATOM__eventTransfer);
	uris->param_sampleRate   = map->map(map->handle, LV2_PARAMETERS__sampleRate);

	/* Note the convention that URIs for types are capitalized, and URIs for
	   everything else (mainly properties) are not, just as in LV2
	   specifications. */
	uris->RawAudio  = map->map(map->handle, SCO_URI "#RawAudio");
	uris->audioData = map->map(map->handle, SCO_URI "#audioData");
	uris->channelID = map->map(map->handle, SCO_URI "#channelID");
	uris->ui_On     = map->map(map->handle, SCO_URI "#UIOn");
	uris->ui_Off    = map->map(map->handle, SCO_URI "#UIOff");
	uris->ui_State  = map->map(map->handle, SCO_URI "#UIState");
	uris->ui_spp    = map->map(map->handle, SCO_URI "#ui-spp");
	uris->ui_amp    = map->map(map->handle, SCO_URI "#ui-amp");
}

#endif  /* SCO_URIS_H */
