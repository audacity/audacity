/*
  LV2 Fifths Example Plugin
  Copyright 2014 David Robillard <d@drobilla.net>

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

#ifndef FIFTHS_URIS_H
#define FIFTHS_URIS_H

#include "lv2/lv2plug.in/ns/ext/log/log.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"
#include "lv2/lv2plug.in/ns/ext/state/state.h"

#define EG_FIFTHS_URI          "http://lv2plug.in/plugins/eg-fifths"
#define EG_FIFTHS__sample      EG_FIFTHS_URI "#sample"
#define EG_FIFTHS__applySample EG_FIFTHS_URI "#applySample"
#define EG_FIFTHS__freeSample  EG_FIFTHS_URI "#freeSample"

typedef struct {
	LV2_URID atom_Blank;
	LV2_URID atom_Path;
	LV2_URID atom_Resource;
	LV2_URID atom_Sequence;
	LV2_URID atom_URID;
	LV2_URID atom_eventTransfer;
	LV2_URID eg_applySample;
	LV2_URID eg_sample;
	LV2_URID eg_freeSample;
	LV2_URID midi_Event;
	LV2_URID patch_Set;
	LV2_URID patch_property;
	LV2_URID patch_value;
} FifthsURIs;

static inline void
map_fifths_uris(LV2_URID_Map* map, FifthsURIs* uris)
{
	uris->atom_Blank         = map->map(map->handle, LV2_ATOM__Blank);
	uris->atom_Path          = map->map(map->handle, LV2_ATOM__Path);
	uris->atom_Resource      = map->map(map->handle, LV2_ATOM__Resource);
	uris->atom_Sequence      = map->map(map->handle, LV2_ATOM__Sequence);
	uris->atom_URID          = map->map(map->handle, LV2_ATOM__URID);
	uris->atom_eventTransfer = map->map(map->handle, LV2_ATOM__eventTransfer);
	uris->eg_applySample     = map->map(map->handle, EG_FIFTHS__applySample);
	uris->eg_freeSample      = map->map(map->handle, EG_FIFTHS__freeSample);
	uris->eg_sample          = map->map(map->handle, EG_FIFTHS__sample);
	uris->midi_Event         = map->map(map->handle, LV2_MIDI__MidiEvent);
	uris->patch_Set          = map->map(map->handle, LV2_PATCH__Set);
	uris->patch_property     = map->map(map->handle, LV2_PATCH__property);
	uris->patch_value        = map->map(map->handle, LV2_PATCH__value);
}

#endif  /* FIFTHS_URIS_H */
