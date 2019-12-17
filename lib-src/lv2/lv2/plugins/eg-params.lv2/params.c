/*
  LV2 Parameter Example Plugin
  Copyright 2014-2016 David Robillard <d@drobilla.net>

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

#include "state_map.h"

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/core/lv2.h"
#include "lv2/core/lv2_util.h"
#include "lv2/log/log.h"
#include "lv2/log/logger.h"
#include "lv2/midi/midi.h"
#include "lv2/patch/patch.h"
#include "lv2/state/state.h"
#include "lv2/urid/urid.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_STRING 1024

#define EG_PARAMS_URI    "http://lv2plug.in/plugins/eg-params"

#define N_PROPS 9

typedef struct {
	LV2_URID plugin;
	LV2_URID atom_Path;
	LV2_URID atom_Sequence;
	LV2_URID atom_URID;
	LV2_URID atom_eventTransfer;
	LV2_URID eg_spring;
	LV2_URID midi_Event;
	LV2_URID patch_Get;
	LV2_URID patch_Set;
	LV2_URID patch_Put;
	LV2_URID patch_body;
	LV2_URID patch_subject;
	LV2_URID patch_property;
	LV2_URID patch_value;
} URIs;

typedef struct {
	LV2_Atom_Int    aint;
	LV2_Atom_Long   along;
	LV2_Atom_Float  afloat;
	LV2_Atom_Double adouble;
	LV2_Atom_Bool   abool;
	LV2_Atom        astring;
	char            string[MAX_STRING];
	LV2_Atom        apath;
	char            path[MAX_STRING];
	LV2_Atom_Float  lfo;
	LV2_Atom_Float  spring;
} State;

static inline void
map_uris(LV2_URID_Map* map, URIs* uris)
{
	uris->plugin             = map->map(map->handle, EG_PARAMS_URI);

	uris->atom_Path          = map->map(map->handle, LV2_ATOM__Path);
	uris->atom_Sequence      = map->map(map->handle, LV2_ATOM__Sequence);
	uris->atom_URID          = map->map(map->handle, LV2_ATOM__URID);
	uris->atom_eventTransfer = map->map(map->handle, LV2_ATOM__eventTransfer);
	uris->eg_spring          = map->map(map->handle, EG_PARAMS_URI "#spring");
	uris->midi_Event         = map->map(map->handle, LV2_MIDI__MidiEvent);
	uris->patch_Get          = map->map(map->handle, LV2_PATCH__Get);
	uris->patch_Set          = map->map(map->handle, LV2_PATCH__Set);
	uris->patch_Put          = map->map(map->handle, LV2_PATCH__Put);
	uris->patch_body         = map->map(map->handle, LV2_PATCH__body);
	uris->patch_subject      = map->map(map->handle, LV2_PATCH__subject);
	uris->patch_property     = map->map(map->handle, LV2_PATCH__property);
	uris->patch_value        = map->map(map->handle, LV2_PATCH__value);
}

enum {
	PARAMS_IN  = 0,
	PARAMS_OUT = 1
};

typedef struct {
	// Features
	LV2_URID_Map*   map;
	LV2_URID_Unmap* unmap;
	LV2_Log_Logger  log;

	// Forge for creating atoms
	LV2_Atom_Forge forge;

	// Ports
	const LV2_Atom_Sequence* in_port;
	LV2_Atom_Sequence*       out_port;

	// URIs
	URIs uris;

	// Plugin state
	StateMapItem props[N_PROPS];
	State        state;

	// Buffer for making strings from URIDs if unmap is not provided
	char urid_buf[12];
} Params;

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	Params* self = (Params*)instance;
	switch (port) {
	case PARAMS_IN:
		self->in_port = (const LV2_Atom_Sequence*)data;
		break;
	case PARAMS_OUT:
		self->out_port = (LV2_Atom_Sequence*)data;
		break;
	default:
		break;
	}
}

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               path,
            const LV2_Feature* const* features)
{
	// Allocate instance
	Params* self = (Params*)calloc(1, sizeof(Params));
	if (!self) {
		return NULL;
	}

	// Get host features
	const char* missing = lv2_features_query(
		features,
		LV2_LOG__log,    &self->log.log, false,
		LV2_URID__map,   &self->map,     true,
		LV2_URID__unmap, &self->unmap,   false,
		NULL);
	lv2_log_logger_set_map(&self->log, self->map);
	if (missing) {
		lv2_log_error(&self->log, "Missing feature <%s>\n", missing);
		free(self);
		return NULL;
	}

	// Map URIs and initialise forge
	map_uris(self->map, &self->uris);
	lv2_atom_forge_init(&self->forge, self->map);

	// Initialise state dictionary
	State* state = &self->state;
	state_map_init(
		self->props, self->map, self->map->handle,
		EG_PARAMS_URI "#int",    STATE_MAP_INIT(Int,    &state->aint),
		EG_PARAMS_URI "#long",   STATE_MAP_INIT(Long,   &state->along),
		EG_PARAMS_URI "#float",  STATE_MAP_INIT(Float,  &state->afloat),
		EG_PARAMS_URI "#double", STATE_MAP_INIT(Double, &state->adouble),
		EG_PARAMS_URI "#bool",   STATE_MAP_INIT(Bool,   &state->abool),
		EG_PARAMS_URI "#string", STATE_MAP_INIT(String, &state->astring),
		EG_PARAMS_URI "#path",   STATE_MAP_INIT(Path,   &state->apath),
		EG_PARAMS_URI "#lfo",    STATE_MAP_INIT(Float,  &state->lfo),
		EG_PARAMS_URI "#spring", STATE_MAP_INIT(Float,  &state->spring),
		NULL);

	return (LV2_Handle)self;
}

static void
cleanup(LV2_Handle instance)
{
	free(instance);
}

/** Helper function to unmap a URID if possible. */
static const char*
unmap(Params* self, LV2_URID urid)
{
	if (self->unmap) {
		return self->unmap->unmap(self->unmap->handle, urid);
	} else {
		snprintf(self->urid_buf, sizeof(self->urid_buf), "%u", urid);
		return self->urid_buf;
	}
}

static LV2_State_Status
check_type(Params*  self,
           LV2_URID key,
           LV2_URID type,
           LV2_URID required_type)
{
	if (type != required_type) {
		lv2_log_trace(
			&self->log, "Bad type <%s> for <%s> (needs <%s>)\n",
			unmap(self, type),
			unmap(self, key),
			unmap(self, required_type));
		return LV2_STATE_ERR_BAD_TYPE;
	}
	return LV2_STATE_SUCCESS;
}

static LV2_State_Status
set_parameter(Params*     self,
              LV2_URID    key,
              uint32_t    size,
              LV2_URID    type,
              const void* body,
              bool        from_state)
{
	// Look up property in state dictionary
	const StateMapItem* entry = state_map_find(self->props, N_PROPS, key);
	if (!entry) {
		lv2_log_trace(&self->log, "Unknown parameter <%s>\n", unmap(self, key));
		return LV2_STATE_ERR_NO_PROPERTY;
	}

	// Ensure given type matches property's type
	if (check_type(self, key, type, entry->value->type)) {
		return LV2_STATE_ERR_BAD_TYPE;
	}

	// Set property value in state dictionary
	lv2_log_trace(&self->log, "Set <%s>\n", entry->uri);
	memcpy(entry->value + 1, body, size);
	entry->value->size = size;
	return LV2_STATE_SUCCESS;
}

static const LV2_Atom*
get_parameter(Params* self, LV2_URID key)
{
	const StateMapItem* entry = state_map_find(self->props, N_PROPS, key);
	if (entry) {
		lv2_log_trace(&self->log, "Get <%s>\n", entry->uri);
		return entry->value;
	}

	lv2_log_trace(&self->log, "Unknown parameter <%s>\n", unmap(self, key));
	return NULL;
}

static LV2_State_Status
write_param_to_forge(LV2_State_Handle handle,
                     uint32_t         key,
                     const void*      value,
                     size_t           size,
                     uint32_t         type,
                     uint32_t         flags)
{
	LV2_Atom_Forge* forge = (LV2_Atom_Forge*)handle;

	if (!lv2_atom_forge_key(forge, key) ||
	    !lv2_atom_forge_atom(forge, size, type) ||
	    !lv2_atom_forge_write(forge, value, size)) {
		return LV2_STATE_ERR_UNKNOWN;
	}

	return LV2_STATE_SUCCESS;
}

static void
store_prop(Params*                  self,
           LV2_State_Map_Path*      map_path,
           LV2_State_Status*        save_status,
           LV2_State_Store_Function store,
           LV2_State_Handle         handle,
           LV2_URID                 key,
           const LV2_Atom*          value)
{
	LV2_State_Status st;
	if (map_path && value->type == self->uris.atom_Path) {
		// Map path to abstract path for portable storage
		const char* path  = (const char*)(value + 1);
		char*       apath = map_path->abstract_path(map_path->handle, path);
		st = store(handle,
		           key,
		           apath,
		           strlen(apath) + 1,
		           self->uris.atom_Path,
		           LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);
		free(apath);
	} else {
		// Store simple property
		st = store(handle,
		           key,
		           value + 1,
		           value->size,
		           value->type,
		           LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);
	}

	if (save_status && !*save_status) {
		*save_status = st;
	}
}

/**
   State save method.

   This is used in the usual way when called by the host to save plugin state,
   but also internally for writing messages in the audio thread by passing a
   "store" function which actually writes the description to the forge.
*/
static LV2_State_Status
save(LV2_Handle                instance,
     LV2_State_Store_Function  store,
     LV2_State_Handle          handle,
     uint32_t                  flags,
     const LV2_Feature* const* features)
{
	Params*             self     = (Params*)instance;
	LV2_State_Map_Path* map_path = (LV2_State_Map_Path*)lv2_features_data(
		features, LV2_STATE__mapPath);

	LV2_State_Status st = LV2_STATE_SUCCESS;
	for (unsigned i = 0; i < N_PROPS; ++i) {
		StateMapItem* prop = &self->props[i];
		store_prop(self, map_path, &st, store, handle, prop->urid, prop->value);
	}

	return st;
}

static void
retrieve_prop(Params*                     self,
              LV2_State_Status*           restore_status,
              LV2_State_Retrieve_Function retrieve,
              LV2_State_Handle            handle,
              LV2_URID                    key)
{
	// Retrieve value from saved state
	size_t      vsize;
	uint32_t    vtype;
	uint32_t    vflags;
	const void* value = retrieve(handle, key, &vsize, &vtype, &vflags);

	// Set plugin instance state
	const LV2_State_Status st = value
		? set_parameter(self, key, vsize, vtype, value, true)
		: LV2_STATE_ERR_NO_PROPERTY;

	if (!*restore_status) {
		*restore_status = st;  // Set status if there has been no error yet
	}
}

/** State restore method. */
static LV2_State_Status
restore(LV2_Handle                  instance,
        LV2_State_Retrieve_Function retrieve,
        LV2_State_Handle            handle,
        uint32_t                    flags,
        const LV2_Feature* const*   features)
{
	Params*          self = (Params*)instance;
	LV2_State_Status st   = LV2_STATE_SUCCESS;

	for (unsigned i = 0; i < N_PROPS; ++i) {
		retrieve_prop(self, &st, retrieve, handle, self->props[i].urid);
	}

	return st;
}

static inline bool
subject_is_plugin(Params* self, const LV2_Atom_URID* subject)
{
	// This simple plugin only supports one subject: itself
	return (!subject || (subject->atom.type == self->uris.atom_URID &&
	                     subject->body      == self->uris.plugin));
}

static void
run(LV2_Handle instance, uint32_t sample_count)
{
	Params* self = (Params*)instance;
	URIs*   uris = &self->uris;

	// Initially, self->out_port contains a Chunk with size set to capacity
	// Set up forge to write directly to output port
	const uint32_t out_capacity = self->out_port->atom.size;
	lv2_atom_forge_set_buffer(&self->forge,
	                          (uint8_t*)self->out_port,
	                          out_capacity);

	// Start a sequence in the output port
	LV2_Atom_Forge_Frame out_frame;
	lv2_atom_forge_sequence_head(&self->forge, &out_frame, 0);

	// Read incoming events
	LV2_ATOM_SEQUENCE_FOREACH(self->in_port, ev) {
		const LV2_Atom_Object* obj = (const LV2_Atom_Object*)&ev->body;
		if (obj->body.otype == uris->patch_Set) {
			// Get the property and value of the set message
			const LV2_Atom_URID* subject  = NULL;
			const LV2_Atom_URID* property = NULL;
			const LV2_Atom*      value    = NULL;
			lv2_atom_object_get(obj,
			                    uris->patch_subject,  (const LV2_Atom**)&subject,
			                    uris->patch_property, (const LV2_Atom**)&property,
			                    uris->patch_value,    &value,
			                    0);
			if (!subject_is_plugin(self, subject)) {
				lv2_log_error(&self->log, "Set for unknown subject\n");
			} else if (!property) {
				lv2_log_error(&self->log, "Set with no property\n");
			} else if (property->atom.type != uris->atom_URID) {
				lv2_log_error(&self->log, "Set property is not a URID\n");
			} else {
				// Set property to the given value
				const LV2_URID key = property->body;
				set_parameter(self, key, value->size, value->type, value + 1, false);
			}
		} else if (obj->body.otype == uris->patch_Get) {
			// Get the property of the get message
			const LV2_Atom_URID* subject  = NULL;
			const LV2_Atom_URID* property = NULL;
			lv2_atom_object_get(obj,
			                    uris->patch_subject,  (const LV2_Atom**)&subject,
			                    uris->patch_property, (const LV2_Atom**)&property,
			                    0);
			if (!subject_is_plugin(self, subject)) {
				lv2_log_error(&self->log, "Get with unknown subject\n");
			} else if (!property) {
				// Get with no property, emit complete state
				lv2_atom_forge_frame_time(&self->forge, ev->time.frames);
				LV2_Atom_Forge_Frame pframe;
				lv2_atom_forge_object(&self->forge, &pframe, 0, uris->patch_Put);
				lv2_atom_forge_key(&self->forge, uris->patch_body);

				LV2_Atom_Forge_Frame bframe;
				lv2_atom_forge_object(&self->forge, &bframe, 0, 0);
				save(self, write_param_to_forge, &self->forge, 0, NULL);

				lv2_atom_forge_pop(&self->forge, &bframe);
				lv2_atom_forge_pop(&self->forge, &pframe);
			} else if (property->atom.type != uris->atom_URID) {
				lv2_log_error(&self->log, "Get property is not a URID\n");
			} else {
				// Get for a specific property
				const LV2_URID  key   = property->body;
				const LV2_Atom* value = get_parameter(self, key);
				if (value) {
					lv2_atom_forge_frame_time(&self->forge, ev->time.frames);
					LV2_Atom_Forge_Frame frame;
					lv2_atom_forge_object(&self->forge, &frame, 0, uris->patch_Set);
					lv2_atom_forge_key(&self->forge, uris->patch_property);
					lv2_atom_forge_urid(&self->forge, property->body);
					store_prop(self, NULL, NULL, write_param_to_forge, &self->forge,
					           uris->patch_value, value);
					lv2_atom_forge_pop(&self->forge, &frame);
				}
			}
		} else {
			lv2_log_trace(&self->log, "Unknown object type <%s>\n",
			              unmap(self, obj->body.otype));
		}
	}

	if (self->state.spring.body > 0.0f) {
		const float spring = self->state.spring.body;
		self->state.spring.body = (spring >= 0.001) ? spring - 0.001 : 0.0;
		lv2_atom_forge_frame_time(&self->forge, 0);
		LV2_Atom_Forge_Frame frame;
		lv2_atom_forge_object(&self->forge, &frame, 0, uris->patch_Set);

		lv2_atom_forge_key(&self->forge, uris->patch_property);
		lv2_atom_forge_urid(&self->forge, uris->eg_spring);
		lv2_atom_forge_key(&self->forge, uris->patch_value);
		lv2_atom_forge_float(&self->forge, self->state.spring.body);

		lv2_atom_forge_pop(&self->forge, &frame);
	}

	lv2_atom_forge_pop(&self->forge, &out_frame);
}

static const void*
extension_data(const char* uri)
{
	static const LV2_State_Interface state = { save, restore };
	if (!strcmp(uri, LV2_STATE__interface)) {
		return &state;
	}
	return NULL;
}

static const LV2_Descriptor descriptor = {
	EG_PARAMS_URI,
	instantiate,
	connect_port,
	NULL,  // activate,
	run,
	NULL,  // deactivate,
	cleanup,
	extension_data
};

LV2_SYMBOL_EXPORT const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
	return (index == 0) ? &descriptor : NULL;
}
