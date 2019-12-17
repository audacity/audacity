/*
  Copyright 2016 David Robillard <d@drobilla.net>
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

#include "./uris.h"

#include "lv2/atom/atom.h"
#include "lv2/atom/forge.h"
#include "lv2/atom/util.h"
#include "lv2/core/lv2.h"
#include "lv2/core/lv2_util.h"
#include "lv2/log/log.h"
#include "lv2/log/logger.h"
#include "lv2/state/state.h"
#include "lv2/urid/urid.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/**
   ==== Private Plugin Instance Structure ====

   In addition to the usual port buffers and features, this plugin stores the
   state of the UI here, so it can be opened and closed without losing the
   current settings.  The UI state is communicated between the plugin and the
   UI using atom messages via a sequence port, similarly to MIDI I/O.
*/
typedef struct {
	// Port buffers
	float*                   input[2];
	float*                   output[2];
	const LV2_Atom_Sequence* control;
	LV2_Atom_Sequence*       notify;

	// Atom forge and URI mapping
	LV2_URID_Map*        map;
	ScoLV2URIs           uris;
	LV2_Atom_Forge       forge;
	LV2_Atom_Forge_Frame frame;

	// Log feature and convenience API
	LV2_Log_Logger logger;

	// Instantiation settings
	uint32_t n_channels;
	double   rate;

	// UI state
	bool     ui_active;
	bool     send_settings_to_ui;
	float    ui_amp;
	uint32_t ui_spp;
} EgScope;

/** ==== Port Indices ==== */
typedef enum {
	SCO_CONTROL = 0,  // Event input
	SCO_NOTIFY  = 1,  // Event output
	SCO_INPUT0  = 2,  // Audio input 0
	SCO_OUTPUT0 = 3,  // Audio output 0
	SCO_INPUT1  = 4,  // Audio input 1 (stereo variant)
	SCO_OUTPUT1 = 5,  // Audio input 2 (stereo variant)
} PortIndex;

/** ==== Instantiate Method ==== */
static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
	(void)descriptor;   // Unused variable
	(void)bundle_path;  // Unused variable

	// Allocate and initialise instance structure.
	EgScope* self = (EgScope*)calloc(1, sizeof(EgScope));
	if (!self) {
		return NULL;
	}

	// Get host features
	const char* missing = lv2_features_query(
		features,
		LV2_LOG__log,  &self->logger.log, false,
		LV2_URID__map, &self->map,        true,
		NULL);
	lv2_log_logger_set_map(&self->logger, self->map);
	if (missing) {
		lv2_log_error(&self->logger, "Missing feature <%s>\n", missing);
		free(self);
		return NULL;
	}

	// Decide which variant to use depending on the plugin URI
	if (!strcmp(descriptor->URI, SCO_URI "#Stereo")) {
		self->n_channels = 2;
	} else if (!strcmp(descriptor->URI, SCO_URI "#Mono")) {
		self->n_channels = 1;
	} else {
		free(self);
		return NULL;
	}

	// Initialise local variables
	self->ui_active           = false;
	self->send_settings_to_ui = false;
	self->rate                = rate;

	// Set default UI settings
	self->ui_spp = 50;
	self->ui_amp = 1.0;

	// Map URIs and initialise forge/logger
	map_sco_uris(self->map, &self->uris);
	lv2_atom_forge_init(&self->forge, self->map);

	return (LV2_Handle)self;
}

/** ==== Connect Port Method ==== */
static void
connect_port(LV2_Handle handle,
             uint32_t   port,
             void*      data)
{
	EgScope* self = (EgScope*)handle;

	switch ((PortIndex)port) {
	case SCO_CONTROL:
		self->control = (const LV2_Atom_Sequence*)data;
		break;
	case SCO_NOTIFY:
		self->notify = (LV2_Atom_Sequence*)data;
		break;
	case SCO_INPUT0:
		self->input[0] = (float*)data;
		break;
	case SCO_OUTPUT0:
		self->output[0] = (float*)data;
		break;
	case SCO_INPUT1:
		self->input[1] = (float*)data;
		break;
	case SCO_OUTPUT1:
		self->output[1] = (float*)data;
		break;
	}
}

/**
   ==== Utility Function: `tx_rawaudio` ====

   This function forges a message for sending a vector of raw data.  The object
   is a http://lv2plug.in/ns/ext/atom#Blank[Blank] with a few properties, like:
   [source,n3]
   --------
   []
   	a sco:RawAudio ;
   	sco:channelID 0 ;
   	sco:audioData [ 0.0, 0.0, ... ] .
   --------

   where the value of the `sco:audioData` property, `[ 0.0, 0.0, ... ]`, is a
   http://lv2plug.in/ns/ext/atom#Vector[Vector] of
   http://lv2plug.in/ns/ext/atom#Float[Float].
*/
static void
tx_rawaudio(LV2_Atom_Forge* forge,
            ScoLV2URIs*     uris,
            const int32_t   channel,
            const size_t    n_samples,
            const float*    data)
{
	LV2_Atom_Forge_Frame frame;

	// Forge container object of type 'RawAudio'
	lv2_atom_forge_frame_time(forge, 0);
	lv2_atom_forge_object(forge, &frame, 0, uris->RawAudio);

	// Add integer 'channelID' property
	lv2_atom_forge_key(forge, uris->channelID);
	lv2_atom_forge_int(forge, channel);

	// Add vector of floats 'audioData' property
	lv2_atom_forge_key(forge, uris->audioData);
	lv2_atom_forge_vector(
		forge, sizeof(float), uris->atom_Float, n_samples, data);

	// Close off object
	lv2_atom_forge_pop(forge, &frame);
}

/** ==== Run Method ==== */
static void
run(LV2_Handle handle, uint32_t n_samples)
{
	EgScope* self = (EgScope*)handle;

	/* Ensure notify port buffer is large enough to hold all audio-samples and
	   configuration settings.  A minimum size was requested in the .ttl file,
	   but check here just to be sure.

	   TODO: Explain these magic numbers.
	*/
	const size_t   size  = (sizeof(float) * n_samples + 64) * self->n_channels;
	const uint32_t space = self->notify->atom.size;
	if (space < size + 128) {
		/* Insufficient space, report error and do nothing.  Note that a
		   real-time production plugin mustn't call log functions in run(), but
		   this can be useful for debugging and example purposes.
		*/
		lv2_log_error(&self->logger, "Buffer size is insufficient\n");
		return;
	}

	// Prepare forge buffer and initialize atom-sequence
	lv2_atom_forge_set_buffer(&self->forge, (uint8_t*)self->notify, space);
	lv2_atom_forge_sequence_head(&self->forge, &self->frame, 0);

	/* Send settings to UI

	   The plugin can continue to run while the UI is closed and re-opened.
	   The state and settings of the UI are kept here and transmitted to the UI
	   every time it asks for them or if the user initializes a 'load preset'.
	*/
	if (self->send_settings_to_ui && self->ui_active) {
		self->send_settings_to_ui = false;
		// Forge container object of type 'ui_state'
		LV2_Atom_Forge_Frame frame;
		lv2_atom_forge_frame_time(&self->forge, 0);
		lv2_atom_forge_object(&self->forge, &frame, 0, self->uris.ui_State);

		// Add UI state as properties
		lv2_atom_forge_key(&self->forge, self->uris.ui_spp);
		lv2_atom_forge_int(&self->forge, self->ui_spp);
		lv2_atom_forge_key(&self->forge, self->uris.ui_amp);
		lv2_atom_forge_float(&self->forge, self->ui_amp);
		lv2_atom_forge_key(&self->forge, self->uris.param_sampleRate);
		lv2_atom_forge_float(&self->forge, self->rate);
		lv2_atom_forge_pop(&self->forge, &frame);
	}

	// Process incoming events from GUI
	if (self->control) {
		const LV2_Atom_Event* ev = lv2_atom_sequence_begin(
			&(self->control)->body);
		// For each incoming message...
		while (!lv2_atom_sequence_is_end(
			       &self->control->body, self->control->atom.size, ev)) {
			// If the event is an atom:Blank object
			if (lv2_atom_forge_is_object_type(&self->forge, ev->body.type)) {
				const LV2_Atom_Object* obj = (const LV2_Atom_Object*)&ev->body;
				if (obj->body.otype == self->uris.ui_On) {
					// If the object is a ui-on, the UI was activated
					self->ui_active           = true;
					self->send_settings_to_ui = true;
				} else if (obj->body.otype == self->uris.ui_Off) {
					// If the object is a ui-off, the UI was closed
					self->ui_active = false;
				} else if (obj->body.otype == self->uris.ui_State) {
					// If the object is a ui-state, it's the current UI settings
					const LV2_Atom* spp = NULL;
					const LV2_Atom* amp = NULL;
					lv2_atom_object_get(obj, self->uris.ui_spp, &spp,
					                    self->uris.ui_amp, &amp,
					                    0);
					if (spp) {
						self->ui_spp = ((const LV2_Atom_Int*)spp)->body;
					}
					if (amp) {
						self->ui_amp = ((const LV2_Atom_Float*)amp)->body;
					}
				}
			}
			ev = lv2_atom_sequence_next(ev);
		}
	}

	// Process audio data
	for (uint32_t c = 0; c < self->n_channels; ++c) {
		if (self->ui_active) {
			// If UI is active, send raw audio data to UI
			tx_rawaudio(&self->forge, &self->uris, c, n_samples, self->input[c]);
		}
		// If not processing audio in-place, forward audio
		if (self->input[c] != self->output[c]) {
			memcpy(self->output[c], self->input[c], sizeof(float) * n_samples);
		}
	}

	// Close off sequence
	lv2_atom_forge_pop(&self->forge, &self->frame);
}

static void
cleanup(LV2_Handle handle)
{
	free(handle);
}


/**
   ==== State Methods ====

   This plugin's state consists of two basic properties: one `int` and one
   `float`.  No files are used.  Note these values are POD, but not portable,
   since different machines may have a different integer endianness or floating
   point format.  However, since standard Atom types are used, a good host will
   be able to save them portably as text anyway.
*/
static LV2_State_Status
state_save(LV2_Handle                instance,
           LV2_State_Store_Function  store,
           LV2_State_Handle          handle,
           uint32_t                  flags,
           const LV2_Feature* const* features)
{
	EgScope* self = (EgScope*)instance;
	if (!self) {
		return LV2_STATE_SUCCESS;
	}

	store(handle, self->uris.ui_spp,
	      (void*)&self->ui_spp, sizeof(uint32_t),
	      self->uris.atom_Int,
	      LV2_STATE_IS_POD);

	store(handle, self->uris.ui_amp,
	      (void*)&self->ui_amp, sizeof(float),
	      self->uris.atom_Float,
	      LV2_STATE_IS_POD);

	return LV2_STATE_SUCCESS;
}

static LV2_State_Status
state_restore(LV2_Handle                  instance,
              LV2_State_Retrieve_Function retrieve,
              LV2_State_Handle            handle,
              uint32_t                    flags,
              const LV2_Feature* const*   features)
{
	EgScope* self = (EgScope*)instance;

	size_t   size;
	uint32_t type;
	uint32_t valflags;

	const void* spp = retrieve(
		handle, self->uris.ui_spp, &size, &type, &valflags);
	if (spp && size == sizeof(uint32_t) && type == self->uris.atom_Int) {
		self->ui_spp              = *((const uint32_t*)spp);
		self->send_settings_to_ui = true;
	}

	const void* amp = retrieve(
		handle, self->uris.ui_amp, &size, &type, &valflags);
	if (amp && size == sizeof(float) && type == self->uris.atom_Float) {
		self->ui_amp              = *((const float*)amp);
		self->send_settings_to_ui = true;
	}

	return LV2_STATE_SUCCESS;
}

static const void*
extension_data(const char* uri)
{
	static const LV2_State_Interface state = { state_save, state_restore };
	if (!strcmp(uri, LV2_STATE__interface)) {
		return &state;
	}
	return NULL;
}

/** ==== Plugin Descriptors ==== */
static const LV2_Descriptor descriptor_mono = {
	SCO_URI "#Mono",
	instantiate,
	connect_port,
	NULL,
	run,
	NULL,
	cleanup,
	extension_data
};

static const LV2_Descriptor descriptor_stereo = {
	SCO_URI "#Stereo",
	instantiate,
	connect_port,
	NULL,
	run,
	NULL,
	cleanup,
	extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor_mono;
	case 1:
		return &descriptor_stereo;
	default:
		return NULL;
	}
}
