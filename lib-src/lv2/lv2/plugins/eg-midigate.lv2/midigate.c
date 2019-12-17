/*
  Copyright 2013-2016 David Robillard <d@drobilla.net>

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
#include "lv2/atom/util.h"
#include "lv2/core/lv2.h"
#include "lv2/core/lv2_util.h"
#include "lv2/log/log.h"
#include "lv2/log/logger.h"
#include "lv2/midi/midi.h"
#include "lv2/urid/urid.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MIDIGATE_URI "http://lv2plug.in/plugins/eg-midigate"

typedef enum {
	MIDIGATE_CONTROL = 0,
	MIDIGATE_IN      = 1,
	MIDIGATE_OUT     = 2
} PortIndex;

typedef struct {
	// Port buffers
	const LV2_Atom_Sequence* control;
	const float*             in;
	float*                   out;

	// Features
	LV2_URID_Map*  map;
	LV2_Log_Logger logger;

	struct {
		LV2_URID midi_MidiEvent;
	} uris;

	unsigned n_active_notes;
	unsigned program;  // 0 = normal, 1 = inverted
} Midigate;

static LV2_Handle
instantiate(const LV2_Descriptor*     descriptor,
            double                    rate,
            const char*               bundle_path,
            const LV2_Feature* const* features)
{
	Midigate* self = (Midigate*)calloc(1, sizeof(Midigate));
	if (!self) {
		return NULL;
	}

	// Scan host features for URID map
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

	self->uris.midi_MidiEvent = self->map->map(
		self->map->handle, LV2_MIDI__MidiEvent);

	return (LV2_Handle)self;
}

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	Midigate* self = (Midigate*)instance;

	switch ((PortIndex)port) {
	case MIDIGATE_CONTROL:
		self->control = (const LV2_Atom_Sequence*)data;
		break;
	case MIDIGATE_IN:
		self->in = (const float*)data;
		break;
	case MIDIGATE_OUT:
		self->out = (float*)data;
		break;
	}
}

static void
activate(LV2_Handle instance)
{
	Midigate* self = (Midigate*)instance;
	self->n_active_notes = 0;
	self->program        = 0;
}

/**
   A function to write a chunk of output, to be called from run().  If the gate
   is high, then the input will be passed through for this chunk, otherwise
   silence is written.
*/
static void
write_output(Midigate* self, uint32_t offset, uint32_t len)
{
	const bool active = (self->program == 0)
		? (self->n_active_notes > 0)
		: (self->n_active_notes == 0);
	if (active) {
		memcpy(self->out + offset, self->in + offset, len * sizeof(float));
	} else {
		memset(self->out + offset, 0, len * sizeof(float));
	}
}

/**
   This plugin works through the cycle in chunks starting at offset zero.  The
   +offset+ represents the current time within this this cycle, so
   the output from 0 to +offset+ has already been written.

   MIDI events are read in a loop.  In each iteration, the number of active
   notes (on note on and note off) or the program (on program change) is
   updated, then the output is written up until the current event time.  Then
   +offset+ is updated and the next event is processed.  After the loop the
   final chunk from the last event to the end of the cycle is emitted.

   There is currently no standard way to describe MIDI programs in LV2, so the
   host has no way of knowing that these programs exist and should be presented
   to the user.  A future version of LV2 will address this shortcoming.

   This pattern of iterating over input events and writing output along the way
   is a common idiom for writing sample accurate output based on event input.

   Note that this simple example simply writes input or zero for each sample
   based on the gate.  A serious implementation would need to envelope the
   transition to avoid aliasing.
*/
static void
run(LV2_Handle instance, uint32_t sample_count)
{
	Midigate* self   = (Midigate*)instance;
	uint32_t  offset = 0;

	LV2_ATOM_SEQUENCE_FOREACH(self->control, ev) {
		if (ev->body.type == self->uris.midi_MidiEvent) {
			const uint8_t* const msg = (const uint8_t*)(ev + 1);
			switch (lv2_midi_message_type(msg)) {
			case LV2_MIDI_MSG_NOTE_ON:
				++self->n_active_notes;
				break;
			case LV2_MIDI_MSG_NOTE_OFF:
				if (self->n_active_notes > 0) {
					--self->n_active_notes;
				}
				break;
			case LV2_MIDI_MSG_CONTROLLER:
				if (msg[1] == LV2_MIDI_CTL_ALL_NOTES_OFF) {
					self->n_active_notes = 0;
				}
				break;
			case LV2_MIDI_MSG_PGM_CHANGE:
				if (msg[1] == 0 || msg[1] == 1) {
					self->program = msg[1];
				}
				break;
			default: break;
			}
		}

		write_output(self, offset, ev->time.frames - offset);
		offset = (uint32_t)ev->time.frames;
	}

	write_output(self, offset, sample_count - offset);
}

/**
   We have no resources to free on deactivation.
   Note that the next call to activate will re-initialise the state, namely
   self->n_active_notes, so there is no need to do so here.
*/
static void
deactivate(LV2_Handle instance)
{}

static void
cleanup(LV2_Handle instance)
{
	free(instance);
}

/**
   This plugin also has no extension data to return.
*/
static const void*
extension_data(const char* uri)
{
	return NULL;
}

static const LV2_Descriptor descriptor = {
	MIDIGATE_URI,
	instantiate,
	connect_port,
	activate,
	run,
	deactivate,
	cleanup,
	extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor*
lv2_descriptor(uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}
