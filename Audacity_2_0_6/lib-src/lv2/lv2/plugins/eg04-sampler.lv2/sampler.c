/*
  LV2 Sampler Example Plugin
  Copyright 2011-2012 David Robillard <d@drobilla.net>
  Copyright 2011 Gabriel M. Beddingfield <gabriel@teuton.org>
  Copyright 2011 James Morris <jwm.art.net@gmail.com>

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

#include <math.h>
#include <stdlib.h>
#include <string.h>
#ifndef __cplusplus
#    include <stdbool.h>
#endif

#include <sndfile.h>

#include "lv2/lv2plug.in/ns/ext/atom/forge.h"
#include "lv2/lv2plug.in/ns/ext/atom/util.h"
#include "lv2/lv2plug.in/ns/ext/log/log.h"
#include "lv2/lv2plug.in/ns/ext/log/logger.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"
#include "lv2/lv2plug.in/ns/ext/patch/patch.h"
#include "lv2/lv2plug.in/ns/ext/state/state.h"
#include "lv2/lv2plug.in/ns/ext/urid/urid.h"
#include "lv2/lv2plug.in/ns/ext/worker/worker.h"
#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#include "./uris.h"

enum {
	SAMPLER_CONTROL = 0,
	SAMPLER_NOTIFY  = 1,
	SAMPLER_OUT     = 2
};

static const char* default_sample_file = "click.wav";

typedef struct {
	SF_INFO info;      // Info about sample from sndfile
	float*  data;      // Sample data in float
	char*   path;      // Path of file
	size_t  path_len;  // Length of path
} Sample;

typedef struct {
	// Features
	LV2_URID_Map*        map;
	LV2_Worker_Schedule* schedule;
	LV2_Log_Log*         log;

	// Forge for creating atoms
	LV2_Atom_Forge forge;

	// Logger convenience API
	LV2_Log_Logger logger;

	// Sample
	Sample* sample;

	// Ports
	const LV2_Atom_Sequence* control_port;
	LV2_Atom_Sequence*       notify_port;
	float*                   output_port;

	// Forge frame for notify port (for writing worker replies)
	LV2_Atom_Forge_Frame notify_frame;

	// URIs
	SamplerURIs uris;

	// Current position in run()
	uint32_t frame_offset;

	// Playback state
	sf_count_t frame;
	bool       play;
} Sampler;

/**
   An atom-like message used internally to apply/free samples.

   This is only used internally to communicate with the worker, it is never
   sent to the outside world via a port since it is not POD.  It is convenient
   to use an Atom header so actual atoms can be easily sent through the same
   ringbuffer.
*/
typedef struct {
	LV2_Atom atom;
	Sample*  sample;
} SampleMessage;

/**
   Load a new sample and return it.

   Since this is of course not a real-time safe action, this is called in the
   worker thread only.  The sample is loaded and returned only, plugin state is
   not modified.
*/
static Sample*
load_sample(Sampler* self, const char* path)
{
	const size_t path_len  = strlen(path);

	lv2_log_trace(&self->logger, "Loading sample %s\n", path);

	Sample* const  sample  = (Sample*)malloc(sizeof(Sample));
	SF_INFO* const info    = &sample->info;
	SNDFILE* const sndfile = sf_open(path, SFM_READ, info);

	if (!sndfile || !info->frames || (info->channels != 1)) {
		lv2_log_error(&self->logger, "Failed to open sample '%s'\n", path);
		free(sample);
		return NULL;
	}

	// Read data
	float* const data = malloc(sizeof(float) * info->frames);
	if (!data) {
		lv2_log_error(&self->logger, "Failed to allocate memory for sample\n");
		return NULL;
	}
	sf_seek(sndfile, 0ul, SEEK_SET);
	sf_read_float(sndfile, data, info->frames);
	sf_close(sndfile);

	// Fill sample struct and return it
	sample->data     = data;
	sample->path     = (char*)malloc(path_len + 1);
	sample->path_len = path_len;
	memcpy(sample->path, path, path_len + 1);

	return sample;
}

static void
free_sample(Sampler* self, Sample* sample)
{
	if (sample) {
		lv2_log_trace(&self->logger, "Freeing %s\n", sample->path);
		free(sample->path);
		free(sample->data);
		free(sample);
	}
}

/**
   Do work in a non-realtime thread.

   This is called for every piece of work scheduled in the audio thread using
   self->schedule->schedule_work().  A reply can be sent back to the audio
   thread using the provided respond function.
*/
static LV2_Worker_Status
work(LV2_Handle                  instance,
     LV2_Worker_Respond_Function respond,
     LV2_Worker_Respond_Handle   handle,
     uint32_t                    size,
     const void*                 data)
{
	Sampler*        self = (Sampler*)instance;
	const LV2_Atom* atom = (const LV2_Atom*)data;
	if (atom->type == self->uris.eg_freeSample) {
		// Free old sample
		const SampleMessage* msg = (const SampleMessage*)data;
		free_sample(self, msg->sample);
	} else {
		// Handle set message (load sample).
		const LV2_Atom_Object* obj = (const LV2_Atom_Object*)data;

		// Get file path from message
		const LV2_Atom* file_path = read_set_file(&self->uris, obj);
		if (!file_path) {
			return LV2_WORKER_ERR_UNKNOWN;
		}

		// Load sample.
		Sample* sample = load_sample(self, LV2_ATOM_BODY_CONST(file_path));
		if (sample) {
			// Loaded sample, send it to run() to be applied.
			respond(handle, sizeof(sample), &sample);
		}
	}

	return LV2_WORKER_SUCCESS;
}

/**
   Handle a response from work() in the audio thread.

   When running normally, this will be called by the host after run().  When
   freewheeling, this will be called immediately at the point the work was
   scheduled.
*/
static LV2_Worker_Status
work_response(LV2_Handle  instance,
              uint32_t    size,
              const void* data)
{
	Sampler* self = (Sampler*)instance;

	SampleMessage msg = { { sizeof(Sample*), self->uris.eg_freeSample },
	                      self->sample };

	// Send a message to the worker to free the current sample
	self->schedule->schedule_work(self->schedule->handle, sizeof(msg), &msg);

	// Install the new sample
	self->sample = *(Sample*const*)data;

	// Send a notification that we're using a new sample.
	lv2_atom_forge_frame_time(&self->forge, self->frame_offset);
	write_set_file(&self->forge, &self->uris,
	               self->sample->path,
	               self->sample->path_len);

	return LV2_WORKER_SUCCESS;
}

static void
connect_port(LV2_Handle instance,
             uint32_t   port,
             void*      data)
{
	Sampler* self = (Sampler*)instance;
	switch (port) {
	case SAMPLER_CONTROL:
		self->control_port = (const LV2_Atom_Sequence*)data;
		break;
	case SAMPLER_NOTIFY:
		self->notify_port = (LV2_Atom_Sequence*)data;
		break;
	case SAMPLER_OUT:
		self->output_port = (float*)data;
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
	// Allocate and initialise instance structure.
	Sampler* self = (Sampler*)malloc(sizeof(Sampler));
	if (!self) {
		return NULL;
	}
	memset(self, 0, sizeof(Sampler));

	// Get host features
	for (int i = 0; features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_URID__map)) {
			self->map = (LV2_URID_Map*)features[i]->data;
		} else if (!strcmp(features[i]->URI, LV2_WORKER__schedule)) {
			self->schedule = (LV2_Worker_Schedule*)features[i]->data;
		} else if (!strcmp(features[i]->URI, LV2_LOG__log)) {
			self->log = (LV2_Log_Log*)features[i]->data;
		}
	}
	if (!self->map) {
		lv2_log_error(&self->logger, "Missing feature urid:map\n");
		goto fail;
	} else if (!self->schedule) {
		lv2_log_error(&self->logger, "Missing feature work:schedule\n");
		goto fail;
	}

	// Map URIs and initialise forge/logger
	map_sampler_uris(self->map, &self->uris);
	lv2_atom_forge_init(&self->forge, self->map);
	lv2_log_logger_init(&self->logger, self->map, self->log);

	// Load the default sample file
	const size_t path_len    = strlen(path);
	const size_t file_len    = strlen(default_sample_file);
	const size_t len         = path_len + file_len;
	char*        sample_path = (char*)malloc(len + 1);
	snprintf(sample_path, len + 1, "%s%s", path, default_sample_file);
	self->sample = load_sample(self, sample_path);
	free(sample_path);

	return (LV2_Handle)self;

fail:
	free(self);
	return 0;
}

static void
cleanup(LV2_Handle instance)
{
	Sampler* self = (Sampler*)instance;
	free_sample(self, self->sample);
	free(self);
}

static void
run(LV2_Handle instance,
    uint32_t   sample_count)
{
	Sampler*     self        = (Sampler*)instance;
	SamplerURIs* uris        = &self->uris;
	sf_count_t   start_frame = 0;
	sf_count_t   pos         = 0;
	float*       output      = self->output_port;

	// Set up forge to write directly to notify output port.
	const uint32_t notify_capacity = self->notify_port->atom.size;
	lv2_atom_forge_set_buffer(&self->forge,
	                          (uint8_t*)self->notify_port,
	                          notify_capacity);

	// Start a sequence in the notify output port.
	lv2_atom_forge_sequence_head(&self->forge, &self->notify_frame, 0);

	// Read incoming events
	LV2_ATOM_SEQUENCE_FOREACH(self->control_port, ev) {
		self->frame_offset = ev->time.frames;
		if (ev->body.type == uris->midi_Event) {
			const uint8_t* const msg = (const uint8_t*)(ev + 1);
			switch (lv2_midi_message_type(msg)) {
			case LV2_MIDI_MSG_NOTE_ON:
				start_frame = ev->time.frames;
				self->frame = 0;
				self->play  = true;
				break;
			default:
				break;
			}
		} else if (is_object_type(uris, ev->body.type)) {
			const LV2_Atom_Object* obj = (LV2_Atom_Object*)&ev->body;
			if (obj->body.otype == uris->patch_Set) {
				// Received a set message, send it to the worker.
				lv2_log_trace(&self->logger, "Queueing set message\n");
				self->schedule->schedule_work(self->schedule->handle,
				                              lv2_atom_total_size(&ev->body),
				                              &ev->body);
			} else {
				lv2_log_trace(&self->logger,
				              "Unknown object type %d\n", obj->body.otype);
			}
		} else {
			lv2_log_trace(&self->logger,
			              "Unknown event type %d\n", ev->body.type);
		}
	}

	// Render the sample (possibly already in progress)
	if (self->play) {
		uint32_t       f  = self->frame;
		const uint32_t lf = self->sample->info.frames;

		for (pos = 0; pos < start_frame; ++pos) {
			output[pos] = 0;
		}

		for (; pos < sample_count && f < lf; ++pos, ++f) {
			output[pos] = self->sample->data[f];
		}

		self->frame = f;

		if (f == lf) {
			self->play = false;
		}
	}

	// Add zeros to end if sample not long enough (or not playing)
	for (; pos < sample_count; ++pos) {
		output[pos] = 0.0f;
	}
}

static LV2_State_Status
save(LV2_Handle                instance,
     LV2_State_Store_Function  store,
     LV2_State_Handle          handle,
     uint32_t                  flags,
     const LV2_Feature* const* features)
{
	Sampler* self = (Sampler*)instance;
	if (!self->sample) {
		return LV2_STATE_SUCCESS;
	}

	LV2_State_Map_Path* map_path = NULL;
	for (int i = 0; features[i]; ++i) {
		if (!strcmp(features[i]->URI, LV2_STATE__mapPath)) {
			map_path = (LV2_State_Map_Path*)features[i]->data;
		}
	}

	char* apath = map_path->abstract_path(map_path->handle, self->sample->path);

	store(handle,
	      self->uris.eg_sample,
	      apath,
	      strlen(self->sample->path) + 1,
	      self->uris.atom_Path,
	      LV2_STATE_IS_POD | LV2_STATE_IS_PORTABLE);

	free(apath);

	return LV2_STATE_SUCCESS;
}

static LV2_State_Status
restore(LV2_Handle                  instance,
        LV2_State_Retrieve_Function retrieve,
        LV2_State_Handle            handle,
        uint32_t                    flags,
        const LV2_Feature* const*   features)
{
	Sampler* self = (Sampler*)instance;

	size_t   size;
	uint32_t type;
	uint32_t valflags;

	const void* value = retrieve(
		handle,
		self->uris.eg_sample,
		&size, &type, &valflags);

	if (value) {
		const char* path = (const char*)value;
		lv2_log_trace(&self->logger, "Restoring file %s\n", path);
		free_sample(self, self->sample);
		self->sample = load_sample(self, path);
	}

	return LV2_STATE_SUCCESS;
}

static const void*
extension_data(const char* uri)
{
	static const LV2_State_Interface  state  = { save, restore };
	static const LV2_Worker_Interface worker = { work, work_response, NULL };
	if (!strcmp(uri, LV2_STATE__interface)) {
		return &state;
	} else if (!strcmp(uri, LV2_WORKER__interface)) {
		return &worker;
	}
	return NULL;
}

static const LV2_Descriptor descriptor = {
	EG_SAMPLER_URI,
	instantiate,
	connect_port,
	NULL,  // activate,
	run,
	NULL,  // deactivate,
	cleanup,
	extension_data
};

LV2_SYMBOL_EXPORT
const LV2_Descriptor* lv2_descriptor(uint32_t index)
{
	switch (index) {
	case 0:
		return &descriptor;
	default:
		return NULL;
	}
}
