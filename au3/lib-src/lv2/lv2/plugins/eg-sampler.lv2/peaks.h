/*
  LV2 audio peaks utilities
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

/**
   This file defines utilities for sending and receiving audio peaks for
   waveform display.  The functionality is divided into two objects:
   PeaksSender, for sending peaks updates from the plugin, and PeaksReceiver,
   for receiving such updates and caching the peaks.

   This allows peaks for a waveform of any size at any resolution to be
   requested, with reasonably sized incremental updates sent over plugin ports.
*/

#ifndef PEAKS_H_INCLUDED
#define PEAKS_H_INCLUDED

#include "lv2/atom/forge.h"

#include <math.h>
#include <stdlib.h>

#define PEAKS_URI          "http://lv2plug.in/ns/peaks#"
#define PEAKS__PeakUpdate  PEAKS_URI "PeakUpdate"
#define PEAKS__magnitudes  PEAKS_URI "magnitudes"
#define PEAKS__offset      PEAKS_URI "offset"
#define PEAKS__total       PEAKS_URI "total"

#ifndef MIN
#    define MIN(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef MAX
#    define MAX(a, b) (((a) > (b)) ? (a) : (b))
#endif

typedef struct {
	LV2_URID atom_Float;
	LV2_URID atom_Int;
	LV2_URID atom_Vector;
	LV2_URID peaks_PeakUpdate;
	LV2_URID peaks_magnitudes;
	LV2_URID peaks_offset;
	LV2_URID peaks_total;
} PeaksURIs;

typedef struct {
	PeaksURIs    uris;            ///< URIDs used in protocol
	const float* samples;         ///< Sample data
	uint32_t     n_samples;       ///< Total number of samples
	uint32_t     n_peaks;         ///< Total number of peaks
	uint32_t     current_offset;  ///< Current peak offset
	bool         sending;         ///< True iff currently sending
} PeaksSender;

typedef struct {
	PeaksURIs uris;     ///< URIDs used in protocol
	float*    peaks;    ///< Received peaks, or zeroes
	uint32_t  n_peaks;  ///< Total number of peaks
} PeaksReceiver;

/**
   Map URIs used in the peaks protocol.
*/
static inline void
peaks_map_uris(PeaksURIs* uris, LV2_URID_Map* map)
{
	uris->atom_Float       = map->map(map->handle, LV2_ATOM__Float);
	uris->atom_Int         = map->map(map->handle, LV2_ATOM__Int);
	uris->atom_Vector      = map->map(map->handle, LV2_ATOM__Vector);
	uris->peaks_PeakUpdate = map->map(map->handle, PEAKS__PeakUpdate);
	uris->peaks_magnitudes = map->map(map->handle, PEAKS__magnitudes);
	uris->peaks_offset     = map->map(map->handle, PEAKS__offset);
	uris->peaks_total      = map->map(map->handle, PEAKS__total);
}

/**
   Initialise peaks sender.  The new sender is inactive and will do nothing
   when `peaks_sender_send()` is called, until a transmission is started with
   `peaks_sender_start()`.
*/
static inline PeaksSender*
peaks_sender_init(PeaksSender* sender, LV2_URID_Map* map)
{
	memset(sender, 0, sizeof(*sender));
	peaks_map_uris(&sender->uris, map);
	return sender;
}

/**
   Prepare to start a new peaks transmission.  After this is called, the peaks
   can be sent with successive calls to `peaks_sender_send()`.
*/
static inline void
peaks_sender_start(PeaksSender* sender,
                   const float* samples,
                   uint32_t     n_samples,
                   uint32_t     n_peaks)
{
	sender->samples        = samples;
	sender->n_samples      = n_samples;
	sender->n_peaks        = n_peaks;
	sender->current_offset = 0;
	sender->sending        = true;
}

/**
   Forge a message which sends a range of peaks.  Writes a peaks:PeakUpdate
   object to `forge`, like:

   [source,n3]
   ----
   []
   	   a peaks:PeakUpdate ;
   	   peaks:offset 256 ;
   	   peaks:total 1024 ;
   	   peaks:magnitudes [ 0.2f, 0.3f, ... ] .
   ----
*/
static inline bool
peaks_sender_send(PeaksSender*    sender,
                  LV2_Atom_Forge* forge,
                  uint32_t        n_frames,
                  uint32_t        offset)
{
	const PeaksURIs* uris = &sender->uris;
	if (!sender->sending || sender->current_offset >= sender->n_peaks) {
		return sender->sending = false;
	}

	// Start PeakUpdate object
	lv2_atom_forge_frame_time(forge, offset);
	LV2_Atom_Forge_Frame frame;
	lv2_atom_forge_object(forge, &frame, 0, uris->peaks_PeakUpdate);

	// eg:offset = OFFSET
	lv2_atom_forge_key(forge, uris->peaks_offset);
	lv2_atom_forge_int(forge, sender->current_offset);

	// eg:total = TOTAL
	lv2_atom_forge_key(forge, uris->peaks_total);
	lv2_atom_forge_int(forge, sender->n_peaks);

	// eg:magnitudes = Vector<Float>(PEAK, PEAK, ...)
	lv2_atom_forge_key(forge, uris->peaks_magnitudes);
	LV2_Atom_Forge_Frame vec_frame;
	lv2_atom_forge_vector_head(
		forge, &vec_frame, sizeof(float), uris->atom_Float);

	// Calculate how many peaks to send this update
	const int      chunk_size = MAX(1, sender->n_samples / sender->n_peaks);
	const uint32_t space      = forge->size - forge->offset;
	const uint32_t remaining  = sender->n_peaks - sender->current_offset;
	const int      n_update   = MIN(remaining,
	                                MIN(n_frames / 4, space / sizeof(float)));

	// Calculate peak (maximum magnitude) for each chunk
	for (int i = 0; i < n_update; ++i) {
		const int start = (sender->current_offset + i) * chunk_size;
		float     peak  = 0.0f;
		for (int j = 0; j < chunk_size; ++j) {
			peak = fmaxf(peak, fabsf(sender->samples[start + j]));
		}
		lv2_atom_forge_float(forge, peak);
	}

	// Finish message
	lv2_atom_forge_pop(forge, &vec_frame);
	lv2_atom_forge_pop(forge, &frame);

	sender->current_offset += n_update;
	return true;
}

/**
   Initialise a peaks receiver.  The receiver stores an array of all peaks,
   which is updated incrementally with peaks_receiver_receive().
*/
static inline PeaksReceiver*
peaks_receiver_init(PeaksReceiver* receiver, LV2_URID_Map* map)
{
	memset(receiver, 0, sizeof(*receiver));
	peaks_map_uris(&receiver->uris, map);
	return receiver;
}

/**
   Clear stored peaks and free all memory.  This should be called when the
   peaks are to be updated with a different audio source.
*/
static inline void
peaks_receiver_clear(PeaksReceiver* receiver)
{
	free(receiver->peaks);
	receiver->peaks   = NULL;
	receiver->n_peaks = 0;
}

/**
   Handle PeakUpdate message.

   The stored peaks array is updated with the slice of peaks in `update`,
   resizing if necessary while preserving contents.

   Returns 0 if peaks have been updated, negative on error.
*/
static inline int
peaks_receiver_receive(PeaksReceiver* receiver, const LV2_Atom_Object* update)
{
	const PeaksURIs* uris = &receiver->uris;

	// Get properties of interest from update
	const LV2_Atom_Int*    offset = NULL;
	const LV2_Atom_Int*    total  = NULL;
	const LV2_Atom_Vector* peaks  = NULL;
	lv2_atom_object_get_typed(update,
	                          uris->peaks_offset,     &offset, uris->atom_Int,
	                          uris->peaks_total,      &total,  uris->atom_Int,
	                          uris->peaks_magnitudes, &peaks,  uris->atom_Vector,
	                          0);

	if (!offset || !total || !peaks ||
	    peaks->body.child_type != uris->atom_Float) {
		return -1;  // Invalid update
	}

	const uint32_t n = (uint32_t)total->body;
	if (receiver->n_peaks != n) {
		// Update is for a different total number of peaks, resize
		receiver->peaks = (float*)realloc(receiver->peaks, n * sizeof(float));
		if (receiver->n_peaks > 0 && receiver->n_peaks < n) {
			/* The peaks array is being expanded.  Copy the old peaks,
			   duplicating each as necessary to fill the new peaks buffer.
			   This preserves the current peaks so that the peaks array can be
			   reasonably drawn at any time, but the resolution will increase
			   as new updates arrive. */
			const int n_per = n / receiver->n_peaks;
			for (int i = n - 1; i >= 0; --i) {
				receiver->peaks[i] = receiver->peaks[i / n_per];
			}
		} else if (receiver->n_peaks > 0) {
			/* The peak array is being shrunk.  Similar to the above. */
			const int n_per = receiver->n_peaks / n;
			for (int i = n - 1; i >= 0; --i) {
				receiver->peaks[i] = receiver->peaks[i * n_per];
			}
		}
		receiver->n_peaks = n;
	}

	// Copy vector contents to corresponding range in peaks array
	memcpy(receiver->peaks + offset->body,
	       peaks + 1,
	       peaks->atom.size - sizeof(LV2_Atom_Vector_Body));

	return 0;
}

#endif // PEAKS_H_INCLUDED
