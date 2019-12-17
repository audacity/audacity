/*
  Copyright 2008-2015 David Robillard <http://drobilla.net>

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
   @file event-helpers.h Helper functions for the LV2 Event extension
   <http://lv2plug.in/ns/ext/event>.
*/

#ifndef LV2_EVENT_HELPERS_H
#define LV2_EVENT_HELPERS_H

#include "lv2/core/attributes.h"
#include "lv2/event/event.h"

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

LV2_DISABLE_DEPRECATION_WARNINGS

/** @file
 * Helper functions for the LV2 Event extension
 * <http://lv2plug.in/ns/ext/event>.
 *
 * These functions are provided for convenience only, use of them is not
 * required for supporting lv2ev (i.e. the events extension is defined by the
 * raw buffer format described in lv2_event.h and NOT by this API).
 *
 * Note that these functions are all static inline which basically means:
 * do not take the address of these functions. */


/** Pad a size to 64 bits (for event sizes) */
static inline uint16_t
lv2_event_pad_size(uint16_t size)
{
	return (uint16_t)(size + 7U) & (uint16_t)(~7U);
}


/** Initialize (empty, reset..) an existing event buffer.
 * The contents of buf are ignored entirely and overwritten, except capacity
 * which is unmodified. */
static inline void
lv2_event_buffer_reset(LV2_Event_Buffer*  buf,
                       uint16_t           stamp_type,
                       uint8_t*           data)
{
	buf->data = data;
	buf->header_size = sizeof(LV2_Event_Buffer);
	buf->stamp_type = stamp_type;
	buf->event_count = 0;
	buf->size = 0;
}


/** Allocate a new, empty event buffer. */
static inline LV2_Event_Buffer*
lv2_event_buffer_new(uint32_t capacity, uint16_t stamp_type)
{
	const size_t      size = sizeof(LV2_Event_Buffer) + capacity;
	LV2_Event_Buffer* buf  = (LV2_Event_Buffer*)malloc(size);
	if (buf != NULL) {
		buf->capacity = capacity;
		lv2_event_buffer_reset(buf, stamp_type, (uint8_t *)(buf + 1));
		return buf;
	}
	return NULL;
}


/** An iterator over an LV2_Event_Buffer.
 *
 * Multiple simultaneous read iterators over a single buffer is fine,
 * but changing the buffer invalidates all iterators (e.g. RW Lock). */
typedef struct {
	LV2_Event_Buffer* buf;
	uint32_t          offset;
} LV2_Event_Iterator;


/** Reset an iterator to point to the start of `buf`.
 * @return True if `iter` is valid, otherwise false (buffer is empty) */
static inline bool
lv2_event_begin(LV2_Event_Iterator* iter,
                LV2_Event_Buffer*   buf)
{
	iter->buf = buf;
	iter->offset = 0;
	return (buf->size > 0);
}


/** Check if `iter` is valid.
 * @return True if `iter` is valid, otherwise false (past end of buffer) */
static inline bool
lv2_event_is_valid(LV2_Event_Iterator* iter)
{
	return (iter->buf && (iter->offset < iter->buf->size));
}


/** Advance `iter` forward one event.
 * `iter` must be valid.
 * @return True if `iter` is valid, otherwise false (reached end of buffer) */
static inline bool
lv2_event_increment(LV2_Event_Iterator* iter)
{
	if (!lv2_event_is_valid(iter)) {
		return false;
	}

	LV2_Event* const ev = (LV2_Event*)(iter->buf->data + iter->offset);

	iter->offset += lv2_event_pad_size(
		(uint16_t)((uint16_t)sizeof(LV2_Event) + ev->size));

	return true;
}


/** Dereference an event iterator (get the event currently pointed at).
 * `iter` must be valid.
 * `data` if non-NULL, will be set to point to the contents of the event
 *         returned.
 * @return A Pointer to the event `iter` is currently pointing at, or NULL
 *         if the end of the buffer is reached (in which case `data` is
 *         also set to NULL). */
static inline LV2_Event*
lv2_event_get(LV2_Event_Iterator* iter,
              uint8_t**           data)
{
	if (!lv2_event_is_valid(iter)) {
		return NULL;
	}

	LV2_Event* const ev = (LV2_Event*)(iter->buf->data + iter->offset);

	if (data) {
		*data = (uint8_t*)ev + sizeof(LV2_Event);
	}

	return ev;
}


/** Write an event at `iter`.
 * The event (if any) pointed to by `iter` will be overwritten, and `iter`
 * incremented to point to the following event (i.e. several calls to this
 * function can be done in sequence without twiddling iter in-between).
 * @return True if event was written, otherwise false (buffer is full). */
static inline bool
lv2_event_write(LV2_Event_Iterator* iter,
                uint32_t            frames,
                uint32_t            subframes,
                uint16_t            type,
                uint16_t            size,
                const uint8_t*      data)
{
	if (!iter->buf) {
		return false;
	}

	if (iter->buf->capacity - iter->buf->size < sizeof(LV2_Event) + size) {
		return false;
	}

	LV2_Event* const ev = (LV2_Event*)(iter->buf->data + iter->offset);

	ev->frames = frames;
	ev->subframes = subframes;
	ev->type = type;
	ev->size = size;
	memcpy((uint8_t*)ev + sizeof(LV2_Event), data, size);
	++iter->buf->event_count;

	size = lv2_event_pad_size((uint16_t)(sizeof(LV2_Event) + size));
	iter->buf->size += size;
	iter->offset    += size;

	return true;
}


/** Reserve space for an event in the buffer and return a pointer to
    the memory where the caller can write the event data, or NULL if there
    is not enough room in the buffer. */
static inline uint8_t*
lv2_event_reserve(LV2_Event_Iterator* iter,
                  uint32_t frames,
                  uint32_t subframes,
                  uint16_t type,
                  uint16_t size)
{
	const uint16_t total_size = (uint16_t)(sizeof(LV2_Event) + size);
	if (iter->buf->capacity - iter->buf->size < total_size) {
		return NULL;
	}

	LV2_Event* const ev = (LV2_Event*)(iter->buf->data + iter->offset);

	ev->frames = frames;
	ev->subframes = subframes;
	ev->type = type;
	ev->size = size;
	++iter->buf->event_count;

	const uint16_t padded_size = lv2_event_pad_size(total_size);
	iter->buf->size += padded_size;
	iter->offset    += padded_size;

	return (uint8_t*)ev + sizeof(LV2_Event);
}


/** Write an event at `iter`.
 * The event (if any) pointed to by `iter` will be overwritten, and `iter`
 * incremented to point to the following event (i.e. several calls to this
 * function can be done in sequence without twiddling iter in-between).
 * @return True if event was written, otherwise false (buffer is full). */
static inline bool
lv2_event_write_event(LV2_Event_Iterator* iter,
                      const LV2_Event*    ev,
                      const uint8_t*      data)
{
	const uint16_t total_size = (uint16_t)(sizeof(LV2_Event) + ev->size);
	if (iter->buf->capacity - iter->buf->size < total_size) {
		return false;
	}

	LV2_Event* const write_ev = (LV2_Event*)(iter->buf->data + iter->offset);

	*write_ev = *ev;
	memcpy((uint8_t*)write_ev + sizeof(LV2_Event), data, ev->size);
	++iter->buf->event_count;

	const uint16_t padded_size = lv2_event_pad_size(total_size);
	iter->buf->size += padded_size;
	iter->offset    += padded_size;

	return true;
}

LV2_RESTORE_WARNINGS

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif /* LV2_EVENT_HELPERS_H */
