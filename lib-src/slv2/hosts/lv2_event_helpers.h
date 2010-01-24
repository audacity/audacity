/* lv2_event_helpers.h - Helper functions for the LV2 events extension.
 *
 * Copyright (C) 2008 Dave Robillard <dave@drobilla.net>
 *
 * This header is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This header is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this header; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 01222-1307 USA
 */

#ifndef LV2_EVENT_HELPERS_H
#define LV2_EVENT_HELPERS_H

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <malloc.h>
#include <assert.h>
#include "lv2_event.h"

/** @file
 * This header defines some helper functions for the the LV2 events extension
 * with URI <http://lv2plug.in/ns/ext/event> ('lv2ev').
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
	return (size + 7) & (~7);
}


/** Initialize (empty, reset..) an existing event buffer.
 * The contents of buf are ignored entirely and overwritten, except capacity
 * which is unmodified. */
static inline void
lv2_event_buffer_reset(LV2_Event_Buffer* buf, uint16_t stamp_type, uint8_t *data)
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
	LV2_Event_Buffer* buf = (LV2_Event_Buffer*)malloc(sizeof(LV2_Event_Buffer) + capacity);
	if (buf != NULL) {
		buf->capacity = capacity;
		lv2_event_buffer_reset(buf, stamp_type, (uint8_t *)(buf + 1));
		return buf;
	} else {
		return NULL;
	}
}


/** An iterator over an LV2_Event_Buffer.
 *
 * Multiple simultaneous read iterators over a single buffer is fine,
 * but changing the buffer invalidates all iterators (e.g. RW Lock). */
typedef struct {
	LV2_Event_Buffer* buf;
	uint32_t          offset;
} LV2_Event_Iterator;


/** Reset an iterator to point to the start of @a buf.
 * @return True if @a iter is valid, otherwise false (buffer is empty) */
static inline bool
lv2_event_begin(LV2_Event_Iterator* iter,
                LV2_Event_Buffer*   buf)
{
	iter->buf = buf;
	iter->offset = 0;
	return (buf->size > 0);
}


/** Check if @a iter is valid..
 * @return True if @a iter is valid, otherwise false (past end of buffer) */
static inline bool
lv2_event_is_valid(LV2_Event_Iterator* iter)
{
	return (iter->offset < iter->buf->size);
}


/** Advance @a iter forward one event.
 * @a iter must be valid.
 * @return True if @a iter is valid, otherwise false (reached end of buffer) */
static inline bool
lv2_event_increment(LV2_Event_Iterator* iter)
{
	assert(lv2_event_is_valid(iter));

	LV2_Event* const ev = (LV2_Event*)(
			(uint8_t*)iter->buf->data + iter->offset);

	iter->offset += lv2_event_pad_size(sizeof(LV2_Event) + ev->size);	
	
	return true;
}


/** Dereference an event iterator (get the event currently pointed at).
 * @a iter must be valid.
 * @a data if non-NULL, will be set to point to the contents of the event
 *         returned.
 * @return A Pointer to the event @a iter is currently pointing at, or NULL
 *         if the end of the buffer is reached (in which case @a data is
 *         also set to NULL). */
static inline LV2_Event*
lv2_event_get(LV2_Event_Iterator* iter,
              uint8_t**           data)
{
	assert(lv2_event_is_valid(iter));

	LV2_Event* const ev = (LV2_Event*)(
			(uint8_t*)iter->buf->data + iter->offset);

	if (data)
		*data = (uint8_t*)ev + sizeof(LV2_Event);

	return ev;
}


/** Write an event at @a iter.
 * The event (if any) pointed to by @iter will be overwritten, and @a iter
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
	if (iter->buf->capacity - iter->buf->size < sizeof(LV2_Event) + size)
		return false;

	LV2_Event* const ev = (LV2_Event*)(
			(uint8_t*)iter->buf->data + iter->offset);
	
	ev->frames = frames;
	ev->subframes = subframes;
	ev->type = type;
	ev->size = size;
	memcpy((uint8_t*)ev + sizeof(LV2_Event), data, size);
	++iter->buf->event_count;
	
	size = lv2_event_pad_size(sizeof(LV2_Event) + size);
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
	size = lv2_event_pad_size(size);
	if (iter->buf->capacity - iter->buf->size < sizeof(LV2_Event) + size)
		return NULL;

	LV2_Event* const ev = (LV2_Event*)((uint8_t*)iter->buf->data + 
					   iter->offset);
	
	ev->frames = frames;
	ev->subframes = subframes;
	ev->type = type;
	ev->size = size;
	++iter->buf->event_count;
	
	size = lv2_event_pad_size(sizeof(LV2_Event) + size);
	iter->buf->size += size;
	iter->offset    += size;
	
	return (uint8_t*)ev + sizeof(LV2_Event);
}


/** Write an event at @a iter.
 * The event (if any) pointed to by @iter will be overwritten, and @a iter
 * incremented to point to the following event (i.e. several calls to this
 * function can be done in sequence without twiddling iter in-between).
 * @return True if event was written, otherwise false (buffer is full). */
static inline bool
lv2_event_write_event(LV2_Event_Iterator* iter,
                      const LV2_Event*    ev,
                      const uint8_t*      data)
{
	if (iter->buf->capacity - iter->buf->size < sizeof(LV2_Event) + ev->size)
		return false;

	LV2_Event* const write_ev = (LV2_Event*)(
			(uint8_t*)iter->buf->data + iter->offset);
	
	*write_ev = *ev;
	memcpy((uint8_t*)write_ev + sizeof(LV2_Event), data, ev->size);
	++iter->buf->event_count;
	
	const uint16_t size = lv2_event_pad_size(sizeof(LV2_Event) + ev->size);
	iter->buf->size += size;
	iter->offset    += size;
	
	return true;
}

#endif // LV2_EVENT_HELPERS_H

