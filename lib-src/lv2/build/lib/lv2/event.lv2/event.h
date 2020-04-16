/*
  Copyright 2008-2016 David Robillard <http://drobilla.net>
  Copyright 2006-2007 Lars Luthman <lars.luthman@gmail.com>

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
   @defgroup event Event

   Generic time-stamped events, see <http://lv2plug.in/ns/ext/event> for
   details.

   @{
*/

#ifndef LV2_EVENT_H
#define LV2_EVENT_H

#define LV2_EVENT_URI    "http://lv2plug.in/ns/ext/event"  ///< http://lv2plug.in/ns/ext/event
#define LV2_EVENT_PREFIX LV2_EVENT_URI "#"                 ///< http://lv2plug.in/ns/ext/event#

#define LV2_EVENT__Event              LV2_EVENT_PREFIX "Event"               ///< http://lv2plug.in/ns/ext/event#Event
#define LV2_EVENT__EventPort          LV2_EVENT_PREFIX "EventPort"           ///< http://lv2plug.in/ns/ext/event#EventPort
#define LV2_EVENT__FrameStamp         LV2_EVENT_PREFIX "FrameStamp"          ///< http://lv2plug.in/ns/ext/event#FrameStamp
#define LV2_EVENT__TimeStamp          LV2_EVENT_PREFIX "TimeStamp"           ///< http://lv2plug.in/ns/ext/event#TimeStamp
#define LV2_EVENT__generatesTimeStamp LV2_EVENT_PREFIX "generatesTimeStamp"  ///< http://lv2plug.in/ns/ext/event#generatesTimeStamp
#define LV2_EVENT__generic            LV2_EVENT_PREFIX "generic"             ///< http://lv2plug.in/ns/ext/event#generic
#define LV2_EVENT__inheritsEvent      LV2_EVENT_PREFIX "inheritsEvent"       ///< http://lv2plug.in/ns/ext/event#inheritsEvent
#define LV2_EVENT__inheritsTimeStamp  LV2_EVENT_PREFIX "inheritsTimeStamp"   ///< http://lv2plug.in/ns/ext/event#inheritsTimeStamp
#define LV2_EVENT__supportsEvent      LV2_EVENT_PREFIX "supportsEvent"       ///< http://lv2plug.in/ns/ext/event#supportsEvent
#define LV2_EVENT__supportsTimeStamp  LV2_EVENT_PREFIX "supportsTimeStamp"   ///< http://lv2plug.in/ns/ext/event#supportsTimeStamp

#define LV2_EVENT_AUDIO_STAMP 0  ///< Special timestamp type for audio frames

#include "lv2/core/attributes.h"

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

LV2_DISABLE_DEPRECATION_WARNINGS

/**
   The best Pulses Per Quarter Note for tempo-based uint32_t timestamps.
   Equal to 2^12 * 5 * 7 * 9 * 11 * 13 * 17, which is evenly divisble
   by all integers from 1 through 18 inclusive, and powers of 2 up to 2^12.
*/
LV2_DEPRECATED
static const uint32_t LV2_EVENT_PPQN = 3136573440U;

/**
   An LV2 event (header only).

   LV2 events are generic time-stamped containers for any type of event.
   The type field defines the format of a given event's contents.

   This struct defines the header of an LV2 event. An LV2 event is a single
   chunk of POD (plain old data), usually contained in a flat buffer (see
   LV2_EventBuffer below). Unless a required feature says otherwise, hosts may
   assume a deep copy of an LV2 event can be created safely using a simple:

   memcpy(ev_copy, ev, sizeof(LV2_Event) + ev->size);  (or equivalent)
*/
LV2_DEPRECATED
typedef struct {
	/**
	   The frames portion of timestamp. The units used here can optionally be
	   set for a port (with the lv2ev:timeUnits property), otherwise this is
	   audio frames, corresponding to the sample_count parameter of the LV2 run
	   method (e.g. frame 0 is the first frame for that call to run).
	*/
	uint32_t frames;

	/**
	   The sub-frames portion of timestamp. The units used here can optionally
	   be set for a port (with the lv2ev:timeUnits property), otherwise this is
	   1/(2^32) of an audio frame.
	*/
	uint32_t subframes;

	/**
	   The type of this event, as a number which represents some URI
	   defining an event type. This value MUST be some value previously
	   returned from a call to the uri_to_id function defined in the LV2
	   URI map extension (see lv2_uri_map.h).
	   There are special rules which must be followed depending on the type
	   of an event. If the plugin recognizes an event type, the definition
	   of that event type will describe how to interpret the event, and
	   any required behaviour. Otherwise, if the type is 0, this event is a
	   non-POD event and lv2_event_unref MUST be called if the event is
	   'dropped' (see above). Even if the plugin does not understand an event,
	   it may pass the event through to an output by simply copying (and NOT
	   calling lv2_event_unref). These rules are designed to allow for generic
	   event handling plugins and large non-POD events, but with minimal hassle
	   on simple plugins that "don't care" about these more advanced features.
	*/
	uint16_t type;

	/**
	   The size of the data portion of this event in bytes, which immediately
	   follows. The header size (12 bytes) is not included in this value.
	*/
	uint16_t size;

	/* size bytes of data follow here */
} LV2_Event;


/**
   A buffer of LV2 events (header only).

   Like events (which this contains) an event buffer is a single chunk of POD:
   the entire buffer (including contents) can be copied with a single memcpy.
   The first contained event begins sizeof(LV2_EventBuffer) bytes after the
   start of this struct.

   After this header, the buffer contains an event header (defined by struct
   LV2_Event), followed by that event's contents (padded to 64 bits), followed
   by another header, etc:

   |       |       |       |       |       |       |
   | | | | | | | | | | | | | | | | | | | | | | | | |
   |FRAMES |SUBFRMS|TYP|LEN|DATA..DATA..PAD|FRAMES | ...
*/
LV2_DEPRECATED
typedef struct {
	/**
	   The contents of the event buffer. This may or may not reside in the
	   same block of memory as this header, plugins must not assume either.
	   The host guarantees this points to at least capacity bytes of allocated
	   memory (though only size bytes of that are valid events).
	*/
	uint8_t* data;

	/**
	   The size of this event header in bytes (including everything).

	   This is to allow for extending this header in the future without
	   breaking binary compatibility. Whenever this header is copied,
	   it MUST be done using this field (and NOT the sizeof this struct).
	*/
	uint16_t header_size;

	/**
	   The type of the time stamps for events in this buffer.
	   As a special exception, '0' always means audio frames and subframes
	   (1/UINT32_MAX'th of a frame) in the sample rate passed to instantiate.

	   INPUTS: The host must set this field to the numeric ID of some URI
	   defining the meaning of the frames/subframes fields of contained events
	   (obtained by the LV2 URI Map uri_to_id function with the URI of this
	   extension as the 'map' argument, see lv2_uri_map.h).  The host must
	   never pass a plugin a buffer which uses a stamp type the plugin does not
	   'understand'. The value of this field must never change, except when
	   connect_port is called on the input port, at which time the host MUST
	   have set the stamp_type field to the value that will be used for all
	   subsequent run calls.

	   OUTPUTS: The plugin may set this to any value that has been returned
	   from uri_to_id with the URI of this extension for a 'map' argument.
	   When connected to a buffer with connect_port, output ports MUST set this
	   field to the type of time stamp they will be writing. On any call to
	   connect_port on an event input port, the plugin may change this field on
	   any output port, it is the responsibility of the host to check if any of
	   these values have changed and act accordingly.
	*/
	uint16_t stamp_type;

	/**
	   The number of events in this buffer.

	   INPUTS: The host must set this field to the number of events contained
	   in the data buffer before calling run(). The plugin must not change
	   this field.

	   OUTPUTS: The plugin must set this field to the number of events it has
	   written to the buffer before returning from run(). Any initial value
	   should be ignored by the plugin.
	*/
	uint32_t event_count;

	/**
	   The size of the data buffer in bytes.
	   This is set by the host and must not be changed by the plugin.
	   The host is allowed to change this between run() calls.
	*/
	uint32_t capacity;

	/**
	   The size of the initial portion of the data buffer containing data.

	   INPUTS: The host must set this field to the number of bytes used
	   by all events it has written to the buffer (including headers)
	   before calling the plugin's run().
	   The plugin must not change this field.

	   OUTPUTS: The plugin must set this field to the number of bytes
	   used by all events it has written to the buffer (including headers)
	   before returning from run().
	   Any initial value should be ignored by the plugin.
	*/
	uint32_t size;
} LV2_Event_Buffer;


/**
   Opaque pointer to host data.
*/
LV2_DEPRECATED
typedef void* LV2_Event_Callback_Data;


/**
   Non-POD events feature.

   To support this feature the host must pass an LV2_Feature struct to the
   plugin's instantiate method with URI "http://lv2plug.in/ns/ext/event"
   and data pointed to an instance of this struct.  Note this feature
   is not mandatory to support the event extension.
*/
LV2_DEPRECATED
typedef struct {
	/**
	   Opaque pointer to host data.

	   The plugin MUST pass this to any call to functions in this struct.
	   Otherwise, it must not be interpreted in any way.
	*/
	LV2_Event_Callback_Data callback_data;

	/**
	   Take a reference to a non-POD event.

	   If a plugin receives an event with type 0, it means the event is a
	   pointer to some object in memory and not a flat sequence of bytes
	   in the buffer. When receiving a non-POD event, the plugin already
	   has an implicit reference to the event. If the event is stored AND
	   passed to an output, lv2_event_ref MUST be called on that event.
	   If the event is only stored OR passed through, this is not necessary
	   (as the plugin already has 1 implicit reference).

	   @param event An event received at an input that will not be copied to
	   an output or stored in any way.

	   @param context The calling context. Like event types, this is a mapped
	   URI, see lv2_context.h. Simple plugin with just a run() method should
	   pass 0 here (the ID of the 'standard' LV2 run context). The host
	   guarantees that this function is realtime safe iff the context is
	   realtime safe.

	   PLUGINS THAT VIOLATE THESE RULES MAY CAUSE CRASHES AND MEMORY LEAKS.
	*/
	uint32_t (*lv2_event_ref)(LV2_Event_Callback_Data callback_data,
	                          LV2_Event*              event);

	/**
	   Drop a reference to a non-POD event.

	   If a plugin receives an event with type 0, it means the event is a
	   pointer to some object in memory and not a flat sequence of bytes
	   in the buffer. If the plugin does not pass the event through to
	   an output or store it internally somehow, it MUST call this function
	   on the event (more information on using non-POD events below).

	   @param event An event received at an input that will not be copied to an
	   output or stored in any way.

	   @param context The calling context. Like event types, this is a mapped
	   URI, see lv2_context.h. Simple plugin with just a run() method should
	   pass 0 here (the ID of the 'standard' LV2 run context). The host
	   guarantees that this function is realtime safe iff the context is
	   realtime safe.

	   PLUGINS THAT VIOLATE THESE RULES MAY CAUSE CRASHES AND MEMORY LEAKS.
	*/
	uint32_t (*lv2_event_unref)(LV2_Event_Callback_Data callback_data,
	                            LV2_Event*              event);
} LV2_Event_Feature;

LV2_RESTORE_WARNINGS

#ifdef __cplusplus
}  /* extern "C" */
#endif

#endif /* LV2_EVENT_H */

/**
   @}
*/
