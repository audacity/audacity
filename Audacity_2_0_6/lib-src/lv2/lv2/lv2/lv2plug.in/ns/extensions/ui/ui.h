/*
  LV2 UI Extension
  Copyright 2009-2012 David Robillard <d@drobilla.net>
  Copyright 2006-2011 Lars Luthman <lars.luthman@gmail.com>

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
   @file ui.h
   C header for the LV2 UI extension <http://lv2plug.in/ns/extensions/ui>.
*/

#ifndef LV2_UI_H
#define LV2_UI_H

#include <stdint.h>

#include "lv2/lv2plug.in/ns/lv2core/lv2.h"

#define LV2_UI_URI    "http://lv2plug.in/ns/extensions/ui"
#define LV2_UI_PREFIX LV2_UI_URI "#"

#define LV2_UI__CocoaUI          LV2_UI_PREFIX "CocoaUI"
#define LV2_UI__Gtk3UI           LV2_UI_PREFIX "Gtk3UI"
#define LV2_UI__GtkUI            LV2_UI_PREFIX "GtkUI"
#define LV2_UI__PortNotification LV2_UI_PREFIX "PortNotification"
#define LV2_UI__Qt4UI            LV2_UI_PREFIX "Qt4UI"
#define LV2_UI__UI               LV2_UI_PREFIX "UI"
#define LV2_UI__WindowsUI        LV2_UI_PREFIX "WindowsUI"
#define LV2_UI__X11UI            LV2_UI_PREFIX "X11UI"
#define LV2_UI__binary           LV2_UI_PREFIX "binary"
#define LV2_UI__fixedSize        LV2_UI_PREFIX "fixedSize"
#define LV2_UI__idleInterface    LV2_UI_PREFIX "idleInterface"
#define LV2_UI__noUserResize     LV2_UI_PREFIX "noUserResize"
#define LV2_UI__notifyType       LV2_UI_PREFIX "notifyType"
#define LV2_UI__parent           LV2_UI_PREFIX "parent"
#define LV2_UI__plugin           LV2_UI_PREFIX "plugin"
#define LV2_UI__portIndex        LV2_UI_PREFIX "portIndex"
#define LV2_UI__portMap          LV2_UI_PREFIX "portMap"
#define LV2_UI__portNotification LV2_UI_PREFIX "portNotification"
#define LV2_UI__portSubscribe    LV2_UI_PREFIX "portSubscribe"
#define LV2_UI__resize           LV2_UI_PREFIX "resize"
#define LV2_UI__touch            LV2_UI_PREFIX "touch"
#define LV2_UI__ui               LV2_UI_PREFIX "ui"
#define LV2_UI__updateRate       LV2_UI_PREFIX "updateRate"

/**
   The index returned by LV2_UI_Port_Port::port_index() for unknown ports.
*/
#define LV2UI_INVALID_PORT_INDEX ((uint32_t)-1)

#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

/**
   A pointer to some widget or other type of UI handle.

   The actual type is defined by the type of the UI.
*/
typedef void* LV2UI_Widget;

/**
   A pointer to an instance of a UI.

   It is valid to compare this to NULL (0 for C++) but otherwise the host MUST
   not attempt to interpret it.  The UI plugin may use it to reference internal
   instance data.
*/
typedef void* LV2UI_Handle;

/**
   A pointer to a controller provided by the host.

   It is valid to compare this to NULL (0 for C++) but otherwise the UI plugin
   MUST NOT attempt to interpret it.  The host may use it to reference internal
   instance data.
*/
typedef void* LV2UI_Controller;

/**
   A pointer to opaque data for a feature.
*/
typedef void* LV2UI_Feature_Handle;

/**
   The type of the host-provided function that the UI can use to
   send data to a plugin's input ports.

   The @p buffer parameter must point to a block of data, @c buffer_size bytes
   large.  The format of this data and how the host should use it is defined by
   the @p port_protocol.  This buffer is owned by the UI and is only valid for
   the duration of this call.

   The @p port_protocol parameter should either be 0 or the URID for a
   ui:PortProtocol.  If it is 0, the protocol is implicitly ui:floatProtocol,
   the port must be an lv2:ControlPort input, @c buffer must point to a single
   float value, and @c buffer_size must be sizeof(float).

   The UI SHOULD NOT use a PortProtocol not supported by the host (i.e. one not
   passed by the host as a feature), but the host MUST gracefully ignore any
   port_protocol it does not understand.
*/
typedef void (*LV2UI_Write_Function)(LV2UI_Controller controller,
                                     uint32_t         port_index,
                                     uint32_t         buffer_size,
                                     uint32_t         port_protocol,
                                     const void*      buffer);

/**
   The implementation of a UI.

   A pointer to an object of this type is returned by the lv2ui_descriptor()
   function.
*/
typedef struct _LV2UI_Descriptor {
	/**
	   The URI for this UI (not for the plugin it controls).
	*/
	const char* URI;

	/**
	   Create a new UI object and return a handle to it.  This function works
	   similarly to the instantiate() member in LV2_Descriptor.

	   @param descriptor The descriptor for the UI that you want to instantiate.

	   @param plugin_uri The URI of the plugin that this UI will control.

	   @param bundle_path The path to the bundle containing the RDF data file
	   that references this shared object file, including the trailing '/'.

	   @param write_function A function provided by the host that the UI can use
	   to send data to the plugin's input ports.

	   @param controller A handle for the plugin instance that should be passed
	   as the first parameter of @p write_function.

	   @param widget A pointer to an LV2UI_Widget.  The UI will write a widget
	   pointer to this location (what type of widget depends on the RDF class of
	   the UI) that will be the main UI widget.

	   @param features An array of LV2_Feature pointers.  The host must pass all
	   feature URIs that it and the UI supports and any additional data, just
	   like in the LV2 plugin instantiate() function.  Note that UI features and
	   plugin features are NOT necessarily the same, they just share the same
	   data structure - this will probably not be the same array as the one the
	   plugin host passes to a plugin.

	*/
	LV2UI_Handle (*instantiate)(const struct _LV2UI_Descriptor* descriptor,
	                            const char*                     plugin_uri,
	                            const char*                     bundle_path,
	                            LV2UI_Write_Function            write_function,
	                            LV2UI_Controller                controller,
	                            LV2UI_Widget*                   widget,
	                            const LV2_Feature* const*       features);


	/**
	   Destroy the UI object and the associated widget. The host must not try
	   to access the widget after calling this function.
	*/
	void (*cleanup)(LV2UI_Handle ui);

	/**
	   Tell the UI that something interesting has happened at a plugin port.

	   What is interesting and how it is written to the buffer passed to this
	   function is defined by the @p format parameter, which has the same
	   meaning as in LV2UI_Write_Function.  The only exception is ports of the
	   class lv2:ControlPort, for which this function should be called when the
	   port value changes (it does not have to be called for every single change
	   if the host's UI thread has problems keeping up with the thread the
	   plugin is running in), @p buffer_size should be 4, the buffer should
	   contain a single IEEE-754 float, and @p format should be 0.

	   By default, the host should only call this function for input ports of
	   the lv2:ControlPort class.  However, this can be modified by using
	   ui:portNotification in the UI data, or the ui:portSubscribe feature.

	   The @p buffer is only valid during the time of this function call, so if
	   the UI wants to keep it for later use it has to copy the contents to an
	   internal buffer.

	   This member may be set to NULL if the UI is not interested in any
	   port events.
	*/
	void (*port_event)(LV2UI_Handle ui,
	                   uint32_t     port_index,
	                   uint32_t     buffer_size,
	                   uint32_t     format,
	                   const void*  buffer);

	/**
	   Return a data structure associated with an extension URI, for example
	   a struct containing additional function pointers.

	   Avoid returning function pointers directly since standard C/C++ has no
	   valid way of casting a void* to a function pointer. This member may be set
	   to NULL if the UI is not interested in supporting any extensions. This is
	   similar to the extension_data() member in LV2_Descriptor.
	*/
	const void* (*extension_data)(const char* uri);
} LV2UI_Descriptor;

/**
   UI Resize Feature (LV2_UI__resize)

   This structure may be used in two ways: as a feature passed by the host via
   LV2UI_Descriptor::instantiate(), or as extension data provided by a UI via
   LV2UI_Descriptor::extension_data()).
*/
typedef struct _LV2UI_Resize {
	/**
	   Pointer to opaque data which must be passed to ui_resize().
	*/
	LV2UI_Feature_Handle handle;

	/**
	   Request or advertise a size change.

	   When this struct is provided by the host, the UI may call this
	   function to inform the host about the size of the UI.

	   When this struct is provided by the UI, the host may call this
	   function to notify the UI that it should change its size accordingly.

	   @return 0 on success.
	*/
	int (*ui_resize)(LV2UI_Feature_Handle handle, int width, int height);
} LV2UI_Resize;

/**
   Port Map Feature (LV2_UI__portMap).

   This feature can be used by the UI to get the index for a port with the
   given symbol.  This makes it possible to implement and distribute a UI
   separately from the plugin (since symbol is a guaranteed stable port
   identifier while index is not).
*/
typedef struct _LV2UI_Port_Map {
	/**
	   Pointer to opaque data which must be passed to ui_resize().
	*/
	LV2UI_Feature_Handle handle;

	/**
	   Get the index for the port with the given @p symbol.

	   @return The index of the port, or LV2_UI_INVALID_PORT_INDEX if no such
	   port is found.
	*/
	uint32_t (*port_index)(LV2UI_Feature_Handle handle, const char* symbol);
} LV2UI_Port_Map;

/**
   Port subscription feature (LV2_UI__portSubscribe);
*/
typedef struct _LV2UI_Port_Subscribe {
	/**
	   Pointer to opaque data which must be passed to ui_resize().
	*/
	LV2UI_Feature_Handle handle;

	/**
	   Subscribe to updates for a port.

	   This means that the host will call the UI's port_event() function when
	   the port value changes (as defined by protocol).

	   Calling this function with the same @p port_index and @p port_protocol
	   as an already active subscription has no effect.

	   @param handle The handle field of this struct.
	   @param port_index The index of the port.
	   @param port_protocol The URID of the ui:PortProtocol.
	   @param features Features for this subscription.
	   @return 0 on success.
	*/
	uint32_t (*subscribe)(LV2UI_Feature_Handle      handle,
	                      uint32_t                  port_index,
	                      uint32_t                  port_protocol,
	                      const LV2_Feature* const* features);

	/**
	   Unsubscribe from updates for a port.

	   This means that the host will cease calling calling port_event() when
	   the port value changes.

	   Calling this function with a @p port_index and @p port_protocol that
	   does not refer to an active port subscription has no effect.

	   @param handle The handle field of this struct.
	   @param port_index The index of the port.
	   @param port_protocol The URID of the ui:PortProtocol.
	   @param features Features for this subscription.
	   @return 0 on success.
	*/
	uint32_t (*unsubscribe)(LV2UI_Feature_Handle      handle,
	                        uint32_t                  port_index,
	                        uint32_t                  port_protocol,
	                        const LV2_Feature* const* features);
} LV2UI_Port_Subscribe;

/**
   A feature to notify the host the user has grabbed a UI control.
*/
typedef struct _LV2UI_Touch {
	/**
	   Pointer to opaque data which must be passed to ui_resize().
	*/
	LV2UI_Feature_Handle handle;

	/**
	   Notify the host that a control has been grabbed or released.

	   @param handle The handle field of this struct.
	   @param port_index The index of the port associated with the control.
	   @param grabbed If true, the control has been grabbed, otherwise the
	   control has been released.
	*/
	void (*touch)(LV2UI_Feature_Handle handle,
	              uint32_t             port_index,
	              bool                 grabbed);
} LV2UI_Touch;

/**
   UI Idle Feature (LV2_UI__idle)

   This feature is an addition to the UI API that provides a callback for the
   host to call rapidly, e.g. to drive the idle callback of a toolkit.
*/
typedef struct _LV2UI_Idle_Interface {
	/**
	   Run a single iteration of the UI's idle loop.

	   This will be called "frequently" in the UI thread at a rate appropriate
	   for a toolkit main loop.  There are no precise timing guarantees.

	   @return 0 on success, or anything else to stop being called.
	*/
	int (*idle)(LV2UI_Handle ui);
} LV2UI_Idle_Interface;

/**
   Peak data for a slice of time, the update format for ui:peakProtocol.
*/
typedef struct _LV2UI_Peak_Data {
	/**
	   The start of the measurement period.  This is just a running counter
	   that is only meaningful in comparison to previous values and must not be
	   interpreted as an absolute time.
	*/
	uint32_t period_start;

	/**
	   The size of the measurement period, in the same units as period_start.
	*/
	uint32_t period_size;

	/**
	   The peak value for the measurement period. This should be the maximal
	   value for abs(sample) over all the samples in the period.
	*/
	float peak;
} LV2UI_Peak_Data;

/**
   A plugin UI programmer must include a function called "lv2ui_descriptor"
   with the following function prototype within the shared object file.  This
   function will have C-style linkage (if you are using C++ this is taken care
   of by the 'extern "C"' clause at the top of the file).  This function is
   loaded from the library by the UI host and called to get a
   LV2UI_Descriptor for the wanted plugin.

   Just like lv2_descriptor(), this function takes an index parameter.  The
   index should only be used for enumeration and not as any sort of ID number -
   the host should just iterate from 0 and upwards until the function returns
   NULL or a descriptor with an URI matching the one the host is looking for.
*/
LV2_SYMBOL_EXPORT
const LV2UI_Descriptor* lv2ui_descriptor(uint32_t index);

/**
   The type of the lv2ui_descriptor() function.
*/
typedef const LV2UI_Descriptor* (*LV2UI_DescriptorFunction)(uint32_t index);

#ifdef __cplusplus
}
#endif

#endif /* LV2_UI_H */
