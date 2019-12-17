/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>

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
   @file suil.h API for Suil, an LV2 UI wrapper library.
*/

#ifndef SUIL_SUIL_H
#define SUIL_SUIL_H

#include <stdint.h>

#include "lv2/core/lv2.h"

#ifdef _WIN32
#    define SUIL_LIB_IMPORT __declspec(dllimport)
#    define SUIL_LIB_EXPORT __declspec(dllexport)
#else
#    define SUIL_LIB_IMPORT __attribute__((visibility("default")))
#    define SUIL_LIB_EXPORT __attribute__((visibility("default")))
#endif

#ifdef SUIL_SHARED
#    ifdef SUIL_INTERNAL
#        define SUIL_API SUIL_LIB_EXPORT
#    else
#        define SUIL_API SUIL_LIB_IMPORT
#    endif
#else
#    define SUIL_API
#endif

#ifdef __cplusplus
extern "C" {
#else
#    include <stdbool.h>
#endif

/**
   @defgroup suil Suil

   Suil is a library for loading and wrapping LV2 plugin UIs.  With Suil, a
   host written in one supported toolkit can embed a plugin UI written in a
   different supported toolkit.  Suil insulates hosts from toolkit libraries
   used by plugin UIs.  For example, a Gtk host can embed a Qt UI without
   linking against Qt at compile time.

   Visit <http://drobilla.net/software/suil> for more information.

   @{
*/

/**
   UI host descriptor.

   This contains the various functions that a plugin UI may use to communicate
   with the plugin.  It is passed to suil_instance_new() to provide these
   functions to the UI.
*/
typedef struct SuilHostImpl SuilHost;

/** An instance of an LV2 plugin UI. */
typedef struct SuilInstanceImpl SuilInstance;

/** Opaque pointer to a UI handle. */
typedef void* SuilHandle;

/** Opaque pointer to a UI widget. */
typedef void* SuilWidget;

/**
   UI controller.

   This is an opaque pointer passed by the user which is passed to the various
   UI control functions (e.g. SuilPortWriteFunc).  It is typically used to pass
   a pointer to some controller object the host uses to communicate with
   plugins.
*/
typedef void* SuilController;

/** Function to write/send a value to a port. */
typedef void (*SuilPortWriteFunc)(
	SuilController controller,
	uint32_t       port_index,
	uint32_t       buffer_size,
	uint32_t       protocol,
	void const*    buffer);

/** Function to return the index for a port by symbol. */
typedef uint32_t (*SuilPortIndexFunc)(
	SuilController controller,
	const char*    port_symbol);

/** Function to subscribe to notifications for a port. */
typedef uint32_t (*SuilPortSubscribeFunc)(
	SuilController            controller,
	uint32_t                  port_index,
	uint32_t                  protocol,
	const LV2_Feature* const* features);

/** Function to unsubscribe from notifications for a port. */
typedef uint32_t (*SuilPortUnsubscribeFunc)(
	SuilController            controller,
	uint32_t                  port_index,
	uint32_t                  protocol,
	const LV2_Feature* const* features);

/** Function called when a control is grabbed or released. */
typedef void (*SuilTouchFunc)(
	SuilController controller,
	uint32_t       port_index,
	bool           grabbed);

/** Initialization argument. */
typedef enum {
	SUIL_ARG_NONE
} SuilArg;

/**
   Initialize suil.

   This function should be called as early as possible, before any other GUI
   toolkit functions.  The variable argument list is a sequence of SuilArg keys
   and corresponding value pairs for passing any necessary platform-specific
   information.  It must be terminated with SUIL_ARG_NONE.
*/
SUIL_API
void
suil_init(int* argc, char*** argv, SuilArg key, ...);

/**
   Create a new UI host descriptor.
   @param write_func Function to send a value to a plugin port.
   @param index_func Function to get the index for a port by symbol.
   @param subscribe_func Function to subscribe to port updates.
   @param unsubscribe_func Function to unsubscribe from port updates.
*/
SUIL_API
SuilHost*
suil_host_new(SuilPortWriteFunc       write_func,
              SuilPortIndexFunc       index_func,
              SuilPortSubscribeFunc   subscribe_func,
              SuilPortUnsubscribeFunc unsubscribe_func);

/**
   Set a touch function for a host descriptor.

   Note this function will only be called if the UI supports it.
*/
SUIL_API
void
suil_host_set_touch_func(SuilHost*     host,
                         SuilTouchFunc touch_func);

/**
   Free `host`.
*/
SUIL_API
void
suil_host_free(SuilHost* host);

/**
   Check if suil can wrap a UI type.
   @param host_type_uri The URI of the desired widget type of the host,
   corresponding to the `type_uri` parameter of suil_instance_new().
   @param ui_type_uri The URI of the UI widget type.
   @return 0 if wrapping is unsupported, otherwise the quality of the wrapping
   where 1 is the highest quality (direct native embedding with no wrapping)
   and increasing values are of a progressively lower quality and/or stability.
*/
SUIL_API
unsigned
suil_ui_supported(const char* host_type_uri,
                  const char* ui_type_uri);

/**
   Instantiate a UI for an LV2 plugin.

   This funcion may load a suil module to adapt the UI to the desired toolkit.
   Suil is configured at compile time to load modules from the appropriate
   place, but this can be changed at run-time via the environment variable
   SUIL_MODULE_DIR.  This makes it possible to bundle suil with an application.

   Note that some situations (Gtk in Qt, Windows in Gtk) require a parent
   container to be passed as a feature with URI LV2_UI__parent
   (http://lv2plug.in/ns/extensions/ui#ui) in order to work correctly.  The
   data must point to a single child container of the host widget set.

   @param host Host descriptor.
   @param controller Opaque host controller pointer.
   @param container_type_uri URI of the desired host container widget type.
   @param plugin_uri URI of the plugin to instantiate this UI for.
   @param ui_uri URI of the specifically desired UI.
   @param ui_type_uri URI of the actual UI widget type.
   @param ui_bundle_path Path of the UI bundle.
   @param ui_binary_path Path of the UI binary.
   @param features NULL-terminated array of supported features, or NULL.
   @return A new UI instance, or NULL if instantiation failed.
*/
SUIL_API
SuilInstance*
suil_instance_new(SuilHost*                 host,
                  SuilController            controller,
                  const char*               container_type_uri,
                  const char*               plugin_uri,
                  const char*               ui_uri,
                  const char*               ui_type_uri,
                  const char*               ui_bundle_path,
                  const char*               ui_binary_path,
                  const LV2_Feature* const* features);

/**
   Free a plugin UI instance.

   The caller must ensure all references to the UI have been dropped before
   calling this function (e.g. it has been removed from its parent).
*/
SUIL_API
void
suil_instance_free(SuilInstance* instance);

/**
   Get the handle for a UI instance.

   Returns the handle to the UI instance.  The returned handle has opaque type
   to insulate the Suil API from LV2 extensions, but in pactice it is currently
   of type `LV2UI_Handle`.  This should not normally be needed.

   The returned handle is shared and must not be deleted.
*/
SUIL_API
SuilHandle
suil_instance_get_handle(SuilInstance* instance);

/**
   Get the widget for a UI instance.

   Returns an opaque pointer to a widget, the type of which matches the
   `container_type_uri` parameter of suil_instance_new().  Note this may be a
   wrapper widget created by Suil, and not necessarily the widget directly
   implemented by the UI.
*/
SUIL_API
SuilWidget
suil_instance_get_widget(SuilInstance* instance);

/**
   Notify the UI about a change in a plugin port.
   @param instance UI instance.
   @param port_index Index of the port which has changed.
   @param buffer_size Size of `buffer` in bytes.
   @param format Format of `buffer` (mapped URI, or 0 for float).
   @param buffer Change data, e.g. the new port value.

   This function can be used to notify the UI about any port change, but in the
   simplest case is used to set the value of lv2:ControlPort ports.  For
   simplicity, this is a special case where `format` is 0, `buffer_size` is 4,
   and `buffer` should point to a single float.

   The `buffer` must be valid only for the duration of this call, the UI must
   not keep a reference to it.
*/
SUIL_API
void
suil_instance_port_event(SuilInstance* instance,
                         uint32_t      port_index,
                         uint32_t      buffer_size,
                         uint32_t      format,
                         const void*   buffer);

/**
   Return a data structure defined by some LV2 extension URI.
*/
SUIL_API
const void*
suil_instance_extension_data(SuilInstance* instance,
                             const char*   uri);

/**
   @}
*/

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* SUIL_SUIL_H */
