/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>
  Copyright 2014 Robin Gareus <robin@gareus.org>

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

#include <string.h>

#include <gtk/gtk.h>
#include <gdk/gdkquartz.h>

#include "./suil_internal.h"

#include "lv2/options/options.h"
#include "lv2/urid/urid.h"

#if MAC_OS_X_VERSION_MAX_ALLOWED < MAC_OS_X_VERSION_10_12
#define NSEventTypeFlagsChanged     NSFlagsChanged
#define NSEventTypeLeftMouseDown    NSLeftMouseDown
#define NSEventTypeLeftMouseDragged NSLeftMouseDragged
#define NSEventTypeLeftMouseUp      NSLeftMouseUp
#define NSEventTypeMouseEntered     NSMouseEntered
#define NSEventTypeMouseExited      NSMouseExited
#define NSEventTypeMouseMoved       NSMouseMoved
#define NSEventTypeRightMouseDown   NSRightMouseDown
#define NSEventTypeRightMouseUp     NSRightMouseUp
#define NSEventTypeScrollWheel      NSScrollWheel
#endif

extern "C" {

#define SUIL_TYPE_COCOA_WRAPPER (suil_cocoa_wrapper_get_type())
#define SUIL_COCOA_WRAPPER(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), SUIL_TYPE_COCOA_WRAPPER, SuilCocoaWrapper))

typedef struct _SuilCocoaWrapper      SuilCocoaWrapper;
typedef struct _SuilCocoaWrapperClass SuilCocoaWrapperClass;

struct _SuilCocoaWrapper {
	GtkWidget     widget;
	SuilWrapper*  wrapper;
	SuilInstance* instance;

	GdkWindow* flt_win;
	bool       custom_size;
	bool       mapped;
	int        req_width;
	int        req_height;
	int        alo_width;
	int        alo_height;

	const LV2UI_Idle_Interface* idle_iface;
	guint                       idle_id;
	guint                       idle_ms;
};

struct _SuilCocoaWrapperClass {
	GtkWidgetClass parent_class;
};

GType suil_cocoa_wrapper_get_type(void);  // Accessor for SUIL_TYPE_COCOA_WRAPPER

G_DEFINE_TYPE(SuilCocoaWrapper, suil_cocoa_wrapper, GTK_TYPE_WIDGET)

static void
suil_cocoa_wrapper_finalize(GObject* gobject)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(gobject);

	self->wrapper->impl = NULL;

	G_OBJECT_CLASS(suil_cocoa_wrapper_parent_class)->finalize(gobject);
}

static void
suil_cocoa_realize(GtkWidget* widget)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	g_return_if_fail(self != NULL);

	GTK_WIDGET_SET_FLAGS(widget, GTK_REALIZED);

	GdkWindowAttr attrs;
	attrs.x           = widget->allocation.x;
	attrs.y           = widget->allocation.y;
	attrs.width       = widget->allocation.width;
	attrs.height      = widget->allocation.height;
	attrs.wclass      = GDK_INPUT_OUTPUT;
	attrs.window_type = GDK_WINDOW_CHILD;
	attrs.visual      = gtk_widget_get_visual(widget);
	attrs.colormap    = gtk_widget_get_colormap(widget);
	attrs.event_mask  = gtk_widget_get_events(widget) |
		GDK_EXPOSURE_MASK | GDK_BUTTON_PRESS_MASK |
		GDK_BUTTON_RELEASE_MASK | GDK_POINTER_MOTION_MASK |
		GDK_POINTER_MOTION_HINT_MASK;

	widget->window = gdk_window_new(
		widget->parent->window,
		&attrs,
		GDK_WA_X | GDK_WA_Y | GDK_WA_VISUAL | GDK_WA_COLORMAP);

	widget->style = gtk_style_attach(widget->style, widget->window);

	gdk_window_set_user_data(widget->window, widget);
	gtk_style_set_background(widget->style, widget->window, GTK_STATE_ACTIVE);
	gtk_widget_queue_resize(widget);
}

static void
suil_cocoa_size_request(GtkWidget* widget, GtkRequisition* requisition)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	if (self->custom_size) {
		requisition->width  = self->req_width;
		requisition->height = self->req_height;
	} else {
		NSView* view  = (NSView*)self->instance->ui_widget;
		NSRect  frame = [view frame];
		requisition->width  = CGRectGetWidth(NSRectToCGRect(frame));
		requisition->height = CGRectGetHeight(NSRectToCGRect(frame));
	}
}

static void
suil_cocoa_size_allocate(GtkWidget* widget, GtkAllocation* allocation)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	self->alo_width  = allocation->width;
	self->alo_height = allocation->height;

	if (!self->mapped) {
		return;
	}

	gint xx, yy;
	gtk_widget_translate_coordinates(
		gtk_widget_get_parent(widget), widget, 0, 0, &xx, &yy);

	NSView* view = (NSView*)self->instance->ui_widget;
	[view setFrame:NSMakeRect(xx, yy, self->alo_width, self->alo_height)];
}

static void
suil_cocoa_map(GtkWidget* widget)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	self->mapped = true;

	if (self->alo_width == 0 || self->alo_height ==0) {
		return;
	}

	gint xx, yy;
	gtk_widget_translate_coordinates(
		gtk_widget_get_parent(widget), widget, 0, 0, &xx, &yy);

	NSView* view = (NSView*)self->instance->ui_widget;
	[view setHidden:NO];
	[view setFrame:NSMakeRect(xx, yy, self->alo_width, self->alo_height)];
}

static void
suil_cocoa_unmap(GtkWidget* widget)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	NSView*                 view = (NSView*)self->instance->ui_widget;

	self->mapped = false;
	[view setHidden:YES];
}

static gboolean
suil_cocoa_key_press(GtkWidget* widget, GdkEventKey* event)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	if (!self->instance || !self->wrapper || !self->wrapper->impl) {
		return FALSE;
	}
	NSEvent* nsevent = gdk_quartz_event_get_nsevent((GdkEvent*)event);
	NSView*  view    = (NSView*)self->instance->ui_widget;
	[view keyDown:nsevent];
	return TRUE;
}

static gboolean
suil_cocoa_key_release(GtkWidget* widget, GdkEventKey* event)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	if (!self->instance || !self->wrapper || !self->wrapper->impl) {
		return FALSE;
	}
	NSEvent* nsevent = gdk_quartz_event_get_nsevent((GdkEvent*)event);
	NSView*  view    = (NSView*)self->instance->ui_widget;
	[view keyUp:nsevent];
	return TRUE;
}

static gboolean
suil_cocoa_expose(GtkWidget* widget, GdkEventExpose* event)
{
	SuilCocoaWrapper* const self = SUIL_COCOA_WRAPPER(widget);
	NSView*                 view = (NSView*)self->instance->ui_widget;
	[view drawRect:NSMakeRect(event->area.x,
	                          event->area.y,
	                          event->area.width,
	                          event->area.height)];
	return TRUE;
}

static void
suil_cocoa_wrapper_class_init(SuilCocoaWrapperClass* klass)
{
	GObjectClass* const   gobject_class = G_OBJECT_CLASS(klass);
	GtkWidgetClass* const widget_class  = (GtkWidgetClass*)(klass);

	gobject_class->finalize = suil_cocoa_wrapper_finalize;

	widget_class->realize           = suil_cocoa_realize;
	widget_class->expose_event      = suil_cocoa_expose;
	widget_class->size_request      = suil_cocoa_size_request;
	widget_class->size_allocate     = suil_cocoa_size_allocate;
	widget_class->map               = suil_cocoa_map;
	widget_class->unmap             = suil_cocoa_unmap;
	widget_class->key_press_event   = suil_cocoa_key_press;
	widget_class->key_release_event = suil_cocoa_key_release;
}

static void
suil_cocoa_wrapper_init(SuilCocoaWrapper* self)
{
	self->wrapper     = NULL;
	self->instance    = NULL;
	self->flt_win     = NULL;
	self->custom_size = false;
	self->mapped      = false;
	self->req_width   = self->req_height = 0;
	self->alo_width   = self->alo_height = 0;
	self->idle_iface  = NULL;
	self->idle_ms     = 1000 / 30;  // 30 Hz default
}

static int
wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	SuilCocoaWrapper* const wrap = SUIL_COCOA_WRAPPER(handle);
	wrap->req_width   = width;
	wrap->req_height  = height;
	wrap->custom_size = true;
	gtk_widget_queue_resize(GTK_WIDGET(handle));
	return 0;
}

static gboolean
suil_cocoa_wrapper_idle(void* data)
{
	SuilCocoaWrapper* const wrap = SUIL_COCOA_WRAPPER(data);
	wrap->idle_iface->idle(wrap->instance->handle);
	return TRUE;  // Continue calling
}

static GdkFilterReturn
event_filter(GdkXEvent* xevent, GdkEvent* event, gpointer data)
{
	SuilCocoaWrapper* wrap = (SuilCocoaWrapper*)data;
	if (!wrap->instance || !wrap->wrapper || !wrap->wrapper->impl) {
		return GDK_FILTER_CONTINUE;
	}

	NSEvent* nsevent = (NSEvent*)xevent;
	NSView*  view    = (NSView*)wrap->instance->ui_widget;
	if (view && nsevent) {
		switch([nsevent type]) {
		case NSEventTypeFlagsChanged:
			[view flagsChanged:nsevent];
			return GDK_FILTER_REMOVE;
		case NSEventTypeMouseEntered:
			[view mouseEntered:nsevent];
			return GDK_FILTER_REMOVE;
		case NSEventTypeMouseExited:
			[view mouseExited:nsevent];
			return GDK_FILTER_REMOVE;

		/* Explicitly pass though mouse events.  Needed for mouse-drags leaving
		   the window, and mouse-up after that. */
		case NSEventTypeMouseMoved:
			[view mouseMoved:nsevent];
			break;
		case NSEventTypeLeftMouseDragged:
			[view mouseDragged:nsevent];
			break;
		case NSEventTypeLeftMouseDown:
			[view mouseDown:nsevent];
			break;
		case NSEventTypeLeftMouseUp:
			[view mouseUp:nsevent];
			break;
		case NSEventTypeRightMouseDown:
			[view rightMouseDown:nsevent];
			break;
		case NSEventTypeRightMouseUp:
			[view rightMouseUp:nsevent];
			break;
		case NSEventTypeScrollWheel:
			[view scrollWheel:nsevent];
			break;
		default:
			break;
		}
	}
	return GDK_FILTER_CONTINUE;
}

static int
wrapper_wrap(SuilWrapper* wrapper, SuilInstance* instance)
{
	SuilCocoaWrapper* const wrap = SUIL_COCOA_WRAPPER(wrapper->impl);

	instance->host_widget = GTK_WIDGET(wrap);
	wrap->wrapper         = wrapper;
	wrap->instance        = instance;

	const LV2UI_Idle_Interface* idle_iface = NULL;
	if (instance->descriptor->extension_data) {
		idle_iface = (const LV2UI_Idle_Interface*)
			instance->descriptor->extension_data(LV2_UI__idleInterface);
	}
	if (idle_iface) {
		wrap->idle_iface = idle_iface;
		wrap->idle_id    = g_timeout_add(
			wrap->idle_ms, suil_cocoa_wrapper_idle, wrap);
	}

	return 0;
}

static void
wrapper_free(SuilWrapper* wrapper)
{
	if (wrapper->impl) {
		SuilCocoaWrapper* const wrap = SUIL_COCOA_WRAPPER(wrapper->impl);
		if (wrap->idle_id) {
			g_source_remove(wrap->idle_id);
			wrap->idle_id = 0;
		}

		gdk_window_remove_filter(wrap->flt_win, event_filter, wrapper->impl);
		gtk_object_destroy(GTK_OBJECT(wrap));
	}
}


SUIL_LIB_EXPORT
SuilWrapper*
suil_wrapper_new(SuilHost*      host,
                 const char*    host_type_uri,
                 const char*    ui_type_uri,
                 LV2_Feature*** features,
                 unsigned       n_features)
{
	GtkWidget* parent = NULL;
	for (unsigned i = 0; i < n_features; ++i) {
		if (!strcmp((*features)[i]->URI, LV2_UI__parent)) {
			parent = (GtkWidget*)(*features)[i]->data;
		}
	}

	if (!GTK_CONTAINER(parent)) {
		SUIL_ERRORF("No GtkContainer parent given for %s UI\n",
		            ui_type_uri);
		return NULL;
	}

	SuilWrapper* wrapper = (SuilWrapper*)calloc(1, sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	SuilCocoaWrapper* const wrap = SUIL_COCOA_WRAPPER(
		g_object_new(SUIL_TYPE_COCOA_WRAPPER, NULL));

	wrapper->impl             = wrap;
	wrapper->resize.handle    = wrap;
	wrapper->resize.ui_resize = wrapper_resize;

	gtk_container_add(GTK_CONTAINER(parent), GTK_WIDGET(wrap));
	gtk_widget_set_can_focus(GTK_WIDGET(wrap), TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(wrap), TRUE);
	gtk_widget_realize(GTK_WIDGET(wrap));

	GdkWindow* window = gtk_widget_get_window(GTK_WIDGET(wrap));
	wrap->flt_win = gtk_widget_get_window(parent);
	gdk_window_add_filter(wrap->flt_win, event_filter, wrap);

	NSView* parent_view = gdk_quartz_window_get_nsview(window);
	suil_add_feature(features, &n_features, LV2_UI__parent, parent_view);
	suil_add_feature(features, &n_features, LV2_UI__resize, &wrapper->resize);
	suil_add_feature(features, &n_features, LV2_UI__idleInterface, NULL);

	// Scan for URID map and options
	LV2_URID_Map*       map     = NULL;
	LV2_Options_Option* options = NULL;
	for (LV2_Feature** f = *features; *f && (!map || !options); ++f) {
		if (!strcmp((*f)->URI, LV2_OPTIONS__options)) {
			options = (LV2_Options_Option*)(*f)->data;
		} else if (!strcmp((*f)->URI, LV2_URID__map)) {
			map = (LV2_URID_Map*)(*f)->data;
		}
	}

	if (map && options) {
		// Set UI update rate if given
		LV2_URID ui_updateRate = map->map(map->handle, LV2_UI__updateRate);
		for (LV2_Options_Option* o = options; o->key; ++o) {
			if (o->key == ui_updateRate) {
				wrap->idle_ms = 1000.0f / *(const float*)o->value;
				break;
			}
		}
	}

	return wrapper;
}

}  // extern "C"
