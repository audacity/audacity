/*
  Copyright 2011-2013 David Robillard <http://drobilla.net>

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
#include <gdk/gdkx.h>

#include "./suil_internal.h"

#ifdef HAVE_LV2_1_6_0
#    include "lv2/lv2plug.in/ns/ext/options/options.h"
#    include "lv2/lv2plug.in/ns/ext/urid/urid.h"
#endif

#define SUIL_TYPE_X11_WRAPPER (suil_x11_wrapper_get_type())
#define SUIL_X11_WRAPPER(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), SUIL_TYPE_X11_WRAPPER, SuilX11Wrapper))

typedef struct _SuilX11Wrapper      SuilX11Wrapper;
typedef struct _SuilX11WrapperClass SuilX11WrapperClass;

struct _SuilX11Wrapper {
	GtkSocket                   socket;
	GtkPlug*                    plug;
	SuilWrapper*                wrapper;
	SuilInstance*               instance;
#ifdef HAVE_LV2_1_6_0
	const LV2UI_Idle_Interface* idle_iface;
	guint                       idle_id;
	guint                       idle_ms;
#endif
};

struct _SuilX11WrapperClass {
	GtkSocketClass parent_class;
};

GType suil_x11_wrapper_get_type(void);  // Accessor for SUIL_TYPE_X11_WRAPPER

G_DEFINE_TYPE(SuilX11Wrapper, suil_x11_wrapper, GTK_TYPE_SOCKET)

/**
   Check if 'swallowed' subwindow is known to the X server.

   Gdk/GTK can mark the window as realized, mapped and visible even though
   there is no window-ID on the X server for it yet.  Then,
   suil_x11_on_size_allocate() will cause a "BadWinow" X error.
*/
static bool
x_window_is_valid(SuilX11Wrapper* socket)
{
	GdkWindow* window     = gtk_widget_get_window(GTK_WIDGET(socket->plug));
	Window     root       = 0;
	Window     parent     = 0;
	Window*    children   = NULL;
	unsigned   childcount = 0;

	XQueryTree(GDK_WINDOW_XDISPLAY(window),
	           GDK_WINDOW_XID(window),
	           &root, &parent, &children, &childcount);
	for (unsigned i = 0; i < childcount; ++i) {
		if (children[i] == (Window)socket->instance->ui_widget) {
			return true;
		}
	}
	return false;
}

static gboolean
on_plug_removed(GtkSocket* sock, gpointer data)
{
	SuilX11Wrapper* const self = SUIL_X11_WRAPPER(sock);

#ifdef HAVE_LV2_1_6_0
	if (self->idle_id) {
		g_source_remove(self->idle_id);
		self->idle_id = 0;
	}
#endif

	if (self->instance->handle) {
		self->instance->descriptor->cleanup(self->instance->handle);
		self->instance->handle = NULL;
	}

	self->plug = NULL;
	return TRUE;
}

static void
suil_x11_wrapper_finalize(GObject* gobject)
{
	SuilX11Wrapper* const self = SUIL_X11_WRAPPER(gobject);

	self->wrapper->impl = NULL;

	G_OBJECT_CLASS(suil_x11_wrapper_parent_class)->finalize(gobject);
}

static void
suil_x11_wrapper_realize(GtkWidget* w)
{
	SuilX11Wrapper* const wrap   = SUIL_X11_WRAPPER(w);
	GtkSocket* const      socket = GTK_SOCKET(w);

	if (GTK_WIDGET_CLASS(suil_x11_wrapper_parent_class)->realize) {
		GTK_WIDGET_CLASS(suil_x11_wrapper_parent_class)->realize(w);
	}

	gtk_socket_add_id(socket, gtk_plug_get_id(wrap->plug));

	gtk_widget_set_sensitive(GTK_WIDGET(wrap->plug), TRUE);
	gtk_widget_set_can_focus(GTK_WIDGET(wrap->plug), TRUE);
	gtk_widget_grab_focus(GTK_WIDGET(wrap->plug));
}

static void
suil_x11_wrapper_show(GtkWidget* w)
{
	SuilX11Wrapper* const wrap = SUIL_X11_WRAPPER(w);

	if (GTK_WIDGET_CLASS(suil_x11_wrapper_parent_class)->show) {
		GTK_WIDGET_CLASS(suil_x11_wrapper_parent_class)->show(w);
	}

	gtk_widget_show(GTK_WIDGET(wrap->plug));
}

static void
forward_key_event(SuilX11Wrapper* socket,
                  GdkEvent*       gdk_event)
{
	GdkWindow* window = gtk_widget_get_window(GTK_WIDGET(socket->plug));
	GdkScreen* screen = gdk_visual_get_screen(gdk_window_get_visual(window));

	XKeyEvent xev;
	memset(&xev, 0, sizeof(xev));
	xev.type      = (gdk_event->type == GDK_KEY_PRESS) ? KeyPress : KeyRelease;
	xev.root      = GDK_WINDOW_XID(gdk_screen_get_root_window(screen));
	xev.window    = GDK_WINDOW_XID(window);
	xev.subwindow = None;
	xev.time      = gdk_event->key.time;
	xev.state     = gdk_event->key.state;
	xev.keycode   = gdk_event->key.hardware_keycode;

	XSendEvent(GDK_WINDOW_XDISPLAY(window),
	           (Window)socket->instance->ui_widget,
	           False,
	           NoEventMask,
	           (XEvent*)&xev);
}

static void
forward_size_request(SuilX11Wrapper* socket,
                     GtkAllocation*  allocation)
{
	GdkWindow* window = gtk_widget_get_window(GTK_WIDGET(socket->plug));
	if (x_window_is_valid(socket)) {
		XResizeWindow(GDK_WINDOW_XDISPLAY(window),
		              (Window) socket->instance->ui_widget,
		              allocation->width, allocation->height);
	}
}

static gboolean
suil_x11_wrapper_key_event(GtkWidget*   widget,
                           GdkEventKey* event)
{
	SuilX11Wrapper* const self = SUIL_X11_WRAPPER(widget);

	if (self->plug) {
		forward_key_event(self, (GdkEvent*)event);
		return TRUE;
	}

	return FALSE;
}

static void
suil_x11_on_size_allocate(GtkWidget*     widget,
                          GtkAllocation* a)
{
	SuilX11Wrapper* const self = SUIL_X11_WRAPPER(widget);

	if (self->plug
	    && GTK_WIDGET_REALIZED(widget)
	    && GTK_WIDGET_MAPPED(widget)
	    && GTK_WIDGET_VISIBLE(widget)) {
		forward_size_request(self, a);
	}
}

static void
suil_x11_wrapper_class_init(SuilX11WrapperClass* klass)
{
	GObjectClass* const   gobject_class = G_OBJECT_CLASS(klass);
	GtkWidgetClass* const widget_class  = GTK_WIDGET_CLASS(klass);

	gobject_class->finalize       = suil_x11_wrapper_finalize;
	widget_class->realize         = suil_x11_wrapper_realize;
	widget_class->show            = suil_x11_wrapper_show;
	widget_class->key_press_event = suil_x11_wrapper_key_event;
}

static void
suil_x11_wrapper_init(SuilX11Wrapper* self)
{
	self->plug       = GTK_PLUG(gtk_plug_new(0));
	self->wrapper    = NULL;
	self->instance   = NULL;
#ifdef HAVE_LV2_1_6_0
	self->idle_iface = NULL;
	self->idle_ms    = 1000 / 30;  // 30 Hz default
#endif
}

static int
wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	gtk_widget_set_size_request(GTK_WIDGET(handle), width, height);
	return 0;
}

#ifdef HAVE_LV2_1_6_0
static gboolean
suil_x11_wrapper_idle(void* data)
{
	SuilX11Wrapper* const wrap = SUIL_X11_WRAPPER(data);

	wrap->idle_iface->idle(wrap->instance->handle);
	
	return TRUE;  // Continue calling
}
#endif

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilX11Wrapper* const wrap = SUIL_X11_WRAPPER(wrapper->impl);

	instance->host_widget = GTK_WIDGET(wrap);
	wrap->wrapper         = wrapper;
	wrap->instance        = instance;

#ifdef HAVE_LV2_1_6_0
	const LV2UI_Idle_Interface* idle_iface = NULL;
	if (instance->descriptor->extension_data) {
		idle_iface = (const LV2UI_Idle_Interface*)
			instance->descriptor->extension_data(LV2_UI__idleInterface);
	}
	if (idle_iface) {
		wrap->idle_iface = idle_iface;
		wrap->idle_id    = g_timeout_add(
			wrap->idle_ms, suil_x11_wrapper_idle, wrap);
	}
#endif

	g_signal_connect(G_OBJECT(wrap),
	                 "plug-removed",
	                 G_CALLBACK(on_plug_removed),
	                 NULL);

	g_signal_connect(G_OBJECT(wrap),
	                 "size-allocate",
	                 G_CALLBACK(suil_x11_on_size_allocate),
	                 NULL);

	return 0;
}

static void
wrapper_free(SuilWrapper* wrapper)
{
	if (wrapper->impl) {
		SuilX11Wrapper* const wrap = SUIL_X11_WRAPPER(wrapper->impl);
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
	SuilWrapper* wrapper = (SuilWrapper*)malloc(sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	SuilX11Wrapper* const wrap = SUIL_X11_WRAPPER(
		g_object_new(SUIL_TYPE_X11_WRAPPER, NULL));

	wrapper->impl             = wrap;
	wrapper->resize.handle    = wrap;
	wrapper->resize.ui_resize = wrapper_resize;

	gtk_widget_set_sensitive(GTK_WIDGET(wrap), TRUE);
	gtk_widget_set_can_focus(GTK_WIDGET(wrap), TRUE);

	suil_add_feature(features, &n_features, LV2_UI__parent,
	                 (void*)(intptr_t)gtk_plug_get_id(wrap->plug));

	suil_add_feature(features, &n_features, LV2_UI__resize,
	                 &wrapper->resize);

#ifdef HAVE_LV2_1_6_0
	suil_add_feature(features, &n_features, LV2_UI__idleInterface, NULL);

	// Scan for URID map and options
	LV2_URID_Map*       map     = NULL;
	LV2_Options_Option* options = NULL;
	for (LV2_Feature** f = *features; *f && (!map || !options); ++f) {
		if (!strcmp((*f)->URI, LV2_OPTIONS__options)) {
			options = (*f)->data;
		} else if (!strcmp((*f)->URI, LV2_URID__map)) {
			map = (*f)->data;
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
#endif

	return wrapper;
}
