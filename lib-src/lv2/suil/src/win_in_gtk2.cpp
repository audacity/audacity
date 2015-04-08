/*
  Copyright 2011-2012 David Robillard <http://drobilla.net>

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
#include <gdk/gdkwin32.h>

#include "./suil_internal.h"

extern "C" {

#define SUIL_TYPE_WIN_WRAPPER (suil_win_wrapper_get_type())
#define SUIL_WIN_WRAPPER(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), SUIL_TYPE_WIN_WRAPPER, SuilWinWrapper))

typedef struct _SuilWinWrapper      SuilWinWrapper;
typedef struct _SuilWinWrapperClass SuilWinWrapperClass;

struct _SuilWinWrapper {
	GtkDrawingArea area;
	SuilWrapper*   wrapper;
	SuilInstance*  instance;
};

struct _SuilWinWrapperClass {
	GtkDrawingAreaClass parent_class;
};

GType suil_win_wrapper_get_type(void);  // Accessor for SUIL_TYPE_WIN_WRAPPER

G_DEFINE_TYPE(SuilWinWrapper, suil_win_wrapper, GTK_TYPE_DRAWING_AREA)

static void
suil_win_wrapper_finalize(GObject* gobject)
{
	SuilWinWrapper* const self = SUIL_WIN_WRAPPER(gobject);

	self->wrapper->impl = NULL;

	G_OBJECT_CLASS(suil_win_wrapper_parent_class)->finalize(gobject);
}

static void
suil_win_wrapper_class_init(SuilWinWrapperClass* klass)
{
	GObjectClass* const gobject_class = G_OBJECT_CLASS(klass);

	gobject_class->finalize = suil_win_wrapper_finalize;
}

static void
suil_win_wrapper_init(SuilWinWrapper* self)
{
	self->instance = NULL;
}

static int
wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	gtk_drawing_area_size(GTK_DRAWING_AREA(handle), width, height);
	return 0;
}

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilWinWrapper* const wrap = SUIL_WIN_WRAPPER(wrapper->impl);

	instance->host_widget = GTK_WIDGET(wrap);
	wrap->wrapper         = wrapper;
	wrap->instance        = instance;

	return 0;
}

static void
wrapper_free(SuilWrapper* wrapper)
{
	if (wrapper->impl) {
		SuilWinWrapper* const wrap = SUIL_WIN_WRAPPER(wrapper->impl);
		gtk_object_destroy(GTK_OBJECT(wrap));
	}
}

static GdkFilterReturn
event_filter(GdkXEvent* xevent, GdkEvent* event, gpointer data)
{
	SuilWinWrapper* wrap = (SuilWinWrapper*)data;
	MSG*            msg  = (MSG*)xevent;
	if (msg->message == WM_KEYDOWN || msg->message == WM_KEYUP) {
		// Forward keyboard events to UI window
		PostMessage((HWND)wrap->instance->ui_widget,
		            msg->message, msg->wParam, msg->lParam);
	}
	return GDK_FILTER_CONTINUE;
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

	SuilWrapper* wrapper = (SuilWrapper*)malloc(sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	SuilWinWrapper* const wrap = SUIL_WIN_WRAPPER(
		g_object_new(SUIL_TYPE_WIN_WRAPPER, NULL));

	wrap->wrapper = NULL;

	wrapper->impl             = wrap;
	wrapper->resize.handle    = wrap;
	wrapper->resize.ui_resize = wrapper_resize;

	gtk_container_add(GTK_CONTAINER(parent), GTK_WIDGET(wrap));
	gtk_widget_set_can_focus(GTK_WIDGET(wrap), TRUE);
	gtk_widget_set_sensitive(GTK_WIDGET(wrap), TRUE);
	gtk_widget_realize(GTK_WIDGET(wrap));

	GdkWindow* window = gtk_widget_get_window(GTK_WIDGET(wrap));
	GdkWindow* parent_window = gtk_widget_get_window(parent);
	gdk_window_add_filter(parent_window, event_filter, wrap);
	gdk_window_add_filter(window, event_filter, wrap);

	suil_add_feature(features, &n_features, LV2_UI__parent,
	                 GDK_WINDOW_HWND(window));

	suil_add_feature(features, &n_features, LV2_UI__resize,
	                 &wrapper->resize);

	return wrapper;
}

}  // extern "C"
