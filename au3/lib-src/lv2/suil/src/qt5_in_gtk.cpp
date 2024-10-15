/*
  Copyright 2011-2017 David Robillard <http://drobilla.net>
  Copyright 2018 Rui Nuno Capela <rncbc@rncbc.org>

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

#include <gtk/gtk.h>

#include <QVBoxLayout>
#include <QWidget>
#include <QWindow>

#if GTK_MAJOR_VERSION == 3
#include <gtk/gtkx.h>
#endif

#include "lv2/options/options.h"
#include "lv2/urid/urid.h"

#include "./suil_internal.h"

extern "C" {

#define SUIL_TYPE_QT_WRAPPER (suil_qt_wrapper_get_type())
#define SUIL_QT_WRAPPER(obj) (G_TYPE_CHECK_INSTANCE_CAST((obj), SUIL_TYPE_QT_WRAPPER, SuilQtWrapper))

typedef struct _SuilQtWrapper      SuilQtWrapper;
typedef struct _SuilQtWrapperClass SuilQtWrapperClass;

struct _SuilQtWrapper {
	GtkSocket                   socket;
	QWidget*                    qembed;
	SuilWrapper*                wrapper;
	SuilInstance*               instance;
	const LV2UI_Idle_Interface* idle_iface;
	guint                       idle_id;
	guint                       idle_ms;
};

struct _SuilQtWrapperClass {
	GtkSocketClass parent_class;
};

GType suil_qt_wrapper_get_type(void);  // Accessor for SUIL_TYPE_QT_WRAPPER

G_DEFINE_TYPE(SuilQtWrapper, suil_qt_wrapper, GTK_TYPE_SOCKET)

static void
suil_qt_wrapper_finalize(GObject* gobject)
{
	SuilQtWrapper* const self = SUIL_QT_WRAPPER(gobject);

	if (self->idle_id) {
		g_source_remove(self->idle_id);
		self->idle_id = 0;
	}

	if (self->instance->handle) {
		self->instance->descriptor->cleanup(self->instance->handle);
		self->instance->handle = NULL;
	}

	if (self->qembed) {
		self->qembed->deleteLater();
	}

	self->qembed        = NULL;
	self->idle_iface    = NULL;
	self->wrapper->impl = NULL;

	G_OBJECT_CLASS(suil_qt_wrapper_parent_class)->finalize(gobject);
}

static void
suil_qt_wrapper_class_init(SuilQtWrapperClass* klass)
{
	GObjectClass* const gobject_class = G_OBJECT_CLASS(klass);

	gobject_class->finalize = suil_qt_wrapper_finalize;
}

static void
suil_qt_wrapper_init(SuilQtWrapper* self)
{
	self->qembed     = NULL;
	self->wrapper    = NULL;
	self->instance   = NULL;
	self->idle_iface = NULL;
	self->idle_id    = 0;
	self->idle_ms    = 1000 / 30;  // 30 Hz default
}

static void
suil_qt_wrapper_realize(GtkWidget* w, gpointer data)
{
	SuilQtWrapper* const wrap = SUIL_QT_WRAPPER(w);
	GtkSocket* const     s    = GTK_SOCKET(w);
	const WId            id   = (WId)gtk_socket_get_id(s);

	wrap->qembed->winId();
	wrap->qembed->windowHandle()->setParent(QWindow::fromWinId(id));
	wrap->qembed->show();
}

static int
suil_qt_wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	gtk_widget_set_size_request(GTK_WIDGET(handle), width, height);

	return 0;
}

static gboolean
suil_qt_wrapper_idle(void* data)
{
	SuilQtWrapper* const wrap = SUIL_QT_WRAPPER(data);

	if (wrap->idle_iface) {
		wrap->idle_iface->idle(wrap->instance->handle);
		return TRUE;  // Continue calling
	}

	return FALSE;
}

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilQtWrapper* const wrap = SUIL_QT_WRAPPER(wrapper->impl);

	wrap->qembed   = new QWidget();
	wrap->wrapper  = wrapper;
	wrap->instance = instance;

	QWidget*     qwidget = (QWidget*)instance->ui_widget;
	QVBoxLayout* layout  = new QVBoxLayout();
	layout->setMargin(0);
	layout->setSpacing(0);
	layout->addWidget(qwidget);

	wrap->qembed->setLayout(layout);

	g_signal_connect_after(G_OBJECT(wrap), "realize",
	                       G_CALLBACK(suil_qt_wrapper_realize), NULL);

	instance->host_widget = GTK_WIDGET(wrap);

	const LV2UI_Idle_Interface* idle_iface = NULL;
	if (instance->descriptor->extension_data) {
		idle_iface = (const LV2UI_Idle_Interface*)
			instance->descriptor->extension_data(LV2_UI__idleInterface);
	}

	if (idle_iface) {
		wrap->idle_iface = idle_iface;
		wrap->idle_id    = g_timeout_add(
			wrap->idle_ms, suil_qt_wrapper_idle, wrap);
	}

	return 0;
}

static void
wrapper_free(SuilWrapper* wrapper)
{
	if (wrapper->impl) {
		SuilQtWrapper* const wrap = SUIL_QT_WRAPPER(wrapper->impl);
		gtk_widget_destroy(GTK_WIDGET(wrap));
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
	SuilWrapper* wrapper = (SuilWrapper*)calloc(1, sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	SuilQtWrapper* const wrap = SUIL_QT_WRAPPER(
		g_object_new(SUIL_TYPE_QT_WRAPPER, NULL));

	wrap->wrapper = NULL;
	wrapper->impl = wrap;

	wrapper->resize.handle    = wrap;
	wrapper->resize.ui_resize = suil_qt_wrapper_resize;

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
