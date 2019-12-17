/*
  Copyright 2011-2015 David Robillard <http://drobilla.net>

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

#include <QX11EmbedContainer>
#undef signals

#include <gtk/gtk.h>
#include <gdk/gdkx.h>

#include "./suil_config.h"
#include "./suil_internal.h"

extern "C" {

typedef struct _SuilGtk2InQt4Wrapper SuilGtk2InQt4Wrapper;

struct _SuilGtk2InQt4Wrapper {
	QX11EmbedContainer* host_widget;
	QWidget*            parent;
	GtkWidget*          plug;
};

static void
on_size_request(GtkWidget*      widget,
                GtkRequisition* requisition,
                gpointer        user_data)
{
	QX11EmbedContainer* const wrap = (QX11EmbedContainer*)user_data;
	wrap->setMinimumSize(requisition->width, requisition->height);
}

static void
on_size_allocate(GtkWidget*    widget,
                 GdkRectangle* allocation,
                 gpointer      user_data)
{
	QX11EmbedContainer* const wrap = (QX11EmbedContainer*)user_data;
	wrap->resize(allocation->width, allocation->height);
}

static void
wrapper_free(SuilWrapper* wrapper)
{
	SuilGtk2InQt4Wrapper* impl = (SuilGtk2InQt4Wrapper*)wrapper->impl;

	if (impl->plug) {
		gtk_widget_destroy(impl->plug);
	}

	if (impl->host_widget) {
		delete impl->host_widget;
	}

	free(impl);
}

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilGtk2InQt4Wrapper* const impl   = (SuilGtk2InQt4Wrapper*)wrapper->impl;
	QWidget*                    root   = static_cast<QWidget*>(impl->parent);
	QX11EmbedContainer* const   wrap   = new QX11EmbedContainer(root);
	GtkWidget* const            plug   = gtk_plug_new(wrap->winId());
	GtkWidget* const            widget = (GtkWidget*)instance->ui_widget;

	gtk_container_add(GTK_CONTAINER(plug), widget);
	gtk_widget_show_all(plug);

#ifdef SUIL_OLD_GTK
	wrap->resize(widget->allocation.width, widget->allocation.height);
#else
	GtkAllocation alloc;
	gtk_widget_get_allocation(widget, &alloc);
	wrap->resize(alloc.width, alloc.height);
#endif

	g_signal_connect(
		G_OBJECT(plug), "size-request", G_CALLBACK(on_size_request), wrap);

	g_signal_connect(
		G_OBJECT(plug), "size-allocate", G_CALLBACK(on_size_allocate), wrap);

	impl->host_widget     = wrap;
	impl->plug            = plug;
	instance->host_widget = wrap;

	return 0;
}

SUIL_LIB_EXPORT
SuilWrapper*
suil_wrapper_new(SuilHost*      host,
                 const char*    host_type_uri,
                 const char*    ui_type_uri,
                 LV2_Feature*** features,
                 unsigned       n_features)
{
	/* We have to open libgtk here, so Gtk type symbols are present and will be
	   found by the introspection stuff.  This is required at least to make
	   GtkBuilder use in UIs work, otherwise they will cause "Invalid object
	   type" errors.
	*/
	if (!host->gtk_lib) {
		dlerror();
		host->gtk_lib = dlopen(SUIL_GTK2_LIB_NAME, RTLD_LAZY|RTLD_GLOBAL);
		if (!host->gtk_lib) {
			SUIL_ERRORF("Failed to open %s (%s)\n",
			            SUIL_GTK2_LIB_NAME, dlerror());
			return NULL;
		}
		gtk_init(NULL, NULL);
	}

	/* Create wrapper implementation. */
	SuilGtk2InQt4Wrapper* const impl = (SuilGtk2InQt4Wrapper*)
		calloc(1, sizeof(SuilGtk2InQt4Wrapper));

	/* Set parent widget if given. */
	for (unsigned i = 0; i < n_features; ++i) {
		if (!strcmp((*features)[i]->URI, LV2_UI__parent)) {
			impl->parent = static_cast<QWidget*>((*features)[i]->data);
		}
	}

	SuilWrapper* wrapper = (SuilWrapper*)calloc(1, sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;
	wrapper->impl = impl;

	return wrapper;
}

}  // extern "C"
