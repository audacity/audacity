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
#include <QtEvents>
#undef signals

#include "./suil_config.h"
#include "./suil_internal.h"

extern "C" {

typedef struct {
	QX11EmbedContainer* host_widget;
	QX11EmbedWidget*    parent;
} SuilX11InQt4Wrapper;

class SuilQX11Container : public QX11EmbedContainer
{
public:
	SuilQX11Container(SuilInstance*               instance,
	                  const LV2UI_Idle_Interface* idle_iface,
	                  QX11EmbedWidget*            widget)
		: QX11EmbedContainer()
		, _instance(instance)
		, _idle_iface(idle_iface)
		, _widget(widget)
		, _ui_timer(0)
	{}

protected:
	void showEvent(QShowEvent* event) {
		if (_idle_iface && _ui_timer == 0) {
			_ui_timer = this->startTimer(30);
			_widget->embedInto(winId());
			resize(_widget->size());
		}
		QX11EmbedContainer::showEvent(event);
	}

	void timerEvent(QTimerEvent* event) {
		if (event->timerId() == _ui_timer && _idle_iface) {
			_idle_iface->idle(_instance->handle);
		}
		QX11EmbedContainer::timerEvent(event);
	}

	void closeEvent(QCloseEvent* event) {
		if (_ui_timer && _idle_iface) {
			this->killTimer(_ui_timer);
			_ui_timer = 0;
		}
		QX11EmbedContainer::closeEvent(event);
	}

private:
	SuilInstance* const               _instance;
	const LV2UI_Idle_Interface* const _idle_iface;
	QX11EmbedWidget* const            _widget;
	int                               _ui_timer;
};

static void
wrapper_free(SuilWrapper* wrapper)
{
	SuilX11InQt4Wrapper* impl = (SuilX11InQt4Wrapper*)wrapper->impl;

	if (impl->parent) {
		delete impl->parent;
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
	const LV2UI_Idle_Interface* idle_iface = NULL;
	if (instance->descriptor->extension_data) {
		idle_iface = (const LV2UI_Idle_Interface*)
			instance->descriptor->extension_data(LV2_UI__idleInterface);
	}

	SuilX11InQt4Wrapper* const impl = (SuilX11InQt4Wrapper*)wrapper->impl;
	QX11EmbedWidget* const     ew   = impl->parent;

	impl->host_widget = new SuilQX11Container(instance, idle_iface, ew);

	instance->host_widget = impl->host_widget;

	return 0;
}

static int
wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	QX11EmbedWidget* const ew = (QX11EmbedWidget*)handle;
	ew->resize(width, height);
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
	SuilX11InQt4Wrapper* const impl = (SuilX11InQt4Wrapper*)
		calloc(1, sizeof(SuilX11InQt4Wrapper));

	SuilWrapper* wrapper = (SuilWrapper*)calloc(1, sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	QX11EmbedWidget* const ew = new QX11EmbedWidget();

	impl->parent = ew;

	wrapper->impl             = impl;
	wrapper->resize.handle    = ew;
	wrapper->resize.ui_resize = wrapper_resize;

	const intptr_t parent_id = (intptr_t)ew->winId();
	suil_add_feature(features, &n_features, LV2_UI__parent, (void*)parent_id);
	suil_add_feature(features, &n_features, LV2_UI__resize, &wrapper->resize);
	suil_add_feature(features, &n_features, LV2_UI__idleInterface, NULL);

	return wrapper;
}

}  // extern "C"
