/*
  Copyright 2011-2015 David Robillard <http://drobilla.net>
  Copyright 2015 Rui Nuno Capela <rncbc@rncbc.org>

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

#include <QWidget>

#include <QTimerEvent>
#include <QCloseEvent>

#undef signals

#include "./suil_config.h"
#include "./suil_internal.h"

extern "C" {

typedef struct {
	QWidget* host_widget;
	QWidget* parent;
} SuilX11InQt5Wrapper;

class SuilQX11Widget : public QWidget
{
public:
	SuilQX11Widget(QWidget* parent, Qt::WindowFlags wflags)
		: QWidget(parent, wflags)
		, _instance(NULL)
		, _idle_iface(NULL)
		, _ui_timer(0)
	{}

	void start_idle(SuilInstance*               instance,
	                const LV2UI_Idle_Interface* idle_iface) {
		_instance   = instance;
		_idle_iface = idle_iface;
		if (_idle_iface && _ui_timer == 0) {
			_ui_timer = this->startTimer(30);
		}
	}

protected:
	void timerEvent(QTimerEvent* event) {
		if (event->timerId() == _ui_timer && _idle_iface) {
			_idle_iface->idle(_instance->handle);
		}
		QWidget::timerEvent(event);
	}

	void closeEvent(QCloseEvent* event) {
		if (_ui_timer && _idle_iface) {
			this->killTimer(_ui_timer);
			_ui_timer = 0;
		}
		QWidget::closeEvent(event);
	}

private:
	SuilInstance*               _instance;
	const LV2UI_Idle_Interface* _idle_iface;
	int                         _ui_timer;
};

static void
wrapper_free(SuilWrapper* wrapper)
{
	SuilX11InQt5Wrapper* impl = (SuilX11InQt5Wrapper*)wrapper->impl;

	if (impl->host_widget) {
		delete impl->host_widget;
	}

	free(impl);
}

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilX11InQt5Wrapper* const impl = (SuilX11InQt5Wrapper*)wrapper->impl;
	SuilQX11Widget* const      ew   = (SuilQX11Widget*)impl->parent;

	if (instance->descriptor->extension_data) {
		const LV2UI_Idle_Interface* idle_iface
			= (const LV2UI_Idle_Interface*)
				instance->descriptor->extension_data(LV2_UI__idleInterface);
		ew->start_idle(instance, idle_iface);
	}

	impl->host_widget = ew;

	instance->host_widget = impl->host_widget;

	return 0;
}

static int
wrapper_resize(LV2UI_Feature_Handle handle, int width, int height)
{
	QWidget* const ew = (QWidget*)handle;
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
	SuilX11InQt5Wrapper* const impl = (SuilX11InQt5Wrapper*)
		calloc(1, sizeof(SuilX11InQt5Wrapper));

	SuilWrapper* wrapper = (SuilWrapper*)malloc(sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;

	QWidget* const ew = new SuilQX11Widget(NULL, Qt::Window);

	impl->parent = ew;

	wrapper->impl             = impl;
	wrapper->resize.handle    = ew;
	wrapper->resize.ui_resize = wrapper_resize;

	void* parent_id = (void*)(intptr_t)ew->winId();
	suil_add_feature(features, &n_features, LV2_UI__parent, parent_id);
	suil_add_feature(features, &n_features, LV2_UI__resize, &wrapper->resize);
	suil_add_feature(features, &n_features, LV2_UI__idleInterface, NULL);

	return wrapper;
}

}  // extern "C"
