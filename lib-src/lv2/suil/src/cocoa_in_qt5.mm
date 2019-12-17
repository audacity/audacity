/*
  Copyright 2017 David Robillard <http://drobilla.net>

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

#import <Cocoa/Cocoa.h>

#include <QCloseEvent>
#include <QMacCocoaViewContainer>
#include <QTimerEvent>
#include <QWidget>

#undef signals

#include "./suil_config.h"
#include "./suil_internal.h"

extern "C" {

typedef struct {
	QWidget* host_widget;
	QWidget* parent;
} SuilCocoaInQt5Wrapper;

class SuilQCocoaWidget : public QMacCocoaViewContainer
{
public:
	SuilQCocoaWidget(NSView* view, QWidget* parent)
		: QMacCocoaViewContainer(view, parent)
		, _instance(NULL)
		, _idle_iface(NULL)
		, _ui_timer(0)
	{
	}

	void start_idle(SuilInstance*               instance,
	                const LV2UI_Idle_Interface* idle_iface) {
		_instance   = instance;
		_idle_iface = idle_iface;
		NSView* view = (NSView*)instance->ui_widget;
		setCocoaView((NSView*)instance->ui_widget);
		setMinimumWidth([view fittingSize].width);
		setMinimumHeight([view fittingSize].height);
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
	SuilCocoaInQt5Wrapper* impl = (SuilCocoaInQt5Wrapper*)wrapper->impl;

	if (impl->host_widget) {
		delete impl->host_widget;
	}

	free(impl);
}

static int
wrapper_wrap(SuilWrapper*  wrapper,
             SuilInstance* instance)
{
	SuilCocoaInQt5Wrapper* const impl = (SuilCocoaInQt5Wrapper*)wrapper->impl;
	SuilQCocoaWidget* const      ew   = (SuilQCocoaWidget*)impl->parent;

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
	SuilQCocoaWidget* const ew = (SuilQCocoaWidget*)handle;
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
	QWidget* parent = NULL;
	for (unsigned i = 0; i < n_features; ++i) {
		if (!strcmp((*features)[i]->URI, LV2_UI__parent)) {
			parent = (QWidget*)(*features)[i]->data;
		}
	}

	if (!parent) {
		SUIL_ERRORF("No QWidget parent given for %s UI\n", ui_type_uri);
		return NULL;
	}

	SuilCocoaInQt5Wrapper* const impl = (SuilCocoaInQt5Wrapper*)
		calloc(1, sizeof(SuilCocoaInQt5Wrapper));

	SuilWrapper* wrapper = (SuilWrapper*)malloc(sizeof(SuilWrapper));
	wrapper->wrap = wrapper_wrap;
	wrapper->free = wrapper_free;
	NSView* view = [NSView new];

	SuilQCocoaWidget* const ew = new SuilQCocoaWidget(view, parent);

	impl->parent = ew;

	wrapper->impl             = impl;
	wrapper->resize.handle    = ew;
	wrapper->resize.ui_resize = wrapper_resize;

	suil_add_feature(features, &n_features, LV2_UI__parent, ew->cocoaView());
	suil_add_feature(features, &n_features, LV2_UI__resize, &wrapper->resize);
	suil_add_feature(features, &n_features, LV2_UI__idleInterface, NULL);

	return wrapper;
}

}  // extern "C"
