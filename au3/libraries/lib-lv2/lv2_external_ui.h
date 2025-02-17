/*
  LV2 External UI extension
  This work is in public domain.

  This file is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  If you have questions, contact Filipe Coelho (aka falkTX) <falktx@falktx.com>
  or ask in #lad channel, FreeNode IRC network.
*/

/**
   @file lv2_external_ui.h
   C header for the LV2 External UI extension <http://kxstudio.sf.net/ns/lv2ext/external-ui>.
*/

#ifndef LV2_EXTERNAL_UI_H
#define LV2_EXTERNAL_UI_H

#include "lv2/lv2plug.in/ns/extensions/ui/ui.h"

#define LV2_EXTERNAL_UI_URI     "http://kxstudio.sf.net/ns/lv2ext/external-ui"
#define LV2_EXTERNAL_UI_PREFIX  LV2_EXTERNAL_UI_URI "#"

#define LV2_EXTERNAL_UI__Host   LV2_EXTERNAL_UI_PREFIX "Host"
#define LV2_EXTERNAL_UI__Widget LV2_EXTERNAL_UI_PREFIX "Widget"

/** This extension used to be defined by a lv2plug.in URI */
#define LV2_EXTERNAL_UI_DEPRECATED_URI "http://lv2plug.in/ns/extensions/ui#external"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * When LV2_EXTERNAL_UI__Widget UI is instantiated, the returned
 * LV2UI_Widget handle must be cast to pointer to LV2_External_UI_Widget.
 * UI is created in invisible state.
 */
typedef struct _LV2_External_UI_Widget {
    /**
     * Host calls this function regularly. UI library implementing the
     * callback may do IPC or redraw the UI.
     *
     * @param _this_ the UI context
     */
    void (* run)(struct _LV2_External_UI_Widget* _this_);

    /**
     * Host calls this function to make the plugin UI visible.
     *
     * @param _this_ the UI context
     */
    void (* show)(struct _LV2_External_UI_Widget* _this_);

    /**
     * Host calls this function to make the plugin UI invisible again.
     *
     * @param _this_ the UI context
     */
    void (* hide)(struct _LV2_External_UI_Widget* _this_);
} LV2_External_UI_Widget;

#define LV2_EXTERNAL_UI_RUN(ptr)  (ptr)->run(ptr)
#define LV2_EXTERNAL_UI_SHOW(ptr) (ptr)->show(ptr)
#define LV2_EXTERNAL_UI_HIDE(ptr) (ptr)->hide(ptr)

/**
 * On UI instantiation, host must supply LV2_EXTERNAL_UI__Host feature.
 * LV2_Feature::data must be pointer to LV2_External_UI_Host.
 */
typedef struct _LV2_External_UI_Host {
    /**
     * Callback that plugin UI will call when UI (GUI window) is closed by user.
     * This callback will be called during execution of LV2_External_UI_Widget::run()
     * (i.e. not from background thread).
     *
     * After this callback is called, UI is defunct. Host must call LV2UI_Descriptor::cleanup().
     * If host wants to make the UI visible again, the UI must be reinstantiated.
     *
     * @note When using the deprecated URI LV2_EXTERNAL_UI_DEPRECATED_URI,
     *       some hosts will not call LV2UI_Descriptor::cleanup() as they should,
     *       and may call show() again without re-initialization.
     *
     * @param controller Host context associated with plugin UI, as
     *                   supplied to LV2UI_Descriptor::instantiate().
     */
    void (* ui_closed)(LV2UI_Controller controller);

    /**
     * Optional (may be NULL) "user friendly" identifier which the UI
     * may display to allow a user to easily associate this particular
     * UI instance with the correct plugin instance as it is represented
     * by the host (e.g. "track 1" or "channel 4").
     *
     * If supplied by host, the string will be referenced only during
     * LV2UI_Descriptor::instantiate()
     */
    const char* plugin_human_id;
} LV2_External_UI_Host;

#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* LV2_EXTERNAL_UI_H */
