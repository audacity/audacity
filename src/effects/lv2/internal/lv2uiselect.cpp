/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2uiselect.h"

#include <cstring>

#include "au3-lv2/LV2Symbols.h"

#include "lv2/ui/ui.h"

namespace au::effects {
namespace {
unsigned uiIsSupported(const char* hostTypeUri, const char* uiTypeUri)
{
    if (strcmp(hostTypeUri, uiTypeUri) == 0
        ||
        // We give X11 UIs a chance to be run externally.
        strcmp(uiTypeUri, LV2_UI__X11UI) == 0) {
        return 1;
    }
    return 0;
}
}

Lv2UiSelection selectPluginUi(const LilvPlugin& plugin)
{
    using namespace LV2Symbols;

    Lv2UiSelection result;
    result.uis.reset(lilv_plugin_get_uis(&plugin));
    if (!result.uis) {
        return result;
    }

    const LilvNodePtr hostUiType { lilv_new_uri(gWorld, LV2_UI__Qt6UI) };
    if (!hostUiType) {
        return result;
    }

    LILV_FOREACH(uis, iter, result.uis.get()) {
        const LilvUI* ui = lilv_uis_get(result.uis.get(), iter);
        const LilvNode* type = nullptr;
        if (lilv_ui_is_supported(ui, uiIsSupported, hostUiType.get(), &type)) {
            result.ui = ui;
            result.type = type;
            return result;
        }
    }

    LILV_FOREACH(uis, iter, result.uis.get()) {
        const LilvUI* ui = lilv_uis_get(result.uis.get(), iter);
        if (lilv_ui_is_a(ui, node_ExternalUI) || lilv_ui_is_a(ui, node_ExternalUIOld)) {
            result.ui = ui;
            result.type = node_ExternalUI;
            return result;
        }
    }

    return result;
}
}
