/*
 * Audacity: A Digital Audio Editor
 */
#include "lv2uidiscovery.h"

#include "au3-lv2/LV2Symbols.h"

#include "lv2/ui/ui.h"

#include <cstring>

namespace au::effects {
namespace {
bool usesX11(const char* pluginTypeUri)
{
    return !strcmp(pluginTypeUri, LV2_UI__X11UI);
}

unsigned uiIsSupported(const char* hostTypeUri, const char* pluginTypeUri)
{
    if (!strcmp(hostTypeUri, pluginTypeUri)
        ||
        // We give X11 UIs a chance to be run externally.
        usesX11(pluginTypeUri)) {
        return 1;
    }
    return 0;
}
}

Lv2UiCandidate findHostableUi(const LilvPlugin& plugin)
{
    using namespace LV2Symbols;
    // Set the native UI type
    const char* const hostNativeType = LV2_UI__Qt6UI;

    Lv2UiCandidate result;
    result.uis.reset(lilv_plugin_get_uis(&plugin));

    bool isGtk = false;

    // Determine if the plugin has a supported UI
    if (result.uis) {
        if (LilvNodePtr ui_host_uri{ lilv_new_uri(gWorld, hostNativeType) }) {
            LILV_FOREACH(uis, iter, result.uis.get()) {
                result.ui = lilv_uis_get(result.uis.get(), iter);
                if (lilv_ui_is_supported(result.ui, uiIsSupported, ui_host_uri.get(), &result.uiType)) {
                    break;
                }
                if (lilv_ui_is_a(result.ui, node_Gtk) || lilv_ui_is_a(result.ui, node_Gtk3)) {
                    result.uiType = node_Gtk;
                    isGtk = true;
                    break;
                }
                result.ui = nullptr;
            }
        }
    }

    // Check for other supported UIs
    if (!result.ui && result.uis) {
        LILV_FOREACH(uis, iter, result.uis.get()) {
            result.ui = lilv_uis_get(result.uis.get(), iter);
            if (lilv_ui_is_a(result.ui, node_ExternalUI) || lilv_ui_is_a(result.ui, node_ExternalUIOld)) {
                result.uiType = node_ExternalUI;
                break;
            }
            result.ui = nullptr;
        }
    }

    if (result.ui && !isGtk) {
        const char* const uiTypeUri = lilv_node_as_uri(result.uiType);
        isGtk = strcmp(uiTypeUri, LV2_UI__GtkUI) == 0 || strcmp(uiTypeUri, LV2_UI__Gtk3UI) == 0 || strcmp(uiTypeUri, LV2_UI__Gtk4UI) == 0;
    }

    if (result.ui == nullptr) {
        result.unsupportedReason = "No UI provided by the plugin (Please report if AU3 provides a UI for this plugin)";
    } else if (isGtk) {
        result.ui = nullptr;
        result.uiType = nullptr;
        result.unsupportedReason = "GTK UIs not supported, falling back to plain UI";
    }

    return result;
}
}
