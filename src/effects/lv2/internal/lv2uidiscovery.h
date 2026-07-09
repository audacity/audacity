/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-lv2/LV2Utils.h"

#include <string>

namespace au::effects {
//! Result of searching a plugin's declared UIs for one we can host.
//! Keeps the LilvUIs collection alive because `ui` points into it.
struct Lv2UiCandidate {
    Lilv_ptr<LilvUIs, lilv_uis_free> uis;
    const LilvUI* ui = nullptr;
    const LilvNode* uiType = nullptr;

    //! Set when `ui` is null (or GTK, which we cannot host)
    std::string unsupportedReason;

    bool isHostable() const { return ui != nullptr; }
};

//! Look for a UI we can host: Qt6, X11 (run externally), or an external UI.
//! GTK UIs are reported as unsupported.
Lv2UiCandidate findHostableUi(const LilvPlugin& plugin);
}
