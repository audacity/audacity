/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-lv2/LV2Utils.h"

namespace au::effects {
struct Lv2UiSelection {
    Lilv_ptr<LilvUIs, lilv_uis_free> uis;
    const LilvUI* ui = nullptr;
    const LilvNode* type = nullptr;

    bool isValid() const { return ui != nullptr; }
};

Lv2UiSelection selectPluginUi(const LilvPlugin& plugin);
}
