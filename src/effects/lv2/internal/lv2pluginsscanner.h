/*
* Audacity: A Digital Audio Editor
*/

#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"

#include "au3-lv2/LoadLV2.h"

namespace au::effects {
class Lv2PluginsScanner : public Au3AudioPluginScanner
{
public:
    Lv2PluginsScanner()
        : Au3AudioPluginScanner(m_lv2Module) {}

private:
    ::LV2EffectsModule m_lv2Module;
};
}
