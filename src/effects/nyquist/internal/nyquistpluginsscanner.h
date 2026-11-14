/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"
#include "au3-nyquist-effects/LoadNyquist.h"

namespace au::effects {
class NyquistPluginsScanner : public Au3AudioPluginScanner
{
public:
    NyquistPluginsScanner()
        : Au3AudioPluginScanner(m_nyquistModule) {}

private:
    ::PluginPaths pluginPaths(BasicUI::ProgressDialog* progress) const override;

    ::NyquistEffectsModule m_nyquistModule;
};
}
