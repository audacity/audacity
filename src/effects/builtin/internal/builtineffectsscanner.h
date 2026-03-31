/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "effects/effects_base/internal/au3/au3audiopluginscanner.h"

#include "au3-effects/LoadEffects.h"

namespace au::effects {
class BuiltinEffectsScanner : public Au3AudioPluginScanner
{
public:
    BuiltinEffectsScanner()
        : Au3AudioPluginScanner(m_module) {}

private:
    ::BuiltinEffectsModule m_module;
};
}
