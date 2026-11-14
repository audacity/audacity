/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "au3-module-manager/IEffectIdResolver.h"

namespace au::effects {
class EffectIdResolver : public IEffectIdResolver
{
public:
    PluginID EffectId(const EffectDefinitionInterface* effect) const override;

    bool IsEffectId(const PluginID& id) const override;

    wxString EffectFamily(const PluginID& id) const override;
    wxString EffectVendor(const PluginID& id) const override;
    wxString EffectName(const PluginID& id) const override;
    wxString EffectPath(const PluginID& id) const override;
};
}
