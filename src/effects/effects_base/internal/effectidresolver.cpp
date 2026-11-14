/*
 * Audacity: A Digital Audio Editor
 */
#include "effectidresolver.h"

#include "effectsutils.h"
#include "au3wrap/internal/wxtypes_convert.h"

namespace au::effects {
PluginID EffectIdResolver::EffectId(const EffectDefinitionInterface* effect) const
{
    return au3::wxFromString(utils::effectId(effect));
}

bool EffectIdResolver::IsEffectId(const PluginID& id) const
{
    return utils::isEffectId(au3::wxToString(id));
}

wxString EffectIdResolver::EffectFamily(const PluginID& id) const
{
    return au3::wxFromStdString(utils::parseEffectFamily(au3::wxToString(id)));
}

wxString EffectIdResolver::EffectVendor(const PluginID& id) const
{
    return au3::wxFromStdString(utils::parseEffectVendor(au3::wxToString(id)));
}

wxString EffectIdResolver::EffectName(const PluginID& id) const
{
    return au3::wxFromStdString(utils::parseEffectName(au3::wxToString(id)));
}

wxString EffectIdResolver::EffectPath(const PluginID& id) const
{
    return au3::wxFromStdString(utils::parseEffectPath(au3::wxToString(id)));
}
}
