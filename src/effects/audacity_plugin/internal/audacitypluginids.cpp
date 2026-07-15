/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginids.h"

#include "au3-strings/wxArrayStringEx.h"
#include "au3wrap/internal/wxtypes_convert.h"

#include "audacityplugin/audacityplugintypes.h"

namespace au::effects::audacity_plugin {
EffectId makeEffectId(const au::audacityplugin::EffectDescriptor& descriptor)
{
    return au3::wxToString(wxJoin(wxArrayStringEx {
            wxString { "Effect" },
            wxString::FromUTF8(EFFECT_FAMILY_ID),
            au3::wxFromStdString(descriptor.pluginId),
            au3::wxFromStdString(descriptor.effectId),
            au3::wxFromStdString(descriptor.pluginId),
        }, '_'));
}
} // namespace au::effects::audacity_plugin
