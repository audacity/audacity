/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginids.h"

#include "audacityplugin/audacityplugintypes.h"
#include "effects/effects_base/internal/effectsutils.h"

namespace au::effects::audacity_plugin {
EffectId makeEffectId(const au::audacityplugin::EffectDescriptor& descriptor)
{
    return utils::makeEffectId(
        EFFECT_FAMILY_ID,
        descriptor.pluginId,
        descriptor.effectId,
        descriptor.pluginId);
}
} // namespace au::effects::audacity_plugin
