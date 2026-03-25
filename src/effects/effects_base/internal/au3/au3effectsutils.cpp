/*
 * Audacity: A Digital Audio Editor
 */

 #include "au3effectsutils.h"
 #include "../effectsutils.h"

 #include "au3wrap/internal/wxtypes_convert.h"

 #include "au3-module-manager/PluginDescriptor.h"

namespace au {
effects::EffectMeta effects::toEffectMeta(const ::PluginDescriptor& desc, EffectFamily effectFamily, const muse::String& title,
                                          const muse::String& description,
                                          bool supportsMultipleClipSelection)
{
    EffectMeta meta;
    meta.id = au3::wxToString(desc.GetID());
    meta.family = effectFamily;
    meta.category = utils::effectCategoryToString(toAu4EffectCategory(desc.GetEffectGroup()));
    meta.title = title;
    meta.description = description;
    meta.isRealtimeCapable = desc.IsEffectRealtime();
    meta.supportsMultipleClipSelection = supportsMultipleClipSelection;
    meta.vendor = au3::wxToString(desc.GetVendor());
    meta.path = au3::wxToString(desc.GetPath());
    meta.type = toAu4EffectType(desc.GetEffectType());
    return meta;
}
}
