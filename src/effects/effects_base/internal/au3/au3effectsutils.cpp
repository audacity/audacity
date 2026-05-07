/*
 * Audacity: A Digital Audio Editor
 */

 #include "au3effectsutils.h"
 #include "../effectsutils.h"

 #include "au3wrap/internal/wxtypes_convert.h"

 #include "au3-module-manager/PluginDescriptor.h"

 #include "framework/global/stringutils.h"

namespace au {
effects::EffectMeta effects::toEffectMeta(const ::PluginDescriptor& desc)
{
    EffectMeta meta;

    meta.id = au3::wxToString(desc.GetID());
    meta.family = utils::effectFamilyFromString(au3::wxToString(desc.GetEffectFamily()));
    meta.type = toAu4EffectType(desc.GetEffectType());
    meta.title = au3::wxToString(desc.GetSymbol().Msgid().MSGID().GET());
    meta.description = au3::wxToString(desc.GetDescription());
    meta.vendor = au3::wxToString(desc.GetVendor());
    meta.version = au3::wxToString(desc.GetUntranslatedVersion());
    meta.module = au3::wxToString(desc.GetProviderID());
    meta.path = au3::wxToString(desc.GetPath());
    meta.category = utils::effectCategoryToString(toAu4EffectCategory(desc.GetEffectGroup()));
    meta.isRealtimeCapable = desc.IsEffectRealtime();
    meta.paramsAreInputAgnostic = desc.ParamsAreInputAgnostic();
    meta.isActivated = desc.IsEnabled();
    meta.isLoadable = desc.IsValid();

    return meta;
}

PluginID effects::effectId(const EffectDefinitionInterface* effect)
{
    return muse::strings::join({
            std::string { "Effect" },
            effect->GetFamily().Internal().ToStdString(),
            effect->GetVendor().Internal().ToStdString(),
            effect->GetSymbol().Internal().ToStdString(),
            effect->GetPath().ToStdString()
        }, "_");
}
}
