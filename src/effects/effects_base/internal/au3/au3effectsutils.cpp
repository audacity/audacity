/*
 * Audacity: A Digital Audio Editor
 */

 #include "au3effectsutils.h"
 #include "../effectsutils.h"

 #include "au3wrap/internal/wxtypes_convert.h"

 #include "au3-module-manager/PluginDescriptor.h"

namespace au {
effects::EffectMeta effects::toEffectMeta(const ::PluginDescriptor& desc)
{
    EffectMeta meta;

    meta.id = au3::wxToString(desc.GetID());
    meta.family = utils::effectFamilyFromString(au3::wxToString(desc.GetEffectFamily()));
    meta.type = toAu4EffectType(desc.GetEffectType());
    const ::TranslatableString& name = desc.GetSymbol().Msgid();
    meta.title = name.msgid();
    if (name.isTranslatable()) {
        meta.titleContext = muse::String::fromUtf8(name.context());
    } else if (meta.family == EffectFamily::Nyquist) {
        meta.titleContext = u"effects-nyquist";
    }
    meta.description = au3::wxToString(desc.GetDescription());
    meta.vendor = au3::wxToString(desc.GetVendor());
    meta.version = au3::wxToString(desc.GetUntranslatedVersion());
    meta.module = au3::wxToString(desc.GetProviderID());
    meta.path = au3::wxToString(desc.GetPath());
    meta.category = utils::effectCategoryToString(toAu4EffectCategory(desc.GetEffectGroup()));
    meta.isRealtimeCapable = desc.IsEffectRealtime();
    meta.paramsAreInputAgnostic = desc.ParamsAreInputAgnostic();
    meta.isActivated = desc.IsEnabled();
    meta.state = desc.IsValid()
                 ? muse::audioplugins::AudioPluginState::Validated
                 : muse::audioplugins::AudioPluginState::Error;

    return meta;
}
}
