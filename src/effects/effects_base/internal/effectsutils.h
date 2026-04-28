/*
 * Audacity: A Digital Audio Editor
 */

#pragma once

// This is required because this file is now used in effects/builtin
#include "effects/effects_base/effectstypes.h"

#include "framework/uicomponents/qml/Muse/UiComponents/menuitem.h"
#include "framework/audio/common/audiotypes.h"

namespace au::effects::utils {
using EffectFilter = std::function<bool (const EffectMeta&)>;

muse::uicomponents::MenuItemList destructiveEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                       const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);

muse::uicomponents::MenuItemList realtimeEffectMenu(EffectMenuOrganization organization, EffectMetaList metaList,
                                                    const EffectFilter& filter, IEffectMenuItemFactory& effectMenu);

muse::String effectTypeToString(EffectType type);
EffectType effectTypeFromString(const muse::String& type);

muse::String effectCategoryToString(EffectCategory category);
EffectCategory effectCategoryFromString(const muse::String& category);

muse::String effectFamilyToString(EffectFamily family);
EffectFamily effectFamilyFromString(const muse::String& family);

muse::audio::AudioResourceMeta auToMuseEffectMeta(const EffectMeta& meta);
EffectMeta museToAuEffectMeta(const muse::io::path_t& path, const muse::audio::AudioResourceMeta& meta, bool enabled = true);

// TODO: may `EffectFamily` be superseded by `AudioResourceType`?
constexpr muse::audio::AudioResourceType toMuseAudioResourceType(EffectFamily family)
{
    switch (family) {
    case EffectFamily::VST3: return muse::audio::AudioResourceType::VstPlugin;
#ifdef Q_OS_LINUX
    case EffectFamily::LV2: return muse::audio::AudioResourceType::Lv2Plugin;
#endif
#ifdef Q_OS_MACOS
    case EffectFamily::AudioUnit: return muse::audio::AudioResourceType::AudioUnit;
#endif
    case EffectFamily::Nyquist: return muse::audio::AudioResourceType::NyquistPlugin;
    case EffectFamily::Builtin: return muse::audio::AudioResourceType::NativeEffect;
    case EffectFamily::Unknown: return muse::audio::AudioResourceType::Undefined;
    default:
        assert(false);
        return muse::audio::AudioResourceType::Undefined;
    }
}

constexpr EffectFamily fromMuseAudioResourceType(muse::audio::AudioResourceType type)
{
    switch (type) {
    case muse::audio::AudioResourceType::VstPlugin: return EffectFamily::VST3;
#ifdef Q_OS_LINUX
    case muse::audio::AudioResourceType::Lv2Plugin: return EffectFamily::LV2;
#endif
#ifdef Q_OS_MACOS
    case muse::audio::AudioResourceType::AudioUnit: return EffectFamily::AudioUnit;
#endif
    case muse::audio::AudioResourceType::NyquistPlugin: return EffectFamily::Nyquist;
    case muse::audio::AudioResourceType::NativeEffect: return EffectFamily::Builtin;
    case muse::audio::AudioResourceType::FluidSoundfont:
    case muse::audio::AudioResourceType::MuseSamplerSoundPack:
    case muse::audio::AudioResourceType::Undefined:
        return EffectFamily::Unknown;
    default:
        assert(false);
        return EffectFamily::Unknown;
    }
}
}
