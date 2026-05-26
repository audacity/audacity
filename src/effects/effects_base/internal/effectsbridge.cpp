/*
* Audacity: A Digital Audio Editor
*/
#include "effectsbridge.h"

#include "effects/vst/internal/vsttypes.h"
#include "effects/audio_unit/internal/audiounittypes.h"
#include "effects/lv2/internal/lv2types.h"
#include "effects/nyquist/internal/nyquisttypes.h"
#include "effects/builtin/internal/builtintypes.h"

namespace au::effects {
std::string toWireString(EffectFamily family)
{
    switch (family) {
    case EffectFamily::VST3:      return std::string(vst::AUDIO_RESOURCE_TYPE_NAME);
#ifdef Q_OS_MACOS
    case EffectFamily::AudioUnit: return std::string(audio_unit::AUDIO_RESOURCE_TYPE_NAME);
#endif
#ifdef Q_OS_LINUX
    case EffectFamily::LV2:       return std::string(lv2::AUDIO_RESOURCE_TYPE_NAME);
#endif
    case EffectFamily::Nyquist:   return std::string(nyquist::AUDIO_RESOURCE_TYPE_NAME);
    case EffectFamily::Builtin:   return std::string(builtin::AUDIO_RESOURCE_TYPE_NAME);
    case EffectFamily::Unknown:
    case EffectFamily::_count:
        break;
    }
    return {};
}

EffectFamily fromWireString(const std::string& s)
{
    if (s == vst::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::VST3;
    }
#ifdef Q_OS_MACOS
    if (s == audio_unit::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::AudioUnit;
    }
#endif
#ifdef Q_OS_LINUX
    if (s == lv2::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::LV2;
    }
#endif
    if (s == nyquist::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::Nyquist;
    }
    if (s == builtin::AUDIO_RESOURCE_TYPE_NAME) {
        return EffectFamily::Builtin;
    }
    return EffectFamily::Unknown;
}

bool isResourceType(const muse::audioplugins::PluginMeta& meta, EffectFamily family)
{
    return fromWireString(meta.type) == family;
}
}
