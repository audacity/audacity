/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginmetareader.h"

#include <algorithm>

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/internal/effectsutils.h"

#include "audacitypluginids.h"

namespace au::effects {
namespace {
EffectType effectType(au::audacityplugin::PresentationGroup group)
{
    using Group = au::audacityplugin::PresentationGroup;
    switch (group) {
    case Group::Generate: return EffectType::Generator;
    case Group::Analyze: return EffectType::Analyzer;
    case Group::Tools: return EffectType::Tool;
    case Group::Effect: return EffectType::Processor;
    }
    return EffectType::Unknown;
}

muse::audioplugins::PluginMeta makeMeta(const au::audacityplugin::EffectDescriptor& descriptor)
{
    muse::audioplugins::PluginMeta meta;
    meta.id = audacity_plugin::makeEffectId(descriptor).toStdString();
    meta.vendor = descriptor.vendor;
    meta.type = std::string(audacity_plugin::AUDIO_RESOURCE_TYPE_NAME);
    meta.attributes.emplace(EFFECT_TITLE_ATTRIBUTE, muse::String::fromStdString(descriptor.name));
    meta.attributes.emplace(EFFECT_DESCRIPTION_ATTRIBUTE, muse::String::fromStdString(descriptor.description));
    meta.attributes.emplace(EFFECT_TYPE_ATTRIBUTE, utils::effectTypeToString(effectType(descriptor.group)));
    meta.attributes.emplace(EFFECT_CATEGORY_ATTRIBUTE, utils::effectCategoryToString(EffectCategory::Unspecified));
    meta.attributes.emplace(EFFECT_IS_REALTIME_CAPABLE_ATTRIBUTE, u"false");
    meta.attributes.emplace(EFFECT_PARAMS_ARE_INPUT_AGNOSTIC_ATTRIBUTE, u"true");
    meta.attributes.emplace(EFFECT_VERSION_ATTRIBUTE, muse::String::fromStdString(descriptor.version));
    meta.attributes.emplace(EFFECT_MODULE_ATTRIBUTE, muse::String::fromStdString(descriptor.pluginName));
    meta.attributes.emplace(EFFECT_ACTIVATED_ATTRIBUTE, u"true");
    return meta;
}
} // namespace

muse::audioplugins::PluginType AudacityPluginMetaReader::metaType() const
{
    return std::string(audacity_plugin::AUDIO_RESOURCE_TYPE_NAME);
}

bool AudacityPluginMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    if (!audacityPluginHost()) {
        return false;
    }
    const std::string path = pluginPath.toStdString();
    const auto& effects = audacityPluginHost()->effects();
    return std::any_of(effects.begin(), effects.end(), [&](const auto& effect) {
        return effect.bundlePath == path;
    });
}

muse::RetVal<muse::audioplugins::PluginMetaList>
AudacityPluginMetaReader::readMeta(const muse::io::path_t& pluginPath) const
{
    if (!audacityPluginHost()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    muse::audioplugins::PluginMetaList result;
    const std::string path = pluginPath.toStdString();
    for (const auto& effect : audacityPluginHost()->effects()) {
        if (effect.bundlePath == path) {
            result.push_back(makeMeta(effect));
        }
    }
    if (result.empty()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    return muse::RetVal<muse::audioplugins::PluginMetaList>::make_ok(std::move(result));
}
} // namespace au::effects
