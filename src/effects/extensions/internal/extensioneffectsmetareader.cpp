/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectsmetareader.h"

#include <utility>

#include "effects/effects_base/effectstypes.h"
#include "effects/effects_base/internal/effectsutils.h"

#include "extensioneffectsrepository.h"
#include "extensiontypes.h"

namespace au::effects::extensions {
namespace {
muse::audioplugins::PluginMeta makeMeta(
    const ExtensionEffectEntry& entry, const muse::audioplugins::IKnownAudioPluginsRegister& knownPlugins)
{
    const auto& descriptor = entry.descriptor;
    muse::audioplugins::PluginMeta meta;
    meta.id = entry.id.toStdString();
    meta.vendor = descriptor.vendor;
    meta.type = std::string(AUDIO_RESOURCE_TYPE_NAME);
    meta.attributes.emplace(EFFECT_TITLE_ATTRIBUTE, muse::String::fromStdString(descriptor.title));
    meta.attributes.emplace(EFFECT_DESCRIPTION_ATTRIBUTE, muse::String::fromStdString(descriptor.description));
    meta.attributes.emplace(EFFECT_TYPE_ATTRIBUTE, utils::effectTypeToString(effectTypeFromGroup(descriptor.group)));
    meta.attributes.emplace(EFFECT_CATEGORY_ATTRIBUTE, utils::effectCategoryToString(EffectCategory::Unspecified));
    meta.attributes.emplace(EFFECT_IS_REALTIME_CAPABLE_ATTRIBUTE, u"false");
    meta.attributes.emplace(EFFECT_PARAMS_ARE_INPUT_AGNOSTIC_ATTRIBUTE, u"true");
    meta.attributes.emplace(EFFECT_VERSION_ATTRIBUTE, muse::String::fromStdString(descriptor.version));
    meta.attributes.emplace(EFFECT_MODULE_ATTRIBUTE, muse::String::fromStdString(descriptor.extensionId));
    meta.attributes.emplace(EFFECT_ACTIVATED_ATTRIBUTE, u"true");
    for (const auto& existing : knownPlugins.pluginInfoList()) {
        if (existing.meta.id == meta.id && existing.meta.type == meta.type) {
            const auto activated = existing.meta.attributes.find(EFFECT_ACTIVATED_ATTRIBUTE);
            if (activated != existing.meta.attributes.cend()) {
                meta.attributes[EFFECT_ACTIVATED_ATTRIBUTE] = activated->second;
            }
            break;
        }
    }
    return meta;
}
} // namespace

ExtensionEffectsMetaReader::ExtensionEffectsMetaReader(std::shared_ptr<ExtensionEffectsRepository> repository)
    : m_repository(std::move(repository))
{
}

muse::audioplugins::PluginType ExtensionEffectsMetaReader::metaType() const
{
    return std::string(AUDIO_RESOURCE_TYPE_NAME);
}

bool ExtensionEffectsMetaReader::canReadMeta(const muse::io::path_t& pluginPath) const
{
    return m_repository->contains(pluginPath);
}

muse::RetVal<muse::audioplugins::PluginMetaList> ExtensionEffectsMetaReader::readMeta(const muse::io::path_t& pluginPath) const
{
    muse::audioplugins::PluginMetaList result;
    for (const auto& effect : m_repository->effects()) {
        if (effect.bundlePath == pluginPath) {
            result.push_back(makeMeta(effect, *knownPlugins()));
        }
    }
    if (result.empty()) {
        return muse::make_ret(muse::Ret::Code::InternalError);
    }
    return muse::RetVal<muse::audioplugins::PluginMetaList>::make_ok(std::move(result));
}
} // namespace au::effects::extensions
