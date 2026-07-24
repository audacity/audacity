/*
 * Audacity: A Digital Audio Editor
 */
#include "loadedplugin.h"

#include <algorithm>
#include <cmath>
#include <limits>
#include <new>
#include <unordered_set>
#include <utility>

#include "log.h"
#include "effectinstance.h"

namespace au::audacityplugin {
namespace {
const void* AUP_CALL queryHost(void*, aup_str) noexcept
{
    return nullptr;
}

const aup_host_v0 hostRoot { nullptr, &queryHost };

bool validSampleRate(uint32_t rate) noexcept
{
    return rate != 0
           && rate <= static_cast<uint32_t>(std::numeric_limits<int>::max());
}

bool discoverEffectCapabilities(
    const aup_effect_v0& effect,
    const aup_effect_offline_v0*& offline,
    const aup_effect_parameters_v0*& params,
    std::vector<ParameterDescriptor>& parameters,
    std::string& error)
{
    if (!effect.query) {
        error = "effect root is incomplete";
        return false;
    }

    const void* offlineCapability = effect.query(
        effect.context, AUP_STR(AUP_CAP_EFFECT_OFFLINE_V0));
    if (!offlineCapability) {
        error = "offline capability is missing";
        return false;
    }
    offline = static_cast<const aup_effect_offline_v0*>(offlineCapability);
    if (!offline->process) {
        error = "offline table is incomplete";
        return false;
    }

    const void* paramsCapability = effect.query(
        effect.context, AUP_STR(AUP_CAP_EFFECT_PARAMETERS_V0));
    if (paramsCapability) {
        params = static_cast<const aup_effect_parameters_v0*>(paramsCapability);
        if (!params->get || !params->set || !params->validate
            || (params->count != 0 && !params->items)) {
            error = "parameters table is incomplete";
            return false;
        }

        std::unordered_set<std::string> keys;
        parameters.reserve(static_cast<size_t>(params->count));
        for (uint64_t i = 0; i < params->count; ++i) {
            ParameterDescriptor descriptor;
            if (!internal::copyValueDescriptor(params->items[i], descriptor, error)) {
                return false;
            }
            if (!keys.insert(descriptor.key).second) {
                error = "parameter key is duplicated";
                return false;
            }
            parameters.push_back(std::move(descriptor));
        }
    }

    return true;
}
} // namespace

LoadedPlugin::LoadedPlugin(Manifest manifest, PluginLibrary library)
    : m_manifest(std::move(manifest)), m_library(std::move(library))
{
}

LoadedPlugin::~LoadedPlugin()
{
    if (!m_root) {
        return;
    }
    try {
        m_root->shutdown(m_root->context);
    } catch (...) {
        LOGE() << "Plugin API v0 shutdown unwound for " << m_manifest.pluginId;
    }
}

std::shared_ptr<LoadedPlugin> LoadedPlugin::load(
    Manifest manifest,
    const std::filesystem::path& bundlePath,
    const std::filesystem::path& dataPath,
    std::string& error)
{
    error.clear();
    try {
        auto library = PluginLibrary::load(bundlePath, manifest.entryPath, error);
        const auto entry = library.entryPoint(error);
        if (!entry) {
            return {};
        }

        const aup_plugin_v0* root = nullptr;
        try {
            root = entry();
        } catch (...) {
            error = "plugin entry unwound";
            return {};
        }
        if (!root || !root->initialize || !root->query || !root->shutdown) {
            error = "plugin root is missing or incomplete";
            return {};
        }

        std::error_code fileError;
        std::filesystem::create_directories(dataPath, fileError);
        if (fileError || !std::filesystem::is_directory(dataPath)) {
            error = "cannot create plugin data directory";
            return {};
        }
        const auto bundleString = bundlePath.u8string();
        const auto dataString = std::filesystem::canonical(dataPath).u8string();
        if (!internal::validUtf8(bundleString.data(), bundleString.size())
            || !internal::validUtf8(dataString.data(), dataString.size())) {
            error = "bundle or data path is not valid UTF-8";
            return {};
        }

        auto plugin = std::shared_ptr<LoadedPlugin>(
            new LoadedPlugin(std::move(manifest), std::move(library)));
        aup_status status;
        try {
            status = root->initialize(root->context, &hostRoot,
                                      internal::stringView(bundleString),
                                      internal::stringView(dataString));
        } catch (...) {
            error = "plugin initialization failed";
            return {};
        }
        if (status != AUP_OK) {
            error = "plugin initialization failed";
            return {};
        }

        plugin->m_root = root;
        plugin->discoverPreferences();
        if (!plugin->discoverEffects(bundlePath, error)) {
            return {};
        }
        return plugin;
    } catch (const std::bad_alloc&) {
        error = "out of memory while loading plugin";
    } catch (const std::exception& exception) {
        error = exception.what();
    } catch (...) {
        error = "unknown plugin loading failure";
    }
    return {};
}

void LoadedPlugin::discoverPreferences()
{
    std::string issue;
    try {
        const void* capability = m_root->query(
            m_root->context, AUP_STR(AUP_CAP_PREFERENCES_V0));
        if (capability) {
            const auto* table = static_cast<const aup_preferences_v0*>(capability);
            if (!table->validate || !table->apply
                || (table->count != 0 && !table->items)) {
                issue = "the returned table is incomplete";
            } else {
                std::unordered_set<std::string> keys;
                std::vector<PluginPreference> items;
                items.reserve(static_cast<size_t>(table->count));
                for (uint64_t i = 0; i < table->count; ++i) {
                    PluginPreference item;
                    if (!internal::copyValueDescriptor(
                            table->items[i], item.parameter, issue)) {
                        break;
                    }
                    item.value = item.parameter.defaultValue;
                    if (!keys.insert(item.parameter.key).second) {
                        issue = "a preference key is duplicated";
                        break;
                    }
                    items.push_back(std::move(item));
                }
                if (issue.empty()) {
                    m_preferences = table;
                    m_preferenceItems = std::move(items);
                }
            }
        }
    } catch (const std::bad_alloc&) {
        throw;
    } catch (...) {
        issue = "capability discovery unwound";
    }

    if (!issue.empty()) {
        LOGW() << "Plugin API v0 ignored optional preferences for "
               << m_manifest.pluginId << ": " << issue;
    }
}

bool LoadedPlugin::discoverEffects(const std::filesystem::path& bundlePath,
                                   std::string& error)
{
    try {
        const void* capability = m_root->query(
            m_root->context, AUP_STR(AUP_CAP_EFFECTS_V0));
        if (!capability) {
            error = "required effects capability is missing";
            return false;
        }

        m_effects = static_cast<const aup_effects_v0*>(capability);
        if (!m_effects->create || !m_effects->destroy
            || (m_effects->count != 0 && !m_effects->items)) {
            error = "effects table is incomplete";
            return false;
        }

        std::unordered_set<std::string> ids;
        m_effectDescriptors.reserve(static_cast<size_t>(m_effects->count));
        for (uint64_t i = 0; i < m_effects->count; ++i) {
            const auto& source = m_effects->items[i];
            const auto inputTrackTypes = source.input_track_types;

            EffectDescriptor descriptor;
            if (!internal::copyString(source.id, descriptor.effectId)
                || descriptor.effectId.empty()
                || !internal::copyString(source.name, descriptor.name)
                || descriptor.name.empty()
                || !internal::copyString(source.description, descriptor.description)
                || !ids.insert(descriptor.effectId).second
                || source.group < AUP_EFFECT_GROUP_GENERATE
                || source.group > AUP_EFFECT_GROUP_TOOLS
                || (inputTrackTypes & ~(InputTrackAudio | InputTrackLabel)) != 0) {
                error = "effect descriptor is invalid or duplicated";
                return false;
            }

            descriptor.pluginId = m_manifest.pluginId;
            descriptor.pluginName = m_manifest.displayName;
            descriptor.vendor = m_manifest.vendor;
            descriptor.version = m_manifest.version;
            descriptor.bundlePath = bundlePath.u8string();
            descriptor.group = static_cast<PresentationGroup>(source.group);
            descriptor.inputTrackTypes = inputTrackTypes;
            m_effectDescriptors.push_back(std::move(descriptor));
        }
        return true;
    } catch (const std::bad_alloc&) {
        throw;
    } catch (...) {
        error = "plugin capability discovery unwound";
        return false;
    }
}

PluginPreferences LoadedPlugin::preferences() const
{
    std::lock_guard<std::mutex> guard(m_mutex);
    PluginPreferences result;
    result.pluginId = m_manifest.pluginId;
    result.pluginName = m_manifest.displayName;
    result.vendor = m_manifest.vendor;
    result.version = m_manifest.version;
    result.items = m_preferenceItems;
    return result;
}

bool LoadedPlugin::repairPreferenceSchema(std::vector<Value>& values) const
{
    bool repaired = false;
    for (size_t i = 0; i < values.size(); ++i) {
        if (!internal::validateValue(m_preferenceItems[i].parameter, values[i])) {
            values[i] = m_preferenceItems[i].parameter.defaultValue;
            repaired = true;
        }
    }
    return repaired;
}

Status LoadedPlugin::callPreferences(const std::vector<Value>& values, bool apply)
{
    std::lock_guard<std::mutex> guard(m_mutex);
    if (!m_preferences) {
        return Status::InvalidState;
    }
    if (values.size() != m_preferenceItems.size()) {
        return Status::InvalidArgument;
    }
    for (size_t i = 0; i < values.size(); ++i) {
        if (!internal::validateValue(m_preferenceItems[i].parameter, values[i])) {
            return Status::ValidationFailed;
        }
    }

    try {
        std::vector<aup_value> converted;
        converted.reserve(values.size());
        for (size_t i = 0; i < values.size(); ++i) {
            converted.push_back(internal::abiValue(
                                    values[i], m_preferenceItems[i].parameter.type));
        }
        const auto count = static_cast<uint64_t>(converted.size());
        const auto* data = converted.empty() ? nullptr : converted.data();
        auto status = internal::statusFromAbi(
            m_preferences->validate(m_preferences->context, count, data));
        if (status != Status::Ok || !apply) {
            return status;
        }

        status = internal::statusFromAbi(
            m_preferences->apply(m_preferences->context, count, data));
        if (status == Status::Ok) {
            for (size_t i = 0; i < values.size(); ++i) {
                m_preferenceItems[i].value = values[i];
            }
        }
        return status;
    } catch (const std::bad_alloc&) {
        return Status::OutOfMemory;
    } catch (...) {
        return Status::PluginError;
    }
}

Status LoadedPlugin::validatePreferences(const std::vector<Value>& values)
{
    return callPreferences(values, false);
}

Status LoadedPlugin::applyPreferences(const std::vector<Value>& values)
{
    return callPreferences(values, true);
}

CreateInstanceResult LoadedPlugin::createInstance(
    const std::string& effectId, const EffectCreateInfo& info)
{
    if (!std::isfinite(info.selectionDurationSeconds)
        || info.selectionDurationSeconds < 0.0
        || !validSampleRate(info.projectSampleRate)) {
        return { Status::InvalidArgument, {} };
    }
    const auto descriptor = std::find_if(
        m_effectDescriptors.begin(), m_effectDescriptors.end(),
        [&](const auto& item) { return item.effectId == effectId; });
    if (descriptor == m_effectDescriptors.end()) {
        return { Status::InvalidArgument, {} };
    }

    std::lock_guard<std::mutex> guard(m_mutex);
    const aup_effect_v0* effect = nullptr;
    const auto destroy = [&] {
        if (!effect) {
            return;
        }
        try {
            m_effects->destroy(m_effects->context, effect);
        } catch (...) {
            LOGE() << "Plugin API v0 effect destruction unwound for "
                   << m_manifest.pluginId;
        }
    };

    try {
        const aup_effect_create_info createInfo {
            info.selectionDurationSeconds,
            info.projectSampleRate,
        };
        const auto status = internal::statusFromAbi(
            m_effects->create(m_effects->context,
                              internal::stringView(descriptor->effectId),
                              &createInfo, &effect));
        if (status != Status::Ok || !effect) {
            destroy();
            return { status == Status::Ok ? Status::PluginError : status, {} };
        }

        const aup_effect_offline_v0* offline = nullptr;
        const aup_effect_parameters_v0* params = nullptr;
        std::vector<ParameterDescriptor> parameters;
        std::string error;
        if (!discoverEffectCapabilities(
                *effect, offline, params, parameters, error)) {
            LOGE() << "Plugin API v0 " << m_manifest.pluginId << ":"
                   << descriptor->effectId << ": " << error;
            destroy();
            return { Status::ValidationFailed, {} };
        }

        auto instance = std::make_shared<EffectInstance>(
            shared_from_this(), effect, offline, params, std::move(parameters));
        effect = nullptr;
        return { Status::Ok, std::move(instance) };
    } catch (const std::bad_alloc&) {
        destroy();
        return { Status::OutOfMemory, {} };
    } catch (...) {
        destroy();
        return { Status::PluginError, {} };
    }
}

void LoadedPlugin::destroyEffect(const aup_effect_v0* effect) noexcept
{
    std::lock_guard<std::mutex> guard(m_mutex);
    try {
        m_effects->destroy(m_effects->context, effect);
    } catch (...) {
        LOGE() << "Plugin API v0 effect destruction unwound for "
               << m_manifest.pluginId;
    }
}
} // namespace au::audacityplugin
