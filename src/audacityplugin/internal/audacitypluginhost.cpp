/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginhost.h"

#include <algorithm>
#include <cmath>
#include <string_view>
#include <system_error>

#include "framework/global/settings.h"
#include "framework/global/stringutils.h"
#include "io/file.h"

#include "log.h"

namespace au::audacityplugin {
namespace {
Platform currentPlatform()
{
#if defined(_WIN32) && (defined(_M_ARM64) || defined(__aarch64__))
    return Platform::WindowsArm64;
#elif defined(_WIN32)
    return Platform::WindowsX64;
#elif defined(__APPLE__) && defined(__aarch64__)
    return Platform::MacArm64;
#elif defined(__APPLE__)
    return Platform::MacX64;
#elif defined(__linux__) && defined(__aarch64__)
    return Platform::LinuxArm64;
#elif defined(__linux__)
    return Platform::LinuxX64;
#else
#error Unsupported Plugin API v0 host platform
#endif
}

muse::Settings::Key preferenceKey(const std::string& pluginId)
{
    return { "audacityplugin", "audacityplugin/preferences/v0/" + pluginId };
}

std::vector<Value> preferenceDefaults(const PluginPreferences& preferences)
{
    std::vector<Value> result;
    result.reserve(preferences.items.size());
    for (const auto& item : preferences.items) {
        result.push_back(item.parameter.defaultValue);
    }
    return result;
}

muse::Val toSettingValue(const Value& value)
{
    return std::visit([](const auto& data) { return muse::Val(data); }, value);
}

bool fromSettingValue(const muse::Val& stored, ParameterType type, Value& result)
{
    switch (type) {
    case ParameterType::Boolean:
        if (stored.type() == muse::Val::Type::Bool) {
            result = stored.toBool();
            return true;
        }
        break;
    case ParameterType::Int64:
        if (stored.type() == muse::Val::Type::Int64) {
            result = stored.toInt64();
            return true;
        }
        break;
    case ParameterType::Double:
        if (stored.type() == muse::Val::Type::Double
            && std::isfinite(stored.toDouble())) {
            result = stored.toDouble();
            return true;
        }
        break;
    case ParameterType::String:
    case ParameterType::Enumeration:
    case ParameterType::File:
    case ParameterType::Directory:
        if (stored.type() == muse::Val::Type::String) {
            result = stored.toString();
            return true;
        }
        break;
    }
    return false;
}

muse::ValMap toSettingsMap(const PluginPreferences& preferences,
                           const std::vector<Value>& values)
{
    muse::ValMap result;
    for (size_t i = 0; i < values.size(); ++i) {
        result.emplace(preferences.items[i].parameter.key,
                       toSettingValue(values[i]));
    }
    return result;
}

struct RestoredPreferences {
    std::vector<Value> values;
    bool repaired = false;
};

RestoredPreferences fromSettingsMap(const PluginPreferences& preferences,
                                    const std::vector<Value>& defaults,
                                    const muse::Val& stored)
{
    RestoredPreferences result { defaults, false };
    const auto values = stored.toMap();
    if (values.size() != preferences.items.size()) {
        result.repaired = true;
    }
    for (size_t i = 0; i < preferences.items.size(); ++i) {
        const auto& descriptor = preferences.items[i].parameter;
        const auto found = values.find(descriptor.key);
        Value value;
        if (found == values.end()
            || !fromSettingValue(found->second, descriptor.type, value)) {
            result.repaired = true;
            continue;
        }
        result.values[i] = std::move(value);
    }
    return result;
}
} // namespace

std::shared_ptr<LoadedPlugin> AudacityPluginHost::loadBundle(
    const std::filesystem::path& bundle,
    const std::filesystem::path& dataRoot)
{
    try {
        const auto canonicalBundle = std::filesystem::canonical(bundle);
        muse::ByteArray json;
        const auto manifestPath = canonicalBundle / "plugin.json";
        if (!muse::io::File::readFile(
                muse::io::path_t(manifestPath.u8string()), json)) {
            LOGE() << "Plugin API v0 " << bundle.u8string()
                   << ": plugin.json is missing or unreadable";
            return {};
        }
        auto parsed = readManifest(
            std::string_view { json.constChar(), json.size() }, currentPlatform());
        if (const auto* issue = std::get_if<ManifestError>(&parsed)) {
            LOGE() << "Plugin API v0 " << bundle.u8string() << ": "
                   << issue->field << ": " << issue->message;
            return {};
        }
        auto manifest = std::get<Manifest>(std::move(parsed));
        const auto pluginId = manifest.pluginId;
        const auto dataPath = dataRoot / std::filesystem::u8path(pluginId);
        std::string error;
        auto plugin = LoadedPlugin::load(
            std::move(manifest), canonicalBundle, dataPath, error);
        if (!plugin) {
            LOGE() << "Plugin API v0 " << pluginId << " ("
                   << bundle.u8string() << "): " << error;
        }
        return plugin;
    } catch (const std::exception& exception) {
        LOGE() << "Plugin API v0 " << bundle.u8string() << ": "
               << exception.what();
    } catch (...) {
        LOGE() << "Plugin API v0 " << bundle.u8string()
               << ": unknown discovery failure";
    }
    return {};
}

void AudacityPluginHost::scanScope(const std::filesystem::path& root,
                                   Scope scope,
                                   const std::filesystem::path& dataRoot,
                                   std::map<std::string, Candidates>& candidates)
{
    std::error_code error;
    if (!std::filesystem::is_directory(root, error)) {
        return;
    }
    std::vector<std::filesystem::path> bundles;
    for (std::filesystem::directory_iterator it(root, error), end; !error && it != end; it.increment(error)) {
        std::error_code itemError;
        if (!it->is_directory(itemError)) {
            continue;
        }
        const std::string filename = it->path().filename().u8string();
        if (muse::strings::endsWith(filename, ".audacity4-plugin")) {
            bundles.push_back(it->path());
        }
    }
    if (error) {
        LOGE() << "Plugin API v0 could not scan "
               << (scope == Scope::User ? "user" : "application")
               << " plugin directory: " << error.message();
    }
    std::sort(bundles.begin(), bundles.end());

    for (const auto& bundle : bundles) {
        auto plugin = loadBundle(bundle, dataRoot);
        if (!plugin) {
            continue;
        }
        auto& matches = candidates[plugin->id()];
        auto& destination = scope == Scope::User
                            ? matches.user : matches.application;
        destination.push_back(std::move(plugin));
    }
}

void AudacityPluginHost::initialize()
{
    std::lock_guard<std::mutex> guard(m_mutex);

    if (!m_globalConfiguration()) {
        LOGE() << "Plugin API v0 global configuration is unavailable; "
                  "plugin discovery was skipped";
        return;
    }
    const auto appRoot = std::filesystem::u8path(
        m_globalConfiguration()->appDataPath().appendingComponent("plugins").toStdString());
    const auto userRoot = std::filesystem::u8path(
        m_globalConfiguration()->userAppDataPath().appendingComponent("plugins").toStdString());
    const auto dataRoot = std::filesystem::u8path(
        m_globalConfiguration()->userAppDataPath().appendingComponent("audacityplugin/v0/data").toStdString());
    std::map<std::string, Candidates> candidatesById;
    scanScope(appRoot, Scope::Application, dataRoot, candidatesById);
    scanScope(userRoot, Scope::User, dataRoot, candidatesById);

    for (auto& [id, matches] : candidatesById) {
        if (matches.application.size() > 1) {
            LOGE() << "Plugin API v0 " << id
                   << ": multiple valid application bundles have the same ID; all were rejected";
        }
        if (matches.user.size() > 1) {
            LOGE() << "Plugin API v0 " << id
                   << ": multiple valid user bundles have the same ID; all were rejected";
        }
        if (matches.user.size() == 1) {
            m_plugins.emplace(id, std::move(matches.user.front()));
        } else if (matches.application.size() == 1) {
            m_plugins.emplace(id, std::move(matches.application.front()));
        }
    }

    for (const auto& entry : m_plugins) {
        restorePreferences(entry.second);
        m_effects.insert(m_effects.end(), entry.second->effects().begin(), entry.second->effects().end());
    }
    LOGI() << "Plugin API v0 discovered " << m_plugins.size() << " plugin bundle(s) and "
           << m_effects.size() << " effect(s)";
}

bool AudacityPluginHost::persistPreferences(const PluginPreferences& preferences,
                                            const std::vector<Value>& values) const
{
    try {
        const auto key = preferenceKey(preferences.pluginId);
        muse::settings()->setSharedValue(
            key, muse::Val(toSettingsMap(preferences, values)));
        return true;
    } catch (...) {
        return false;
    }
}

void AudacityPluginHost::restorePreferences(const std::shared_ptr<LoadedPlugin>& plugin)
{
    auto preferences = plugin->preferences();
    if (preferences.items.empty()) {
        return;
    }

    const auto key = preferenceKey(preferences.pluginId);
    const auto preferenceDefaultValues = preferenceDefaults(preferences);
    const auto defaults = toSettingsMap(preferences, preferenceDefaultValues);

    RestoredPreferences restored { preferenceDefaultValues, true };
    try {
        muse::settings()->setDefaultValue(key, muse::Val(defaults));
        restored = fromSettingsMap(
            preferences, preferenceDefaultValues, muse::settings()->value(key));
    } catch (...) {}

    if (plugin->repairPreferenceSchema(restored.values)) {
        restored.repaired = true;
    }

    const auto applied = plugin->applyPreferences(restored.values);
    if (applied != Status::Ok) {
        LOGW() << "Plugin API v0 " << preferences.pluginId
               << ": stored preferences were rejected; using defaults";
        restored.values = preferenceDefaultValues;
        restored.repaired = true;
        if (plugin->applyPreferences(restored.values) != Status::Ok) {
            LOGE() << "Plugin API v0 " << preferences.pluginId
                   << ": declared preference defaults were rejected";
            return;
        }
    }
    if (restored.repaired && !persistPreferences(preferences, restored.values)) {
        LOGE() << "Plugin API v0 " << preferences.pluginId
               << ": repaired preferences could not be persisted";
    }
}

void AudacityPluginHost::shutdown()
{
    std::lock_guard<std::mutex> guard(m_mutex);
    m_effects.clear();
    m_plugins.clear();
}

const std::vector<EffectDescriptor>& AudacityPluginHost::effects() const
{
    return m_effects;
}

std::vector<PluginPreferences> AudacityPluginHost::preferences() const
{
    std::lock_guard<std::mutex> guard(m_mutex);
    std::vector<PluginPreferences> result;
    for (const auto& entry : m_plugins) {
        auto preferences = entry.second->preferences();
        if (preferences.items.empty()) {
            continue;
        }
        result.push_back(std::move(preferences));
    }
    return result;
}

Status AudacityPluginHost::validatePreferences(
    const std::string& pluginId, const std::vector<Value>& values)
{
    std::shared_ptr<LoadedPlugin> plugin;
    {
        std::lock_guard<std::mutex> guard(m_mutex);
        const auto found = m_plugins.find(pluginId);
        if (found == m_plugins.end()) {
            return Status::InvalidArgument;
        }
        plugin = found->second;
    }
    return plugin->validatePreferences(values);
}

Status AudacityPluginHost::applyPreferences(
    const std::string& pluginId, const std::vector<Value>& values)
{
    std::lock_guard<std::mutex> guard(m_mutex);
    const auto found = m_plugins.find(pluginId);
    if (found == m_plugins.end()) {
        return Status::InvalidArgument;
    }
    const auto& plugin = found->second;

    const auto preferences = plugin->preferences();
    std::vector<Value> previous;
    previous.reserve(preferences.items.size());
    for (const auto& item : preferences.items) {
        previous.push_back(item.value);
    }
    auto result = plugin->applyPreferences(values);
    if (result != Status::Ok) {
        return result;
    }
    if (!persistPreferences(preferences, values)) {
        const auto restored = plugin->applyPreferences(previous);
        LOGE() << "Plugin API v0 " << pluginId << ": "
               << (restored == Status::Ok
            ? "preferences could not be persisted; the active snapshot was restored"
            : "preferences could not be persisted or restored");
        return Status::HostError;
    }
    return result;
}

CreateInstanceResult AudacityPluginHost::createInstance(const std::string& pluginId,
                                                        const std::string& effectId,
                                                        const EffectCreateInfo& info)
{
    std::shared_ptr<LoadedPlugin> plugin;
    {
        std::lock_guard<std::mutex> guard(m_mutex);
        auto found = m_plugins.find(pluginId);
        if (found == m_plugins.end()) {
            return { Status::InvalidArgument, {} };
        }
        plugin = found->second;
    }
    return plugin->createInstance(effectId, info);
}
} // namespace au::audacityplugin
