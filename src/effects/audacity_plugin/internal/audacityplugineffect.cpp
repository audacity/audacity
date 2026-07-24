/*
 * Audacity: A Digital Audio Editor
 */
#include "audacityplugineffect.h"

#include <charconv>
#include <cmath>
#include <string_view>
#include <utility>

#include <wx/string.h>

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-components/EffectAutomationParameters.h"
#include "au3-strings/Identifier.h"
#include "au3-strings/Internat.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/log.h"
#include "framework/global/serialization/json.h"

#include "audacitypluginids.h"

namespace au::effects {
namespace {
constexpr const wxChar* STATE_KEY = wxT("State");

std::string serialize(const AudacityPluginSettings& settings)
{
    muse::JsonObject root;
    for (const auto& [key, value] : settings.values) {
        muse::JsonObject item;
        if (const auto* data = std::get_if<bool>(&value)) {
            item["type"] = "bool";
            item["value"] = *data;
        } else if (const auto* data = std::get_if<int64_t>(&value)) {
            item["type"] = "int64";
            item["value"] = std::to_string(*data);
        } else if (const auto* data = std::get_if<double>(&value)) {
            item["type"] = "double";
            item["value"] = *data;
        } else {
            item["type"] = "string";
            item["value"] = std::get<std::string>(value);
        }
        root[key] = item;
    }
    const auto json = muse::JsonDocument(root).toJson(
        muse::JsonDocument::Format::Compact);
    return { json.constChar(), json.size() };
}

AudacityPluginSettings deserialize(std::string_view state)
{
    AudacityPluginSettings result;
    std::string error;
    const char* data = state.empty() ? "" : state.data();
    const auto document = muse::JsonDocument::fromJson(
        muse::ByteArray::fromRawData(data, state.size()), &error);
    if (!error.empty() || !document.isObject()) {
        return result;
    }

    const auto root = document.rootObject();
    for (const auto& key : root.keys()) {
        const auto itemValue = root.value(key);
        if (!itemValue.isObject()) {
            continue;
        }
        const auto item = itemValue.toObject();
        const auto type = item.value("type");
        const auto stored = item.value("value");
        if (!type.isString()) {
            continue;
        }

        au::audacityplugin::Value value;
        if (type.toStdString() == "bool" && stored.isBool()) {
            value = stored.toBool();
        } else if (type.toStdString() == "int64" && stored.isString()) {
            const auto& text = stored.toStdString();
            int64_t parsed = 0;
            const auto converted = std::from_chars(
                text.data(), text.data() + text.size(), parsed);
            if (converted.ec != std::errc()
                || converted.ptr != text.data() + text.size()) {
                continue;
            }
            value = parsed;
        } else if (type.toStdString() == "double" && stored.isNumber()
                   && std::isfinite(stored.toDouble())) {
            value = stored.toDouble();
        } else if (type.toStdString() == "string" && stored.isString()) {
            value = stored.toStdString();
        } else {
            continue;
        }
        result.values.emplace(key, std::move(value));
    }
    return result;
}

::EffectType effectType(au::audacityplugin::PresentationGroup group)
{
    using Group = au::audacityplugin::PresentationGroup;
    switch (group) {
    case Group::Generate: return EffectTypeGenerate;
    case Group::Analyze: return EffectTypeAnalyze;
    case Group::Tools: return EffectTypeTool;
    case Group::Effect: return EffectTypeProcess;
    }
    return EffectTypeNone;
}
} // namespace

AudacityPluginEffectInstance::AudacityPluginEffectInstance(
    AudacityPluginEffect& effect,
    std::shared_ptr<au::audacityplugin::IEffectInstance> pluginInstance)
    : StatefulEffect::Instance(effect), m_pluginInstance(std::move(pluginInstance))
{
}

au::audacityplugin::IEffectInstance&
AudacityPluginEffectInstance::pluginInstance() const noexcept
{
    return *m_pluginInstance;
}

bool AudacityPluginEffectInstance::applySettings(const EffectSettings& settings)
{
    if (isGenerator()) {
        m_generatorDurationSeconds = settings.extra.GetDuration();
    }
    const auto& stored = AudacityPluginEffect::GetSettings(settings).values;
    const auto& descriptors = m_pluginInstance->parameters();
    for (uint64_t index = 0; index < descriptors.size(); ++index) {
        const auto& descriptor = descriptors[static_cast<size_t>(index)];
        auto found = stored.find(descriptor.key);
        const auto& requested = found != stored.end()
                                ? found->second : descriptor.defaultValue;
        auto status = m_pluginInstance->setValue(index, requested);
        if (status != au::audacityplugin::Status::Ok && found != stored.end()) {
            LOGW() << "Plugin API v0 ignored invalid stored value for " << descriptor.key;
            status = m_pluginInstance->setValue(index, descriptor.defaultValue);
        }
        if (status != au::audacityplugin::Status::Ok) {
            m_lastError = "Plugin rejected the declared default for parameter " + descriptor.key;
            return false;
        }
    }
    m_lastError.clear();
    return true;
}

bool AudacityPluginEffectInstance::isGenerator() const noexcept
{
    return static_cast<AudacityPluginEffect&>(GetEffect()).GetType()
           == EffectTypeGenerate;
}

double AudacityPluginEffectInstance::generatorDuration() const noexcept
{
    return m_generatorDurationSeconds;
}

bool AudacityPluginEffectInstance::setGeneratorDuration(double durationSeconds)
{
    if (!isGenerator() || !std::isfinite(durationSeconds)
        || durationSeconds < 0.0) {
        return false;
    }
    m_generatorDurationSeconds = durationSeconds;
    return true;
}

void AudacityPluginEffectInstance::writeCurrentSettings(EffectSettings& settings) const
{
    if (isGenerator()) {
        settings.extra.SetDuration(m_generatorDurationSeconds);
    }
    auto& target = AudacityPluginEffect::GetSettings(settings).values;
    target.clear();
    const auto& descriptors = m_pluginInstance->parameters();
    for (uint64_t index = 0; index < descriptors.size(); ++index) {
        target.insert_or_assign(descriptors[static_cast<size_t>(index)].key,
                                m_pluginInstance->value(index));
    }
}

std::string AudacityPluginEffectInstance::GetLastError() const
{
    return m_lastError;
}

void AudacityPluginEffectInstance::setLastError(std::string error)
{
    m_lastError = std::move(error);
}

AudacityPluginEffect::AudacityPluginEffect(au::audacityplugin::EffectDescriptor descriptor)
    : m_descriptor(std::move(descriptor))
{
}

const au::audacityplugin::EffectDescriptor& AudacityPluginEffect::descriptor() const noexcept
{
    return m_descriptor;
}

PluginPath AudacityPluginEffect::GetPath() const
{
    return au3::wxFromStdString(m_descriptor.pluginId);
}

ComponentInterfaceSymbol AudacityPluginEffect::GetSymbol() const
{
    return { Identifier(au3::wxFromStdString(m_descriptor.effectId)),
             Verbatim(m_descriptor.name) };
}

VendorSymbol AudacityPluginEffect::GetVendor() const
{
    return { Identifier(au3::wxFromStdString(m_descriptor.pluginId)),
             Verbatim(m_descriptor.vendor) };
}

wxString AudacityPluginEffect::GetVersion() const
{
    return au3::wxFromStdString(m_descriptor.version);
}

TranslatableString AudacityPluginEffect::GetDescription() const
{
    return Verbatim(m_descriptor.description);
}

::EffectType AudacityPluginEffect::GetType() const
{
    return effectType(m_descriptor.group);
}

EffectFamilySymbol AudacityPluginEffect::GetFamily() const
{
    return { Identifier(audacity_plugin::EFFECT_FAMILY_ID),
             TranslatableString("effects", "Audacity plugin") };
}

bool AudacityPluginEffect::IsDefault() const
{
    return false;
}

bool AudacityPluginEffect::ParamsAreInputAgnostic() const
{
    return true;
}

bool AudacityPluginEffect::applyEffectToAllAudio() const
{
    return requiresInputSelection()
           && (m_descriptor.inputTrackTypes
               & au::audacityplugin::InputTrackAudio) != 0;
}

bool AudacityPluginEffect::requiresInputSelection() const
{
    return GetType() != EffectTypeGenerate && GetType() != EffectTypeTool;
}

bool AudacityPluginEffect::VisitSettings(
    SettingsVisitor& visitor, EffectSettings& settings)
{
    if (auto* shuttle = dynamic_cast<ShuttleSetAutomation*>(&visitor)) {
        return shuttle->mpEap && LoadSettings(*shuttle->mpEap, settings);
    }
    return true;
}

bool AudacityPluginEffect::VisitSettings(
    ConstSettingsVisitor& visitor, const EffectSettings& settings) const
{
    if (auto* shuttle = dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
        return shuttle->mpEap && SaveSettings(settings, *shuttle->mpEap);
    }
    return true;
}

bool AudacityPluginEffect::SaveSettings(const EffectSettings& settings,
                                        CommandParameters& parameters) const
{
    return parameters.Write(
        STATE_KEY, au3::wxFromStdString(serialize(GetSettings(settings))));
}

bool AudacityPluginEffect::LoadSettings(const CommandParameters& parameters,
                                        EffectSettings& settings) const
{
    wxString state;
    if (!parameters.Read(STATE_KEY, &state)) {
        GetSettings(settings).values.clear();
        return true;
    }
    const auto utf8 = state.utf8_str();
    GetSettings(settings) = deserialize(std::string_view(utf8.data(), utf8.length()));
    return true;
}
} // namespace au::effects
