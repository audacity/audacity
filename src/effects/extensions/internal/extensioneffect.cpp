/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffect.h"

#include <algorithm>
#include <charconv>
#include <cmath>
#include <map>
#include <string_view>

#include <QCoreApplication>
#include <QEventLoop>
#include <QTimer>

#include <wx/string.h>

#include "au3-command-parameters/ShuttleAutomation.h"
#include "au3-components/EffectAutomationParameters.h"
#include "au3-strings/Identifier.h"
#include "au3-track/Track.h"
#include "au3wrap/au3projectcontext.h"
#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/serialization/json.h"
#include "framework/global/log.h"

#include "extensioneffectrunner.h"

namespace au::effects::extensions {
namespace {
constexpr const wxChar* STATE_KEY = wxT("State");

bool sameType(const Value& value, ParameterType type)
{
    switch (type) {
    case ParameterType::Boolean:
        return std::holds_alternative<bool>(value);
    case ParameterType::Int64:
        return std::holds_alternative<int64_t>(value);
    case ParameterType::Double:
        return std::holds_alternative<double>(value);
    case ParameterType::String:
    case ParameterType::Enumeration:
    case ParameterType::File:
    case ParameterType::Directory:
        return std::holds_alternative<std::string>(value);
    }
    return false;
}

bool validValue(const ParameterDescriptor& descriptor, const Value& value)
{
    if (!sameType(value, descriptor.type)) {
        return false;
    }
    if (const auto* number = std::get_if<int64_t>(&value)) {
        if (descriptor.minimum && *number < std::get<int64_t>(*descriptor.minimum)) {
            return false;
        }
        if (descriptor.maximum && *number > std::get<int64_t>(*descriptor.maximum)) {
            return false;
        }
    } else if (const auto* number = std::get_if<double>(&value)) {
        if (!std::isfinite(*number)) {
            return false;
        }
        if (descriptor.minimum && *number < std::get<double>(*descriptor.minimum)) {
            return false;
        }
        if (descriptor.maximum && *number > std::get<double>(*descriptor.maximum)) {
            return false;
        }
    } else if (descriptor.type == ParameterType::Enumeration) {
        const auto& token = std::get<std::string>(value);
        return std::any_of(descriptor.choices.begin(), descriptor.choices.end(), [&](const EnumChoice& choice) {
            return choice.token == token;
        });
    }
    return true;
}

std::string serialize(const Settings& settings)
{
    muse::JsonObject root;
    for (const auto&[key, value] : settings.values) {
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
    const muse::ByteArray json = muse::JsonDocument(root).toJson(muse::JsonDocument::Format::Compact);
    return { json.constChar(), json.size() };
}

Settings deserialize(std::string_view state)
{
    Settings result;
    std::string error;
    const muse::JsonDocument document = muse::JsonDocument::fromJson(muse::ByteArray::fromRawData(state.data(), state.size()), &error);
    if (!error.empty() || !document.isObject()) {
        return result;
    }

    const muse::JsonObject root = document.rootObject();
    for (const std::string& key : root.keys()) {
        const muse::JsonObject item = root.value(key).toObject();
        const std::string type = item.value("type").toStdString();
        const muse::JsonValue stored = item.value("value");
        if (type == "bool" && stored.isBool()) {
            result.values.emplace(key, stored.toBool());
        } else if (type == "int64" && stored.isString()) {
            int64_t number = 0;
            const std::string text = stored.toStdString();
            const auto parsed = std::from_chars(text.data(), text.data() + text.size(), number);
            if (parsed.ec == std::errc() && parsed.ptr == text.data() + text.size()) {
                result.values.emplace(key, number);
            }
        } else if (type == "double" && stored.isNumber() && std::isfinite(stored.toDouble())) {
            result.values.emplace(key, stored.toDouble());
        } else if (type == "string" && stored.isString()) {
            result.values.emplace(key, stored.toStdString());
        }
    }
    return result;
}

::EffectType effectType(const std::string& group)
{
    switch (effectTypeFromGroup(group)) {
    case EffectType::Generator:
        return EffectTypeGenerate;
    case EffectType::Analyzer:
        return EffectTypeAnalyze;
    case EffectType::Tool:
        return EffectTypeTool;
    default:
        return EffectTypeProcess;
    }
}
} // namespace

ExtensionEffectInstance::ExtensionEffectInstance(ExtensionEffect& effect, double selectionDuration,
                                                 const muse::modularity::ContextPtr& context)
    : StatefulEffect::Instance(effect), m_runtime(std::make_unique<ExtensionEffectRuntime>(effect.descriptor(), context)),
    m_duration(selectionDuration > 0.0 ? selectionDuration : 30.0)
{
}

bool ExtensionEffectInstance::Init()
{
    const auto& descriptor = static_cast<ExtensionEffect&>(GetEffect()).descriptor();
    muse::Ret initialized = m_runtime->initialize();
    if (!initialized) {
        m_lastError = initialized.toString();
        LOGE() << "failed to initialize extension effect " << descriptor.effectId << ": " << m_lastError;
        return false;
    }
    auto parameters = m_runtime->parameters();
    if (!parameters.ret) {
        m_lastError = parameters.ret.toString();
        LOGE() << "failed to read parameters for extension effect " << descriptor.effectId << ": " << m_lastError;
        return false;
    }
    m_parameters = std::move(parameters.val);
    for (const auto& parameter : m_parameters) {
        if (!validValue(parameter, parameter.defaultValue)) {
            m_lastError = "Invalid default value for parameter " + parameter.id;
            LOGE() << "failed to initialize extension effect " << descriptor.effectId << ": " << m_lastError;
            return false;
        }
        m_settings.values.emplace(parameter.id, parameter.defaultValue);
    }
    return true;
}

const std::vector<ParameterDescriptor>& ExtensionEffectInstance::parameters() const
{
    return m_parameters;
}

const Value& ExtensionEffectInstance::value(size_t index) const
{
    return m_settings.values.at(m_parameters.at(index).id);
}

bool ExtensionEffectInstance::setValue(size_t index, Value value)
{
    if (index >= m_parameters.size() || !validValue(m_parameters[index], value)) {
        return false;
    }
    m_settings.values.insert_or_assign(m_parameters[index].id, std::move(value));
    return true;
}

bool ExtensionEffectInstance::applySettings(const EffectSettings& settings)
{
    const Settings& stored = ExtensionEffect::GetSettings(settings);
    Settings next = m_settings;
    for (size_t index = 0; index < m_parameters.size(); ++index) {
        const auto found = stored.values.find(m_parameters[index].id);
        const Value& value = found != stored.values.end() ? found->second : m_parameters[index].defaultValue;
        if (!validValue(m_parameters[index], value)) {
            return false;
        }
        next.values.insert_or_assign(m_parameters[index].id, value);
    }
    double duration = m_duration;
    if (static_cast<ExtensionEffect&>(GetEffect()).GetType() == EffectTypeGenerate) {
        duration = settings.extra.GetDuration();
        if (!std::isfinite(duration) || duration <= 0.0) {
            return false;
        }
    }
    m_settings = std::move(next);
    return static_cast<ExtensionEffect&>(GetEffect()).GetType() != EffectTypeGenerate || setDuration(duration);
}

void ExtensionEffectInstance::writeCurrentSettings(EffectSettings& settings) const
{
    ExtensionEffect::GetSettings(settings) = m_settings;
    if (static_cast<ExtensionEffect&>(GetEffect()).GetType() == EffectTypeGenerate) {
        settings.extra.SetDuration(m_duration);
    }
}

const Settings& ExtensionEffectInstance::currentSettings() const
{
    return m_settings;
}

double ExtensionEffectInstance::duration() const
{
    return m_duration;
}

bool ExtensionEffectInstance::setDuration(double seconds)
{
    if (!std::isfinite(seconds) || seconds <= 0.0) {
        return false;
    }
    m_duration = seconds;
    auto& effect = static_cast<ExtensionEffect&>(GetEffect());
    effect.mT1 = effect.mT0 + seconds;
    return true;
}

void ExtensionEffectInstance::setLastError(std::string error)
{
    m_lastError = std::move(error);
}

namespace {
std::map<std::string, int>& orphanCounts()
{
    static std::map<std::string, int> counts;
    return counts;
}

class OrphanRuntime final : public QObject
{
public:
    OrphanRuntime(std::unique_ptr<ExtensionEffectRuntime> runtime, const QJSValue& pendingPromise, std::string extensionId)
        : QObject(QCoreApplication::instance()), m_runtime(std::move(runtime)), m_extensionId(std::move(extensionId))
    {
        ++orphanCounts()[m_extensionId];
        auto* observer = new ExtensionPromiseObserver(this);
        connect(observer, &ExtensionPromiseObserver::resolved, this, [this](const QJSValue&) {
            settled();
        });
        connect(observer, &ExtensionPromiseObserver::rejected, this, [this](const QString&) {
            settled();
        });
        const auto observing = m_runtime->observePromise(pendingPromise, observer);
        if (!observing.ret || !observing.val) {
            LOGE() << "could not observe the abandoned promise of extension " << m_extensionId << "; the runtime is kept, and the "
                   << "extension stays unavailable, until the application exits";
        }
    }

    ~OrphanRuntime() override
    {
        if (!m_settled) {
            m_runtime.release();
        }
        auto& counts = orphanCounts();
        if (--counts[m_extensionId] <= 0) {
            counts.erase(m_extensionId);
        }
    }

private:
    void settled()
    {
        m_settled = true;
        deleteLater();
    }

    std::unique_ptr<ExtensionEffectRuntime> m_runtime;
    std::string m_extensionId;
    bool m_settled = false;
};
} // namespace

bool extensionBusy(const std::string& extensionId)
{
    const auto& counts = orphanCounts();
    return counts.find(extensionId) != counts.end();
}

void ExtensionEffectInstance::orphanRuntime(const QJSValue& pendingPromise)
{
    if (m_runtime) {
        new OrphanRuntime(std::move(m_runtime), pendingPromise, static_cast<ExtensionEffect&>(GetEffect()).descriptor().extensionId);
    }
}

muse::Ret ExtensionEffectInstance::validate()
{
    if (!m_runtime) {
        return muse::make_ret(muse::Ret::Code::InternalError, std::string { "The previous run of this effect was abandoned" });
    }
    const auto result = m_runtime->validate(m_settings);
    if (!result.ret) {
        return result.ret;
    }
    return result.val.empty() ? muse::make_ok() : muse::make_ret(muse::Ret::Code::BadData, result.val);
}

std::string ExtensionEffectInstance::GetLastError() const
{
    return m_lastError;
}

ExtensionEffect::ExtensionEffect(EffectDescriptor descriptor)
    : m_descriptor(std::move(descriptor))
{
}

const EffectDescriptor& ExtensionEffect::descriptor() const
{
    return m_descriptor;
}

PluginPath ExtensionEffect::GetPath() const
{
    return au3::wxFromStdString(m_descriptor.extensionId);
}

ComponentInterfaceSymbol ExtensionEffect::GetSymbol() const
{
    return { Identifier(au3::wxFromStdString(m_descriptor.effectId)), Verbatim(m_descriptor.title) };
}

VendorSymbol ExtensionEffect::GetVendor() const
{
    return { Identifier(au3::wxFromStdString(m_descriptor.extensionId)), Verbatim(m_descriptor.vendor) };
}

wxString ExtensionEffect::GetVersion() const
{
    return au3::wxFromStdString(m_descriptor.version);
}

TranslatableString ExtensionEffect::GetDescription() const
{
    return Verbatim(m_descriptor.description);
}

::EffectType ExtensionEffect::GetType() const
{
    return effectType(m_descriptor.group);
}

EffectFamilySymbol ExtensionEffect::GetFamily() const
{
    return { Identifier(wxT("Extension")), TranslatableString("effects", "Extension") };
}

bool ExtensionEffect::IsDefault() const
{
    return false;
}

bool ExtensionEffect::ParamsAreInputAgnostic() const
{
    return true;
}

bool ExtensionEffect::VisitSettings(SettingsVisitor& visitor, EffectSettings& settings)
{
    if (auto* shuttle = dynamic_cast<ShuttleSetAutomation*>(&visitor)) {
        return shuttle->mpEap && LoadSettings(*shuttle->mpEap, settings);
    }
    return true;
}

bool ExtensionEffect::VisitSettings(ConstSettingsVisitor& visitor, const EffectSettings& settings) const
{
    if (auto* shuttle = dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
        return shuttle->mpEap && SaveSettings(settings, *shuttle->mpEap);
    }
    return true;
}

bool ExtensionEffect::SaveSettings(const EffectSettings& settings, CommandParameters& parameters) const
{
    return parameters.Write(STATE_KEY, au3::wxFromStdString(serialize(GetSettings(settings))));
}

bool ExtensionEffect::LoadSettings(const CommandParameters& parameters, EffectSettings& settings) const
{
    wxString state;
    if (!parameters.Read(STATE_KEY, &state)) {
        GetSettings(settings).values.clear();
        return true;
    }
    const auto utf8 = state.utf8_str();
    GetSettings(settings) = deserialize({ utf8.data(), utf8.length() });
    return true;
}

std::shared_ptr<::EffectInstance> ExtensionEffect::MakeInstance() const
{
    auto* project = mTracks ? mTracks->GetOwner() : nullptr;
    return std::make_shared<ExtensionEffectInstance>(const_cast<ExtensionEffect&>(*this), std::abs(
                                                         mT1 - mT0), project ? au::au3::projectIocContext(*project) : nullptr);
}

bool ExtensionEffect::Process(::EffectInstance& instance, ::EffectSettings& settings)
{
    auto& extensionInstance = dynamic_cast<ExtensionEffectInstance&>(instance);
    if (extensionInstance.abandoned()) {
        extensionInstance.setLastError("The previous run of this effect was abandoned");
        return false;
    }
    if (extensionBusy(m_descriptor.extensionId)) {
        extensionInstance.setLastError("A previous run of this extension has not finished yet");
        return false;
    }
    bool finished = false;
    bool success = false;
    QEventLoop loop;
    ExtensionEffectRun run(*this, extensionInstance, settings, [&](bool result) {
        finished = true;
        success = result;
        loop.quit();
    });
    run.start();
    if (finished) {
        return success;
    }

    QTimer poll;
    poll.setInterval(50);
    int cancelledTicks = 0;
    QObject::connect(&poll, &QTimer::timeout, &loop, [&] {
        run.updateProgress();
        if (finished || !run.cancelled()) {
            return;
        }
        if (++cancelledTicks >= 100) {
            run.abort("The extension did not respond to cancellation");
        }
    });
    poll.start();
    loop.exec();
    return success;
}
} // namespace au::effects::extensions
