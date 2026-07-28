/*
 * Audacity: A Digital Audio Editor
 */
#include "extensioneffectruntime.h"

#include <cmath>
#include <set>
#include <string>
#include <type_traits>

#include "framework/extensions/iextensionsprovider.h"
#include "extensioneffectrunner.h"
#include "trackedit/api/projectedit.h"

namespace au::effects::extensions {
namespace {
std::optional<ParameterType> parameterType(const QString& value)
{
    if (value == u"bool") {
        return ParameterType::Boolean;
    }
    if (value == u"int64") {
        return ParameterType::Int64;
    }
    if (value == u"double") {
        return ParameterType::Double;
    }
    if (value == u"string") {
        return ParameterType::String;
    }
    if (value == u"enum") {
        return ParameterType::Enumeration;
    }
    if (value == u"file") {
        return ParameterType::File;
    }
    if (value == u"directory") {
        return ParameterType::Directory;
    }
    return std::nullopt;
}

std::optional<Value> value(const QJSValue& input, ParameterType type)
{
    switch (type) {
    case ParameterType::Boolean:
        return input.isBool() ? std::optional<Value> { input.toBool() } : std::nullopt;
    case ParameterType::Int64:
        if (input.isNumber() && std::isfinite(input.toNumber()) && std::trunc(input.toNumber()) == input.toNumber()
            && std::abs(input.toNumber()) <= 9007199254740991.0) {
            return static_cast<int64_t>(input.toNumber());
        }
        if (input.isString()) {
            bool ok = false;
            const qlonglong converted = input.toString().toLongLong(&ok);
            if (ok) {
                return static_cast<int64_t>(converted);
            }
        }
        return std::nullopt;
    case ParameterType::Double:
        return input.isNumber() && std::isfinite(input.toNumber()) ? std::optional<Value> { input.toNumber() } : std::nullopt;
    case ParameterType::String:
    case ParameterType::Enumeration:
    case ParameterType::File:
    case ParameterType::Directory:
        return input.isString() ? std::optional<Value> { input.toString().toStdString() } : std::nullopt;
    }
    return std::nullopt;
}
} // namespace

ExtensionEffectRuntime::ExtensionEffectRuntime(const EffectDescriptor& descriptor, const muse::modularity::ContextPtr& context)
    : m_descriptor(descriptor)
{
    if (!context) {
        LOGE() << "extension effect context is unavailable";
        return;
    }
    auto provider = muse::modularity::ioc(context)->resolve<muse::extensions::IExtensionsProvider>("effects_extensions");
    if (!provider) {
        LOGE() << "extensions provider is unavailable";
        return;
    }
    const auto& manifest = provider->manifest(descriptor.manifest.uri);
    if (!manifest.isValid() || manifest.path != descriptor.manifest.path || manifest.version != descriptor.manifest.version) {
        provider->reloadExtensions();
    }
    m_session = provider->newSession(descriptor.manifest.uri, descriptor.scriptPath);
}

ExtensionEffectRuntime::~ExtensionEffectRuntime()
{
    if (m_effect.isObject()) {
        trackedit::api::ProjectApiScope scope;
        call("destroy");
    }
}

muse::Ret ExtensionEffectRuntime::initialize()
{
    trackedit::api::ProjectApiScope scope;
    if (!m_session) {
        return muse::make_ret(muse::Ret::Code::InternalError, std::string { "could not create extension session" });
    }

    muse::Ret result = m_session->evaluate();
    if (!result) {
        return result;
    }

    QJSValue factory = m_session->exports().property(QString::fromStdString(m_descriptor.factory));
    if (!factory.isCallable()) {
        return muse::make_ret(muse::Ret::Code::BadData, std::string { "effect factory is not callable" });
    }
    m_effect = factory.call();
    if (m_effect.isError() || !m_effect.isObject()) {
        return muse::make_ret(muse::Ret::Code::BadData, m_effect.toString().toStdString());
    }
    return muse::make_ok();
}

muse::RetVal<std::vector<ParameterDescriptor> > ExtensionEffectRuntime::parameters()
{
    trackedit::api::ProjectApiScope scope;
    QJSValue result = call("parameters");
    if (result.isError() || !result.isArray()) {
        return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData, "parameters() must return an array");
    }

    std::vector<ParameterDescriptor> descriptors;
    const double length = result.property("length").toNumber();
    if (!(length >= 0.0 && length <= 256.0)) {
        return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData,
                                                                         "parameters() returned too many parameters");
    }
    const quint32 count = static_cast<quint32>(length);
    descriptors.reserve(count);
    std::set<std::string> seenIds;
    for (quint32 index = 0; index < count; ++index) {
        const QJSValue item = result.property(index);
        const auto type = parameterType(item.property("type").toString());
        if (!item.isObject() || !type) {
            return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData, "invalid parameter descriptor");
        }

        ParameterDescriptor descriptor;
        descriptor.id = item.property("id").toString().toStdString();
        descriptor.name = item.property("name").toString().toStdString();
        const QJSValue description = item.property("description");
        const QJSValue unit = item.property("unit");
        descriptor.description = description.isString() ? description.toString().toStdString() : std::string{};
        descriptor.unit = unit.isString() ? unit.toString().toStdString() : std::string{};
        descriptor.type = *type;
        const auto defaultValue = value(item.property("defaultValue"), *type);
        if (descriptor.id.empty() || descriptor.name.empty() || !defaultValue) {
            return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData,
                                                                             "parameter id, name, and defaultValue are required");
        }
        descriptor.defaultValue = *defaultValue;
        if (!seenIds.insert(descriptor.id).second) {
            return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData, "duplicate parameter id");
        }
        if (const auto converted = value(item.property("min"), *type)) {
            descriptor.minimum = *converted;
        }
        if (const auto converted = value(item.property("max"), *type)) {
            descriptor.maximum = *converted;
        }
        if (const auto converted = value(item.property("step"), *type)) {
            descriptor.step = *converted;
        }

        if (*type == ParameterType::Enumeration) {
            const QJSValue choices = item.property("choices");
            const double choiceLength = choices.isArray() ? choices.property("length").toNumber() : -1.0;
            if (!(choiceLength >= 1.0 && choiceLength <= 1024.0)) {
                return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData,
                                                                                 "enum parameters require a non-empty choices array");
            }
            const quint32 choiceCount = static_cast<quint32>(choiceLength);
            std::set<std::string> seenTokens;
            for (quint32 choiceIndex = 0; choiceIndex < choiceCount; ++choiceIndex) {
                const QJSValue choice = choices.property(choiceIndex);
                EnumChoice parsed{
                    choice.property("token").toString().toStdString(),
                    choice.property("name").toString().toStdString(),
                };
                if (parsed.token.empty() || parsed.name.empty() || !seenTokens.insert(parsed.token).second) {
                    return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData,
                                                                                     "enum choices require unique tokens and names");
                }
                descriptor.choices.push_back(std::move(parsed));
            }
            if (seenTokens.find(std::get<std::string>(descriptor.defaultValue)) == seenTokens.end()) {
                return muse::RetVal<std::vector<ParameterDescriptor> >::make_ret(muse::Ret::Code::BadData,
                                                                                 "the enum defaultValue must be one of the choice tokens");
            }
        }
        descriptors.push_back(std::move(descriptor));
    }
    return muse::RetVal<std::vector<ParameterDescriptor> >::make_ok(std::move(descriptors));
}

muse::RetVal<std::string> ExtensionEffectRuntime::validate(const Settings& settings)
{
    trackedit::api::ProjectApiScope scope;
    QJSValue validator = m_effect.property("validate");
    if (!validator.isCallable()) {
        return muse::RetVal<std::string>::make_ok({});
    }
    QJSValue result = validator.callWithInstance(m_effect, { m_session->toScriptValue(settingsMap(settings)) });
    if (result.isError()) {
        return muse::RetVal<std::string>::make_ret(muse::Ret::Code::UnknownError, result.toString().toStdString());
    }
    if (result.isString()) {
        return muse::RetVal<std::string>::make_ok(result.toString().toStdString());
    }
    if (result.isBool() && !result.toBool()) {
        return muse::RetVal<std::string>::make_ok("The extension is not ready");
    }
    return muse::RetVal<std::string>::make_ok({});
}

muse::RetVal<QJSValue> ExtensionEffectRuntime::process(const Settings& settings, trackedit::api::ProjectEditSession& projectSession,
                                                       QObject* task)
{
    trackedit::api::ProjectApiScope scope(&projectSession);
    if (!m_effect.property(QStringLiteral("process")).isCallable()) {
        return muse::RetVal<QJSValue>::make_ret(muse::Ret::Code::BadData, "process() is required");
    }
    QJSValue result = call("process", {
            m_session->toScriptValue(settingsMap(settings)),
            m_session->wrapQObject(&projectSession),
            m_session->wrapQObject(task),
        });
    if (result.isError()) {
        return muse::RetVal<QJSValue>::make_ret(muse::Ret::Code::UnknownError, result.toString().toStdString());
    }
    return muse::RetVal<QJSValue>::make_ok(result);
}

muse::RetVal<bool> ExtensionEffectRuntime::observePromise(const QJSValue& value, QObject* observer)
{
    const QJSValue then = value.property(QStringLiteral("then"));
    if (!then.isCallable()) {
        return muse::RetVal<bool>::make_ok(false);
    }
    const QJSValue wrapped = m_session->wrapQObject(observer);
    const QJSValue scheduled = then.callWithInstance(value, {
            wrapped.property(QStringLiteral("resolve")),
            wrapped.property(QStringLiteral("reject")),
        });
    if (scheduled.isError()) {
        return muse::RetVal<bool>::make_ret(muse::Ret::Code::UnknownError, scheduled.toString().toStdString());
    }
    return muse::RetVal<bool>::make_ok(true);
}

bool ExtensionEffectRuntime::hasProgressCallback() const
{
    return m_effect.property(QStringLiteral("poll")).isCallable();
}

muse::Ret ExtensionEffectRuntime::updateProgress(trackedit::api::ProjectEditSession& projectSession, QObject* task)
{
    trackedit::api::ProjectApiScope scope(&projectSession);
    const QJSValue result = call("poll", { m_session->wrapQObject(task) });
    if (result.isError()) {
        return muse::make_ret(muse::Ret::Code::UnknownError, result.toString().toStdString());
    }
    return muse::make_ok();
}

QJSValue ExtensionEffectRuntime::call(const char* method, const QJSValueList& arguments)
{
    QJSValue function = m_effect.property(QString::fromUtf8(method));
    if (!function.isCallable()) {
        return QJSValue(QJSValue::UndefinedValue);
    }
    return function.callWithInstance(m_effect, arguments);
}

QVariantMap ExtensionEffectRuntime::settingsMap(const Settings& settings) const
{
    QVariantMap result;
    for (const auto&[id, value] : settings.values) {
        const QString key = QString::fromStdString(id);
        std::visit(
            [&](const auto& stored) {
            using Stored = std::decay_t<decltype(stored)>;
            if constexpr (std::is_same_v<Stored, std::string>) {
                result.insert(key, QString::fromStdString(stored));
            } else if constexpr (std::is_same_v<Stored, int64_t>) {
                result.insert(key, QString::number(stored));
            } else {
                result.insert(key, QVariant::fromValue(stored));
            }
        },
            value);
    }
    return result;
}
} // namespace au::effects::extensions
