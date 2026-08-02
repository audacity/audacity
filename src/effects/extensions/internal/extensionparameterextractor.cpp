/*
 * Audacity: A Digital Audio Editor
 */
#include "extensionparameterextractor.h"

#include <cmath>
#include <limits>

#include "extensioneffect.h"

namespace au::effects::extensions {
namespace {
const muse::String& durationId()
{
    static const muse::String id{ u"audacity.generator-duration" };
    return id;
}

ExtensionEffectInstance* extensionInstance(EffectInstance* instance)
{
    return dynamic_cast<ExtensionEffectInstance*>(instance);
}

std::optional<size_t> parameterIndex(const ExtensionEffectInstance& instance, const muse::String& id)
{
    const std::string key = id.toStdString();
    const auto& parameters = instance.parameters();
    for (size_t index = 0; index < parameters.size(); ++index) {
        if (parameters[index].id == key) {
            return index;
        }
    }
    return std::nullopt;
}

muse::String valueString(const Value& value)
{
    if (const auto* data = std::get_if<bool>(&value)) {
        return *data ? u"true" : u"false";
    }
    if (const auto* data = std::get_if<int64_t>(&value)) {
        return muse::String::fromStdString(std::to_string(*data));
    }
    if (const auto* data = std::get_if<double>(&value)) {
        return muse::String::number(*data, 15);
    }
    return muse::String::fromStdString(std::get<std::string>(value));
}

ParameterInfo parameterInfo(const ExtensionEffectInstance& instance, size_t index)
{
    const auto& descriptor = instance.parameters().at(index);
    const auto& current = instance.value(index);
    ParameterInfo info;
    info.id = muse::String::fromStdString(descriptor.id);
    info.name = muse::String::fromStdString(descriptor.name);
    info.description = muse::String::fromStdString(descriptor.description);
    info.units = muse::String::fromStdString(descriptor.unit);
    info.currentValueString = valueString(current);

    switch (descriptor.type) {
    case ParameterType::Boolean:
        info.type = au::effects::ParameterType::Toggle;
        info.minValue = 0.0;
        info.maxValue = 1.0;
        info.defaultValue = std::get<bool>(descriptor.defaultValue) ? 1.0 : 0.0;
        info.currentValue = std::get<bool>(current) ? 1.0 : 0.0;
        info.stepSize = 1.0;
        info.stepCount = 1;
        info.isInteger = true;
        break;
    case ParameterType::Int64:
        info.type = au::effects::ParameterType::Text;
        info.isInteger = true;
        break;
    case ParameterType::Double:
        info.type = descriptor.minimum && descriptor.maximum ? au::effects::ParameterType::Slider : au::effects::ParameterType::Numeric;
        info.minValue = descriptor.minimum ? std::get<double>(*descriptor.minimum) : -std::numeric_limits<double>::max();
        info.maxValue = descriptor.maximum ? std::get<double>(*descriptor.maximum) : std::numeric_limits<double>::max();
        info.defaultValue = std::get<double>(descriptor.defaultValue);
        info.currentValue = std::get<double>(current);
        if (descriptor.step) {
            info.stepSize = std::get<double>(*descriptor.step);
        }
        break;
    case ParameterType::Enumeration: {
        info.type = au::effects::ParameterType::Dropdown;
        info.minValue = 0.0;
        info.maxValue = descriptor.choices.empty() ? 0.0 : static_cast<double>(descriptor.choices.size() - 1);
        info.stepSize = 1.0;
        info.stepCount = static_cast<int>(descriptor.choices.size());
        const auto& currentToken = std::get<std::string>(current);
        const auto& defaultToken = std::get<std::string>(descriptor.defaultValue);
        for (size_t choice = 0; choice < descriptor.choices.size(); ++choice) {
            info.enumValues.push_back(muse::String::fromStdString(descriptor.choices[choice].name));
            info.enumIndices.push_back(static_cast<double>(choice));
            if (descriptor.choices[choice].token == currentToken) {
                info.currentValue = choice;
            }
            if (descriptor.choices[choice].token == defaultToken) {
                info.defaultValue = choice;
            }
        }
        break;
    }
    case ParameterType::File:
        info.type = au::effects::ParameterType::File;
        break;
    case ParameterType::Directory:
        info.type = au::effects::ParameterType::File;
        info.isDirectory = true;
        break;
    case ParameterType::String:
        info.type = au::effects::ParameterType::Text;
        break;
    }
    return info;
}

ParameterInfo durationInfo(const ExtensionEffectInstance& instance)
{
    ParameterInfo info;
    info.id = durationId();
    info.name = muse::qtrc("effects", "Duration");
    info.description = muse::qtrc("effects", "Generated audio duration");
    info.type = au::effects::ParameterType::Time;
    info.minValue = 0.001;
    info.maxValue = std::numeric_limits<double>::max();
    info.defaultValue = 30.0;
    info.currentValue = instance.duration();
    info.currentValueString = muse::String::number(info.currentValue, 15);
    return info;
}

void store(ExtensionEffectInstance& instance, const EffectSettingsAccessPtr& access)
{
    if (!access) {
        return;
    }
    access->ModifySettings([&](EffectSettings& settings) {
        instance.writeCurrentSettings(settings);
        return nullptr;
    });
}
} // namespace

EffectFamily ExtensionParameterExtractor::family() const
{
    return EffectFamily::Extension;
}

ParameterInfoList ExtensionParameterExtractor::extractParameters(EffectInstance* effectInstance, EffectSettingsAccessPtr access) const
{
    auto* instance = extensionInstance(effectInstance);
    if (!instance) {
        return ParameterInfoList{};
    }
    if (access && instance->applySettings(access->Get())) {
        store(*instance, access);
    }
    ParameterInfoList result;
    for (size_t index = 0; index < instance->parameters().size(); ++index) {
        result.push_back(parameterInfo(*instance, index));
    }
    if (static_cast<ExtensionEffect&>(instance->GetEffect()).GetType() == EffectTypeGenerate) {
        result.push_back(durationInfo(*instance));
    }
    return result;
}

ParameterInfo ExtensionParameterExtractor::getParameter(EffectInstance* effectInstance, const muse::String& id) const
{
    auto* instance = extensionInstance(effectInstance);
    if (!instance) {
        return ParameterInfo{};
    }
    if (id == durationId()) {
        return durationInfo(*instance);
    }
    const auto index = parameterIndex(*instance, id);
    return index ? parameterInfo(*instance, *index) : ParameterInfo {};
}

double ExtensionParameterExtractor::getParameterValue(EffectInstance* effectInstance, const muse::String& id) const
{
    const ParameterInfo info = getParameter(effectInstance, id);
    return info.isValid() ? info.currentValue : 0.0;
}

bool ExtensionParameterExtractor::setParameterValue(EffectInstance* effectInstance, const muse::String& id, double value,
                                                    EffectSettingsAccessPtr access)
{
    auto* instance = extensionInstance(effectInstance);
    if (!instance || !std::isfinite(value)) {
        return false;
    }
    if (id == durationId()) {
        if (!instance->setDuration(value)) {
            return false;
        }
        store(*instance, access);
        return true;
    }
    const auto index = parameterIndex(*instance, id);
    if (!index) {
        return false;
    }
    const auto& descriptor = instance->parameters()[*index];
    Value converted;
    switch (descriptor.type) {
    case ParameterType::Boolean:
        converted = value >= 0.5;
        break;
    case ParameterType::Double:
        converted = value;
        break;
    case ParameterType::Enumeration: {
        const auto choice = static_cast<size_t>(std::llround(value));
        if (choice >= descriptor.choices.size()) {
            return false;
        }
        converted = descriptor.choices[choice].token;
        break;
    }
    default:
        return false;
    }
    if (!instance->setValue(*index, std::move(converted))) {
        return false;
    }
    store(*instance, access);
    return true;
}

bool ExtensionParameterExtractor::setParameterStringValue(EffectInstance* effectInstance, const muse::String& id, const muse::String& value,
                                                          EffectSettingsAccessPtr access)
{
    auto* instance = extensionInstance(effectInstance);
    if (!instance) {
        return false;
    }
    const auto index = parameterIndex(*instance, id);
    if (!index) {
        return false;
    }
    const auto type = instance->parameters()[*index].type;
    Value converted;
    if (type == ParameterType::Int64) {
        bool ok = false;
        const qlonglong number = value.toQString().toLongLong(&ok);
        if (!ok) {
            return false;
        }
        converted = static_cast<int64_t>(number);
    } else if (type == ParameterType::String || type == ParameterType::File || type == ParameterType::Directory) {
        converted = value.toStdString();
    } else {
        return false;
    }
    if (!instance->setValue(*index, std::move(converted))) {
        return false;
    }
    store(*instance, access);
    return true;
}

muse::String ExtensionParameterExtractor::getParameterValueString(EffectInstance* effectInstance, const muse::String& id,
                                                                  double value) const
{
    const auto* instance = extensionInstance(effectInstance);
    if (!instance) {
        return muse::String{};
    }
    if (id == durationId()) {
        return muse::String::number(value, 15);
    }
    const auto index = parameterIndex(*instance, id);
    if (!index) {
        return muse::String{};
    }
    const auto& descriptor = instance->parameters()[*index];
    if (descriptor.type == ParameterType::Enumeration) {
        const auto choice = static_cast<size_t>(std::llround(value));
        return choice < descriptor.choices.size() ? muse::String::fromStdString(descriptor.choices[choice].name) : muse::String{};
    }
    if (descriptor.type == ParameterType::Boolean) {
        return valueString(value >= 0.5);
    }
    if (descriptor.type == ParameterType::Double) {
        return valueString(value);
    }
    return valueString(instance->value(*index));
}
} // namespace au::effects::extensions
