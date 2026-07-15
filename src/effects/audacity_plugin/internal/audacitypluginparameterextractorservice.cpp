/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginparameterextractorservice.h"

#include <charconv>
#include <cmath>
#include <limits>

#include "framework/global/translation.h"

#include "audacityplugineffect.h"

namespace au::effects {
namespace {
AudacityPluginEffectInstance* asAudacityPluginEffectInstance(EffectInstance* instance)
{
    return dynamic_cast<AudacityPluginEffectInstance*>(instance);
}

const muse::String& standardDurationId()
{
    static const muse::String id { u"audacity.generator-duration" };
    return id;
}

struct ParameterRef {
    au::audacityplugin::IEffectInstance* pluginInstance = nullptr;
    const au::audacityplugin::ParameterDescriptor* descriptor = nullptr;
    uint64_t index = 0;
};

ParameterRef findParameter(AudacityPluginEffectInstance& instance,
                           const muse::String& parameterId)
{
    auto& pluginInstance = instance.pluginInstance();

    const std::string key = parameterId.toStdString();
    const auto& descriptors = pluginInstance.parameters();
    for (uint64_t index = 0; index < descriptors.size(); ++index) {
        if (descriptors[static_cast<size_t>(index)].key == key) {
            return { &pluginInstance, &descriptors[static_cast<size_t>(index)], index };
        }
    }
    return {};
}

muse::String valueString(const au::audacityplugin::Value& value)
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

ParameterInfo makeParameterInfo(const ParameterRef& parameter)
{
    ParameterInfo info;
    if (!parameter.descriptor) {
        return info;
    }

    const auto& descriptor = *parameter.descriptor;
    const auto current = parameter.pluginInstance->value(parameter.index);
    info.id = muse::String::fromStdString(descriptor.key);
    info.name = muse::String::fromStdString(descriptor.name);
    info.description = muse::String::fromStdString(descriptor.description);
    info.units = muse::String::fromStdString(descriptor.unit);
    info.currentValueString = valueString(current);

    using Type = au::audacityplugin::ParameterType;
    switch (descriptor.type) {
    case Type::Boolean:
        info.type = ParameterType::Toggle;
        info.defaultValue = std::get<bool>(descriptor.defaultValue) ? 1.0 : 0.0;
        info.currentValue = std::get<bool>(current) ? 1.0 : 0.0;
        info.minValue = 0.0;
        info.maxValue = 1.0;
        info.stepSize = 1.0;
        info.stepCount = 1;
        info.isInteger = true;
        break;
    case Type::Int64:
        // int64_t is passed as text, because doesn't fit in a double
        info.type = ParameterType::Text;
        break;
    case Type::Double:
        info.type = descriptor.minimum && descriptor.maximum
                    ? ParameterType::Slider : ParameterType::Numeric;
        info.defaultValue = std::get<double>(descriptor.defaultValue);
        info.currentValue = std::get<double>(current);
        info.minValue = descriptor.minimum
                        ? std::get<double>(*descriptor.minimum)
                        : -std::numeric_limits<double>::max();
        info.maxValue = descriptor.maximum
                        ? std::get<double>(*descriptor.maximum)
                        : std::numeric_limits<double>::max();
        if (descriptor.step) {
            info.stepSize = std::get<double>(*descriptor.step);
        }
        break;
    case Type::String:
        info.type = ParameterType::Text;
        break;
    case Type::File:
        info.type = ParameterType::File;
        break;
    case Type::Directory:
        info.type = ParameterType::Directory;
        break;
    case Type::Enumeration: {
        info.type = ParameterType::Dropdown;
        info.minValue = 0.0;
        info.maxValue = static_cast<double>(descriptor.enumChoices.size() - 1);
        info.stepSize = 1.0;
        info.stepCount = static_cast<int>(descriptor.enumChoices.size());
        const auto& currentToken = std::get<std::string>(current);
        const auto& defaultToken = std::get<std::string>(descriptor.defaultValue);
        for (size_t index = 0; index < descriptor.enumChoices.size(); ++index) {
            const auto& choice = descriptor.enumChoices[index];
            info.enumValues.push_back(muse::String::fromStdString(choice.name));
            info.enumIndices.push_back(static_cast<double>(index));
            if (choice.token == currentToken) {
                info.currentValue = static_cast<double>(index);
            }
            if (choice.token == defaultToken) {
                info.defaultValue = static_cast<double>(index);
            }
        }
        break;
    }
    }
    return info;
}

ParameterInfo makeStandardDurationInfo(const AudacityPluginEffectInstance& instance)
{
    ParameterInfo info;
    if (!instance.isGenerator()) {
        return info;
    }

    info.id = standardDurationId();
    info.name = muse::qtrc("effects", "Duration");
    info.description = muse::qtrc("effects", "Generated audio duration");
    info.type = ParameterType::Time;
    info.minValue = 0.0;
    info.maxValue = std::numeric_limits<double>::max();
    info.defaultValue = 30.0;
    info.currentValue = instance.generatorDuration();
    info.currentValueString = muse::String::number(info.currentValue, 15);
    return info;
}

void storeCurrent(AudacityPluginEffectInstance& instance,
                  const EffectSettingsAccessPtr& settingsAccess)
{
    if (!settingsAccess) {
        return;
    }
    settingsAccess->ModifySettings([&](EffectSettings& settings) {
        instance.writeCurrentSettings(settings);
        return nullptr;
    });
}
} // namespace

EffectFamily AudacityPluginParameterExtractorService::family() const
{
    return EffectFamily::AudacityPlugin;
}

ParameterInfoList AudacityPluginParameterExtractorService::extractParameters(
    EffectInstance* effectInstance, EffectSettingsAccessPtr settingsAccess) const
{
    auto* instance = asAudacityPluginEffectInstance(effectInstance);
    if (!instance) {
        return {};
    }
    if (settingsAccess) {
        instance->applySettings(settingsAccess->Get());
        storeCurrent(*instance, settingsAccess);
    }

    ParameterInfoList result;
    auto& pluginInstance = instance->pluginInstance();
    const auto& descriptors = pluginInstance.parameters();
    result.reserve(descriptors.size() + (instance->isGenerator() ? 1u : 0u));
    for (uint64_t index = 0; index < descriptors.size(); ++index) {
        result.push_back(makeParameterInfo({ &pluginInstance,
                                             &descriptors[static_cast<size_t>(index)], index }));
    }
    if (instance->isGenerator()) {
        result.push_back(makeStandardDurationInfo(*instance));
    }
    return result;
}

ParameterInfo AudacityPluginParameterExtractorService::getParameter(
    EffectInstance* instance, const muse::String& parameterId) const
{
    auto* plugin = asAudacityPluginEffectInstance(instance);
    if (!plugin) {
        return {};
    }
    if (parameterId == standardDurationId()) {
        return makeStandardDurationInfo(*plugin);
    }
    return makeParameterInfo(findParameter(*plugin, parameterId));
}

double AudacityPluginParameterExtractorService::getParameterValue(
    EffectInstance* instance, const muse::String& parameterId) const
{
    return getParameter(instance, parameterId).currentValue;
}

bool AudacityPluginParameterExtractorService::setParameterValue(
    EffectInstance* effectInstance, const muse::String& parameterId,
    double fullRangeValue, EffectSettingsAccessPtr settingsAccess)
{
    auto* instance = asAudacityPluginEffectInstance(effectInstance);
    if (!instance) {
        return false;
    }
    if (parameterId == standardDurationId()) {
        if (!instance->setGeneratorDuration(fullRangeValue)) {
            return false;
        }
        storeCurrent(*instance, settingsAccess);
        return true;
    }

    auto parameter = findParameter(*instance, parameterId);
    if (!parameter.descriptor || !std::isfinite(fullRangeValue)) {
        return false;
    }

    au::audacityplugin::Value value;
    using Type = au::audacityplugin::ParameterType;
    switch (parameter.descriptor->type) {
    case Type::Boolean:
        if (fullRangeValue != 0.0 && fullRangeValue != 1.0) {
            return false;
        }
        value = fullRangeValue == 1.0;
        break;
    case Type::Int64:
        return false;
    case Type::Double:
        value = fullRangeValue;
        break;
    case Type::Enumeration: {
        if (std::trunc(fullRangeValue) != fullRangeValue || fullRangeValue < 0.0
            || fullRangeValue >= static_cast<double>(parameter.descriptor->enumChoices.size())) {
            return false;
        }
        value = parameter.descriptor->enumChoices[static_cast<size_t>(fullRangeValue)].token;
        break;
    }
    case Type::String:
    case Type::File:
    case Type::Directory:
        return false;
    }

    if (parameter.pluginInstance->setValue(parameter.index, value) != au::audacityplugin::Status::Ok) {
        return false;
    }
    storeCurrent(*instance, settingsAccess);
    return true;
}

bool AudacityPluginParameterExtractorService::setParameterStringValue(
    EffectInstance* effectInstance, const muse::String& parameterId,
    const muse::String& stringValue, EffectSettingsAccessPtr settingsAccess)
{
    auto* instance = asAudacityPluginEffectInstance(effectInstance);
    if (!instance || parameterId == standardDurationId()) {
        return false;
    }
    auto parameter = findParameter(*instance, parameterId);
    if (!parameter.descriptor) {
        return false;
    }

    au::audacityplugin::Value value;
    const std::string text = stringValue.toStdString();
    using Type = au::audacityplugin::ParameterType;
    switch (parameter.descriptor->type) {
    case Type::Int64: {
        int64_t parsed = 0;
        const auto result = std::from_chars(text.data(), text.data() + text.size(), parsed);
        if (result.ec != std::errc() || result.ptr != text.data() + text.size()) {
            return false;
        }
        value = parsed;
        break;
    }
    case Type::String:
    case Type::Enumeration:
    case Type::File:
    case Type::Directory:
        value = text;
        break;
    case Type::Boolean:
    case Type::Double:
        return false;
    }

    if (parameter.pluginInstance->setValue(parameter.index, value) != au::audacityplugin::Status::Ok) {
        return false;
    }
    storeCurrent(*instance, settingsAccess);
    return true;
}

muse::String AudacityPluginParameterExtractorService::getParameterValueString(
    EffectInstance* instance, const muse::String& parameterId, double value) const
{
    auto* plugin = asAudacityPluginEffectInstance(instance);
    if (!plugin) {
        return {};
    }
    if (parameterId == standardDurationId()) {
        return muse::String::number(value, 15);
    }
    const auto parameter = findParameter(*plugin, parameterId);
    if (!parameter.pluginInstance) {
        return {};
    }

    using Type = au::audacityplugin::ParameterType;
    switch (parameter.descriptor->type) {
    case Type::Boolean:
        return value != 0.0 ? u"true" : u"false";
    case Type::Double:
        return muse::String::number(value, 15);
    case Type::Enumeration: {
        if (value >= 0.0 && std::trunc(value) == value
            && value < static_cast<double>(parameter.descriptor->enumChoices.size())) {
            return muse::String::fromStdString(parameter.descriptor->enumChoices[
                                                   static_cast<size_t>(value)].name);
        }
        return {};
    }
    case Type::Int64:
    case Type::String:
    case Type::File:
    case Type::Directory:
        return valueString(parameter.pluginInstance->value(parameter.index));
    }
    return {};
}
} // namespace au::effects
