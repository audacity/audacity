/*
 * Audacity: A Digital Audio Editor
 */
#include "effectinstance.h"

#include <utility>

#include "loadedplugin.h"
#include "offlineeffectadapter.h"

namespace au::audacityplugin {
EffectInstance::EffectInstance(
    std::shared_ptr<LoadedPlugin> plugin,
    const aup_effect_v0* effect,
    const aup_effect_offline_v0* offline,
    const aup_effect_parameters_v0* params,
    std::vector<ParameterDescriptor> parameters) noexcept
    : m_plugin(std::move(plugin))
    , m_effect(effect)
    , m_offline(offline)
    , m_params(params)
    , m_parameters(std::move(parameters))
{
}

EffectInstance::~EffectInstance()
{
    m_plugin->destroyEffect(m_effect);
}

const std::vector<ParameterDescriptor>& EffectInstance::parameters() const
{
    return m_parameters;
}

Value EffectInstance::value(uint64_t index) const
{
    std::lock_guard<std::mutex> guard(m_mutex);
    if (!m_params || index >= m_parameters.size()) {
        return {};
    }

    const auto& descriptor = m_parameters[index];
    auto output = internal::abiValue(descriptor.defaultValue, descriptor.type);
    try {
        m_params->get(m_params->context, index, &output);
        Value value;
        if (internal::copyValue(output, descriptor.type, value)
            && internal::validateValue(descriptor, value)) {
            return value;
        }
    } catch (...) {
    }
    return descriptor.defaultValue;
}

Status EffectInstance::setValue(uint64_t index, const Value& value)
{
    std::lock_guard<std::mutex> guard(m_mutex);
    if (!m_params || index >= m_parameters.size()) {
        return Status::InvalidArgument;
    }
    if (!internal::validateValue(m_parameters[index], value)) {
        return Status::ValidationFailed;
    }
    try {
        const auto converted = internal::abiValue(value, m_parameters[index].type);
        return internal::statusFromAbi(
            m_params->set(m_params->context, index, &converted));
    } catch (...) {
        return Status::PluginError;
    }
}

Status EffectInstance::validate()
{
    std::lock_guard<std::mutex> guard(m_mutex);
    if (!m_params) {
        return Status::Ok;
    }
    try {
        return internal::statusFromAbi(m_params->validate(m_params->context));
    } catch (...) {
        return Status::PluginError;
    }
}

Status EffectInstance::apply(const OfflineArgs& args, IOfflineHost& host)
{
    std::lock_guard<std::mutex> guard(m_mutex);
    return internal::invokeOfflineEffect(*m_offline, args, host);
}
} // namespace au::audacityplugin
