/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <mutex>
#include <vector>

#include "abi.h"

namespace au::audacityplugin {
class LoadedPlugin;

class EffectInstance final : public IEffectInstance
{
public:
    EffectInstance(std::shared_ptr<LoadedPlugin> plugin, const aup_effect_v0* effect, const aup_effect_offline_v0* offline,
                   const aup_effect_parameters_v0* params, std::vector<ParameterDescriptor> parameters) noexcept;
    ~EffectInstance() override;

    const std::vector<ParameterDescriptor>& parameters() const override;
    Value value(uint64_t index) const override;
    Status setValue(uint64_t index, const Value& value) override;
    Status validate() override;
    Status apply(const OfflineArgs& args, IOfflineHost& host) override;

private:
    std::shared_ptr<LoadedPlugin> m_plugin;
    const aup_effect_v0* m_effect;
    const aup_effect_offline_v0* m_offline;
    const aup_effect_parameters_v0* m_params = nullptr;
    std::vector<ParameterDescriptor> m_parameters;
    mutable std::mutex m_mutex;
};
} // namespace au::audacityplugin
