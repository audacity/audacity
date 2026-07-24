/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <map>
#include <memory>
#include <string>

#include "au3-effects/StatefulEffect.h"

#include "audacityplugin/audacityplugintypes.h"

namespace au::effects {
struct AudacityPluginSettings {
    std::map<std::string, au::audacityplugin::Value> values;
};

class AudacityPluginEffect;

class AudacityPluginEffectInstance final : public StatefulEffect::Instance
{
public:
    AudacityPluginEffectInstance(AudacityPluginEffect& effect, std::shared_ptr<au::audacityplugin::IEffectInstance> pluginInstance);

    au::audacityplugin::IEffectInstance& pluginInstance() const noexcept;
    bool applySettings(const EffectSettings& settings);
    bool isGenerator() const noexcept;
    double generatorDuration() const noexcept;
    bool setGeneratorDuration(double durationSeconds);
    void writeCurrentSettings(EffectSettings& settings) const;
    void setLastError(std::string error);

    std::string GetLastError() const override;

private:
    std::shared_ptr<au::audacityplugin::IEffectInstance> m_pluginInstance;
    double m_generatorDurationSeconds = 0.0;
    std::string m_lastError;
};

class AudacityPluginEffect final : public EffectWithSettings<AudacityPluginSettings, StatefulEffect>
{
public:
    explicit AudacityPluginEffect(au::audacityplugin::EffectDescriptor descriptor);

    const au::audacityplugin::EffectDescriptor& descriptor() const noexcept;

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    ::EffectType GetType() const override;
    EffectFamilySymbol GetFamily() const override;
    bool IsDefault() const override;
    bool ParamsAreInputAgnostic() const override;
    bool applyEffectToAllAudio() const override;

    bool VisitSettings(SettingsVisitor& visitor, EffectSettings& settings) override;
    bool VisitSettings(ConstSettingsVisitor& visitor, const EffectSettings& settings) const override;

    bool SaveSettings(const EffectSettings& settings, CommandParameters& parameters) const override;
    bool LoadSettings(const CommandParameters& parameters, EffectSettings& settings) const override;

    std::shared_ptr<::EffectInstance> MakeInstance() const override;
    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

private:
    bool requiresInputSelection() const;

    au::audacityplugin::EffectDescriptor m_descriptor;
};
} // namespace au::effects
