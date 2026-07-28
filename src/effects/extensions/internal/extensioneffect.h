/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include <memory>
#include <string>

#include <QJSValue>

#include "au3-effects/StatefulEffect.h"

#include "extensioneffectruntime.h"
#include "extensioneffecttypes.h"

namespace au::effects::extensions {
class ExtensionEffect;
class ExtensionEffectRun;

bool extensionBusy(const std::string& extensionId);

class ExtensionEffectInstance final : public StatefulEffect::Instance
{
public:
    ExtensionEffectInstance(ExtensionEffect& effect, double selectionDuration, const muse::modularity::ContextPtr& context);

    const std::vector<ParameterDescriptor>& parameters() const;
    const Value& value(size_t index) const;
    bool setValue(size_t index, Value value);
    bool applySettings(const EffectSettings& settings);
    void writeCurrentSettings(EffectSettings& settings) const;
    const Settings& currentSettings() const;
    double duration() const;
    bool setDuration(double seconds);
    void setLastError(std::string error);
    muse::Ret validate();
    void orphanRuntime(const QJSValue& pendingPromise);
    bool abandoned() const
    {
        return !m_runtime;
    }

    bool Init() override;
    std::string GetLastError() const override;

private:
    friend class ExtensionEffectRun;

    std::unique_ptr<ExtensionEffectRuntime> m_runtime;
    std::vector<ParameterDescriptor> m_parameters;
    Settings m_settings;
    double m_duration = 0.0;
    std::string m_lastError;
};

class ExtensionEffect final : public EffectWithSettings<Settings, StatefulEffect>
{
public:
    explicit ExtensionEffect(EffectDescriptor descriptor);

    const EffectDescriptor& descriptor() const;

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;
    ::EffectType GetType() const override;
    EffectFamilySymbol GetFamily() const override;
    bool IsDefault() const override;
    bool ParamsAreInputAgnostic() const override;
    bool VisitSettings(SettingsVisitor& visitor, EffectSettings& settings) override;
    bool VisitSettings(ConstSettingsVisitor& visitor, const EffectSettings& settings) const override;
    bool SaveSettings(const EffectSettings& settings, CommandParameters& parameters) const override;
    bool LoadSettings(const CommandParameters& parameters, EffectSettings& settings) const override;
    std::shared_ptr<::EffectInstance> MakeInstance() const override;
    bool Process(::EffectInstance& instance, ::EffectSettings& settings) override;

private:
    friend class ExtensionEffectRun;

    EffectDescriptor m_descriptor;
};
} // namespace au::effects::extensions
