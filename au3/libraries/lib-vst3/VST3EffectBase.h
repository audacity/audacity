/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3EffectBase.h

  @author Vitaly Sverchinsky

  Paul Licameli split from VST3Effect.h

  @brief Part of Audacity VST3 module

**********************************************************************/
#pragma once

#include <public.sdk/source/vst/hosting/module.h>
#include "PerTrackEffect.h"

/**
 * \brief Objects of this class connect Audacity with VST3 effects
 */
class VST3_API VST3EffectBase : public PerTrackEffect
{
    friend class VST3PluginValidator;

protected:
    // Keep strong reference to a module; this because it has to be destroyed in the destructor of this class,
    // otherwise the destruction of mEditController and mEffectComponent would trigger a memory fault.
    std::shared_ptr<VST3::Hosting::Module> mModule;
    const VST3::Hosting::ClassInfo mEffectClassInfo;

    // Mutable cache fields computed once on demand
    mutable bool mRescanFactoryPresets { true };
    mutable RegistryPaths mFactoryPresetNames;
    mutable std::vector<wxString> mFactoryPresetIDs;

public:
    static EffectFamilySymbol GetFamilySymbol();

    VST3EffectBase(
        std::shared_ptr<VST3::Hosting::Module> module, VST3::Hosting::ClassInfo effectClassInfo);

    VST3EffectBase(const VST3EffectBase&) = delete;
    VST3EffectBase(VST3EffectBase&&) = delete;
    VST3EffectBase& operator=(const VST3EffectBase&) = delete;
    VST3EffectBase& operator=(VST3EffectBase&) = delete;

    ~VST3EffectBase() override;

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    EffectType GetType() const override;
    EffectFamilySymbol GetFamily() const override;
    bool IsInteractive() const override;
    bool IsDefault() const override;
    RealtimeSince RealtimeSupport() const override;
    bool SupportsAutomation() const override;
    bool SaveSettings(
        const EffectSettings& settings, CommandParameters& parms) const override;
    bool LoadSettings(
        const CommandParameters& parms, EffectSettings& settings) const override;
    OptionalMessage LoadUserPreset(
        const RegistryPath& name, EffectSettings& settings) const override;
    bool SaveUserPreset(
        const RegistryPath& name, const EffectSettings& settings) const override;
    RegistryPaths GetFactoryPresets() const override;
    OptionalMessage LoadFactoryPreset(int id, EffectSettings& settings)
    const override;

    std::shared_ptr<EffectInstance> MakeInstance() const override;

    bool CanExportPresets() const override;

    bool HasOptions() const override;

    EffectSettings MakeSettings() const override;
    bool CopySettingsContents(const EffectSettings& src, EffectSettings& dst) const override;

    std::shared_ptr<VST3::Hosting::Module> vstModule() const { return mModule; }

protected:
    void LoadPreset(const wxString& id, EffectSettings& settings) const;
};
