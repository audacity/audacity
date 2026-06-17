/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "au3-components/EffectInterface.h"

namespace au::project {
// Minimal EffectInstanceFactory so that RealtimeEffectList::AddState succeeds
// (it requires state->GetEffect() != nullptr). No real plugin / PluginManager is
// needed: serialization only writes the plugin id, and the state is retained on
// reload regardless. All methods return trivial/empty values; MakeInstance() is
// never called in this no-playback test.
class DummyEffectInstanceFactory final : public EffectInstanceFactory
{
public:
    // ComponentInterface
    PluginPath GetPath() const override { return {}; }
    ComponentInterfaceSymbol GetSymbol() const override { return {}; }
    VendorSymbol GetVendor() const override { return {}; }
    wxString GetVersion() const override { return {}; }
    TranslatableString GetDescription() const override { return {}; }

    // EffectDefinitionInterface
    EffectType GetType() const override { return EffectTypeProcess; }
    EffectFamilySymbol GetFamily() const override { return {}; }
    bool IsInteractive() const override { return false; }
    bool IsDefault() const override { return false; }
    RealtimeSince RealtimeSupport() const override { return RealtimeSince::Always; }
    bool SupportsAutomation() const override { return false; }

    // EffectSettingsManager
    bool SaveSettings(const EffectSettings&, CommandParameters&) const override { return true; }
    bool LoadSettings(const CommandParameters&, EffectSettings&) const override { return true; }
    RegistryPaths GetFactoryPresets() const override { return {}; }
    OptionalMessage LoadUserPreset(const RegistryPath&, EffectSettings&) const override { return {}; }
    bool SaveUserPreset(const RegistryPath&, const EffectSettings&) const override { return true; }
    OptionalMessage LoadFactoryPreset(int, EffectSettings&) const override { return {}; }
    OptionalMessage LoadFactoryDefaults(EffectSettings&) const override { return {}; }

    // EffectInstanceFactory
    std::shared_ptr<EffectInstance> MakeInstance() const override { return nullptr; }
};

inline const EffectInstanceFactory& dummyFactory()
{
    static DummyEffectInstanceFactory instance;
    return instance;
}
}
