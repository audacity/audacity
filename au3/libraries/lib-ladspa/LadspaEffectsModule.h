/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffectsModule.h

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.h

**********************************************************************/
#ifndef __AUDACITY_LADSPA_EFFECTS_MODULE__
#define __AUDACITY_LADSPA_EFFECTS_MODULE__

#include "Callable.h"
#include "GlobalVariable.h"
#include "LadspaEffectBase.h"
#include "PluginProvider.h"

class LadspaEffectsModule final : public PluginProvider
{
public:
    struct LADSPA_API Factory : DefaultedGlobalHook<Factory,
                                                    Callable::UniquePtrFactory<LadspaEffectBase, const wxString&, int>
                                                    ::Function
                                                    > {};

    LadspaEffectsModule();
    virtual ~LadspaEffectsModule();

    // ComponentInterface implementation

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    // PluginProvider implementation

    bool Initialize() override;
    void Terminate() override;
    bool SupportsCustomModulePaths() const override;
    EffectFamilySymbol GetOptionalFamilySymbol() override;

    const FileExtensions& GetFileExtensions() override;
    FilePath InstallPath() override;

    void AutoRegisterPlugins(PluginManagerInterface& pm) override;
    PluginPaths FindModulePaths(PluginManagerInterface& pm) override;
    unsigned DiscoverPluginsAtPath(
        const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback)
    override;

    bool CheckPluginExist(const PluginPath& path) const override;

    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) override;

    // LadspaEffectModule implementation

    FilePaths GetSearchPaths(PluginManagerInterface& pluginManager);
};

#endif
