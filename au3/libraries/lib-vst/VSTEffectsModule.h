/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffectsModule.h

  Dominic Mazzoni

  Paul Licameli split from VSTEffect.h

**********************************************************************/

#ifndef __AUDACITY_VST_EFFECTS_MODULE__
#define __AUDACITY_VST_EFFECTS_MODULE__

#include "Callable.h"
#include "GlobalVariable.h"
#include "PluginProvider.h"
#include "VSTEffectBase.h"

class VST_API VSTEffectsModule final : public PluginProvider
{
public:
public:
    struct VST_API Factory : DefaultedGlobalHook<Factory,
                                                 Callable::UniquePtrFactory<VSTEffectBase, const PluginPath&>::Function
                                                 > {};

    VSTEffectsModule();
    virtual ~VSTEffectsModule();

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
};

#endif
