/**********************************************************************

  Audacity: A Digital Audio Editor

  @file VST3EffectsModule.h

  @author Vitaly Sverchinsky

  @brief Part of Audacity VST3 module

**********************************************************************/

#pragma once

#include <unordered_map>
#include <memory>

#include "Callable.h"
#include "GlobalVariable.h"
#include "PluginProvider.h"
#include "VST3EffectBase.h"

namespace VST3 {
namespace Hosting {
class Module;
}
}

/**
 * \brief VST3Effect factory.
 */
class VST3EffectsModule final : public PluginProvider
{
    //Holds weak pointers to the unique modules which were accessed
    //through VST3EffectsModule::GetModule() during the lifetime.
    std::unordered_map<wxString, std::weak_ptr<VST3::Hosting::Module> > mModules;

    //Attempts to look up for a module, or load it from the hard drive if
    //none was found (or not valid pointers)
    std::shared_ptr<VST3::Hosting::Module> GetModule(const wxString& path);

public:
    struct VST3_API Factory : DefaultedGlobalHook<Factory,
                                                  Callable::UniquePtrFactory<VST3EffectBase,
                                                                             std::shared_ptr<VST3::Hosting::Module>,
                                                                             VST3::Hosting::ClassInfo
                                                                             >::Function
                                                  > {};

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    bool Initialize() override;
    void Terminate() override;
    EffectFamilySymbol GetOptionalFamilySymbol() override;
    const FileExtensions& GetFileExtensions() override;
    FilePath InstallPath() override;
    void AutoRegisterPlugins(PluginManagerInterface& pluginManager) override;
    bool SupportsCustomModulePaths() const override;
    PluginPaths FindModulePaths(PluginManagerInterface& pluginManager) override;
    unsigned DiscoverPluginsAtPath(const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback) override;
    bool CheckPluginExist(const PluginPath& path) const override;
    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) override;
    std::unique_ptr<Validator> MakeValidator() const override;
};
