/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LoadLV2.h
  @brief Defines the module to handle the LV2 effect protocol

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/
#ifndef LV2EFFECTSMODULE_H
#define LV2EFFECTSMODULE_H

#include <memory>

#include "lilv/lilv.h"

#include "Callable.h"
#include "GlobalVariable.h"
#include "LV2EffectBase.h"
#include "PluginProvider.h"

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class LV2_API LV2EffectsModule final : public PluginProvider
{
public:
    struct LV2_API Factory : DefaultedGlobalHook<Factory,
                                                 Callable::UniquePtrFactory<LV2EffectBase, const LilvPlugin&>::Function
                                                 > {};

    LV2EffectsModule();
    virtual ~LV2EffectsModule();

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
    FilePath InstallPath() override { return {}; }

    void AutoRegisterPlugins(PluginManagerInterface& pm) override;
    PluginPaths FindModulePaths(PluginManagerInterface& pm) override;
    unsigned DiscoverPluginsAtPath(
        const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback)
    override;

    bool CheckPluginExist(const PluginPath& path) const override;

    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) override;

    // LV2EffectModule implementation

    std::unique_ptr<Validator> MakeValidator() const override;

private:

    static const LilvPlugin* GetPlugin(const PluginPath& path);

    //During initialization LV2 module will update LV2_PATH
    //environment variable, we need to preserve the its contents
    //on startup to avoid appended duplications
    wxString mStartupPathVar;
};

#endif
