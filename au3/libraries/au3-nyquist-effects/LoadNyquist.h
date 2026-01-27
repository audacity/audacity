/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.h

  Dominic Mazzoni

**********************************************************************/

#include "au3-utility/Callable.h"
#include "au3-utility/GlobalVariable.h"
#include "au3-components/PluginProvider.h"
#include "au3-components/EffectInterface.h"
#include "au3-module-manager/PluginInterface.h"
#include "NyquistBase.h"

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

class NyquistEffectsModule final : public PluginProvider
{
public:
    //! Factory for creating NyquistBase instances
    struct NYQUIST_EFFECTS_API Factory : DefaultedGlobalHook<Factory,
                                                             Callable::UniquePtrFactory<NyquistBase, const wxString&>::Function
                                                             > {};

    NyquistEffectsModule();
    virtual ~NyquistEffectsModule();

    // ComponentInterface implementation

    PluginPath GetPath() const override;
    ComponentInterfaceSymbol GetSymbol() const override;
    VendorSymbol GetVendor() const override;
    wxString GetVersion() const override;
    TranslatableString GetDescription() const override;

    // PluginProvider implementation

    bool Initialize() override;
    void Terminate() override;
    EffectFamilySymbol GetOptionalFamilySymbol() override;

    const FileExtensions& GetFileExtensions() override;
    FilePath InstallPath() override;

    void AutoRegisterPlugins(PluginManagerInterface& pm) override;
    PluginPaths FindModulePaths(PluginManagerInterface& pm) const override;
    unsigned DiscoverPluginsAtPath(
        const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback)
    override;

    bool CheckPluginExist(const PluginPath& path) const override;

    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) override;
};
