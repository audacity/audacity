/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadCommands.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_LOAD_COMMANDS__
#define __AUDACITY_LOAD_COMMANDS__

#include "PluginProvider.h"

#include <functional>
#include <memory>
#include <unordered_map>
#include <memory>

class AudacityCommand;

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinCommandsModule
//
///////////////////////////////////////////////////////////////////////////////

class AUDACITY_DLL_API BuiltinCommandsModule final : public PluginProvider
{
public:
    BuiltinCommandsModule();
    virtual ~BuiltinCommandsModule();

    using Factory = std::function< std::unique_ptr<AudacityCommand>() >;

    // Typically you make a static object of this type in the .cpp file that
    // also implements the Command subclass.
    template< typename Subclass >
    struct Registration final {
        Registration()
        {
            DoRegistration(
                Subclass::Symbol, []{ return std::make_unique< Subclass >(); });
        }
    };

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
    FilePath InstallPath() override { return {}; }

    void AutoRegisterPlugins(PluginManagerInterface& pm) override;
    PluginPaths FindModulePaths(PluginManagerInterface& pm) override;
    unsigned DiscoverPluginsAtPath(
        const PluginPath& path, TranslatableString& errMsg, const RegistrationCallback& callback)
    override;

    bool CheckPluginExist(const PluginPath& path) const override;

    std::unique_ptr<ComponentInterface>
    LoadPlugin(const PluginPath& path) override;

private:
    // BuiltinEffectModule implementation

    std::unique_ptr<AudacityCommand> Instantiate(const PluginPath& path);

private:
    struct Entry;

    static void DoRegistration(
        const ComponentInterfaceSymbol& name, const Factory& factory);

    using CommandHash = std::unordered_map< wxString, const Entry* >;
    CommandHash mCommands;
};

#endif
