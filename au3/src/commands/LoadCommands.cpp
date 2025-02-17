/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadCommands.cpp

  Dominic Mazzoni
  James Crook

**************************************************************************//**
\class BuiltinCommandsModule
\brief Internal module to auto register all built in commands.  It is closely
modelled on BuiltinEffectsModule
*****************************************************************************/

#include "LoadCommands.h"
#include "AudacityCommand.h"
#include "ModuleManager.h"
#include "PluginInterface.h"

#include "Prefs.h"

namespace {
bool sInitialized = false;
}

struct BuiltinCommandsModule::Entry {
    ComponentInterfaceSymbol name;
    Factory factory;

    using Entries = std::vector< Entry >;
    static Entries& Registry()
    {
        static Entries result;
        return result;
    }
};

void BuiltinCommandsModule::DoRegistration(
    const ComponentInterfaceSymbol& name, const Factory& factory)
{
    wxASSERT(!sInitialized);
    Entry::Registry().emplace_back(Entry { name, factory });
}

// ============================================================================
// Module registration entry point
//
// This is the symbol that Audacity looks for when the module is built as a
// dynamic library.
//
// When the module is builtin to Audacity, we use the same function, but it is
// declared static so as not to clash with other builtin modules.
// ============================================================================
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
    // Create and register the importer
    // Trust the module manager not to leak this
    return std::make_unique<BuiltinCommandsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(BuiltinsCommandBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinCommandsModule
//
///////////////////////////////////////////////////////////////////////////////

BuiltinCommandsModule::BuiltinCommandsModule()
{
}

BuiltinCommandsModule::~BuiltinCommandsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath BuiltinCommandsModule::GetPath() const
{
    return {};
}

ComponentInterfaceSymbol BuiltinCommandsModule::GetSymbol() const
{
    return XO("Builtin Commands");
}

VendorSymbol BuiltinCommandsModule::GetVendor() const
{
    return XO("The Audacity Team");
}

wxString BuiltinCommandsModule::GetVersion() const
{
    // This "may" be different if this were to be maintained as a separate DLL
    return AUDACITY_VERSION_STRING;
}

TranslatableString BuiltinCommandsModule::GetDescription() const
{
    return XO("Provides builtin commands to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool BuiltinCommandsModule::Initialize()
{
    for ( const auto& entry : Entry::Registry()) {
        auto path = wxString(BUILTIN_GENERIC_COMMAND_PREFIX)
                    + entry.name.Internal();
        mCommands[ path ] = &entry;
    }
    sInitialized = true;
    return true;
}

void BuiltinCommandsModule::Terminate()
{
    // Nothing to do here
    return;
}

EffectFamilySymbol BuiltinCommandsModule::GetOptionalFamilySymbol()
{
    // Commands are not enabled and disabled in EffectsPrefs
    return {};
}

const FileExtensions& BuiltinCommandsModule::GetFileExtensions()
{
    static FileExtensions empty;
    return empty;
}

void BuiltinCommandsModule::AutoRegisterPlugins(PluginManagerInterface& pm)
{
    TranslatableString ignoredErrMsg;
    for (const auto& pair : mCommands) {
        const auto& path = pair.first;
        if (!pm.IsPluginRegistered(path, &pair.second->name.Msgid())) {
            // No checking of error ?
            // Uses Generic Registration, not Default.
            // Registers as TypeGeneric, not TypeEffect.
            DiscoverPluginsAtPath(path, ignoredErrMsg,
                                  PluginManagerInterface::AudacityCommandRegistrationCallback);
        }
    }
}

PluginPaths BuiltinCommandsModule::FindModulePaths(PluginManagerInterface&)
{
    // Not really libraries
    PluginPaths names;
    for ( const auto& pair : mCommands ) {
        names.push_back(pair.first);
    }
    return names;
}

unsigned BuiltinCommandsModule::DiscoverPluginsAtPath(
    const PluginPath& path, TranslatableString& errMsg,
    const RegistrationCallback& callback)
{
    // At most one
    errMsg = {};
    auto Command = Instantiate(path);
    if (Command) {
        if (callback) {
            callback(this, Command.get());
        }
        return 1;
    }

    errMsg = XO("Unknown built-in command name");
    return 0;
}

std::unique_ptr<ComponentInterface>
BuiltinCommandsModule::LoadPlugin(const PluginPath& path)
{
    // Acquires a resource for the application.
    return Instantiate(path);
}

bool BuiltinCommandsModule::CheckPluginExist(const PluginPath& path) const
{
    return mCommands.find(path) != mCommands.end();
}

// ============================================================================
// BuiltinCommandsModule implementation
// ============================================================================

std::unique_ptr<AudacityCommand> BuiltinCommandsModule::Instantiate(const PluginPath& path)
{
    wxASSERT(path.StartsWith(BUILTIN_GENERIC_COMMAND_PREFIX));
    auto iter = mCommands.find(path);
    if (iter != mCommands.end()) {
        return iter->second->factory();
    }

    wxASSERT(false);
    return nullptr;
}
