/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**************************************************************************//**
\class BuiltinEffectsModule
\brief Internal module to auto register all built in effects.  
*****************************************************************************/


#include "LoadEffects.h"

#include "Prefs.h"

#include "Effect.h"
#include "ModuleManager.h"
#include "PluginManager.h"

static bool sInitialized = false;

struct BuiltinEffectsModule::Entry {
   ComponentInterfaceSymbol name;
   BuiltinEffectsModule::Factory factory;
   bool excluded;

   using Entries = std::vector< Entry >;
   static Entries &Registry()
   {
      static Entries result;
      return result;
   }
};

void BuiltinEffectsModule::DoRegistration(
   const ComponentInterfaceSymbol &name, const Factory &factory, bool excluded )
{
   wxASSERT( !sInitialized );
   Entry::Registry().emplace_back( Entry{ name, factory, excluded } );
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
   return safenew BuiltinEffectsModule();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(BuiltinsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

BuiltinEffectsModule::BuiltinEffectsModule()
{
}

BuiltinEffectsModule::~BuiltinEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath BuiltinEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol BuiltinEffectsModule::GetSymbol() const
{
   return XO("Builtin Effects");
}

VendorSymbol BuiltinEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString BuiltinEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

TranslatableString BuiltinEffectsModule::GetDescription() const
{
   return XO("Provides builtin effects to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool BuiltinEffectsModule::Initialize()
{
   for ( const auto &entry : Entry::Registry() ) {
      auto path = wxString(BUILTIN_EFFECT_PREFIX) + entry.name.Internal();
      mEffects[ path ] = &entry;
   }
   sInitialized = true;
   return true;
}

void BuiltinEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol BuiltinEffectsModule::GetOptionalFamilySymbol()
{
   // Returns empty, because there should not be an option in Preferences to
   // disable the built-in effects.
   return {};
}

const FileExtensions &BuiltinEffectsModule::GetFileExtensions()
{
   static FileExtensions empty;
   return empty;
}

void BuiltinEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Assume initial PluginManager::Save is not yet done

   // The set of built-in functions that are realtime capable may differ with
   // the plugin registry version
   bool rediscoverAll = !Regver_eq(pm.GetRegistryVersion(), REGVERCUR);

   TranslatableString ignoredErrMsg;
   for (const auto &pair : mEffects) {
      const auto &path = pair.first;
      if (rediscoverAll ||
         !pm.IsPluginRegistered(path, &pair.second->name.Msgid())
      ){
         if ( pair.second->excluded )
            continue;
         // No checking of error ?
         DiscoverPluginsAtPath(path, ignoredErrMsg,
            PluginManagerInterface::DefaultRegistrationCallback);
      }
   }
}

PluginPaths BuiltinEffectsModule::FindModulePaths(PluginManagerInterface &)
{
   // Not really libraries
   PluginPaths names;
   for ( const auto &pair : mEffects )
      names.push_back( pair.first );
   return names;
}

unsigned BuiltinEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   // At most one
   errMsg = {};
   auto effect = Instantiate(path);
   if (effect)
   {
      if (callback)
         callback(this, effect.get());
      return 1;
   }

   errMsg = XO("Unknown built-in effect name");
   return 0;
}

bool BuiltinEffectsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   // bFast is unused as checking in the list is fast.
   static_cast<void>(bFast);
   return mEffects.find( path ) != mEffects.end();
}

std::unique_ptr<ComponentInterface>
BuiltinEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   return Instantiate(path);
}

// ============================================================================
// BuiltinEffectsModule implementation
// ============================================================================

std::unique_ptr<Effect> BuiltinEffectsModule::Instantiate(const PluginPath & path)
{
   wxASSERT(path.StartsWith(BUILTIN_EFFECT_PREFIX));
   auto iter = mEffects.find( path );
   if ( iter != mEffects.end() )
      return iter->second->factory();
 
   wxASSERT( false );
   return nullptr;
}
