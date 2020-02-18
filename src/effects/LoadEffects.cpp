/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadEffects.cpp

  Dominic Mazzoni

**************************************************************************//**
\class BuiltinEffectsModule
\brief Internal module to auto register all built in effects.  
*****************************************************************************/

#include "../Audacity.h" // for USE_* macros
#include "LoadEffects.h"

#include "../Prefs.h"

#include "Effect.h"

static bool sInitialized = false;

struct BuiltinEffectsModule::Entry {
   wxString name;
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
   Entry::Registry().emplace_back( Entry{ name.Internal(), factory, excluded } );
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
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew BuiltinEffectsModule(path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(BuiltinsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// BuiltinEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

BuiltinEffectsModule::BuiltinEffectsModule(const wxString *path)
{
   if (path)
   {
      mPath = *path;
   }
}

BuiltinEffectsModule::~BuiltinEffectsModule()
{
   mPath.clear();
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath BuiltinEffectsModule::GetPath()
{
   return mPath;
}

ComponentInterfaceSymbol BuiltinEffectsModule::GetSymbol()
{
   return XO("Builtin Effects");
}

VendorSymbol BuiltinEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString BuiltinEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return AUDACITY_VERSION_STRING;
}

TranslatableString BuiltinEffectsModule::GetDescription()
{
   return XO("Provides builtin effects to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool BuiltinEffectsModule::Initialize()
{
   for ( const auto &entry : Entry::Registry() ) {
      auto path = wxString(BUILTIN_EFFECT_PREFIX) + entry.name;
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

bool BuiltinEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   TranslatableString ignoredErrMsg;
   for (const auto &pair : mEffects)
   {
      if ( pair.second->excluded )
         continue;
      const auto &path = pair.first;
      if (!pm.IsPluginRegistered(path))
      {
         // No checking of error ?
         DiscoverPluginsAtPath(path, ignoredErrMsg,
            PluginManagerInterface::DefaultRegistrationCallback);
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

PluginPaths BuiltinEffectsModule::FindPluginPaths(PluginManagerInterface & WXUNUSED(pm))
{
   PluginPaths names;
   for ( const auto &pair : mEffects )
      names.push_back( pair.first );
   return names;
}

unsigned BuiltinEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
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

ComponentInterface *BuiltinEffectsModule::CreateInstance(const PluginPath & path)
{
   // Acquires a resource for the application.
   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return Instantiate(path).release();
}

void BuiltinEffectsModule::DeleteInstance(ComponentInterface *instance)
{
   // Releases the resource.
   std::unique_ptr < Effect > {
      dynamic_cast<Effect *>(instance)
   };
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
