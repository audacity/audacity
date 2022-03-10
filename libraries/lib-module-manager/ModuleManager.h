/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULEMANAGER_H__
#define __AUDACITY_MODULEMANAGER_H__

#include "MemoryX.h"
#include <functional>
#include <map>
#include <vector>
#include <wx/string.h>

#include "Identifier.h"

class wxArrayString;
class wxDynamicLibrary;
class ComponentInterface;
class PluginProvider;
class wxWindow;
using PluginID = wxString;
class TranslatableString;

//
// Module Manager
//
// wxPluginManager would be MUCH better, but it's an "undocumented" framework.
//

#include "ModuleConstants.h"

typedef int (*fnModuleDispatch)(ModuleDispatchTypes type);

class Module
{
public:
   Module(const FilePath & name);
   virtual ~Module();

   void ShowLoadFailureError(const wxString &Error);
   bool Load(wxString &deferredErrorMessage);
   void Unload();
   bool HasDispatch() { return mDispatch != NULL; };
   int Dispatch(ModuleDispatchTypes type);
   void * GetSymbol(const wxString &name);
   const FilePath &GetName() const { return mName; }

private:
   const FilePath mName;
   std::unique_ptr<wxDynamicLibrary> mLib;
   fnModuleDispatch mDispatch;
};

struct PluginProviderDeleter {
   void operator ()(PluginProvider *pInterface) const;
};

using PluginProviderHandle = std::unique_ptr<
   PluginProvider, PluginProviderDeleter
>;

typedef std::map<wxString, PluginProviderHandle> PluginProviderMap;

class MODULE_MANAGER_API ModuleManager final
{
public:

   // -------------------------------------------------------------------------
   // ModuleManager implementation
   // -------------------------------------------------------------------------

   static ModuleManager & Get();
   
   // This string persists in configuration files
   // So config compatibility will break if it is changed across Audacity versions
   static wxString GetPluginTypeString();

   static PluginID GetID(PluginProvider *provider);

private:
   static void FindModules(FilePaths &files);
   using DelayedErrors =
      std::vector< std::pair< std::unique_ptr<Module>, wxString > >;
   static void TryLoadModules(
      const FilePaths &files, FilePaths &decided, DelayedErrors &errors);

public:
   void Initialize();
   int Dispatch(ModuleDispatchTypes type);

   // PluginManager use
   // Can be called before Initialize()
   bool DiscoverProviders();

   // Supports range-for iteration
   auto Providers() const
   { return make_iterator_range(mProviders.cbegin(), mProviders.cend()); }

   bool RegisterEffectPlugin(const PluginID & provider, const PluginPath & path,
                       TranslatableString &errMsg);

   PluginProvider *CreateProviderInstance(
      const PluginID & provider, const PluginPath & path);
   std::unique_ptr<ComponentInterface>
      LoadPlugin(const PluginID & provider, const PluginPath & path);

   bool IsProviderValid(const PluginID & provider, const PluginPath & path);
   bool IsPluginValid(const PluginID & provider, const PluginPath & path, bool bFast);

private:
   // I'm a singleton class
   ModuleManager();
   ~ModuleManager();
   ModuleManager(const ModuleManager&) PROHIBITED;
   ModuleManager &operator=(const ModuleManager&) PROHIBITED;

   void InitializeBuiltins();

private:
   friend PluginProviderDeleter;
   friend std::default_delete<ModuleManager>;
   static std::unique_ptr<ModuleManager> mInstance;

   // Providers can each report availability of any number of Plug-Ins
   // identified by "paths", and are also factories of ComponentInterface
   // objects for each path
   PluginProviderMap mProviders;

   // Other libraries that receive notifications of events described by
   // ModuleDispatchTypes:
   std::vector<std::unique_ptr<Module>> mModules;
};

// ----------------------------------------------------------------------------
// The module entry point prototype (a factory of PluginProvider objects)
// ----------------------------------------------------------------------------
using PluginProviderMain = PluginProvider *(*)();

MODULE_MANAGER_API
void RegisterProvider(PluginProviderMain rtn);
MODULE_MANAGER_API
void UnregisterProvider(PluginProviderMain rtn);

// Guarantee the registry exists before any registrations, so it will
// be destroyed only after the un-registrations
static struct Init{
   Init() { RegisterProvider(nullptr); } } sInitBuiltinModules;

#endif /* __AUDACITY_MODULEMANAGER_H__ */
