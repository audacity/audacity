/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULEMANAGER_H__
#define __AUDACITY_MODULEMANAGER_H__

#include "MemoryX.h"
#include <map>
#include <vector>

#include "audacity/Types.h"

class wxArrayString;
class wxDynamicLibrary;
class CommandHandler;
class ComponentInterface;
class ModuleInterface;

wxWindow *  MakeHijackPanel();

//
// Module Manager
//
// wxPluginManager would be MUCH better, but it's an "undocumented" framework.
//
#define ModuleDispatchName "ModuleDispatch"

typedef enum
{
   ModuleInitialize,
   ModuleTerminate,
   AppInitialized,
   AppQuiting,
   ProjectInitialized,
   ProjectClosing
} ModuleDispatchTypes;

typedef int (*fnModuleDispatch)(ModuleDispatchTypes type);

class Module
{
public:
   Module(const FilePath & name);
   virtual ~Module();

   bool Load();
   void Unload();
   bool HasDispatch() { return mDispatch != NULL; };
   int Dispatch(ModuleDispatchTypes type);
   void * GetSymbol(const wxString &name);

private:
   FilePath mName;
   std::unique_ptr<wxDynamicLibrary> mLib;
   fnModuleDispatch mDispatch;
};

struct ModuleInterfaceDeleter {
   void operator ()(ModuleInterface *pInterface) const;
};

using ModuleInterfaceHandle = std::unique_ptr<
   ModuleInterface, ModuleInterfaceDeleter
>;

typedef std::map<wxString, ModuleInterfaceHandle> ModuleMap;
typedef std::map<ModuleInterface *, std::unique_ptr<wxDynamicLibrary>> LibraryMap;
using PluginIDs = wxArrayString;

class ModuleManager final
{
public:

   // -------------------------------------------------------------------------
   // ModuleManager implementation
   // -------------------------------------------------------------------------

   static ModuleManager & Get();

   void Initialize(CommandHandler & cmdHandler);
   int Dispatch(ModuleDispatchTypes type);

   // PluginManager use
   // Can be called before Initialize()
   bool DiscoverProviders();

   PluginPaths FindPluginsForProvider(const PluginID & provider, const PluginPath & path);
   bool RegisterEffectPlugin(const PluginID & provider, const PluginPath & path,
                       TranslatableString &errMsg);

   ModuleInterface *CreateProviderInstance(const PluginID & provider, const PluginPath & path);
   ComponentInterface *CreateInstance(const PluginID & provider, const PluginPath & path);
   void DeleteInstance(const PluginID & provider, ComponentInterface *instance);

   bool IsProviderValid(const PluginID & provider, const PluginPath & path);
   bool IsPluginValid(const PluginID & provider, const PluginPath & path, bool bFast);

private:
   // I'm a singleton class
   ModuleManager();
   ~ModuleManager();

   void InitializeBuiltins();
   ModuleInterface *LoadModule(const PluginPath & path);

private:
   friend ModuleInterfaceDeleter;
   friend std::default_delete<ModuleManager>;
   static std::unique_ptr<ModuleManager> mInstance;

   // Module objects, also called Providers, can each report availability of any
   // number of Plug-Ins identified by "paths", and are also factories of
   // ComponentInterface objects for each path:
   ModuleMap mDynModules;

   // Dynamically loaded libraries, each one a factory that makes one of the
   // (non-built-in) providers:
   LibraryMap mLibs;

   // Other libraries that receive notifications of events described by
   // ModuleDispatchTypes:
   std::vector<std::unique_ptr<Module>> mModules;
};

#endif /* __AUDACITY_MODULEMANAGER_H__ */
