/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULEMANAGER_H__
#define __AUDACITY_MODULEMANAGER_H__

#include <wx/dynlib.h>

#include "MemoryX.h"
#include <map>
#include <vector>

#include "audacity/ModuleInterface.h"

class wxArrayString;
class CommandHandler;

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
   ProjectClosing,
   MenusRebuilt
} ModuleDispatchTypes;

typedef int (*fnModuleDispatch)(ModuleDispatchTypes type);

class Module
{
public:
   Module(const FilePath & name);
   virtual ~Module();

   bool Load();
   void Unload();
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

typedef std::map<wxString, ModuleMain *> ModuleMainMap;
typedef std::map<wxString, ModuleInterfaceHandle> ModuleMap;
typedef std::map<ModuleInterface *, std::unique_ptr<wxDynamicLibrary>> LibraryMap;
using PluginIDs = wxArrayString;

class ModuleManager final : public ModuleManagerInterface
{
public:
   // -------------------------------------------------------------------------
   // ModuleManagerInterface implementation
   // -------------------------------------------------------------------------

   void RegisterModule(ModuleInterface *module) override;

   // -------------------------------------------------------------------------
   // ModuleManager implementation
   // -------------------------------------------------------------------------

   static ModuleManager & Get();

   void Initialize(CommandHandler & cmdHandler);
   int Dispatch(ModuleDispatchTypes type);

   // PluginManager use
   bool DiscoverProviders();

   // Seems we don't currently use FindAllPlugins
   void FindAllPlugins(PluginIDs & providers, PluginPaths & paths);

   PluginPaths FindPluginsForProvider(const PluginID & provider, const PluginPath & path);
   bool RegisterEffectPlugin(const PluginID & provider, const PluginPath & path,
                       wxString &errMsg);

   ComponentInterface *CreateProviderInstance(const PluginID & provider, const PluginPath & path);
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

   ModuleMainMap mModuleMains;
   ModuleMap mDynModules;
   LibraryMap mLibs;

   std::vector<std::unique_ptr<Module>> mModules;
};

#endif /* __AUDACITY_MODULEMANAGER_H__ */
