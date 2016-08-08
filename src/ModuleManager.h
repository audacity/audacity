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
#include "PluginManager.h"

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
   Module(const wxString & name);
   virtual ~Module();

   bool Load();
   void Unload();
   int Dispatch(ModuleDispatchTypes type);
   void * GetSymbol(const wxString &name);

private:
   wxString mName;
   std::unique_ptr<wxDynamicLibrary> mLib;
   fnModuleDispatch mDispatch;
};

struct ModuleInterfaceDeleter {
   void operator ()(ModuleInterface *pInterface) const;
};

using ModuleInterfaceHandle = movable_ptr_with_deleter<
   ModuleInterface, ModuleInterfaceDeleter
>;

typedef std::map<wxString, ModuleMain *> ModuleMainMap;
typedef std::map<wxString, ModuleInterfaceHandle> ModuleMap;
typedef std::map<ModuleInterface *, movable_ptr<wxDynamicLibrary>> LibraryMap;

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

   void FindAllPlugins(PluginIDList & providers, wxArrayString & paths);
   wxArrayString FindPluginsForProvider(const PluginID & provider, const wxString & path);
   bool RegisterPlugin(const PluginID & provider, const wxString & path);

   IdentInterface *CreateProviderInstance(const PluginID & provider, const wxString & path);
   IdentInterface *CreateInstance(const PluginID & provider, const wxString & path);
   void DeleteInstance(const PluginID & provider, IdentInterface *instance);

   bool IsProviderValid(const PluginID & provider, const wxString & path);
   bool IsPluginValid(const PluginID & provider, const wxString & path);

private:
   // I'm a singleton class
   ModuleManager();
   ~ModuleManager();

   void InitializeBuiltins();
   ModuleInterface *LoadModule(const wxString & path);

private:
   friend ModuleInterfaceDeleter;
   friend std::default_delete<ModuleManager>;
   static std::unique_ptr<ModuleManager> mInstance;

   ModuleMainMap mModuleMains;
   ModuleMap mDynModules;
   LibraryMap mLibs;

   std::vector<movable_ptr<Module>> mModules;
};

#endif /* __AUDACITY_MODULEMANAGER_H__ */
