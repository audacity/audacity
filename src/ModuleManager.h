/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_MODULEMANAGER_H__
#define __AUDACITY_MODULEMANAGER_H__

#include <wx/dynlib.h>

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
   void * GetSymbol(wxString name);

private:
   wxString mName;
   wxDynamicLibrary *mLib;
   fnModuleDispatch mDispatch;
};

typedef std::map<wxString, ModuleMain *> ModuleMainMap;
typedef std::map<wxString, ModuleInterface *> ModuleMap;
typedef std::map<ModuleInterface *, wxDynamicLibrary *> LibraryMap;

class ModuleManager : public ModuleManagerInterface
{
public:
   ModuleManager();
   virtual ~ModuleManager();

   // -------------------------------------------------------------------------
   // ModuleManagerInterface implementation
   // -------------------------------------------------------------------------

   virtual void RegisterModule(ModuleInterface *module);

   // -------------------------------------------------------------------------
   // ModuleManager implementation
   // -------------------------------------------------------------------------

   static ModuleManager & Get();
   static void Destroy();

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
   void InitializeBuiltins();
   ModuleInterface *LoadModule(const wxString & path);
   void UnloadModule(ModuleInterface *module);

private:
   static ModuleManager *mInstance;

   ModuleMainMap mModuleMains;
   ModuleMap mDynModules;
   LibraryMap mLibs;

   wxArrayPtrVoid mModules;
};

#endif /* __AUDACITY_MODULEMANAGER_H__ */
