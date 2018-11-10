/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleManager.cpp

  Dominic Mazzoni
  James Crook


*******************************************************************//*!

\file ModuleManager.cpp
\brief Based on LoadLadspa, this code loads pluggable Audacity 
extension modules.  It also has the code to (a) invoke a script
server and (b) invoke a function returning a replacement window,
i.e. an alternative to the usual interface, for Audacity.

*//*******************************************************************/

#include "Audacity.h"
#include "ModuleManager.h"

#include "Experimental.h"

#include <wx/dynlib.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>
#include <wx/filename.h>

#include "AudacityApp.h"
#include "FileNames.h"
#include "Internat.h"
#include "PluginManager.h"

#include "commands/ScriptCommandRelay.h"
#include <NonGuiThread.h>  // header from libwidgetextra

#include "audacity/PluginInterface.h"

#ifdef EXPERIMENTAL_MODULE_PREFS
#include "Prefs.h"
#include "./prefs/ModulePrefs.h"
#endif

#include "widgets/MultiDialog.h"

#include "widgets/ErrorDialog.h"

#define initFnName      "ExtensionModuleInit"
#define versionFnName   "GetVersionString"
#define scriptFnName    "RegScriptServerFunc"
#define mainPanelFnName "MainPanelFunc"

typedef wxWindow * pwxWindow;
typedef int (*tModuleInit)(int);
//typedef wxString (*tVersionFn)();
typedef wxChar * (*tVersionFn)();
typedef pwxWindow (*tPanelFn)(int);

// This variable will hold the address of a subroutine in 
// a DLL that can hijack the normal panel.
static tPanelFn pPanelHijack=NULL;

// Next two commented out lines are handy when investigating
// strange DLL behaviour.  Instead of dynamic linking,
// link the library which has the replacement panel statically.
// Give the address of the routine here.
// This is a great help in identifying missing 
// symbols which otherwise cause a dll to unload after loading
// without an explanation as to why!
//extern wxWindow * MainPanelFunc( int i );
//tPanelFn pPanelHijack=&MainPanelFunc;

/// IF pPanelHijack has been found in a module DLL
/// THEN when this function is called we'll go and
/// create that window instead of the normal one.
wxWindow * MakeHijackPanel()
{
   if( pPanelHijack == NULL )
      return NULL;
   return pPanelHijack(0);
}

// This variable will hold the address of a subroutine in a DLL that
// starts a thread and reads script commands.
static tpRegScriptServerFunc scriptFn;

Module::Module(const FilePath & name)
{
   mName = name;
   mLib = std::make_unique<wxDynamicLibrary>();
   mDispatch = NULL;
}

Module::~Module()
{
}

bool Module::Load()
{
   if (mLib->IsLoaded()) {
      if (mDispatch) {
         return true;
      }
      return false;
   }

   if (!mLib->Load(mName, wxDL_LAZY)) {
      return false;
   }

   // Check version string matches.  (For now, they must match exactly)
   tVersionFn versionFn = (tVersionFn)(mLib->GetSymbol(wxT(versionFnName)));
   if (versionFn == NULL){
      wxString ShortName = wxFileName( mName ).GetName();
      AudacityMessageBox(wxString::Format(_("The module %s does not provide a version string.\nIt will not be loaded."), ShortName), _("Module Unsuitable"));
      wxLogMessage(wxString::Format(_("The module %s does not provide a version string. It will not be loaded."), mName));
      mLib->Unload();
      return false;
   }

   wxString moduleVersion = versionFn();
   if( moduleVersion != AUDACITY_VERSION_STRING) {
      wxString ShortName = wxFileName( mName ).GetName();
      AudacityMessageBox(wxString::Format(_("The module %s is matched with Audacity version %s.\n\nIt will not be loaded."), ShortName, moduleVersion), _("Module Unsuitable"));
      wxLogMessage(wxString::Format(_("The module %s is matched with Audacity version %s. It will not be loaded."), mName, moduleVersion));
      mLib->Unload();
      return false;
   }

   mDispatch = (fnModuleDispatch) mLib->GetSymbol(wxT(ModuleDispatchName));
   if (!mDispatch) {
      // Module does not provide a dispatch function...
      // That can be OK, as long as we never try to call it.
      return true;
   }

   // However if we do have it and it does not work, 
   // then the module is bad.
   bool res = ((mDispatch(ModuleInitialize))!=0);
   if (res) {
      return true;
   }

   mDispatch = NULL;
   return false;
}

void Module::Unload()
{
   if (mLib->IsLoaded()) {
      mDispatch(ModuleTerminate);
   }

   mLib->Unload();
}

int Module::Dispatch(ModuleDispatchTypes type)
{
   if (mLib->IsLoaded())
      if( mDispatch != NULL )
         return mDispatch(type);

   return 0;
}

void * Module::GetSymbol(const wxString &name)
{
   return mLib->GetSymbol(name);
}

// ============================================================================
//
// ModuleManager
//
// ============================================================================

// The one and only ModuleManager
std::unique_ptr<ModuleManager> ModuleManager::mInstance{};

// Provide builtin modules a means to identify themselves
using BuiltinModuleList = std::vector<ModuleMain>;
namespace {
   BuiltinModuleList &builtinModuleList()
   {
      static BuiltinModuleList theList;
      return theList;
   }
}

void RegisterBuiltinModule(ModuleMain moduleMain)
{
   builtinModuleList().push_back(moduleMain);

   return;
}

// ----------------------------------------------------------------------------
// Creation/Destruction
// ----------------------------------------------------------------------------

ModuleManager::ModuleManager()
{
}

ModuleManager::~ModuleManager()
{
   mDynModules.clear();
   builtinModuleList().clear();
}

// static 
void ModuleManager::Initialize(CommandHandler &cmdHandler)
{
   const auto &audacityPathList = wxGetApp().audacityPathList;
   FilePaths pathList;
   FilePaths files;
   wxString pathVar;
   size_t i;

   // Code from LoadLadspa that might be useful in load modules.
   pathVar = wxGetenv(wxT("AUDACITY_MODULES_PATH"));
   if (!pathVar.empty())
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   for (i = 0; i < audacityPathList.size(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("modules"),
                                         pathList);
   }

   #if defined(__WXMSW__)
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files);
   #else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);
   #endif

   wxString saveOldCWD = ::wxGetCwd();
   for (i = 0; i < files.size(); i++) {
      // As a courtesy to some modules that might be bridges to
      // open other modules, we set the current working
      // directory to be the module's directory.
      auto prefix = ::wxPathOnly(files[i]);
      ::wxSetWorkingDirectory(prefix);

#ifdef EXPERIMENTAL_MODULE_PREFS
      int iModuleStatus = ModulePrefs::GetModuleStatus( files[i] );
      if( iModuleStatus == kModuleDisabled )
         continue;
      if( iModuleStatus == kModuleFailed )
         continue;
      // New module?  You have to go and explicitly enable it.
      if( iModuleStatus == kModuleNew ){
         // To ensure it is noted in config file and so
         // appears on modules page.
         ModulePrefs::SetModuleStatus( files[i], kModuleNew);
         continue;
      }

      if( iModuleStatus == kModuleAsk )
#endif
      // JKC: I don't like prompting for the plug-ins individually
      // I think it would be better to show the module prefs page,
      // and let the user decide for each one.
      {
         wxString ShortName = wxFileName( files[i] ).GetName();
         wxString msg;
         msg.Printf(_("Module \"%s\" found."), ShortName);
         msg += _("\n\nOnly use modules from trusted sources");
         const wxChar *buttons[] = {_("Yes"), _("No"), NULL};  // could add a button here for 'yes and remember that', and put it into the cfg file.  Needs more thought.
         int action;
         action = ShowMultiDialog(msg, _("Audacity Module Loader"), buttons, _("Try and load this module?"), false);
#ifdef EXPERIMENTAL_MODULE_PREFS
         // If we're not prompting always, accept the answer permanantly
         if( iModuleStatus == kModuleNew ){
            iModuleStatus = (action==1)?kModuleDisabled : kModuleEnabled;
            ModulePrefs::SetModuleStatus( files[i], iModuleStatus );
         }
#endif
         if(action == 1){   // "No"
            continue;
         }
      }
#ifdef EXPERIMENTAL_MODULE_PREFS
      // Before attempting to load, we set the state to bad.
      // That way, if we crash, we won't try again.
      ModulePrefs::SetModuleStatus( files[i], kModuleFailed );
#endif

      auto umodule = std::make_unique<Module>(files[i]);
      if (umodule->Load())   // it will get rejected if there  are version problems
      {
         auto module = umodule.get();
         Get().mModules.push_back(std::move(umodule));
         // We've loaded and initialised OK.
         // So look for special case functions:
         wxLogNull logNo; // Don't show wxWidgets errors if we can't do these. (Was: Fix bug 544.)
         // (a) for scripting.
         if( scriptFn == NULL )
            scriptFn = (tpRegScriptServerFunc)(module->GetSymbol(wxT(scriptFnName)));
         // (b) for hijacking the entire Audacity panel.
         if( pPanelHijack==NULL )
         {
            pPanelHijack = (tPanelFn)(module->GetSymbol(wxT(mainPanelFnName)));
         }
#ifdef EXPERIMENTAL_MODULE_PREFS
         // Loaded successfully, restore the status.
         ModulePrefs::SetModuleStatus( files[i], iModuleStatus);
#endif
      }
   }
   ::wxSetWorkingDirectory(saveOldCWD);

   // After loading all the modules, we may have a registered scripting function.
   if(scriptFn)
   {
      ScriptCommandRelay::SetCommandHandler(cmdHandler);
      ScriptCommandRelay::SetRegScriptServerFunc(scriptFn);
      NonGuiThread::StartChild(&ScriptCommandRelay::Run);
   }
}

// static
int ModuleManager::Dispatch(ModuleDispatchTypes type)
{
   for (const auto &module: mModules) {
      module->Dispatch(type);
   }
   return 0;
}

// ============================================================================
//
// Return reference to singleton
//
// (Thread-safe...no active threading during construction or after destruction)
// ============================================================================
ModuleManager & ModuleManager::Get()
{
   if (!mInstance)
   {
      mInstance.reset(safenew ModuleManager);
   }

   return *mInstance;
}

bool ModuleManager::DiscoverProviders()
{
   InitializeBuiltins();

   FilePaths provList;
   FilePaths pathList;

   // Code from LoadLadspa that might be useful in load modules.
   wxString pathVar = wxString::FromUTF8(getenv("AUDACITY_MODULES_PATH"));

   if (!pathVar.empty())
   {
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);
   }
   else
   {
      wxGetApp().AddUniquePathToPathList(FileNames::ModulesDir(), pathList);
   }

#if defined(__WXMSW__)
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, provList);
#elif defined(__WXMAC__)
   wxGetApp().FindFilesInPathList(wxT("*.dylib"), pathList, provList);
#else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, provList);
#endif

   PluginManager & pm = PluginManager::Get();

   for (int i = 0, cnt = provList.size(); i < cnt; i++)
   {
      ModuleInterface *module = LoadModule(provList[i]);
      if (module)
      {
         // Register the provider
         pm.RegisterPlugin(module);

         // Now, allow the module to auto-register children
         module->AutoRegisterPlugins(pm);
      }
   }

   return true;
}

void ModuleManager::InitializeBuiltins()
{
   PluginManager & pm = PluginManager::Get();

   for (auto moduleMain : builtinModuleList())
   {
      ModuleInterfaceHandle module {
         moduleMain(this, NULL), ModuleInterfaceDeleter{}
      };

      if (module->Initialize())
      {
         // Register the provider
         ModuleInterface *pInterface = module.get();
         const PluginID & id = pm.RegisterPlugin(pInterface);

         // Need to remember it 
         mDynModules[id] = std::move(module);

         // Allow the module to auto-register children
         pInterface->AutoRegisterPlugins(pm);
      }
      else
      {
         // Don't leak!  Destructor of module does that.
      }
   }
}

ModuleInterface *ModuleManager::LoadModule(const PluginPath & path)
{
   auto lib = std::make_unique<wxDynamicLibrary>();

   if (lib->Load(path, wxDL_NOW))
   {
      bool success = false;
      ModuleMain audacityMain = (ModuleMain) lib->GetSymbol(wxSTRINGIZE_T(MODULE_ENTRY),
                                                            &success);
      if (success && audacityMain)
      {
         ModuleInterfaceHandle handle {
            audacityMain(this, &path), ModuleInterfaceDeleter{}
         };
         if (handle)
         {
            if (handle->Initialize())
            {

               auto module = handle.get();
               mDynModules[PluginManager::GetID(module)] = std::move(handle);
               mLibs[module] = std::move(lib);

               return module;
            }
         }
      }

      lib->Unload();
   }

   return NULL;
}

void ModuleInterfaceDeleter::operator() (ModuleInterface *pInterface) const
{
   if (pInterface)
   {
      pInterface->Terminate();

      auto &libs = ModuleManager::Get().mLibs;

      auto iter = libs.find(pInterface);
      if (iter != libs.end())
         libs.erase(iter); // This causes unloading in ~wxDynamicLibrary

      std::unique_ptr < ModuleInterface > { pInterface }; // DELETE it
   }
}

void ModuleManager::RegisterModule(ModuleInterface *inModule)
{
   std::unique_ptr<ModuleInterface> module{ inModule };

   PluginID id = PluginManager::GetID(module.get());

   if (mDynModules.find(id) != mDynModules.end())
   {
      // TODO:  Should we complain about a duplicate registeration????
      // PRL:  Don't leak resources!
      module->Terminate();
      return;
   }

   mDynModules[id] = ModuleInterfaceHandle {
      module.release(), ModuleInterfaceDeleter{}
   };

   PluginManager::Get().RegisterPlugin(inModule);
}

void ModuleManager::FindAllPlugins(PluginIDs & providers, PluginPaths & paths)
{
   PluginManager & pm = PluginManager::Get();

   wxArrayString modIDs;
   PluginPaths modPaths;
   const PluginDescriptor *plug = pm.GetFirstPlugin(PluginTypeModule);
   while (plug)
   {
      modIDs.push_back(plug->GetID());
      modPaths.push_back(plug->GetPath());
      plug = pm.GetNextPlugin(PluginTypeModule);
   }

   for (size_t i = 0, cntIds = modIDs.size(); i < cntIds; i++)
   {
      PluginID providerID = modIDs[i];

      ModuleInterface *module =
         static_cast<ModuleInterface *>(CreateProviderInstance(providerID, modPaths[i]));

      if (!module)
         continue;

      auto newpaths = module->FindPluginPaths(pm);
      for (size_t j = 0, cntPaths = newpaths.size(); j < cntPaths; j++)
      {
         providers.push_back(providerID);
         paths.push_back(newpaths[j]);
      }
   }
}

PluginPaths ModuleManager::FindPluginsForProvider(const PluginID & providerID,
                                                    const PluginPath & path)
{
   // Instantiate if it hasn't already been done
   if (mDynModules.find(providerID) == mDynModules.end())
   {
      // If it couldn't be created, just give up and return an empty list
      if (!CreateProviderInstance(providerID, path))
      {
         return {};
      }
   }

   return mDynModules[providerID]->FindPluginPaths(PluginManager::Get());
}

bool ModuleManager::RegisterEffectPlugin(const PluginID & providerID, const PluginPath & path, wxString &errMsg)
{
   errMsg.clear();
   if (mDynModules.find(providerID) == mDynModules.end())
   {
      return false;
   }

   auto nFound = mDynModules[providerID]->DiscoverPluginsAtPath(path, errMsg, PluginManagerInterface::DefaultRegistrationCallback);

   return nFound > 0;
}

ComponentInterface *ModuleManager::CreateProviderInstance(const PluginID & providerID,
                                                      const PluginPath & path)
{
   if (path.empty() && mDynModules.find(providerID) != mDynModules.end())
   {
      return mDynModules[providerID].get();
   }

   return LoadModule(path);
}

ComponentInterface *ModuleManager::CreateInstance(const PluginID & providerID,
                                              const PluginPath & path)
{
   if (mDynModules.find(providerID) == mDynModules.end())
   {
      return NULL;
   }

   return mDynModules[providerID]->CreateInstance(path);
}

void ModuleManager::DeleteInstance(const PluginID & providerID,
                                   ComponentInterface *instance)
{
   if (mDynModules.find(providerID) == mDynModules.end())
   {
      return;
   }

   mDynModules[providerID]->DeleteInstance(instance);
}

bool ModuleManager::IsProviderValid(const PluginID & WXUNUSED(providerID),
                                    const PluginPath & path)
{
   // Builtin modules do not have a path
   if (path.empty())
   {
      return true;  
   }

   wxFileName lib(path);
   if (lib.FileExists() || lib.DirExists())
   {
      return true;
   }

   return false;
}

bool ModuleManager::IsPluginValid(const PluginID & providerID,
                                  const PluginPath & path,
                                  bool bFast)
{
   if (mDynModules.find(providerID) == mDynModules.end())
   {
      return false;
   }

   return mDynModules[providerID]->IsPluginValid(path, bFast);
}

