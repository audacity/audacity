/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadModules.cpp

  Dominic Mazzoni
  James Crook


*******************************************************************//*!

\file LoadModules.cpp
\brief Based on LoadLadspa, this code loads pluggable Audacity
extension modules.  It also has the code to (a) invoke a script
server and (b) invoke a function returning a replacement window,
i.e. an alternative to the usual interface, for Audacity.

*//*******************************************************************/

#include <wx/dynlib.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/msgdlg.h>
#include <wx/string.h>
#include <wx/filename.h>

#include "Audacity.h"
#include "AudacityApp.h"
#include "Internat.h"

#include "commands/ScriptCommandRelay.h"
#include <NonGuiThread.h>  // header from libwidgetextra

#ifdef EXPERIMENTAL_MODULE_PREFS
#include "Prefs.h"
#endif
#include "LoadModules.h"
#include "widgets/MultiDialog.h"

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

#ifdef EXPERIMENTAL_MODULE_PREFS
bool IsAllowedModule( wxString fname )
{
   bool bLoad = false;
   wxString ShortName = wxFileName( fname ).GetName();
   if( (ShortName.CmpNoCase( wxT("mod-script-pipe")) == 0 ))
   {
      gPrefs->Read(wxT("/Module/mod-script-pipe"), &bLoad, false);
   }
   else if( (ShortName.CmpNoCase( wxT("mod-nyq-bench")) == 0 ))
   {
      gPrefs->Read(wxT("/Module/mod-nyq-bench"), &bLoad, false);
   }
   else if( (ShortName.CmpNoCase( wxT("mod-track-panel")) == 0 ))
   {
      gPrefs->Read(wxT("/Module/mod-track-panel"), &bLoad, false);
   }
   return bLoad;
}
#endif  // EXPERIMENTAL_MODULE_PREFS

Module::Module(const wxString & name)
{
   mName = name;
   mLib = new wxDynamicLibrary();
   mDispatch = NULL;
}

Module::~Module()
{
   delete mLib;
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
      wxMessageBox(wxString::Format(_("The module %s does not provide a version string.\nIt will not be loaded."), ShortName.c_str()), _("Module Unsuitable"));
      wxLogMessage(wxString::Format(_("The module %s does not provide a version string.  It will not be loaded."), mName.c_str()));
      mLib->Unload();
      return false;
   }

   wxString moduleVersion = versionFn();
   if( !moduleVersion.IsSameAs(AUDACITY_VERSION_STRING)) {
      wxString ShortName = wxFileName( mName ).GetName();
      wxMessageBox(wxString::Format(_("The module %s is matched with Audacity version %s.\n\nIt will not be loaded."), ShortName.c_str(), moduleVersion.c_str()), _("Module Unsuitable"));
      wxLogMessage(wxString::Format(_("The module %s is matched with Audacity version %s.  It will not be loaded."), mName.c_str(), moduleVersion.c_str()));
      mLib->Unload();
      return false;
   }

   mDispatch = (fnModuleDispatch) mLib->GetSymbol(wxT(ModuleDispatchName));
   if (!mDispatch) {
      // Module does not provide a dispatch function...
      return false;
   }

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
   if (mLib->IsLoaded()) {
      return mDispatch(type);
   }

   return 0;
}

void * Module::GetSymbol(wxString name)
{
   return mLib->GetSymbol(name);
}

//
// Module Manager
//
ModuleManager *ModuleManager::mInstance;

bool ModuleManager::OnInit()
{
   mInstance = this;

   return true;
}

void ModuleManager::OnExit()
{
   size_t cnt = mModules.GetCount();

   for (size_t ndx = 0; ndx < cnt; ndx++) {
      delete (Module *) mModules[ndx];
   }
   mModules.Clear();
}

void ModuleManager::Initialize(CommandHandler &cmdHandler)
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxString pathVar;
   size_t i;

   // Code from LoadLadspa that might be useful in load modules.
   pathVar = wxGetenv(wxT("AUDACITY_MODULES_PATH"));
   if (pathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   for (i = 0; i < audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("modules"),
                                         pathList);
   }

   #if defined(__WXMSW__)
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files);
   #else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);
   #endif

   for (i = 0; i < files.GetCount(); i++) {
      // As a courtesy to some modules that might be bridges to
      // open other modules, we set the current working
      // directory to be the module's directory.
      wxString saveOldCWD = ::wxGetCwd();
      wxString prefix = ::wxPathOnly(files[i]);
      ::wxSetWorkingDirectory(prefix);

#ifdef EXPERIMENTAL_MODULE_PREFS
      if( !IsAllowedModule( files[i] ) )  // don't try and check the in-date-ness before this as that means loading the module to call it's GetVersionString, which could do anything.
#endif
      {
         wxString ShortName = wxFileName( files[i] ).GetName();
         wxString msg;
         msg.Printf(_("Module \"%s\" found."), ShortName.c_str());
         msg += _("\n\nOnly use modules from trusted sources");
         const wxChar *buttons[] = {_("Yes"), _("No"), NULL};  // could add a button here for 'yes and remember that', and put it into the cfg file.  Needs more thought.
         int action;
         action = ShowMultiDialog(msg, _("Module Loader"), buttons, _("Try and load this module?"), false);
         if(action == 1)   // "No"
            continue;
      }

      Module *module = new Module(files[i]);
      if (module->Load())   // it will get rejected if there  are version problems
      {
         mInstance->mModules.Add(module);
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
      }
      else {
         delete module;
      }
      ::wxSetWorkingDirectory(saveOldCWD);
   }
   // After loading all the modules, we may have a registered scripting function.
   if(scriptFn)
   {
      ScriptCommandRelay::SetCommandHandler(cmdHandler);
      ScriptCommandRelay::SetRegScriptServerFunc(scriptFn);
      NonGuiThread::StartChild(&ScriptCommandRelay::Run);
   }
}

int ModuleManager::Dispatch(ModuleDispatchTypes type)
{
   size_t cnt = mInstance->mModules.GetCount();

   for (size_t ndx = 0; ndx < cnt; ndx++) {
      Module *module = (Module *)mInstance->mModules[ndx];

      module->Dispatch(type);
   }
   return 0;
}

IMPLEMENT_DYNAMIC_CLASS(ModuleManager, wxModule);
