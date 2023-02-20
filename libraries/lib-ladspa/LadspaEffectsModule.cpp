/**********************************************************************

  Audacity: A Digital Audio Editor

  LadspaEffectsModule.cpp

  Dominic Mazzoni

  Paul Licameli split from LadspaEffect.cpp

  This class implements a LADSPA Plug-in effect.

*//*******************************************************************/
#include "LadspaEffectsModule.h"
#include "LadspaEffectBase.h"

#include "ConfigInterface.h"

#if !defined(__WXMSW__)
#include <dlfcn.h>

#ifndef RTLD_DEEPBIND
#define RTLD_DEEPBIND 0
#endif
#endif

#include <chrono>
#include <thread>
#include <wx/log.h>
#include <wx/tokenzr.h>
#include <wx/utils.h>
#include "FileNames.h"
#include "ModuleManager.h"

// ============================================================================
// List of effects that ship with Audacity.  These will be autoregistered.
// ============================================================================
const static wxChar *kShippedEffects[] =
{
   wxT("sc4_1882.dll"),
};

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
   return std::make_unique<LadspaEffectsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(LadspaBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// LadspaEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

LadspaEffectsModule::LadspaEffectsModule()
{
}

LadspaEffectsModule::~LadspaEffectsModule()
{
}

PluginPath LadspaEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol LadspaEffectsModule::GetSymbol() const
{
   /* i18n-hint: abbreviates "Linux Audio Developer's Simple Plugin API"
      (Application programming interface)
    */
   return XO("LADSPA Effects");
}

VendorSymbol LadspaEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString LadspaEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LADSPAEFFECTS_VERSION;
}

TranslatableString LadspaEffectsModule::GetDescription() const
{
   return XO("Provides LADSPA Effects");
}

bool LadspaEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void LadspaEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

EffectFamilySymbol LadspaEffectsModule::GetOptionalFamilySymbol()
{
#if USE_LADSPA
   return LADSPAEFFECTS_FAMILY;
#else
   return {};
#endif
}

const FileExtensions &LadspaEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{

#ifdef __WXMSW__

      _T("dll")

#else

      _T("so")

   #ifdef __WXMAC__
   // Is it correct that these are candidate plug-in files too for macOs?
      , _T("dylib")
   #endif

#endif

   }};
   return result;
}

FilePath LadspaEffectsModule::InstallPath()
{
   // To do: better choice
   return FileNames::PlugInDir();
}

void LadspaEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Autoregister effects that we "think" are ones that have been shipped with
   // Audacity.  A little simplistic, but it should suffice for now.
   auto pathList = GetSearchPaths();
   FilePaths files;
   TranslatableString ignoredErrMsg;

   for (int i = 0; i < (int)WXSIZEOF(kShippedEffects); i++)
   {
      files.clear();
      pm.FindFilesInPathList(kShippedEffects[i], pathList, files);
      for (size_t j = 0, cnt = files.size(); j < cnt; j++)
      {
         if (!pm.IsPluginRegistered(files[j]))
         {
            // No checking for error ?
            DiscoverPluginsAtPath(files[j], ignoredErrMsg,
               PluginManagerInterface::DefaultRegistrationCallback);
         }
      }
   }
}

PluginPaths LadspaEffectsModule::FindModulePaths(PluginManagerInterface & pm)
{
   auto pathList = GetSearchPaths();
   FilePaths files;

#if defined(__WXMAC__)

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#elif defined(__WXMSW__)

   // Recursively scan for all DLLs
   pm.FindFilesInPathList(wxT("*.dll"), pathList, files, true);

#else

   // Recursively scan for all shared objects
   pm.FindFilesInPathList(wxT("*.so"), pathList, files, true);

#endif

   return { files.begin(), files.end() };
}

unsigned LadspaEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   // Since we now have builtin VST support, ignore the VST bridge as it
   // causes duplicate menu entries to appear.
   wxFileName ff(path);
   if (ff.GetName().CmpNoCase(wxT("vst-bridge")) == 0) {
      errMsg = XO("Audacity no longer uses vst-bridge");
      return 0;
   }

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.
   wxString envpath;
   bool hadpath = wxGetEnv(wxT("PATH"), &envpath);
   wxSetEnv(wxT("PATH"), ff.GetPath() + wxFILE_SEP_PATH + envpath);
   wxString saveOldCWD = ff.GetCwd();
   ff.SetCwd();

   int index = 0;
   int nLoaded = 0;
   LADSPA_Descriptor_Function mainFn = NULL;

#if defined(__WXMSW__)
   wxDynamicLibrary lib;
   if (lib.Load(path, wxDL_NOW))
#else
   void *lib = dlopen((const char *)path.ToUTF8(), RTLD_NOW | RTLD_LOCAL | RTLD_DEEPBIND);
   if (lib)
#endif
   {

#if defined(__WXMSW__)
      wxLogNull logNo;

      mainFn = (LADSPA_Descriptor_Function) lib.GetSymbol(wxT("ladspa_descriptor"));
#else
      mainFn = (LADSPA_Descriptor_Function) dlsym(lib, "ladspa_descriptor");
#endif

      if (mainFn) {
         const LADSPA_Descriptor *data;

         for (data = mainFn(index); data; data = mainFn(++index)) {
            LadspaEffectBase effect(path, index);
            if (effect.InitializePlugin()) {
               ++nLoaded;
               if (callback)
                  callback( this, &effect );
            }
            else
               errMsg = XO("Could not load the library");
         }
      }
   }
   else
      errMsg = XO("Could not load the library");

#if defined(__WXMSW__)
   if (lib.IsLoaded()) {
      // PRL:  I suspect Bug1257 -- Crash when enabling Amplio2 -- is the fault of a timing-
      // dependent multi-threading bug in the Amplio2 library itself, in case the unload of the .dll
      // comes too soon after the load.  I saw the bug in Release builds but not Debug.
      // A sleep of even 1 ms was enough to fix the problem for me, but let's be even more generous.
      using namespace std::chrono;
      std::this_thread::sleep_for(10ms);
      lib.Unload();
   }
#else
   if (lib) {
      dlclose(lib);
   }
#endif

   wxSetWorkingDirectory(saveOldCWD);
   hadpath ? wxSetEnv(wxT("PATH"), envpath) : wxUnsetEnv(wxT("PATH"));

   return nLoaded;
}

std::unique_ptr<ComponentInterface>
LadspaEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   // For us, the path is two words.
   // 1)  The library's path
   // 2)  The LADSPA descriptor index
   long index;
   wxString realPath = path.BeforeFirst(wxT(';'));
   path.AfterFirst(wxT(';')).ToLong(&index);
   auto result = LadspaEffectBase::Factory::Call(realPath, (int)index);
   result->InitializePlugin();
   return result;
}

bool LadspaEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   const auto realPath = path.BeforeFirst(wxT(';'));
   return wxFileName::FileExists(realPath);
}

FilePaths LadspaEffectsModule::GetSearchPaths()
{
   FilePaths pathList;
   wxString pathVar;

   // Check for the LADSPA_PATH environment variable
   pathVar = wxString::FromUTF8(getenv("LADSPA_PATH"));
   if (!pathVar.empty())
   {
      wxStringTokenizer tok(pathVar, wxPATH_SEP);
      while (tok.HasMoreTokens())
      {
         pathList.push_back(tok.GetNextToken());
      }
   }

#if defined(__WXMAC__)
#define LADSPAPATH wxT("/Library/Audio/Plug-Ins/LADSPA")

   // Look in ~/Library/Audio/Plug-Ins/LADSPA and /Library/Audio/Plug-Ins/LADSPA
   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + LADSPAPATH);
   pathList.push_back(LADSPAPATH);

#elif defined(__WXMSW__)

   // No special paths...probably should look in %CommonProgramFiles%\LADSPA

#else

   pathList.push_back(wxGetHomeDir() + wxFILE_SEP_PATH + wxT(".ladspa"));
#if defined(__LP64__)
   pathList.push_back(wxT("/usr/local/lib64/ladspa"));
   pathList.push_back(wxT("/usr/lib64/ladspa"));
#endif
   pathList.push_back(wxT("/usr/local/lib/ladspa"));
   pathList.push_back(wxT("/usr/lib/ladspa"));
   pathList.push_back(wxT(LIBDIR) wxT("/ladspa"));

#endif

   return pathList;
}
