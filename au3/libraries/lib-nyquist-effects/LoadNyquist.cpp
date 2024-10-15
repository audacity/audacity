/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/



#include "LoadNyquist.h"

#include <wx/log.h>

#include "NyquistBase.h"

#include "FileNames.h"
#include "PluginManager.h"
#include "ModuleManager.h"

#include <nyx.h>

// ============================================================================
// List of effects that ship with Audacity.  These will be autoregistered.
// ============================================================================
const static wxChar *kShippedEffects[] =
{
   wxT("adjustable-fade.ny"),
   wxT("beat.ny"),
   wxT("clipfix.ny"),
   wxT("crossfadeclips.ny"),
   wxT("crossfadetracks.ny"),
   wxT("delay.ny"),
   wxT("equalabel.ny"),
   wxT("highpass.ny"),
   wxT("label-sounds.ny"),
   wxT("legacy-limiter.ny"),
   wxT("lowpass.ny"),
   wxT("noisegate.ny"),
   wxT("notch.ny"),
   wxT("nyquist-plug-in-installer.ny"),
   wxT("pluck.ny"),
   wxT("rhythmtrack.ny"),
   wxT("rissetdrum.ny"),
   wxT("sample-data-export.ny"),
   wxT("sample-data-import.ny"),
   wxT("ShelfFilter.ny"),
   wxT("spectral-delete.ny"),
   wxT("SpectralEditMulti.ny"),
   wxT("SpectralEditParametricEQ.ny"),
   wxT("SpectralEditShelves.ny"),
   wxT("StudioFadeOut.ny"),
   wxT("tremolo.ny"),
   wxT("vocoder.ny"),
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
   return std::make_unique<NyquistEffectsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(NyquistsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

NyquistEffectsModule::NyquistEffectsModule()
{
}

NyquistEffectsModule::~NyquistEffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath NyquistEffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol NyquistEffectsModule::GetSymbol() const
{
   return XO("Nyquist Effects");
}

VendorSymbol NyquistEffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString NyquistEffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return NYQUISTEFFECTS_VERSION;
}

TranslatableString NyquistEffectsModule::GetDescription() const
{
   return XO("Provides Nyquist Effects support to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool NyquistEffectsModule::Initialize()
{
   const auto &audacityPathList = FileNames::AudacityPathList();

   for (size_t i = 0, cnt = audacityPathList.size(); i < cnt; i++)
   {
      wxFileName name(audacityPathList[i], wxT(""));
      name.AppendDir(wxT("nyquist"));
      name.SetFullName(wxT("nyquist.lsp"));
      if (name.FileExists())
      {
         // set_xlisp_path doesn't handle fn_Str() in Unicode build. May or may not actually work.
         nyx_set_xlisp_path(name.GetPath().ToUTF8());
         return true;
      }
   }

   wxLogWarning(wxT("Critical Nyquist files could not be found. Nyquist effects will not work."));

   return false;
}

void NyquistEffectsModule::Terminate()
{
   nyx_set_xlisp_path(NULL);

   return;
}

EffectFamilySymbol NyquistEffectsModule::GetOptionalFamilySymbol()
{
#if USE_NYQUIST
   return NYQUISTEFFECTS_FAMILY;
#else
   return {};
#endif
}

const FileExtensions &NyquistEffectsModule::GetFileExtensions()
{
   static FileExtensions result{{ _T("ny") }};
   return result;
}

FilePath NyquistEffectsModule::InstallPath()
{
   return FileNames::PlugInDir();
}

void NyquistEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Autoregister effects that we "think" are ones that have been shipped with
   // Audacity.  A little simplistic, but it should suffice for now.
   auto pathList = NyquistBase::GetNyquistSearchPath();
   FilePaths files;
   TranslatableString ignoredErrMsg;

   auto name = NYQUIST_PROMPT_NAME;
   if (!pm.IsPluginRegistered(NYQUIST_PROMPT_ID, &name))
   {
      // No checking of error ?
      DiscoverPluginsAtPath(NYQUIST_PROMPT_ID, ignoredErrMsg,
         PluginManagerInterface::DefaultRegistrationCallback);
   }

   for (size_t i = 0; i < WXSIZEOF(kShippedEffects); i++)
   {
      files.clear();
      pm.FindFilesInPathList(kShippedEffects[i], pathList, files);
      for (size_t j = 0, cnt = files.size(); j < cnt; j++)
      {
         /*
           TODO: Currently the names of Nyquist plug-ins cannot have
          context specific translations or internal names different from
          the visible English names.

          This makes it unnecessary to pass a second argument to
          IsPluginRegistered for correction of the registry (as is needed
          in the case of built-in effects).

          If it does become necessary in the future, we will need to open the
          .ny files to access their $name lines so that this argument could
          be supplied.
          */
         if (!pm.IsPluginRegistered(files[j]))
         {
            // No checking of error ?
            DiscoverPluginsAtPath(files[j], ignoredErrMsg,
               PluginManagerInterface::DefaultRegistrationCallback);
         }
      }
   }
}

PluginPaths NyquistEffectsModule::FindModulePaths(PluginManagerInterface & pm)
{
   auto pathList = NyquistBase::GetNyquistSearchPath();
   FilePaths files;

   // Add the Nyquist prompt
   files.push_back(NYQUIST_PROMPT_ID);

   // Load .ny plug-ins
   pm.FindFilesInPathList(wxT("*.ny"), pathList, files);
   // LLL:  Works for all platform with NEW plugin support (dups are removed)
   pm.FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179

   return { files.begin(), files.end() };
}

unsigned NyquistEffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   NyquistBase effect(path);
   if (effect.IsOk())
   {
      if (callback)
         callback(this, &effect);
      return 1;
   }

   errMsg = effect.InitializationError();
   return 0;
}

std::unique_ptr<ComponentInterface>
NyquistEffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   auto effect = NyquistBase::GetEffectHook::Call(path);
   if (effect && effect->IsOk())
      return effect;
   return nullptr;
}

bool NyquistEffectsModule::CheckPluginExist(const PluginPath& path) const
{
   if(path == NYQUIST_PROMPT_ID)
      return true;

   return wxFileName::FileExists(path);
}

// ============================================================================
// NyquistEffectsModule implementation
// ============================================================================
