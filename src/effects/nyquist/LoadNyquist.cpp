/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../../AudacityApp.h"

#include <wx/log.h>

#include "Nyquist.h"

#include "LoadNyquist.h"

// ============================================================================
// List of effects that ship with Audacity.  These will be autoregistered.
// ============================================================================
const static wxChar *kShippedEffects[] =
{
   wxT("adjustable-fade.ny"),
   wxT("beat.ny"),
   wxT("clicktrack.ny"),
   wxT("clipfix.ny"),
   wxT("crossfadeclips.ny"),
   wxT("crossfadetracks.ny"),
   wxT("delay.ny"),
   wxT("equalabel.ny"),
   wxT("highpass.ny"),
   wxT("limiter.ny"),
   wxT("lowpass.ny"),
   wxT("notch.ny"),
   wxT("pluck.ny"),
   wxT("rissetdrum.ny"),
   wxT("sample-data-export.ny"),
   wxT("SilenceMarker.ny"),
   wxT("SoundFinder.ny"),
   wxT("SpectralEditMulti.ny"),
   wxT("SpectralEditParametricEQ.ny"),
   wxT("SpectralEditShelves.ny"),
   wxT("StudioFadeOut.ny"),
   wxT("tremolo.ny"),
   wxT("vocalrediso.ny"),
   wxT("vocalremover.ny"),
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
DECLARE_MODULE_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return safenew NyquistEffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(NyquistsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// NyquistEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

NyquistEffectsModule::NyquistEffectsModule(ModuleManagerInterface *moduleManager,
                                           const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

NyquistEffectsModule::~NyquistEffectsModule()
{
   mPath.Clear();
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString NyquistEffectsModule::GetPath()
{
   return mPath;
}

wxString NyquistEffectsModule::GetSymbol()
{
   return XO("Nyquist Effects");
}

wxString NyquistEffectsModule::GetName()
{
   return GetSymbol();
}

wxString NyquistEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString NyquistEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return NYQUISTEFFECTS_VERSION;
}

wxString NyquistEffectsModule::GetDescription()
{
   return XO("Provides Nyquist Effects support to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool NyquistEffectsModule::Initialize()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;

   for (size_t i = 0, cnt = audacityPathList.GetCount(); i < cnt; i++)
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

bool NyquistEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // Autoregister effects that we "think" are ones that have been shipped with
   // Audacity.  A little simplistic, but it should suffice for now.
   wxArrayString pathList = NyquistEffect::GetNyquistSearchPath();
   wxArrayString files;

   if (!pm.IsPluginRegistered(NYQUIST_PROMPT_ID))
   {
      RegisterPlugin(pm, NYQUIST_PROMPT_ID);
   }

   for (int i = 0; i < WXSIZEOF(kShippedEffects); i++)
   {
      files.Clear();
      pm.FindFilesInPathList(kShippedEffects[i], pathList, files);
      for (size_t j = 0, cnt = files.GetCount(); j < cnt; j++)
      {
         if (!pm.IsPluginRegistered(files[j]))
         {
            RegisterPlugin(pm, files[j]);
         }
      }
   }

   // We still want to be called during the normal registration process
   return false;
}

wxArrayString NyquistEffectsModule::FindPlugins(PluginManagerInterface & pm)
{
   wxArrayString pathList = NyquistEffect::GetNyquistSearchPath();
   wxArrayString files;

   // Add the Nyquist prompt effect
   files.Add(NYQUIST_PROMPT_ID);
   
   // Load .ny plug-ins
   pm.FindFilesInPathList(wxT("*.ny"), pathList, files);
   // LLL:  Works for all platform with NEW plugin support (dups are removed)
   pm.FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179

   return files;
}

bool NyquistEffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   NyquistEffect effect(path);
   if (effect.IsOk())
   {
      pm.RegisterPlugin(this, &effect);
      return true;
   }

   return false;
}

bool NyquistEffectsModule::IsPluginValid(const wxString & path)
{
   if (path == NYQUIST_PROMPT_ID)
   {
      return true;
   }

   return wxFileName::FileExists(path);
}

IdentInterface *NyquistEffectsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   auto effect = std::make_unique<NyquistEffect>(path);
   if (effect->IsOk())
   {
      // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
      return effect.release();
   }

   return NULL;
}

void NyquistEffectsModule::DeleteInstance(IdentInterface *instance)
{
   std::unique_ptr < NyquistEffect > {
      dynamic_cast<NyquistEffect *>(instance)
   };
}

// ============================================================================
// NyquistEffectsModule implementation
// ============================================================================
