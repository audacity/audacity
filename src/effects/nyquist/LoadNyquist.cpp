/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadNyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include "../EffectManager.h"
#include "Nyquist.h"
#include "LoadNyquist.h"

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
   return new NyquistEffectsModule(moduleManager, path);
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
   return wxT("Nyquist Effects");
}

wxString NyquistEffectsModule::GetName()
{
   return wxTRANSLATE("Nyquist Effects");
}

wxString NyquistEffectsModule::GetVendor()
{
   return wxTRANSLATE("The Audacity Team");
}

wxString NyquistEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return NYQUISTEFFECTS_VERSION;
}

wxString NyquistEffectsModule::GetDescription()
{
   return wxTRANSLATE("Provides Nyquist Effects support to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool NyquistEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void NyquistEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool NyquistEffectsModule::AutoRegisterPlugins(PluginManagerInterface & pm)
{
   // For Nyquist, we autoregister plugins at this time using the legacy
   // interface.  This will change eventually.
   
   wxArrayString pathList = EffectNyquist::GetNyquistSearchPath();
   wxArrayString files;

   // Create one "interactive Nyquist" effect
   EffectNyquist *effect = new EffectNyquist(wxT(""));
   EffectManager::Get().RegisterEffect(this, effect);

   // Load .ny plug-ins
   pm.FindFilesInPathList(wxT("*.ny"), pathList, files);
#ifdef  __WXGTK__
   pm.FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179
#endif

   for (size_t i = 0; i < files.GetCount(); i++)
   {
      EffectNyquist *effect = new EffectNyquist(files[i]);
      if (effect->LoadedNyFile())
      {
         EffectManager::Get().RegisterEffect(this, effect);
      }
      else
      {
         delete effect;
      }
   }

   return true;
}

wxArrayString NyquistEffectsModule::FindPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   // Nothing to do here yet
   return wxArrayString();
}

bool NyquistEffectsModule::RegisterPlugin(PluginManagerInterface & WXUNUSED(pm), const wxString & WXUNUSED(path))
{
   // Nothing to do here yet
   return false;
}

bool NyquistEffectsModule::IsPluginValid(const wxString & path)
{
   if (path == wxT("nyquist prompt"))
   {
      return true;
   }

   return wxFileName::FileExists(path);
}

IdentInterface *NyquistEffectsModule::CreateInstance(const wxString & path)
{
   // Normally, we wouldn't have anything to do here since we're autoregistering, but
   // if we have Nyquist effects in directories we didn't scan.

   EffectNyquist *effect = new EffectNyquist(path);
   if (effect->LoadedNyFile())
   {
      return effect;
   }

   delete effect;

   return NULL;
}

void NyquistEffectsModule::DeleteInstance(IdentInterface *instance)
{
   // Nothing to do here
}

// ============================================================================
// NyquistEffectsModule implementation
// ============================================================================

