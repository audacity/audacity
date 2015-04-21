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

bool NyquistEffectsModule::AutoRegisterPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   // Nothing to do here
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
#ifdef  __WXGTK__
   pm.FindFilesInPathList(wxT("*.NY"), pathList, files); // Ed's fix for bug 179
#endif

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
   NyquistEffect *effect = new NyquistEffect(path);
   if (effect->IsOk())
   {
      return effect;
   }

   delete effect;

   return NULL;
}

void NyquistEffectsModule::DeleteInstance(IdentInterface *instance)
{
   NyquistEffect *effect = dynamic_cast<NyquistEffect *>(instance);
   if (effect)
   {
      delete effect;
   }
}

// ============================================================================
// NyquistEffectsModule implementation
// ============================================================================
