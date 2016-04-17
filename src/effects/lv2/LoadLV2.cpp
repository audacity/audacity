/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*******************************************************************//**

\file
Functions that find and load all LV2 plugins on the system.

*//*******************************************************************/

#include "../../Audacity.h"

#if defined(USE_LV2)

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "../EffectManager.h"

#include "LV2Effect.h"
#include "lv2/lv2plug.in/ns/ext/event/event.h"
#include "lv2/lv2plug.in/ns/ext/instance-access/instance-access.h"
#include "lv2/lv2plug.in/ns/ext/port-groups/port-groups.h"
#include "lv2/lv2plug.in/ns/ext/port-props/port-props.h"
#include "lv2/lv2plug.in/ns/ext/uri-map/uri-map.h"
#include "lv2/lv2plug.in/ns/ext/presets/presets.h"

#include "LoadLV2.h"

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
   return safenew LV2EffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(LV2sEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectsModule
//
///////////////////////////////////////////////////////////////////////////////
WX_DECLARE_STRING_HASH_MAP(LilvNode *, UriHash);

LilvWorld *gWorld = NULL;

LV2EffectsModule::LV2EffectsModule(ModuleManagerInterface *moduleManager,
                                   const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

LV2EffectsModule::~LV2EffectsModule()
{
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString LV2EffectsModule::GetPath()
{
   return mPath;
}

wxString LV2EffectsModule::GetSymbol()
{
   return XO("LV2 Effects");
}

wxString LV2EffectsModule::GetName()
{
   return GetSymbol();
}

wxString LV2EffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString LV2EffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LV2EFFECTS_VERSION;
}

wxString LV2EffectsModule::GetDescription()
{
   return XO("Provides LV2 Effects support to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool LV2EffectsModule::Initialize()
{
   // Try to initialise Lilv, or return.
   gWorld = lilv_world_new();
   if (!gWorld)
   {
      return false;
   }

   // Create LilvNodes for each of the URIs we need
   #undef URI
   #define URI(n, u) LV2Effect::n = lilv_new_uri(gWorld, u);
   URILIST

   wxString newVar;

#if defined(__WXMAC__)
#define LV2PATH wxT("/Library/Audio/Plug-Ins/LV2")

   wxFileName libdir;
//   libdir.AssignDir(wxT(LIBDIR));
   libdir.AppendDir(wxT("lv2"));

   newVar += wxT(":$HOME/.lv2");

   // Look in ~/Library/Audio/Plug-Ins/lv2 and /Library/Audio/Plug-Ins/lv2
   newVar += wxT(":$HOME") LV2PATH;
   newVar += wxT(":") LV2PATH;
   
   newVar += wxT(":/usr/local/lib/lv2");
   newVar += wxT(":/usr/lib/lv2");
   newVar += wxT(":") + libdir.GetPath();

#elif defined(__WXMSW__)

   newVar += wxT(";%APPDATA%\\LV2");
   newVar += wxT(";%COMMONPROGRAMFILES%\\LV2");
   newVar += wxT(";%COMMONPROGRAMFILES(x86)%\\LV2");

#else

   wxFileName libdir;
   libdir.AssignDir(wxT(LIBDIR));
   libdir.AppendDir(wxT("lv2"));

   newVar += wxT(":$HOME/.lv2");
   newVar += wxT(":/usr/local/lib/lv2");
   newVar += wxT(":/usr/lib/lv2");
   newVar += wxT(":/usr/local/lib64/lv2");
   newVar += wxT(":/usr/lib64/lv2");
   newVar += wxT(":") + libdir.GetPath();

#endif

   // Start with the LV2_PATH environment variable (if any)
   wxString pathVar;
   wxGetEnv(wxT("LV2_PATH"), &pathVar);

   if (pathVar.IsEmpty())
   {
      pathVar = newVar.Mid(1);
   }
   else
   {
      pathVar += newVar;
   }

   wxSetEnv(wxT("LV2_PATH"), pathVar);

   lilv_world_load_all(gWorld);

   return true;
}

void LV2EffectsModule::Terminate()
{
   // Free the LilvNodes for each of the URIs we need
   #undef URI
   #define URI(n, u) lilv_node_free(LV2Effect::n);
   URILIST

   lilv_world_free(gWorld);
   gWorld = NULL;

   return;
}

bool LV2EffectsModule::AutoRegisterPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   return false;
}

wxArrayString LV2EffectsModule::FindPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   // Retrieve data about all LV2 plugins
   const LilvPlugins *plugs = lilv_world_get_all_plugins(gWorld);

   // Iterate over all plugins retrieve their URI
   wxArrayString plugins;
   LILV_FOREACH(plugins, i, plugs)
   {
      const LilvPlugin *plug = lilv_plugins_get(plugs, i);

      // Bypass Instrument (MIDI) plugins for now
      const LilvPluginClass *cls = lilv_plugin_get_class(plug);
      if (lilv_node_equals(lilv_plugin_class_get_uri(cls), LV2Effect::gInstrument))
      {
         continue;
      }

      plugins.Add(LilvString(lilv_plugin_get_uri(plug)));
   }

   return plugins;
}

bool LV2EffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   const LilvPlugin *plug = GetPlugin(path);
   if (!plug)
   {
      return false;
   }

   LV2Effect effect(plug);
   if (effect.SetHost(NULL))
   {
      pm.RegisterPlugin(this, &effect);
   }

   return true;
}

bool LV2EffectsModule::IsPluginValid(const wxString & path)
{
   return GetPlugin(path) != NULL;
}

IdentInterface *LV2EffectsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   const LilvPlugin *plug = GetPlugin(path);
   if (!plug)
   {
      return NULL;
   }

   // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
   return safenew LV2Effect(plug);
}

void LV2EffectsModule::DeleteInstance(IdentInterface *instance)
{
   std::unique_ptr < LV2Effect > {
      dynamic_cast<LV2Effect *>(instance)
   };
}

// ============================================================================
// LV2EffectsModule implementation
// ============================================================================

const LilvPlugin *LV2EffectsModule::GetPlugin(const wxString & path)
{
   LilvNode *uri = lilv_new_uri(gWorld, path.ToUTF8());
   if (!uri)
   {
      return NULL;
   }

   const LilvPlugin *plug = lilv_plugins_get_by_uri(lilv_world_get_all_plugins(gWorld), uri);

   lilv_node_free(uri);

   return plug;
}

#endif
