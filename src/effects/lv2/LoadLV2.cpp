/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*******************************************************************//**

\file
Functions that find and load all LV2 plugins on the system.

*//*******************************************************************/



#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#endif

#include "LoadLV2.h"
#include "../../ModuleManager.h"

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/string.h>

#include "Internat.h"
#include "wxArrayStringEx.h"

#include "LV2Effect.h"
#include "lv2/event/event.h"
#include "lv2/instance-access/instance-access.h"
#include "lv2/port-groups/port-groups.h"
#include "lv2/port-props/port-props.h"
#include "lv2/uri-map/uri-map.h"
#include "lv2/presets/presets.h"

#include <unordered_map>

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
   return safenew LV2EffectsModule();
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
using UriHash = std::unordered_map<wxString, LilvNode*>;

LilvWorld *gWorld = NULL;

LV2EffectsModule::LV2EffectsModule()
{
}

LV2EffectsModule::~LV2EffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath LV2EffectsModule::GetPath()
{
   return {};
}

ComponentInterfaceSymbol LV2EffectsModule::GetSymbol()
{
   return XO("LV2 Effects");
}

VendorSymbol LV2EffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString LV2EffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LV2EFFECTS_VERSION;
}

TranslatableString LV2EffectsModule::GetDescription()
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
   #undef NODE
   #define NODE(n, u) LV2Effect::node_##n = lilv_new_uri(gWorld, u);
   NODELIST

   // Generate URIDs
   #undef URID
   #define URID(n, u) LV2Effect::urid_##n = LV2Effect::Lookup_URI(LV2Effect::gURIDMap, u);
      URIDLIST

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
#if defined(__LP64__)
   newVar += wxT(":/usr/local/lib64/lv2");
   newVar += wxT(":/usr/lib64/lv2");
#endif
   newVar += wxT(":/usr/local/lib/lv2");
   newVar += wxT(":/usr/lib/lv2");
   newVar += wxT(":") + libdir.GetPath();

   // Tell SUIL where to find his GUI support modules
   wxSetEnv(wxT("SUIL_MODULE_DIR"), wxT(PKGLIBDIR));
#endif

   // Start with the LV2_PATH environment variable (if any)
   wxString pathVar;
   wxGetEnv(wxT("LV2_PATH"), &pathVar);

   if (pathVar.empty())
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
   #undef NODE
   #define NODE(n, u) \
      lilv_node_free(LV2Effect::node_##n);
   NODELIST

   lilv_world_free(gWorld);
   gWorld = NULL;

   return;
}

EffectFamilySymbol LV2EffectsModule::GetOptionalFamilySymbol()
{
#if USE_LV2
   return LV2EFFECTS_FAMILY;
#else
   return {};
#endif
}

const FileExtensions &LV2EffectsModule::GetFileExtensions()
{
   static FileExtensions empty;
   return empty;
}

bool LV2EffectsModule::AutoRegisterPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   return false;
}

PluginPaths LV2EffectsModule::FindPluginPaths(PluginManagerInterface & WXUNUSED(pm))
{
   // Retrieve data about all LV2 plugins
   const LilvPlugins *plugs = lilv_world_get_all_plugins(gWorld);

   // Iterate over all plugins retrieve their URI
   PluginPaths plugins;
   LILV_FOREACH(plugins, i, plugs)
   {
      const LilvPlugin *plug = lilv_plugins_get(plugs, i);
      const LilvNode *cls = lilv_plugin_class_get_uri(lilv_plugin_get_class(plug));
      const LilvNode *name = lilv_plugin_get_name(plug);

      // Bypass unsupported plugin types
      if (lilv_node_equals(cls, LV2Effect::node_InstrumentPlugin) ||
          lilv_node_equals(cls, LV2Effect::node_MIDIPlugin) ||
          lilv_node_equals(cls, LV2Effect::node_MathConstants) ||
          lilv_node_equals(cls, LV2Effect::node_MathFunctions))
      {
         wxLogInfo(wxT("LV2 plugin '%s' has unsupported type '%s'"), lilv_node_as_string(lilv_plugin_get_uri(plug)), lilv_node_as_string(cls));
         printf("LV2 plugin '%s' has unsupported type '%s'\n", lilv_node_as_string(lilv_plugin_get_uri(plug)), lilv_node_as_string(cls));
         continue;
      }

      // If it doesn't have a name or has no ports, then it's not valid
      if (!name || !lilv_plugin_get_port_by_index(plug, 0))
      {
         wxLogInfo(wxT("LV2 plugin '%s' is invalid"), lilv_node_as_string(lilv_plugin_get_uri(plug)));
         printf("LV2 plugin '%s' is invalid\n", lilv_node_as_string(lilv_plugin_get_uri(plug)));
         continue;
      }

      plugins.push_back(LilvString(lilv_plugin_get_uri(plug)));
   }

   return plugins;
}

unsigned LV2EffectsModule::DiscoverPluginsAtPath(
   const PluginPath & path, TranslatableString &errMsg,
   const RegistrationCallback &callback)
{
   errMsg = {};
   const LilvPlugin *plug = GetPlugin(path);
   if (plug)
   {
      LV2Effect effect(plug);
      if (effect.SetHost(NULL))
      {
         if (callback)
            callback( this, &effect );
         return 1;
      }
   }

   errMsg = XO("Could not load the library");
   return 0;
}

bool LV2EffectsModule::IsPluginValid(const PluginPath & path, bool bFast)
{
   if( bFast )
      return true;
   return GetPlugin(path) != NULL;
}

ComponentInterface *LV2EffectsModule::CreateInstance(const PluginPath & path)
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

void LV2EffectsModule::DeleteInstance(ComponentInterface *instance)
{
   std::unique_ptr < LV2Effect > {
      dynamic_cast<LV2Effect *>(instance)
   };
}

// ============================================================================
// LV2EffectsModule implementation
// ============================================================================

const LilvPlugin *LV2EffectsModule::GetPlugin(const PluginPath & path)
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
