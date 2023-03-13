/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LoadLV2.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*******************************************************************//**

\file
Functions that find and load all LV2 plugins on the system.

*//*******************************************************************/



#include "LV2Wrapper.h"
#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#endif

#include "LoadLV2.h"
#include "ModuleManager.h"

#include <cstdlib>
#include <cstring>
#include <iostream>

#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/log.h>
#include <wx/utils.h>

#include "Internat.h"
#include "wxArrayStringEx.h"

#include "LV2EffectBase.h"

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
DECLARE_PROVIDER_ENTRY(AudacityModule)
{
   // Create and register the importer
   // Trust the module manager not to leak this
   return std::make_unique<LV2EffectsModule>();
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_PROVIDER(LV2sEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// LV2EffectsModule
//
///////////////////////////////////////////////////////////////////////////////
using UriHash = std::unordered_map<wxString, LilvNode*>;

LV2EffectsModule::LV2EffectsModule()
{
}

LV2EffectsModule::~LV2EffectsModule()
{
}

// ============================================================================
// ComponentInterface implementation
// ============================================================================

PluginPath LV2EffectsModule::GetPath() const
{
   return {};
}

ComponentInterfaceSymbol LV2EffectsModule::GetSymbol() const
{
   return XO("LV2 Effects");
}

VendorSymbol LV2EffectsModule::GetVendor() const
{
   return XO("The Audacity Team");
}

wxString LV2EffectsModule::GetVersion() const
{
   // This "may" be different if this were to be maintained as a separate DLL
   return LV2EFFECTS_VERSION;
}

TranslatableString LV2EffectsModule::GetDescription() const
{
   return XO("Provides LV2 Effects support to Audacity");
}

// ============================================================================
// PluginProvider implementation
// ============================================================================

bool LV2EffectsModule::Initialize()
{
   if (!LV2Symbols::InitializeGWorld())
      return false;

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
   lilv_world_load_all(LV2Symbols::gWorld);

   return true;
}

void LV2EffectsModule::Terminate()
{
   LV2Symbols::FinalizeGWorld();
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

void LV2EffectsModule::AutoRegisterPlugins(PluginManagerInterface &)
{
}

PluginPaths LV2EffectsModule::FindModulePaths(PluginManagerInterface &)
{
   // Retrieve data about all LV2 plugins
   const LilvPlugins *plugs = lilv_world_get_all_plugins(LV2Symbols::gWorld);

   // Iterate over all plugins retrieve their URI
   PluginPaths plugins;
   LILV_FOREACH(plugins, i, plugs)
   {
      const LilvPlugin *plug = lilv_plugins_get(plugs, i);
      const LilvNode *cls = lilv_plugin_class_get_uri(lilv_plugin_get_class(plug));
      LilvNodePtr name{ lilv_plugin_get_name(plug) };

      // Bypass unsupported plugin types
      using namespace LV2Symbols;
      if (lilv_node_equals(cls, node_InstrumentPlugin) ||
          lilv_node_equals(cls, node_MIDIPlugin) ||
          lilv_node_equals(cls, node_MathConstants) ||
          lilv_node_equals(cls, node_MathFunctions))
      {
         wxLogInfo(wxT("LV2 plugin '%s' has unsupported type '%s'"), lilv_node_as_string(lilv_plugin_get_uri(plug)), lilv_node_as_string(cls));
         continue;
      }

      // If it doesn't have a name or has no ports, then it's not valid
      if (!name || !lilv_plugin_get_port_by_index(plug, 0))
      {
         wxLogInfo(wxT("LV2 plugin '%s' is invalid"), lilv_node_as_string(lilv_plugin_get_uri(plug)));
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
   if (const auto plug = GetPlugin(path)) {
      LV2EffectBase effect(*plug);
      if (effect.InitializePlugin()) {
         if (callback)
            callback( this, &effect );
         return 1;
      }
   }

   errMsg = XO("Could not load the library");
   return 0;
}

std::unique_ptr<ComponentInterface>
LV2EffectsModule::LoadPlugin(const PluginPath & path)
{
   // Acquires a resource for the application.
   if (const auto plug = GetPlugin(path)) {
      auto result = LV2EffectBase::Factory::Call(*plug);
      result->InitializePlugin();
      return result;
   }
   return nullptr;
}

bool LV2EffectsModule::CheckPluginExist(const PluginPath & path) const
{
   return GetPlugin(path) != nullptr;
}

namespace {
class LV2PluginValidator : public PluginProvider::Validator
{
public:
   void Validate(ComponentInterface& pluginInterface) override
   {
      if(auto lv2effect = dynamic_cast<LV2EffectBase*>(&pluginInterface))
      {
         LV2_Atom_Forge forge;
         lv2_atom_forge_init(&forge, lv2effect->mFeatures.URIDMapFeature());

         LV2PortStates portStates { lv2effect->mPorts };
         LV2InstanceFeaturesList instanceFeatures { lv2effect->mFeatures };
         
         auto settings = lv2effect->MakeSettings();
         auto wrapper = LV2Wrapper::Create(
            instanceFeatures,
            lv2effect->mPorts,
            portStates,
            GetSettings(settings),
            44100.0,
            nullptr);

         if(!wrapper)
            throw std::runtime_error("Cannot create LV2 instance");
         
      }
      else
         throw std::runtime_error("Not a LV2Effect");
   }
};
}

std::unique_ptr<PluginProvider::Validator> LV2EffectsModule::MakeValidator() const
{
   return std::make_unique<LV2PluginValidator>();
}


// ============================================================================
// LV2EffectsModule implementation
// ============================================================================

const LilvPlugin *LV2EffectsModule::GetPlugin(const PluginPath & path)
{
   using namespace LV2Symbols;
   if (LilvNodePtr uri{ lilv_new_uri(gWorld, path.ToUTF8()) })
      // lilv.h says returns from the following two functions don't need freeing
      return lilv_plugins_get_by_uri(
         lilv_world_get_all_plugins(gWorld), uri.get());
   return nullptr;
}

#endif
