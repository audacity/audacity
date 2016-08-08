/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.cpp

  Chris Cannam

**********************************************************************/

#include "../../Audacity.h"

#if defined(USE_VAMP)

#include <wx/filename.h>

#include "../EffectManager.h"
#include "VampEffect.h"
#include "LoadVamp.h"

#include <iostream>
#include <map>

using namespace Vamp;
using namespace Vamp::HostExt;
using namespace Vamp::HostExt;

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
   return safenew VampEffectsModule(moduleManager, path);
}

// ============================================================================
// Register this as a builtin module
// ============================================================================
DECLARE_BUILTIN_MODULE(VampsEffectBuiltin);

///////////////////////////////////////////////////////////////////////////////
//
// VampEffectsModule
//
///////////////////////////////////////////////////////////////////////////////

VampEffectsModule::VampEffectsModule(ModuleManagerInterface *moduleManager,
                                           const wxString *path)
{
   mModMan = moduleManager;
   if (path)
   {
      mPath = *path;
   }
}

VampEffectsModule::~VampEffectsModule()
{
}

// ============================================================================
// IdentInterface implementation
// ============================================================================

wxString VampEffectsModule::GetPath()
{
   return mPath;
}

wxString VampEffectsModule::GetSymbol()
{
   return wxT("Vamp Effects");
}

wxString VampEffectsModule::GetName()
{
   return XO("Vamp Effects");
}

wxString VampEffectsModule::GetVendor()
{
   return XO("The Audacity Team");
}

wxString VampEffectsModule::GetVersion()
{
   // This "may" be different if this were to be maintained as a separate DLL
   return VAMPEFFECTS_VERSION;
}

wxString VampEffectsModule::GetDescription()
{
   return XO("Provides Vamp Effects support to Audacity");
}

// ============================================================================
// ModuleInterface implementation
// ============================================================================

bool VampEffectsModule::Initialize()
{
   // Nothing to do here
   return true;
}

void VampEffectsModule::Terminate()
{
   // Nothing to do here
   return;
}

bool VampEffectsModule::AutoRegisterPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   return false;
}

wxArrayString VampEffectsModule::FindPlugins(PluginManagerInterface & WXUNUSED(pm))
{
   wxArrayString names;

   PluginLoader *loader = PluginLoader::getInstance();

   PluginLoader::PluginKeyList keys = loader->listPlugins();

   for (PluginLoader::PluginKeyList::iterator i = keys.begin(); i != keys.end(); ++i)
   {
      std::unique_ptr<Plugin> vp{ PluginLoader::getInstance()->loadPlugin(*i, 48000) }; // rate doesn't matter here
      if (!vp)
      {
         continue;
      }

      // We limit the listed plugin outputs to those whose results can
      // readily be displayed in an Audacity label track.
      //
      // - Any output whose features have no values (time instants only),
      //   with or without duration, is fine
      //
      // - Any output whose features have more than one value, or an
      //   unknown or variable number of values, is right out
      //
      // - Any output whose features have exactly one value, with
      //   variable sample rate or with duration, should be OK --
      //   this implies a sparse feature, of which the time and/or
      //   duration are significant aspects worth displaying
      //
      // - An output whose features have exactly one value, with
      //   fixed sample rate and no duration, cannot be usefully
      //   displayed -- the value is the only significant piece of
      //   data there and we have no good value plot

      Plugin::OutputList outputs = vp->getOutputDescriptors();

      int output = 0;

      for (Plugin::OutputList::iterator j = outputs.begin(); j != outputs.end(); ++j)
      {
         if (j->sampleType == Plugin::OutputDescriptor::FixedSampleRate ||
               j->sampleType == Plugin::OutputDescriptor::OneSamplePerStep ||
               !j->hasFixedBinCount ||
               (j->hasFixedBinCount && j->binCount > 1))
         {
            // All of these qualities disqualify (see notes above)

            ++output;
            continue;
         }

         wxString name = wxString::FromUTF8(vp->getName().c_str());

         if (outputs.size() > 1)
         {
            // This is not the plugin's only output.
            // Use "plugin name: output name" as the effect name,
            // unless the output name is the same as the plugin name
            wxString outputName = wxString::FromUTF8(j->name.c_str());
            if (outputName != name)
            {
               name = wxString::Format(wxT("%s: %s"),
                                       name.c_str(), outputName.c_str());
            }
         }

         wxString path = wxString::FromUTF8(i->c_str()) + wxT("/") + name;
         names.Add(path);

         ++output;
      }
   }

   return names;
}

bool VampEffectsModule::RegisterPlugin(PluginManagerInterface & pm, const wxString & path)
{
   int output;
   bool hasParameters;

   auto vp = FindPlugin(path, output, hasParameters);
   if (vp)
   {
      VampEffect effect(std::move(vp), path, output, hasParameters);
      pm.RegisterPlugin(this, &effect);

      return true;
   }

   return false;
}

bool VampEffectsModule::IsPluginValid(const wxString & path)
{
   int output;
   bool hasParameters;

   auto vp = FindPlugin(path, output, hasParameters);
   return bool(vp);
}

IdentInterface *VampEffectsModule::CreateInstance(const wxString & path)
{
   // Acquires a resource for the application.
   int output;
   bool hasParameters;

   auto vp = FindPlugin(path, output, hasParameters);
   if (vp)
   {
      // Safety of this depends on complementary calls to DeleteInstance on the module manager side.
      return safenew VampEffect(std::move(vp), path, output, hasParameters);
   }

   return NULL;
}

void VampEffectsModule::DeleteInstance(IdentInterface *instance)
{
   std::unique_ptr < VampEffect > {
      dynamic_cast<VampEffect *>(instance)
   };
}

// VampEffectsModule implementation

std::unique_ptr<Vamp::Plugin> VampEffectsModule::FindPlugin(const wxString & path,
                                      int & output,
                                      bool & hasParameters)
{
   PluginLoader::PluginKey key = path.BeforeLast(wxT('/')).ToUTF8().data();

   std::unique_ptr<Plugin> vp{ PluginLoader::getInstance()->loadPlugin(key, 48000) }; // rate doesn't matter here
   if (!vp)
   {
      return nullptr;
   }

   // We limit the listed plugin outputs to those whose results can
   // readily be displayed in an Audacity label track.
   //
   // - Any output whose features have no values (time instants only),
   //   with or without duration, is fine
   //
   // - Any output whose features have more than one value, or an
   //   unknown or variable number of values, is right out
   //
   // - Any output whose features have exactly one value, with
   //   variable sample rate or with duration, should be OK --
   //   this implies a sparse feature, of which the time and/or
   //   duration are significant aspects worth displaying
   //
   // - An output whose features have exactly one value, with
   //   fixed sample rate and no duration, cannot be usefully
   //   displayed -- the value is the only significant piece of
   //   data there and we have no good value plot

   Plugin::OutputList outputs = vp->getOutputDescriptors();

   output = 0;

   hasParameters = !vp->getParameterDescriptors().empty();

   for (Plugin::OutputList::iterator j = outputs.begin(); j != outputs.end(); ++j)
   {
      if (j->sampleType == Plugin::OutputDescriptor::FixedSampleRate ||
            j->sampleType == Plugin::OutputDescriptor::OneSamplePerStep ||
            !j->hasFixedBinCount ||
            (j->hasFixedBinCount && j->binCount > 1))
      {
         // All of these qualities disqualify (see notes above)

         ++output;
         continue;
      }

      wxString name = wxString::FromUTF8(vp->getName().c_str());

      if (outputs.size() > 1)
      {
         // This is not the plugin's only output.
         // Use "plugin name: output name" as the effect name,
         // unless the output name is the same as the plugin name
         wxString outputName = wxString::FromUTF8(j->name.c_str());
         if (outputName != name)
         {
            name = wxString::Format(wxT("%s: %s"),
                                    name.c_str(), outputName.c_str());
         }
      }

      if (wxString::FromUTF8(key.c_str()) + wxT("/") + name == path)
      {
         return vp;
      }

      ++output;
   }

   return {};
}

#endif
