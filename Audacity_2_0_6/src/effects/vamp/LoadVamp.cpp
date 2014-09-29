/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadVamp.cpp

  Chris Cannam

**********************************************************************/

#include "../EffectManager.h"
#include "VampEffect.h"
#include "LoadVamp.h"

#include <iostream>
#include <map>

using namespace Vamp;
using namespace Vamp::HostExt;

#ifdef EFFECT_CATEGORIES

static std::map<wxString, wxString> gCategoryMap;

#define VAMP(S) wxT("http://audacityteam.org/namespace#VampCategories") wxT(S)
#define ATEAM(S) wxT("http://audacityteam.org/namespace#") wxT(S)

/** Initialise the data structure that maps locally generated URI strings to
    internal ones. */
static void InitCategoryMap()
{
   gCategoryMap[VAMP("/Time")] = ATEAM("TimeAnalyser");
   gCategoryMap[VAMP("/Time/Onsets")] = ATEAM("OnsetDetector");
}

/** Map the generated VAMP category URI to the internal ones. */
static wxString MapCategoryUri(const wxString& uri)
{
   std::map<wxString, wxString>::const_iterator iter;
   iter = gCategoryMap.find(uri);
   if (iter != gCategoryMap.end())
      return iter->second;
   return uri;
}

/** Generate category URIs for all levels in a VAMP category hierarchy,
    add them to the EffectManager and return the most detailed one. */
static wxString VampHierarchyToUri(const PluginLoader::PluginCategoryHierarchy& h)
{
   // Else, generate URIs and add them to the EffectManager
   EffectManager& em = EffectManager::Get();
   wxString vampCategory =
      wxString::FromAscii("http://audacityteam.org/namespace#VampCategories");
   EffectCategory* parent =
      em.LookupCategory(wxT("http://lv2plug.in/ns/lv2core#AnalyserPlugin"));
   if (parent) {
      for (size_t c = 0; c < h.size(); ++c) {
         vampCategory += wxT("/");
         wxString catName = wxString::FromAscii(h[c].c_str());
         vampCategory += catName;
         EffectCategory* ec = em.AddCategory(MapCategoryUri(vampCategory),
                                             catName);
         em.AddCategoryParent(ec, parent);
         parent = ec;
      }
   }
   return MapCategoryUri(vampCategory);
}

#endif

void LoadVampPlugins()
{

#ifdef EFFECT_CATEGORIES
   InitCategoryMap();
#endif

   PluginLoader *loader = PluginLoader::getInstance();

   EffectManager& em = EffectManager::Get();

   PluginLoader::PluginKeyList keys = loader->listPlugins();

   for (PluginLoader::PluginKeyList::iterator i = keys.begin();
        i != keys.end(); ++i) {

      Plugin *vp = loader->loadPlugin(*i, 48000); // rate doesn't matter here
      if (!vp) continue;

#ifdef EFFECT_CATEGORIES

      PluginLoader::PluginCategoryHierarchy category =
         loader->getPluginCategory(*i);
      wxString vampCategory = VampHierarchyToUri(category);

#endif

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

      int n = 0;

      bool hasParameters = !vp->getParameterDescriptors().empty();

      for (Plugin::OutputList::iterator j = outputs.begin();
           j != outputs.end(); ++j) {

         if (j->sampleType == Plugin::OutputDescriptor::FixedSampleRate ||
             j->sampleType == Plugin::OutputDescriptor::OneSamplePerStep ||
             !j->hasFixedBinCount ||
             (j->hasFixedBinCount && j->binCount > 1)) {

            // All of these qualities disqualify (see notes above)

            ++n;
            continue;
         }

         wxString name = LAT1CTOWX(vp->getName().c_str());

         if (outputs.size() > 1) {
            // This is not the plugin's only output.
            // Use "plugin name: output name" as the effect name,
            // unless the output name is the same as the plugin name
            wxString outputName = LAT1CTOWX(j->name.c_str());
            if (outputName != name) {
               name = wxString::Format(wxT("%s: %s"),
                                       name.c_str(), outputName.c_str());
            }
         }

#ifdef EFFECT_CATEGORIES
         VampEffect *effect = new VampEffect(*i, n, hasParameters, name,
                                                vampCategory);
#else
         VampEffect *effect = new VampEffect(*i, n, hasParameters, name);
#endif
         em.RegisterEffect(effect);

         ++n;
      }

      delete vp;
   }
}

void UnloadVampPlugins()
{
}
