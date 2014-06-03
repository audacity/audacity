/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLadspa.cpp

  Dominic Mazzoni

  From the ladspa docs:
  "To allow multiple hosts to
   share plugin types, hosts may wish to check for environment
   variable LADSPA_PATH. If present, this should contain a
   colon-separated path indicating directories that should be searched
   (in order) when loading plugin types."

**********************************************************************/

#define descriptorFnName "ladspa_descriptor"

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <map>
#include <queue>

#include <wx/dynlib.h>
#include <wx/filename.h>
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "ladspa.h"

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Experimental.h"
#include "../../Internat.h"
#include "../EffectManager.h"
#include "LadspaEffect.h"
#include "LoadLadspa.h"

#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)
#include <lrdf.h>
#endif

WX_DEFINE_ARRAY(wxDynamicLibrary*, DL_Array);

static DL_Array ladspa_dls;

#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)

#define LADSPA(S) wxT("http://ladspa.org/ontology#") wxT(S)
#define LV2(S) wxT("http://lv2plug.in/ns/lv2core#") wxT(S)

static std::map<wxString, wxString> gCategoryMap;
static std::multimap<unsigned long, wxString> gPluginCategories;

/** This function initialises the data structure that maps common LRDF
    category URIs to LV2 or internal Audacity category URIs. */
static void InitCategoryMap()
{
   gCategoryMap[LADSPA("UtilityPlugin")] = LV2("UtilityPlugin");
   gCategoryMap[LADSPA("GeneratorPlugin")] = LV2("GeneratorPlugin");
   gCategoryMap[LADSPA("SimulatorPlugin")] = LV2("SimulatorPlugin");
   gCategoryMap[LADSPA("OscillatorPlugin")] = LV2("OscillatorPlugin");
   gCategoryMap[LADSPA("TimePlugin")] = LV2("DelayPlugin");
   gCategoryMap[LADSPA("DelayPlugin")] = LV2("DelayPlugin");
   gCategoryMap[LADSPA("PhaserPlugin")] = LV2("PhaserPlugin");
   gCategoryMap[LADSPA("FlangerPlugin")] = LV2("FlangerPlugin");
   gCategoryMap[LADSPA("ChorusPlugin")] = LV2("ChorusPlugin");
   gCategoryMap[LADSPA("ReverbPlugin")] = LV2("ReverbPlugin");
   gCategoryMap[LADSPA("FrequencyPlugin")] = LV2("SpectralPlugin");
   gCategoryMap[LADSPA("FrequencyMeterPlugin")] = LV2("AnalyserPlugin");
   gCategoryMap[LADSPA("FilterPlugin")] = LV2("FilterPlugin");
   gCategoryMap[LADSPA("LowpassPlugin")] = LV2("LowpassPlugin");
   gCategoryMap[LADSPA("HighpassPlugin")] = LV2("HighpassPlugin");
   gCategoryMap[LADSPA("BandpassPlugin")] = LV2("BandpassPlugin");
   gCategoryMap[LADSPA("CombPlugin")] = LV2("CombPlugin");
   gCategoryMap[LADSPA("AllpassPlugin")] = LV2("AllpassPlugin");
   gCategoryMap[LADSPA("EQPlugin")] = LV2("EQPlugin");
   gCategoryMap[LADSPA("ParaEQPlugin")] = LV2("ParaEQPlugin");
   gCategoryMap[LADSPA("MultiEQPlugin")] = LV2("MultiEQPlugin");
   gCategoryMap[LADSPA("AmplitudePlugin")] = LV2("AmplifierPlugin");
   gCategoryMap[LADSPA("PitchPlugin")] = LV2("PitchPlugin");
   gCategoryMap[LADSPA("AmplifierPlugin")] = LV2("AmplifierPlugin");
   gCategoryMap[LADSPA("WaveshaperPlugin")] = LV2("WaveshaperPlugin");
   gCategoryMap[LADSPA("ModulatorPlugin")] = LV2("ModulatorPlugin");
   gCategoryMap[LADSPA("DistortionPlugin")] = LV2("DistortionPlugin");
   gCategoryMap[LADSPA("DynamicsPlugin")] = LV2("DynamicsPlugin");
   gCategoryMap[LADSPA("CompressorPlugin")] = LV2("CompressorPlugin");
   gCategoryMap[LADSPA("ExpanderPlugin")] = LV2("ExpanderPlugin");
   gCategoryMap[LADSPA("LimiterPlugin")] = LV2("LimiterPlugin");
   gCategoryMap[LADSPA("GatePlugin")] = LV2("GatePlugin");

   // This isn't in ladspa.rdfs, it's added by the swh-plugins -
   // we add it here to avoid having multiple "Spectral" categories
   gCategoryMap[LADSPA("SpectralPlugin")] = LV2("SpectralPlugin");

}

/** This function maps a LADSPA category URI to the category URIs used
    by Audacity (LV2 ones). If there is no known mapping for the
    given LADSPA category URI it is returned unchanged. */
static wxString MapCategoryUri(const wxString& ladspaCategory)
{
   std::map<wxString, wxString>::const_iterator iter;
   iter = gCategoryMap.find(ladspaCategory);
   if (iter != gCategoryMap.end())
      return iter->second;
   return ladspaCategory;
}

#endif

static void LoadLadspaEffect(wxSortedArrayString &uniq, wxString fname,
                             DL_Array &dls)
{
   wxLogNull logNo;
   LADSPA_Descriptor_Function mainFn = NULL;

   // Since we now have builtin VST support, ignore the VST bridge as it
   // causes duplicate menu entries to appear.
   wxFileName f(fname);
   if (f.GetName().CmpNoCase(wxT("vst-bridge")) == 0) {
      return;
   }

   // As a courtesy to some plug-ins that might be bridges to
   // open other plug-ins, we set the current working
   // directory to be the plug-in's directory.

   wxString saveOldCWD = ::wxGetCwd();
   wxString prefix = ::wxPathOnly(fname);
   ::wxSetWorkingDirectory(prefix);

   wxDynamicLibrary* pDLL = new wxDynamicLibrary();
   dls.push_back(pDLL);
   if (pDLL && pDLL->Load(fname, wxDL_LAZY)) {
      mainFn = (LADSPA_Descriptor_Function)(pDLL->GetSymbol(wxT(descriptorFnName)));
   }

   if (mainFn) {
      int index = 0;
      const LADSPA_Descriptor *data;

      data = mainFn(index);
      while(data) {

         wxString uniqid = wxString::Format(wxT("%08x-%s"), data->UniqueID, LAT1CTOWX(data->Label).c_str());
         if (uniq.Index(uniqid) == wxNOT_FOUND) {
            uniq.Add(uniqid);
            std::set<wxString> categories;

#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)
            std::multimap<unsigned long, wxString>::const_iterator iter;
            iter = gPluginCategories.lower_bound(data->UniqueID);
            for ( ; (iter != gPluginCategories.end() &&
                     iter->first == data->UniqueID); ++iter)
               categories.insert(iter->second);
#endif

            LadspaEffect *effect = new LadspaEffect(data, categories);
            EffectManager::Get().RegisterEffect(effect);
         }

         // Get next plugin
         index++;
         data = mainFn(index);
      }
   }

   ::wxSetWorkingDirectory(saveOldCWD);
}

void LoadLadspaPlugins()
{
   wxArrayString audacityPathList = wxGetApp().audacityPathList;
   wxArrayString pathList;
   wxArrayString files;
   wxSortedArrayString uniq;
   wxString pathVar;
   unsigned int i;

#if defined(USE_LIBLRDF) && defined(EFFECT_CATEGORIES)

   EffectManager& em = EffectManager::Get();
   wxArrayString rdfPathList;
   wxString rdfPathVar;
   wxArrayString rdfFiles;

   InitCategoryMap();
   lrdf_init();

   rdfPathVar = wxGetenv(wxT("LADSPA_RDF_PATH"));
   if (rdfPathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(rdfPathVar, rdfPathList);

#ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(wxT("/usr/share/ladspa/rdf"),
                                      rdfPathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/local/share/ladspa/rdf"),
                                      rdfPathList);
#endif

#ifdef __WXMAC__
   wxGetApp().AddUniquePathToPathList(wxT("/usr/share/ladspa/rdf"),
                                      rdfPathList);
   // XXX Maybe other Mac paths here?
#endif

#ifdef __WXMSW__
   //wxGetApp().AddUniquePathToPathList(wxT("WINDOWS LRDF PATH"),
   //                                   rdfPathList);
   // XXX Other Windows paths here.
#endif

   // Add the Audacity paths so we get ladspa.rdfs if we are using a local
   // liblrdf
   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("rdf"),
                                         rdfPathList);
   }

   wxGetApp().FindFilesInPathList(wxT("*.rdf"), rdfPathList, rdfFiles);
   wxGetApp().FindFilesInPathList(wxT("*.rdfs"), rdfPathList, rdfFiles);
   for(size_t i = 0; i < rdfFiles.GetCount(); ++i) {
      wxString fileUri(wxT("file://"));
      fileUri += rdfFiles[i];
      lrdf_read_file(fileUri.mb_str(wxConvUTF8));
   }


   // Add all plugin categories found by LRDF
   lrdf_uris* cats =
      lrdf_get_all_subclasses("http://ladspa.org/ontology#Plugin");
   if (cats) {

      // Add the categories and find the plugins belonging to them
      for (size_t i = 0; i < cats->count; ++i) {
         char* label = lrdf_get_label(cats->items[i]);
         if (!label)
            continue;
         wxString uri = MapCategoryUri(wxString::FromAscii(cats->items[i]));
         em.AddCategory(uri, wxString::FromUTF8(label));
         std::free(label);

         lrdf_uris* plugs = lrdf_get_instances(cats->items[i]);
         if (plugs) {
            for (size_t j = 0; j < plugs->count; ++j) {
               unsigned long uid = lrdf_get_uid(plugs->items[j]);
               gPluginCategories.insert(std::make_pair(uid, uri));
            }
            lrdf_free_uris(plugs);
         }
      }

      // And their relationships
      for (size_t i = 0; i < cats->count; ++i) {
         EffectCategory* p =
            em.LookupCategory(MapCategoryUri(wxString::FromAscii(cats->
                                                                 items[i])));
         if (!p)
            continue;
         lrdf_uris* subs = lrdf_get_subclasses(cats->items[i]);
         if (subs) {
            for (size_t j = 0; j < subs->count; ++j) {
               EffectCategory* c =
                  em.LookupCategory(MapCategoryUri(wxString::FromAscii(subs->items[j])));
               if (c)
                  em.AddCategoryParent(c, p);
            }
            lrdf_free_uris(subs);
         }
      }

      lrdf_free_uris(cats);

   }

#endif

   pathVar = wxGetenv(wxT("LADSPA_PATH"));
   if (pathVar != wxT(""))
      wxGetApp().AddMultiPathsToPathList(pathVar, pathList);

   #ifdef __WXGTK__
   wxGetApp().AddUniquePathToPathList(wxT("~/.ladspa"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/local/lib/ladspa"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/usr/lib/ladspa"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT(LIBDIR) wxT("/ladspa"), pathList);
   #endif

   #ifdef __WXMAC__
   wxGetApp().AddUniquePathToPathList(wxT("~/Library/Audio/Plug-Ins/LADSPA"), pathList);
   wxGetApp().AddUniquePathToPathList(wxT("/Library/Audio/Plug-Ins/LADSPA"), pathList);
   #endif

   for(i=0; i<audacityPathList.GetCount(); i++) {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      wxGetApp().AddUniquePathToPathList(prefix + wxT("ladspa"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plugins"),
                                         pathList);
      wxGetApp().AddUniquePathToPathList(prefix + wxT("plug-ins"),
                                         pathList);
   }

   #ifdef __WXMSW__
   wxGetApp().FindFilesInPathList(wxT("*.dll"), pathList, files);
   #else
   wxGetApp().FindFilesInPathList(wxT("*.so"), pathList, files);
   #endif

   for(i=0; i<files.GetCount(); i++)
      LoadLadspaEffect(uniq, files[i], ladspa_dls);
}

void UnloadLadspaPlugins()
{
   int count=ladspa_dls.GetCount();
   for (int i=0; i<count; i++)
   {
      delete ladspa_dls[i];
   }
}
