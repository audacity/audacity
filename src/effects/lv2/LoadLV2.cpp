/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*******************************************************************//**

\file
Functions that find and load all LV2 plugins on the system.

*//*******************************************************************/


#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iostream>

#include <wx/dynlib.h>
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../Audacity.h"
#include "../../AudacityApp.h"
#include "../../Experimental.h"
#include "../../Internat.h"
#include "../EffectManager.h"

#if defined(USE_SLV2)

#include "LV2Effect.h"
#include "lv2_uri_map.h"
#include "lv2_event.h"

#include "LoadLV2.h"

SLV2World gWorld = 0;

// This is the URI Map Feature object. It is required for loading synth
// plugins.
static uint32_t uri_to_id(LV2_URI_Map_Callback_Data cbd,
                          const char* map, const char* uri) {
   if (!std::strcmp(map, "http://lv2plug.in/ns/ext/event")) {
      if (!std::strcmp(uri, "http://lv2plug.in/ns/ext/midi#MidiEvent"))
         return 1;
      else if (!std::strcmp(uri, "http://lv2plug.in/ns/ext/event#TimeStamp"))
         return 2;
   }
   return 0;
}
static LV2_URI_Map_Feature gURIMap = { 0, &uri_to_id };
static LV2_Feature gURIMapFeature = { "http://lv2plug.in/ns/ext/uri-map",
                                      &gURIMap };

// This is the event refcounter object. We don't actually implement it
// since we only ever send flat MIDI events to the plugins, but it is 
// still required.
uint32_t event_ref(LV2_Event_Callback_Data callback_data, LV2_Event* event) {
   return 0;
}
static LV2_Event_Feature gEventRef = { 0, &event_ref, &event_ref };
static LV2_Feature gEventRefFeature = { "http://lv2plug.in/ns/ext/event",
                                        &gEventRef };

// These are the LV2 Features we support.
LV2_Feature*const gLV2Features[] = { &gURIMapFeature, &gEventRefFeature, 0 };


SLV2Value gAudioPortClass;
SLV2Value gControlPortClass;
SLV2Value gMidiPortClass;
SLV2Value gInputPortClass;
SLV2Value gOutputPortClass;
SLV2Value gPortToggled;
SLV2Value gPortIsInteger;
SLV2Value gPortIsSampleRate;


void LoadLV2Plugins() {
   
   EffectManager& em = EffectManager::Get();
   
   // If gWorld isn't 0 we have already initialised SLV2 - unload all plugins
   // and initialise again.
   if (gWorld)
      UnloadLV2Plugins();
   
   // Try to initialise SLV2, or return.
   gWorld = slv2_world_new();
   if (!gWorld) {
      std::cerr<<"Could not initialise SLV2!"<<std::endl;
      return;
   }
   
   gAudioPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_AUDIO);
   gControlPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_CONTROL);
   gMidiPortClass = slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/ext/event#EventPort");
   gInputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_INPUT);
   gOutputPortClass = slv2_value_new_uri(gWorld, SLV2_PORT_CLASS_OUTPUT);
   gPortToggled = slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#toggled");
   gPortIsInteger = slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#integer");
   gPortIsSampleRate = slv2_value_new_uri(gWorld, "http://lv2plug.in/ns/lv2core#sampleRate");

   slv2_world_load_all(gWorld);
   
#ifdef EFFECT_CATEGORIES
   
   // Add all LV2 categories and their relationships
   SLV2PluginClasses classes = slv2_world_get_plugin_classes(gWorld);
   for (unsigned index = 0; index < slv2_plugin_classes_size(classes);++index){
      SLV2PluginClass c = slv2_plugin_classes_get_at(classes, index);
      em.AddCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(c))), 
                     wxString::FromUTF8(slv2_value_as_string(slv2_plugin_class_get_label(c))));
   }
   for (unsigned index = 0; index < slv2_plugin_classes_size(classes);++index){
      SLV2PluginClass c = slv2_plugin_classes_get_at(classes, index);
      SLV2PluginClasses ch = slv2_plugin_class_get_children(c);
      EffectCategory* pCat = em.LookupCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(c))));
      for (unsigned j = 0; j < slv2_plugin_classes_size(ch); ++j) {
         EffectCategory* chCat = em.LookupCategory(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(slv2_plugin_classes_get_at(ch, j)))));
         if (chCat && pCat) {
            em.AddCategoryParent(chCat, pCat);
         }
      }
   }

#endif
   
   // Retrieve data about all plugins
   SLV2Plugins plugs = slv2_world_get_all_plugins(gWorld);
   
   // Iterate over all plugins and register them with the EffectManager
   for (unsigned index = 0; index < slv2_plugins_size(plugs); ++index) {
      SLV2Plugin plug = slv2_plugins_get_at(plugs, index);
      std::set<wxString> cats;
      cats.insert(wxString::FromUTF8(slv2_value_as_uri(slv2_plugin_class_get_uri(slv2_plugin_get_class(plug)))));
      LV2Effect *effect = new LV2Effect(plug, cats);
      if (effect->IsValid())
         em.RegisterEffect(effect);
      else
         delete effect;
      //std::cerr<<"Loaded LV2 \""<<slv2_value_as_string(slv2_plugin_get_name(plug))<<"\""<<std::endl;
   }
   
   // Deallocate the plugin list (but not the plugins)
   slv2_plugins_free(gWorld, plugs);
}

void UnloadLV2Plugins()
{
}

#endif
