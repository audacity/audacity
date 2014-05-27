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
#include <wx/hashmap.h>
#include <wx/list.h>
#include <wx/log.h>
#include <wx/string.h>

#include "../../AudacityApp.h"
#include "../../Internat.h"
#include "../EffectManager.h"

#include "LV2Effect.h"
#include "lv2/lv2plug.in/ns/ext/event/event.h"
#include "lv2/lv2plug.in/ns/ext/midi/midi.h"
#include "lv2/lv2plug.in/ns/ext/port-groups/port-groups.h"
#include "lv2/lv2plug.in/ns/ext/uri-map/uri-map.h"

#include "LoadLV2.h"

LilvWorld *gWorld = NULL;

// This is the URI Map Feature object. It is required for loading synth
// plugins.
static uint32_t uri_to_id(LV2_URI_Map_Callback_Data WXUNUSED(cbd),
                          const char *map, const char *uri)
{
   if (!std::strcmp(map, LV2_EVENT_URI))
   {
      if (!std::strcmp(uri, LV2_MIDI__MidiEvent))
      {
         return 1;
      }
      else if (!std::strcmp(uri, LV2_EVENT__TimeStamp))
      {
         return 2;
      }
   }

   return 0;
}
static LV2_URI_Map_Feature gURIMap = { 0, &uri_to_id };
static LV2_Feature gURIMapFeature = { "http://lv2plug.in/ns/ext/uri-map",
                                      &gURIMap };

// This is the event refcounter object. We don't actually implement it
// since we only ever send flat MIDI events to the plugins, but it is 
// still required.
uint32_t event_ref(LV2_Event_Callback_Data WXUNUSED(callback_data),
                   LV2_Event *WXUNUSED(event))
{
   return 0;
}
static LV2_Event_Feature gEventRef = { 0, &event_ref, &event_ref };
static LV2_Feature gEventRefFeature = { "http://lv2plug.in/ns/ext/event",
                                        &gEventRef };

// These are the LV2 Features we support.
LV2_Feature*const gLV2Features[] = { &gURIMapFeature, &gEventRefFeature, 0 };

LilvNode *gAudioPortClass;
LilvNode *gControlPortClass;
LilvNode *gMidiPortClass;
LilvNode *gInputPortClass;
LilvNode *gOutputPortClass;
LilvNode *gPortToggled;
LilvNode *gPortIsInteger;
LilvNode *gPortIsSampleRate;
LilvNode *gPortIsEnumeration;
LilvNode *gPortIsLatency;
LilvNode *gPortIsOptional;
LilvNode *gName;
LilvNode *gPortGroup;
LilvNode *gSubGroupOf;

void LoadLV2Plugins()
{
   
   EffectManager& em = EffectManager::Get();
   
   // If gWorld isn't 0 we have already initialised Lilv - unload all plugins
   // and initialise again.
   if (gWorld)
   {
      UnloadLV2Plugins();
   }
   
   // Try to initialise Lilv, or return.
   gWorld = lilv_world_new();
   if (!gWorld)
   {
      wxLogMessage(wxT("Could not initialise lilv!"));
      return;
   }
   
   gAudioPortClass = lilv_new_uri(gWorld, LV2_CORE__AudioPort);
   gControlPortClass = lilv_new_uri(gWorld, LV2_CORE__ControlPort);
   gMidiPortClass = lilv_new_uri(gWorld, LV2_EVENT__EventPort);
   gInputPortClass = lilv_new_uri(gWorld, LV2_CORE__InputPort);
   gOutputPortClass = lilv_new_uri(gWorld, LV2_CORE__OutputPort);
   gPortToggled = lilv_new_uri(gWorld, LV2_CORE__toggled);
   gPortIsInteger = lilv_new_uri(gWorld, LV2_CORE__integer);
   gPortIsSampleRate = lilv_new_uri(gWorld, LV2_CORE__sampleRate);
   gPortIsEnumeration = lilv_new_uri(gWorld, LV2_CORE__enumeration);
   gPortIsLatency = lilv_new_uri(gWorld, LV2_CORE__reportsLatency);
   gPortIsOptional = lilv_new_uri(gWorld, LV2_CORE__connectionOptional);
   gName = lilv_new_uri(gWorld, LV2_CORE__name);
   gPortGroup = lilv_new_uri(gWorld, LV2_PORT_GROUPS__group);
   gSubGroupOf = lilv_new_uri(gWorld, LV2_PORT_GROUPS__subGroupOf);

   lilv_world_load_all(gWorld);
   
#ifdef EFFECT_CATEGORIES
   
   // Add all LV2 categories and their relationships
   LilvPluginClasses classes = Lilv_world_get_plugin_classes(gWorld);
   for (unsigned index = 0; index < Lilv_plugin_classes_size(classes);++index){
      LilvPluginClass c = Lilv_plugin_classes_get_at(classes, index);
      em.AddCategory(wxString::FromUTF8(lilv_node_as_uri(Lilv_plugin_class_get_uri(c))), 
                     wxString::FromUTF8(lilv_node_as_string(Lilv_plugin_class_get_label(c))));
   }
   for (unsigned index = 0; index < Lilv_plugin_classes_size(classes);++index){
      LilvPluginClass c = Lilv_plugin_classes_get_at(classes, index);
      LilvPluginClasses ch = Lilv_plugin_class_get_children(c);
      EffectCategory* pCat = em.LookupCategory(wxString::FromUTF8(lilv_node_as_uri(Lilv_plugin_class_get_uri(c))));
      for (unsigned j = 0; j < Lilv_plugin_classes_size(ch); ++j) {
         EffectCategory* chCat = em.LookupCategory(wxString::FromUTF8(lilv_node_as_uri(Lilv_plugin_class_get_uri(Lilv_plugin_classes_get_at(ch, j)))));
         if (chCat && pCat) {
            em.AddCategoryParent(chCat, pCat);
         }
      }
   }

#endif
   
   // Retrieve data about all plugins
   const LilvPlugins *plugs = lilv_world_get_all_plugins(gWorld);

   // Iterate over all plugins and register them with the EffectManager
   LILV_FOREACH(plugins, i, plugs)
   {
      const LilvPlugin *plug = lilv_plugins_get(plugs, i);
      std::set<wxString> cats;
      cats.insert(wxString::FromUTF8(lilv_node_as_uri(lilv_plugin_class_get_uri(lilv_plugin_get_class(plug)))));
      LV2Effect *effect = new LV2Effect(plug, cats);
      if (effect->IsValid())
         em.RegisterEffect(effect);
      else
         delete effect;
      //std::cerr<<"Loaded LV2 \""<<lilv_node_as_string(Lilv_plugin_get_name(plug))<<"\""<<std::endl;
   }
  
}

void UnloadLV2Plugins()
{
   lilv_node_free(gAudioPortClass);
   gAudioPortClass = NULL;

   lilv_node_free(gControlPortClass);
   gControlPortClass = NULL;

   lilv_node_free(gMidiPortClass);
   gMidiPortClass = NULL;

   lilv_node_free(gInputPortClass);
   gInputPortClass = NULL;

   lilv_node_free(gOutputPortClass);
   gOutputPortClass = NULL;

   lilv_node_free(gPortToggled);
   gPortToggled = NULL;

   lilv_node_free(gPortIsInteger);
   gPortIsInteger = NULL;

   lilv_node_free(gPortIsSampleRate);
   gPortIsSampleRate = NULL;

   lilv_node_free(gPortIsEnumeration);
   gPortIsEnumeration = NULL;

   lilv_node_free(gPortIsLatency);
   gPortIsLatency = NULL;

   lilv_node_free(gPortIsOptional);
   gPortIsOptional = NULL;

   lilv_node_free(gName);
   gName = NULL;

   lilv_node_free(gPortGroup);
   gPortGroup = NULL;

   lilv_node_free(gSubGroupOf);
   gSubGroupOf = NULL;

   lilv_world_free(gWorld);
   gWorld = NULL;
}

#endif
