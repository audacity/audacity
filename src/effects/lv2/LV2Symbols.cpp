/*!********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2Symbols.cpp

  Paul Licameli split from LoadLV2.cpp

*********************************************************************/

#include "LV2Symbols.h"
#include <wx/crt.h>
#include "lv2/atom/atom.h"
#include "lv2/buf-size/buf-size.h"
#include "lv2/log/log.h"
#include "lv2/midi/midi.h"
#include "lv2/options/options.h"
#include "lv2/parameters/parameters.h"
#include "lv2/port-groups/port-groups.h"
#include "lv2/port-props/port-props.h"
#include "lv2/presets/presets.h"
#include "lv2/resize-port/resize-port.h"
#include "lv2/time/time.h"
#include "lv2/units/units.h"
#include "lv2_external_ui.h"

namespace LV2Symbols {

LilvWorld *gWorld = nullptr;

// Define the static URI map
URIDMap gURIDMap;

// Define the static LILV URI nodes
#undef NODE
#define NODE(n, u) LilvNode *node_##n = nullptr;
NODELIST

// Define the static URIDs
#undef URID
#define URID(n, u) LV2_URID urid_##n = 0;
URIDLIST

bool InitializeGWorld()
{
   // Try to initialise Lilv, or return.
   gWorld = lilv_world_new();
   if (!gWorld)
      return false;

   // Create LilvNodes for each of the URIs we need
   #undef NODE
   #define NODE(n, u) node_##n = lilv_new_uri(gWorld, u);
   NODELIST

   // Generate URIDs
   #undef URID
   #define URID(n, u) urid_##n = Lookup_URI(gURIDMap, u);
      URIDLIST

   return true;
}

void FinalizeGWorld()
{
   // Free the LilvNodes for each of the URIs we need
   #undef NODE
   #define NODE(n, u) \
      lilv_node_free(node_##n);
   NODELIST

   lilv_world_free(gWorld);
   gWorld = nullptr;
}

LV2_URID Lookup_URI(URIDMap & map, const char *uri, bool add)
{
   size_t ndx = map.size();
   for (size_t i = 0; i < ndx; ++i)
      if (strcmp(map[i].get(), uri) == 0)
         return i + 1;
   if (add) {
      // Almost all compilers have strdup(),
      // but VC++ and MinGW call it _strdup().
      map.push_back(MallocString<>(wxCRT_StrdupA(uri)));
      return ndx + 1;
   }
   return 0;
}

}
