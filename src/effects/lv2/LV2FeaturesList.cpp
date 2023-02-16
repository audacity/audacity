/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2FeaturesList.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#if defined(USE_LV2)

#if defined(__GNUC__)
#pragma GCC diagnostic ignored "-Wparentheses"
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#elif defined(__clang__)
#pragma clang diagnostic ignored "-Wparentheses"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#endif

#include "LV2FeaturesList.h"
#include <wx/crt.h>
#include <wx/log.h>
#include "lv2/buf-size/buf-size.h"
#include "lv2_external_ui.h"
#include "lv2/worker/worker.h"

LV2FeaturesListBase::~LV2FeaturesListBase() = default;

ExtendedLV2FeaturesList::~ExtendedLV2FeaturesList() = default;
ExtendedLV2FeaturesList::ExtendedLV2FeaturesList(WithBase_t,
   const LV2FeaturesListBase &baseFeatures
) : LV2FeaturesListBase{ baseFeatures.mPlug }
  , mBaseFeatures{ baseFeatures }
{
}

auto ExtendedLV2FeaturesList::GetFeaturePointers() const -> FeaturePointers
{
   FeaturePointers result{ mBaseFeatures.GetFeaturePointers() };
   result.pop_back();
   for (auto &feature : mFeatures)
      result.push_back(&feature);
   result.push_back(nullptr);
   return result;
}

void ExtendedLV2FeaturesList::AddFeature(const char *uri, const void *data)
{
   // This casting to const is innocent
   // We pass our "virtual function tables" or array of options, which the
   // library presumably will not change
   mFeatures.emplace_back(LV2_Feature{ uri, const_cast<void*>(data) });
}

LV2FeaturesList::~LV2FeaturesList() = default;

ComponentInterfaceSymbol
LV2FeaturesList::GetPluginSymbol(const LilvPlugin &plug)
{
   return LilvStringMove(lilv_plugin_get_name(&plug));
}

LV2FeaturesListBase::LV2FeaturesListBase(const LilvPlugin &plug) : mPlug{ plug }
{
}

LV2FeaturesList::LV2FeaturesList(const LilvPlugin &plug)
   : LV2FeaturesListBase{ plug }
   , mSuppliesWorkerInterface{ SuppliesWorkerInterface(plug) }
   , mOk{ InitializeFeatures() }
{
}

bool LV2FeaturesList::InitializeFeatures()
{
   // Construct null-terminated array of "features" describing our capabilities
   // to lv2, and validate
   AddFeature(LV2_UI__noUserResize, nullptr);
   AddFeature(LV2_UI__fixedSize, nullptr);
   AddFeature(LV2_UI__idleInterface, nullptr);
   AddFeature(LV2_UI__makeResident, nullptr);
   AddFeature(LV2_BUF_SIZE__boundedBlockLength, nullptr);
   AddFeature(LV2_BUF_SIZE__fixedBlockLength, nullptr);
   AddFeature(LV2_URI_MAP_URI, &mUriMapFeature);
   AddFeature(LV2_URID__map, &mURIDMapFeature);
   AddFeature(LV2_URID__unmap, &mURIDUnmapFeature);
   AddFeature(LV2_LOG__log, &mLogFeature);
   // Some plugins specify this as a feature
   AddFeature(LV2_EXTERNAL_UI__Widget, nullptr);
   return true;
}

bool LV2FeaturesList::SuppliesWorkerInterface(const LilvPlugin &plug)
{
   bool result = false;
   if (LilvNodesPtr extdata{ lilv_plugin_get_extension_data(&plug) }) {
      LILV_FOREACH(nodes, i, extdata.get()) {
         const auto node = lilv_nodes_get(extdata.get(), i);
         const auto uri = lilv_node_as_string(node);
         if (strcmp(uri, LV2_WORKER__interface) == 0)
            result = true;
      }
   }
   return result;
}

void LV2FeaturesList::AddFeature(const char *uri, const void *data)
{
   // This casting to const is innocent
   // We pass our "virtual function tables" or array of options, which the
   // library presumably will not change
   mFeatures.emplace_back(LV2_Feature{ uri, const_cast<void*>(data) });
}

auto LV2FeaturesList::GetFeaturePointers() const -> FeaturePointers
{
   FeaturePointers result;
   for (auto &feature : mFeatures)
      result.push_back(&feature);
   result.push_back(nullptr);
   return result;
}

bool LV2FeaturesListBase::ValidateFeatures(const LilvNode *subject)
{
   return CheckFeatures(subject, true) && CheckFeatures(subject, false);
}

bool LV2FeaturesListBase::CheckFeatures(const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   auto predicate = required ? node_RequiredFeature : node_OptionalFeature;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }
   ){
      LILV_FOREACH(nodes, i, nodes.get()) {
         const auto node = lilv_nodes_get(nodes.get(), i);
         const auto uri = lilv_node_as_string(node);
         if ((strcmp(uri, LV2_UI__noUserResize) == 0) ||
             (strcmp(uri, LV2_UI__fixedSize) == 0))
            mNoResize = true;
         else if (strcmp(uri, LV2_WORKER__schedule) == 0) {
            /* Supported but handled in LV2Wrapper */
         }
         else if (required) {
            auto features = GetFeaturePointers();
            const auto end = features.end();
            supported = (end != std::find_if(features.begin(), end,
               [&](auto &pFeature){ return pFeature &&
                  strcmp(pFeature->URI, uri) == 0; }));
            if (!supported) {
               wxLogError(wxT("%s requires unsupported feature %s"),
                  lilv_node_as_string(lilv_plugin_get_uri(&mPlug)), uri);
               break;
            }
         }
      }
   }
   return supported;
}

// ============================================================================
// Feature handlers
// ============================================================================

// static callback
uint32_t LV2FeaturesList::uri_to_id(
   LV2_URI_Map_Callback_Data callback_data, const char *, const char *uri)
{
   return static_cast<LV2FeaturesList *>(callback_data)->URID_Map(uri);
}

// static callback
LV2_URID LV2FeaturesList::urid_map(LV2_URID_Map_Handle handle, const char *uri)
{
   return static_cast<LV2FeaturesList *>(handle)->URID_Map(uri);
}

LV2_URID LV2FeaturesList::URID_Map(const char *uri) const
{
   using namespace LV2Symbols;
   // Map global URIs to lower indices
   auto urid = Lookup_URI(gURIDMap, uri, false);
   if (urid > 0)
      return urid;
   // Map local URIs to higher indices
   urid = Lookup_URI(mURIDMap, uri);
   if (urid > 0)
      return urid + gURIDMap.size();
   return 0;
}

// static callback
const char *LV2FeaturesList::urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid)
{
   return static_cast<LV2FeaturesList *>(handle)->URID_Unmap(urid);
}

const char *LV2FeaturesList::URID_Unmap(LV2_URID urid)
{
   using namespace LV2Symbols;
   if (urid > 0) {
      // Unmap lower indices to global URIs
      if (urid <= static_cast<LV2_URID>(gURIDMap.size()))
         return mURIDMap[urid - 1].get();
      // Unmap higher indices to local URIs
      urid -= gURIDMap.size();
      if (urid <= static_cast<LV2_URID>(mURIDMap.size()))
         return mURIDMap[urid - 1].get();
   }
   return nullptr;
}

// static callback
int LV2FeaturesList::log_printf(
   LV2_Log_Handle handle, LV2_URID type, const char *fmt, ...)
{
   va_list ap;
   int len;

   va_start(ap, fmt);
   len = static_cast<LV2FeaturesList *>(handle)->LogVPrintf(type, fmt, ap);
   va_end(ap);

   return len;
}

// static callback
int LV2FeaturesList::log_vprintf(
   LV2_Log_Handle handle, LV2_URID type, const char *fmt, va_list ap)
{
   return static_cast<LV2FeaturesList *>(handle)->LogVPrintf(type, fmt, ap);
}

int LV2FeaturesList::LogVPrintf(LV2_URID type, const char *fmt, va_list ap)
{
   using namespace LV2Symbols;
   long level = wxLOG_Error;
   if (type == urid_Error)
      level = wxLOG_Error;
   else if (type == urid_Note)
      level = wxLOG_Info;
   else if (type == urid_Trace)
      level = wxLOG_Trace;
   else if (type == urid_Warning)
      level = wxLOG_Warning;
   else
      level = wxLOG_Message;
   int len = wxCRT_VsnprintfA(nullptr, 0, fmt, ap);
   auto msg = std::make_unique<char[]>(len + 1);
   wxCRT_VsnprintfA(msg.get(), len, fmt, ap);
   wxString text(msg.get());
   wxLogGeneric(level,
      wxT("%s: %s"), GetPluginSymbol(mPlug).Msgid().Translation(), text);
   return len;
}
#endif
