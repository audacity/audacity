/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2FeaturesList.cpp

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

LV2FeaturesList::LV2FeaturesList(const LilvPlugin &plug) : mPlug{ plug }
   , mSuppliesWorkerInterface{ SuppliesWorkerInterface(plug) }
{
}

bool LV2FeaturesList::InitializeOptions()
{
   using namespace LV2Symbols;

   // Construct the null-terminated array describing options, and validate it
   AddOption(urid_SequenceSize, sizeof(mSeqSize), urid_Int, &mSeqSize);
   AddOption(urid_MinBlockLength,
      sizeof(mMinBlockSize), urid_Int, &mMinBlockSize);
   AddOption(urid_MaxBlockLength,
      sizeof(mMaxBlockSize), urid_Int, &mMaxBlockSize);
   // Two options are reset later
   mBlockSizeOption = AddOption(urid_NominalBlockLength,
      sizeof(mBlockSize), urid_Int, &mBlockSize);
   AddOption(urid_SampleRate,
      sizeof(mSampleRate), urid_Float, &mSampleRate);
   AddOption(0, 0, 0, nullptr);
   if (!ValidateOptions(lilv_plugin_get_uri(&mPlug)))
      return false;

   // Adjust the values in the block size features according to the plugin
   if (LilvNodePtr minLength{ lilv_world_get(gWorld,
         lilv_plugin_get_uri(&mPlug), node_MinBlockLength, nullptr) }
      ; lilv_node_is_int(minLength.get())
   ){
      if (auto value = lilv_node_as_int(minLength.get())
         ; value >= 0
      )
         mMinBlockSize = std::max<size_t>(mMinBlockSize, value);
   }
   if (LilvNodePtr maxLength{ lilv_world_get(gWorld,
         lilv_plugin_get_uri(&mPlug), node_MaxBlockLength, nullptr) }
      ; lilv_node_is_int(maxLength.get())
   ){
      if (auto value = lilv_node_as_int(maxLength.get())
         ; value >= 1
      )
         mMaxBlockSize = std::min<size_t>(mMaxBlockSize, value);
   }
   mMaxBlockSize = std::max(mMaxBlockSize, mMinBlockSize);

   return true;
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
   AddFeature(LV2_OPTIONS__options, mOptions.data());
   AddFeature(LV2_URI_MAP_URI, &mUriMapFeature);
   AddFeature(LV2_URID__map, &mURIDMapFeature);
   AddFeature(LV2_URID__unmap, &mURIDUnmapFeature);
   AddFeature(LV2_LOG__log, &mLogFeature);
   // Some plugins specify this as a feature
   AddFeature(LV2_EXTERNAL_UI__Widget, nullptr);
   AddFeature(LV2_UI__parent, nullptr);

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

size_t LV2FeaturesList::AddOption(
   LV2_URID key, uint32_t size, LV2_URID type, const void *value)
{
   int ndx = mOptions.size();
   if (key != 0)
      mOptions.emplace_back(LV2_Options_Option{
         LV2_OPTIONS_INSTANCE, 0, key, size, type, value });
   else
      mOptions.emplace_back(LV2_Options_Option{});
   return ndx;
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

const LV2_Options_Option *LV2FeaturesList::NominalBlockLengthOption() const
{
   if (mSupportsNominalBlockLength)
      return &mOptions[mBlockSizeOption];
   else
      return nullptr;
}

bool LV2FeaturesList::ValidateFeatures(const LilvNode *subject)
{
   return CheckFeatures(subject, true) && CheckFeatures(subject, false);
}

bool LV2FeaturesList::CheckFeatures(const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   auto predicate = required ? node_RequiredFeature : node_OptionalFeature;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }) {
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
            const auto end = mFeatures.end();
            supported = (end != std::find_if(mFeatures.begin(), end,
               [&](auto &feature){ return strcmp(feature.URI, uri) == 0; }));
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

bool LV2FeaturesList::ValidateOptions(const LilvNode *subject)
{
   return CheckOptions(subject, true) && CheckOptions(subject, false);
}

bool LV2FeaturesList::CheckOptions(const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   const auto predicate =
      required ? node_RequiredOption : node_SupportedOption;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }) {
      LILV_FOREACH(nodes, i, nodes.get()) {
         const auto node = lilv_nodes_get(nodes.get(), i);
         const auto uri = lilv_node_as_string(node);
         const auto urid = URID_Map(uri);
         if (urid == urid_NominalBlockLength)
            mSupportsNominalBlockLength = true;
         // else if (urid == urid_SampleRate)
            // mSupportsSampleRate = true; // supports changing sample rate
         else if (required) {
            const auto end = mOptions.end();
            supported = (end != std::find_if(mOptions.begin(), end,
               [&](const auto &option){ return option.key == urid; }));
            if (!supported) {
               wxLogError(wxT("%s requires unsupported option %s"),
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

LV2_URID LV2FeaturesList::URID_Map(const char *uri)
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
      wxT("%s: %s"), GetSymbol().Msgid().Translation(), text);
   return len;
}

#endif
