/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2InstanceFeaturesList.cpp

  Paul Licameli split from LV2Effect.cpp

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2 or later.  See License.txt.

**********************************************************************/

#include "LV2InstanceFeaturesList.h"
#include <wx/log.h>

LV2InstanceFeaturesList::LV2InstanceFeaturesList(
   const LV2FeaturesList &baseFeatures
)  : ExtendedLV2FeaturesList{ WithBase, baseFeatures }
   , mOk{ InitializeOptions() }
{
   AddFeature(LV2_OPTIONS__options, mOptions.data());
}

bool LV2InstanceFeaturesList::InitializeOptions()
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

size_t LV2InstanceFeaturesList::AddOption(
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

const LV2_Options_Option *
LV2InstanceFeaturesList::NominalBlockLengthOption() const
{
   if (mSupportsNominalBlockLength)
      return &mOptions[mBlockSizeOption];
   else
      return nullptr;
}

bool LV2InstanceFeaturesList::ValidateOptions(const LilvNode *subject)
{
   return CheckOptions(subject, true) && CheckOptions(subject, false);
}

bool LV2InstanceFeaturesList::CheckOptions(
   const LilvNode *subject, bool required)
{
   using namespace LV2Symbols;
   bool supported = true;
   const auto predicate =
      required ? node_RequiredOption : node_SupportedOption;
   if (LilvNodesPtr nodes{
      lilv_world_find_nodes(gWorld, subject, predicate, nullptr) }
   ){
      LILV_FOREACH(nodes, i, nodes.get()) {
         const auto node = lilv_nodes_get(nodes.get(), i);
         const auto uri = lilv_node_as_string(node);
         // The signature of our constructor justifies this static_cast
         const auto urid = static_cast<const LV2FeaturesList&>(mBaseFeatures)
            .URID_Map(uri);
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

LV2WrapperFeaturesList::LV2WrapperFeaturesList(
   LV2InstanceFeaturesList &baseFeatures, float sampleRate,
   const LV2_Worker_Schedule *pWorkerSchedule
) : ExtendedLV2FeaturesList{ WithBase, baseFeatures }
{
   baseFeatures.mSampleRate = sampleRate;
   auto &base = baseFeatures.Base();
   if (base.SuppliesWorkerInterface()) {
      // Inform the plugin how to send work to another thread
      AddFeature( LV2_WORKER__schedule, pWorkerSchedule );
   }
}
