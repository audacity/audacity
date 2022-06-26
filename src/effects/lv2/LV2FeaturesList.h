/**********************************************************************

  Audacity: A Digital Audio Editor

  LV2Effect.h

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_FEATURES_LIST__
#define __AUDACITY_LV2_FEATURES_LIST__

#if USE_LV2

#include "lv2/log/log.h"
#include "lv2/options/options.h"
#include "lv2/uri-map/uri-map.h"

#include "../StatefulPerTrackEffect.h"
#include "LV2Symbols.h"
#include "LV2Preferences.h" // for DEFAULT_BLOCKSIZE

// Define a reasonable default sequence size in bytes
#define DEFAULT_SEQSIZE 8192

using LilvNodesPtr = Lilv_ptr<LilvNodes, lilv_nodes_free>;

class LV2FeaturesList : public StatefulPerTrackEffect {
public:
   explicit LV2FeaturesList(const LilvPlugin &plug);

   //! @return success
   bool InitializeOptions();

   //! To be called after InitializeOptions()
   //! @return success
   bool InitializeFeatures();

   //! Get vector of pointers to features, whose `.data()` can be passed to lv2
   using FeaturePointers = std::vector<const LV2_Feature *>;
   FeaturePointers GetFeaturePointers() const;

   //! @return whether our host should reciprocally supply the
   //! LV2_Worker_Schedule interface to the plug-in
   static bool SuppliesWorkerInterface(const LilvPlugin &plug);

   //! @return whether our host should reciprocally supply the
   //! LV2_Worker_Schedule interface to the plug-in
   bool SuppliesWorkerInterface() const { return mSuppliesWorkerInterface; }
   //! @return may be null
   const LV2_Options_Option *NominalBlockLengthOption() const;

   size_t AddOption(LV2_URID, uint32_t size, LV2_URID, const void *value);

   /*!
    @param subject URI of a plugin
    @return whether all required features of subject are supported
    */
   bool ValidateOptions(const LilvNode *subject);

   /*!
    @param subject URI of a plugin
    @param required whether to check required or optional features of subject
    @return true only if `!required` or else all checked features are supported
    */
   bool CheckOptions(const LilvNode *subject, bool required);

   void AddFeature(const char *uri, const void *data);

   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @return whether all required features of subject are supported
    */
   bool ValidateFeatures(const LilvNode *subject);

   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @param required whether to check required or optional features of subject
    @return true only if `!required` or else all checked features are supported
    */
   bool CheckFeatures(const LilvNode *subject, bool required);

   //! May be needed before exposing features and options to the plugin
   void SetSampleRate(float sampleRate) const { mSampleRate = sampleRate; }

   const LilvPlugin &mPlug;

protected:
   // lv2 functions require a pointer to non-const in places, but presumably
   // have no need to mutate the members of this structure
   LV2_URID_Map *URIDMapFeature() const
   { return const_cast<LV2_URID_Map*>(&mURIDMapFeature); }

   static uint32_t uri_to_id(LV2_URI_Map_Callback_Data callback_data,
                             const char *map,
                             const char *uri);
   static LV2_URID urid_map(LV2_URID_Map_Handle handle, const char *uri);
   LV2_URID URID_Map(const char *uri);

   static const char *urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid);
   const char *URID_Unmap(LV2_URID urid);

   static int log_printf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, ...);
   static int log_vprintf(LV2_Log_Handle handle, LV2_URID type, const char *fmt, va_list ap);
   int LogVPrintf(LV2_URID type, const char *fmt, va_list ap);

   // These objects contain C-style virtual function tables that we fill in
   const LV2_URI_Map_Feature mUriMapFeature{
      this, LV2FeaturesList::uri_to_id }; // Features we support
   const LV2_URID_Map mURIDMapFeature{ this, LV2FeaturesList::urid_map };
   const LV2_URID_Unmap mURIDUnmapFeature{ this, LV2FeaturesList::urid_unmap };
   const LV2_Log_Log mLogFeature{
      this, LV2FeaturesList::log_printf, LV2FeaturesList::log_vprintf };

   // Declare local URI map
   LV2Symbols::URIDMap mURIDMap;

   std::vector<LV2_Options_Option> mOptions;
   size_t mBlockSizeOption{};

   std::vector<LV2_Feature> mFeatures;

   mutable float mSampleRate{ 44100 };
   size_t mBlockSize{ LV2Preferences::DEFAULT_BLOCKSIZE };
   int mSeqSize{ DEFAULT_SEQSIZE };

   size_t mMinBlockSize{ 1 };
   size_t mMaxBlockSize{ mBlockSize };

   const bool mSuppliesWorkerInterface;
   bool mSupportsNominalBlockLength{ false };

   bool mNoResize{ false };
};

#endif
#endif
