/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LV2FeaturesList.h

  Paul Licameli split from LV2Effect.h

  Audacity(R) is copyright (c) 1999-2013 Audacity Team.
  License: GPL v2 or later.  See License.txt.

*********************************************************************/

#ifndef __AUDACITY_LV2_FEATURES_LIST__
#define __AUDACITY_LV2_FEATURES_LIST__

#if USE_LV2

#include "lv2/log/log.h"
#include "lv2/uri-map/uri-map.h"

#include "ComponentInterfaceSymbol.h"
#include "LV2Symbols.h"
#include "LV2Preferences.h" // for DEFAULT_BLOCKSIZE

// Define a reasonable default sequence size in bytes
#define DEFAULT_SEQSIZE 8192

using LilvNodesPtr = Lilv_ptr<LilvNodes, lilv_nodes_free>;

//! Abstraction of a list of features, with a check for satisfaction of
//! requirements of a given lv2 "subject"
class LV2_API LV2FeaturesListBase {
public:
   explicit LV2FeaturesListBase(const LilvPlugin &plug);
   virtual ~LV2FeaturesListBase();

   LV2FeaturesListBase(const LV2FeaturesListBase&) = delete;
   LV2FeaturesListBase& operator=(const LV2FeaturesListBase&) = delete;

   //! Get vector of pointers to features, whose `.data()` can be passed to lv2
   using FeaturePointers = std::vector<const LV2_Feature *>;
   virtual FeaturePointers GetFeaturePointers() const = 0;

   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @return whether all required features of subject are supported
    */
   bool ValidateFeatures(const LilvNode *subject);

   /*!
    @param subject URI of the host or of the UI identifies a resource in lv2
    @param required whether to check required or optional features of subject
    @return true only if `!required` or else all required features are supported
    */
   bool CheckFeatures(const LilvNode *subject, bool required);

   const LilvPlugin &mPlug;
   bool mNoResize{ false };
};

// Dummy "keyword" argument prevents overload resolution to the copy ctor
struct WithBase_t{};
constexpr WithBase_t WithBase;

//! Extends one (immutable) feature list (whose lifetime contains this one's)
class LV2_API ExtendedLV2FeaturesList : public LV2FeaturesListBase {
public:
   explicit ExtendedLV2FeaturesList(WithBase_t,
      const LV2FeaturesListBase &baseFeatures);
   virtual ~ExtendedLV2FeaturesList();
   FeaturePointers GetFeaturePointers() const override;
   void AddFeature(const char *uri, const void *data);
   const LV2FeaturesListBase &mBaseFeatures;
protected:
   std::vector<LV2_Feature> mFeatures;
};

class LV2_API LV2FeaturesList : public LV2FeaturesListBase {
public:
   static ComponentInterfaceSymbol GetPluginSymbol(const LilvPlugin &plug);

   explicit LV2FeaturesList(const LilvPlugin &plug);
   ~LV2FeaturesList() override;

   //! @return success
   bool InitializeFeatures();

   FeaturePointers GetFeaturePointers() const override;

   //! @return whether our host should reciprocally supply the
   //! LV2_Worker_Schedule interface to the plug-in
   static bool SuppliesWorkerInterface(const LilvPlugin &plug);

   //! @return whether our host should reciprocally supply the
   //! LV2_Worker_Schedule interface to the plug-in
   bool SuppliesWorkerInterface() const { return mSuppliesWorkerInterface; }

   void AddFeature(const char *uri, const void *data);

   // lv2 functions require a pointer to non-const in places, but presumably
   // have no need to mutate the members of this structure
   LV2_URID_Map *URIDMapFeature() const
   { return const_cast<LV2_URID_Map*>(&mURIDMapFeature); }

   LV2_URID URID_Map(const char *uri) const;

protected:
   static uint32_t uri_to_id(LV2_URI_Map_Callback_Data callback_data,
      const char *map, const char *uri);
   static LV2_URID urid_map(LV2_URID_Map_Handle handle, const char *uri);
   static const char *urid_unmap(LV2_URID_Unmap_Handle handle, LV2_URID urid);
   const char *URID_Unmap(LV2_URID urid);

   static int log_printf(LV2_Log_Handle handle,
      LV2_URID type, const char *fmt, ...);
   static int log_vprintf(LV2_Log_Handle handle,
      LV2_URID type, const char *fmt, va_list ap);
   int LogVPrintf(LV2_URID type, const char *fmt, va_list ap);

   // These objects contain C-style virtual function tables that we fill in
   const LV2_URI_Map_Feature mUriMapFeature{
      this, LV2FeaturesList::uri_to_id }; // Features we support
   const LV2_URID_Map mURIDMapFeature{ this, LV2FeaturesList::urid_map };
   const LV2_URID_Unmap mURIDUnmapFeature{ this, LV2FeaturesList::urid_unmap };
   const LV2_Log_Log mLogFeature{
      this, LV2FeaturesList::log_printf, LV2FeaturesList::log_vprintf };

   //! Per-effect URID map allocates an ID for each URI on first lookup
   /*!
    This is some state shared among all instances of an effect, but logically
    const as a mapping, assuming all reverse lookups of any urid (integer) are
    done only after at least one lookup of the related uri (string)
    */
   mutable LV2Symbols::URIDMap mURIDMap;

   std::vector<LV2_Feature> mFeatures;

   const bool mSuppliesWorkerInterface;

public:
   const bool mOk;
};

#endif
#endif
