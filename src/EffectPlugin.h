/*!********************************************************************

   Audacity: A Digital Audio Editor

   @file EffectPlugin.h

   Paul Licameli
   split from EffectInterface.h

**********************************************************************/
#ifndef __AUDACITY_EFFECTPLUGIN_H__
#define __AUDACITY_EFFECTPLUGIN_H__

#include "EffectInterface.h"

#include "BasicUI.h"
#include <functional>
#include <memory>

class EffectSettingsManager;

class EffectSettings;
class EffectSettingsAccess;
class EffectPlugin;
class TrackList;
class WaveTrackFactory;

//! Information about dialogs connected to an effect instance,
//! and the selection of tracks, time, and frequencies to process
struct EffectContext {
   explicit EffectContext(unsigned uiFlags = 0)
      : uiFlags{ uiFlags }
   {}

   //! Count wave tracks and stereo groups
   void CountWaveTracks(const TrackList &tracks);

   // The Progress methods all return true if the user has cancelled;
   // you should exit immediately if this happens (cleaning up memory
   // is okay, but don't try to undo).

   // Pass a fraction between 0.0 and 1.0
   bool TotalProgress(double frac, const TranslatableString & = {}) const;

   // Pass a fraction between 0.0 and 1.0, for the current track
   // (when doing one track at a time)
   bool TrackProgress(int whichTrack, double frac,
      const TranslatableString & = {}) const;

   // Pass a fraction between 0.0 and 1.0, for the current track group
   // (when doing stereo groups at a time)
   bool TrackGroupProgress(int whichGroup, double frac,
      const TranslatableString & = {}) const;

   unsigned TestUIFlags(unsigned mask) {
      return mask & uiFlags;
   }

   BasicUI::ProgressDialog *pProgress{};
   /* const */ unsigned numTracks{}; //!< This is really mNumWaveTracks
   /* const */ unsigned numGroups{};

   bool isPreviewing{ false };

   /*!
    Effects can assign true if we want to skip pushing state after processing
    */
   bool skipState{ false };

   WaveTrackFactory   *factory{};
   unsigned       uiFlags{ 0 };
};

class TrackList;
class WaveTrackFactory;
class NotifyingSelectedRegion;
class EffectInstanceEx;

/***************************************************************************//**
\class EffectPlugin
@brief Factory of instances of an effect
*******************************************************************************/
class AUDACITY_DLL_API EffectPlugin
   : public EffectInstanceFactory
{
public:
   using EffectSettingsAccessPtr = std::shared_ptr<EffectSettingsAccess>;

   const static wxString kUserPresetIdent;
   const static wxString kFactoryPresetIdent;
   const static wxString kCurrentSettingsIdent;
   const static wxString kFactoryDefaultsIdent;

   EffectPlugin &operator=(EffectPlugin&) = delete;
   virtual ~EffectPlugin();

   //! Whether there are preferences applying to the effect's family, for which
   //! a UI may be needed to change them
   virtual bool HasOptions() const = 0;

   //! Whether the effect supports export of presets to files, and importing too
   virtual bool CanExportPresets() const = 0;

   virtual const EffectSettingsManager& GetDefinition() const = 0;

   //! Calculate temporary tracks of limited length with effect applied and play
   /*!
    @param updateUI called after adjusting temporary settings and before play
    */
   virtual void Preview(EffectContext &context,
      EffectSettingsAccess &access, std::function<void()> updateUI,
      bool dryOnly) = 0;
   virtual bool SaveSettingsAsString(
      const EffectSettings &settings, wxString & parms) const = 0;
   // @return nullptr for failure
   [[nodiscard]] virtual OptionalMessage LoadSettingsFromString(
      const wxString & parms, EffectSettings &settings) const = 0;
   virtual bool IsBatchProcessing() const = 0;
   virtual void SetBatchProcessing() = 0;
   virtual void UnsetBatchProcessing() = 0;

   using InstancePointer = std::shared_ptr<EffectInstanceEx>;
   //! nullopt for failure is distinct from successful null return
   //! If a non-null pointer is returned, assume Init() also succeeded on it
   //! If a null pointer, then DoEffect will use MakeInstance()
   using InstanceFinder = std::function<
      std::optional<InstancePointer>(EffectSettings &settings)
   >;

   /*!
    Will only operate on tracks that have the "selected" flag set to true,
    which is consistent with Audacity's standard UI, and only when
    finder succeeds

    @return true on success
    */
   virtual bool DoEffect(EffectContext &context,
      EffectSettings &settings, //!< Always given; only for processing
      const InstanceFinder &finder,
      double projectRate, TrackList *list,
      NotifyingSelectedRegion &selectedRegion,
      const EffectSettingsAccessPtr &pAccess = nullptr
         //!< Sometimes given; only for UI
   ) = 0;
};

/***************************************************************************//**
\class EffectInstanceEx
@brief Performs effect computation
*******************************************************************************/
class AUDACITY_DLL_API EffectInstanceEx : public virtual EffectInstance {
public:
   //! Call once to set up state for whole list of tracks to be processed
   /*!
    @return success
    Default implementation does nothing, returns true
    */
   virtual bool Init();

   //! Actually do the effect here.
   /*!
    @return success
    */
   virtual bool Process(EffectContext &context, EffectSettings &settings) = 0;

   ~EffectInstanceEx() override;
};
#endif
