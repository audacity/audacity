/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectBase.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASE__
#define __AUDACITY_EFFECT_BASE__

#include "EffectPlugin.h" // to inherit

#include <any>

namespace BasicUI { class ProgressDialog; }

class AudacityProject;
class Track;

class EFFECTS_API EffectBase /* not final */
   : public EffectPlugin
{
public:
   EffectBase();
   ~EffectBase() override;

   void SetTracks(TrackList *pTracks) { mTracks = pTracks; }

protected:
   //! Called when Preview() starts, to allow temporary effect state changes
   /*!
    default returns a null
    @return will undo its effects in its destructor before Preview() finishes
    */
   virtual std::any BeginPreview(const EffectSettings &settings);

   // The EffectBase class fully implements the Preview method for you.
   // Only override it if you need to do preprocessing or cleanup.
   void Preview(
      EffectSettingsAccess &access, std::function<void()> updateUI,
      bool dryOnly) final;

 public:
   bool DoEffect(EffectSettings &settings, //!< Always given; only for processing
      const InstanceFinder &finder,
      double projectRate, TrackList *list,
      WaveTrackFactory *factory, NotifyingSelectedRegion &selectedRegion,
      unsigned flags,
      const EffectSettingsAccessPtr &pAccess //!< Sometimes given; only for UI
   ) override;

protected:
   //! After Init(), tell whether Process() should be skipped
   /*
     Typically this is only useful in automation, for example
     detecting that zero noise reduction is to be done,
     or that normalisation is being done without Dc bias shift
     or amplitude modification.
    */
   virtual bool CheckWhetherSkipEffect(const EffectSettings &settings) const
      = 0;

   // Determine duration of effect preview, given a suggested value
   /*
     Most effects just use the previewLength, but time-stretching/compressing
     effects need to use a different input length, so override this method.

     @return seconds
    */
   virtual double CalcPreviewInputLength(
      const EffectSettings &settings, double previewLength) const = 0;

   // Previewing linear effect can be optimised by pre-mixing. However this
   // should not be used for non-linear effects such as dynamic processors
   // To allow pre-mixing before Preview, set linearEffectFlag to true.
   void SetLinearEffectFlag(bool linearEffectFlag);

   // Most effects only need to preview a short selection. However some
   // (such as fade effects) need to know the full selection length.
   void SetPreviewFullSelectionFlag(bool previewDurationFlag);

   // Use this if the effect needs to know if it is previewing
   bool IsPreviewing() const { return mIsPreview; }

   // Most effects only require selected tracks to be copied for Preview.
   // If IncludeNotSelectedPreviewTracks(true), then non-linear effects have
   // preview copies of all wave tracks.
   void IncludeNotSelectedPreviewTracks(bool includeNotSelected);

   // A global counter of all the successful Effect invocations.
   static int nEffectsDone;

   // If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed
   // mOutputTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputTracks.
   // Else clear and DELETE mOutputTracks copies.
   void ReplaceProcessedTracks(const bool bGoodResult);

   BasicUI::ProgressDialog *mProgress{}; // Temporary pointer, NOT deleted in destructor.
   double         mProjectRate{}; // Sample rate of the project - NEW tracks should
                               // be created with this rate...
   WaveTrackFactory   *mFactory{};
   const TrackList *inputTracks() const { return mTracks; }
   const AudacityProject *FindProject() const;
   // used only if CopyInputTracks() is called.
   std::shared_ptr<TrackList> mOutputTracks;
   double         mT0{};
   double         mT1{};
#ifdef EXPERIMENTAL_SPECTRAL_EDITING
   double         mF0{};
   double         mF1{};
#endif
   wxArrayString  mPresetNames;
   unsigned       mUIFlags{ 0 };

private:
   friend class Effect;

   double GetDefaultDuration();

   void CountWaveTracks();

public:
   // Public until we can move this field out of here into EffectContext
   TrackList *mTracks{}; // the complete list of all tracks

private:
   bool mIsLinearEffect{ false };
   bool mPreviewWithNotSelected{ false };
   bool mPreviewFullSelection{ false };

   bool mIsPreview{ false };

   std::vector<Track*> mIMap;
   std::vector<Track*> mOMap;

   int mNumTracks{}; //v This is really mNumWaveTracks, per CountWaveTracks() and GetNumWaveTracks().
   int mNumGroups{};
};

/* i18n-hint: "Nyquist" is an embedded interpreted programming language in
 Audacity, named in honor of the Swedish-American Harry Nyquist (or Nyqvist).
 In the translations of this and other strings, you may transliterate the
 name into another alphabet.  */
#define NYQUISTEFFECTS_FAMILY ( EffectFamilySymbol{ XO("Nyquist") } )

#endif
