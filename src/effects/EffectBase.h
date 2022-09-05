/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectBase.h

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from Effect.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_BASE__
#define __AUDACITY_EFFECT_BASE__

// Wanted to include just wx/weakref.h, but it fails to compile on Windows
// when you have wxWeakRef to an incomplete type
#include <wx/dialog.h>

#include "EffectPlugin.h" // to inherit

#include <any>

namespace BasicUI { class ProgressDialog; }

class AudacityProject;
class DoubleSetting;
class Track;

class AUDACITY_DLL_API EffectBase /* not final */
   : public EffectPlugin
{
public:
   EffectBase();
   ~EffectBase() override;

   void SetTracks(TrackList *pTracks) { mTracks = pTracks; }

   //! Called when Preview() starts, to allow temporary effect state changes
   /*!
    default returns a null
    @return will undo its effects in its destructor before Preview() finishes
    */
   virtual std::any BeginPreview(const EffectSettings &settings);

   // The EffectBase class fully implements the Preview method for you.
   // Only override it if you need to do preprocessing or cleanup.
   void Preview(EffectContext &context,
      EffectSettingsAccess &access, std::function<void()> updateUI,
      bool dryOnly) final;

   bool DoEffect(EffectContext &context,
      EffectSettings &settings, //!< Always given; only for processing
      const InstanceFinder &finder,
      double projectRate, TrackList *list,
      NotifyingSelectedRegion &selectedRegion,
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
   virtual double CalcPreviewInputLength(const EffectContext &context,
      const EffectSettings &settings, double previewLength) const = 0;

   // Previewing linear effect can be optimised by pre-mixing. However this
   // should not be used for non-linear effects such as dynamic processors
   // To allow pre-mixing before Preview, set linearEffectFlag to true.
   void SetLinearEffectFlag(bool linearEffectFlag);

   // Most effects only need to preview a short selection. However some
   // (such as fade effects) need to know the full selection length.
   void SetPreviewFullSelectionFlag(bool previewDurationFlag);

   // A global counter of all the successful Effect invocations.
   static int nEffectsDone;

   // If bGoodResult, replace mWaveTracks tracks in mTracks with successfully processed
   // mOutputTracks copies, get rid of old mWaveTracks, and set mWaveTracks to mOutputTracks.
   // Else clear and DELETE mOutputTracks copies.
   void ReplaceProcessedTracks(const bool bGoodResult);

   double         mProjectRate{}; // Sample rate of the project - NEW tracks should
                               // be created with this rate...
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

private:
   friend class Effect;

   double GetDefaultDuration();

   TrackList *mTracks{}; // the complete list of all tracks

   /* Consider these boolean members to be immutable properties of the effect,
    not state variables.  Usually set only during construction of the effect
    object.  (Exception possibly only for Nyquist effects that might detect
    in-session editing of the script file and reassign them; but that is
    interactive development by power users only)
    */
   bool mIsLinearEffect{ false };
   bool mPreviewFullSelection{ false };

   std::vector<Track*> mIMap;
   std::vector<Track*> mOMap;
};

/* i18n-hint: "Nyquist" is an embedded interpreted programming language in
 Audacity, named in honor of the Swedish-American Harry Nyquist (or Nyqvist).
 In the translations of this and other strings, you may transliterate the
 name into another alphabet.  */
#define NYQUISTEFFECTS_FAMILY ( EffectFamilySymbol{ XO("Nyquist") } )

extern AUDACITY_DLL_API DoubleSetting EffectPreviewLength;

#endif
