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

namespace BasicUI {
class ProgressDialog;
}

namespace au::effects {
class EffectsProvider;
class EffectExecutionScenario;
}

class AudacityProject;
class Track;

class EFFECTS_API EffectBase /* not final */ : public EffectPlugin
{
public:
    EffectBase();
    ~EffectBase() override;

    bool IsLinearEffect() const { return mIsLinearEffect; }
    bool PreviewsFullSelection() const { return mPreviewFullSelection; }

    void SetTracks(TrackList* pTracks);

    double GetDefaultDuration();

    //! Called when Preview() starts, to allow temporary effect state changes
    /*!
     default returns a null
     @return will undo its effects in its destructor before Preview() finishes
     */
    virtual std::any BeginPreview(const EffectSettings& settings);

    bool DoEffect(EffectSettings& settings, //!< Always given; only for processing
                  const InstanceFinder& finder, double projectRate, TrackList* list, WaveTrackFactory* factory,
                  NotifyingSelectedRegion& selectedRegion, unsigned flags, const EffectSettingsAccessPtr& pAccess //!< Sometimes given; only for UI
                  ) override;

    static std::optional<InstancePointer> FindInstance(EffectPlugin& plugin);
    static InstanceFinder DefaultInstanceFinder(EffectPlugin& plugin);

protected:
    //! NOTE Temporary solution
    friend class au::effects::EffectsProvider;
    friend class au::effects::EffectExecutionScenario;

    //! After Init(), tell whether Process() should be skipped
    /*
      Typically this is only useful in automation, for example
      detecting that zero noise reduction is to be done,
      or that normalisation is being done without Dc bias shift
      or amplitude modification.
     */
    virtual bool CheckWhetherSkipEffect(const EffectSettings& settings) const
    = 0;

public:
    // Determine duration of effect preview, given a suggested value
    /*
      Most effects just use the previewLength, but time-stretching/compressing
      effects need to use a different input length, so override this method.

      @return seconds
     */
    virtual double CalcPreviewInputLength(
        const EffectSettings& settings, double previewLength) const = 0;

protected:
    // Previewing linear effect can be optimised by pre-mixing. However this
    // should not be used for non-linear effects such as dynamic processors
    // To allow pre-mixing before Preview, set linearEffectFlag to true.
    void SetLinearEffectFlag(bool linearEffectFlag);

    // Most effects only need to preview a short selection. However some
    // (such as fade effects) need to know the full selection length.
    void SetPreviewFullSelectionFlag(bool previewDurationFlag);

    // Use this if the effect needs to know if it is previewing
    bool IsPreviewing() const { return mIsPreview; }

    const TrackList* inputTracks() const { return mTracks.get(); }
    const AudacityProject* FindProject() const;

    double mF0{};
    double mF1{};

    wxArrayString mPresetNames;
    unsigned mUIFlags{ 0 };

private:
    friend class Effect;

public:
    // Public until we can move these fields out of here into EffectContext
    std::shared_ptr<TrackList> mTracks{}; // the complete list of all tracks
    int mNumTracks{}; // This is really mNumWaveTracks, per CountWaveTracks() and GetNumWaveTracks().
    BasicUI::ProgressDialog* mProgress{}; // Temporary pointer, NOT deleted in destructor.
    double mProjectRate{};        // Sample rate of the project - NEW tracks should
                                  // be created with this rate...
    WaveTrackFactory* mFactory{};
    double mT0{};
    double mT1{};
    bool mIsPreview{ false };

    // Some public members that only change "context" fields

    void CountWaveTracks();

private:
    bool mIsLinearEffect{ false };
    bool mPreviewFullSelection{ false };

    int mNumGroups{};
};

/* i18n-hint: "Nyquist" is an embedded interpreted programming language in
 Audacity, named in honor of the Swedish-American Harry Nyquist (or Nyqvist).
 In the translations of this and other strings, you may transliterate the
 name into another alphabet.  */
#define NYQUISTEFFECTS_FAMILY (EffectFamilySymbol{ XO("Nyquist") })

#endif
